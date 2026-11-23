// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - JSON-RPC 2.0 transport layer

module copilot

import io
import json
import strings
import sync
import x.json2

// JsonRpcRequest is a JSON-RPC 2.0 request object.
pub struct JsonRpcRequest {
pub:
	jsonrpc string @[json: 'jsonrpc'] // always "2.0"
	id      int    @[json: 'id']
	method  string @[json: 'method']
	params  string @[json: 'params'] // raw JSON params
}

// JsonRpcResponse is a JSON-RPC 2.0 response object.
pub struct JsonRpcResponse {
pub:
	jsonrpc string       @[json: 'jsonrpc']
	id      int          @[json: 'id']
	result  string       @[json: 'result'] // raw JSON result
	error   JsonRpcError @[json: 'error']
}

// JsonRpcError represents the error field in a JSON-RPC response.
pub struct JsonRpcError {
pub:
	code    int    @[json: 'code']
	message string @[json: 'message']
	data    string @[json: 'data'] // raw JSON error data ('' when absent)
}

// JsonRpcNotification is a JSON-RPC notification (no id, no response expected).
pub struct JsonRpcNotification {
pub:
	jsonrpc string @[json: 'jsonrpc']
	method  string @[json: 'method']
	params  string @[json: 'params']
}

// RpcError is returned when the server answers a request with a JSON-RPC error.
// `data` holds the raw JSON error data, for example a `policyHelperFailure`.
pub struct RpcError {
	Error
pub:
	error_code int
	message    string
	data       string
}

// msg formats the error for display.
pub fn (e RpcError) msg() string {
	return 'JSON-RPC error ${e.error_code}: ${e.message}'
}

// code returns the JSON-RPC error code.
pub fn (e RpcError) code() int {
	return e.error_code
}

// IncomingMessage is one decoded message read from the server.
pub struct IncomingMessage {
pub:
	id        string // raw JSON id; empty for notifications
	method    string
	params    string // raw JSON params ('{}' when absent)
	result    string // raw JSON result ('null' when absent)
	error     JsonRpcError
	has_error bool
}

// is_request reports whether the message is a server-to-client request.
pub fn (m IncomingMessage) is_request() bool {
	return m.id.len > 0 && m.method.len > 0
}

// is_response reports whether the message answers one of our requests.
pub fn (m IncomingMessage) is_response() bool {
	return m.id.len > 0 && m.method.len == 0
}

// RequestHandler answers a server-to-client request with a raw JSON result.
pub type RequestHandler = fn (method string, params string) !string

// NotificationHandler receives server notifications.
pub type NotificationHandler = fn (method string, params string)

struct RpcReply {
	result string
	err    JsonRpcError
	failed bool
}

// RpcTransport manages sending and receiving JSON-RPC messages over a stream
// with Content-Length header framing (LSP-style).
@[heap]
pub struct RpcTransport {
mut:
	reader   io.BufferedReader
	writer   io.Writer
	next_id  int = 1
	mu       &sync.Mutex = sync.new_mutex()
	write_mu &sync.Mutex = sync.new_mutex()
	pending  map[int]chan RpcReply
pub mut:
	request_handler      RequestHandler      = unsafe { nil }
	notification_handler NotificationHandler = unsafe { nil }
}

// new_rpc_transport creates a transport from a reader/writer pair.
pub fn new_rpc_transport(reader io.Reader, writer io.Writer) &RpcTransport {
	return &RpcTransport{
		reader:  io.new_buffered_reader(reader: reader)
		writer:  writer
		mu:      sync.new_mutex()
		pending: map[int]chan RpcReply{}
	}
}

// encode_request renders a request with raw JSON params.
pub fn encode_request(id int, method string, params string) string {
	p := if params.trim_space().len == 0 { '{}' } else { params }
	return '{"jsonrpc":"2.0","id":${id},"method":${json.encode(method)},"params":${p}}'
}

// encode_notification renders a notification with raw JSON params.
pub fn encode_notification(method string, params string) string {
	p := if params.trim_space().len == 0 { '{}' } else { params }
	return '{"jsonrpc":"2.0","method":${json.encode(method)},"params":${p}}'
}

// encode_result_response renders a success response for a server request.
pub fn encode_result_response(raw_id string, result string) string {
	r := if result.trim_space().len == 0 { 'null' } else { result }
	return '{"jsonrpc":"2.0","id":${raw_id},"result":${r}}'
}

// encode_error_response renders an error response for a server request.
pub fn encode_error_response(raw_id string, code int, message string) string {
	return '{"jsonrpc":"2.0","id":${raw_id},"error":{"code":${code},"message":${json.encode(message)}}}'
}

// decode_incoming parses one raw message from the server.
pub fn decode_incoming(raw string) !IncomingMessage {
	value := json2.decode[json2.Any](raw)!
	obj := value.as_map()
	mut id := ''
	if v := obj['id'] {
		if v !is json2.Null {
			id = v.json_str()
		}
	}
	mut method := ''
	if v := obj['method'] {
		method = v.str()
	}
	mut params := '{}'
	if v := obj['params'] {
		params = v.json_str()
	}
	mut result := 'null'
	if v := obj['result'] {
		result = v.json_str()
	}
	mut rpc_err := JsonRpcError{}
	mut has_error := false
	if v := obj['error'] {
		if v is map[string]json2.Any {
			has_error = true
			mut code := 0
			if c := v['code'] {
				code = c.int()
			}
			mut message := ''
			if m := v['message'] {
				message = m.str()
			}
			mut data := ''
			if d := v['data'] {
				if d !is json2.Null {
					data = d.json_str()
				}
			}
			rpc_err = JsonRpcError{
				code:    code
				message: message
				data:    data
			}
		}
	}
	return IncomingMessage{
		id:        id
		method:    method
		params:    params
		result:    result
		error:     rpc_err
		has_error: has_error
	}
}

// send_request sends a JSON-RPC request and waits for the matching response.
// Server errors are returned as `RpcError`.
pub fn (mut t RpcTransport) send_request(method string, params string) !string {
	t.mu.lock()
	id := t.next_id
	t.next_id++
	ch := chan RpcReply{cap: 1}
	t.pending[id] = ch
	t.mu.unlock()

	t.write_message(encode_request(id, method, params))!

	reply := <-ch
	if reply.failed {
		return RpcError{
			error_code: reply.err.code
			message:    reply.err.message
			data:       reply.err.data
		}
	}
	return reply.result
}

// send_notification sends a JSON-RPC notification (fire-and-forget).
pub fn (mut t RpcTransport) send_notification(method string, params string) ! {
	t.write_message(encode_notification(method, params))!
}

// write_message frames a JSON payload with Content-Length headers and writes it.
fn (mut t RpcTransport) write_message(payload string) ! {
	t.write_mu.lock()
	defer {
		t.write_mu.unlock()
	}
	header := 'Content-Length: ${payload.len}\r\n\r\n'
	t.writer.write(header.bytes()) or { return error('write header failed: ${err}') }
	t.writer.write(payload.bytes()) or { return error('write body failed: ${err}') }
}

// read_message reads one Content-Length-framed JSON-RPC message.
pub fn (mut t RpcTransport) read_message() !string {
	// Read headers until empty line
	mut content_length := 0
	for {
		line := t.reader.read_line() or { return error('read header failed: ${err}') }
		trimmed := line.trim_space()
		if trimmed.len == 0 {
			break
		}
		if trimmed.starts_with('Content-Length:') {
			length_str := trimmed.all_after(':').trim_space()
			content_length = length_str.int()
		}
	}

	if content_length <= 0 {
		return error('invalid Content-Length: ${content_length}')
	}

	// Read exactly content_length bytes
	mut buf := []u8{len: content_length}
	mut read_so_far := 0
	for read_so_far < content_length {
		n := t.reader.read(mut buf[read_so_far..]) or {
			return error('read body failed: ${err}')
		}
		read_so_far += n
	}

	return buf.bytestr()
}

// dispatch_response routes a received response to its pending caller.
pub fn (mut t RpcTransport) dispatch_response(id int, result string, rpc_err JsonRpcError) {
	t.mu.lock()
	ch := t.pending[id] or {
		t.mu.unlock()
		return
	}
	t.pending.delete(id)
	t.mu.unlock()

	if rpc_err.code != 0 {
		ch <- RpcReply{
			err:    rpc_err
			failed: true
		}
	} else {
		ch <- RpcReply{
			result: result
		}
	}
}

// handle_incoming routes one decoded message: responses complete pending
// requests, requests are answered through `request_handler`, and
// notifications go to `notification_handler`.
pub fn (mut t RpcTransport) handle_incoming(msg IncomingMessage) {
	if msg.is_response() {
		mut rpc_err := msg.error
		if msg.has_error && rpc_err.code == 0 {
			rpc_err = JsonRpcError{
				code:    -32603
				message: rpc_err.message
				data:    rpc_err.data
			}
		}
		t.dispatch_response(msg.id.int(), msg.result, rpc_err)
		return
	}
	if msg.is_request() {
		if isnil(t.request_handler) {
			t.write_message(encode_error_response(msg.id, -32601, 'Method not found: ${msg.method}')) or {}
			return
		}
		result := t.request_handler(msg.method, msg.params) or {
			code := if err.code() != 0 { err.code() } else { -32603 }
			t.write_message(encode_error_response(msg.id, code, err.msg())) or {}
			return
		}
		t.write_message(encode_result_response(msg.id, result)) or {}
		return
	}
	if msg.method.len > 0 && !isnil(t.notification_handler) {
		t.notification_handler(msg.method, msg.params)
	}
}

// run reads and routes messages until the stream closes. The client starts
// it on a background thread after connecting.
pub fn (mut t RpcTransport) run() {
	for {
		raw := t.read_message() or { break }
		msg := decode_incoming(raw) or { continue }
		t.handle_incoming(msg)
	}
	t.fail_pending('connection closed')
}

// fail_pending fails every outstanding request, for example after disconnect.
pub fn (mut t RpcTransport) fail_pending(reason string) {
	t.mu.lock()
	ids := t.pending.keys()
	mut channels := []chan RpcReply{}
	for id in ids {
		channels << t.pending[id] or { continue }
	}
	t.pending.clear()
	t.mu.unlock()
	for ch in channels {
		ch <- RpcReply{
			err:    JsonRpcError{
				code:    -32000
				message: reason
			}
			failed: true
		}
	}
}

// parse_response_result checks a raw response string for an RPC error prefix.
pub fn parse_response_result(raw string) !string {
	if raw.starts_with('ERROR:') {
		parts := raw.split_nth(':', 3)
		code := if parts.len > 1 { parts[1] } else { '0' }
		msg := if parts.len > 2 { parts[2] } else { 'unknown error' }
		return error('JSON-RPC error ${code}: ${msg}')
	}
	return raw
}

// encode_params encodes a value as JSON for use in params.
pub fn encode_params(v string) string {
	return v
}

// build_params builds a JSON object string from key-value pairs. Values are
// raw JSON fragments; keys may be given with or without surrounding quotes.
pub fn build_params(pairs map[string]string) string {
	mut sb := strings.new_builder(256)
	sb.write_string('{')
	mut first := true
	for k, v in pairs {
		if !first {
			sb.write_string(',')
		}
		key := if k.starts_with('"') && k.ends_with('"') && k.len >= 2 { k } else { '"${k}"' }
		sb.write_string('${key}:${v}')
		first = false
	}
	sb.write_string('}')
	return sb.str()
}

// json_str encodes a V string as a JSON string literal.
pub fn json_str(s string) string {
	return json.encode(s)
}
