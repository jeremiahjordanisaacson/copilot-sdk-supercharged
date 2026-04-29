// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - JSON-RPC 2.0 transport layer

module copilot

import json
import io
import strings
import sync

// JsonRpcRequest is a JSON-RPC 2.0 request object.
pub struct JsonRpcRequest {
pub:
	jsonrpc string         [json: 'jsonrpc'] // always "2.0"
	id      int            [json: 'id']
	method  string         [json: 'method']
	params  string         [json: 'params']  // raw JSON params
}

// JsonRpcResponse is a JSON-RPC 2.0 response object.
pub struct JsonRpcResponse {
pub:
	jsonrpc string         [json: 'jsonrpc']
	id      int            [json: 'id']
	result  string         [json: 'result']  // raw JSON result
	error   JsonRpcError   [json: 'error']
}

// JsonRpcError represents the error field in a JSON-RPC response.
pub struct JsonRpcError {
pub:
	code    int    [json: 'code']
	message string [json: 'message']
	data    string [json: 'data']
}

// JsonRpcNotification is a JSON-RPC notification (no id, no response expected).
pub struct JsonRpcNotification {
pub:
	jsonrpc string [json: 'jsonrpc']
	method  string [json: 'method']
	params  string [json: 'params']
}

// RpcTransport manages sending and receiving JSON-RPC messages over a stream
// with Content-Length header framing (LSP-style).
pub struct RpcTransport {
mut:
	reader   io.BufferedReader
	writer   io.Writer
	next_id  int = 1
	mu       sync.Mutex
	pending  map[int]chan string
}

// new_rpc_transport creates a transport from a reader/writer pair.
pub fn new_rpc_transport(reader io.Reader, writer io.Writer) &RpcTransport {
	return &RpcTransport{
		reader: io.new_buffered_reader(reader: reader)
		writer: writer
		mu: sync.new_mutex()
		pending: map[int]chan string{}
	}
}

// send_request sends a JSON-RPC request and waits for the matching response.
pub fn (mut t RpcTransport) send_request(method string, params string) !string {
	t.mu.@lock()
	id := t.next_id
	t.next_id++
	ch := chan string{cap: 1}
	t.pending[id] = ch
	t.mu.unlock()

	req := JsonRpcRequest{
		jsonrpc: '2.0'
		id: id
		method: method
		params: params
	}
	t.write_message(json.encode(req))!

	// Wait for the response on the channel
	result := <-ch
	return result
}

// send_notification sends a JSON-RPC notification (fire-and-forget).
pub fn (mut t RpcTransport) send_notification(method string, params string) ! {
	notif := JsonRpcNotification{
		jsonrpc: '2.0'
		method: method
		params: params
	}
	t.write_message(json.encode(notif))!
}

// write_message frames a JSON payload with Content-Length headers and writes it.
fn (mut t RpcTransport) write_message(payload string) ! {
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
	t.mu.@lock()
	ch := t.pending[id] or {
		t.mu.unlock()
		return
	}
	t.pending.delete(id)
	t.mu.unlock()

	if rpc_err.code != 0 {
		ch <- 'ERROR:${rpc_err.code}:${rpc_err.message}'
	} else {
		ch <- result
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

// build_params builds a JSON object string from key-value pairs.
pub fn build_params(pairs map[string]string) string {
	mut sb := strings.new_builder(256)
	sb.write_string('{')
	mut first := true
	for k, v in pairs {
		if !first {
			sb.write_string(',')
		}
		sb.write_string('"${k}":${v}')
		first = false
	}
	sb.write_string('}')
	return sb.str()
}
