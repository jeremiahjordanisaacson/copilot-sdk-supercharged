# Copyright (c) Microsoft Corporation. All rights reserved.

# JSON-RPC 2.0 implementation for the Copilot SDK.
#
# Handles Content-Length header framing (LSP protocol style), request/response
# matching, notification dispatch, and method routing over IO streams.

require "json"
require "uuid"

module CopilotSDK
  # Error raised for JSON-RPC protocol-level failures.
  class JsonRpcError < Exception
    getter code : Int32
    getter data : JSON::Any?

    def initialize(message : String, @code = -32603, @data = nil)
      super(message)
    end
  end

  # A pending request waiting for its response.
  private record PendingRequest,
    channel : Channel(JSON::Any | Exception)

  # JSON-RPC 2.0 client for communicating over IO streams with
  # Content-Length header framing.
  class JsonRpcClient
    @reader : IO
    @writer : IO
    @pending : Hash(String, PendingRequest)
    @request_handlers : Hash(String, Proc(JSON::Any, JSON::Any))
    @notification_handlers : Hash(String, Proc(JSON::Any, Nil))
    @mutex : Mutex
    @running : Bool
    @read_fiber : Fiber?

    def initialize(@reader : IO, @writer : IO)
      @pending = {} of String => PendingRequest
      @request_handlers = {} of String => Proc(JSON::Any, JSON::Any)
      @notification_handlers = {} of String => Proc(JSON::Any, Nil)
      @mutex = Mutex.new
      @running = false
    end

    # Register a handler for incoming JSON-RPC requests (expects a response).
    def on_request(method : String, &handler : JSON::Any -> JSON::Any) : Nil
      @request_handlers[method] = handler
    end

    # Register a handler for incoming JSON-RPC notifications (no response).
    def on_notification(method : String, &handler : JSON::Any -> Nil) : Nil
      @notification_handlers[method] = handler
    end

    # Start the read loop that processes incoming messages.
    def start : Nil
      @running = true
      @read_fiber = spawn { read_loop }
    end

    # Stop the read loop and reject all pending requests.
    def stop : Nil
      @running = false
      @mutex.synchronize do
        @pending.each_value do |req|
          req.channel.send(JsonRpcError.new("Connection closed"))
        end
        @pending.clear
      end
    end

    # Send a JSON-RPC request and wait for the response.
    def send_request(method : String, params : JSON::Any? = nil, timeout : Int32 = 30) : JSON::Any
      id = UUID.random.to_s
      ch = Channel(JSON::Any | Exception).new(1)
      pending_req = PendingRequest.new(channel: ch)

      @mutex.synchronize { @pending[id] = pending_req }

      message = JSON.build do |json|
        json.object do
          json.field "jsonrpc", "2.0"
          json.field "id", id
          json.field "method", method
          json.field "params", params if params
        end
      end

      write_message(message)

      select
      when result = ch.receive
        if result.is_a?(Exception)
          raise result
        end
        result
      when timeout(timeout.seconds)
        @mutex.synchronize { @pending.delete(id) }
        raise JsonRpcError.new("Request timed out after #{timeout}s: #{method}")
      end
    end

    # Send a JSON-RPC notification (no response expected).
    def send_notification(method : String, params : JSON::Any? = nil) : Nil
      message = JSON.build do |json|
        json.object do
          json.field "jsonrpc", "2.0"
          json.field "method", method
          json.field "params", params if params
        end
      end

      write_message(message)
    end

    # Returns true if the read loop is active.
    def running? : Bool
      @running
    end

    # --------------------------------------------------------------------------
    # Private
    # --------------------------------------------------------------------------

    private def write_message(body : String) : Nil
      header = "Content-Length: #{body.bytesize}\r\n\r\n"
      @mutex.synchronize do
        @writer.print(header)
        @writer.print(body)
        @writer.flush
      end
    end

    private def read_message : String?
      # Read Content-Length header
      content_length = -1
      loop do
        line = @reader.gets
        return nil if line.nil?
        line = line.chomp
        break if line.empty?
        if line.starts_with?("Content-Length:")
          content_length = line.split(":").last.strip.to_i
        end
      end

      return nil if content_length < 0

      # Read exactly content_length bytes
      buffer = Bytes.new(content_length)
      bytes_read = 0
      while bytes_read < content_length
        n = @reader.read(buffer[bytes_read..])
        return nil if n == 0
        bytes_read += n
      end

      String.new(buffer)
    end

    private def read_loop : Nil
      while @running
        raw = read_message
        break if raw.nil?

        begin
          msg = JSON.parse(raw)
          handle_message(msg)
        rescue ex
          STDERR.puts "JSON-RPC parse error: #{ex.message}"
        end
      end
    end

    private def handle_message(msg : JSON::Any) : Nil
      if msg["id"]?
        if msg["method"]?
          # Incoming request
          handle_incoming_request(msg)
        else
          # Response to our request
          handle_response(msg)
        end
      elsif msg["method"]?
        # Notification
        handle_notification(msg)
      end
    end

    private def handle_response(msg : JSON::Any) : Nil
      id = msg["id"].as_s
      pending_req = @mutex.synchronize { @pending.delete(id) }
      return unless pending_req

      if error = msg["error"]?
        code = error["code"]?.try(&.as_i) || -32603
        message = error["message"]?.try(&.as_s) || "Unknown error"
        data = error["data"]?
        pending_req.channel.send(JsonRpcError.new(message, code, data))
      else
        result = msg["result"]? || JSON::Any.new(nil)
        pending_req.channel.send(result)
      end
    end

    private def handle_incoming_request(msg : JSON::Any) : Nil
      id = msg["id"]
      method = msg["method"].as_s
      params = msg["params"]? || JSON::Any.new(nil)

      handler = @request_handlers[method]?

      response = if handler
                   begin
                     result = handler.call(params)
                     JSON.build do |json|
                       json.object do
                         json.field "jsonrpc", "2.0"
                         json.field "id", id
                         json.field "result", result
                       end
                     end
                   rescue ex
                     JSON.build do |json|
                       json.object do
                         json.field "jsonrpc", "2.0"
                         json.field "id", id
                         json.field "error" do
                           json.object do
                             json.field "code", -32603
                             json.field "message", ex.message || "Internal error"
                           end
                         end
                       end
                     end
                   end
                 else
                   JSON.build do |json|
                     json.object do
                       json.field "jsonrpc", "2.0"
                       json.field "id", id
                       json.field "error" do
                         json.object do
                           json.field "code", -32601
                           json.field "message", "Method not found: #{method}"
                         end
                       end
                     end
                   end
                 end

      write_message(response)
    end

    private def handle_notification(msg : JSON::Any) : Nil
      method = msg["method"].as_s
      params = msg["params"]? || JSON::Any.new(nil)

      if handler = @notification_handlers[method]?
        spawn { handler.call(params) }
      end
    end
  end
end
