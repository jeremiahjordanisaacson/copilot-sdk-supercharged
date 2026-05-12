# Copyright (c) Microsoft Corporation. All rights reserved.

# CopilotSession - represents a single conversation session with the Copilot CLI.
#
# Sessions are created via `CopilotClient#create_session` or resumed via
# `CopilotClient#resume_session`. They maintain conversation state, handle events,
# and manage tool execution.

require "json"
require "uuid"

module CopilotSDK
  # Represents a single conversation session with the Copilot CLI.
  #
  # Each session maintains its own event handlers, tool registrations,
  # and conversation state. Sessions communicate with the server through
  # the shared `JsonRpcClient` owned by the parent `CopilotClient`.
  class CopilotSession
    getter session_id : String
    getter model : String?

    @rpc : JsonRpcClient
    @event_handlers : Hash(String, SessionEventHandler)
    @tool_handlers : Hash(String, ToolHandler)
    @tool_definitions : Array(ToolDefinition)
    @permission_handler : PermissionHandler?
    @exit_plan_mode_handler : ExitPlanModeHandler?
    @trace_context_provider : TraceContextProvider?
    @mutex : Mutex
    @destroyed : Bool
    @next_handler_id : Int64

    def initialize(@session_id, @rpc, @model = nil, @trace_context_provider = nil)
      @event_handlers = {} of String => SessionEventHandler
      @tool_handlers = {} of String => ToolHandler
      @tool_definitions = [] of ToolDefinition
      @permission_handler = nil
      @exit_plan_mode_handler = nil
      @mutex = Mutex.new
      @destroyed = false
      @next_handler_id = 0_i64
    end

    # Register a handler for all session events. Returns a subscription ID
    # that can be passed to `#off` to unsubscribe.
    def on(&handler : SessionEvent -> Nil) : String
      id = @mutex.synchronize do
        @next_handler_id += 1
        "handler_#{@next_handler_id}"
      end
      @mutex.synchronize { @event_handlers[id] = handler }
      id
    end

    # Register a handler that only fires for a specific event type.
    # Returns a subscription ID for unsubscribing.
    def on(event_type : String, &handler : SessionEvent -> Nil) : String
      captured_handler = handler
      wrapped : SessionEventHandler = ->(event : SessionEvent) : Nil {
        captured_handler.call(event) if event.type == event_type
      }
      id = @mutex.synchronize do
        @next_handler_id += 1
        "handler_#{@next_handler_id}"
      end
      @mutex.synchronize { @event_handlers[id] = wrapped }
      id
    end

    # Remove a previously registered event handler by subscription ID.
    def off(subscription_id : String) : Nil
      @mutex.synchronize { @event_handlers.delete(subscription_id) }
    end

    # Register a tool by providing a ToolDefinition and its handler.
    def add_tool(definition : ToolDefinition, &handler : JSON::Any, ToolInvocation -> JSON::Any) : Nil
      @mutex.synchronize do
        @tool_definitions << definition
        @tool_handlers[definition.name] = handler
      end
    end

    # Register a tool using the builder DSL.
    def define_tool(name : String, &block : ToolBuilder ->) : Nil
      defn, handler = CopilotSDK.define_tool(name) { |t| yield t }
      @mutex.synchronize do
        @tool_definitions << defn
        @tool_handlers[defn.name] = handler
      end
    end

    # Set the permission handler for this session.
    def on_permission(&handler : PermissionRequest, String -> PermissionRequestResult) : Nil
      @permission_handler = handler
    end

    # Set the exit plan mode handler for this session.
    def on_exit_plan_mode(&handler : ExitPlanModeRequest, String -> ExitPlanModeResult) : Nil
      @exit_plan_mode_handler = handler
    end

    # Handle an incoming exit-plan-mode request from the server.
    def handle_exit_plan_mode(request : ExitPlanModeRequest) : ExitPlanModeResult
      if handler = @exit_plan_mode_handler
        handler.call(request, @session_id)
      else
        ExitPlanModeResult.new(approved: true)
      end
    end

    # Send a message to the session and return the raw response.
    def send(options : MessageOptions) : JSON::Any
      raise "Session has been destroyed" if @destroyed

      params = JSON.parse(JSON.build { |json|
        json.object do
          json.field "sessionId", @session_id
          json.field "prompt", options.prompt
          json.field "attachments", options.attachments if options.attachments
          json.field "mode", options.mode if options.mode
          json.field "tools", @tool_definitions if @tool_definitions.any?

          trace = get_trace_context
          trace.each { |k, v| json.field k, v }
        end
      })

      @rpc.send_request("session/send", params)
    end

    # Send a message and wait for the final assistant.message event.
    # Collects all events and returns the final message content, or nil on timeout.
    def send_and_wait(options : MessageOptions, timeout : Int32 = 60) : String?
      result_channel = Channel(String?).new(1)
      collected_content = ""

      sub_id = on("assistant.message") do |event|
        if content = event.assistant_message_content
          collected_content = content
        end
      end

      done_id = on("turn.end") do |_event|
        result_channel.send(collected_content.empty? ? nil : collected_content)
      end

      begin
        send(options)

        select
        when result = result_channel.receive
          result
        when timeout(timeout.seconds)
          nil
        end
      ensure
        off(sub_id)
        off(done_id)
      end
    end

    # Destroy this session, releasing server-side resources.
    def destroy : Nil
      return if @destroyed

      begin
        params = JSON.parse(%Q({"sessionId": "#{@session_id}"}))
        @rpc.send_request("session/destroy", params)
      rescue
        # Best-effort cleanup
      end

      @destroyed = true
      @mutex.synchronize do
        @event_handlers.clear
        @tool_handlers.clear
      end
    end

    # Returns true if this session has been destroyed.
    def destroyed? : Bool
      @destroyed
    end

    # Dispatch an event to all registered handlers.
    # Called internally by CopilotClient when events arrive.
    def dispatch_event(event : SessionEvent) : Nil
      handlers = @mutex.synchronize { @event_handlers.values.dup }
      handlers.each do |handler|
        begin
          handler.call(event)
        rescue ex
          STDERR.puts "Event handler error: #{ex.message}"
        end
      end
    end

    # Handle an incoming tool call from the server.
    # Returns the tool result, or an error if the tool is not registered.
    def handle_tool_call(payload : ToolCallRequestPayload) : JSON::Any
      handler = @mutex.synchronize { @tool_handlers[payload.tool_name]? }

      unless handler
        return JSON.parse(%Q({"error": "Unknown tool: #{payload.tool_name}"}))
      end

      invocation = ToolInvocation.new(
        session_id: payload.session_id,
        tool_call_id: payload.tool_call_id,
        tool_name: payload.tool_name,
        arguments: payload.arguments
      )

      begin
        handler.call(payload.arguments, invocation)
      rescue ex
        JSON.parse(%Q({"error": "Tool execution failed: #{ex.message}"}))
      end
    end

    # Handle an incoming permission request from the server.
    def handle_permission_request(request : PermissionRequest) : PermissionRequestResult
      if handler = @permission_handler
        handler.call(request, @session_id)
      else
        CopilotSDK.deny_all_permissions(request, @session_id)
      end
    end

    # Returns the list of currently registered tool definitions.
    def tool_definitions : Array(ToolDefinition)
      @mutex.synchronize { @tool_definitions.dup }
    end

    private def get_trace_context : Hash(String, String)
      if provider = @trace_context_provider
        begin
          ctx = provider.call
          result = {} of String => String
          result["traceparent"] = ctx.traceparent.not_nil! if ctx.traceparent
          result["tracestate"] = ctx.tracestate.not_nil! if ctx.tracestate
          result
        rescue
          {} of String => String
        end
      else
        {} of String => String
      end
    end
  end
end
