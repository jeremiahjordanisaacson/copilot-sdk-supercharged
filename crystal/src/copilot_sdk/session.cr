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
    # Invoked once when the session is detached or destroyed (set by the client).
    property on_close : Proc(Nil)? = nil

    @rpc : JsonRpcClient
    @event_handlers : Hash(String, SessionEventHandler)
    @tool_handlers : Hash(String, ToolHandler)
    @tool_definitions : Array(ToolDefinition)
    @permission_handler : PermissionHandler?
    @exit_plan_mode_handler : ExitPlanModeHandler?
    @trace_context_provider : TraceContextProvider?
    @hooks : SessionHooks?
    @shell_authentication_handler : ShellAuthenticationHandler?
    @user_input_handler : UserInputHandler?
    @memory_store_handler : MemoryStoreHandler?
    @pending_shell_authentications : Hash(String, Channel(ShellAuthenticationResponse))
    @mutex : Mutex
    @destroyed : Bool
    @next_handler_id : Int64

    def initialize(@session_id, @rpc, @model = nil, @trace_context_provider = nil)
      @event_handlers = {} of String => SessionEventHandler
      @tool_handlers = {} of String => ToolHandler
      @tool_definitions = [] of ToolDefinition
      @permission_handler = nil
      @exit_plan_mode_handler = nil
      @hooks = nil
      @shell_authentication_handler = nil
      @user_input_handler = nil
      @memory_store_handler = nil
      @pending_shell_authentications = {} of String => Channel(ShellAuthenticationResponse)
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
          json.field "agentMode", options.agent_mode if options.agent_mode
          json.field "displayPrompt", options.display_prompt if options.display_prompt
          json.field "requestHeaders", options.request_headers if options.request_headers
          json.field "source", options.source if options.source
          json.field "clientTurnId", options.client_turn_id if options.client_turn_id
          if schema = options.response_schema
            # Structured output: the final assistant response must match `schema`.
            json.field "responseFormat" do
              json.object do
                json.field "type", "json_schema"
                json.field "jsonSchema" do
                  json.object do
                    json.field "name", "response"
                    json.field "strict", true
                    json.field "schema", schema
                  end
                end
              end
            end
          end
          json.field "tools", @tool_definitions if @tool_definitions.any?

          trace = get_trace_context
          trace.each { |k, v| json.field k, v }
        end
      })

      @rpc.send_request("session.send", params)
    end

    # Send a message and wait for the final assistant.message event.
    # Collects all events and returns the final message content, or nil on
    # timeout. Completes on `session.idle` (or the legacy `turn.end`).
    def send_and_wait(options : MessageOptions, timeout : Int32 = 60) : String?
      result_channel = Channel(String?).new(1)
      collected_content = ""

      sub_id = on("assistant.message") do |event|
        if content = event.assistant_message_content
          collected_content = content
        end
      end

      finish = ->(_event : SessionEvent) do
        select
        when result_channel.send(collected_content.empty? ? nil : collected_content)
        else
        end
        nil
      end
      done_id = on("turn.end") { |event| finish.call(event) }
      idle_id = on("session.idle") { |event| finish.call(event) }

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
        off(idle_id)
      end
    end

    # Release this session without deleting its on-disk state
    # (`session.detach`), so it can be resumed later with
    # `CopilotClient#resume_session`. Falls back to the legacy
    # `session.destroy` on runtimes without `session.detach`.
    def detach : Nil
      return if @destroyed

      params = JSON::Any.new({"sessionId" => JSON::Any.new(@session_id)})
      response = begin
        @rpc.send_request("session.detach", params)
      rescue ex : JsonRpcError
        raise ex unless ex.method_not_found?
        @rpc.send_request("session.destroy", params)
      end
      if response.as_h?.try(&.["success"]?).try(&.as_bool?) == false
        message = response["error"]?.try(&.as_s?) || "Unknown error"
        raise CopilotError.new("Failed to detach session #{@session_id}: #{message}")
      end
      release
    end

    # Destroy this session, releasing server-side resources. Best-effort
    # `detach`: the on-disk state is kept, so the session can be resumed.
    def destroy : Nil
      return if @destroyed

      begin
        detach
      rescue
        # Best-effort cleanup
      end

      release
    end

    # Returns true if this session has been destroyed.
    def destroyed? : Bool
      @destroyed
    end

    # Change the Auto routing preference without changing the session's model
    # (`session.model.switchAutoTier`). `tier` is one of `AutoTier` (`fast`,
    # `efficiency`, `balance`, `intelligence`), or nil for the provider's
    # default Auto routing. Requires the session model to be `auto`.
    def switch_auto_tier(tier : String?) : JSON::Any
      @rpc.send_request("session.model.switchAutoTier", JSON::Any.new({
        "sessionId" => JSON::Any.new(@session_id),
        "autoTier"  => JSON::Any.new(tier),
      }))
    end

    # Re-run the managed-settings Policy Helper for this session
    # (`session.managedSettings.recheckPolicyHelper`). Returns the managed
    # settings snapshot; when the helper still fails it carries
    # `policyHelperFailure` (parse it with `PolicyHelperFailure.from_json`).
    def recheck_policy_helper : JSON::Any
      @rpc.send_request("session.managedSettings.recheckPolicyHelper",
        JSON::Any.new({"sessionId" => JSON::Any.new(@session_id)}))
    end

    # Set the permission mode (`session.permissions.setMode`), for example
    # `"manual"`, `"assisted"`, or `"allow-all"`. A refused mode returns
    # `success == false` with a `reason` such as
    # `PermissionModeRefusalReason::MANAGED_POLICY`.
    def set_permission_mode(mode : String) : PermissionModeSetResult
      result = @rpc.send_request("session.permissions.setMode", JSON::Any.new({
        "sessionId" => JSON::Any.new(@session_id),
        "mode"      => JSON::Any.new(mode),
      }))
      return PermissionModeSetResult.new(false, mode) unless result.as_h?
      PermissionModeSetResult.from_json(result.to_json)
    end

    # Subscribe to live shell output of running tools (`tool.shell_output`
    # events). Returns a subscription ID for `#off`.
    def on_shell_output(&handler : ToolShellOutputData -> Nil) : String
      on(SessionEventTypes::TOOL_SHELL_OUTPUT) do |event|
        if data = ToolShellOutputData.from_event(event)
          handler.call(data)
        end
        nil
      end
    end

    # Register the session hook handlers (called by CopilotClient).
    def register_hooks(hooks : SessionHooks?) : Nil
      @hooks = hooks
    end

    # Answer a `hooks.invoke` request; returns the hook output, or nil.
    def handle_hooks_invoke(hook_type : String, input : JSON::Any) : JSON::Any?
      hooks = @hooks
      return nil unless hooks

      case hook_type
      when "subagentStart"
        if handler = hooks.on_subagent_start
          output = handler.call(SubagentStartHookInput.from_json(input.to_json), @session_id)
          return output ? JSON.parse(output.to_json) : nil
        end
      when "subagentStop"
        if handler = hooks.on_subagent_stop
          output = handler.call(SubagentStopHookInput.from_json(input.to_json), @session_id)
          return output ? JSON.parse(output.to_json) : nil
        end
      end

      if handler = hooks.handlers[hook_type]?
        return handler.call(input, @session_id)
      end
      nil
    end

    # Register the shell authentication handler (called by CopilotClient).
    def register_shell_authentication_handler(handler : ShellAuthenticationHandler?) : Nil
      @shell_authentication_handler = handler
    end

    # Answer a `shellAuthentication.request`. Resolves to cancel when no handler
    # is registered, the handler raises, or the runtime cancels the request first.
    def handle_shell_authentication_request(request : ShellAuthenticationRequest) : ShellAuthenticationResponse
      handler = @shell_authentication_handler
      return ShellAuthenticationResponse.cancel if handler.nil? || @destroyed || request.request_id.empty?

      channel = Channel(ShellAuthenticationResponse).new(1)
      @mutex.synchronize { @pending_shell_authentications[request.request_id] = channel }
      spawn do
        answer = begin
          handler.call(request)
        rescue
          ShellAuthenticationResponse.cancel
        end
        select
        when channel.send(answer)
        else
        end
      end

      response = channel.receive
      @mutex.synchronize do
        if @pending_shell_authentications[request.request_id]? == channel
          @pending_shell_authentications.delete(request.request_id)
        end
      end
      response
    end

    # Handle a `shellAuthentication.cancel`: the pending request resolves to
    # cancel and a later answer from the handler is ignored.
    def cancel_shell_authentication_request(request_id : String) : Nil
      channel = @mutex.synchronize { @pending_shell_authentications.delete(request_id) }
      return unless channel
      select
      when channel.send(ShellAuthenticationResponse.cancel)
      else
      end
    end

    # Register the app-side memory store (called by CopilotClient).
    def register_memory_store(handler : MemoryStoreHandler?) : Nil
      @memory_store_handler = handler
    end

    # Register the ask_user handler (called by CopilotClient).
    def register_user_input_handler(handler : UserInputHandler?) : Nil
      @user_input_handler = handler
    end

    # Answer a `userInput.request` with the registered handler. The request
    # carries `image_attachment_constraints` when the runtime accepts image
    # answers; the response's `attachments` are sent only when set.
    def handle_user_input_request(request : UserInputRequest) : UserInputResponse
      handler = @user_input_handler
      raise CopilotError.new("User input requested but no handler registered") if handler.nil? || @destroyed
      handler.call(request)
    end

    # Store a memory with the registered handler (`memoryStore.store`).
    def handle_memory_store(memory : StoredMemory) : Nil
      handler = @memory_store_handler
      raise CopilotError.new("Memory store owner is unavailable") if handler.nil? || @destroyed
      handler.call(memory)
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

    # Clear local state, resolve pending prompts, and notify the owning client.
    private def release : Nil
      return if @destroyed
      @destroyed = true
      pending = @mutex.synchronize do
        @event_handlers.clear
        @tool_handlers.clear
        channels = @pending_shell_authentications.values
        @pending_shell_authentications.clear
        channels
      end
      pending.each do |channel|
        select
        when channel.send(ShellAuthenticationResponse.cancel)
        else
        end
      end
      @hooks = nil
      @shell_authentication_handler = nil
      @user_input_handler = nil
      @memory_store_handler = nil
      if close = @on_close
        @on_close = nil
        close.call
      end
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
