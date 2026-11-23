/// Copilot session management.
///
/// A `CopilotSession` wraps a single conversation with the assistant. It
/// holds registered tools, dispatches events, and exposes `send` /
/// `sendAndWait` for the request-response lifecycle.
module copilot.session;

import std.json;
import std.typecons : Nullable;
import std.algorithm : filter, map;
import std.array : array;
import std.exception : enforce;
import std.string : strip;
import core.sync.mutex : Mutex;
import core.sync.condition : Condition;

import copilot.types;
import copilot.jsonrpc : JsonRpcClient, JsonRpcError, methodNotFoundCode;
import copilot.tools : ToolBinding, ToolHandler;

/// Callback signature for session event listeners.
alias EventListener = void delegate(SessionEvent event) @safe;

/// Callback for exit-plan-mode requests.
alias ExitPlanModeHandler = ExitPlanModeResponse delegate(ExitPlanModeRequest req) @safe;

/// Represents a single conversation session with the Copilot assistant.
final class CopilotSession
{
    private
    {
        string _sessionId;
        JsonRpcClient _rpc;
        ToolHandler[string] _toolHandlers;
        EventListener[] _listeners;
        Mutex _idleMutex;
        Condition _idleCond;
        bool _idle = true;
        bool _stopped = false;
        ExitPlanModeHandler _exitPlanModeHandler;
        SubagentStartHandler _onSubagentStart;
        SubagentStopHandler _onSubagentStop;
        ShellAuthenticationHandler _onShellAuthentication;
        StoreMemoryHandler _onStoreMemory;
        void delegate(string sessionId) _onRelease;
    }

    /// Construct a session (called internally by CopilotClient).
    package this(string sessionId, JsonRpcClient rpc) @trusted
    {
        _sessionId = sessionId;
        _rpc = rpc;
        _idleMutex = new Mutex();
        _idleCond = new Condition(_idleMutex);
    }

    // ------------------------------------------------------------------
    // Properties
    // ------------------------------------------------------------------

    /// The unique session identifier.
    @property string sessionId() const @safe pure nothrow { return _sessionId; }

    /// Whether the session is currently idle (not waiting for a response).
    @property bool isIdle() @trusted
    {
        _idleMutex.lock();
        scope(exit) _idleMutex.unlock();
        return _idle;
    }

    // ------------------------------------------------------------------
    // Event handling
    // ------------------------------------------------------------------

    /// Register a listener that will be called for every session event.
    /// Returns an opaque index that can be used to remove the listener.
    size_t addEventListener(EventListener listener) @safe
    {
        _listeners ~= listener;
        return _listeners.length - 1;
    }

    /// Dispatch an event to all registered listeners.
    package void dispatchEvent(SessionEvent event) @safe
    {
        // Track idle state.
        if (event.type == SessionEventType.sessionIdle)
        {
            () @trusted {
                _idleMutex.lock();
                scope(exit) _idleMutex.unlock();
                _idle = true;
                _idleCond.notifyAll();
            }();
        }

        // Auto-handle tool calls.
        if (event.type == SessionEventType.toolCall)
            _handleToolCall(event.payload);

        foreach (listener; _listeners)
        {
            try { listener(event); } catch (Exception) {}
        }
    }

    // ------------------------------------------------------------------
    // Sending messages
    // ------------------------------------------------------------------

    /// Send a user message without waiting for the full response.
    void send(string message, MessageOptions opts = MessageOptions.init) @trusted
    {
        _idleMutex.lock();
        _idle = false;
        _idleMutex.unlock();

        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        params["prompt"] = message;

        if (opts.attachments.length > 0)
            params["attachments"] = JSONValue(opts.attachments);
        // The legacy string `responseFormat` is no longer sent: protocol 4
        // only accepts the structured `responseFormat` built from `responseSchema`.
        if (!opts.agentMode.isNull)
            params["agentMode"] = opts.agentMode.get;
        if (!opts.displayPrompt.isNull)
            params["displayPrompt"] = opts.displayPrompt.get;
        if (opts.requestHeaders.length > 0)
        {
            auto hdrObj = JSONValue(string[string].init);
            foreach (key, val; opts.requestHeaders)
                hdrObj[key] = val;
            params["requestHeaders"] = hdrObj;
        }
        if (!opts.source.isNull)
            params["source"] = opts.source.get;
        if (!opts.clientTurnId.isNull)
            params["clientTurnId"] = opts.clientTurnId.get;
        if (opts.responseSchema.type != JSONType.null_)
        {
            // Structured output: the protocol 4 `responseFormat` object.
            auto jsonSchema = JSONValue(string[string].init);
            jsonSchema["name"] = "response";
            jsonSchema["strict"] = true;
            jsonSchema["schema"] = opts.responseSchema;
            auto structured = JSONValue(string[string].init);
            structured["type"] = "json_schema";
            structured["jsonSchema"] = jsonSchema;
            params["responseFormat"] = structured;
        }

        try
        {
            _rpc.request("session.send", params);
        }
        catch (Exception e)
        {
            _idleMutex.lock();
            _idle = true;
            _idleCond.notifyAll();
            _idleMutex.unlock();
            throw e;
        }
    }

    /// Send a user message and block until the session becomes idle.
    /// Returns all events collected during the exchange.
    SessionEvent[] sendAndWait(string message, MessageOptions opts = MessageOptions.init) @trusted
    {
        SessionEvent[] collected;

        auto idx = addEventListener((SessionEvent ev) @safe {
            collected ~= ev;
        });

        send(message, opts);

        // Block until idle.
        _idleMutex.lock();
        scope(exit) _idleMutex.unlock();
        while (!_idle && !_stopped)
            _idleCond.wait();

        return collected;
    }

    /// Abort the current in-flight request (`session.abort`). A refusal from
    /// the runtime surfaces as a `JsonRpcError`.
    void abort() @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        _rpc.request("session.abort", params);
    }

    /// Send a message requesting structured output that matches `schema`,
    /// block until the session is idle, and return the final assistant
    /// content parsed as JSON. Throws if no structured content was produced.
    JSONValue sendAndWaitStructured(string message, JSONValue schema,
        MessageOptions opts = MessageOptions.init) @trusted
    {
        opts.responseSchema = schema;
        auto events = sendAndWait(message, opts);

        string content;
        foreach (ref ev; events)
        {
            if (ev.type != SessionEventType.assistantMessage) continue;
            auto c = assistantContent(ev);
            if (c.length > 0) content = c;
        }
        enforce(content.strip.length > 0,
            "The requested run completed without a structured assistant response.");
        return parseJSON(content);
    }

    /// Release this session via `session.detach`, falling back to
    /// `session.destroy` on runtimes without detach support. Session state
    /// on disk is preserved, so the session can be resumed later.
    void disconnect() @trusted
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        try
        {
            _rpc.request("session.detach", params);
        }
        catch (JsonRpcError e)
        {
            if (e.code != methodNotFoundCode) throw e;
            _rpc.request("session.destroy", params);
        }
        if (_onRelease !is null)
            _onRelease(_sessionId);
        markStopped();
    }

    /// Change the Auto routing preference (experimental) without changing the
    /// model (`session.model.switchAutoTier`). A null tier returns to the
    /// provider's default routing. Returns the runtime's acknowledgement.
    JSONValue setAutoTier(Nullable!string autoTier) @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        params["autoTier"] = autoTier.isNull ? JSONValue(null) : JSONValue(autoTier.get);
        return _rpc.request("session.model.switchAutoTier", params);
    }

    /// ditto
    JSONValue setAutoTier(string autoTier) @safe
    {
        return setAutoTier(autoTier is null ? Nullable!string.init : Nullable!string(autoTier));
    }

    /// Get the managed settings in effect for this session
    /// (`session.managedSettings.get`).
    JSONValue getManagedSettings() @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        return _rpc.request("session.managedSettings.get", params);
    }

    /// Re-run the managed-settings policy helper for this session
    /// (`session.managedSettings.recheckPolicyHelper`).
    JSONValue recheckPolicyHelper() @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        return _rpc.request("session.managedSettings.recheckPolicyHelper", params);
    }

    /// Detach from the session (`session.detach`) without deleting it; the
    /// same as `disconnect`. The session can be resumed later.
    void detach() @trusted
    {
        disconnect();
    }

    /// Set the session permission mode (`session.permissions.setMode`). When
    /// the runtime refuses the mode, `reason` explains why (for example
    /// `permissionModeRefusalManagedPolicy`).
    PermissionModeResult setPermissionMode(string mode) @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        params["mode"] = mode;
        return PermissionModeResult.fromJson(_rpc.request("session.permissions.setMode", params));
    }

    // ------------------------------------------------------------------
    // Tool registration
    // ------------------------------------------------------------------

    /// Register a tool binding so the session can auto-handle calls.
    void registerTool(ToolBinding binding) @safe
    {
        _toolHandlers[binding.tool.name] = binding.handler;
    }

    /// Register a handler for exit-plan-mode requests.
    void registerExitPlanModeHandler(ExitPlanModeHandler handler) @safe
    {
        _exitPlanModeHandler = handler;
    }

    /// Register multiple tool bindings.
    void registerTools(ToolBinding[] bindings) @safe
    {
        foreach (ref b; bindings)
            registerTool(b);
    }

    // ------------------------------------------------------------------
    // Internal
    // ------------------------------------------------------------------

    /// Handle an incoming tool call by invoking the registered handler.
    private void _handleToolCall(JSONValue payload) @safe
    {
        auto pName = "name" in payload;
        auto pId = "id" in payload;
        auto pArgs = "arguments" in payload;

        if (pName is null || pId is null) return;

        string name = (*pName).type == JSONType.string ? (*pName).str : "";
        string callId = (*pId).type == JSONType.string ? (*pId).str : "";
        JSONValue args = (pArgs !is null) ? *pArgs : JSONValue(null);

        auto handler = name in _toolHandlers;
        ToolResult res;

        if (handler is null)
        {
            res = ToolResult(callId, "unknown tool: " ~ name, true);
        }
        else
        {
            try
            {
                res = (*handler)(callId, args);
            }
            catch (Exception e)
            {
                res = ToolResult(callId, "tool error: " ~ e.msg, true);
            }
        }

        // Send the result back.
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        params["result"] = res.toJson();
        _rpc.notify("session/toolResult", params);
    }

    /// Mark the session as stopped so waiters unblock.
    package void markStopped() @trusted
    {
        _stopped = true;
        _idleMutex.lock();
        scope(exit) _idleMutex.unlock();
        _idle = true;
        _idleCond.notifyAll();
    }

    /// Install the sub-agent lifecycle hooks (called by CopilotClient).
    package void configureHooks(SubagentStartHandler onStart, SubagentStopHandler onStop) @safe
    {
        _onSubagentStart = onStart;
        _onSubagentStop = onStop;
    }

    /// Install the shell-authentication and memory-store callbacks (called by CopilotClient).
    package void configureCallbacks(ShellAuthenticationHandler onShellAuthentication,
        StoreMemoryHandler onStoreMemory) @safe
    {
        _onShellAuthentication = onShellAuthentication;
        _onStoreMemory = onStoreMemory;
    }

    /// Answer a `shellAuthentication.request`; cancels when no handler is set
    /// or the handler throws.
    package JSONValue handleShellAuthentication(JSONValue params) @trusted
    {
        if (_onShellAuthentication is null)
            return ShellAuthenticationResponse.cancel().toJson();
        try
        {
            return _onShellAuthentication(ShellAuthenticationRequest.fromJson(params)).toJson();
        }
        catch (Exception)
        {
            return ShellAuthenticationResponse.cancel().toJson();
        }
    }

    /// Deliver a `memoryStore.store` request to the memory-store callback.
    package void handleMemoryStore(JSONValue params) @trusted
    {
        enforce(_onStoreMemory !is null, "Memory store owner is unavailable");
        _onStoreMemory(StoredMemory.fromJson(params));
    }

    /// Install the callback run when the session is released (called by CopilotClient).
    package void setReleaseHandler(void delegate(string sessionId) handler) @safe
    {
        _onRelease = handler;
    }

    /// Dispatch a `hooks.invoke` callback. Returns the hook output, or JSON
    /// null when no handler is registered for the hook type or it throws.
    package JSONValue handleHooksInvoke(string hookType, JSONValue input) @trusted
    {
        try
        {
            if (hookType == HookType.subagentStart && _onSubagentStart !is null)
                return _onSubagentStart(SubagentStartHookInput.fromJson(input)).toJson();
            if (hookType == HookType.subagentStop && _onSubagentStop !is null)
                return _onSubagentStop(SubagentStopHookInput.fromJson(input)).toJson();
        }
        catch (Exception) {}
        return JSONValue(null);
    }
}

/// Extract the content of an `assistant.message` event, whether it is carried
/// at the top level of the payload or under `data`.
private string assistantContent(const ref SessionEvent ev) @safe
{
    auto direct = ev.stringField("content");
    if (!direct.isNull) return direct.get;
    if (ev.payload.type != JSONType.object) return null;
    auto pData = "data" in ev.payload;
    if (pData is null || (*pData).type != JSONType.object) return null;
    auto pContent = "content" in *pData;
    return (pContent !is null && (*pContent).type == JSONType.string) ? (*pContent).str : null;
}
