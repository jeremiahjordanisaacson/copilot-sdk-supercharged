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
import core.sync.mutex : Mutex;
import core.sync.condition : Condition;

import copilot.types;
import copilot.jsonrpc : JsonRpcClient;
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
        params["message"] = message;

        if (opts.attachments.length > 0)
            params["attachments"] = JSONValue(opts.attachments);
        if (!opts.responseFormat.isNull)
            params["responseFormat"] = opts.responseFormat.get;

        _rpc.notify("session/send", params);
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

    /// Abort the current in-flight request.
    void abort() @safe
    {
        auto params = JSONValue(string[string].init);
        params["sessionId"] = _sessionId;
        _rpc.notify("session/abort", params);
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
}
