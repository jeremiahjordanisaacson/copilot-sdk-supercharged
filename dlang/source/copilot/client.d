/// Copilot SDK client -- manages the CLI process lifecycle and sessions.
///
/// The `CopilotClient` spawns (or connects to) the Copilot CLI, maintains
/// a JSON-RPC connection, and provides the top-level API for creating and
/// resuming sessions.
module copilot.client;

import std.json;
import std.conv : to, text;
import std.string : strip;
import std.typecons : Nullable;
import std.process : Pid, spawnProcess, kill, wait,
                     Redirect, Config, Pipe, pipe;
import std.exception : enforce;
import std.format : format;
import std.stdio : File, stderr;
import core.sync.mutex : Mutex;

import copilot.types;
import copilot.jsonrpc;
import copilot.session;
import copilot.tools : ToolBinding;

// ---------------------------------------------------------------------------
// CopilotClient
// ---------------------------------------------------------------------------

/// High-level client for the GitHub Copilot CLI.
///
/// Typical usage:
/// ---
/// auto client = new CopilotClient(CopilotClientOptions.init);
/// client.start();
/// scope(exit) client.stop();
///
/// auto session = client.createSession(SessionConfig(streaming: false));
/// auto events = session.sendAndWait("Hello!");
/// ---
final class CopilotClient
{
    private
    {
        CopilotClientOptions _opts;
        JsonRpcClient _rpc;
        Pid _childPid;
        File _childStdin;
        File _childStdout;
        bool _started = false;
        bool _isExternal = false;
        Mutex _sessionsMutex;
        CopilotSession[string] _sessions;
    }

    /// Construct a client with the given options.
    this(CopilotClientOptions opts = CopilotClientOptions.init) @trusted
    {
        _opts = opts;
        _sessionsMutex = new Mutex();

        if (!_opts.cliUrl.isNull)
            _isExternal = true;

        if (_opts.autoStart)
            start();
    }

    // ------------------------------------------------------------------
    // Lifecycle
    // ------------------------------------------------------------------

    /// Start the CLI process (or connect to an external server).
    void start() @trusted
    {
        if (_started) return;

        if (_isExternal)
        {
            _startExternal();
        }
        else
        {
            _startSubprocess();
        }
        _started = true;
    }

    /// Gracefully stop the CLI and clean up resources.
    void stop() @trusted
    {
        if (!_started) return;

        // Notify all sessions.
        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        foreach (ref s; _sessions)
            s.markStopped();
        _sessions = null;

        if (_rpc !is null)
        {
            try { _rpc.request("shutdown"); } catch (Exception) {}
            _rpc.stop();
            _rpc = null;
        }

        if (!_isExternal && _childPid !is null)
        {
            try { _childPid.wait(); } catch (Exception) {}
            _childPid = null;
        }

        _started = false;
    }

    /// Forcibly kill the CLI process without a graceful shutdown.
    void forceStop() @trusted
    {
        if (!_started) return;

        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        foreach (ref s; _sessions)
            s.markStopped();
        _sessions = null;

        if (_rpc !is null)
        {
            _rpc.stop();
            _rpc = null;
        }

        if (!_isExternal && _childPid !is null)
        {
            try { kill(_childPid, 9); } catch (Exception) {}
            try { _childPid.wait(); } catch (Exception) {}
            _childPid = null;
        }

        _started = false;
    }

    /// Whether the client has been started.
    @property bool isStarted() const @safe pure nothrow { return _started; }

    // ------------------------------------------------------------------
    // Session management
    // ------------------------------------------------------------------

    /// Create a new conversation session.
    CopilotSession createSession(SessionConfig config = SessionConfig.init) @trusted
    {
        enforce(_started, "client not started");

        auto result = _rpc.request("session/create", config.toJson());
        auto info = SessionInfo.fromJson(result);
        enforce(info.sessionId.length > 0, "server returned empty sessionId");

        auto session = new CopilotSession(info.sessionId, _rpc);

        // Register tool handlers if tools were provided.
        // (The config.tools array carries the schema; the caller registers
        // ToolBinding handlers separately via session.registerTool.)

        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        _sessions[info.sessionId] = session;

        return session;
    }

    /// Create a session with pre-registered tool bindings.
    CopilotSession createSession(SessionConfig config, ToolBinding[] bindings) @trusted
    {
        // Merge tool schemas into the config.
        import std.algorithm : map;
        import std.array : array;
        config.tools = bindings.map!(b => b.tool).array;
        auto session = createSession(config);
        session.registerTools(bindings);
        return session;
    }

    /// Resume a previously created session.
    CopilotSession resumeSession(ResumeSessionConfig config) @trusted
    {
        enforce(_started, "client not started");

        auto result = _rpc.request("session/resume", config.toJson());
        auto info = SessionInfo.fromJson(result);
        string sid = info.sessionId.length > 0 ? info.sessionId : config.sessionId;

        auto session = new CopilotSession(sid, _rpc);

        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        _sessions[sid] = session;

        return session;
    }

    // ------------------------------------------------------------------
    // Utility RPCs
    // ------------------------------------------------------------------

    /// Ping the CLI server.
    bool ping() @trusted
    {
        if (!_started) return false;
        try
        {
            _rpc.request("ping");
            return true;
        }
        catch (Exception) { return false; }
    }

    /// Get the CLI server status.
    CopilotStatus getStatus() @trusted
    {
        enforce(_started, "client not started");
        auto result = _rpc.request("getStatus");
        return CopilotStatus.fromJson(result);
    }

    /// List available models.
    JSONValue listModels() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("listModels");
    }

    /// Get the current authentication status.
    JSONValue getAuthStatus() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("getAuthStatus");
    }

    // ------------------------------------------------------------------
    // Internal startup helpers
    // ------------------------------------------------------------------

    /// Spawn the CLI as a child process using stdio transport.
    private void _startSubprocess() @trusted
    {
        string[] args = [_opts.cliPath, "--headless", "--no-auto-update"];
        args ~= "--log-level";
        args ~= _opts.logLevel;
        args ~= "--stdio";
        args ~= _opts.cliArgs;

        auto stdinPipe  = pipe();
        auto stdoutPipe = pipe();

        string[string] env = null;
        if (_opts.env.length > 0)
            env = _opts.env;

        _childPid = spawnProcess(
            args,
            stdinPipe.readEnd,   // child stdin
            stdoutPipe.writeEnd, // child stdout
            stderr,              // child stderr
            env,
            Config.none,
            _opts.cwd.isNull ? null : _opts.cwd.get,
        );

        _childStdin  = stdinPipe.writeEnd;
        _childStdout = stdoutPipe.readEnd;

        _rpc = new JsonRpcClient(_childStdout, _childStdin);
        _rpc.onNotification(&_onNotification);
        _rpc.start();
    }

    /// Connect to an external CLI server over TCP.
    private void _startExternal() @trusted
    {
        import std.socket : TcpSocket, InternetAddress, AddressFamily;
        import std.uri : URI;

        // For external servers we would open a TCP socket.
        // This is a simplified placeholder: real implementation would
        // wrap the socket as File-like objects for the RPC layer.
        throw new Exception("external CLI connection via cliUrl is not yet implemented; use stdio mode");
    }

    /// Route incoming notifications to the appropriate session.
    private void _onNotification(string method, JSONValue params) @safe
    {
        // Session events arrive as "session/event".
        if (method != "session/event") return;

        auto pSid = "sessionId" in params;
        if (pSid is null || (*pSid).type != JSONType.string) return;
        string sid = (*pSid).str;

        auto pType = "type" in params;
        string evType = (pType !is null && (*pType).type == JSONType.string)
            ? (*pType).str : "";

        auto event = SessionEvent(parseEventType(evType), params);

        CopilotSession session;
        () @trusted {
            _sessionsMutex.lock();
            scope(exit) _sessionsMutex.unlock();
            auto ps = sid in _sessions;
            if (ps !is null) session = *ps;
        }();

        if (session !is null)
            session.dispatchEvent(event);
    }
}
