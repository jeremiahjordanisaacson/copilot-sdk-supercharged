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
import std.uuid : randomUUID;
import core.sync.mutex : Mutex;

import copilot.types;
import copilot.jsonrpc;
import copilot.session;
import copilot.tools : ToolBinding;

/// A session's GitHub token provider in the client-level registry.
private struct GitHubTokenRegistration
{
    GitHubTokenProvider provider;
    string sessionId;
}

/// Read a string member of a JSON object (null when absent or not a string).
private string jsonString(JSONValue obj, string key) @safe
{
    if (obj.type != JSONType.object) return null;
    auto p = key in obj;
    return (p !is null && (*p).type == JSONType.string) ? (*p).str : null;
}

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
        Mutex _tokenProvidersMutex;
        GitHubTokenRegistration[string] _gitHubTokenProviders;
    }

    /// Construct a client with the given options.
    this(CopilotClientOptions opts = CopilotClientOptions.init) @trusted
    {
        _opts = opts;
        _sessionsMutex = new Mutex();
        _tokenProvidersMutex = new Mutex();

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

        // Declare the embedding application's identity before other requests.
        _sendClientInfo();

        // Set up session filesystem if configured.
        if (!_opts.sessionFs.isNull)
        {
            try { setSessionFsProvider(_opts.sessionFs.get); }
            catch (Exception) {}
        }
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
        _clearGitHubTokenProviders();

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
        _clearGitHubTokenProviders();

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
        enforce(config.gitHubToken.isNull || config.gitHubTokenProvider is null,
            "gitHubToken and gitHubTokenProvider are mutually exclusive");

        auto params = config.toJson();
        auto registrationId = _registerGitHubTokenProvider(config.gitHubTokenProvider, params);
        scope (failure) _unregisterGitHubTokenProvider(registrationId);

        auto result = _rpc.request("session.create", params);
        auto info = SessionInfo.fromJson(result);
        enforce(info.sessionId.length > 0, "server returned empty sessionId");
        _assignGitHubTokenProvider(registrationId, info.sessionId);

        auto session = new CopilotSession(info.sessionId, _rpc);
        session.configureHooks(config.onSubagentStart, config.onSubagentStop);
        session.setReleaseHandler(&_releaseSession);

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

        auto params = config.toJson();
        auto registrationId = _registerGitHubTokenProvider(config.gitHubTokenProvider, params);
        scope (failure) _unregisterGitHubTokenProvider(registrationId);

        auto result = _rpc.request("session.resume", params);
        auto info = SessionInfo.fromJson(result);
        string sid = info.sessionId.length > 0 ? info.sessionId : config.sessionId;
        _assignGitHubTokenProvider(registrationId, sid);

        auto session = new CopilotSession(sid, _rpc);
        session.configureHooks(config.onSubagentStart, config.onSubagentStop);
        session.setReleaseHandler(&_releaseSession);

        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        _sessions[sid] = session;

        return session;
    }

    /// Delete a session by ID.
    void deleteSession(string sessionId) @trusted
    {
        enforce(_started, "client not started");
        auto params = JSONValue(["sessionId": JSONValue(sessionId)]);
        _rpc.request("session.delete", params);

        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        _sessions.remove(sessionId);
    }

    /// List all sessions known to the server.
    JSONValue listSessions() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("session.list");
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
        auto result = _rpc.request("status.get");
        return CopilotStatus.fromJson(result);
    }

    /// List available models.
    JSONValue listModels() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("models.list");
    }

    /// Get the current authentication status.
    JSONValue getAuthStatus() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("auth.getStatus");
    }

    /// Clear the runtime's managed-settings cache (`managedSettings.clearCache`).
    JSONValue clearManagedSettingsCache() @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("managedSettings.clearCache", JSONValue(string[string].init));
    }

    /// Get the last session ID, or null if none.
    Nullable!string getLastSessionId() @trusted
    {
        enforce(_started, "client not started");
        auto result = _rpc.request("session.getLastId");
        auto pSid = "sessionId" in result;
        if (pSid !is null && (*pSid).type == JSONType.string)
            return Nullable!string((*pSid).str);
        return Nullable!string.init;
    }

    /// Get session metadata by session ID.
    JSONValue getSessionMetadata(string sessionId) @trusted
    {
        enforce(_started, "client not started");
        auto params = JSONValue(["sessionId": JSONValue(sessionId)]);
        return _rpc.request("session.getMetadata", params);
    }

    /// Get the foreground session ID.
    string getForegroundSessionId() @trusted
    {
        enforce(_started, "client not started");
        auto result = _rpc.request("session.getForeground");
        auto pSid = "sessionId" in result;
        enforce(pSid !is null && (*pSid).type == JSONType.string,
            "No sessionId in response");
        return (*pSid).str;
    }

    /// Set the foreground session ID.
    void setForegroundSessionId(string sessionId) @trusted
    {
        enforce(_started, "client not started");
        auto params = JSONValue(["sessionId": JSONValue(sessionId)]);
        auto result = _rpc.request("session.setForeground", params);
        auto pSuccess = "success" in result;
        if (pSuccess is null || (*pSuccess).type != JSONType.true_)
        {
            auto pErr = "error" in result;
            string errMsg = (pErr !is null && (*pErr).type == JSONType.string)
                ? (*pErr).str : "Unknown";
            throw new Exception("Failed to set foreground session: " ~ errMsg);
        }
    }

    // ------------------------------------------------------------------
    // SessionFs
    // ------------------------------------------------------------------

    /// Register a session filesystem provider with the CLI server.
    JSONValue setSessionFsProvider(SessionFsConfig config) @trusted
    {
        enforce(_started, "client not started");
        return _rpc.request("sessionFs.setProvider", config.toJson());
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
        if (_opts.remote)
            args ~= "--remote";
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
        _registerRequestHandlers();
        _rpc.start();
    }

    /// Connect to an external CLI server over TCP.
    private void _startExternal() @trusted
    {
        import std.socket : TcpSocket, InternetAddress, AddressFamily;

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

    // ------------------------------------------------------------------
    // v2.6.0: client info, GitHub token providers, hooks
    // ------------------------------------------------------------------

    /// Declare the embedding application's identity via `connect`. Older
    /// runtimes without `connect` are tolerated.
    private void _sendClientInfo() @trusted
    {
        auto info = _opts.clientInfo.toWire();
        if (info.type != JSONType.object) return;

        auto params = JSONValue(string[string].init);
        params["clientInfo"] = info;
        try
        {
            _rpc.request("connect", params);
        }
        catch (JsonRpcError e)
        {
            import std.algorithm.searching : canFind;
            if (e.code != methodNotFoundCode && !e.msg.canFind("Unhandled method connect"))
                throw e;
        }
    }

    /// Register handlers for server-to-client requests.
    private void _registerRequestHandlers() @trusted
    {
        _rpc.onRequest("gitHubToken.getToken", &_onGitHubTokenRequest);
        _rpc.onRequest("hooks.invoke", &_onHooksInvoke);
    }

    /// Store a session's GitHub token provider and add its registration ID
    /// to the request params. Returns null when no provider is configured.
    private string _registerGitHubTokenProvider(GitHubTokenProvider provider,
        ref JSONValue params) @trusted
    {
        if (provider is null) return null;
        auto id = randomUUID().toString();
        _tokenProvidersMutex.lock();
        scope(exit) _tokenProvidersMutex.unlock();
        _gitHubTokenProviders[id] = GitHubTokenRegistration(provider, null);
        params["gitHubTokenProviderRegistrationId"] = id;
        return id;
    }

    /// Drop a registration (e.g. when session creation fails).
    private void _unregisterGitHubTokenProvider(string registrationId) @trusted
    {
        if (registrationId is null) return;
        _tokenProvidersMutex.lock();
        scope(exit) _tokenProvidersMutex.unlock();
        _gitHubTokenProviders.remove(registrationId);
    }

    /// Bind a registration to the session ID returned by the runtime.
    private void _assignGitHubTokenProvider(string registrationId, string sessionId) @trusted
    {
        if (registrationId is null) return;
        _tokenProvidersMutex.lock();
        scope(exit) _tokenProvidersMutex.unlock();
        auto p = registrationId in _gitHubTokenProviders;
        if (p !is null)
            p.sessionId = sessionId;
    }

    /// Drop every registration owned by a released session.
    private void _releaseSession(string sessionId) @trusted
    {
        _tokenProvidersMutex.lock();
        scope(exit) _tokenProvidersMutex.unlock();
        string[] stale;
        foreach (id, ref reg; _gitHubTokenProviders)
            if (reg.sessionId == sessionId)
                stale ~= id;
        foreach (id; stale)
            _gitHubTokenProviders.remove(id);
    }

    /// Drop every registration (client stop).
    private void _clearGitHubTokenProviders() @trusted
    {
        _tokenProvidersMutex.lock();
        scope(exit) _tokenProvidersMutex.unlock();
        _gitHubTokenProviders = null;
    }

    /// Handle `gitHubToken.getToken` by calling the registered provider.
    private JSONValue _onGitHubTokenRequest(JSONValue params) @trusted
    {
        auto registrationId = jsonString(params, "registrationId");
        GitHubTokenRegistration reg;
        bool found = false;
        {
            _tokenProvidersMutex.lock();
            scope(exit) _tokenProvidersMutex.unlock();
            auto p = registrationId in _gitHubTokenProviders;
            if (p !is null)
            {
                reg = *p;
                found = true;
            }
        }
        enforce(found, format!`No GitHub token provider registered for registration ID "%s"`(
            registrationId));

        GitHubTokenProviderArgs args;
        args.host = jsonString(params, "host");
        auto sid = jsonString(params, "sessionId");
        if (sid.length > 0)
            args.sessionId = sid;
        else if (reg.sessionId.length > 0)
            args.sessionId = reg.sessionId;
        args.reason = jsonString(params, "reason");
        return reg.provider(args).toJson();
    }

    /// Handle `hooks.invoke` by dispatching to the target session.
    private JSONValue _onHooksInvoke(JSONValue params) @trusted
    {
        auto sid = jsonString(params, "sessionId");
        CopilotSession session;
        {
            _sessionsMutex.lock();
            scope(exit) _sessionsMutex.unlock();
            auto ps = sid in _sessions;
            if (ps !is null) session = *ps;
        }
        enforce(session !is null, "Session not found: " ~ sid);

        JSONValue input = JSONValue(null);
        if (params.type == JSONType.object)
        {
            auto pInput = "input" in params;
            if (pInput !is null) input = *pInput;
        }

        auto output = session.handleHooksInvoke(jsonString(params, "hookType"), input);
        auto reply = JSONValue(string[string].init);
        if (output.type != JSONType.null_)
            reply["output"] = output;
        return reply;
    }
}
