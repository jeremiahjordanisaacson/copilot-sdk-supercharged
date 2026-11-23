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
import copilot.session_params : groupSessionParams, applyEmptyModeDefaults;
import copilot.sdk_protocol_version : SdkProtocolVersion;
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

/// Throw unless a `connect` / `ping` result reports exactly `SdkProtocolVersion`.
void verifyProtocolVersion(JSONValue handshake) @safe
{
    auto p = handshake.type == JSONType.object ? "protocolVersion" in handshake : null;
    if (p is null || (*p).type == JSONType.null_)
        throw new Exception(format!("SDK protocol version mismatch: SDK supports version %d, "
            ~ "but server does not report a protocol version. "
            ~ "Please update your SDK or server to ensure compatibility.")(SdkProtocolVersion));

    long reported = -1;
    string shown;
    if ((*p).type == JSONType.integer)
    {
        reported = (*p).integer;
        shown = to!string(reported);
    }
    else if ((*p).type == JSONType.uinteger)
    {
        reported = cast(long) (*p).uinteger;
        shown = to!string(reported);
    }
    else if ((*p).type == JSONType.float_)
    {
        reported = cast(long) (*p).floating;
        shown = to!string((*p).floating);
    }
    else
    {
        shown = (*p).toString();
    }
    if (reported != SdkProtocolVersion)
        throw new Exception(format!("SDK protocol version mismatch: SDK supports version %d, "
            ~ "but server reports version %s. "
            ~ "Please update your SDK or server to ensure compatibility.")(SdkProtocolVersion, shown));
}

/// Resolve the CLI command line: an explicit `cliPath`, else `COPILOT_CLI_PATH`,
/// else the default binary name. A `.js` entry point (for example
/// `npm-loader.js`) runs under node; any other path runs directly.
string[] resolveCliCommand(string cliPath) @safe
{
    import std.process : environment;
    import std.algorithm.searching : endsWith;
    import std.uni : toLower;

    string path = cliPath;
    if (path.length == 0 || path == CopilotClientOptions.init.cliPath)
    {
        auto fromEnv = environment.get("COPILOT_CLI_PATH", "");
        path = fromEnv.length > 0 ? fromEnv : CopilotClientOptions.init.cliPath;
    }
    if (path.toLower.endsWith(".js"))
        return ["node", path];
    return [path];
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

        // Handshake (connect with clientInfo, else ping) and protocol check.
        try
        {
            _handshake();
            _registerNetworkProvider();
        }
        catch (Exception e)
        {
            forceStop();
            throw e;
        }

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
        enforce(_opts.mode != ClientMode.empty || !config.availableTools.isNull,
            `client mode "empty" requires availableTools on every session`);

        auto params = config.toJson();
        auto registrationId = _registerGitHubTokenProvider(config.gitHubTokenProvider, params);
        scope (failure) _unregisterGitHubTokenProvider(registrationId);

        auto payload = groupSessionParams(params, false);
        if (_opts.mode == ClientMode.empty)
            applyEmptyModeDefaults(payload);

        auto result = _rpc.request("session.create", payload);
        auto info = SessionInfo.fromJson(result);
        enforce(info.sessionId.length > 0, "server returned empty sessionId");
        _assignGitHubTokenProvider(registrationId, info.sessionId);

        auto session = new CopilotSession(info.sessionId, _rpc);
        session.configureHooks(config.onSubagentStart, config.onSubagentStop);
        session.configureCallbacks(config.onShellAuthenticationRequest, config.onStoreMemory);
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
        enforce(_opts.mode != ClientMode.empty || !config.availableTools.isNull,
            `client mode "empty" requires availableTools on every session`);

        auto params = config.toJson();
        auto registrationId = _registerGitHubTokenProvider(config.gitHubTokenProvider, params);
        scope (failure) _unregisterGitHubTokenProvider(registrationId);

        auto payload = groupSessionParams(params, true);
        if (_opts.mode == ClientMode.empty)
            applyEmptyModeDefaults(payload);

        auto result = _rpc.request("session.resume", payload);
        auto info = SessionInfo.fromJson(result);
        string sid = info.sessionId.length > 0 ? info.sessionId : config.sessionId;
        _assignGitHubTokenProvider(registrationId, sid);

        auto session = new CopilotSession(sid, _rpc);
        session.configureHooks(config.onSubagentStart, config.onSubagentStop);
        session.configureCallbacks(config.onShellAuthenticationRequest, config.onStoreMemory);
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
        string[] args = resolveCliCommand(_opts.cliPath);
        args ~= ["--headless", "--no-auto-update"];
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
        // Session events arrive as "session.event" with the event nested
        // under `event` (legacy servers sent "session/event" with the event
        // fields at the top level). Unknown event types pass through.
        if (method != "session.event" && method != "session/event") return;
        if (params.type != JSONType.object) return;

        auto pSid = "sessionId" in params;
        if (pSid is null || (*pSid).type != JSONType.string) return;
        string sid = (*pSid).str;

        JSONValue eventJson = params;
        auto pEvent = "event" in params;
        if (method == "session.event" && pEvent !is null && (*pEvent).type == JSONType.object)
            eventJson = *pEvent;

        auto pType = "type" in eventJson;
        string evType = (pType !is null && (*pType).type == JSONType.string)
            ? (*pType).str : "";

        auto event = SessionEvent(parseEventType(evType), eventJson);
        auto pId = "id" in eventJson;
        if (pId !is null && (*pId).type == JSONType.string)
            event.id = (*pId).str;
        auto pTimestamp = "timestamp" in eventJson;
        if (pTimestamp !is null && (*pTimestamp).type == JSONType.string)
            event.timestamp = (*pTimestamp).str;
        auto pParent = "parentId" in eventJson;
        if (pParent !is null && (*pParent).type == JSONType.string)
            event.parentId = (*pParent).str;
        auto pAgent = "agentId" in eventJson;
        if (pAgent !is null && (*pAgent).type == JSONType.string)
            event.agentId = (*pAgent).str;
        auto pEphemeral = "ephemeral" in eventJson;
        if (pEphemeral !is null && ((*pEphemeral).type == JSONType.true_
            || (*pEphemeral).type == JSONType.false_))
            event.ephemeral = (*pEphemeral).type == JSONType.true_;

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
    // Handshake, network callbacks, GitHub token providers, hooks
    // ------------------------------------------------------------------

    /// Handshake with the runtime and verify SDK protocol 4. `connect` is
    /// only sent to declare `clientInfo`; runtimes without `connect` fall back
    /// to `ping`, which is also used when no client info is configured.
    private void _handshake() @trusted
    {
        JSONValue handshake;
        bool connected = false;
        auto info = _opts.clientInfo.toWire();
        if (info.type == JSONType.object)
        {
            auto params = JSONValue(string[string].init);
            params["clientInfo"] = info;
            try
            {
                handshake = _rpc.request("connect", params);
                connected = true;
            }
            catch (JsonRpcError e)
            {
                import std.algorithm.searching : canFind;
                if (e.code != methodNotFoundCode && !e.msg.canFind("Unhandled method connect"))
                    throw e;
            }
        }
        if (!connected)
            handshake = _rpc.request("ping", JSONValue(string[string].init));
        verifyProtocolVersion(handshake);
    }

    /// Register the configured network callbacks (`network.setProvider`).
    /// An empty object marks each registered callback.
    private void _registerNetworkProvider() @trusted
    {
        if (!_opts.network.hasCallbacks()) return;
        auto params = JSONValue(string[string].init);
        if (!_opts.network.scope_.isNull)
            params["scope"] = _opts.network.scope_.get;
        if (_opts.network.resolveProxy !is null)
            params["resolveProxy"] = JSONValue(string[string].init);
        if (_opts.network.authenticateNegotiate !is null)
            params["authenticateNegotiate"] = JSONValue(string[string].init);
        if (_opts.network.authenticateBasic !is null)
            params["authenticateBasic"] = JSONValue(string[string].init);
        _rpc.request("network.setProvider", params);
    }

    /// Register handlers for server-to-client requests.
    private void _registerRequestHandlers() @trusted
    {
        _rpc.onRequest("gitHubToken.getToken", &_onGitHubTokenRequest);
        _rpc.onRequest("hooks.invoke", &_onHooksInvoke);
        _rpc.onRequest("shellAuthentication.request", &_onShellAuthenticationRequest);
        _rpc.onRequest("shellAuthentication.cancel", &_onShellAuthenticationCancel);
        _rpc.onRequest("memoryStore.store", &_onMemoryStore);
        if (_opts.network.resolveProxy !is null)
            _rpc.onRequest("network.resolveProxy", &_onResolveProxy);
        if (_opts.network.authenticateNegotiate !is null)
            _rpc.onRequest("network.authenticateNegotiate", &_onAuthenticateNegotiate);
        if (_opts.network.authenticateBasic !is null)
            _rpc.onRequest("network.authenticateBasic", &_onAuthenticateBasic);
    }

    /// Find a tracked session by ID (null when unknown).
    private CopilotSession _findSession(string sid) @trusted
    {
        _sessionsMutex.lock();
        scope(exit) _sessionsMutex.unlock();
        auto ps = sid in _sessions;
        return ps !is null ? *ps : null;
    }

    /// Answer `shellAuthentication.request` through the session's handler.
    private JSONValue _onShellAuthenticationRequest(JSONValue params) @trusted
    {
        auto session = _findSession(jsonString(params, "sessionId"));
        if (session is null)
            return ShellAuthenticationResponse.cancel().toJson();
        return session.handleShellAuthentication(params);
    }

    /// Acknowledge `shellAuthentication.cancel`. Requests are answered
    /// synchronously on the reader thread, so the request was already answered.
    private JSONValue _onShellAuthenticationCancel(JSONValue params) @trusted
    {
        return JSONValue(string[string].init);
    }

    /// Deliver `memoryStore.store` to the session's memory-store callback.
    private JSONValue _onMemoryStore(JSONValue params) @trusted
    {
        auto session = _findSession(jsonString(params, "sessionId"));
        enforce(session !is null, "Memory store owner is unavailable");
        session.handleMemoryStore(params);
        return JSONValue(null);
    }

    /// Answer `network.resolveProxy`.
    private JSONValue _onResolveProxy(JSONValue params) @trusted
    {
        auto reply = JSONValue(string[string].init);
        auto proxy = _opts.network.resolveProxy(jsonString(params, "url"));
        if (!proxy.isNull && proxy.get.length > 0)
            reply["proxy"] = proxy.get;
        return reply;
    }

    /// Answer `network.authenticateNegotiate`. Tokens are never logged.
    private JSONValue _onAuthenticateNegotiate(JSONValue params) @trusted
    {
        auto reply = JSONValue(string[string].init);
        auto token = _opts.network.authenticateNegotiate(jsonString(params, "spn"));
        if (!token.isNull && token.get.length > 0)
            reply["token"] = token.get;
        return reply;
    }

    /// Answer `network.authenticateBasic`. Credentials are never logged.
    private JSONValue _onAuthenticateBasic(JSONValue params) @trusted
    {
        NetworkBasicAuthRequest request;
        request.proxy = jsonString(params, "proxy");
        auto realm = jsonString(params, "realm");
        if (realm !is null)
            request.realm = realm;
        if (params.type == JSONType.object)
        {
            auto pRejected = "previousRejected" in params;
            request.previousRejected = pRejected !is null && (*pRejected).type == JSONType.true_;
        }
        auto reply = JSONValue(string[string].init);
        auto credentials = _opts.network.authenticateBasic(request);
        if (!credentials.isNull)
        {
            if (credentials.get.username.length > 0)
                reply["username"] = credentials.get.username;
            if (credentials.get.password.length > 0)
                reply["password"] = credentials.get.password;
        }
        return reply;
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
