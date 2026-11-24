// client.zig -- CopilotClient: the primary entry point for the Zig SDK.
//
// Spawns (or connects to) the Copilot CLI server, manages the JSON-RPC
// transport, and creates / resumes sessions.

const std = @import("std");
const builtin = @import("builtin");
const types = @import("types.zig");
const jsonrpc = @import("jsonrpc.zig");
const session_mod = @import("session.zig");
const tools_mod = @import("tools.zig");
const session_params = @import("session_params.zig");

const Allocator = std.mem.Allocator;
const JsonValue = types.JsonValue;
const SdkError = types.SdkError;
const log = std.log.scoped(.copilot_sdk);

pub const ClientOptions = types.ClientOptions;
pub const SessionConfig = types.SessionConfig;
pub const ConnectionState = types.ConnectionState;
pub const ServerStatus = types.ServerStatus;
pub const AuthStatus = types.AuthStatus;
pub const SessionMetadata = types.SessionMetadata;
pub const SessionEvent = types.SessionEvent;
pub const CopilotSession = session_mod.CopilotSession;

// ---------------------------------------------------------------------------
// Lifecycle callback
// ---------------------------------------------------------------------------

pub const LifecycleCallback = *const fn (state: ConnectionState) void;

// ---------------------------------------------------------------------------
// CopilotClient
// ---------------------------------------------------------------------------

pub const CopilotClient = struct {
    allocator: Allocator,
    options: ClientOptions,
    state: ConnectionState = .disconnected,
    transport: ?jsonrpc.JsonRpcTransport = null,
    process: ?std.process.Child = null,
    sessions: std.StringHashMap(*CopilotSession),
    lifecycle_callbacks: std.ArrayList(LifecycleCallback),
    /// GitHub token providers keyed by registration ID (owned keys).
    github_token_providers: std.StringHashMap(types.GitHubTokenProvider),
    /// Sessions superseded by a resume of the same session ID; freed when the client stops.
    replaced_sessions: std.ArrayList(*CopilotSession),

    // Pipes to the CLI process. The transport points at these, so the client
    // must not be moved after `start`.
    stdin_writer: std.fs.File.Writer = undefined,
    stdout_reader: std.fs.File.Reader = undefined,

    // Buffers owned by the client for process I/O
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    /// Create a new client with the given options.
    pub fn init(allocator: Allocator, options: ClientOptions) CopilotClient {
        return .{
            .allocator = allocator,
            .options = options,
            .sessions = std.StringHashMap(*CopilotSession).init(allocator),
            .lifecycle_callbacks = std.ArrayList(LifecycleCallback).init(allocator),
            .github_token_providers = std.StringHashMap(types.GitHubTokenProvider).init(allocator),
            .replaced_sessions = std.ArrayList(*CopilotSession).init(allocator),
        };
    }

    pub fn deinit(self: *CopilotClient) void {
        self.stopInternal(false);
        self.sessions.deinit();
        self.replaced_sessions.deinit();
        self.lifecycle_callbacks.deinit();
        var it = self.github_token_providers.keyIterator();
        while (it.next()) |key| self.allocator.free(key.*);
        self.github_token_providers.deinit();
    }

    // -------------------------------------------------------------------
    // Connection lifecycle
    // -------------------------------------------------------------------

    /// Start the client: spawn the CLI process (or connect to an external
    /// server), send the `connect` handshake when `client_info` is set, verify
    /// the SDK protocol version (4; requires Copilot CLI 1.0.113 or newer), and
    /// register network callbacks.
    pub fn start(self: *CopilotClient) SdkError!void {
        self.setState(.connecting);
        errdefer self.setState(.error_state);

        if (self.options.cli_url) |_| {
            // External server -- not yet implemented in this minimal SDK.
            // A full implementation would open a TCP connection here.
            return SdkError.NotConnected;
        }

        try self.spawnCliProcess();
        if (self.transport) |*t| {
            t.handler_ctx = self;
            t.server_request_fn = handleServerRequest;
            t.notification_fn = handleNotification;
        }
        try self.sendConnectHandshake();
        try self.verifyProtocol();
        try self.registerNetworkProvider();

        // Set up session filesystem provider if configured
        if (self.options.session_fs) |fs| {
            var params = std.json.ObjectMap.init(self.allocator);
            defer params.deinit();
            params.put("initialCwd", .{ .string = fs.initial_cwd }) catch return SdkError.AllocationFailed;
            params.put("sessionStatePath", .{ .string = fs.session_state_path }) catch return SdkError.AllocationFailed;
            params.put("conventions", .{ .string = fs.conventions }) catch return SdkError.AllocationFailed;
            _ = try self.transport.?.sendRequest("sessionFs.setProvider", .{ .object = params });
        }

        self.setState(.connected);
    }

    /// Graceful stop: detach all sessions, then kill the process.
    pub fn stop(self: *CopilotClient) SdkError!void {
        self.stopInternal(true);
    }

    /// Force-stop without graceful session teardown.
    pub fn forceStop(self: *CopilotClient) void {
        self.stopInternal(false);
    }

    fn stopInternal(self: *CopilotClient, graceful: bool) void {
        var it = self.sessions.iterator();
        while (it.next()) |entry| {
            if (graceful) entry.value_ptr.*.detach() catch {};
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.sessions.clearAndFree();

        // Replaced sessions share their ID with a live one, so they are only freed.
        for (self.replaced_sessions.items) |sess| {
            sess.deinit();
            self.allocator.destroy(sess);
        }
        self.replaced_sessions.clearAndFree();

        if (self.transport) |*t| {
            t.deinit();
            self.transport = null;
        }

        if (self.process) |*proc| {
            _ = proc.kill() catch {};
            _ = proc.wait() catch {};
            self.process = null;
        }

        self.setState(.disconnected);
    }

    // -------------------------------------------------------------------
    // Session management
    // -------------------------------------------------------------------

    /// Create a new conversation session.
    pub fn createSession(self: *CopilotClient, config: SessionConfig) SdkError!*CopilotSession {
        return self.openSession(null, config);
    }

    /// Resume a previously created session by ID. When this client still tracks a
    /// session with the same ID, the resumed session replaces it for event routing
    /// (the previous `*CopilotSession` stays allocated until the client stops).
    pub fn resumeSession(self: *CopilotClient, session_id: []const u8, config: SessionConfig) SdkError!*CopilotSession {
        return self.openSession(session_id, config);
    }

    /// Shared session.create / session.resume flow: builds the flat options, adds the
    /// GitHub token provider registration, converts them to the grouped SDK protocol 4
    /// payload (`session_params.groupSessionParams`), and sends the request.
    fn openSession(self: *CopilotClient, resume_session_id: ?[]const u8, config: SessionConfig) SdkError!*CopilotSession {
        if (self.state != .connected) return SdkError.NotConnected;
        if (self.sessionConfigError(config)) |message| {
            log.err("{s}", .{message});
            return SdkError.InvalidConfig;
        }
        const is_resume = resume_session_id != null;

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const a = arena.allocator();

        var flat = std.json.ObjectMap.init(a);
        if (resume_session_id) |sid| try putValue(&flat, "sessionId", .{ .string = sid });
        try self.buildSessionParams(a, &flat, config);

        // Registered before sending: the runtime may request a token while the
        // session is being created.
        var registration_id: ?[]const u8 = null;
        if (config.github_token_provider) |provider| {
            const id = try self.registerTokenProvider(provider);
            putValue(&flat, "gitHubTokenProviderRegistrationId", .{ .string = id }) catch |err| {
                self.unregisterTokenProvider(id);
                return err;
            };
            registration_id = id;
        }
        errdefer if (registration_id) |id| self.unregisterTokenProvider(id);

        var params = session_params.groupSessionParams(a, flat, is_resume) catch return SdkError.AllocationFailed;
        if (self.options.mode == .empty) {
            session_params.applyEmptyModeDefaults(a, &params.object) catch return SdkError.AllocationFailed;
        }

        const method = if (is_resume) "session.resume" else "session.create";
        const result = try self.transport.?.sendRequest(method, params);
        return self.sessionFromResult(result, config, resume_session_id, registration_id);
    }

    /// Describes why `config` cannot be used with this client, or null when it is valid.
    fn sessionConfigError(self: *const CopilotClient, config: SessionConfig) ?[]const u8 {
        if (config.github_token != null and config.github_token_provider != null) {
            return "github_token and github_token_provider are mutually exclusive";
        }
        if (self.options.mode == .empty and config.available_tools == null) {
            return "available_tools is required for every session when the client mode is \"empty\"";
        }
        return null;
    }

    /// Delete a session on the server side. A tracked session with this ID is freed.
    pub fn deleteSession(self: *CopilotClient, session_id: []const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;

        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        const result = try self.transport.?.sendRequest("session.delete", .{ .object = params });
        try expectSuccess(result);

        if (self.sessions.fetchRemove(session_id)) |kv| {
            kv.value.deinit();
            self.allocator.destroy(kv.value);
        }
    }

    /// List all session IDs known to the server.
    pub fn listSessions(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.list", null);
    }

    // -------------------------------------------------------------------
    // Utility RPCs
    // -------------------------------------------------------------------

    /// Ping the server; returns the echoed message.
    pub fn ping(self: *CopilotClient, message: []const u8) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("message", .{ .string = message }) catch return SdkError.AllocationFailed;
        return try self.transport.?.sendRequest("ping", .{ .object = params });
    }

    /// Get server version and protocol information.
    pub fn getStatus(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("status.get", null);
    }

    /// Get authentication status.
    pub fn getAuthStatus(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("auth.getStatus", null);
    }

    /// List available models.
    pub fn listModels(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("models.list", null);
    }

    /// Get session metadata by session ID.
    pub fn getSessionMetadata(self: *CopilotClient, session_id: []const u8) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        return try self.transport.?.sendRequest("session.getMetadata", .{ .object = params });
    }

    /// Get the foreground session ID.
    pub fn getForegroundSessionId(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.getForeground", .{ .object = std.json.ObjectMap.init(self.allocator) });
    }

    /// Set the foreground session ID. Failures surface as `SdkError.JsonRpcError`
    /// (SDK protocol 4) or `SdkError.RequestFailed` (`success: false`).
    pub fn setForegroundSessionId(self: *CopilotClient, session_id: []const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        const result = try self.transport.?.sendRequest("session.setForeground", .{ .object = params });
        try expectSuccess(result);
    }

    /// Get the last session ID.
    pub fn getLastSessionId(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.getLastId", .{ .object = std.json.ObjectMap.init(self.allocator) });
    }

    /// Clear the runtime's cached managed settings (`managedSettings.clearCache`), so the
    /// next session fetches them again.
    pub fn clearManagedSettingsCache(self: *CopilotClient) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        _ = try self.transport.?.sendRequest("managedSettings.clearCache", .{ .object = params });
    }

    // -------------------------------------------------------------------
    // MCP secret store and plugin extension approvals (Copilot CLI 1.0.116+)
    // Older CLIs fail these with `SdkError.JsonRpcError` (method not found).
    // -------------------------------------------------------------------

    /// Save a legacy MCP configuration credential in the runtime-owned secret store
    /// (`mcp.secrets.save`). `value` is a credential and is never logged. `config_dir`
    /// selects the configuration directory (null for the runtime's default).
    /// `used_keychain` is true when the OS keychain stored it, false for the file backend.
    pub fn mcpSecretsSave(self: *CopilotClient, secret_id: []const u8, value: []const u8, config_dir: ?[]const u8) SdkError!types.McpSecretsSaveResult {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try putValue(&params, "secretId", .{ .string = secret_id });
        try putValue(&params, "value", .{ .string = value });
        if (config_dir) |dir| try putValue(&params, "configDir", .{ .string = dir });
        const result = try self.transport.?.sendRequest("mcp.secrets.save", .{ .object = params });
        return types.McpSecretsSaveResult.fromJson(result);
    }

    /// Delete a legacy MCP configuration credential from the runtime-owned secret store
    /// (`mcp.secrets.delete`).
    pub fn mcpSecretsDelete(self: *CopilotClient, secret_id: []const u8, config_dir: ?[]const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try putValue(&params, "secretId", .{ .string = secret_id });
        if (config_dir) |dir| try putValue(&params, "configDir", .{ .string = dir });
        _ = try self.transport.?.sendRequest("mcp.secrets.delete", .{ .object = params });
    }

    /// Delete the legacy credentials indexed for an MCP server (`mcp.secrets.deleteServer`).
    pub fn mcpSecretsDeleteServer(self: *CopilotClient, server_name: []const u8, config_dir: ?[]const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try putValue(&params, "serverName", .{ .string = server_name });
        if (config_dir) |dir| try putValue(&params, "configDir", .{ .string = dir });
        _ = try self.transport.?.sendRequest("mcp.secrets.deleteServer", .{ .object = params });
    }

    /// Persist a pending plugin extension permission request as a user-scoped approval,
    /// before the host answers that request normally (`permissions.extensionApprovals.grant`).
    /// The returned approval is allocated from `arena`.
    pub fn extensionApprovalsGrant(self: *CopilotClient, arena: Allocator, session_id: []const u8, request_id: []const u8) SdkError!types.PermissionsExtensionApprovalsGrantResult {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try putValue(&params, "sessionId", .{ .string = session_id });
        try putValue(&params, "requestId", .{ .string = request_id });
        const result = try self.transport.?.sendRequest("permissions.extensionApprovals.grant", .{ .object = params });
        return types.PermissionsExtensionApprovalsGrantResult.fromJson(arena, result) catch SdkError.AllocationFailed;
    }

    /// List the user-scoped plugin extension approvals (`permissions.extensionApprovals.list`).
    /// `session_id` selects an attached session whose configuration profile to list. The
    /// approvals are allocated from `arena`, so they outlive later requests.
    pub fn extensionApprovalsList(self: *CopilotClient, arena: Allocator, session_id: ?[]const u8) SdkError!types.PermissionsExtensionApprovalsListResult {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        if (session_id) |sid| try putValue(&params, "sessionId", .{ .string = sid });
        const result = try self.transport.?.sendRequest("permissions.extensionApprovals.list", .{ .object = params });
        return types.PermissionsExtensionApprovalsListResult.fromJson(arena, result) catch SdkError.AllocationFailed;
    }

    /// Revoke one user-scoped plugin extension approval (`permissions.extensionApprovals.revoke`).
    /// Pass the approval's `extension_name` and `installation_key` unchanged; `session_id`
    /// selects an attached session whose configuration profile to use.
    pub fn extensionApprovalsRevoke(self: *CopilotClient, extension_name: []const u8, installation_key: []const u8, session_id: ?[]const u8) SdkError!types.PermissionsExtensionApprovalsRevokeResult {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try putValue(&params, "extensionName", .{ .string = extension_name });
        try putValue(&params, "installationKey", .{ .string = installation_key });
        if (session_id) |sid| try putValue(&params, "sessionId", .{ .string = sid });
        const result = try self.transport.?.sendRequest("permissions.extensionApprovals.revoke", .{ .object = params });
        return types.PermissionsExtensionApprovalsRevokeResult.fromJson(result);
    }

    /// JSON-RPC error of the most recent request that failed with `SdkError.JsonRpcError`.
    /// Valid until the next request.
    pub fn lastRpcError(self: *const CopilotClient) ?jsonrpc.RpcErrorInfo {
        if (self.transport) |*t| return t.lastError();
        return null;
    }

    /// Policy Helper failure carried in the error data (`error.data.policyHelperFailure`) of
    /// the most recent failed request, for example a session.create or session.resume that
    /// managed policy blocked. The result is allocated from `arena`.
    pub fn lastPolicyHelperFailure(self: *const CopilotClient, arena: Allocator) ?types.PolicyHelperFailure {
        const info = self.lastRpcError() orelse return null;
        const data_json = info.data_json orelse return null;
        const data = std.json.parseFromSliceLeaky(JsonValue, arena, data_json, .{ .allocate = .alloc_always }) catch return null;
        return types.PolicyHelperFailure.fromJson(data);
    }

    /// Get the current connection state.
    pub fn getState(self: *const CopilotClient) ConnectionState {
        return self.state;
    }

    /// Subscribe to lifecycle state changes.
    pub fn onLifecycle(self: *CopilotClient, callback: LifecycleCallback) void {
        self.lifecycle_callbacks.append(callback) catch {};
    }

    // -------------------------------------------------------------------
    // Internal helpers
    // -------------------------------------------------------------------

    fn setState(self: *CopilotClient, new_state: ConnectionState) void {
        self.state = new_state;
        for (self.lifecycle_callbacks.items) |cb| {
            cb(new_state);
        }
    }

    fn spawnCliProcess(self: *CopilotClient) SdkError!void {
        const cli_path = self.options.cli_path orelse "copilot";

        var argv = std.ArrayList([]const u8).init(self.allocator);
        defer argv.deinit();

        // JavaScript entry points (for example @github/copilot/npm-loader.js) run under
        // node; any other path (such as the node_modules/.bin/copilot shim) runs directly.
        if (std.mem.endsWith(u8, cli_path, ".js")) {
            argv.append("node") catch return SdkError.AllocationFailed;
        }
        argv.append(cli_path) catch return SdkError.AllocationFailed;
        argv.append("--headless") catch return SdkError.AllocationFailed;
        argv.append("--no-auto-update") catch return SdkError.AllocationFailed;
        argv.append("--log-level") catch return SdkError.AllocationFailed;
        argv.append(self.options.log_level) catch return SdkError.AllocationFailed;

        if (self.options.use_stdio) {
            argv.append("--stdio") catch return SdkError.AllocationFailed;
        }

        if (self.options.cli_args) |extra| {
            for (extra) |arg| {
                argv.append(arg) catch return SdkError.AllocationFailed;
            }
        }

        var child = std.process.Child.init(argv.items, self.allocator);
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        // Inherited so CLI diagnostics stay visible and an undrained pipe cannot block it.
        child.stderr_behavior = .Inherit;
        child.cwd = self.options.cwd;

        child.spawn() catch return SdkError.ProcessSpawn;

        self.process = child;

        // The transport talks to the child through client-owned File wrappers.
        self.stdin_writer = self.process.?.stdin.?.writer();
        self.stdout_reader = self.process.?.stdout.?.reader();
        self.transport = jsonrpc.JsonRpcTransport.init(self.allocator, self.stdin_writer.any(), self.stdout_reader.any());
    }

    /// Declares the integrating application (`clientInfo`) with `connect` before the
    /// ping handshake. Skipped when no client_info field is set; CLIs that do not
    /// implement `connect` are handled by the ping handshake alone.
    fn sendConnectHandshake(self: *CopilotClient) SdkError!void {
        const info = self.options.client_info orelse return;

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const a = arena.allocator();

        var client_info = std.json.ObjectMap.init(a);
        try putNonEmpty(&client_info, "editorName", info.editor_name);
        try putNonEmpty(&client_info, "editorVersion", info.editor_version);
        try putNonEmpty(&client_info, "extensionName", info.extension_name);
        try putNonEmpty(&client_info, "extensionVersion", info.extension_version);
        if (client_info.count() == 0) return;

        var params = std.json.ObjectMap.init(a);
        try putValue(&params, "clientInfo", .{ .object = client_info });
        if (self.transport.?.sendRequest("connect", .{ .object = params })) |_| {} else |err| {
            if (err != SdkError.JsonRpcError or !self.transport.?.lastErrorIsMethodNotFound()) return err;
        }
    }

    /// Checks that the server speaks SDK protocol 4 (Copilot CLI 1.0.113 or newer).
    fn verifyProtocol(self: *CopilotClient) SdkError!void {
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("message", .{ .string = "sdk-init" }) catch return SdkError.AllocationFailed;
        const result = try self.transport.?.sendRequest("ping", .{ .object = params });

        const reported: ?i64 = blk: {
            if (result != .object) break :blk null;
            const pv = result.object.get("protocolVersion") orelse break :blk null;
            break :blk switch (pv) {
                .integer => |i| i,
                .float => |f| @as(i64, @intFromFloat(f)),
                else => null,
            };
        };
        const server_version = reported orelse {
            log.err("SDK protocol version mismatch: SDK supports version {d}, but server does not report a protocol version. Please update your SDK or server to ensure compatibility.", .{types.PROTOCOL_VERSION});
            return SdkError.ProtocolMismatch;
        };
        if (server_version != types.PROTOCOL_VERSION) {
            log.err("SDK protocol version mismatch: SDK supports version {d}, but server reports version {d}. Please update your SDK or server to ensure compatibility.", .{ types.PROTOCOL_VERSION, server_version });
            return SdkError.ProtocolMismatch;
        }
    }

    /// Registers the configured network callbacks with `network.setProvider`; each
    /// present member (an empty object) marks one registered callback.
    fn registerNetworkProvider(self: *CopilotClient) SdkError!void {
        const network = self.options.network orelse return;
        if (network.resolve_proxy == null and network.authenticate_negotiate == null and network.authenticate_basic == null) return;

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const a = arena.allocator();

        var params = std.json.ObjectMap.init(a);
        if (network.scope) |scope| try putValue(&params, "scope", .{ .string = scope });
        if (network.resolve_proxy != null) try putValue(&params, "resolveProxy", .{ .object = std.json.ObjectMap.init(a) });
        if (network.authenticate_negotiate != null) try putValue(&params, "authenticateNegotiate", .{ .object = std.json.ObjectMap.init(a) });
        if (network.authenticate_basic != null) try putValue(&params, "authenticateBasic", .{ .object = std.json.ObjectMap.init(a) });
        _ = try self.transport.?.sendRequest("network.setProvider", .{ .object = params });
    }

    /// Builds the flat session options (the SDK's long-standing option names);
    /// `openSession` groups them for SDK protocol 4 before sending.
    fn buildSessionParams(self: *CopilotClient, a: Allocator, params: *std.json.ObjectMap, config: SessionConfig) SdkError!void {
        if (config.model) |m| try putValue(params, "model", .{ .string = m });
        if (config.system_prompt) |sp| {
            var system_message = std.json.ObjectMap.init(a);
            try putValue(&system_message, "mode", .{ .string = "append" });
            try putValue(&system_message, "content", .{ .string = sp });
            try putValue(params, "systemMessage", .{ .object = system_message });
        }
        try putValue(params, "streaming", .{ .bool = config.streaming });
        if (config.tools) |defs| try putValue(params, "tools", try toolDefinitionsJson(a, defs));
        if (config.skill_directories) |v| try putValue(params, "skillDirectories", try stringArray(a, v));
        if (config.disabled_skills) |v| try putValue(params, "disabledSkills", try stringArray(a, v));
        if (config.excluded_tools) |v| try putValue(params, "excludedTools", try stringArray(a, v));
        if (config.include_sub_agent_streaming_events) |v| try putValue(params, "includeSubAgentStreamingEvents", .{ .bool = v });
        if (config.provider) |p| try putValue(params, "provider", try legacyProviderJson(a, p));
        if (config.enable_config_discovery) |v| try putValue(params, "enableConfigDiscovery", .{ .bool = v });
        if (try modelCapabilitiesJson(a, config)) |caps| try putValue(params, "modelCapabilities", caps);
        if (config.mcp_servers_json) |text| try putValue(params, "mcpServers", try parseJsonOption(a, text));
        if (config.commands_json) |text| try putValue(params, "commands", try parseJsonOption(a, text));
        if (config.github_token) |token| try putValue(params, "gitHubToken", .{ .string = token });
        if (config.instruction_directories) |v| try putValue(params, "instructionDirectories", try stringArray(a, v));

        // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
        if (config.enable_citations) |v| try putValue(params, "enableCitations", .{ .bool = v });
        if (config.excluded_builtin_agents) |v| try putValue(params, "excludedBuiltinAgents", try stringArray(a, v));
        if (config.session_limits) |sl| {
            var limits = std.json.ObjectMap.init(a);
            if (sl.max_ai_credits) |credits| try putValue(&limits, "maxAiCredits", .{ .float = credits });
            try putValue(params, "sessionLimits", .{ .object = limits });
        }
        if (config.memory) |mem| {
            var memory = std.json.ObjectMap.init(a);
            if (mem.enabled) |enabled| try putValue(&memory, "enabled", .{ .bool = enabled });
            try putValue(params, "memory", .{ .object = memory });
        }
        if (config.otlp_protocol) |v| try putValue(params, "otlpProtocol", .{ .string = v });
        if (config.exp_assignments_json) |text| try putValue(params, "expAssignments", try parseJsonOption(a, text));
        if (config.mcp_auth_handler) try putValue(params, "mcpAuthHandler", .{ .bool = true });

        // --- 2026-08 upstream-sync session options ---
        if (config.rewind_enabled) |v| try putValue(params, "rewindEnabled", .{ .bool = v });
        if (config.additional_directories) |v| try putValue(params, "additionalDirectories", try stringArray(a, v));
        if (config.disabled_mcp_servers) |v| try putValue(params, "disabledMcpServers", try stringArray(a, v));
        if (config.github_mcp_tool_config_json) |text| try putValue(params, "githubMcpToolConfig", try parseJsonOption(a, text));
        if (config.canvas_provider_json) |text| try putValue(params, "canvasProvider", try parseJsonOption(a, text));
        if (config.custom_agents_local_only) |v| try putValue(params, "customAgentsLocalOnly", .{ .bool = v });
        if (config.reasoning_effort) |v| try putValue(params, "reasoningEffort", .{ .string = v });
        if (config.tool_search_json) |text| try putValue(params, "toolSearch", try parseJsonOption(a, text));
        if (config.experimental_mode) |v| try putValue(params, "enableExperimentalMode", .{ .bool = v });
        if (config.content_exclusion) |v| try putValue(params, "contentExclusion", .{ .bool = v });
        if (config.args_schema_json) |text| try putValue(params, "argsSchema", try parseJsonOption(a, text));

        // --- v2.6.0 session options ---
        if (config.name) |v| try putValue(params, "name", .{ .string = v });
        if (config.working_directory) |v| try putValue(params, "workingDirectory", .{ .string = v });
        if (config.available_tools) |v| try putValue(params, "availableTools", try stringArray(a, v));
        if (config.ask_user_variant) |v| try putValue(params, "askUserVariant", .{ .string = v });
        if (config.sandbox_config) |sc| try putValue(params, "sandboxConfig", try sandboxConfigJson(a, sc));
        if (config.feature_flags_json) |text| try putValue(params, "featureFlags", try parseJsonOption(a, text));
        if (config.allowed_models) |v| try putValue(params, "allowedModels", try stringArray(a, v));
        if (config.auth_client_id_metadata_url) |v| try putValue(params, "authClientIdMetadataUrl", .{ .string = v });
        if (config.agent_directories) |v| try putValue(params, "agentDirectories", try stringArray(a, v));
        if (config.hook_directories) |v| try putValue(params, "hookDirectories", try stringArray(a, v));
        if (config.interactive_user) |v| try putValue(params, "interactiveUser", .{ .bool = v });
        if (config.excluded_managed_settings) |v| try putValue(params, "excludedManagedSettings", try stringArray(a, v));
        if (config.enforce_managed_model_defaults) |v| try putValue(params, "enforceManagedModelDefaults", .{ .bool = v });
        if (config.trust_working_directory) |v| try putValue(params, "trustWorkingDirectory", .{ .string = v });
        if (config.refresh_custom_instructions) |v| try putValue(params, "refreshCustomInstructions", .{ .bool = v });
        if (config.allow_transcript_recovery) |v| try putValue(params, "allowTranscriptRecovery", .{ .bool = v });
        if (config.image_generation) |ig| {
            var image_generation = std.json.ObjectMap.init(a);
            if (ig.enabled) |v| try putValue(&image_generation, "enabled", .{ .bool = v });
            if (ig.watermark) |v| try putValue(&image_generation, "watermark", .{ .bool = v });
            if (ig.save_to_disk) |v| try putValue(&image_generation, "saveToDisk", .{ .bool = v });
            try putValue(params, "imageGeneration", .{ .object = image_generation });
        }
        if (config.autopilot) |ap| {
            var autopilot = std.json.ObjectMap.init(a);
            if (ap.continuation_enabled) |v| try putValue(&autopilot, "continuationEnabled", .{ .bool = v });
            if (ap.max_continues) |v| try putValue(&autopilot, "maxContinues", .{ .integer = v });
            try putValue(params, "autopilot", .{ .object = autopilot });
        }
        if (config.on_hook != null) try putValue(params, "hooks", .{ .bool = true });

        // --- v3.0.0 session options ---
        if (config.suppress_resume_event) |v| try putValue(params, "suppressResumeEvent", .{ .bool = v });
        if (config.providers) |list| try putValue(params, "providers", try namedProvidersJson(a, list));
        if (config.models) |list| try putValue(params, "models", try providerModelsJson(a, list));
        if (config.default_provider) |v| {
            try putValue(params, "defaultProvider", .{ .string = v });
        } else if (config.clear_default_provider) {
            // An explicit null clears the default provider (session.resume only).
            try putValue(params, "defaultProvider", .null);
        }
        if (try capiJson(a, config)) |capi| try putValue(params, "capi", capi);
        if (config.permission_mode) |v| try putValue(params, "permissionMode", .{ .string = v });
        if (config.on_shell_authentication != null) try putValue(params, "requestShellAuthentication", .{ .bool = true });
        if (config.on_memory_store != null) try putValue(params, "hasMemoryStore", .{ .bool = true });

        // --- v3.1.0 session options ---
        if (config.on_user_input != null) try putValue(params, "requestUserInput", .{ .bool = true });

        // W3C trace context for distributed tracing
        if (self.options.on_get_trace_context) |provider| {
            const trace = provider();
            if (trace.traceparent) |tp| try putValue(params, "traceparent", .{ .string = tp });
            if (trace.tracestate) |ts| try putValue(params, "tracestate", .{ .string = ts });
        }
    }

    fn sessionFromResult(
        self: *CopilotClient,
        result: JsonValue,
        config: SessionConfig,
        resume_session_id: ?[]const u8,
        registration_id: ?[]const u8,
    ) SdkError!*CopilotSession {
        var sid: []const u8 = resume_session_id orelse "";
        var wpath: ?[]const u8 = null;
        if (result == .object) {
            if (result.object.get("sessionId")) |sv| {
                if (sv == .string) sid = sv.string;
            }
            if (result.object.get("workspacePath")) |wp| {
                if (wp == .string) wpath = wp.string;
            }
        }
        if (sid.len == 0) return SdkError.InvalidResponse;

        // Reserve map space first so nothing can fail once the session exists.
        self.sessions.ensureUnusedCapacity(1) catch return SdkError.AllocationFailed;
        self.replaced_sessions.ensureUnusedCapacity(1) catch return SdkError.AllocationFailed;

        // The result lives in the transport's arena, so the session keeps its own copies.
        const owned_sid = self.allocator.dupe(u8, sid) catch return SdkError.AllocationFailed;
        errdefer self.allocator.free(owned_sid);
        var owned_wpath: ?[]const u8 = null;
        if (wpath) |wp| owned_wpath = self.allocator.dupe(u8, wp) catch return SdkError.AllocationFailed;
        errdefer if (owned_wpath) |wp| self.allocator.free(wp);

        const sess = self.allocator.create(CopilotSession) catch return SdkError.AllocationFailed;
        sess.* = CopilotSession.init(self.allocator, owned_sid, owned_wpath, &self.transport.?, self.options.on_get_trace_context);
        sess.owns_strings = true;
        sess.hook_handler = config.on_hook;
        sess.shell_authentication_handler = config.on_shell_authentication;
        sess.memory_store_handler = config.on_memory_store;
        sess.user_input_handler = config.on_user_input;
        if (registration_id) |id| {
            sess.github_token_registration_id = id;
            sess.close_hook = .{ .ctx = self, .func = releaseTokenProvider };
        }

        if (self.sessions.fetchRemove(owned_sid)) |old| self.replaced_sessions.appendAssumeCapacity(old.value);
        self.sessions.putAssumeCapacity(owned_sid, sess);
        return sess;
    }

    // -------------------------------------------------------------------
    // GitHub token providers
    // -------------------------------------------------------------------

    /// Registers a GitHub token provider under a fresh UUID; returns the client-owned ID.
    fn registerTokenProvider(self: *CopilotClient, provider: types.GitHubTokenProvider) SdkError![]const u8 {
        const id = newRegistrationId(self.allocator) catch return SdkError.AllocationFailed;
        self.github_token_providers.put(id, provider) catch {
            self.allocator.free(id);
            return SdkError.AllocationFailed;
        };
        return id;
    }

    fn unregisterTokenProvider(self: *CopilotClient, id: []const u8) void {
        if (self.github_token_providers.fetchRemove(id)) |kv| self.allocator.free(kv.key);
    }

    /// Session close hook: drops the session's GitHub token provider registration.
    fn releaseTokenProvider(ctx: *anyopaque, sess: *CopilotSession) void {
        const self: *CopilotClient = @ptrCast(@alignCast(ctx));
        if (sess.github_token_registration_id) |id| {
            sess.github_token_registration_id = null;
            self.unregisterTokenProvider(id);
        }
    }

    // -------------------------------------------------------------------
    // Server-to-client requests and notifications
    // -------------------------------------------------------------------

    /// Answers server requests. Runs while the transport waits for a response
    /// (any request, or `CopilotSession.sendAndWait`).
    fn handleServerRequest(ctx: *anyopaque, arena: Allocator, method: []const u8, params: ?JsonValue) jsonrpc.ServerReply {
        const self: *CopilotClient = @ptrCast(@alignCast(ctx));
        const p: std.json.ObjectMap = blk: {
            if (params) |value| {
                if (value == .object) break :blk value.object;
            }
            break :blk std.json.ObjectMap.init(arena);
        };
        return self.dispatchServerRequest(arena, method, p) catch |err| replyError(@errorName(err));
    }

    fn dispatchServerRequest(self: *CopilotClient, arena: Allocator, method: []const u8, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        if (std.mem.eql(u8, method, "gitHubToken.getToken")) return self.answerGitHubToken(arena, params);
        if (std.mem.eql(u8, method, "shellAuthentication.request")) return self.answerShellAuthentication(arena, params);
        if (std.mem.eql(u8, method, "shellAuthentication.cancel")) {
            // Requests are answered synchronously, so none is pending any more.
            return .{ .result = .{ .object = std.json.ObjectMap.init(arena) } };
        }
        if (std.mem.eql(u8, method, "memoryStore.store")) return self.answerMemoryStore(params);
        if (std.mem.eql(u8, method, "userInput.request")) return self.answerUserInput(arena, params);
        if (std.mem.eql(u8, method, "hooks.invoke")) return self.answerHook(arena, params);
        if (std.mem.eql(u8, method, "network.resolveProxy")) return self.answerResolveProxy(arena, params);
        if (std.mem.eql(u8, method, "network.authenticateNegotiate")) return self.answerAuthenticateNegotiate(arena, params);
        if (std.mem.eql(u8, method, "network.authenticateBasic")) return self.answerAuthenticateBasic(arena, params);
        return .{ .failure = .{ .code = jsonrpc.METHOD_NOT_FOUND, .message = "Method not found" } };
    }

    /// `gitHubToken.getToken` {registrationId, host, sessionId?, reason} ->
    /// {kind: "token", accessToken, tokenType?, expiresIn} or {kind: "cancelled"}.
    fn answerGitHubToken(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        const registration_id = stringField(params, "registrationId") orelse return replyError("Invalid gitHubToken.getToken payload");
        const provider = self.github_token_providers.get(registration_id) orelse return replyError("GitHub token provider is unavailable");
        const outcome = try provider(arena, .{
            .host = stringField(params, "host") orelse "",
            .session_id = stringField(params, "sessionId"),
            .reason = stringField(params, "reason") orelse "initial",
        });

        var reply = std.json.ObjectMap.init(arena);
        switch (outcome) {
            .token => |token| {
                try reply.put("kind", .{ .string = "token" });
                try reply.put("accessToken", .{ .string = token.access_token });
                if (token.token_type) |token_type| try reply.put("tokenType", .{ .string = token_type });
                try reply.put("expiresIn", .{ .integer = token.expires_in });
            },
            .cancelled => try reply.put("kind", .{ .string = "cancelled" }),
        }
        return .{ .result = .{ .object = reply } };
    }

    /// `shellAuthentication.request` -> {action: "accept", value} or {action: "cancel"}.
    /// Without a handler (or when it fails) the request is cancelled. Answers are never logged.
    fn answerShellAuthentication(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        const session_id = stringField(params, "sessionId") orelse "";
        var response: types.ShellAuthenticationResponse = .cancel;
        if (self.sessions.get(session_id)) |sess| {
            if (sess.shell_authentication_handler) |handler| {
                response = handler(arena, .{
                    .session_id = session_id,
                    .request_id = stringField(params, "requestId") orelse "",
                    .program = stringField(params, "program") orelse "",
                    .prompt = stringField(params, "prompt") orelse "",
                    .input_kind = stringField(params, "inputKind") orelse types.ShellAuthenticationInputKind.secret,
                    .command = stringField(params, "command") orelse "",
                    .cwd = stringField(params, "cwd") orelse "",
                }) catch .cancel;
            }
        }

        var reply = std.json.ObjectMap.init(arena);
        switch (response) {
            .accept => |value| {
                try reply.put("action", .{ .string = "accept" });
                try reply.put("value", .{ .string = value });
            },
            .cancel => try reply.put("action", .{ .string = "cancel" }),
        }
        return .{ .result = .{ .object = reply } };
    }

    /// `memoryStore.store` {sessionId, subject, fact, citations, reason} -> null.
    fn answerMemoryStore(self: *CopilotClient, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        const session_id = stringField(params, "sessionId") orelse "";
        const sess = self.sessions.get(session_id) orelse return replyError("Memory store owner is unavailable");
        const handler = sess.memory_store_handler orelse return replyError("Memory store owner is unavailable");
        try handler(.{
            .session_id = session_id,
            .subject = stringField(params, "subject") orelse "",
            .fact = stringField(params, "fact") orelse "",
            .citations = stringField(params, "citations") orelse "",
            .reason = stringField(params, "reason") orelse "",
        });
        return .{ .result = .null };
    }

    /// `userInput.request` {sessionId, question, choices?, allowFreeform?, requestId?,
    /// imageAttachmentConstraints?, _meta?} -> {answer, wasFreeform, attachments?}.
    /// Attachments are sent only when the handler sets them.
    fn answerUserInput(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        const session_id = stringField(params, "sessionId") orelse return replyError("Invalid user input request payload");
        const sess = self.sessions.get(session_id) orelse return replyError("Session not found");
        const handler = sess.user_input_handler orelse return replyError("No user input handler registered");

        var request = types.UserInputRequest{
            .question = stringField(params, "question") orelse "",
            .request_id = stringField(params, "requestId"),
        };
        if (params.get("choices")) |choices| {
            if (choices == .array) {
                var options = std.ArrayList([]const u8).init(arena);
                for (choices.array.items) |choice| {
                    if (choice == .string) try options.append(choice.string);
                }
                request.options = options.items;
            }
        }
        if (params.get("allowFreeform")) |v| {
            if (v == .bool) request.allow_freeform = v.bool;
        }
        if (params.get("imageAttachmentConstraints")) |v| {
            request.image_attachment_constraints = try types.UserInputImageAttachmentConstraints.fromJson(arena, v);
        }
        if (params.get("_meta")) |v| {
            if (v == .object) request.meta = v;
        }

        const response = try handler(request, session_id);
        return .{ .result = try response.toJson(arena) };
    }

    /// `hooks.invoke` {sessionId, hookType, input} -> {output?}, including the
    /// `subagentStart` and `subagentStop` hooks.
    fn answerHook(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        const session_id = stringField(params, "sessionId") orelse return replyError("Invalid hooks invoke payload");
        const hook_type = stringField(params, "hookType") orelse return replyError("Invalid hooks invoke payload");
        const sess = self.sessions.get(session_id) orelse return replyError("Session not found");

        var reply = std.json.ObjectMap.init(arena);
        if (sess.hook_handler) |handler| {
            if (try handler(arena, hook_type, params.get("input") orelse .null, session_id)) |output| {
                try reply.put("output", output);
            }
        }
        return .{ .result = .{ .object = reply } };
    }

    /// `network.resolveProxy` {url} -> {proxy?}.
    fn answerResolveProxy(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        var reply = std.json.ObjectMap.init(arena);
        const network = self.options.network orelse types.NetworkOptions{};
        if (network.resolve_proxy) |resolve| {
            if (try resolve(arena, stringField(params, "url") orelse "")) |proxy| {
                try reply.put("proxy", .{ .string = proxy });
            }
        }
        return .{ .result = .{ .object = reply } };
    }

    /// `network.authenticateNegotiate` {spn} -> {token?}.
    fn answerAuthenticateNegotiate(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        var reply = std.json.ObjectMap.init(arena);
        const network = self.options.network orelse types.NetworkOptions{};
        if (network.authenticate_negotiate) |authenticate| {
            if (try authenticate(arena, stringField(params, "spn") orelse "")) |token| {
                try reply.put("token", .{ .string = token });
            }
        }
        return .{ .result = .{ .object = reply } };
    }

    /// `network.authenticateBasic` {proxy, realm?, previousRejected} -> {username?, password?}.
    fn answerAuthenticateBasic(self: *CopilotClient, arena: Allocator, params: std.json.ObjectMap) anyerror!jsonrpc.ServerReply {
        var reply = std.json.ObjectMap.init(arena);
        const network = self.options.network orelse types.NetworkOptions{};
        if (network.authenticate_basic) |authenticate| {
            var request = types.NetworkBasicAuthRequest{
                .proxy = stringField(params, "proxy") orelse "",
                .realm = stringField(params, "realm"),
            };
            if (params.get("previousRejected")) |v| {
                if (v == .bool) request.previous_rejected = v.bool;
            }
            const credentials = try authenticate(arena, request);
            if (credentials.username) |username| {
                try reply.put("username", .{ .string = username });
                if (credentials.password) |password| try reply.put("password", .{ .string = password });
            }
        }
        return .{ .result = .{ .object = reply } };
    }

    /// Routes `session.event` notifications to the owning session.
    fn handleNotification(ctx: *anyopaque, method: []const u8, params: ?JsonValue) void {
        const self: *CopilotClient = @ptrCast(@alignCast(ctx));
        if (!std.mem.eql(u8, method, "session.event")) return;
        const p = params orelse return;
        if (p != .object) return;
        const session_id = stringField(p.object, "sessionId") orelse return;
        const event_value = p.object.get("event") orelse return;
        if (event_value != .object) return;
        const sess = self.sessions.get(session_id) orelse return;

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const event = sessionEventFromJson(arena.allocator(), session_id, event_value);
        sess.emitEvent(event);

        // Tool calls arrive as broadcast events; answer those this session can run.
        if (std.mem.eql(u8, event.event_type, "external_tool.requested")) {
            // Listeners may have removed the session; look it up again.
            const owner = self.sessions.get(session_id) orelse return;
            owner.handleExternalToolRequest(arena.allocator(), event_value.object.get("data") orelse .null);
        }
    }
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// JSON-RPC "internal error" code used when a client callback fails.
const INTERNAL_ERROR: i64 = -32603;

fn replyError(message: []const u8) jsonrpc.ServerReply {
    return .{ .failure = .{ .code = INTERNAL_ERROR, .message = message } };
}

fn stringField(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

fn putValue(obj: *std.json.ObjectMap, key: []const u8, value: JsonValue) SdkError!void {
    obj.put(key, value) catch return SdkError.AllocationFailed;
}

fn putNonEmpty(obj: *std.json.ObjectMap, key: []const u8, value: ?[]const u8) SdkError!void {
    const v = value orelse return;
    if (v.len > 0) try putValue(obj, key, .{ .string = v });
}

/// Parses an option supplied as a JSON string (for example `mcp_servers_json`).
fn parseJsonOption(a: Allocator, text: []const u8) SdkError!JsonValue {
    return std.json.parseFromSliceLeaky(JsonValue, a, text, .{}) catch |err| switch (err) {
        error.OutOfMemory => SdkError.AllocationFailed,
        else => SdkError.InvalidConfig,
    };
}

fn stringArray(a: Allocator, items: []const []const u8) SdkError!JsonValue {
    var arr = std.json.Array.initCapacity(a, items.len) catch return SdkError.AllocationFailed;
    for (items) |item| arr.appendAssumeCapacity(.{ .string = item });
    return .{ .array = arr };
}

/// Tool definitions sent as `tools.definitions`, including the `idempotent` flag.
fn toolDefinitionsJson(a: Allocator, defs: []const types.ToolDefinition) SdkError!JsonValue {
    var arr = std.json.Array.init(a);
    for (defs) |def| {
        var obj = std.json.ObjectMap.init(a);
        try putValue(&obj, "name", .{ .string = def.name });
        if (def.description) |d| try putValue(&obj, "description", .{ .string = d });
        if (def.parameters_json) |text| try putValue(&obj, "parameters", try parseJsonOption(a, text));
        if (def.idempotent) |v| try putValue(&obj, "idempotent", .{ .bool = v });
        arr.append(.{ .object = obj }) catch return SdkError.AllocationFailed;
    }
    return .{ .array = arr };
}

/// The legacy single provider; `session_params` translates it to a named provider.
fn legacyProviderJson(a: Allocator, p: types.ProviderConfig) SdkError!JsonValue {
    var obj = std.json.ObjectMap.init(a);
    try putValue(&obj, "type", .{ .string = p.type_name });
    if (p.base_url) |v| try putValue(&obj, "baseUrl", .{ .string = v });
    if (p.api_key) |v| try putValue(&obj, "apiKey", .{ .string = v });
    if (p.api_key_command) |v| try putValue(&obj, "apiKeyCommand", .{ .string = v });
    if (p.bearer_token) |v| try putValue(&obj, "bearerToken", .{ .string = v });
    if (p.wire_api) |v| try putValue(&obj, "wireApi", .{ .string = v });
    return .{ .object = obj };
}

fn namedProvidersJson(a: Allocator, list: []const types.NamedProviderConfig) SdkError!JsonValue {
    var arr = std.json.Array.init(a);
    for (list) |p| {
        var obj = std.json.ObjectMap.init(a);
        try putValue(&obj, "name", .{ .string = p.name });
        if (p.type_name) |v| try putValue(&obj, "type", .{ .string = v });
        try putValue(&obj, "baseUrl", .{ .string = p.base_url });
        if (p.api_key) |v| try putValue(&obj, "apiKey", .{ .string = v });
        if (p.api_key_command) |v| try putValue(&obj, "apiKeyCommand", .{ .string = v });
        if (p.bearer_token) |v| try putValue(&obj, "bearerToken", .{ .string = v });
        if (p.wire_api) |v| try putValue(&obj, "wireApi", .{ .string = v });
        if (p.transport) |v| try putValue(&obj, "transport", .{ .string = v });
        if (p.azure_api_version) |v| {
            var azure = std.json.ObjectMap.init(a);
            try putValue(&azure, "apiVersion", .{ .string = v });
            try putValue(&obj, "azure", .{ .object = azure });
        }
        if (p.headers_json) |text| try putValue(&obj, "headers", try parseJsonOption(a, text));
        if (p.model_provider) |v| try putValue(&obj, "modelProvider", .{ .string = v });
        arr.append(.{ .object = obj }) catch return SdkError.AllocationFailed;
    }
    return .{ .array = arr };
}

fn providerModelsJson(a: Allocator, list: []const types.ProviderModelConfig) SdkError!JsonValue {
    var arr = std.json.Array.init(a);
    for (list) |m| {
        var obj = std.json.ObjectMap.init(a);
        try putValue(&obj, "provider", .{ .string = m.provider });
        try putValue(&obj, "id", .{ .string = m.id });
        if (m.model_id) |v| try putValue(&obj, "modelId", .{ .string = v });
        if (m.wire_model) |v| try putValue(&obj, "wireModel", .{ .string = v });
        if (m.name) |v| try putValue(&obj, "name", .{ .string = v });
        if (m.max_prompt_tokens) |v| try putValue(&obj, "maxPromptTokens", .{ .integer = v });
        if (m.max_context_window_tokens) |v| try putValue(&obj, "maxContextWindowTokens", .{ .integer = v });
        if (m.max_output_tokens) |v| try putValue(&obj, "maxOutputTokens", .{ .integer = v });
        if (m.capabilities_json) |text| try putValue(&obj, "capabilities", try parseJsonOption(a, text));
        arr.append(.{ .object = obj }) catch return SdkError.AllocationFailed;
    }
    return .{ .array = arr };
}

fn sandboxConfigJson(a: Allocator, sc: types.SandboxConfig) SdkError!JsonValue {
    var obj = std.json.ObjectMap.init(a);
    try putValue(&obj, "enabled", .{ .bool = sc.enabled });
    if (sc.user_policy_json) |text| try putValue(&obj, "userPolicy", try parseJsonOption(a, text));
    if (sc.add_current_working_directory) |v| try putValue(&obj, "addCurrentWorkingDirectory", .{ .bool = v });
    if (sc.sandbox_mcp_servers) |v| try putValue(&obj, "sandboxMcpServers", .{ .bool = v });
    if (sc.sandbox_lsp_servers) |v| try putValue(&obj, "sandboxLspServers", .{ .bool = v });
    if (sc.sandbox_extensions) |v| try putValue(&obj, "sandboxExtensions", .{ .bool = v });
    if (sc.allow_bypass) |v| try putValue(&obj, "allowBypass", .{ .bool = v });
    return .{ .object = obj };
}

/// Copilot API options (`github.capi`): `capi.enabled`, the Auto routing tier, and
/// the WebSocket Responses transport switch.
fn capiJson(a: Allocator, config: SessionConfig) SdkError!?JsonValue {
    var obj = std.json.ObjectMap.init(a);
    if (config.capi) |capi| {
        if (capi.enabled) |enabled| try putValue(&obj, "enabled", .{ .bool = enabled });
    }
    if (config.auto_tier) |tier| try putValue(&obj, "autoTier", .{ .string = tier });
    if (config.enable_web_socket_responses) |ws| try putValue(&obj, "enableWebSocketResponses", .{ .bool = ws });
    if (obj.count() == 0) return null;
    return .{ .object = obj };
}

/// Model capabilities override, with `max_output_tokens` merged into
/// `limits.max_output_tokens` (snake_case inside capabilities).
fn modelCapabilitiesJson(a: Allocator, config: SessionConfig) SdkError!?JsonValue {
    var caps: JsonValue = if (config.model_capabilities_json) |text|
        try parseJsonOption(a, text)
    else if (config.max_output_tokens != null)
        .{ .object = std.json.ObjectMap.init(a) }
    else
        return null;
    if (config.max_output_tokens) |max_tokens| {
        if (caps != .object) return SdkError.InvalidConfig;
        const limits = session_params.childObject(a, &caps.object, "limits") catch return SdkError.AllocationFailed;
        try putValue(limits, "max_output_tokens", .{ .integer = max_tokens });
    }
    return caps;
}

/// Lifecycle RPCs that still answer `{success: false}` instead of a JSON-RPC error.
fn expectSuccess(result: JsonValue) SdkError!void {
    if (result != .object) return;
    const success = result.object.get("success") orelse return;
    if (success == .bool and !success.bool) return SdkError.RequestFailed;
}

/// Random RFC 4122 version 4 UUID string (caller frees).
fn newRegistrationId(allocator: Allocator) ![]u8 {
    var bytes: [16]u8 = undefined;
    std.crypto.random.bytes(&bytes);
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;
    const hex = std.fmt.bytesToHex(bytes, .lower);
    return std.fmt.allocPrint(allocator, "{s}-{s}-{s}-{s}-{s}", .{ hex[0..8], hex[8..12], hex[12..16], hex[16..20], hex[20..32] });
}

/// Builds a `SessionEvent` from a `session.event` notification's `event` object.
/// Strings reference `value`; `raw_json` is allocated from `a`.
fn sessionEventFromJson(a: Allocator, session_id: []const u8, value: JsonValue) SessionEvent {
    const obj = value.object;
    var event = SessionEvent{
        .id = stringField(obj, "id") orelse "",
        .timestamp = stringField(obj, "timestamp") orelse "",
        .parent_id = stringField(obj, "parentId"),
        .agent_id = stringField(obj, "agentId"),
        .session_id = session_id,
        .event_type = stringField(obj, "type") orelse "unknown",
        .raw_json = std.json.stringifyAlloc(a, value, .{}) catch null,
    };
    if (obj.get("ephemeral")) |v| {
        if (v == .bool) event.ephemeral = v.bool;
    }
    if (obj.get("data")) |data| {
        if (data == .object) {
            event.content = stringField(data.object, "content");
            event.delta_content = stringField(data.object, "deltaContent");
        }
        if (event.isShellOutput()) event.shell_output = types.ToolShellOutputData.fromJson(data);
    }
    return event;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "CopilotClient init and deinit" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, client.getState());
}

test "CopilotClient default options" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{
        .cli_path = "/usr/bin/copilot-cli",
    });
    defer client.deinit();
    try std.testing.expect(client.options.use_stdio);
    try std.testing.expect(client.options.auto_start);
}

test "CopilotClient lifecycle callback fires on state change" {
    const allocator = std.testing.allocator;

    const S = struct {
        var last_state: ConnectionState = .disconnected;
        fn handler(state: ConnectionState) void {
            last_state = state;
        }
    };

    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();

    client.onLifecycle(S.handler);
    client.setState(.connecting);
    try std.testing.expectEqual(ConnectionState.connecting, S.last_state);
}

test "CopilotClient getState returns current state" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, client.getState());
}

test "CopilotClient operations fail when not connected" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();

    const result = client.createSession(.{});
    try std.testing.expectError(SdkError.NotConnected, result);
}

fn testTokenProvider(_: Allocator, args: types.GitHubTokenProviderArgs) anyerror!types.GitHubTokenResult {
    if (!std.mem.eql(u8, args.reason, "refresh")) return .cancelled;
    return .{ .token = .{ .access_token = "gho_test", .expires_in = 60 } };
}

fn testMemoryStore(_: types.StoredMemory) anyerror!void {}

fn testUserInput(_: types.UserInputRequest, _: []const u8) anyerror!types.UserInputResponse {
    return .{ .answer = "ok" };
}

/// Frames a JSON-RPC message the way the CLI sends it.
fn framed(comptime body: []const u8) []const u8 {
    return std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n{s}", .{ body.len, body });
}

/// Parses the bodies of the framed messages a test transport wrote.
fn writtenMessages(a: Allocator, written: []const u8) ![]JsonValue {
    var messages = std.ArrayList(JsonValue).init(a);
    var rest = written;
    while (std.mem.indexOf(u8, rest, "\r\n\r\n")) |separator| {
        const header = rest[0..separator];
        const length = try std.fmt.parseInt(usize, std.mem.trim(u8, header["Content-Length:".len..], " "), 10);
        const body = rest[separator + 4 .. separator + 4 + length];
        try messages.append(try std.json.parseFromSliceLeaky(JsonValue, a, body, .{}));
        rest = rest[separator + 4 + length ..];
    }
    return messages.items;
}

fn jsonPath(value: JsonValue, path: []const []const u8) ?JsonValue {
    var current = value;
    for (path) |key| {
        if (current != .object) return null;
        current = current.object.get(key) orelse return null;
    }
    return current;
}

test "CopilotClient validates empty mode and token options" {
    var client = CopilotClient.init(std.testing.allocator, .{ .mode = .empty });
    defer client.deinit();

    try std.testing.expect(client.sessionConfigError(.{}) != null);
    try std.testing.expect(client.sessionConfigError(.{ .available_tools = &.{"view"} }) == null);
    try std.testing.expect(client.sessionConfigError(.{
        .available_tools = &.{"view"},
        .github_token = "gho_static",
        .github_token_provider = testTokenProvider,
    }) != null);
}

test "createSession and resumeSession send grouped protocol 4 payloads" {
    const allocator = std.testing.allocator;
    const responses = comptime framed("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"sessionId\":\"s1\"}}") ++
        framed("{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"success\":true}}") ++
        framed("{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"sessionId\":\"s1\"}}");
    var input = std.io.fixedBufferStream(responses);
    const input_reader = input.reader();
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const output_writer = output.writer();

    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    client.transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    client.state = .connected;

    const created = try client.createSession(.{
        .model = "gpt-5",
        .tools = &.{.{ .name = "lookup", .idempotent = true }},
        .trust_working_directory = types.TrustWorkingDirectory.session,
        .auto_tier = types.AutoTier.fast,
        .github_token_provider = testTokenProvider,
        .on_memory_store = testMemoryStore,
        .on_user_input = testUserInput,
        .max_output_tokens = 1000,
    });
    try std.testing.expectEqual(@as(u32, 1), client.github_token_providers.count());
    try created.detach();
    try std.testing.expectEqual(@as(u32, 0), client.github_token_providers.count());

    const resumed = try client.resumeSession("s1", .{
        .streaming = false,
        .suppress_resume_event = true,
        .trust_working_directory = types.TrustWorkingDirectory.session,
        .clear_default_provider = true,
    });
    try std.testing.expectEqualStrings("s1", resumed.getId());

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const messages = try writtenMessages(arena.allocator(), output.items);
    try std.testing.expectEqual(@as(usize, 3), messages.len);

    try std.testing.expectEqualStrings("session.create", messages[0].object.get("method").?.string);
    const create = messages[0].object.get("params").?;
    try std.testing.expect(create.object.get("streaming") == null);
    try std.testing.expect(jsonPath(create, &.{ "events", "streaming" }).?.bool);
    const definitions = jsonPath(create, &.{ "tools", "definitions" }).?.array.items;
    try std.testing.expect(definitions[0].object.get("idempotent").?.bool);
    try std.testing.expectEqualStrings("session", jsonPath(create, &.{ "filesystem", "trustWorkingDirectory" }).?.string);
    try std.testing.expectEqualStrings("fast", jsonPath(create, &.{ "github", "capi", "autoTier" }).?.string);
    try std.testing.expect(jsonPath(create, &.{ "github", "tokenProviderRegistrationId" }).? == .string);
    try std.testing.expect(jsonPath(create, &.{ "state", "hasMemoryStore" }).?.bool);
    try std.testing.expect(jsonPath(create, &.{ "interaction", "requestUserInput" }).?.bool);
    try std.testing.expectEqual(@as(i64, 1000), jsonPath(create, &.{ "inference", "capabilities", "limits", "max_output_tokens" }).?.integer);

    try std.testing.expectEqualStrings("session.detach", messages[1].object.get("method").?.string);

    try std.testing.expectEqualStrings("session.resume", messages[2].object.get("method").?.string);
    const resume_params = messages[2].object.get("params").?;
    try std.testing.expectEqualStrings("s1", resume_params.object.get("sessionId").?.string);
    try std.testing.expect(!jsonPath(resume_params, &.{ "events", "streaming" }).?.bool);
    try std.testing.expect(resume_params.object.get("suppressResumeEvent").?.bool);
    try std.testing.expect(jsonPath(resume_params, &.{ "inference", "defaultProvider" }).? == .null);
    try std.testing.expect(resume_params.object.get("filesystem") == null);
}

test "handleServerRequest answers token, shell authentication, and network requests" {
    const allocator = std.testing.allocator;
    const S = struct {
        fn shell(_: Allocator, request: types.ShellAuthenticationRequest) anyerror!types.ShellAuthenticationResponse {
            if (std.mem.eql(u8, request.input_kind, types.ShellAuthenticationInputKind.confirm)) return .{ .accept = "yes" };
            return .cancel;
        }
        fn proxy(_: Allocator, url: []const u8) anyerror!?[]const u8 {
            return if (std.mem.startsWith(u8, url, "https://")) "PROXY proxy.example:8080" else null;
        }
    };

    var input_buf: [1]u8 = undefined;
    var input = std.io.fixedBufferStream(&input_buf);
    const input_reader = input.reader();
    var output_buf: [1]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buf);
    const output_writer = output.writer();
    var transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    defer transport.deinit();

    var client = CopilotClient.init(allocator, .{ .network = .{ .resolve_proxy = S.proxy } });
    defer client.deinit();
    const registration_id = try client.registerTokenProvider(testTokenProvider);

    var session = CopilotSession.init(allocator, "s1", null, &transport, null);
    defer session.deinit();
    session.shell_authentication_handler = S.shell;
    try client.sessions.put("s1", &session);
    defer _ = client.sessions.remove("s1");

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const token_params = try std.fmt.allocPrint(a, "{{\"registrationId\":\"{s}\",\"host\":\"github.com\",\"reason\":\"refresh\"}}", .{registration_id});
    const token = CopilotClient.handleServerRequest(&client, a, "gitHubToken.getToken", try std.json.parseFromSliceLeaky(JsonValue, a, token_params, .{}));
    try std.testing.expectEqualStrings("token", token.result.object.get("kind").?.string);
    try std.testing.expectEqualStrings("gho_test", token.result.object.get("accessToken").?.string);
    try std.testing.expectEqual(@as(i64, 60), token.result.object.get("expiresIn").?.integer);

    const shell = CopilotClient.handleServerRequest(&client, a, "shellAuthentication.request", try std.json.parseFromSliceLeaky(JsonValue, a,
        \\{"sessionId":"s1","requestId":"r1","program":"sudo","prompt":"Continue?","inputKind":"confirm","command":"sudo ls","cwd":"/"}
    , .{}));
    try std.testing.expectEqualStrings("accept", shell.result.object.get("action").?.string);
    try std.testing.expectEqualStrings("yes", shell.result.object.get("value").?.string);

    const proxy = CopilotClient.handleServerRequest(&client, a, "network.resolveProxy", try std.json.parseFromSliceLeaky(JsonValue, a,
        \\{"url":"https://github.com"}
    , .{}));
    try std.testing.expectEqualStrings("PROXY proxy.example:8080", proxy.result.object.get("proxy").?.string);

    const unknown = CopilotClient.handleServerRequest(&client, a, "bogus.method", null);
    try std.testing.expectEqual(jsonrpc.METHOD_NOT_FOUND, unknown.failure.code);
}

test "session.event notifications carry tool.shell_output data" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const value = try std.json.parseFromSliceLeaky(JsonValue, a,
        \\{"id":"e1","timestamp":"t","type":"tool.shell_output","ephemeral":true,"data":{"toolCallId":"c1","text":"hi\n","sequence":3,"stream":"stderr"}}
    , .{});
    const event = sessionEventFromJson(a, "s1", value);
    try std.testing.expect(event.isShellOutput());
    try std.testing.expect(event.ephemeral);
    const data = event.shell_output.?;
    try std.testing.expectEqualStrings("c1", data.tool_call_id);
    try std.testing.expectEqualStrings("hi\n", data.text);
    try std.testing.expectEqual(@as(i64, 3), data.sequence);
    try std.testing.expectEqualStrings("stderr", data.stream.?);
}

test "MCP secret store and extension approval requests send camelCase params" {
    const allocator = std.testing.allocator;
    const responses = comptime framed("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"usedKeychain\":true}}") ++
        framed("{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":null}") ++
        framed("{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"approvals\":[{\"extensionName\":\"plugin:p:e\",\"pluginName\":\"p\",\"capabilities\":[\"network\",\"fs\"],\"grantedAt\":\"2026-11-24T00:00:00Z\",\"installationKey\":\"k1\"}]}}") ++
        framed("{\"jsonrpc\":\"2.0\",\"id\":4,\"result\":{\"revoked\":true}}");
    var input = std.io.fixedBufferStream(responses);
    const input_reader = input.reader();
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const output_writer = output.writer();

    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    client.transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    client.state = .connected;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const saved = try client.mcpSecretsSave("my-token", "test-value", "/cfg");
    try std.testing.expect(saved.used_keychain);
    try client.mcpSecretsDeleteServer("my-server", null);
    const listed = try client.extensionApprovalsList(a, "s1");
    try std.testing.expectEqual(@as(usize, 1), listed.approvals.len);
    const approval = listed.approvals[0];
    try std.testing.expectEqualStrings("fs", approval.capabilities[1]);
    try std.testing.expect(approval.installation_label == null);
    // The approval is copied into the arena, so it outlives the next request.
    const revoked = try client.extensionApprovalsRevoke(approval.extension_name, approval.installation_key, null);
    try std.testing.expect(revoked.revoked);

    const messages = try writtenMessages(a, output.items);
    try std.testing.expectEqual(@as(usize, 4), messages.len);
    try std.testing.expectEqualStrings("mcp.secrets.save", messages[0].object.get("method").?.string);
    const save_params = messages[0].object.get("params").?.object;
    try std.testing.expectEqualStrings("my-token", save_params.get("secretId").?.string);
    try std.testing.expectEqualStrings("test-value", save_params.get("value").?.string);
    try std.testing.expectEqualStrings("/cfg", save_params.get("configDir").?.string);
    try std.testing.expectEqualStrings("mcp.secrets.deleteServer", messages[1].object.get("method").?.string);
    const delete_params = messages[1].object.get("params").?.object;
    try std.testing.expectEqualStrings("my-server", delete_params.get("serverName").?.string);
    try std.testing.expect(delete_params.get("configDir") == null);
    try std.testing.expectEqualStrings("permissions.extensionApprovals.list", messages[2].object.get("method").?.string);
    try std.testing.expectEqualStrings("s1", messages[2].object.get("params").?.object.get("sessionId").?.string);
    try std.testing.expectEqualStrings("permissions.extensionApprovals.revoke", messages[3].object.get("method").?.string);
    const revoke_params = messages[3].object.get("params").?.object;
    try std.testing.expectEqualStrings("plugin:p:e", revoke_params.get("extensionName").?.string);
    try std.testing.expectEqualStrings("k1", revoke_params.get("installationKey").?.string);
    try std.testing.expect(revoke_params.get("sessionId") == null);
}

test "userInput.request passes image constraints through and sends attachments only when set" {
    const allocator = std.testing.allocator;
    const S = struct {
        fn answer(request: types.UserInputRequest, _: []const u8) anyerror!types.UserInputResponse {
            const constraints = request.image_attachment_constraints orelse return .{ .answer = "typed answer" };
            try std.testing.expectEqualStrings("image/png", constraints.allowed_mime_types[0]);
            try std.testing.expectEqual(@as(i64, 1048576), constraints.max_bytes_per_image);
            try std.testing.expectEqual(@as(i64, 2), constraints.max_count);
            try std.testing.expectEqualStrings("ui-1", request.request_id.?);
            try std.testing.expect(request.meta.? == .object);
            return .{ .answer = "", .attachments = &.{.{ .data = "aGVsbG8=", .mime_type = "image/png", .display_name = "shot.png" }} };
        }
    };

    var input_buf: [1]u8 = undefined;
    var input = std.io.fixedBufferStream(&input_buf);
    const input_reader = input.reader();
    var output_buf: [1]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buf);
    const output_writer = output.writer();
    var transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    defer transport.deinit();

    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();

    var session = CopilotSession.init(allocator, "s1", null, &transport, null);
    defer session.deinit();
    session.registerUserInputHandler(S.answer);
    try client.sessions.put("s1", &session);
    defer _ = client.sessions.remove("s1");

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const with_images = CopilotClient.handleServerRequest(&client, a, "userInput.request", try std.json.parseFromSliceLeaky(JsonValue, a,
        \\{"sessionId":"s1","question":"Attach a screenshot?","requestId":"ui-1","imageAttachmentConstraints":{"allowedMimeTypes":["image/png"],"maxBytesPerImage":1048576,"maxCount":2},"_meta":{"ext":{"a":1}}}
    , .{}));
    const reply = with_images.result.object;
    try std.testing.expectEqualStrings("", reply.get("answer").?.string);
    try std.testing.expect(reply.get("wasFreeform").?.bool);
    const image = reply.get("attachments").?.array.items[0].object;
    try std.testing.expectEqualStrings("image", image.get("type").?.string);
    try std.testing.expectEqualStrings("aGVsbG8=", image.get("data").?.string);
    try std.testing.expectEqualStrings("image/png", image.get("mimeType").?.string);
    try std.testing.expectEqualStrings("shot.png", image.get("displayName").?.string);
    try std.testing.expect(image.get("description") == null);

    const plain = CopilotClient.handleServerRequest(&client, a, "userInput.request", try std.json.parseFromSliceLeaky(JsonValue, a,
        \\{"sessionId":"s1","question":"Continue?","choices":["yes","no"]}
    , .{}));
    try std.testing.expectEqualStrings("typed answer", plain.result.object.get("answer").?.string);
    try std.testing.expect(plain.result.object.get("attachments") == null);
}
