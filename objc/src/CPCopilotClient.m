/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPCopilotClient.h"
#import "CPJsonRpcClient.h"
#import "CPSdkProtocolVersion.h"
#import "CPSessionParams.h"
#import "CPTypes.h"

static NSString *const kCPClientErrorDomain = @"CPCopilotClientErrorDomain";

/// Answers a server-to-client JSON-RPC request with a result or an error dictionary.
typedef void (^CPRpcResponder)(NSDictionary * _Nullable result, NSDictionary * _Nullable error);

static NSString *CPStringOrNil(id value) {
    return [value isKindOfClass:[NSString class]] ? value : nil;
}

static NSDictionary *CPRpcError(NSString *message) {
    return @{@"code": @(-32603), @"message": message};
}

static NSError *CPClientError(NSInteger code, NSString *message) {
    return [NSError errorWithDomain:kCPClientErrorDomain
                               code:code
                           userInfo:@{NSLocalizedDescriptionKey: message}];
}

/// Lifecycle RPCs that answer `{success: false, error}` instead of a JSON-RPC error.
static NSError *CPLifecycleFailure(NSDictionary *result, NSString *what) {
    id success = result[@"success"];
    if (![success isKindOfClass:[NSNumber class]] || [success boolValue]) return nil;
    NSString *message = CPStringOrNil(result[@"error"]) ?: @"Unknown error";
    return CPClientError(-7, [NSString stringWithFormat:@"%@: %@", what, message]);
}

/// Attaches the Policy Helper failure carried in the JSON-RPC error data, if any.
static NSError *CPErrorWithPolicyHelperFailure(NSError *error) {
    if (error.userInfo[CPPolicyHelperFailureErrorKey]) return error;
    CPPolicyHelperFailure *failure = [CPPolicyHelperFailure failureFromError:error];
    if (!failure) return error;
    NSMutableDictionary *userInfo = [NSMutableDictionary dictionaryWithDictionary:error.userInfo ?: @{}];
    userInfo[CPPolicyHelperFailureErrorKey] = failure;
    return [NSError errorWithDomain:error.domain code:error.code userInfo:userInfo];
}

@interface CPCopilotSession ()

@property (nonatomic, copy, readwrite) NSString *sessionId;
@property (nonatomic, copy, readwrite, nullable) NSString *workspacePath;
@property (nonatomic, copy, nullable) CPSessionConfig *config;
@property (nonatomic, copy, nullable) void (^onRelease)(void);

- (instancetype)initWithSessionId:(NSString *)sessionId
                    workspacePath:(nullable NSString *)workspacePath
                        rpcClient:(CPJsonRpcClient *)rpcClient
                           config:(nullable CPSessionConfig *)config;

- (void)dispatchEvent:(CPSessionEvent *)event;
- (void)handleBroadcastEvent:(CPSessionEvent *)event;

@end

@interface CPCopilotClient ()

@property (nonatomic, strong) CPCopilotClientOptions *options;
@property (nonatomic, strong, nullable) NSTask *cliProcess;
@property (nonatomic, strong, nullable) NSFileHandle *stderrHandle;
@property (nonatomic, strong, nullable) CPJsonRpcClient *rpcClient;
@property (nonatomic, strong) NSMutableDictionary<NSString *, CPCopilotSession *> *sessions;
/// GitHub token providers keyed by registration ID.
@property (nonatomic, strong) NSMutableDictionary<NSString *, CPGitHubTokenProvider> *tokenProviders;
/// Unanswered shell authentication requests keyed by "sessionId/requestId".
@property (nonatomic, strong) NSMutableDictionary<NSString *, CPRpcResponder> *pendingShellAuthentications;
@property (nonatomic, assign, readwrite) CPConnectionState connectionState;
@property (nonatomic, strong) dispatch_queue_t clientQueue;

@end

@implementation CPCopilotClient

- (instancetype)initWithOptions:(nullable CPCopilotClientOptions *)options {
    self = [super init];
    if (self) {
        _options = options ?: [CPCopilotClientOptions defaultOptions];
        _sessions = [NSMutableDictionary dictionary];
        _tokenProviders = [NSMutableDictionary dictionary];
        _pendingShellAuthentications = [NSMutableDictionary dictionary];
        _connectionState = CPConnectionStateDisconnected;
        _clientQueue = dispatch_queue_create("com.github.copilot.client", DISPATCH_QUEUE_SERIAL);
    }
    return self;
}

- (BOOL)isEmptyMode {
    return [self.options.mode isEqualToString:CPClientModeEmpty];
}

#pragma mark - Start / Stop

- (void)startWithCompletion:(void (^)(NSError * _Nullable))completion {
    dispatch_async(self.clientQueue, ^{
        if (self.connectionState == CPConnectionStateConnected) {
            completion(nil);
            return;
        }

        NSString *mode = self.options.mode;
        if (mode && ![mode isEqualToString:CPClientModeCodingAgent] &&
            ![mode isEqualToString:CPClientModeCopilotCli] && ![mode isEqualToString:CPClientModeEmpty]) {
            completion(CPClientError(-8, [NSString stringWithFormat:
                @"Invalid client mode \"%@\": use \"coding-agent\" or \"empty\"", mode]));
            return;
        }

        self.connectionState = CPConnectionStateConnecting;

        // If cliUrl is set, connect to external server
        if (self.options.cliUrl) {
            [self connectToExternalServerWithCompletion:completion];
            return;
        }

        // Spawn CLI process
        NSError *spawnError = nil;
        if (![self spawnCLIProcess:&spawnError]) {
            self.connectionState = CPConnectionStateError;
            completion(spawnError);
            return;
        }

        // connect (clientInfo) -> ping (protocol check) -> network callbacks
        [self sendConnectHandshakeWithCompletion:^(NSError *connectError) {
            if (connectError) {
                [self failStartWithError:connectError completion:completion];
                return;
            }
            [self verifyProtocolVersionWithCompletion:^(NSError *versionError) {
                if (versionError) {
                    [self failStartWithError:versionError completion:completion];
                    return;
                }
                [self registerNetworkProviderWithCompletion:^(NSError *networkError) {
                    if (networkError) {
                        [self failStartWithError:networkError completion:completion];
                        return;
                    }

                    self.connectionState = CPConnectionStateConnected;

                    // Set up session filesystem provider if configured
                    if (self.options.sessionFs && self.options.sessionFs.initialCwd) {
                        NSDictionary *fsParams = [self.options.sessionFs toDictionary];
                        [self.rpcClient sendRequest:@"sessionFs.setProvider"
                                             params:fsParams
                                         completion:^(NSDictionary *fsResp, NSError *fsErr) {
                            // Provider set; ignore errors silently
                        }];
                    }

                    completion(nil);
                }];
            }];
        }];
    });
}

- (void)failStartWithError:(NSError *)error completion:(void (^)(NSError * _Nullable))completion {
    self.connectionState = CPConnectionStateError;
    completion(error);
}

/// Declares the integrating application (`clientInfo`) with `connect` before the ping
/// handshake. Skipped when no clientInfo field is set; CLIs without `connect` are accepted.
- (void)sendConnectHandshakeWithCompletion:(void (^)(NSError * _Nullable))completion {
    NSDictionary *clientInfo = [self.options.clientInfo toDictionary];
    if (clientInfo.count == 0) {
        completion(nil);
        return;
    }
    [self.rpcClient sendRequest:@"connect"
                         params:@{@"clientInfo": clientInfo}
                     completion:^(NSDictionary *result, NSError *error) {
        completion(CPJsonRpcIsMethodNotFoundError(error) ? nil : error);
    }];
}

/// Accepts exactly SDK protocol 4 (Copilot CLI 1.0.113 or newer).
- (void)verifyProtocolVersionWithCompletion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"ping"
                         params:@{@"message": @"sdk-init"}
                     completion:^(NSDictionary *response, NSError *error) {
        if (error) {
            completion(error);
            return;
        }
        id reported = response[@"protocolVersion"];
        if (![reported isKindOfClass:[NSNumber class]]) {
            completion(CPClientError(-4, [NSString stringWithFormat:
                @"SDK protocol version mismatch: SDK supports version %ld, but server does not report a protocol version. Please update your SDK or server to ensure compatibility.",
                (long)CPSdkProtocolVersion]));
            return;
        }
        NSInteger serverVersion = [reported integerValue];
        if (serverVersion != CPSdkProtocolVersion) {
            completion(CPClientError(-4, [NSString stringWithFormat:
                @"SDK protocol version mismatch: SDK supports version %ld, but server reports version %ld. Please update your SDK or server to ensure compatibility.",
                (long)CPSdkProtocolVersion, (long)serverVersion]));
            return;
        }
        completion(nil);
    }];
}

/// Registers the configured network callbacks with `network.setProvider`; each present
/// member (an empty object) marks one registered callback.
- (void)registerNetworkProviderWithCompletion:(void (^)(NSError * _Nullable))completion {
    CPNetworkOptions *network = self.options.network;
    if (!network.resolveProxy && !network.authenticateNegotiate && !network.authenticateBasic) {
        completion(nil);
        return;
    }
    NSMutableDictionary *params = [NSMutableDictionary dictionary];
    if (network.scope) params[@"scope"] = network.scope;
    if (network.resolveProxy) params[@"resolveProxy"] = @{};
    if (network.authenticateNegotiate) params[@"authenticateNegotiate"] = @{};
    if (network.authenticateBasic) params[@"authenticateBasic"] = @{};
    [self.rpcClient sendRequest:@"network.setProvider"
                         params:params
                     completion:^(NSDictionary *result, NSError *error) {
        completion(error);
    }];
}

- (BOOL)spawnCLIProcess:(NSError **)error {
    NSString *cliPath = self.options.cliPath ?: @"copilot";

    NSMutableArray<NSString *> *cliArgs = [NSMutableArray arrayWithObjects:@"--headless", @"--no-auto-update", nil];
    [cliArgs addObject:@"--log-level"];
    [cliArgs addObject:self.options.logLevel ?: @"info"];
    [cliArgs addObject:@"--stdio"];

    if (self.options.extraArgs) {
        [cliArgs addObjectsFromArray:self.options.extraArgs];
    }

    NSTask *task = [[NSTask alloc] init];
    if ([cliPath.pathExtension isEqualToString:@"js"]) {
        // JavaScript entry points (for example @github/copilot/npm-loader.js) run under node.
        task.executableURL = [NSURL fileURLWithPath:@"/usr/bin/env"];
        task.arguments = [@[@"node", cliPath] arrayByAddingObjectsFromArray:cliArgs];
    } else if (cliPath.isAbsolutePath) {
        task.executableURL = [NSURL fileURLWithPath:cliPath];
        task.arguments = cliArgs;
    } else {
        // Bare command names (and relative paths) are resolved by /usr/bin/env.
        task.executableURL = [NSURL fileURLWithPath:@"/usr/bin/env"];
        task.arguments = [@[cliPath] arrayByAddingObjectsFromArray:cliArgs];
    }

    if (self.options.cwd) {
        task.currentDirectoryURL = [NSURL fileURLWithPath:self.options.cwd isDirectory:YES];
    }

    // Set up environment
    NSMutableDictionary<NSString *, NSString *> *env =
        [NSMutableDictionary dictionaryWithDictionary:[[NSProcessInfo processInfo] environment]];
    if (self.options.githubToken) {
        env[@"GITHUB_TOKEN"] = self.options.githubToken;
    }
    task.environment = env;

    // Pipes for stdio
    NSPipe *stdinPipe = [NSPipe pipe];
    NSPipe *stdoutPipe = [NSPipe pipe];
    NSPipe *stderrPipe = [NSPipe pipe];
    task.standardInput = stdinPipe;
    task.standardOutput = stdoutPipe;
    task.standardError = stderrPipe;

    // Set up JSON-RPC client
    self.rpcClient = [[CPJsonRpcClient alloc] init];

    // Register handlers for server-to-client calls
    [self registerServerHandlers];

    // Handle notifications (session events)
    __weak typeof(self) weakSelf = self;
    self.rpcClient.notificationHandler = ^(NSString *method, NSDictionary<NSString *, id> *params) {
        [weakSelf handleNotification:method params:params];
    };

    [self.rpcClient startWithReadHandle:[stdoutPipe fileHandleForReading]
                            writeHandle:[stdinPipe fileHandleForWriting]];

    // Handle stderr (logging)
    self.stderrHandle = [stderrPipe fileHandleForReading];
    self.stderrHandle.readabilityHandler = ^(NSFileHandle *handle) {
        NSData *data = [handle availableData];
        if (data.length > 0) {
            NSString *str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            if (str) NSLog(@"[CopilotCLI] %@", str);
        }
    };

    NSError *launchError = nil;
    if (![task launchAndReturnError:&launchError]) {
        self.stderrHandle.readabilityHandler = nil;
        [self.rpcClient stop];
        if (error) *error = launchError;
        return NO;
    }

    self.cliProcess = task;

    // Handle termination
    task.terminationHandler = ^(NSTask *t) {
        CPCopilotClient *strongSelf = weakSelf;
        if (!strongSelf) return;
        dispatch_async(strongSelf.clientQueue, ^{
            if (weakSelf.connectionState == CPConnectionStateConnected) {
                weakSelf.connectionState = CPConnectionStateDisconnected;
                if (weakSelf.options.autoRestart) {
                    [weakSelf startWithCompletion:^(NSError *err) {
                        if (err) NSLog(@"[CPCopilotClient] Auto-restart failed: %@", err);
                    }];
                }
            }
        });
    };

    return YES;
}

- (void)connectToExternalServerWithCompletion:(void (^)(NSError * _Nullable))completion {
    // For external server connections via cliUrl, we would use NSURLSession
    // for HTTP-based JSON-RPC. This is a simplified implementation.
    NSError *error = [NSError errorWithDomain:kCPClientErrorDomain
                                         code:-1
                                     userInfo:@{NSLocalizedDescriptionKey: @"External server (cliUrl) connections are not yet implemented. Use stdio mode."}];
    completion(error);
}

- (void)stopWithCompletion:(void (^)(NSError * _Nullable))completion {
    dispatch_async(self.clientQueue, ^{
        [self.rpcClient stop];
        self.stderrHandle.readabilityHandler = nil;
        self.stderrHandle = nil;

        if (self.cliProcess && self.cliProcess.isRunning) {
            [self.cliProcess terminate];
        }

        self.cliProcess = nil;
        self.connectionState = CPConnectionStateDisconnected;

        @synchronized (self.sessions) {
            [self.sessions removeAllObjects];
        }
        @synchronized (self.tokenProviders) {
            [self.tokenProviders removeAllObjects];
        }
        @synchronized (self.pendingShellAuthentications) {
            [self.pendingShellAuthentications removeAllObjects];
        }

        completion(nil);
    });
}

#pragma mark - Server Handlers

- (void)registerServerHandlers {
    __weak typeof(self) weakSelf = self;

    // Handle tool invocations from the server
    [self.rpcClient registerRequestHandler:@"tool/invoke"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleToolInvocation:params respond:respond];
    }];

    // Handle permission requests
    [self.rpcClient registerRequestHandler:@"session/permission"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handlePermissionRequest:params respond:respond];
    }];

    // Handle exit plan mode requests
    [self.rpcClient registerRequestHandler:@"exitPlanMode.request"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleExitPlanModeRequest:params respond:respond];
    }];

    // GitHub token providers (CPSessionConfig.gitHubTokenProvider)
    [self.rpcClient registerRequestHandler:@"gitHubToken.getToken"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleGitHubTokenRequest:params respond:respond];
    }];

    // Shell authentication (CPSessionConfig.onShellAuthentication)
    [self.rpcClient registerRequestHandler:@"shellAuthentication.request"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleShellAuthenticationRequest:params respond:respond];
    }];
    [self.rpcClient registerRequestHandler:@"shellAuthentication.cancel"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf cancelShellAuthenticationRequest:params];
        respond(@{}, nil);
    }];

    // Memory store (CPSessionConfig.onMemoryStore)
    [self.rpcClient registerRequestHandler:@"memoryStore.store"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleMemoryStoreRequest:params respond:respond];
    }];

    // Session hooks, including subagentStart / subagentStop (CPSessionConfig.hooks)
    [self.rpcClient registerRequestHandler:@"hooks.invoke"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleHooksInvoke:params respond:respond];
    }];

    // Network callbacks (CPCopilotClientOptions.network)
    [self.rpcClient registerRequestHandler:@"network.resolveProxy"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleResolveProxy:params respond:respond];
    }];
    [self.rpcClient registerRequestHandler:@"network.authenticateNegotiate"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleAuthenticateNegotiate:params respond:respond];
    }];
    [self.rpcClient registerRequestHandler:@"network.authenticateBasic"
                                   handler:^(NSDictionary<NSString *, id> *params,
                                             void (^respond)(NSDictionary *, NSDictionary *)) {
        [weakSelf handleAuthenticateBasic:params respond:respond];
    }];
}

- (nullable CPCopilotSession *)sessionWithId:(nullable NSString *)sessionId {
    if (!sessionId) return nil;
    CPCopilotSession *session = nil;
    @synchronized (self.sessions) {
        session = self.sessions[sessionId];
    }
    return session;
}

#pragma mark - GitHub Token Providers

- (void)registerTokenProvider:(CPGitHubTokenProvider)provider registrationId:(NSString *)registrationId {
    @synchronized (self.tokenProviders) {
        self.tokenProviders[registrationId] = [provider copy];
    }
}

- (void)unregisterTokenProvider:(NSString *)registrationId {
    @synchronized (self.tokenProviders) {
        [self.tokenProviders removeObjectForKey:registrationId];
    }
}

/// `gitHubToken.getToken` {registrationId, host, sessionId?, reason} ->
/// {kind: "token", accessToken, tokenType?, expiresIn} or {kind: "cancelled"}.
- (void)handleGitHubTokenRequest:(NSDictionary *)params respond:(CPRpcResponder)respond {
    NSString *registrationId = CPStringOrNil(params[@"registrationId"]);
    CPGitHubTokenProvider provider = nil;
    if (registrationId) {
        @synchronized (self.tokenProviders) {
            provider = self.tokenProviders[registrationId];
        }
    }
    if (!provider) {
        respond(nil, CPRpcError(@"GitHub token provider is unavailable"));
        return;
    }

    CPGitHubTokenProviderArgs *args = [[CPGitHubTokenProviderArgs alloc] init];
    args.host = CPStringOrNil(params[@"host"]) ?: @"";
    args.sessionId = CPStringOrNil(params[@"sessionId"]);
    args.reason = CPStringOrNil(params[@"reason"]) ?: @"initial";
    provider(args, ^(CPGitHubToken * _Nullable token) {
        if (!token) {
            respond(@{@"kind": @"cancelled"}, nil);
            return;
        }
        NSMutableDictionary *reply = [NSMutableDictionary dictionary];
        reply[@"kind"] = @"token";
        reply[@"accessToken"] = token.accessToken ?: @"";
        if (token.tokenType) reply[@"tokenType"] = token.tokenType;
        reply[@"expiresIn"] = @(token.expiresIn);
        respond(reply, nil);
    });
}

#pragma mark - Shell Authentication / Memory Store / Hooks

/// `shellAuthentication.request` -> {action: "accept", value} or {action: "cancel"}.
/// Requests without a handler (or with an unknown input kind) are cancelled. Answers are never logged.
- (void)handleShellAuthenticationRequest:(NSDictionary *)params respond:(CPRpcResponder)respond {
    NSDictionary *cancel = [[CPShellAuthenticationResponse cancel] toDictionary];
    CPShellAuthenticationRequest *request = [[CPShellAuthenticationRequest alloc] initWithDictionary:params];
    NSArray<NSString *> *inputKinds = @[CPShellAuthenticationInputKindSecret,
                                        CPShellAuthenticationInputKindText,
                                        CPShellAuthenticationInputKindConfirm];
    CPShellAuthenticationHandler handler = [self sessionWithId:request.sessionId].config.onShellAuthentication;
    if (!handler || request.requestId.length == 0 || ![inputKinds containsObject:request.inputKind]) {
        respond(cancel, nil);
        return;
    }

    NSString *key = [NSString stringWithFormat:@"%@/%@", request.sessionId, request.requestId];
    @synchronized (self.pendingShellAuthentications) {
        if (self.pendingShellAuthentications[key]) {
            respond(cancel, nil);
            return;
        }
        self.pendingShellAuthentications[key] = [respond copy];
    }

    handler(request, ^(CPShellAuthenticationResponse *response) {
        CPRpcResponder pending = nil;
        @synchronized (self.pendingShellAuthentications) {
            pending = self.pendingShellAuthentications[key];
            [self.pendingShellAuthentications removeObjectForKey:key];
        }
        // nil when shellAuthentication.cancel already answered the request
        if (pending) pending(response ? [response toDictionary] : cancel, nil);
    });
}

/// `shellAuthentication.cancel` {sessionId, requestId}: answers the pending request with "cancel".
- (void)cancelShellAuthenticationRequest:(NSDictionary *)params {
    NSString *key = [NSString stringWithFormat:@"%@/%@",
                     CPStringOrNil(params[@"sessionId"]) ?: @"", CPStringOrNil(params[@"requestId"]) ?: @""];
    CPRpcResponder pending = nil;
    @synchronized (self.pendingShellAuthentications) {
        pending = self.pendingShellAuthentications[key];
        [self.pendingShellAuthentications removeObjectForKey:key];
    }
    if (pending) pending([[CPShellAuthenticationResponse cancel] toDictionary], nil);
}

/// `memoryStore.store` {sessionId, subject, fact, citations, reason} -> null.
- (void)handleMemoryStoreRequest:(NSDictionary *)params respond:(CPRpcResponder)respond {
    CPStoredMemory *memory = [[CPStoredMemory alloc] initWithDictionary:params];
    CPMemoryStoreHandler handler = [self sessionWithId:memory.sessionId].config.onMemoryStore;
    if (!handler) {
        respond(nil, CPRpcError(@"Memory store owner is unavailable"));
        return;
    }
    handler(memory, ^(NSError * _Nullable error) {
        if (error) {
            respond(nil, CPRpcError(error.localizedDescription ?: @"Memory store failed"));
        } else {
            respond((NSDictionary *)[NSNull null], nil);
        }
    });
}

/// `hooks.invoke` {sessionId, hookType, input} -> {output?}.
- (void)handleHooksInvoke:(NSDictionary *)params respond:(CPRpcResponder)respond {
    NSString *sessionId = CPStringOrNil(params[@"sessionId"]);
    NSString *hookType = CPStringOrNil(params[@"hookType"]);
    if (!sessionId || !hookType) {
        respond(nil, CPRpcError(@"Invalid hooks invoke payload"));
        return;
    }
    CPCopilotSession *session = [self sessionWithId:sessionId];
    if (!session) {
        respond(nil, CPRpcError([NSString stringWithFormat:@"Session not found: %@", sessionId]));
        return;
    }
    CPHookHandler handler = [session.config.hooks handlerForHookType:hookType];
    if (!handler) {
        respond(@{}, nil);
        return;
    }
    id input = params[@"input"];
    handler([input isKindOfClass:[NSDictionary class]] ? input : @{}, sessionId,
            ^(NSDictionary<NSString *, id> * _Nullable output) {
        respond(output ? @{@"output": output} : @{}, nil);
    });
}

#pragma mark - Network Callbacks

/// `network.resolveProxy` {url} -> {proxy?}.
- (void)handleResolveProxy:(NSDictionary *)params respond:(CPRpcResponder)respond {
    CPNetworkResolveProxyHandler handler = self.options.network.resolveProxy;
    if (!handler) {
        respond(@{}, nil);
        return;
    }
    handler(CPStringOrNil(params[@"url"]) ?: @"", ^(NSString * _Nullable proxy) {
        respond(proxy ? @{@"proxy": proxy} : @{}, nil);
    });
}

/// `network.authenticateNegotiate` {spn} -> {token?}.
- (void)handleAuthenticateNegotiate:(NSDictionary *)params respond:(CPRpcResponder)respond {
    CPNetworkAuthenticateNegotiateHandler handler = self.options.network.authenticateNegotiate;
    if (!handler) {
        respond(@{}, nil);
        return;
    }
    handler(CPStringOrNil(params[@"spn"]) ?: @"", ^(NSString * _Nullable token) {
        respond(token ? @{@"token": token} : @{}, nil);
    });
}

/// `network.authenticateBasic` {proxy, realm?, previousRejected} -> {username?, password?}.
- (void)handleAuthenticateBasic:(NSDictionary *)params respond:(CPRpcResponder)respond {
    CPNetworkAuthenticateBasicHandler handler = self.options.network.authenticateBasic;
    if (!handler) {
        respond(@{}, nil);
        return;
    }
    CPNetworkBasicAuthRequest *request = [[CPNetworkBasicAuthRequest alloc] initWithDictionary:params];
    handler(request, ^(CPNetworkBasicCredentials * _Nullable credentials) {
        NSMutableDictionary *reply = [NSMutableDictionary dictionary];
        if (credentials.username) {
            reply[@"username"] = credentials.username;
            if (credentials.password) reply[@"password"] = credentials.password;
        }
        respond(reply, nil);
    });
}

- (void)handleToolInvocation:(NSDictionary *)params
                     respond:(void (^)(NSDictionary *, NSDictionary *))respond {
    NSString *sessionId = params[@"sessionId"] ?: @"";
    CPCopilotSession *session = nil;

    @synchronized (self.sessions) {
        session = self.sessions[sessionId];
    }

    if (!session) {
        respond(nil, @{@"code": @(-32603), @"message": @"Session not found"});
        return;
    }

    // Dispatch tool call event
    CPSessionEvent *event = [[CPSessionEvent alloc] initWithType:@"tool.executing" data:params];
    [session dispatchEvent:event];

    // Default response - tool not found
    respond(@{@"textResultForLlm": @"Tool handler not implemented",
              @"resultType": @"failure"}, nil);
}

- (void)handlePermissionRequest:(NSDictionary *)params
                        respond:(void (^)(NSDictionary *, NSDictionary *))respond {
    NSString *sessionId = params[@"sessionId"] ?: @"";
    CPCopilotSession *session = nil;

    @synchronized (self.sessions) {
        session = self.sessions[sessionId];
    }

    // Default: approve all permissions
    NSMutableDictionary *reply = [@{@"kind": @"approved"} mutableCopy];
    if (session.config.decisionContext) {
        reply[@"decisionContext"] = session.config.decisionContext;
    }
    respond(reply, nil);
}

- (void)handleExitPlanModeRequest:(NSDictionary *)params
                          respond:(void (^)(NSDictionary *, NSDictionary *))respond {
    NSString *sessionId = params[@"sessionId"] ?: @"";
    CPCopilotSession *session = nil;

    @synchronized (self.sessions) {
        session = self.sessions[sessionId];
    }

    CPExitPlanModeHandler handler = session.config.exitPlanModeHandler;
    if (handler) {
        handler(sessionId, ^(BOOL approved) {
            respond(@{@"approved": @(approved)}, nil);
        });
    } else {
        // Default: approve
        respond(@{@"approved": @YES}, nil);
    }
}

- (void)handleNotification:(NSString *)method params:(NSDictionary *)params {
    // Session events: {sessionId, event: {id, timestamp, parentId, agentId, ephemeral, type, data}}
    if ([method isEqualToString:@"session.event"]) {
        CPCopilotSession *session = [self sessionWithId:CPStringOrNil(params[@"sessionId"])];
        id envelope = params[@"event"];
        if (!session || ![envelope isKindOfClass:[NSDictionary class]]) return;
        CPSessionEvent *event = [[CPSessionEvent alloc] initWithEnvelope:envelope];
        [session dispatchEvent:event];
        // Tool calls and permission prompts arrive as broadcast events in SDK protocol 4.
        [session handleBroadcastEvent:event];
        return;
    }

    // Route session events
    if ([method hasPrefix:@"session/"]) {
        NSString *sessionId = params[@"sessionId"] ?: @"";
        CPCopilotSession *session = nil;

        @synchronized (self.sessions) {
            session = self.sessions[sessionId];
        }

        if (session) {
            NSString *eventType = [method substringFromIndex:@"session/".length];
            // Handle dot-notation events (e.g. session/assistant.message)
            if ([eventType rangeOfString:@"."].location == NSNotFound &&
                params[@"type"]) {
                eventType = params[@"type"];
            }
            CPSessionEvent *event = [[CPSessionEvent alloc] initWithType:eventType data:params];
            [session dispatchEvent:event];
        }
        return;
    }

    // Route generic events to matching sessions
    if (params[@"sessionId"]) {
        NSString *sessionId = params[@"sessionId"];
        CPCopilotSession *session = nil;

        @synchronized (self.sessions) {
            session = self.sessions[sessionId];
        }

        if (session) {
            CPSessionEvent *event = [[CPSessionEvent alloc] initWithType:method data:params];
            [session dispatchEvent:event];
        }
    }
}

#pragma mark - Session Management

- (void)createSession:(nullable CPSessionConfig *)config
           completion:(void (^)(CPCopilotSession * _Nullable, NSError * _Nullable))completion {
    // Auto-start if needed
    if (self.connectionState != CPConnectionStateConnected && self.options.autoStart) {
        [self startWithCompletion:^(NSError *error) {
            if (error) {
                completion(nil, error);
                return;
            }
            [self doCreateSession:config completion:completion];
        }];
        return;
    }

    if (self.connectionState != CPConnectionStateConnected) {
        NSError *error = [NSError errorWithDomain:kCPClientErrorDomain
                                             code:-2
                                         userInfo:@{NSLocalizedDescriptionKey: @"Client not connected"}];
        completion(nil, error);
        return;
    }

    [self doCreateSession:config completion:completion];
}

- (void)doCreateSession:(nullable CPSessionConfig *)config
             completion:(void (^)(CPCopilotSession * _Nullable, NSError * _Nullable))completion {
    [self openSessionWithId:nil config:config completion:completion];
}

/// Describes why `config` cannot be used with this client, or nil when it is valid.
- (nullable NSString *)sessionConfigProblem:(nullable CPSessionConfig *)config {
    if (config.githubToken && config.gitHubTokenProvider) {
        return @"githubToken and gitHubTokenProvider are mutually exclusive";
    }
    if ([self isEmptyMode] && !config.availableTools) {
        return @"availableTools is required for every session when the client mode is \"empty\"";
    }
    return nil;
}

/// Shared session.create / session.resume flow: builds the flat options, adds the GitHub
/// token provider registration, converts them to the grouped SDK protocol 4 payload with
/// `CPGroupSessionParams`, and registers the session before sending so server requests
/// that arrive while it is being created are routed to it.
- (void)openSessionWithId:(nullable NSString *)resumeSessionId
                   config:(nullable CPSessionConfig *)config
               completion:(void (^)(CPCopilotSession * _Nullable, NSError * _Nullable))completion {
    BOOL isResume = resumeSessionId != nil;
    NSString *problem = [self sessionConfigProblem:config];
    if (problem) {
        completion(nil, CPClientError(-8, problem));
        return;
    }

    NSMutableDictionary *flat = [NSMutableDictionary dictionaryWithDictionary:config ? [config toDictionary] : @{}];
    NSString *requestedId = resumeSessionId ?: (config.sessionId ?: [[NSUUID UUID] UUIDString]);
    flat[@"sessionId"] = requestedId;

    // Inject trace context
    if (self.options.onGetTraceContext) {
        CPTraceContext *ctx = self.options.onGetTraceContext();
        if (ctx.traceparent) flat[@"traceparent"] = ctx.traceparent;
        if (ctx.tracestate) flat[@"tracestate"] = ctx.tracestate;
    }

    NSString *registrationId = nil;
    if (config.gitHubTokenProvider) {
        registrationId = [[NSUUID UUID] UUIDString];
        [self registerTokenProvider:config.gitHubTokenProvider registrationId:registrationId];
        flat[@"gitHubTokenProviderRegistrationId"] = registrationId;
    }

    NSDictionary *params = CPGroupSessionParams(flat, isResume);
    if ([self isEmptyMode]) params = CPApplyEmptyModeDefaults(params);

    CPCopilotSession *session = [[CPCopilotSession alloc] initWithSessionId:requestedId
                                                              workspacePath:nil
                                                                  rpcClient:self.rpcClient
                                                                     config:config];
    if (registrationId) {
        __weak typeof(self) weakSelf = self;
        session.onRelease = ^{
            [weakSelf unregisterTokenProvider:registrationId];
        };
    }
    CPCopilotSession *previous = nil;
    @synchronized (self.sessions) {
        previous = self.sessions[requestedId];
        self.sessions[requestedId] = session;
    }

    NSString *method = isResume ? @"session.resume" : @"session.create";
    [self.rpcClient sendRequest:method
                         params:params
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            @synchronized (self.sessions) {
                if (self.sessions[requestedId] == session) {
                    if (previous) {
                        self.sessions[requestedId] = previous;
                    } else {
                        [self.sessions removeObjectForKey:requestedId];
                    }
                }
            }
            if (registrationId) [self unregisterTokenProvider:registrationId];
            completion(nil, CPErrorWithPolicyHelperFailure(error));
            return;
        }

        NSString *sessionId = CPStringOrNil(result[@"sessionId"]) ?: requestedId;
        if (![sessionId isEqualToString:requestedId]) {
            @synchronized (self.sessions) {
                if (self.sessions[requestedId] == session) [self.sessions removeObjectForKey:requestedId];
                self.sessions[sessionId] = session;
            }
            session.sessionId = sessionId;
        }
        session.workspacePath = CPStringOrNil(result[@"workspacePath"]);
        completion(session, nil);
    }];
}

- (void)resumeSession:(NSString *)sessionId
               config:(nullable CPSessionConfig *)config
           completion:(void (^)(CPCopilotSession * _Nullable, NSError * _Nullable))completion {
    // Auto-start if needed
    if (self.connectionState != CPConnectionStateConnected && self.options.autoStart) {
        [self startWithCompletion:^(NSError *error) {
            if (error) {
                completion(nil, error);
                return;
            }
            [self doResumeSession:sessionId config:config completion:completion];
        }];
        return;
    }

    [self doResumeSession:sessionId config:config completion:completion];
}

- (void)doResumeSession:(NSString *)sessionId
                 config:(nullable CPSessionConfig *)config
             completion:(void (^)(CPCopilotSession * _Nullable, NSError * _Nullable))completion {
    [self openSessionWithId:sessionId config:config completion:completion];
}

#pragma mark - Administrative Operations

- (void)pingWithMessage:(nullable NSString *)message
             completion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    NSMutableDictionary *params = [NSMutableDictionary dictionary];
    if (message) params[@"message"] = message;

    [self.rpcClient sendRequest:@"ping"
                         params:params
                     completion:completion];
}

- (void)getMetadataWithCompletion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"getMetadata"
                         params:nil
                     completion:completion];
}

- (void)setForeground:(NSString *)sessionId
           completion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.setForeground"
                         params:@{@"sessionId": sessionId}
                     completion:^(NSDictionary *result, NSError *error) {
        // SDK protocol 4 reports failures as JSON-RPC errors; older CLIs answer success: false.
        completion(error ?: CPLifecycleFailure(result, @"Failed to set foreground session"));
    }];
}

- (void)getForegroundSessionIdWithCompletion:(void (^)(NSString * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.getForeground"
                         params:@{}
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }
        NSString *sessionId = result[@"sessionId"];
        completion(sessionId, nil);
    }];
}

- (void)listSessionsWithCompletion:(void (^)(NSArray<CPSessionMetadata *> * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.list"
                         params:nil
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }

        NSArray *rawSessions = result[@"sessions"] ?: @[];
        NSMutableArray<CPSessionMetadata *> *sessions = [NSMutableArray array];
        for (NSDictionary *dict in rawSessions) {
            [sessions addObject:[[CPSessionMetadata alloc] initWithDictionary:dict]];
        }
        completion(sessions, nil);
    }];
}

- (void)deleteSession:(NSString *)sessionId
           completion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.delete"
                         params:@{@"sessionId": sessionId}
                     completion:^(NSDictionary *result, NSError *error) {
        NSError *failure = error ?: CPLifecycleFailure(result, @"Failed to delete session");
        if (!failure) {
            @synchronized (self.sessions) {
                [self.sessions removeObjectForKey:sessionId];
            }
        }
        completion(failure);
    }];
}

- (void)clearManagedSettingsCacheWithCompletion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"managedSettings.clearCache"
                         params:@{}
                     completion:^(NSDictionary *result, NSError *error) {
        completion(error);
    }];
}

- (void)getLastSessionIdWithCompletion:(void (^)(NSString * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.getLastId"
                         params:@{}
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }
        NSString *sessionId = result[@"sessionId"];
        completion(sessionId, nil);
    }];
}

- (void)getSessionMetadata:(NSString *)sessionId
                completion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.getMetadata"
                         params:@{@"sessionId": sessionId}
                     completion:completion];
}

- (void)listModelsWithCompletion:(void (^)(NSArray * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"models.list"
                         params:@{}
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }
        NSArray *models = result[@"models"] ?: @[];
        completion(models, nil);
    }];
}

- (void)getStatusWithCompletion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"status.get"
                         params:@{}
                     completion:completion];
}

- (void)getAuthStatusWithCompletion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"auth.getStatus"
                         params:@{}
                     completion:completion];
}

@end
