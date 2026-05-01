/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPCopilotClient.h"
#import "CPJsonRpcClient.h"
#import "CPSdkProtocolVersion.h"
#import "CPTypes.h"

static NSString *const kCPClientErrorDomain = @"CPCopilotClientErrorDomain";

@interface CPCopilotSession ()

- (instancetype)initWithSessionId:(NSString *)sessionId
                    workspacePath:(nullable NSString *)workspacePath
                        rpcClient:(CPJsonRpcClient *)rpcClient
                           config:(nullable CPSessionConfig *)config;

- (void)dispatchEvent:(CPSessionEvent *)event;

@end

@interface CPCopilotClient ()

@property (nonatomic, strong) CPCopilotClientOptions *options;
@property (nonatomic, strong, nullable) NSTask *cliProcess;
@property (nonatomic, strong, nullable) CPJsonRpcClient *rpcClient;
@property (nonatomic, strong) NSMutableDictionary<NSString *, CPCopilotSession *> *sessions;
@property (nonatomic, assign, readwrite) CPConnectionState connectionState;
@property (nonatomic, strong) dispatch_queue_t clientQueue;

@end

@implementation CPCopilotClient

- (instancetype)initWithOptions:(nullable CPCopilotClientOptions *)options {
    self = [super init];
    if (self) {
        _options = options ?: [CPCopilotClientOptions defaultOptions];
        _sessions = [NSMutableDictionary dictionary];
        _connectionState = CPConnectionStateDisconnected;
        _clientQueue = dispatch_queue_create("com.github.copilot.client", DISPATCH_QUEUE_SERIAL);
    }
    return self;
}

#pragma mark - Start / Stop

- (void)startWithCompletion:(void (^)(NSError * _Nullable))completion {
    dispatch_async(self.clientQueue, ^{
        if (self.connectionState == CPConnectionStateConnected) {
            completion(nil);
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

        // Verify protocol version via ping
        [self pingWithMessage:@"init" completion:^(NSDictionary *response, NSError *error) {
            if (error) {
                self.connectionState = CPConnectionStateError;
                completion(error);
                return;
            }

            NSInteger serverVersion = [response[@"protocolVersion"] integerValue];
            if (serverVersion != 0 && serverVersion != CPSdkProtocolVersion) {
                self.connectionState = CPConnectionStateError;
                NSError *versionError = [NSError errorWithDomain:kCPClientErrorDomain
                    code:-4
                    userInfo:@{NSLocalizedDescriptionKey:
                        [NSString stringWithFormat:@"Protocol version mismatch: expected %ld, got %ld",
                         (long)CPSdkProtocolVersion, (long)serverVersion]}];
                completion(versionError);
                return;
            }

            self.connectionState = CPConnectionStateConnected;
            completion(nil);
        }];
    });
}

- (BOOL)spawnCLIProcess:(NSError **)error {
    NSString *cliPath = self.options.cliPath ?: @"copilot";

    NSMutableArray<NSString *> *args = [NSMutableArray arrayWithObjects:@"--server-mode", nil];

    if (self.options.logLevel) {
        [args addObject:@"--log-level"];
        [args addObject:self.options.logLevel];
    }

    if (self.options.extraArgs) {
        [args addObjectsFromArray:self.options.extraArgs];
    }

    NSTask *task = [[NSTask alloc] init];
    task.executableURL = [NSURL fileURLWithPath:cliPath];
    task.arguments = args;

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
    [[stderrPipe fileHandleForReading] readabilityHandler = ^(NSFileHandle *handle) {
        NSData *data = [handle availableData];
        if (data.length > 0) {
            NSString *str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            if (str) NSLog(@"[CopilotCLI] %@", str);
        }
    }];

    NSError *launchError = nil;
    [task launchAndReturnError:&launchError];
    if (launchError) {
        if (error) *error = launchError;
        return NO;
    }

    self.cliProcess = task;

    // Handle termination
    task.terminationHandler = ^(NSTask *t) {
        dispatch_async(self.clientQueue, ^{
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

        if (self.cliProcess && self.cliProcess.isRunning) {
            [self.cliProcess terminate];
        }

        self.cliProcess = nil;
        self.connectionState = CPConnectionStateDisconnected;

        @synchronized (self.sessions) {
            [self.sessions removeAllObjects];
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
    respond(@{@"kind": @"approved"}, nil);
}

- (void)handleNotification:(NSString *)method params:(NSDictionary *)params {
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
    NSDictionary *params = config ? [config toDictionary] : @{};

    [self.rpcClient sendRequest:@"session/create"
                         params:params
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }

        NSString *sessionId = result[@"sessionId"];
        if (!sessionId) {
            NSError *parseError = [NSError errorWithDomain:kCPClientErrorDomain
                                                      code:-6
                                                  userInfo:@{NSLocalizedDescriptionKey: @"No sessionId in response"}];
            completion(nil, parseError);
            return;
        }

        NSString *workspacePath = result[@"workspacePath"];
        CPCopilotSession *session = [[CPCopilotSession alloc] initWithSessionId:sessionId
                                                                  workspacePath:workspacePath
                                                                      rpcClient:self.rpcClient
                                                                         config:config];

        @synchronized (self.sessions) {
            self.sessions[sessionId] = session;
        }

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
    NSMutableDictionary *params = [NSMutableDictionary dictionary];
    params[@"sessionId"] = sessionId;
    if (config) {
        [params addEntriesFromDictionary:[config toDictionary]];
    }

    [self.rpcClient sendRequest:@"session/resume"
                         params:params
                     completion:^(NSDictionary *result, NSError *error) {
        if (error) {
            completion(nil, error);
            return;
        }

        NSString *sid = result[@"sessionId"] ?: sessionId;
        NSString *workspacePath = result[@"workspacePath"];
        CPCopilotSession *session = [[CPCopilotSession alloc] initWithSessionId:sid
                                                                  workspacePath:workspacePath
                                                                      rpcClient:self.rpcClient
                                                                         config:config];

        @synchronized (self.sessions) {
            self.sessions[sid] = session;
        }

        completion(session, nil);
    }];
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
        if (error) {
            completion(error);
            return;
        }
        NSNumber *success = result[@"success"];
        if (![success boolValue]) {
            NSString *errMsg = result[@"error"] ?: @"Unknown error";
            NSError *failError = [NSError errorWithDomain:kCPClientErrorDomain
                                                     code:-7
                                                 userInfo:@{NSLocalizedDescriptionKey:
                                                     [NSString stringWithFormat:@"Failed to set foreground session: %@", errMsg]}];
            completion(failError);
            return;
        }
        completion(nil);
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
    [self.rpcClient sendRequest:@"session/list"
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
    [self.rpcClient sendRequest:@"session/delete"
                         params:@{@"sessionId": sessionId}
                     completion:^(NSDictionary *result, NSError *error) {
        if (!error) {
            @synchronized (self.sessions) {
                [self.sessions removeObjectForKey:sessionId];
            }
        }
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
