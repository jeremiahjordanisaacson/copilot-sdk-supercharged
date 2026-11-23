/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPCopilotSession.h"
#import "CPJsonRpcClient.h"
#import "CPTypes.h"

typedef struct {
    NSString *eventType; // nil means all events
    CPSessionEventHandler handler;
} CPEventRegistration;

static NSString *CPStringOrNil(id value) {
    return [value isKindOfClass:[NSString class]] ? value : nil;
}

/// Lifecycle RPCs that answer `{success: false, error}` instead of a JSON-RPC error.
static NSError *CPLifecycleFailure(NSDictionary *result, NSString *what) {
    id success = result[@"success"];
    if (![success isKindOfClass:[NSNumber class]] || [success boolValue]) return nil;
    NSString *message = CPStringOrNil(result[@"error"]) ?: @"Unknown error";
    return [NSError errorWithDomain:@"CPCopilotSessionErrorDomain"
                               code:-7
                           userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"%@: %@", what, message]}];
}

/// Maps the SDK's permission decision to the protocol 4 `PermissionDecision` kind.
static NSString *CPPermissionDecisionKind(CPPermissionKind kind) {
    switch (kind) {
        case CPPermissionKindApproved: return @"approved";
        case CPPermissionKindDeniedByRules: return @"denied-by-rules";
        case CPPermissionKindDeniedNoRule: return @"denied-no-approval-rule-and-could-not-request-from-user";
        case CPPermissionKindDeniedByUser: return @"denied-interactively-by-user";
    }
    return @"denied-interactively-by-user";
}

@interface CPCopilotSession ()

@property (nonatomic, copy, readwrite) NSString *sessionId;
@property (nonatomic, copy, readwrite, nullable) NSString *workspacePath;
@property (nonatomic, strong) CPJsonRpcClient *rpcClient;
@property (nonatomic, strong) NSMutableDictionary<NSNumber *, NSDictionary *> *eventHandlers;
@property (nonatomic, assign) NSInteger nextHandlerId;
@property (nonatomic, strong) dispatch_queue_t eventQueue;
@property (nonatomic, copy, nullable) CPSessionConfig *config;
/// Runs once when the session is detached (drops the GitHub token provider registration).
@property (nonatomic, copy, nullable) void (^onRelease)(void);

- (instancetype)initWithSessionId:(NSString *)sessionId
                    workspacePath:(nullable NSString *)workspacePath
                        rpcClient:(CPJsonRpcClient *)rpcClient
                           config:(nullable CPSessionConfig *)config;

@end

@implementation CPCopilotSession

- (instancetype)initWithSessionId:(NSString *)sessionId
                    workspacePath:(nullable NSString *)workspacePath
                        rpcClient:(CPJsonRpcClient *)rpcClient
                           config:(nullable CPSessionConfig *)config {
    self = [super init];
    if (self) {
        _sessionId = [sessionId copy];
        _workspacePath = [workspacePath copy];
        _rpcClient = rpcClient;
        _config = config;
        _eventHandlers = [NSMutableDictionary dictionary];
        _nextHandlerId = 0;
        _eventQueue = dispatch_queue_create("com.github.copilot.session.events", DISPATCH_QUEUE_SERIAL);
    }
    return self;
}

#pragma mark - Event Registration

- (NSInteger)onEvent:(CPSessionEventHandler)handler {
    NSInteger handlerId;
    @synchronized (self.eventHandlers) {
        handlerId = self.nextHandlerId++;
        self.eventHandlers[@(handlerId)] = @{
            @"type": [NSNull null],
            @"handler": [handler copy],
        };
    }
    return handlerId;
}

- (NSInteger)onEventType:(NSString *)eventType handler:(CPSessionEventHandler)handler {
    NSInteger handlerId;
    @synchronized (self.eventHandlers) {
        handlerId = self.nextHandlerId++;
        self.eventHandlers[@(handlerId)] = @{
            @"type": eventType,
            @"handler": [handler copy],
        };
    }
    return handlerId;
}

- (void)removeHandler:(NSInteger)handlerId {
    @synchronized (self.eventHandlers) {
        [self.eventHandlers removeObjectForKey:@(handlerId)];
    }
}

- (void)dispatchEvent:(CPSessionEvent *)event {
    NSArray<NSDictionary *> *handlers;
    @synchronized (self.eventHandlers) {
        handlers = [self.eventHandlers.allValues copy];
    }

    dispatch_async(self.eventQueue, ^{
        for (NSDictionary *registration in handlers) {
            id typeFilter = registration[@"type"];
            CPSessionEventHandler handler = registration[@"handler"];

            if ([typeFilter isKindOfClass:[NSNull class]] ||
                [typeFilter isEqual:event.type]) {
                handler(event);
            }
        }
    });
}

#pragma mark - Send

- (void)send:(CPMessageOptions *)options
  completion:(void (^)(NSString * _Nullable, NSError * _Nullable))completion {
    NSMutableDictionary *params = [NSMutableDictionary dictionaryWithDictionary:[options toDictionary]];
    params[@"sessionId"] = self.sessionId;

    [self.rpcClient sendRequest:@"session.send"
                         params:params
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        if (error) {
            completion(nil, error);
            return;
        }
        completion(CPStringOrNil(result[@"messageId"]), nil);
    }];
}

- (void)sendAndWait:(CPMessageOptions *)options
            timeout:(NSTimeInterval)timeoutMs
         completion:(void (^)(NSString * _Nullable, NSError * _Nullable))completion {
    NSTimeInterval actualTimeout = timeoutMs > 0 ? timeoutMs : 60000;

    __block NSString *lastContent = nil;
    __block BOOL completed = NO;

    dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);

    // Register event handler to capture content and detect idle
    __block NSInteger handlerId = [self onEvent:^(CPSessionEvent *event) {
        if ([event.type isEqualToString:@"assistant.message"] && event.content) {
            lastContent = event.content;
        } else if ([event.type isEqualToString:@"assistant.message_delta"] && event.deltaContent) {
            if (!lastContent) lastContent = @"";
            lastContent = [lastContent stringByAppendingString:event.deltaContent];
        } else if ([event.type isEqualToString:@"session.idle"]) {
            if (!completed) {
                completed = YES;
                dispatch_semaphore_signal(semaphore);
            }
        } else if ([event.type isEqualToString:@"session.error"]) {
            if (!completed) {
                completed = YES;
                dispatch_semaphore_signal(semaphore);
            }
        }
    }];

    // Send the message
    [self send:options completion:^(NSString * _Nullable messageId, NSError * _Nullable error) {
        if (error) {
            completed = YES;
            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
                completion(nil, error);
            });
            dispatch_semaphore_signal(semaphore);
        }
    }];

    // Wait with timeout on a background queue
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        long result = dispatch_semaphore_wait(semaphore,
            dispatch_time(DISPATCH_TIME_NOW, (int64_t)(actualTimeout / 1000.0 * NSEC_PER_SEC)));

        [self removeHandler:handlerId];

        if (result != 0) {
            NSError *timeoutError = [NSError errorWithDomain:@"CPCopilotSessionErrorDomain"
                                                        code:-5
                                                    userInfo:@{NSLocalizedDescriptionKey: @"Timed out waiting for response"}];
            completion(nil, timeoutError);
        } else if (!completed) {
            completion(nil, nil);
        } else {
            completion(lastContent, nil);
        }
    });
}

#pragma mark - Abort

- (void)abortWithCompletion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.abort"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(error);
    }];
}

#pragma mark - Disconnect / Detach

- (void)disconnectWithCompletion:(void (^)(NSError * _Nullable))completion {
    [self detachWithCompletion:completion];
}

- (void)detachWithCompletion:(void (^)(NSError * _Nullable))completion {
    NSDictionary *params = @{@"sessionId": self.sessionId};
    [self.rpcClient sendRequest:@"session.detach"
                         params:params
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        if (CPJsonRpcIsMethodNotFoundError(error)) {
            // CLIs that predate session.detach release sessions with session.destroy.
            [self.rpcClient sendRequest:@"session.destroy"
                                 params:params
                             completion:^(NSDictionary * _Nullable destroyResult, NSError * _Nullable destroyError) {
                [self finishDetachWithResult:destroyResult error:destroyError completion:completion];
            }];
            return;
        }
        [self finishDetachWithResult:result error:error completion:completion];
    }];
}

- (void)finishDetachWithResult:(nullable NSDictionary *)result
                         error:(nullable NSError *)error
                    completion:(void (^)(NSError * _Nullable))completion {
    NSError *failure = error ?: CPLifecycleFailure(result, @"Failed to detach session");
    if (!failure) {
        void (^release)(void) = nil;
        @synchronized (self) {
            release = self.onRelease;
            self.onRelease = nil;
        }
        if (release) release();
    }
    completion(failure);
}

#pragma mark - Metadata

- (void)getMetadataWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable,
                                            NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.getMetadata"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(result, error);
    }];
}

#pragma mark - Model, Permissions, Managed Settings

- (void)setAutoTier:(nullable NSString *)autoTier
         completion:(void (^)(NSError * _Nullable))completion {
    NSDictionary *params = @{
        @"sessionId": self.sessionId,
        @"autoTier": autoTier ?: (id)[NSNull null],
    };
    [self.rpcClient sendRequest:@"session.model.switchAutoTier"
                         params:params
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(error);
    }];
}

- (void)setPermissionMode:(NSString *)mode
               completion:(void (^)(CPPermissionModeResult * _Nullable, NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.permissions.setMode"
                         params:@{@"sessionId": self.sessionId, @"mode": mode}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        if (error) {
            completion(nil, error);
            return;
        }
        completion([[CPPermissionModeResult alloc] initWithDictionary:result ?: @{}], nil);
    }];
}

- (void)getManagedSettingsWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable,
                                                   NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.managedSettings.get"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(result, error);
    }];
}

- (void)recheckPolicyHelperWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable,
                                                    NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session.managedSettings.recheckPolicyHelper"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(result, error);
    }];
}

#pragma mark - Broadcast Requests (tools, permissions)

/// Answers requests the runtime broadcasts as session events, off the transport's read queue.
/// Called by the client.
- (void)handleBroadcastEvent:(CPSessionEvent *)event {
    NSDictionary *data = event.rawData;
    BOOL isToolRequest = [event.type isEqualToString:@"external_tool.requested"];
    BOOL isPermissionRequest = [event.type isEqualToString:@"permission.requested"];
    if (!data || (!isToolRequest && !isPermissionRequest)) return;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        if (isToolRequest) {
            [self handleExternalToolRequest:data];
        } else {
            [self handlePermissionRequested:data];
        }
    });
}

/// Handler of a tool defined with `CPDefineTool` in `CPSessionConfig.tools`, if any.
- (nullable CPToolHandler)toolHandlerNamed:(NSString *)name {
    for (id tool in self.config.tools) {
        if (![tool isKindOfClass:[NSDictionary class]]) continue;
        NSDictionary *definition = tool;
        if (![definition[@"name"] isEqual:name]) continue;
        id handler = definition[@"_handler"];
        return handler;
    }
    return nil;
}

/// Runs the tool handler for `external_tool.requested` and answers with
/// `session.tools.handlePendingToolCall`. Tools without a handler here are left to other clients.
- (void)handleExternalToolRequest:(NSDictionary *)data {
    if ([data[@"hasHandler"] isEqual:@NO]) return;
    NSString *requestId = CPStringOrNil(data[@"requestId"]);
    NSString *toolName = CPStringOrNil(data[@"toolName"]);
    if (!requestId || !toolName) return;
    CPToolHandler handler = [self toolHandlerNamed:toolName];
    if (!handler) return;

    id arguments = data[@"arguments"];
    CPToolInvocation *invocation =
        [[CPToolInvocation alloc] initWithSessionId:self.sessionId
                                         toolCallId:CPStringOrNil(data[@"toolCallId"]) ?: @""
                                           toolName:toolName
                                          arguments:[arguments isKindOfClass:[NSDictionary class]] ? arguments : nil];
    NSString *sessionId = self.sessionId;
    CPJsonRpcClient *rpc = self.rpcClient;
    handler(invocation, ^(CPToolResult *result) {
        NSMutableDictionary *params = [NSMutableDictionary dictionary];
        params[@"sessionId"] = sessionId;
        params[@"requestId"] = requestId;
        if (result) {
            params[@"result"] = [result toDictionary];
        } else {
            params[@"error"] = @"Tool handler returned no result";
        }
        [rpc sendRequest:@"session.tools.handlePendingToolCall"
                  params:params
              completion:^(NSDictionary * _Nullable response, NSError * _Nullable error) {}];
    });
}

/// Runs `CPSessionConfig.permissionHandler` for `permission.requested` and answers with
/// `session.permissions.handlePendingPermissionRequest`.
- (void)handlePermissionRequested:(NSDictionary *)data {
    CPPermissionHandler handler = self.config.permissionHandler;
    NSString *requestId = CPStringOrNil(data[@"requestId"]);
    if (!handler || !requestId || [data[@"resolvedByHook"] isEqual:@YES]) return;

    id request = data[@"permissionRequest"];
    NSDictionary *requestDict = [request isKindOfClass:[NSDictionary class]] ? request : @{};
    NSString *sessionId = self.sessionId;
    NSDictionary *decisionContext = self.config.decisionContext;
    CPJsonRpcClient *rpc = self.rpcClient;
    handler(requestDict, sessionId, ^(CPPermissionKind kind) {
        NSMutableDictionary *params = [NSMutableDictionary dictionary];
        params[@"sessionId"] = sessionId;
        params[@"requestId"] = requestId;
        params[@"result"] = @{@"kind": CPPermissionDecisionKind(kind)};
        if (decisionContext) params[@"decisionContext"] = decisionContext;
        [rpc sendRequest:@"session.permissions.handlePendingPermissionRequest"
                  params:params
              completion:^(NSDictionary * _Nullable response, NSError * _Nullable error) {}];
    });
}

@end
