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

@interface CPCopilotSession ()

@property (nonatomic, copy, readwrite) NSString *sessionId;
@property (nonatomic, copy, readwrite, nullable) NSString *workspacePath;
@property (nonatomic, strong) CPJsonRpcClient *rpcClient;
@property (nonatomic, strong) NSMutableDictionary<NSNumber *, NSDictionary *> *eventHandlers;
@property (nonatomic, assign) NSInteger nextHandlerId;
@property (nonatomic, strong) dispatch_queue_t eventQueue;
@property (nonatomic, copy, nullable) CPSessionConfig *config;

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
    NSDictionary *params = @{
        @"sessionId": self.sessionId,
        @"message": [options toDictionary],
    };

    [self.rpcClient sendRequest:@"session/send"
                         params:params
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        if (error) {
            completion(nil, error);
            return;
        }
        completion(result[@"messageId"], nil);
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
    [self.rpcClient sendRequest:@"session/abort"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(error);
    }];
}

#pragma mark - Disconnect

- (void)disconnectWithCompletion:(void (^)(NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session/disconnect"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(error);
    }];
}

#pragma mark - Metadata

- (void)getMetadataWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable,
                                            NSError * _Nullable))completion {
    [self.rpcClient sendRequest:@"session/getMetadata"
                         params:@{@"sessionId": self.sessionId}
                     completion:^(NSDictionary * _Nullable result, NSError * _Nullable error) {
        completion(result, error);
    }];
}

@end
