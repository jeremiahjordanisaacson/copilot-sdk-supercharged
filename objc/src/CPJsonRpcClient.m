/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPJsonRpcClient.h"

static NSString *const kCPJsonRpcVersion = @"2.0";
static NSString *const kCPJsonRpcErrorDomain = @"CPJsonRpcErrorDomain";

@interface CPJsonRpcClient ()

@property (nonatomic, assign) NSInteger nextId;
@property (nonatomic, strong) NSMutableDictionary<NSString *, void (^)(NSDictionary * _Nullable, NSError * _Nullable)> *pendingRequests;
@property (nonatomic, strong) NSMutableDictionary<NSString *, CPJsonRpcRequestHandler> *requestHandlers;
@property (nonatomic, strong, nullable) NSFileHandle *writeHandle;
@property (nonatomic, strong) dispatch_queue_t sendQueue;
@property (nonatomic, strong) dispatch_queue_t readQueue;
@property (nonatomic, assign) BOOL running;
@property (nonatomic, strong) NSMutableData *readBuffer;

@end

@implementation CPJsonRpcClient

- (instancetype)init {
    self = [super init];
    if (self) {
        _nextId = 1;
        _pendingRequests = [NSMutableDictionary dictionary];
        _requestHandlers = [NSMutableDictionary dictionary];
        _sendQueue = dispatch_queue_create("com.github.copilot.jsonrpc.send", DISPATCH_QUEUE_SERIAL);
        _readQueue = dispatch_queue_create("com.github.copilot.jsonrpc.read", DISPATCH_QUEUE_SERIAL);
        _readBuffer = [NSMutableData data];
        _running = NO;
    }
    return self;
}

- (void)registerRequestHandler:(NSString *)method handler:(CPJsonRpcRequestHandler)handler {
    @synchronized (self.requestHandlers) {
        self.requestHandlers[method] = handler;
    }
}

#pragma mark - Start / Stop

- (void)startWithReadHandle:(NSFileHandle *)readHandle writeHandle:(NSFileHandle *)writeHandle {
    self.writeHandle = writeHandle;
    self.running = YES;

    dispatch_async(self.readQueue, ^{
        [self readLoopWithHandle:readHandle];
    });
}

- (void)stop {
    self.running = NO;
    self.writeHandle = nil;

    @synchronized (self.pendingRequests) {
        NSError *cancelError = [NSError errorWithDomain:kCPJsonRpcErrorDomain
                                                   code:-1
                                               userInfo:@{NSLocalizedDescriptionKey: @"Client stopped"}];
        for (NSString *key in self.pendingRequests.allKeys) {
            void (^handler)(NSDictionary *, NSError *) = self.pendingRequests[key];
            handler(nil, cancelError);
        }
        [self.pendingRequests removeAllObjects];
    }
}

#pragma mark - Sending

- (void)sendRequest:(NSString *)method
             params:(nullable NSDictionary<NSString *, id> *)params
         completion:(void (^)(NSDictionary * _Nullable, NSError * _Nullable))completion {
    dispatch_async(self.sendQueue, ^{
        NSString *requestId = [NSString stringWithFormat:@"%ld", (long)self.nextId++];

        @synchronized (self.pendingRequests) {
            self.pendingRequests[requestId] = completion;
        }

        NSMutableDictionary *message = [NSMutableDictionary dictionary];
        message[@"jsonrpc"] = kCPJsonRpcVersion;
        message[@"id"] = requestId;
        message[@"method"] = method;
        if (params) {
            message[@"params"] = params;
        }

        [self writeMessage:message];
    });
}

- (void)sendNotification:(NSString *)method
                  params:(nullable NSDictionary<NSString *, id> *)params {
    dispatch_async(self.sendQueue, ^{
        NSMutableDictionary *message = [NSMutableDictionary dictionary];
        message[@"jsonrpc"] = kCPJsonRpcVersion;
        message[@"method"] = method;
        if (params) {
            message[@"params"] = params;
        }

        [self writeMessage:message];
    });
}

- (void)sendResponse:(NSString *)requestId
              result:(nullable NSDictionary<NSString *, id> *)result
               error:(nullable NSDictionary<NSString *, id> *)error {
    dispatch_async(self.sendQueue, ^{
        NSMutableDictionary *message = [NSMutableDictionary dictionary];
        message[@"jsonrpc"] = kCPJsonRpcVersion;
        message[@"id"] = requestId;
        if (error) {
            message[@"error"] = error;
        } else {
            message[@"result"] = result ?: [NSDictionary dictionary];
        }

        [self writeMessage:message];
    });
}

#pragma mark - Wire Format

- (void)writeMessage:(NSDictionary *)message {
    NSError *serError = nil;
    NSData *bodyData = [NSJSONSerialization dataWithJSONObject:message options:0 error:&serError];
    if (!bodyData) {
        NSLog(@"[CPJsonRpc] Failed to serialize message: %@", serError);
        return;
    }

    NSString *header = [NSString stringWithFormat:@"Content-Length: %lu\r\n\r\n",
                        (unsigned long)bodyData.length];
    NSData *headerData = [header dataUsingEncoding:NSUTF8StringEncoding];

    NSMutableData *frame = [NSMutableData dataWithData:headerData];
    [frame appendData:bodyData];

    @try {
        [self.writeHandle writeData:frame];
    } @catch (NSException *exception) {
        NSLog(@"[CPJsonRpc] Write failed: %@", exception);
    }
}

#pragma mark - Reading

- (void)readLoopWithHandle:(NSFileHandle *)readHandle {
    while (self.running) {
        @autoreleasepool {
            NSData *chunk = nil;
            @try {
                chunk = [readHandle availableData];
            } @catch (NSException *exception) {
                break;
            }

            if (chunk.length == 0) {
                // EOF
                break;
            }

            [self.readBuffer appendData:chunk];
            [self processBuffer];
        }
    }
}

- (void)processBuffer {
    while (YES) {
        // Look for Content-Length header
        NSString *bufferStr = [[NSString alloc] initWithData:self.readBuffer
                                                    encoding:NSUTF8StringEncoding];
        if (!bufferStr) break;

        NSRange headerEnd = [bufferStr rangeOfString:@"\r\n\r\n"];
        if (headerEnd.location == NSNotFound) break;

        NSString *headerPart = [bufferStr substringToIndex:headerEnd.location];
        NSInteger contentLength = -1;

        NSArray *lines = [headerPart componentsSeparatedByString:@"\r\n"];
        for (NSString *line in lines) {
            if ([line.lowercaseString hasPrefix:@"content-length:"]) {
                NSString *value = [[line substringFromIndex:@"content-length:".length]
                                   stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                contentLength = [value integerValue];
                break;
            }
        }

        if (contentLength < 0) break;

        NSUInteger bodyStart = headerEnd.location + headerEnd.length;
        NSUInteger totalNeeded = bodyStart + (NSUInteger)contentLength;

        // Convert to byte lengths for accurate comparison
        NSData *headerBytes = [bufferStr substringToIndex:bodyStart].length
            ? [[bufferStr substringToIndex:bodyStart] dataUsingEncoding:NSUTF8StringEncoding]
            : [NSData data];
        NSUInteger byteBodyStart = headerBytes.length;
        NSUInteger byteTotalNeeded = byteBodyStart + (NSUInteger)contentLength;

        if (self.readBuffer.length < byteTotalNeeded) break;

        NSData *bodyData = [self.readBuffer subdataWithRange:NSMakeRange(byteBodyStart, contentLength)];

        // Remove consumed data from buffer
        NSData *remaining = [self.readBuffer subdataWithRange:
                             NSMakeRange(byteTotalNeeded, self.readBuffer.length - byteTotalNeeded)];
        self.readBuffer = [NSMutableData dataWithData:remaining];

        // Parse JSON
        NSError *jsonError = nil;
        NSDictionary *msg = [NSJSONSerialization JSONObjectWithData:bodyData options:0 error:&jsonError];
        if (!msg) {
            NSLog(@"[CPJsonRpc] JSON parse error: %@", jsonError);
            continue;
        }

        [self handleMessage:msg];
    }
}

#pragma mark - Message Dispatch

- (void)handleMessage:(NSDictionary *)msg {
    NSString *msgId = msg[@"id"];

    // Response to our request
    if (msgId && (msg[@"result"] || msg[@"error"])) {
        NSString *idStr = [NSString stringWithFormat:@"%@", msgId];
        void (^handler)(NSDictionary *, NSError *) = nil;

        @synchronized (self.pendingRequests) {
            handler = self.pendingRequests[idStr];
            [self.pendingRequests removeObjectForKey:idStr];
        }

        if (handler) {
            if (msg[@"error"]) {
                NSDictionary *rpcErr = msg[@"error"];
                NSError *error = [NSError errorWithDomain:kCPJsonRpcErrorDomain
                                                     code:[rpcErr[@"code"] integerValue]
                                                 userInfo:@{NSLocalizedDescriptionKey: rpcErr[@"message"] ?: @"Unknown error"}];
                handler(nil, error);
            } else {
                handler(msg[@"result"], nil);
            }
        }
        return;
    }

    NSString *method = msg[@"method"];
    if (!method) return;

    NSDictionary *params = msg[@"params"] ?: @{};

    // Server request (has id and method)
    if (msgId) {
        CPJsonRpcRequestHandler reqHandler = nil;
        @synchronized (self.requestHandlers) {
            reqHandler = self.requestHandlers[method];
        }

        if (reqHandler) {
            NSString *idStr = [NSString stringWithFormat:@"%@", msgId];
            reqHandler(params, ^(NSDictionary * _Nullable result, NSDictionary * _Nullable error) {
                [self sendResponse:idStr result:result error:error];
            });
        } else {
            [self sendResponse:[NSString stringWithFormat:@"%@", msgId]
                        result:nil
                         error:@{@"code": @(-32601), @"message": @"Method not found"}];
        }
        return;
    }

    // Notification (no id)
    if (self.notificationHandler) {
        self.notificationHandler(method, params);
    }
}

@end
