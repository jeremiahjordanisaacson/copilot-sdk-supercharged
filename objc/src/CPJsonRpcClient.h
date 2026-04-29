/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/// Block called when a JSON-RPC request from the server needs handling.
typedef void (^CPJsonRpcRequestHandler)(NSDictionary<NSString *, id> *params,
                                        void (^respond)(NSDictionary<NSString *, id> * _Nullable result,
                                                        NSDictionary<NSString *, id> * _Nullable error));

/// Block called when a JSON-RPC notification arrives from the server.
typedef void (^CPJsonRpcNotificationHandler)(NSString *method,
                                             NSDictionary<NSString *, id> *params);

/// A JSON-RPC 2.0 client that communicates over stdio pipes using Content-Length header framing.
///
/// Handles:
/// - Sending requests (with ID) and waiting for responses
/// - Sending notifications (no ID, no response expected)
/// - Receiving notifications from the server
/// - Receiving requests from the server and sending responses back
@interface CPJsonRpcClient : NSObject

/// Registers a handler for incoming requests with the given method name.
- (void)registerRequestHandler:(NSString *)method handler:(CPJsonRpcRequestHandler)handler;

/// Sets the handler for all incoming notifications.
@property (nonatomic, copy, nullable) CPJsonRpcNotificationHandler notificationHandler;

/// Starts reading from the given file handle and writing to the write handle.
- (void)startWithReadHandle:(NSFileHandle *)readHandle writeHandle:(NSFileHandle *)writeHandle;

/// Stops reading and cancels all pending requests.
- (void)stop;

/// Sends a JSON-RPC request and calls the completion block with the result or error.
- (void)sendRequest:(NSString *)method
             params:(nullable NSDictionary<NSString *, id> *)params
         completion:(void (^)(NSDictionary<NSString *, id> * _Nullable result,
                              NSError * _Nullable error))completion;

/// Sends a JSON-RPC notification (no response expected).
- (void)sendNotification:(NSString *)method
                  params:(nullable NSDictionary<NSString *, id> *)params;

@end

NS_ASSUME_NONNULL_END
