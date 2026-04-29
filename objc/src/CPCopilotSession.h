/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>
#import "CPTypes.h"

NS_ASSUME_NONNULL_BEGIN

/// Block called for each session event.
typedef void (^CPSessionEventHandler)(CPSessionEvent *event);

/// A conversation session with the Copilot CLI.
///
/// Created via `[CPCopilotClient createSession:completion:]` or
/// `[CPCopilotClient resumeSession:config:completion:]`.
///
/// Use `-send:completion:` to send messages asynchronously (events arrive via handlers),
/// or `-sendAndWait:timeout:completion:` to block until the assistant responds.
@interface CPCopilotSession : NSObject

/// The unique session identifier assigned by the server.
@property (nonatomic, copy, readonly) NSString *sessionId;

/// The workspace path for infinite sessions, or nil.
@property (nonatomic, copy, readonly, nullable) NSString *workspacePath;

/// Registers a block to receive all session events.
/// Returns a handler ID that can be passed to `-removeHandler:`.
- (NSInteger)onEvent:(CPSessionEventHandler)handler;

/// Registers a block to receive only events of the given type.
/// Returns a handler ID that can be passed to `-removeHandler:`.
- (NSInteger)onEventType:(NSString *)eventType handler:(CPSessionEventHandler)handler;

/// Removes a previously registered event handler.
- (void)removeHandler:(NSInteger)handlerId;

/// Sends a message to the session. The completion block receives a message ID or error.
/// Responses arrive asynchronously via event handlers.
- (void)send:(CPMessageOptions *)options
  completion:(void (^)(NSString * _Nullable messageId, NSError * _Nullable error))completion;

/// Sends a message and waits (up to `timeoutMs` milliseconds) until the session becomes idle.
/// If `timeoutMs` is 0, defaults to 60000ms.
/// The completion block receives the final assistant message content or an error.
- (void)sendAndWait:(CPMessageOptions *)options
            timeout:(NSTimeInterval)timeoutMs
         completion:(void (^)(NSString * _Nullable content, NSError * _Nullable error))completion;

/// Aborts the currently processing message in this session.
- (void)abortWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Disconnects this session from the server.
- (void)disconnectWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Retrieves metadata for this session.
- (void)getMetadataWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable metadata,
                                            NSError * _Nullable error))completion;

@end

NS_ASSUME_NONNULL_END
