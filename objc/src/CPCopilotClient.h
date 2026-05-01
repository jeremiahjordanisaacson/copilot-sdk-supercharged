/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>
#import "CPTypes.h"
#import "CPCopilotSession.h"

NS_ASSUME_NONNULL_BEGIN

/// The main entry point for the Copilot Objective-C SDK.
///
/// `CPCopilotClient` manages the lifecycle of the Copilot CLI server process
/// and provides methods for creating sessions, pinging the server, listing sessions,
/// and other administrative operations.
///
/// Usage:
/// ```objc
/// CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
/// CPCopilotClient *client = [[CPCopilotClient alloc] initWithOptions:opts];
///
/// [client startWithCompletion:^(NSError *error) {
///     if (error) { NSLog(@"Start failed: %@", error); return; }
///
///     CPSessionConfig *config = [[CPSessionConfig alloc] init];
///     [client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
///         // Use session...
///     }];
/// }];
/// ```
@interface CPCopilotClient : NSObject

/// The current connection state.
@property (nonatomic, assign, readonly) CPConnectionState connectionState;

/// Creates a new client with the given options.
- (instancetype)initWithOptions:(nullable CPCopilotClientOptions *)options;

/// Starts the CLI server process and establishes a connection.
/// Spawns the CLI process and verifies protocol compatibility via ping.
- (void)startWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Stops the client, terminates all sessions, and kills the CLI process.
- (void)stopWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Creates a new conversation session.
/// If the client is not started and `autoStart` is YES, starts the client first.
- (void)createSession:(nullable CPSessionConfig *)config
           completion:(void (^)(CPCopilotSession * _Nullable session,
                                NSError * _Nullable error))completion;

/// Resumes an existing session by its ID.
- (void)resumeSession:(NSString *)sessionId
               config:(nullable CPSessionConfig *)config
           completion:(void (^)(CPCopilotSession * _Nullable session,
                                NSError * _Nullable error))completion;

/// Pings the server to verify connectivity.
- (void)pingWithMessage:(nullable NSString *)message
             completion:(void (^)(NSDictionary<NSString *, id> * _Nullable response,
                                  NSError * _Nullable error))completion;

/// Retrieves server metadata.
- (void)getMetadataWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable metadata,
                                            NSError * _Nullable error))completion;

/// Sets the foreground session hint.
- (void)setForeground:(NSString *)sessionId
           completion:(void (^)(NSError * _Nullable error))completion;

/// Gets the foreground session ID.
- (void)getForegroundSessionIdWithCompletion:(void (^)(NSString * _Nullable sessionId,
                                                       NSError * _Nullable error))completion;

/// Lists all sessions known to the server.
- (void)listSessionsWithCompletion:(void (^)(NSArray<CPSessionMetadata *> * _Nullable sessions,
                                             NSError * _Nullable error))completion;

/// Returns the last-used session ID, or nil if none exists.
- (void)getLastSessionIdWithCompletion:(void (^)(NSString * _Nullable sessionId,
                                                  NSError * _Nullable error))completion;

/// Retrieves metadata for a specific session.
- (void)getSessionMetadata:(NSString *)sessionId
                completion:(void (^)(NSDictionary<NSString *, id> * _Nullable metadata,
                                     NSError * _Nullable error))completion;

/// Lists available models.
- (void)listModelsWithCompletion:(void (^)(NSArray<NSDictionary<NSString *, id> *> * _Nullable models,
                                           NSError * _Nullable error))completion;

/// Returns the CLI server status.
- (void)getStatusWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable status,
                                          NSError * _Nullable error))completion;

/// Returns the current authentication status.
- (void)getAuthStatusWithCompletion:(void (^)(NSDictionary<NSString *, id> * _Nullable authStatus,
                                              NSError * _Nullable error))completion;

/// Deletes a session from the server.
- (void)deleteSession:(NSString *)sessionId
           completion:(void (^)(NSError * _Nullable error))completion;

@end

NS_ASSUME_NONNULL_END
