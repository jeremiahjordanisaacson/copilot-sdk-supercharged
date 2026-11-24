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
/// Spawns the CLI process, sends the `connect` handshake when `clientInfo` is set, verifies
/// SDK protocol 4 (Copilot CLI 1.0.113 or newer) via ping, and registers network callbacks.
- (void)startWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Stops the client, terminates all sessions, and kills the CLI process.
- (void)stopWithCompletion:(void (^)(NSError * _Nullable error))completion;

/// Creates a new conversation session.
/// If the client is not started and `autoStart` is YES, starts the client first.
/// When session.create fails because of managed policy, the error's userInfo carries a
/// `CPPolicyHelperFailure` under `CPPolicyHelperFailureErrorKey`.
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

/// Clears the runtime's cached managed settings (`managedSettings.clearCache`) so the next
/// session fetches them again.
- (void)clearManagedSettingsCacheWithCompletion:(void (^)(NSError * _Nullable error))completion;

#pragma mark - MCP Secrets / Plugin Extension Approvals (Copilot CLI 1.0.116 or newer)

/// Saves a legacy MCP configuration credential in the runtime-owned secret store
/// (`mcp.secrets.save`). `value` is a credential: it is sent to the runtime and never logged.
/// `configDir` selects the configuration directory (the runtime's default when nil). The result's
/// `usedKeychain` is YES when the OS keychain stored the credential, NO for the file backend.
/// Requires Copilot CLI 1.0.116 or newer; older runtimes fail with a method-not-found error.
- (void)mcpSecretsSave:(NSString *)secretId
                 value:(NSString *)value
             configDir:(nullable NSString *)configDir
            completion:(void (^)(CPMcpSecretsSaveResult * _Nullable result,
                                 NSError * _Nullable error))completion;

/// Deletes a legacy MCP configuration credential from the runtime-owned secret store
/// (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
- (void)mcpSecretsDelete:(NSString *)secretId
               configDir:(nullable NSString *)configDir
              completion:(void (^)(NSError * _Nullable error))completion;

/// Deletes every legacy credential indexed for an MCP server (`mcp.secrets.deleteServer`).
/// Requires Copilot CLI 1.0.116 or newer.
- (void)mcpSecretsDeleteServer:(NSString *)serverName
                     configDir:(nullable NSString *)configDir
                    completion:(void (^)(NSError * _Nullable error))completion;

/// Persists a pending plugin extension permission request as a user-scoped approval, before
/// the host answers that request normally (`permissions.extensionApprovals.grant`).
/// Requires Copilot CLI 1.0.116 or newer.
- (void)extensionApprovalsGrantForSession:(NSString *)sessionId
                                requestId:(NSString *)requestId
                               completion:(void (^)(CPPermissionsExtensionApprovalsGrantResult * _Nullable result,
                                                    NSError * _Nullable error))completion;

/// Lists the user-scoped plugin extension approvals (`permissions.extensionApprovals.list`).
/// `sessionId` selects an attached session's configuration profile (the server's when nil).
/// Requires Copilot CLI 1.0.116 or newer.
- (void)extensionApprovalsListForSession:(nullable NSString *)sessionId
                              completion:(void (^)(CPPermissionsExtensionApprovalsListResult * _Nullable result,
                                                   NSError * _Nullable error))completion;

/// Revokes one user-scoped plugin extension approval (`permissions.extensionApprovals.revoke`).
/// Pass the approval's `extensionName` and `installationKey` unchanged; `sessionId` selects an
/// attached session's configuration profile. Requires Copilot CLI 1.0.116 or newer.
- (void)extensionApprovalsRevoke:(NSString *)extensionName
                 installationKey:(NSString *)installationKey
                       sessionId:(nullable NSString *)sessionId
                      completion:(void (^)(CPPermissionsExtensionApprovalsRevokeResult * _Nullable result,
                                           NSError * _Nullable error))completion;

@end

NS_ASSUME_NONNULL_END
