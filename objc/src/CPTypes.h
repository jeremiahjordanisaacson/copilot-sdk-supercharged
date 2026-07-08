/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Connection State

/// Represents the client connection state.
typedef NS_ENUM(NSInteger, CPConnectionState) {
    CPConnectionStateDisconnected = 0,
    CPConnectionStateConnecting,
    CPConnectionStateConnected,
    CPConnectionStateError,
};

#pragma mark - Tool Result Types

/// The outcome type of a tool invocation.
typedef NS_ENUM(NSInteger, CPToolResultType) {
    CPToolResultTypeSuccess = 0,
    CPToolResultTypeFailure,
    CPToolResultTypeRejected,
    CPToolResultTypeDenied,
};

#pragma mark - Permission Types

/// Permission decision kind.
typedef NS_ENUM(NSInteger, CPPermissionKind) {
    CPPermissionKindApproved = 0,
    CPPermissionKindDeniedByRules,
    CPPermissionKindDeniedNoRule,
    CPPermissionKindDeniedByUser,
};

#pragma mark - Tool Invocation

/// Context passed to tool handlers when a tool is invoked.
@interface CPToolInvocation : NSObject

@property (nonatomic, copy, readonly) NSString *sessionId;
@property (nonatomic, copy, readonly) NSString *toolCallId;
@property (nonatomic, copy, readonly) NSString *toolName;
@property (nonatomic, copy, readonly, nullable) NSDictionary<NSString *, id> *arguments;

- (instancetype)initWithSessionId:(NSString *)sessionId
                       toolCallId:(NSString *)toolCallId
                         toolName:(NSString *)toolName
                        arguments:(nullable NSDictionary<NSString *, id> *)arguments;

@end

#pragma mark - Tool Result

/// Structured result from a tool invocation.
@interface CPToolResult : NSObject

@property (nonatomic, copy) NSString *textResultForLlm;
@property (nonatomic, assign) CPToolResultType resultType;
@property (nonatomic, copy, nullable) NSString *error;
@property (nonatomic, copy, nullable) NSString *sessionLog;

+ (instancetype)successWithText:(NSString *)text;
+ (instancetype)failureWithError:(NSString *)error;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Tool Definition Blocks

/// Block type for tool handlers.
typedef void (^CPToolHandler)(CPToolInvocation *invocation,
                              void (^completion)(CPToolResult *result));

#pragma mark - Permission Handler

/// Block type for permission request handlers.
typedef void (^CPPermissionHandler)(NSDictionary<NSString *, id> *request,
                                    NSString *sessionId,
                                    void (^completion)(CPPermissionKind kind));

#pragma mark - Elicitation Handler

/// Block type for elicitation request handlers.
typedef void (^CPElicitationHandler)(NSDictionary<NSString *, id> *request,
                                     NSString *sessionId,
                                     void (^completion)(NSDictionary<NSString *, id> *result));

#pragma mark - Provider Token / Request Handlers

/// Provider bearer-token request arguments (bring-your-own-key).
@interface CPProviderTokenArgs : NSObject
@property (nonatomic, copy) NSString *sessionId;
@end

/// Block that supplies a bearer token for outbound model requests (BYOK).
typedef void (^CPBearerTokenProvider)(CPProviderTokenArgs *args,
                                      void (^completion)(NSString * _Nullable token));

/// Block that handles an MCP OAuth authorization request, returning an access token.
typedef void (^CPMcpAuthHandler)(CPProviderTokenArgs *args,
                                 void (^completion)(NSString * _Nullable token));

/// Block providing a custom transport for outbound Copilot HTTP requests.
typedef void (^CPCopilotRequestHandler)(NSDictionary<NSString *, id> *request,
                                        void (^completion)(NSDictionary<NSString *, id> * _Nullable response));

#pragma mark - Session Limits / Memory

/// Per-session resource limits.
@interface CPSessionLimitsConfig : NSObject
@property (nonatomic, copy, nullable) NSNumber *maxAiCredits;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Long-term memory configuration for a session.
@interface CPMemoryConfiguration : NSObject
@property (nonatomic, assign) BOOL enabled;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

#pragma mark - Constant Identifiers

/// Tool deferral policies for a tool's "defer" field.
FOUNDATION_EXPORT NSString * const CPToolDeferAuto;
FOUNDATION_EXPORT NSString * const CPToolDeferNever;

/// Known system message section identifiers for section overrides.
FOUNDATION_EXPORT NSString * const CPSystemMessageSectionPreamble;
FOUNDATION_EXPORT NSString * const CPSystemMessageSectionIdentity;
FOUNDATION_EXPORT NSString * const CPSystemMessageSectionToolInstructions;
FOUNDATION_EXPORT NSString * const CPSystemMessageSectionPreserve;

/// GitHub attachment type constants.
FOUNDATION_EXPORT NSString * const CPGitHubAttachmentCommit;
FOUNDATION_EXPORT NSString * const CPGitHubAttachmentRepository;
FOUNDATION_EXPORT NSString * const CPGitHubAttachmentPullRequest;
FOUNDATION_EXPORT NSString * const CPGitHubAttachmentIssue;

#pragma mark - Session Hooks

/// Block type for a session hook handler.
typedef void (^CPHookHandler)(NSDictionary<NSString *, id> *input,
                              NSString *sessionId,
                              void (^completion)(NSDictionary<NSString *, id> * _Nullable output));

/// Registers hook handlers for a session's lifecycle and tool events.
@interface CPSessionHooks : NSObject
@property (nonatomic, copy, nullable) CPHookHandler onPreToolUse;
@property (nonatomic, copy, nullable) CPHookHandler onPostToolUse;
@property (nonatomic, copy, nullable) CPHookHandler onPreMcpToolCall;
@property (nonatomic, copy, nullable) CPHookHandler onUserPromptSubmitted;
@property (nonatomic, copy, nullable) CPHookHandler onSessionStart;
@property (nonatomic, copy, nullable) CPHookHandler onSessionEnd;
@property (nonatomic, copy, nullable) CPHookHandler onErrorOccurred;

/// Returns YES if at least one hook handler is registered.
- (BOOL)hasAnyHandler;
/// Returns the wire identifiers of the registered hook types.
- (NSArray<NSString *> *)hookTypes;
@end

#pragma mark - Session Event

/// An event received from the session.
@interface CPSessionEvent : NSObject

@property (nonatomic, copy, readonly) NSString *type;
@property (nonatomic, copy, readonly, nullable) NSString *eventId;
@property (nonatomic, copy, readonly, nullable) NSString *timestamp;
@property (nonatomic, copy, readonly, nullable) NSString *parentId;
@property (nonatomic, copy, readonly, nullable) NSString *agentId;
@property (nonatomic, readonly) BOOL ephemeral;
@property (nonatomic, copy, readonly, nullable) NSDictionary<NSString *, id> *rawData;
@property (nonatomic, copy, readonly, nullable) NSString *content;
@property (nonatomic, copy, readonly, nullable) NSString *deltaContent;
@property (nonatomic, copy, readonly, nullable) NSString *message;
@property (nonatomic, copy, readonly, nullable) NSString *toolName;
@property (nonatomic, copy, readonly, nullable) NSString *toolCallId;

- (instancetype)initWithType:(NSString *)type data:(nullable NSDictionary<NSString *, id> *)data;

@end

#pragma mark - Session Filesystem Config

/// Configuration for session filesystem paths.
@interface CPSessionFsConfig : NSObject

@property (nonatomic, copy, nullable) NSString *initialCwd;
@property (nonatomic, copy, nullable) NSString *sessionStatePath;
@property (nonatomic, copy, nullable) NSString *conventions;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Session Metadata

/// Metadata about a session.
@interface CPSessionMetadata : NSObject

@property (nonatomic, copy) NSString *sessionId;
@property (nonatomic, copy, nullable) NSString *startTime;
@property (nonatomic, copy, nullable) NSString *modifiedTime;
@property (nonatomic, copy, nullable) NSString *summary;
@property (nonatomic, assign) BOOL isRemote;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

#pragma mark - Attachment

/// Type of file attachment.
typedef NS_ENUM(NSInteger, CPAttachmentType) {
    CPAttachmentTypeFile = 0,
    CPAttachmentTypeDirectory,
    CPAttachmentTypeSelection,
};

/// A file attachment for a message.
@interface CPAttachment : NSObject

@property (nonatomic, assign) CPAttachmentType type;
@property (nonatomic, copy) NSString *path;
@property (nonatomic, copy, nullable) NSString *displayName;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Message Options

/// Options for sending a message to a session.
@interface CPMessageOptions : NSObject

@property (nonatomic, copy) NSString *prompt;
@property (nonatomic, copy, nullable) NSArray<CPAttachment *> *attachments;
@property (nonatomic, copy, nullable) NSString *mode;
/// Custom HTTP headers to include in outbound model requests for this turn.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *requestHeaders;
/// Agent execution mode for this turn (e.g. "agent", "chat").
@property (nonatomic, copy, nullable) NSString *agentMode;
/// Prompt text to display in the UI in place of the actual prompt.
@property (nonatomic, copy, nullable) NSString *displayPrompt;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Session Config

/// Configuration for creating or resuming a session.
@interface CPSessionConfig : NSObject

@property (nonatomic, copy, nullable) NSString *sessionId;
@property (nonatomic, copy, nullable) NSString *model;
@property (nonatomic, copy, nullable) NSString *reasoningEffort;
@property (nonatomic, copy, nullable) NSString *workingDirectory;
@property (nonatomic, assign) BOOL streaming;
@property (nonatomic, copy, nullable) NSArray<NSDictionary<NSString *, id> *> *tools;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *systemMessage;
@property (nonatomic, copy, nullable) NSString *githubToken;
@property (nonatomic, copy, nullable) CPPermissionHandler permissionHandler;
@property (nonatomic, copy, nullable) NSArray<NSString *> *excludedTools;
@property (nonatomic, copy, nullable) NSArray<NSString *> *skillDirectories;
@property (nonatomic, copy, nullable) NSArray<NSString *> *disabledSkills;
@property (nonatomic, assign) BOOL includeSubAgentStreamingEvents;
@property (nonatomic, assign) BOOL enableConfigDiscovery;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *modelCapabilities;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *requestHeaders;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *mcpServers;
@property (nonatomic, copy, nullable) NSArray<NSDictionary<NSString *, id> *> *commands;
@property (nonatomic, copy, nullable) NSString *responseFormat;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *imageOptions;
@property (nonatomic, copy, nullable) CPElicitationHandler elicitationHandler;
@property (nonatomic, copy, nullable) NSString *authToken;
@property (nonatomic, copy, nullable) NSArray<NSString *> *instructionDirectories;

/// Hook handlers for this session's lifecycle and tool events.
@property (nonatomic, strong, nullable) CPSessionHooks *hooks;
/// Enable inline source citations in assistant responses.
@property (nonatomic, copy, nullable) NSNumber *enableCitations;
/// Names of built-in agents to exclude from this session.
@property (nonatomic, copy, nullable) NSArray<NSString *> *excludedBuiltinAgents;
/// Per-session resource limits (e.g. max AI credits).
@property (nonatomic, strong, nullable) CPSessionLimitsConfig *sessionLimits;
/// Long-term memory configuration.
@property (nonatomic, strong, nullable) CPMemoryConfiguration *memory;
/// OTLP export protocol for telemetry ("grpc" or "http/protobuf").
@property (nonatomic, copy, nullable) NSString *otlpProtocol;
/// Stream model responses over WebSocket transport.
@property (nonatomic, copy, nullable) NSNumber *enableWebSocketResponses;
/// Experiment assignment overrides.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *expAssignments;
/// Handler invoked when an MCP server requires OAuth authorization.
@property (nonatomic, copy, nullable) CPMcpAuthHandler onMcpAuthRequest;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Client Options

/// Options for configuring a CopilotClient.
@interface CPCopilotClientOptions : NSObject

@property (nonatomic, copy, nullable) NSString *cliPath;
@property (nonatomic, copy, nullable) NSString *cliUrl;
@property (nonatomic, copy, nullable) NSString *cwd;
@property (nonatomic, copy, nullable) NSString *logLevel;
@property (nonatomic, assign) BOOL autoStart;
@property (nonatomic, assign) BOOL autoRestart;
@property (nonatomic, copy, nullable) NSString *githubToken;
@property (nonatomic, assign) BOOL useLoggedInUser;
@property (nonatomic, copy, nullable) NSArray<NSString *> *extraArgs;
@property (nonatomic, strong, nullable) CPSessionFsConfig *sessionFs;
@property (nonatomic, assign) NSInteger sessionIdleTimeoutSeconds;
@property (nonatomic, copy, nullable) NSString *copilotHome;
@property (nonatomic, copy, nullable) NSString *tcpConnectionToken;
/// Custom transport for outbound Copilot HTTP requests.
@property (nonatomic, copy, nullable) CPCopilotRequestHandler requestHandler;
/// Supplies bearer tokens for outbound model requests (bring-your-own-key).
@property (nonatomic, copy, nullable) CPBearerTokenProvider bearerTokenProvider;

+ (instancetype)defaultOptions;

@end

NS_ASSUME_NONNULL_END
