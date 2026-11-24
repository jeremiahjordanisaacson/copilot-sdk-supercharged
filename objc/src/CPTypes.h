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

#pragma mark - Exit Plan Mode

/// Block type for exit plan mode request handlers.
/// The handler receives the session ID and must call the completion block with YES (approved) or NO (denied).
typedef void (^CPExitPlanModeHandler)(NSString *sessionId,
                                      void (^completion)(BOOL approved));

#pragma mark - Trace Context

/// Trace context for distributed tracing.
@interface CPTraceContext : NSObject

@property (nonatomic, copy, nullable) NSString *traceparent;
@property (nonatomic, copy, nullable) NSString *tracestate;

@end

/// Block type for trace context provider.
typedef CPTraceContext * _Nullable (^CPTraceContextProvider)(void);

#pragma mark - Slash Command Types

/// Completion type for slash command inputs.
typedef NS_ENUM(NSInteger, CPSlashCommandInputCompletion) {
    CPSlashCommandInputCompletionDirectory = 0,
};

/// Kind of slash command.
typedef NS_ENUM(NSInteger, CPSlashCommandKind) {
    CPSlashCommandKindBuiltin = 0,
    CPSlashCommandKindClient,
    CPSlashCommandKindSkill,
};

/// Price category for model picker.
typedef NS_ENUM(NSInteger, CPModelPickerPriceCategory) {
    CPModelPickerPriceCategoryHigh = 0,
    CPModelPickerPriceCategoryLow,
    CPModelPickerPriceCategoryMedium,
    CPModelPickerPriceCategoryVeryHigh,
};

/// Input definition for a slash command.
@interface CPSlashCommandInput : NSObject

@property (nonatomic, copy, readonly) NSString *hint;
@property (nonatomic, assign, readonly) CPSlashCommandInputCompletion completion;
@property (nonatomic, assign, readonly) BOOL hasCompletion;

- (instancetype)initWithHint:(NSString *)hint
                  completion:(CPSlashCommandInputCompletion)completion
               hasCompletion:(BOOL)hasCompletion;

@end

/// Information about a slash command.
@interface CPSlashCommandInfo : NSObject

@property (nonatomic, assign, readonly) BOOL allowDuringAgentExecution;
@property (nonatomic, copy, readonly) NSString *commandDescription;
@property (nonatomic, assign, readonly) CPSlashCommandKind kind;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, copy, readonly, nullable) NSArray<NSString *> *aliases;
@property (nonatomic, strong, readonly, nullable) NSNumber *experimental;
@property (nonatomic, strong, readonly, nullable) CPSlashCommandInput *input;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Request to invoke a command.
@interface CPCommandsInvokeRequest : NSObject

@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy, nullable) NSString *input;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Request to list available commands.
@interface CPCommandsListRequest : NSObject

@property (nonatomic, strong, nullable) NSNumber *includeBuiltins;
@property (nonatomic, strong, nullable) NSNumber *includeClientCommands;
@property (nonatomic, strong, nullable) NSNumber *includeSkills;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Token pricing information for model billing.
@interface CPModelBillingTokenPrices : NSObject

@property (nonatomic, strong, nullable) NSNumber *batchSize;
@property (nonatomic, strong, nullable) NSNumber *cachePrice;
@property (nonatomic, strong, nullable) NSNumber *inputPrice;
@property (nonatomic, strong, nullable) NSNumber *outputPrice;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Experimental: Diagnostics from loading skills.
@interface CPSkillsLoadDiagnostics : NSObject

@property (nonatomic, copy, readonly) NSArray<NSString *> *errors;
@property (nonatomic, copy, readonly) NSArray<NSString *> *warnings;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

#pragma mark - Remote Session

/// Mode for remote session control.
typedef NS_ENUM(NSInteger, CPRemoteSessionMode) {
    CPRemoteSessionModeExport = 0,
    CPRemoteSessionModeOff,
    CPRemoteSessionModeOn,
};

/// Experimental: Request to enable or configure a remote session.
@interface CPRemoteEnableRequest : NSObject

@property (nonatomic, strong, nullable) NSNumber *mode; // CPRemoteSessionMode wrapped as NSNumber, nil if not set

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Experimental: Result of enabling a remote session.
@interface CPRemoteEnableResult : NSObject

@property (nonatomic, assign, readonly) BOOL remoteSteerable;
@property (nonatomic, copy, readonly, nullable) NSString *url;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

#pragma mark - Cloud Session

/// GitHub repository metadata associated with a cloud session.
@interface CPCloudSessionRepository : NSObject

@property (nonatomic, copy) NSString *owner;
@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy, nullable) NSString *branch;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Options for creating a remote session in the cloud.
@interface CPCloudSessionOptions : NSObject

@property (nonatomic, strong, nullable) CPCloudSessionRepository *repository;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - Canvas Types

/// A single agent-callable action contributed by a canvas.
@interface CPCanvasAction : NSObject

@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy) NSString *actionDescription;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *inputSchema;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Declarative metadata for a single canvas.
@interface CPCanvasDeclaration : NSObject

@property (nonatomic, copy) NSString *canvasId;
@property (nonatomic, copy) NSString *displayName;
@property (nonatomic, copy) NSString *canvasDescription;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *inputSchema;
@property (nonatomic, copy, nullable) NSArray<CPCanvasAction *> *actions;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Response returned from a canvas open handler.
@interface CPCanvasOpenResponse : NSObject

@property (nonatomic, copy, nullable) NSString *url;
@property (nonatomic, copy, nullable) NSString *title;
@property (nonatomic, copy, nullable) NSString *status;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Host capability details passed to canvas callbacks.
@interface CPCanvasHostCapabilities : NSObject

@property (nonatomic, assign) BOOL canvases;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Host context passed to canvas callbacks.
@interface CPCanvasHostContext : NSObject

@property (nonatomic, strong) CPCanvasHostCapabilities *capabilities;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Context handed to a canvas open handler.
@interface CPCanvasOpenContext : NSObject

@property (nonatomic, copy) NSString *sessionId;
@property (nonatomic, copy) NSString *extensionId;
@property (nonatomic, copy) NSString *canvasId;
@property (nonatomic, copy) NSString *instanceId;
@property (nonatomic, strong) id input;
@property (nonatomic, strong, nullable) CPCanvasHostContext *host;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Context handed to a canvas action handler.
@interface CPCanvasActionContext : NSObject

@property (nonatomic, copy) NSString *sessionId;
@property (nonatomic, copy) NSString *extensionId;
@property (nonatomic, copy) NSString *canvasId;
@property (nonatomic, copy) NSString *instanceId;
@property (nonatomic, copy) NSString *actionName;
@property (nonatomic, strong) id input;
@property (nonatomic, strong, nullable) CPCanvasHostContext *host;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Context handed to a canvas lifecycle handler.
@interface CPCanvasLifecycleContext : NSObject

@property (nonatomic, copy) NSString *sessionId;
@property (nonatomic, copy) NSString *extensionId;
@property (nonatomic, copy) NSString *canvasId;
@property (nonatomic, copy) NSString *instanceId;
@property (nonatomic, strong, nullable) CPCanvasHostContext *host;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

#pragma mark - System Message Configuration

/// Override operation for a single system prompt section.
@interface CPSectionOverride : NSObject

@property (nonatomic, copy) NSString *action;
@property (nonatomic, copy, nullable) NSString *content;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// System message in append mode (default).
@interface CPSystemMessageAppendConfig : NSObject

@property (nonatomic, copy, nullable) NSString *content;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// System message in replace mode.
@interface CPSystemMessageReplaceConfig : NSObject

@property (nonatomic, copy) NSString *content;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// System message in customize mode.
@interface CPSystemMessageCustomizeConfig : NSObject

@property (nonatomic, copy, nullable) NSDictionary<NSString *, CPSectionOverride *> *sections;
@property (nonatomic, copy, nullable) NSString *content;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

#pragma mark - User Input Types

/// Image support advertised by a `userInput.request` (`CPUserInputRequest.imageAttachmentConstraints`).
/// Absent (nil) when the runtime does not accept images in the answer.
@interface CPUserInputImageAttachmentConstraints : NSObject

/// MIME types accepted by the request.
@property (nonatomic, copy, readonly) NSArray<NSString *> *allowedMimeTypes;
/// Maximum decoded byte length of each image.
@property (nonatomic, assign, readonly) NSInteger maxBytesPerImage;
/// Maximum number of images accepted in one answer.
@property (nonatomic, assign, readonly) NSInteger maxCount;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// An inline image attached to an ask_user answer (`CPUserInputResponse.attachments`).
/// The wire `type` is always "image".
@interface CPUserInputImageAttachment : NSObject

/// Base64-encoded image bytes, without a data URL prefix.
@property (nonatomic, copy) NSString *data;
/// MIME type of the image, matching one of the request's `allowedMimeTypes`.
@property (nonatomic, copy) NSString *mimeType;
/// Optional description of the image (wire `description`).
@property (nonatomic, copy, nullable) NSString *imageDescription;
/// Optional client-facing name of the image.
@property (nonatomic, copy, nullable) NSString *displayName;

+ (instancetype)attachmentWithData:(NSString *)data mimeType:(NSString *)mimeType;
- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Request for user input from the agent.
@interface CPUserInputRequest : NSObject

@property (nonatomic, copy, nullable) NSString *question;
@property (nonatomic, copy, nullable) NSArray<NSString *> *choices;
@property (nonatomic, strong, nullable) NSNumber *allowFreeform;
/// Runtime ID of the pending request, when the runtime sends it.
@property (nonatomic, copy, nullable) NSString *requestId;
/// Image support advertised by the runtime; nil when the answer cannot carry images.
@property (nonatomic, strong, nullable) CPUserInputImageAttachmentConstraints *imageAttachmentConstraints;
/// The request's opaque `_meta` object, forwarded unchanged when the runtime sends it.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *meta;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Response to a user input request.
@interface CPUserInputResponse : NSObject

@property (nonatomic, copy) NSString *answer;
@property (nonatomic, assign) BOOL wasFreeform;
/// Images attached to the answer, sent only when set. Only valid when the request advertised
/// `imageAttachmentConstraints`; the runtime enforces the advertised limits. Image-only
/// answers use an empty `answer` and `wasFreeform = YES`.
@property (nonatomic, copy, nullable) NSArray<CPUserInputImageAttachment *> *attachments;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

/// Answers the agent's ask_user questions (`userInput.request`). Call `completion` with nil to
/// fail the request.
typedef void (^CPUserInputHandler)(CPUserInputRequest *request,
                                   NSString *sessionId,
                                   void (^completion)(CPUserInputResponse * _Nullable response));

#pragma mark - Response Format

/// Response format for message responses.
typedef NS_ENUM(NSInteger, CPResponseFormat) {
    CPResponseFormatText = 0,
    CPResponseFormatImage,
    CPResponseFormatJSONObject,
};

/// Options for image generation.
@interface CPImageOptions : NSObject

@property (nonatomic, copy, nullable) NSString *size;
@property (nonatomic, copy, nullable) NSString *quality;
@property (nonatomic, copy, nullable) NSString *style;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

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
@property (nonatomic, copy, nullable) CPHookHandler onUserPromptTransformed;
@property (nonatomic, copy, nullable) CPHookHandler onSessionStart;
@property (nonatomic, copy, nullable) CPHookHandler onSessionEnd;
@property (nonatomic, copy, nullable) CPHookHandler onErrorOccurred;
/// Fired before a sub-agent's first turn (`subagentStart`).
@property (nonatomic, copy, nullable) CPHookHandler onSubagentStart;
/// Fired after a sub-agent completes a turn (`subagentStop`).
@property (nonatomic, copy, nullable) CPHookHandler onSubagentStop;

/// Returns YES if at least one hook handler is registered.
- (BOOL)hasAnyHandler;
/// Returns the wire identifiers of the registered hook types.
- (NSArray<NSString *> *)hookTypes;
/// Returns the handler for a `hooks.invoke` hook type (for example `CPHookTypeSubagentStart`), if registered.
- (nullable CPHookHandler)handlerForHookType:(NSString *)hookType;
@end

#pragma mark - v2.6.0 / v3.0.0 Types (SDK protocol 4)

/// Client modes (`CPCopilotClientOptions.mode`). The default keeps the full coding-agent defaults.
FOUNDATION_EXPORT NSString * const CPClientModeCodingAgent;
/// Minimal defaults; every create/resume must set `CPSessionConfig.availableTools`.
FOUNDATION_EXPORT NSString * const CPClientModeEmpty;
/// Legacy alias of `CPClientModeCodingAgent`.
FOUNDATION_EXPORT NSString * const CPClientModeCopilotCli;

/// Auto routing tiers for the `auto` model (`CPSessionConfig.autoTier`).
FOUNDATION_EXPORT NSString * const CPAutoTierFast;
FOUNDATION_EXPORT NSString * const CPAutoTierEfficiency;
FOUNDATION_EXPORT NSString * const CPAutoTierBalance;
FOUNDATION_EXPORT NSString * const CPAutoTierIntelligence;

/// Ask-user tool variants (`CPSessionConfig.askUserVariant`).
FOUNDATION_EXPORT NSString * const CPAskUserVariantLegacy;
FOUNDATION_EXPORT NSString * const CPAskUserVariantElicitation;

/// Working-directory trust scopes (`CPSessionConfig.trustWorkingDirectory`).
FOUNDATION_EXPORT NSString * const CPTrustWorkingDirectorySession;
FOUNDATION_EXPORT NSString * const CPTrustWorkingDirectoryUser;

/// Message provenance values (`CPMessageOptions.source`).
FOUNDATION_EXPORT NSString * const CPMessageSourceUser;
FOUNDATION_EXPORT NSString * const CPMessageSourceSystem;
/// Returns "agent-<agentId>" for a message that originates from an identified agent.
FOUNDATION_EXPORT NSString *CPMessageSourceAgent(NSString *agentId);

/// Hook types delivered through `hooks.invoke` for sub-agent lifecycle events.
FOUNDATION_EXPORT NSString * const CPHookTypeSubagentStart;
FOUNDATION_EXPORT NSString * const CPHookTypeSubagentStop;

/// Permission modes (`CPSessionConfig.permissionMode`, `-[CPCopilotSession setPermissionMode:completion:]`).
FOUNDATION_EXPORT NSString * const CPPermissionModeManual;
FOUNDATION_EXPORT NSString * const CPPermissionModeAssisted;
FOUNDATION_EXPORT NSString * const CPPermissionModeAllowAll;
/// Reason reported when managed policy refused a permission mode change.
FOUNDATION_EXPORT NSString * const CPPermissionModeRefusalReasonManagedPolicy;

/// Live, append-only shell output event type (not persisted or replayed).
FOUNDATION_EXPORT NSString * const CPEventTypeToolShellOutput;

/// Input kinds of a shell authentication request ("secret" input must be masked).
FOUNDATION_EXPORT NSString * const CPShellAuthenticationInputKindSecret;
FOUNDATION_EXPORT NSString * const CPShellAuthenticationInputKindText;
FOUNDATION_EXPORT NSString * const CPShellAuthenticationInputKindConfirm;

/// Traffic that network callbacks serve (`CPNetworkOptions.scope`).
FOUNDATION_EXPORT NSString * const CPNetworkScopeConnection;
FOUNDATION_EXPORT NSString * const CPNetworkScopeRuntime;

/// `NSError.userInfo` key holding the `CPPolicyHelperFailure` of a failed session.create or session.resume.
FOUNDATION_EXPORT NSString * const CPPolicyHelperFailureErrorKey;

/// Identity of the integrating application, sent as `clientInfo` on the `connect` handshake.
/// Empty fields are dropped; the handshake is skipped when every field is empty.
@interface CPClientInfo : NSObject
@property (nonatomic, copy, nullable) NSString *editorName;
@property (nonatomic, copy, nullable) NSString *editorVersion;
@property (nonatomic, copy, nullable) NSString *extensionName;
@property (nonatomic, copy, nullable) NSString *extensionVersion;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Arguments passed to a GitHub token provider (`gitHubToken.getToken`).
@interface CPGitHubTokenProviderArgs : NSObject
/// Effective GitHub host the token is requested for.
@property (nonatomic, copy) NSString *host;
/// Session receiving the token, when known.
@property (nonatomic, copy, nullable) NSString *sessionId;
/// "initial" or "refresh".
@property (nonatomic, copy) NSString *reason;
@end

/// A GitHub token returned by a `CPGitHubTokenProvider`.
@interface CPGitHubToken : NSObject
@property (nonatomic, copy) NSString *accessToken;
/// OAuth token type; bearer when nil.
@property (nonatomic, copy, nullable) NSString *tokenType;
/// Remaining token lifetime in seconds.
@property (nonatomic, assign) NSInteger expiresIn;
+ (instancetype)tokenWithAccessToken:(NSString *)accessToken expiresIn:(NSInteger)expiresIn;
@end

/// Acquires GitHub tokens for one session. Call `completion` with nil to cancel the request.
typedef void (^CPGitHubTokenProvider)(CPGitHubTokenProviderArgs *args,
                                      void (^completion)(CPGitHubToken * _Nullable token));

/// Sandbox configuration (`permissions.sandbox`).
@interface CPSandboxConfig : NSObject
@property (nonatomic, assign) BOOL enabled;
/// User-managed policy fragment merged into the auto-discovered base policy.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *userPolicy;
@property (nonatomic, copy, nullable) NSNumber *addCurrentWorkingDirectory;
@property (nonatomic, copy, nullable) NSNumber *sandboxMcpServers;
@property (nonatomic, copy, nullable) NSNumber *sandboxLspServers;
@property (nonatomic, copy, nullable) NSNumber *sandboxExtensions;
/// Lets the agent request that an individual command run outside the sandbox through the permission flow.
@property (nonatomic, copy, nullable) NSNumber *allowBypass;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Session-level image generation configuration (`tools.imageGeneration`).
@interface CPImageGenerationConfig : NSObject
@property (nonatomic, copy, nullable) NSNumber *enabled;
@property (nonatomic, copy, nullable) NSNumber *watermark;
@property (nonatomic, copy, nullable) NSNumber *saveToDisk;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Autopilot continuation options (`execution.autopilot`).
@interface CPAutopilotOptions : NSObject
@property (nonatomic, copy, nullable) NSNumber *continuationEnabled;
/// Maximum automatic continuations outside an active objective.
@property (nonatomic, copy, nullable) NSNumber *maxContinues;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Named BYOK provider connection (`inference.providers[]`).
@interface CPNamedProviderConfig : NSObject
/// Name referenced by `CPProviderModelConfig.provider` (must not contain "/").
@property (nonatomic, copy) NSString *name;
/// Provider type, for example "openai", "azure", or "anthropic".
@property (nonatomic, copy, nullable) NSString *type;
@property (nonatomic, copy) NSString *baseUrl;
@property (nonatomic, copy, nullable) NSString *apiKey;
/// Command run before each request; its output is used as the API key.
@property (nonatomic, copy, nullable) NSString *apiKeyCommand;
@property (nonatomic, copy, nullable) NSString *bearerToken;
/// "completions" or "responses".
@property (nonatomic, copy, nullable) NSString *wireApi;
/// "http" or "websockets".
@property (nonatomic, copy, nullable) NSString *transport;
/// Azure API version (sent as `azure.apiVersion`).
@property (nonatomic, copy, nullable) NSString *azureApiVersion;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, NSString *> *headers;
@property (nonatomic, copy, nullable) NSString *modelProvider;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// BYOK model definition (`inference.models[]`). Select it with the model "<provider>/<identifier>".
@interface CPProviderModelConfig : NSObject
/// Name of the `CPNamedProviderConfig` serving this model.
@property (nonatomic, copy) NSString *provider;
/// Provider-local model id (wire `id`).
@property (nonatomic, copy) NSString *identifier;
/// Well-known base model id used for capability lookup.
@property (nonatomic, copy, nullable) NSString *modelId;
/// Model name sent to the provider API.
@property (nonatomic, copy, nullable) NSString *wireModel;
/// Display name (wire `name`).
@property (nonatomic, copy, nullable) NSString *displayName;
@property (nonatomic, copy, nullable) NSNumber *maxPromptTokens;
@property (nonatomic, copy, nullable) NSNumber *maxContextWindowTokens;
@property (nonatomic, copy, nullable) NSNumber *maxOutputTokens;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *capabilities;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Copilot API options (`github.capi`).
@interface CPCapiConfig : NSObject
/// Enables Copilot API models alongside or instead of BYOK providers.
@property (nonatomic, copy, nullable) NSNumber *enabled;
@end

/// Result of `-[CPCopilotSession setPermissionMode:completion:]`.
@interface CPPermissionModeResult : NSObject
@property (nonatomic, assign, readonly) BOOL success;
@property (nonatomic, copy, readonly) NSString *mode;
/// Why the change was refused (for example `CPPermissionModeRefusalReasonManagedPolicy`), when known.
@property (nonatomic, copy, readonly, nullable) NSString *reason;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Data of a `tool.shell_output` event (`CPEventTypeToolShellOutput`).
@interface CPToolShellOutputData : NSObject
@property (nonatomic, copy, readonly) NSString *toolCallId;
@property (nonatomic, copy, readonly) NSString *text;
/// Zero-based publication sequence across all streams of the tool call.
@property (nonatomic, assign, readonly) NSInteger sequence;
/// "stdout", "stderr", or nil (stdout).
@property (nonatomic, copy, readonly, nullable) NSString *stream;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// A password or other answer a shell command run by the agent asks for
/// (`shellAuthentication.request`). The answer goes only to the program that asked.
@interface CPShellAuthenticationRequest : NSObject
@property (nonatomic, copy, readonly) NSString *sessionId;
@property (nonatomic, copy, readonly) NSString *requestId;
/// Program that asked, such as "sudo", "ssh", or "git".
@property (nonatomic, copy, readonly) NSString *program;
@property (nonatomic, copy, readonly) NSString *prompt;
/// One of the `CPShellAuthenticationInputKind*` constants.
@property (nonatomic, copy, readonly) NSString *inputKind;
@property (nonatomic, copy, readonly) NSString *command;
@property (nonatomic, copy, readonly) NSString *cwd;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Answer to a shell authentication request.
@interface CPShellAuthenticationResponse : NSObject
/// Gives `value` to the program that asked (answer "confirm" requests with "yes").
+ (instancetype)acceptWithValue:(NSString *)value;
/// Declines the request.
+ (instancetype)cancel;
- (NSDictionary<NSString *, id> *)toDictionary;
@end

/// Answers shell authentication requests. Never log the answer.
typedef void (^CPShellAuthenticationHandler)(CPShellAuthenticationRequest *request,
                                             void (^completion)(CPShellAuthenticationResponse *response));

/// A memory the agent asked to store, after validation, secret filtering, and approval (`memoryStore.store`).
@interface CPStoredMemory : NSObject
@property (nonatomic, copy, readonly) NSString *sessionId;
@property (nonatomic, copy, readonly) NSString *subject;
@property (nonatomic, copy, readonly) NSString *fact;
@property (nonatomic, copy, readonly) NSString *citations;
@property (nonatomic, copy, readonly) NSString *reason;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Persists a stored memory. Call `completion` with an error to fail the store request.
typedef void (^CPMemoryStoreHandler)(CPStoredMemory *memory,
                                     void (^completion)(NSError * _Nullable error));

/// A proxy's Basic authentication challenge (`network.authenticateBasic`).
@interface CPNetworkBasicAuthRequest : NSObject
@property (nonatomic, copy, readonly) NSString *proxy;
/// Realm named by the proxy (untrusted text), when present.
@property (nonatomic, copy, readonly, nullable) NSString *realm;
/// YES when the proxy rejected the credentials sent last.
@property (nonatomic, assign, readonly) BOOL previousRejected;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Basic credentials; leave `username` nil to supply none.
@interface CPNetworkBasicCredentials : NSObject
@property (nonatomic, copy, nullable) NSString *username;
@property (nonatomic, copy, nullable) NSString *password;
@end

/// Returns a PAC-style proxy list (for example "PROXY host:8080; DIRECT"), or nil to let the
/// runtime resolve the proxy itself.
typedef void (^CPNetworkResolveProxyHandler)(NSString *url, void (^completion)(NSString * _Nullable proxy));
/// Returns a base64 SPNEGO token for a Kerberos service principal, or nil.
typedef void (^CPNetworkAuthenticateNegotiateHandler)(NSString *spn, void (^completion)(NSString * _Nullable token));
/// Answers a proxy Basic challenge.
typedef void (^CPNetworkAuthenticateBasicHandler)(CPNetworkBasicAuthRequest *request,
                                                  void (^completion)(CPNetworkBasicCredentials * _Nullable credentials));

/// Network callbacks registered with `network.setProvider` after connecting.
/// The SDK never logs tokens or passwords.
@interface CPNetworkOptions : NSObject
/// `CPNetworkScopeConnection` (the runtime default) or `CPNetworkScopeRuntime`.
@property (nonatomic, copy, nullable) NSString *scope;
@property (nonatomic, copy, nullable) CPNetworkResolveProxyHandler resolveProxy;
@property (nonatomic, copy, nullable) CPNetworkAuthenticateNegotiateHandler authenticateNegotiate;
@property (nonatomic, copy, nullable) CPNetworkAuthenticateBasicHandler authenticateBasic;
@end

/// Structured Policy Helper failure, reported in the JSON-RPC error data of a failed
/// session.create or session.resume and on managed-settings snapshots.
/// Helper-authored text is untrusted: render it as plain text.
@interface CPPolicyHelperFailure : NSObject
@property (nonatomic, copy, readonly) NSString *category;
@property (nonatomic, copy, readonly) NSString *failureMode;
@property (nonatomic, copy, readonly) NSString *message;
@property (nonatomic, assign, readonly) BOOL previousPolicyRetained;
@property (nonatomic, copy, readonly) NSString *reason;
@property (nonatomic, copy, readonly, nullable) NSString *severity;
@property (nonatomic, copy, readonly, nullable) NSString *authority;
@property (nonatomic, copy, readonly, nullable) NSString *code;
@property (nonatomic, copy, readonly, nullable) NSString *diagnostic;
@property (nonatomic, copy, readonly, nullable) NSString *remediation;
@property (nonatomic, copy, readonly, nullable) NSString *helpLinkUrl;
/// Field-specific findings: dictionaries with `message` and an optional `path`.
@property (nonatomic, copy, readonly, nullable) NSArray<NSDictionary<NSString *, id> *> *details;
/// Reads a failure from the failure object or from a dictionary that carries it under
/// `policyHelperFailure` (JSON-RPC error data, managed-settings snapshots).
+ (nullable instancetype)failureFromDictionary:(nullable NSDictionary<NSString *, id> *)dict;
/// The failure attached to an error returned by session.create or session.resume, if any.
+ (nullable instancetype)failureFromError:(nullable NSError *)error;
@end

#pragma mark - v3.1.0 Types (Microsoft Copilot SDK 2.0.3-preview.0 parity)

/// Result of `-[CPCopilotClient mcpSecretsSave:value:configDir:completion:]` (`mcp.secrets.save`).
@interface CPMcpSecretsSaveResult : NSObject
/// YES when the OS keychain stored the credential, NO for the file backend.
@property (nonatomic, assign, readonly) BOOL usedKeychain;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// A user-scoped approval for one installed plugin extension.
@interface CPPermissionsExtensionApproval : NSObject
/// Stable identity, for example "plugin:plugin-name:extension-name".
@property (nonatomic, copy, readonly) NSString *extensionName;
/// Name of the plugin that contributes the extension.
@property (nonatomic, copy, readonly) NSString *pluginName;
/// Exact privileged capabilities the user approved.
@property (nonatomic, copy, readonly) NSArray<NSString *> *capabilities;
/// When the approval was granted (RFC 3339 timestamp).
@property (nonatomic, copy, readonly) NSString *grantedAt;
/// Opaque installation key; pass it back unchanged to revoke the approval.
@property (nonatomic, copy, readonly) NSString *installationKey;
/// Human-readable installation label, when known.
@property (nonatomic, copy, readonly, nullable) NSString *installationLabel;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Result of `-[CPCopilotClient extensionApprovalsGrantForSession:requestId:completion:]`
/// (`permissions.extensionApprovals.grant`).
@interface CPPermissionsExtensionApprovalsGrantResult : NSObject
/// YES when the pending request was eligible and its approval was persisted.
@property (nonatomic, assign, readonly) BOOL granted;
/// The stored approval, when one was persisted.
@property (nonatomic, strong, readonly, nullable) CPPermissionsExtensionApproval *approval;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Result of `-[CPCopilotClient extensionApprovalsListForSession:completion:]`
/// (`permissions.extensionApprovals.list`).
@interface CPPermissionsExtensionApprovalsListResult : NSObject
@property (nonatomic, copy, readonly) NSArray<CPPermissionsExtensionApproval *> *approvals;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
@end

/// Result of `-[CPCopilotClient extensionApprovalsRevoke:installationKey:sessionId:completion:]`
/// (`permissions.extensionApprovals.revoke`).
@interface CPPermissionsExtensionApprovalsRevokeResult : NSObject
/// YES when a matching approval was revoked.
@property (nonatomic, assign, readonly) BOOL revoked;
- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;
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
/// Parsed data of a `tool.shell_output` event (`CPEventTypeToolShellOutput`), otherwise nil.
@property (nonatomic, strong, readonly, nullable) CPToolShellOutputData *shellOutput;

- (instancetype)initWithType:(NSString *)type data:(nullable NSDictionary<NSString *, id> *)data;

/// Creates an event from the `event` object of a `session.event` notification
/// ({id, timestamp, parentId, agentId, ephemeral, type, data}).
- (instancetype)initWithEnvelope:(NSDictionary<NSString *, id> *)envelope;

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
/// Message provenance: `CPMessageSourceUser`, `CPMessageSourceSystem`, or `CPMessageSourceAgent(agentId)`.
@property (nonatomic, copy, nullable) NSString *source;
/// JSON Schema for structured output, sent as
/// `responseFormat: {type: "json_schema", jsonSchema: {name: "response", strict: true, schema}}`.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *responseSchema;
/// Opaque client-generated turn ID echoed on this turn's events (`clientTurnId`).
@property (nonatomic, copy, nullable) NSString *clientTurnId;

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
@property (nonatomic, copy, nullable) CPExitPlanModeHandler exitPlanModeHandler;
@property (nonatomic, assign) BOOL enableSessionTelemetry;
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

// --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
/// Enable session rewind so the conversation can be rolled back.
@property (nonatomic, copy, nullable) NSNumber *rewindEnabled;
/// Extra workspace directories the session may access.
@property (nonatomic, copy, nullable) NSArray<NSString *> *additionalDirectories;
/// Names of MCP servers to disable for this session.
@property (nonatomic, copy, nullable) NSArray<NSString *> *disabledMcpServers;
/// GitHub MCP tool configuration.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *githubMcpToolConfig;
/// Canvas provider configuration.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *canvasProvider;
/// Restrict custom agents to locally-defined ones only.
@property (nonatomic, copy, nullable) NSNumber *customAgentsLocalOnly;
/// Tool-search configuration.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *toolSearch;
/// Enable experimental mode for this session.
@property (nonatomic, copy, nullable) NSNumber *experimentalMode;
/// Enable content-exclusion enforcement.
@property (nonatomic, copy, nullable) NSNumber *contentExclusion;
/// JSON schema describing arguments accepted by an agent factory (authoring).
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *argsSchema;
/// Decision context echoed on permission replies.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *decisionContext;

// --- v2.6.0 session options (parity with @github/copilot-sdk) ---
/// Session name (session.create only).
@property (nonatomic, copy, nullable) NSString *name;
/// Allowed tools (`tools.availableTools`). Required for every session in `CPClientModeEmpty`.
@property (nonatomic, copy, nullable) NSArray<NSString *> *availableTools;
/// Auto routing tier for the `auto` model (`CPAutoTierFast`, ...); sent as `github.capi.autoTier`.
@property (nonatomic, copy, nullable) NSString *autoTier;
/// Ask-user tool variant: `CPAskUserVariantLegacy` or `CPAskUserVariantElicitation`.
@property (nonatomic, copy, nullable) NSString *askUserVariant;
/// Supplies GitHub tokens through `gitHubToken.getToken`. Cannot be combined with `githubToken`.
@property (nonatomic, copy, nullable) CPGitHubTokenProvider gitHubTokenProvider;
/// Sandbox configuration (`permissions.sandbox`).
@property (nonatomic, strong, nullable) CPSandboxConfig *sandboxConfig;
/// Runtime feature-flag overrides (`experiments.flags`).
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *featureFlags;
/// Model allowlist (`policy.allowedModels`).
@property (nonatomic, copy, nullable) NSArray<NSString *> *allowedModels;
/// OAuth client ID metadata document URL used for MCP OAuth (`mcp.authClientIdMetadataUrl`).
@property (nonatomic, copy, nullable) NSString *authClientIdMetadataUrl;
/// Directories containing custom agent definitions (`agents.directories`).
@property (nonatomic, copy, nullable) NSArray<NSString *> *agentDirectories;
/// Directories containing hook definitions (`hooks.directories`).
@property (nonatomic, copy, nullable) NSArray<NSString *> *hookDirectories;
/// Whether an interactive user is present (`github.interactiveUser`).
@property (nonatomic, copy, nullable) NSNumber *interactiveUser;
/// Managed settings to exclude from enforcement (`policy.excludedManagedSettings`).
@property (nonatomic, copy, nullable) NSArray<NSString *> *excludedManagedSettings;
/// Enforce managed model defaults (`policy.enforceManagedModelDefaults`).
@property (nonatomic, copy, nullable) NSNumber *enforceManagedModelDefaults;
/// Working-directory trust: `CPTrustWorkingDirectorySession` or `CPTrustWorkingDirectoryUser` (session.create only).
@property (nonatomic, copy, nullable) NSString *trustWorkingDirectory;
/// Re-read custom instructions (session.create only).
@property (nonatomic, copy, nullable) NSNumber *refreshCustomInstructions;
/// Allow transcript recovery (session.resume only).
@property (nonatomic, copy, nullable) NSNumber *allowTranscriptRecovery;
/// Session-level image generation configuration (`tools.imageGeneration`).
@property (nonatomic, strong, nullable) CPImageGenerationConfig *imageGeneration;
/// Autopilot continuation options (`execution.autopilot`).
@property (nonatomic, strong, nullable) CPAutopilotOptions *autopilot;
/// Overrides the model's maximum output tokens (`inference.capabilities.limits.max_output_tokens`).
@property (nonatomic, copy, nullable) NSNumber *maxOutputTokens;

// --- v3.0.0 session options (SDK protocol 4 parity with @github/copilot-sdk 2.0) ---
/// Skip the session.resume event and its side effects (session.resume only).
@property (nonatomic, copy, nullable) NSNumber *suppressResumeEvent;
/// Named BYOK provider connections (`inference.providers`).
@property (nonatomic, copy, nullable) NSArray<CPNamedProviderConfig *> *providers;
/// BYOK model definitions (`inference.models`).
@property (nonatomic, copy, nullable) NSArray<CPProviderModelConfig *> *models;
/// Provider used for unqualified model IDs (`inference.defaultProvider`).
@property (nonatomic, copy, nullable) NSString *defaultProvider;
/// Sends `defaultProvider: null` to clear the default provider (session.resume only).
@property (nonatomic, assign) BOOL clearDefaultProvider;
/// Copilot API options (`github.capi`).
@property (nonatomic, strong, nullable) CPCapiConfig *capi;
/// Legacy single BYOK provider (type, baseUrl, apiKey, apiKeyCommand, bearerToken, wireApi, ...).
/// Sent as the named provider "default" with Copilot API models disabled.
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *provider;
/// Initial permission mode (`CPPermissionModeManual`, `CPPermissionModeAssisted`, `CPPermissionModeAllowAll`).
@property (nonatomic, copy, nullable) NSString *permissionMode;
/// Answers password and confirmation prompts from shell commands (`shellAuthentication.request`).
@property (nonatomic, copy, nullable) CPShellAuthenticationHandler onShellAuthentication;
/// Persists memories the agent stores (`memoryStore.store`).
@property (nonatomic, copy, nullable) CPMemoryStoreHandler onMemoryStore;

// --- v3.1.0 session options ---
/// Answers the agent's ask_user questions (`userInput.request`); setting it enables the
/// ask_user tool (`requestUserInput`). The request carries `imageAttachmentConstraints` when
/// the answer may include image `attachments`.
@property (nonatomic, copy, nullable) CPUserInputHandler userInputHandler;

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
/// Built-in plugin directories to load.
@property (nonatomic, copy, nullable) NSArray<NSString *> *builtinPluginDirectories;
/// Use the in-process FFI transport instead of spawning a CLI.
@property (nonatomic, assign) BOOL inProcess;
/// Supplies W3C trace context injected into session.create and session.resume.
@property (nonatomic, copy, nullable) CPTraceContextProvider onGetTraceContext;
/// Integration identity sent on the `connect` handshake.
@property (nonatomic, strong, nullable) CPClientInfo *clientInfo;
/// Client mode: `CPClientModeCodingAgent` (default when nil) or `CPClientModeEmpty`.
@property (nonatomic, copy, nullable) NSString *mode;
/// Network callbacks registered with `network.setProvider` after connecting.
@property (nonatomic, strong, nullable) CPNetworkOptions *network;

+ (instancetype)defaultOptions;

@end

NS_ASSUME_NONNULL_END
