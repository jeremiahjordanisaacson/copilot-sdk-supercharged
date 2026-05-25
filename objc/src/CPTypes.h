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

/// Request for user input from the agent.
@interface CPUserInputRequest : NSObject

@property (nonatomic, copy, nullable) NSString *question;
@property (nonatomic, copy, nullable) NSArray<NSString *> *choices;
@property (nonatomic, strong, nullable) NSNumber *allowFreeform;

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict;

@end

/// Response to a user input request.
@interface CPUserInputResponse : NSObject

@property (nonatomic, copy) NSString *answer;
@property (nonatomic, assign) BOOL wasFreeform;

- (NSDictionary<NSString *, id> *)toDictionary;

@end

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
@property (nonatomic, copy, nullable) CPTraceContextProvider onGetTraceContext;

+ (instancetype)defaultOptions;

@end

NS_ASSUME_NONNULL_END
