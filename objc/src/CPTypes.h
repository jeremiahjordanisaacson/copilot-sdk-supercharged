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

#pragma mark - Session Event

/// An event received from the session.
@interface CPSessionEvent : NSObject

@property (nonatomic, copy, readonly) NSString *type;
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

+ (instancetype)defaultOptions;

@end

NS_ASSUME_NONNULL_END
