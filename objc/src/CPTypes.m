/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPTypes.h"

#pragma mark - CPToolInvocation

@implementation CPToolInvocation

- (instancetype)initWithSessionId:(NSString *)sessionId
                       toolCallId:(NSString *)toolCallId
                         toolName:(NSString *)toolName
                        arguments:(nullable NSDictionary<NSString *, id> *)arguments {
    self = [super init];
    if (self) {
        _sessionId = [sessionId copy];
        _toolCallId = [toolCallId copy];
        _toolName = [toolName copy];
        _arguments = [arguments copy];
    }
    return self;
}

@end

#pragma mark - CPToolResult

@implementation CPToolResult

+ (instancetype)successWithText:(NSString *)text {
    CPToolResult *result = [[CPToolResult alloc] init];
    result.textResultForLlm = text;
    result.resultType = CPToolResultTypeSuccess;
    return result;
}

+ (instancetype)failureWithError:(NSString *)error {
    CPToolResult *result = [[CPToolResult alloc] init];
    result.textResultForLlm = error;
    result.resultType = CPToolResultTypeFailure;
    result.error = error;
    return result;
}

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"textResultForLlm"] = self.textResultForLlm ?: @"";

    switch (self.resultType) {
        case CPToolResultTypeSuccess:  dict[@"resultType"] = @"success"; break;
        case CPToolResultTypeFailure:  dict[@"resultType"] = @"failure"; break;
        case CPToolResultTypeRejected: dict[@"resultType"] = @"rejected"; break;
        case CPToolResultTypeDenied:   dict[@"resultType"] = @"denied"; break;
    }

    if (self.error) dict[@"error"] = self.error;
    if (self.sessionLog) dict[@"sessionLog"] = self.sessionLog;
    return [dict copy];
}

@end

#pragma mark - CPTraceContext

@implementation CPTraceContext
@end

#pragma mark - CPSlashCommandInput

@implementation CPSlashCommandInput

- (instancetype)initWithHint:(NSString *)hint
                  completion:(CPSlashCommandInputCompletion)completion
               hasCompletion:(BOOL)hasCompletion {
    self = [super init];
    if (self) {
        _hint = [hint copy];
        _completion = completion;
        _hasCompletion = hasCompletion;
    }
    return self;
}

@end

#pragma mark - CPSlashCommandInfo

@implementation CPSlashCommandInfo

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _allowDuringAgentExecution = [dict[@"allowDuringAgentExecution"] boolValue];
        _commandDescription = dict[@"description"] ?: @"";
        _name = dict[@"name"] ?: @"";

        NSString *kindStr = dict[@"kind"];
        if ([kindStr isEqualToString:@"client"]) _kind = CPSlashCommandKindClient;
        else if ([kindStr isEqualToString:@"skill"]) _kind = CPSlashCommandKindSkill;
        else _kind = CPSlashCommandKindBuiltin;

        _aliases = dict[@"aliases"];
        _experimental = dict[@"experimental"];

        NSDictionary *inputDict = dict[@"input"];
        if (inputDict) {
            NSString *comp = inputDict[@"completion"];
            _input = [[CPSlashCommandInput alloc] initWithHint:inputDict[@"hint"] ?: @""
                                                    completion:CPSlashCommandInputCompletionDirectory
                                                 hasCompletion:(comp != nil)];
        }
    }
    return self;
}

@end

#pragma mark - CPCommandsInvokeRequest

@implementation CPCommandsInvokeRequest

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"name"] = self.name;
    if (self.input) dict[@"input"] = self.input;
    return [dict copy];
}

@end

#pragma mark - CPCommandsListRequest

@implementation CPCommandsListRequest

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.includeBuiltins) dict[@"includeBuiltins"] = self.includeBuiltins;
    if (self.includeClientCommands) dict[@"includeClientCommands"] = self.includeClientCommands;
    if (self.includeSkills) dict[@"includeSkills"] = self.includeSkills;
    return [dict copy];
}

@end

#pragma mark - CPModelBillingTokenPrices

@implementation CPModelBillingTokenPrices

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _batchSize = dict[@"batchSize"];
        _cachePrice = dict[@"cachePrice"];
        _inputPrice = dict[@"inputPrice"];
        _outputPrice = dict[@"outputPrice"];
    }
    return self;
}

@end

#pragma mark - CPSkillsLoadDiagnostics

@implementation CPSkillsLoadDiagnostics

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _errors = dict[@"errors"] ?: @[];
        _warnings = dict[@"warnings"] ?: @[];
    }
    return self;
}

@end

#pragma mark - CPRemoteEnableRequest

@implementation CPRemoteEnableRequest

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.mode) {
        switch ((CPRemoteSessionMode)[self.mode integerValue]) {
            case CPRemoteSessionModeExport: dict[@"mode"] = @"export"; break;
            case CPRemoteSessionModeOff:    dict[@"mode"] = @"off"; break;
            case CPRemoteSessionModeOn:     dict[@"mode"] = @"on"; break;
        }
    }
    return [dict copy];
}

@end

#pragma mark - CPRemoteEnableResult

@implementation CPRemoteEnableResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _remoteSteerable = [dict[@"remoteSteerable"] boolValue];
        _url = dict[@"url"];
    }
    return self;
}

@end

#pragma mark - CPSessionEvent

@implementation CPSessionEvent

- (instancetype)initWithType:(NSString *)type data:(nullable NSDictionary<NSString *, id> *)data {
    self = [super init];
    if (self) {
        _type = [type copy];
        _rawData = [data copy];

        if (data) {
            _content = data[@"content"];
            _deltaContent = data[@"delta"];
            _message = data[@"message"];
            _toolName = data[@"toolName"];
            _toolCallId = data[@"toolCallId"];
        }
    }
    return self;
}

@end

#pragma mark - CPSessionFsConfig

@implementation CPSessionFsConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.initialCwd) dict[@"initialCwd"] = self.initialCwd;
    if (self.sessionStatePath) dict[@"sessionStatePath"] = self.sessionStatePath;
    if (self.conventions) dict[@"conventions"] = self.conventions;
    return [dict copy];
}

@end

#pragma mark - CPSessionMetadata

@implementation CPSessionMetadata

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = dict[@"sessionId"] ?: @"";
        _startTime = dict[@"startTime"];
        _modifiedTime = dict[@"modifiedTime"];
        _summary = dict[@"summary"];
        _isRemote = [dict[@"isRemote"] boolValue];
    }
    return self;
}

@end

#pragma mark - CPAttachment

@implementation CPAttachment

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"path"] = self.path;

    switch (self.type) {
        case CPAttachmentTypeFile:      dict[@"type"] = @"file"; break;
        case CPAttachmentTypeDirectory: dict[@"type"] = @"directory"; break;
        case CPAttachmentTypeSelection: dict[@"type"] = @"selection"; break;
    }

    if (self.displayName) dict[@"displayName"] = self.displayName;
    return [dict copy];
}

@end

#pragma mark - CPMessageOptions

@implementation CPMessageOptions

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"prompt"] = self.prompt;

    if (self.attachments.count > 0) {
        NSMutableArray *arr = [NSMutableArray array];
        for (CPAttachment *a in self.attachments) {
            [arr addObject:[a toDictionary]];
        }
        dict[@"attachments"] = arr;
    }

    if (self.mode) dict[@"mode"] = self.mode;
    return [dict copy];
}

@end

#pragma mark - CPSessionConfig

@implementation CPSessionConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];

    if (self.sessionId) dict[@"sessionId"] = self.sessionId;
    if (self.model) dict[@"model"] = self.model;
    if (self.reasoningEffort) dict[@"reasoningEffort"] = self.reasoningEffort;
    if (self.workingDirectory) dict[@"workingDirectory"] = self.workingDirectory;
    dict[@"streaming"] = @(self.streaming);
    if (self.tools) dict[@"tools"] = self.tools;
    if (self.systemMessage) dict[@"systemMessage"] = self.systemMessage;
    if (self.githubToken) dict[@"githubToken"] = self.githubToken;
    if (self.instructionDirectories) dict[@"instructionDirectories"] = self.instructionDirectories;
    if (self.permissionHandler) dict[@"requestPermission"] = @YES;
    if (self.exitPlanModeHandler) dict[@"requestExitPlanMode"] = @YES;
    if (self.enableSessionTelemetry) dict[@"enableSessionTelemetry"] = @YES;
    if (self.modelCapabilities) dict[@"modelCapabilities"] = self.modelCapabilities;

    return [dict copy];
}

@end

#pragma mark - CPCopilotClientOptions

@implementation CPCopilotClientOptions

+ (instancetype)defaultOptions {
    CPCopilotClientOptions *opts = [[CPCopilotClientOptions alloc] init];
    opts.autoStart = YES;
    opts.autoRestart = YES;
    opts.useLoggedInUser = YES;
    opts.logLevel = @"info";
    return opts;
}

@end
