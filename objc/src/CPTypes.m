/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPTypes.h"
#import "CPJsonRpcClient.h"

/// Returns `value` when it is a string (JSON values may be NSNull or another type).
static NSString *CPStringOrNil(id value) {
    return [value isKindOfClass:[NSString class]] ? value : nil;
}

static NSNumber *CPNumberOrNil(id value) {
    return [value isKindOfClass:[NSNumber class]] ? value : nil;
}

#pragma mark - Constant Identifiers

NSString * const CPToolDeferAuto = @"auto";
NSString * const CPToolDeferNever = @"never";

NSString * const CPSystemMessageSectionPreamble = @"preamble";
NSString * const CPSystemMessageSectionIdentity = @"identity";
NSString * const CPSystemMessageSectionToolInstructions = @"tool_instructions";
NSString * const CPSystemMessageSectionPreserve = @"preserve";

NSString * const CPGitHubAttachmentCommit = @"GitHubCommit";
NSString * const CPGitHubAttachmentRepository = @"GitHubRepository";
NSString * const CPGitHubAttachmentPullRequest = @"GitHubPullRequest";
NSString * const CPGitHubAttachmentIssue = @"GitHubIssue";

NSString * const CPClientModeCodingAgent = @"coding-agent";
NSString * const CPClientModeEmpty = @"empty";
NSString * const CPClientModeCopilotCli = @"copilot-cli";

NSString * const CPAutoTierFast = @"fast";
NSString * const CPAutoTierEfficiency = @"efficiency";
NSString * const CPAutoTierBalance = @"balance";
NSString * const CPAutoTierIntelligence = @"intelligence";

NSString * const CPAskUserVariantLegacy = @"legacy";
NSString * const CPAskUserVariantElicitation = @"elicitation";

NSString * const CPTrustWorkingDirectorySession = @"session";
NSString * const CPTrustWorkingDirectoryUser = @"user";

NSString * const CPMessageSourceUser = @"user";
NSString * const CPMessageSourceSystem = @"system";

NSString *CPMessageSourceAgent(NSString *agentId) {
    return [NSString stringWithFormat:@"agent-%@", agentId];
}

NSString * const CPHookTypeSubagentStart = @"subagentStart";
NSString * const CPHookTypeSubagentStop = @"subagentStop";

NSString * const CPPermissionModeManual = @"manual";
NSString * const CPPermissionModeAssisted = @"assisted";
NSString * const CPPermissionModeAllowAll = @"allow-all";
NSString * const CPPermissionModeRefusalReasonManagedPolicy = @"managed_policy";

NSString * const CPEventTypeToolShellOutput = @"tool.shell_output";

NSString * const CPShellAuthenticationInputKindSecret = @"secret";
NSString * const CPShellAuthenticationInputKindText = @"text";
NSString * const CPShellAuthenticationInputKindConfirm = @"confirm";

NSString * const CPNetworkScopeConnection = @"connection";
NSString * const CPNetworkScopeRuntime = @"runtime";

NSString * const CPPolicyHelperFailureErrorKey = @"CPPolicyHelperFailure";

#pragma mark - CPProviderTokenArgs

@implementation CPProviderTokenArgs
@end

#pragma mark - CPSessionLimitsConfig

@implementation CPSessionLimitsConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.maxAiCredits) dict[@"maxAiCredits"] = self.maxAiCredits;
    return [dict copy];
}

@end

#pragma mark - CPMemoryConfiguration

@implementation CPMemoryConfiguration

- (NSDictionary<NSString *, id> *)toDictionary {
    return @{@"enabled": @(self.enabled)};
}

@end

#pragma mark - CPSessionHooks

@implementation CPSessionHooks

- (BOOL)hasAnyHandler {
    return self.onPreToolUse != nil || self.onPostToolUse != nil
        || self.onPreMcpToolCall != nil || self.onUserPromptSubmitted != nil
        || self.onUserPromptTransformed != nil
        || self.onSessionStart != nil || self.onSessionEnd != nil
        || self.onErrorOccurred != nil
        || self.onSubagentStart != nil || self.onSubagentStop != nil;
}

- (NSArray<NSString *> *)hookTypes {
    NSMutableArray<NSString *> *types = [NSMutableArray array];
    if (self.onPreToolUse) [types addObject:@"preToolUse"];
    if (self.onPostToolUse) [types addObject:@"postToolUse"];
    if (self.onPreMcpToolCall) [types addObject:@"preMcpToolCall"];
    if (self.onUserPromptSubmitted) [types addObject:@"userPromptSubmitted"];
    if (self.onUserPromptTransformed) [types addObject:@"userPromptTransformed"];
    if (self.onSessionStart) [types addObject:@"sessionStart"];
    if (self.onSessionEnd) [types addObject:@"sessionEnd"];
    if (self.onErrorOccurred) [types addObject:@"errorOccurred"];
    if (self.onSubagentStart) [types addObject:CPHookTypeSubagentStart];
    if (self.onSubagentStop) [types addObject:CPHookTypeSubagentStop];
    return [types copy];
}

- (nullable CPHookHandler)handlerForHookType:(NSString *)hookType {
    if ([hookType isEqualToString:@"preToolUse"]) return self.onPreToolUse;
    if ([hookType isEqualToString:@"postToolUse"]) return self.onPostToolUse;
    if ([hookType isEqualToString:@"preMcpToolCall"]) return self.onPreMcpToolCall;
    if ([hookType isEqualToString:@"userPromptSubmitted"]) return self.onUserPromptSubmitted;
    if ([hookType isEqualToString:@"userPromptTransformed"]) return self.onUserPromptTransformed;
    if ([hookType isEqualToString:@"sessionStart"]) return self.onSessionStart;
    if ([hookType isEqualToString:@"sessionEnd"]) return self.onSessionEnd;
    if ([hookType isEqualToString:@"errorOccurred"]) return self.onErrorOccurred;
    if ([hookType isEqualToString:CPHookTypeSubagentStart]) return self.onSubagentStart;
    if ([hookType isEqualToString:CPHookTypeSubagentStop]) return self.onSubagentStop;
    return nil;
}

@end

#pragma mark - CPClientInfo

@implementation CPClientInfo

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.editorName.length > 0) dict[@"editorName"] = self.editorName;
    if (self.editorVersion.length > 0) dict[@"editorVersion"] = self.editorVersion;
    if (self.extensionName.length > 0) dict[@"extensionName"] = self.extensionName;
    if (self.extensionVersion.length > 0) dict[@"extensionVersion"] = self.extensionVersion;
    return [dict copy];
}

@end

#pragma mark - CPGitHubTokenProviderArgs / CPGitHubToken

@implementation CPGitHubTokenProviderArgs
@end

@implementation CPGitHubToken

+ (instancetype)tokenWithAccessToken:(NSString *)accessToken expiresIn:(NSInteger)expiresIn {
    CPGitHubToken *token = [[CPGitHubToken alloc] init];
    token.accessToken = accessToken;
    token.expiresIn = expiresIn;
    return token;
}

@end

#pragma mark - CPSandboxConfig

@implementation CPSandboxConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"enabled"] = @(self.enabled);
    if (self.userPolicy) dict[@"userPolicy"] = self.userPolicy;
    if (self.addCurrentWorkingDirectory) dict[@"addCurrentWorkingDirectory"] = self.addCurrentWorkingDirectory;
    if (self.sandboxMcpServers) dict[@"sandboxMcpServers"] = self.sandboxMcpServers;
    if (self.sandboxLspServers) dict[@"sandboxLspServers"] = self.sandboxLspServers;
    if (self.sandboxExtensions) dict[@"sandboxExtensions"] = self.sandboxExtensions;
    if (self.allowBypass) dict[@"allowBypass"] = self.allowBypass;
    return [dict copy];
}

@end

#pragma mark - CPImageGenerationConfig

@implementation CPImageGenerationConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.enabled) dict[@"enabled"] = self.enabled;
    if (self.watermark) dict[@"watermark"] = self.watermark;
    if (self.saveToDisk) dict[@"saveToDisk"] = self.saveToDisk;
    return [dict copy];
}

@end

#pragma mark - CPAutopilotOptions

@implementation CPAutopilotOptions

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.continuationEnabled) dict[@"continuationEnabled"] = self.continuationEnabled;
    if (self.maxContinues) dict[@"maxContinues"] = self.maxContinues;
    return [dict copy];
}

@end

#pragma mark - CPNamedProviderConfig

@implementation CPNamedProviderConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"name"] = self.name ?: @"";
    if (self.type) dict[@"type"] = self.type;
    dict[@"baseUrl"] = self.baseUrl ?: @"";
    if (self.apiKey) dict[@"apiKey"] = self.apiKey;
    if (self.apiKeyCommand) dict[@"apiKeyCommand"] = self.apiKeyCommand;
    if (self.bearerToken) dict[@"bearerToken"] = self.bearerToken;
    if (self.wireApi) dict[@"wireApi"] = self.wireApi;
    if (self.transport) dict[@"transport"] = self.transport;
    if (self.azureApiVersion) dict[@"azure"] = @{@"apiVersion": self.azureApiVersion};
    if (self.headers) dict[@"headers"] = self.headers;
    if (self.modelProvider) dict[@"modelProvider"] = self.modelProvider;
    return [dict copy];
}

@end

#pragma mark - CPProviderModelConfig

@implementation CPProviderModelConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"provider"] = self.provider ?: @"";
    dict[@"id"] = self.identifier ?: @"";
    if (self.modelId) dict[@"modelId"] = self.modelId;
    if (self.wireModel) dict[@"wireModel"] = self.wireModel;
    if (self.displayName) dict[@"name"] = self.displayName;
    if (self.maxPromptTokens) dict[@"maxPromptTokens"] = self.maxPromptTokens;
    if (self.maxContextWindowTokens) dict[@"maxContextWindowTokens"] = self.maxContextWindowTokens;
    if (self.maxOutputTokens) dict[@"maxOutputTokens"] = self.maxOutputTokens;
    if (self.capabilities) dict[@"capabilities"] = self.capabilities;
    return [dict copy];
}

@end

#pragma mark - CPCapiConfig

@implementation CPCapiConfig
@end

#pragma mark - CPPermissionModeResult

@implementation CPPermissionModeResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _success = [CPNumberOrNil(dict[@"success"]) boolValue];
        _mode = [(CPStringOrNil(dict[@"mode"]) ?: @"") copy];
        _reason = [CPStringOrNil(dict[@"reason"]) copy];
    }
    return self;
}

@end

#pragma mark - CPToolShellOutputData

@implementation CPToolShellOutputData

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _toolCallId = [(CPStringOrNil(dict[@"toolCallId"]) ?: @"") copy];
        _text = [(CPStringOrNil(dict[@"text"]) ?: @"") copy];
        _sequence = [CPNumberOrNil(dict[@"sequence"]) integerValue];
        _stream = [CPStringOrNil(dict[@"stream"]) copy];
    }
    return self;
}

@end

#pragma mark - CPShellAuthenticationRequest / CPShellAuthenticationResponse

@implementation CPShellAuthenticationRequest

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = [(CPStringOrNil(dict[@"sessionId"]) ?: @"") copy];
        _requestId = [(CPStringOrNil(dict[@"requestId"]) ?: @"") copy];
        _program = [(CPStringOrNil(dict[@"program"]) ?: @"") copy];
        _prompt = [(CPStringOrNil(dict[@"prompt"]) ?: @"") copy];
        _inputKind = [(CPStringOrNil(dict[@"inputKind"]) ?: @"") copy];
        _command = [(CPStringOrNil(dict[@"command"]) ?: @"") copy];
        _cwd = [(CPStringOrNil(dict[@"cwd"]) ?: @"") copy];
    }
    return self;
}

@end

@interface CPShellAuthenticationResponse ()
@property (nonatomic, copy, nullable) NSString *value;
@end

@implementation CPShellAuthenticationResponse

+ (instancetype)acceptWithValue:(NSString *)value {
    CPShellAuthenticationResponse *response = [[CPShellAuthenticationResponse alloc] init];
    response.value = value;
    return response;
}

+ (instancetype)cancel {
    return [[CPShellAuthenticationResponse alloc] init];
}

- (NSDictionary<NSString *, id> *)toDictionary {
    if (self.value) return @{@"action": @"accept", @"value": self.value};
    return @{@"action": @"cancel"};
}

@end

#pragma mark - CPStoredMemory

@implementation CPStoredMemory

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = [(CPStringOrNil(dict[@"sessionId"]) ?: @"") copy];
        _subject = [(CPStringOrNil(dict[@"subject"]) ?: @"") copy];
        _fact = [(CPStringOrNil(dict[@"fact"]) ?: @"") copy];
        _citations = [(CPStringOrNil(dict[@"citations"]) ?: @"") copy];
        _reason = [(CPStringOrNil(dict[@"reason"]) ?: @"") copy];
    }
    return self;
}

@end

#pragma mark - Network

@implementation CPNetworkBasicAuthRequest

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _proxy = [(CPStringOrNil(dict[@"proxy"]) ?: @"") copy];
        _realm = [CPStringOrNil(dict[@"realm"]) copy];
        _previousRejected = [CPNumberOrNil(dict[@"previousRejected"]) boolValue];
    }
    return self;
}

@end

@implementation CPNetworkBasicCredentials
@end

@implementation CPNetworkOptions
@end

#pragma mark - CPPolicyHelperFailure

@implementation CPPolicyHelperFailure

+ (nullable instancetype)failureFromDictionary:(nullable NSDictionary<NSString *, id> *)dict {
    if (![dict isKindOfClass:[NSDictionary class]]) return nil;
    id nested = dict[@"policyHelperFailure"];
    NSDictionary *failure = [nested isKindOfClass:[NSDictionary class]] ? nested : dict;
    NSString *category = CPStringOrNil(failure[@"category"]);
    NSString *message = CPStringOrNil(failure[@"message"]);
    if (!category || !message) return nil;

    CPPolicyHelperFailure *result = [[CPPolicyHelperFailure alloc] init];
    result->_category = [category copy];
    result->_message = [message copy];
    result->_failureMode = [(CPStringOrNil(failure[@"failureMode"]) ?: @"") copy];
    result->_reason = [(CPStringOrNil(failure[@"reason"]) ?: @"") copy];
    result->_previousPolicyRetained = [CPNumberOrNil(failure[@"previousPolicyRetained"]) boolValue];
    result->_severity = [CPStringOrNil(failure[@"severity"]) copy];
    result->_authority = [CPStringOrNil(failure[@"authority"]) copy];
    result->_code = [CPStringOrNil(failure[@"code"]) copy];
    result->_diagnostic = [CPStringOrNil(failure[@"diagnostic"]) copy];
    result->_remediation = [CPStringOrNil(failure[@"remediation"]) copy];
    id helpLink = failure[@"helpLink"];
    if ([helpLink isKindOfClass:[NSDictionary class]]) {
        result->_helpLinkUrl = [CPStringOrNil(((NSDictionary *)helpLink)[@"url"]) copy];
    }
    id details = failure[@"details"];
    if ([details isKindOfClass:[NSArray class]]) result->_details = [details copy];
    return result;
}

+ (nullable instancetype)failureFromError:(nullable NSError *)error {
    id attached = error.userInfo[CPPolicyHelperFailureErrorKey];
    if ([attached isKindOfClass:[CPPolicyHelperFailure class]]) return attached;
    id data = error.userInfo[CPJsonRpcErrorDataKey];
    return [data isKindOfClass:[NSDictionary class]] ? [self failureFromDictionary:data] : nil;
}

@end

#pragma mark - v3.1.0 Types (MCP secrets, plugin extension approvals)

@implementation CPMcpSecretsSaveResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _usedKeychain = [CPNumberOrNil(dict[@"usedKeychain"]) boolValue];
    }
    return self;
}

@end

@implementation CPPermissionsExtensionApproval

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _extensionName = [(CPStringOrNil(dict[@"extensionName"]) ?: @"") copy];
        _pluginName = [(CPStringOrNil(dict[@"pluginName"]) ?: @"") copy];
        NSMutableArray<NSString *> *capabilities = [NSMutableArray array];
        id rawCapabilities = dict[@"capabilities"];
        if ([rawCapabilities isKindOfClass:[NSArray class]]) {
            for (id capability in (NSArray *)rawCapabilities) {
                if ([capability isKindOfClass:[NSString class]]) [capabilities addObject:capability];
            }
        }
        _capabilities = [capabilities copy];
        _grantedAt = [(CPStringOrNil(dict[@"grantedAt"]) ?: @"") copy];
        _installationKey = [(CPStringOrNil(dict[@"installationKey"]) ?: @"") copy];
        _installationLabel = [CPStringOrNil(dict[@"installationLabel"]) copy];
    }
    return self;
}

@end

@implementation CPPermissionsExtensionApprovalsGrantResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _granted = [CPNumberOrNil(dict[@"granted"]) boolValue];
        id approval = dict[@"approval"];
        if ([approval isKindOfClass:[NSDictionary class]]) {
            _approval = [[CPPermissionsExtensionApproval alloc] initWithDictionary:approval];
        }
    }
    return self;
}

@end

@implementation CPPermissionsExtensionApprovalsListResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        NSMutableArray<CPPermissionsExtensionApproval *> *approvals = [NSMutableArray array];
        id rawApprovals = dict[@"approvals"];
        if ([rawApprovals isKindOfClass:[NSArray class]]) {
            for (id approval in (NSArray *)rawApprovals) {
                if (![approval isKindOfClass:[NSDictionary class]]) continue;
                [approvals addObject:[[CPPermissionsExtensionApproval alloc] initWithDictionary:approval]];
            }
        }
        _approvals = [approvals copy];
    }
    return self;
}

@end

@implementation CPPermissionsExtensionApprovalsRevokeResult

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _revoked = [CPNumberOrNil(dict[@"revoked"]) boolValue];
    }
    return self;
}

@end

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

#pragma mark - CPCloudSessionRepository

@implementation CPCloudSessionRepository

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"owner"] = self.owner ?: @"";
    dict[@"name"] = self.name ?: @"";
    if (self.branch) dict[@"branch"] = self.branch;
    return [dict copy];
}

@end

#pragma mark - CPCloudSessionOptions

@implementation CPCloudSessionOptions

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.repository) dict[@"repository"] = [self.repository toDictionary];
    return [dict copy];
}

@end

#pragma mark - CPCanvasAction

@implementation CPCanvasAction

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"name"] = self.name ?: @"";
    dict[@"description"] = self.actionDescription ?: @"";
    if (self.inputSchema) dict[@"inputSchema"] = self.inputSchema;
    return [dict copy];
}

@end

#pragma mark - CPCanvasDeclaration

@implementation CPCanvasDeclaration

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"id"] = self.canvasId ?: @"";
    dict[@"displayName"] = self.displayName ?: @"";
    dict[@"description"] = self.canvasDescription ?: @"";
    if (self.inputSchema) dict[@"inputSchema"] = self.inputSchema;
    if (self.actions.count > 0) {
        NSMutableArray *actions = [NSMutableArray arrayWithCapacity:self.actions.count];
        for (CPCanvasAction *action in self.actions) {
            [actions addObject:[action toDictionary]];
        }
        dict[@"actions"] = actions;
    }
    return [dict copy];
}

@end

#pragma mark - CPCanvasOpenResponse

@implementation CPCanvasOpenResponse

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.url) dict[@"url"] = self.url;
    if (self.title) dict[@"title"] = self.title;
    if (self.status) dict[@"status"] = self.status;
    return [dict copy];
}

@end

#pragma mark - CPCanvasHostCapabilities

@implementation CPCanvasHostCapabilities

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _canvases = [dict[@"canvases"] boolValue];
    }
    return self;
}

@end

#pragma mark - CPCanvasHostContext

@implementation CPCanvasHostContext

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        NSDictionary *capabilities = dict[@"capabilities"];
        _capabilities = capabilities ? [[CPCanvasHostCapabilities alloc] initWithDictionary:capabilities] : [[CPCanvasHostCapabilities alloc] init];
    }
    return self;
}

@end

#pragma mark - CPCanvasOpenContext

@implementation CPCanvasOpenContext

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = [(dict[@"sessionId"] ?: @"") copy];
        _extensionId = [(dict[@"extensionId"] ?: @"") copy];
        _canvasId = [(dict[@"canvasId"] ?: @"") copy];
        _instanceId = [(dict[@"instanceId"] ?: @"") copy];
        _input = dict[@"input"];
        NSDictionary *host = dict[@"host"];
        _host = host ? [[CPCanvasHostContext alloc] initWithDictionary:host] : nil;
    }
    return self;
}

@end

#pragma mark - CPCanvasActionContext

@implementation CPCanvasActionContext

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = [(dict[@"sessionId"] ?: @"") copy];
        _extensionId = [(dict[@"extensionId"] ?: @"") copy];
        _canvasId = [(dict[@"canvasId"] ?: @"") copy];
        _instanceId = [(dict[@"instanceId"] ?: @"") copy];
        _actionName = [(dict[@"actionName"] ?: @"") copy];
        _input = dict[@"input"];
        NSDictionary *host = dict[@"host"];
        _host = host ? [[CPCanvasHostContext alloc] initWithDictionary:host] : nil;
    }
    return self;
}

@end

#pragma mark - CPCanvasLifecycleContext

@implementation CPCanvasLifecycleContext

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _sessionId = [(dict[@"sessionId"] ?: @"") copy];
        _extensionId = [(dict[@"extensionId"] ?: @"") copy];
        _canvasId = [(dict[@"canvasId"] ?: @"") copy];
        _instanceId = [(dict[@"instanceId"] ?: @"") copy];
        NSDictionary *host = dict[@"host"];
        _host = host ? [[CPCanvasHostContext alloc] initWithDictionary:host] : nil;
    }
    return self;
}

@end

#pragma mark - CPSectionOverride

@implementation CPSectionOverride

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _action = [(dict[@"action"] ?: @"") copy];
        _content = [dict[@"content"] copy];
    }
    return self;
}

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"action"] = self.action ?: @"";
    if (self.content) dict[@"content"] = self.content;
    return [dict copy];
}

@end

#pragma mark - CPSystemMessageAppendConfig

@implementation CPSystemMessageAppendConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"mode"] = @"append";
    if (self.content) dict[@"content"] = self.content;
    return [dict copy];
}

@end

#pragma mark - CPSystemMessageReplaceConfig

@implementation CPSystemMessageReplaceConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"mode"] = @"replace";
    dict[@"content"] = self.content ?: @"";
    return [dict copy];
}

@end

#pragma mark - CPSystemMessageCustomizeConfig

@implementation CPSystemMessageCustomizeConfig

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"mode"] = @"customize";
    if (self.sections.count > 0) {
        NSMutableDictionary *sections = [NSMutableDictionary dictionaryWithCapacity:self.sections.count];
        [self.sections enumerateKeysAndObjectsUsingBlock:^(NSString *key, CPSectionOverride *obj, BOOL *stop) {
            sections[key] = [obj toDictionary];
        }];
        dict[@"sections"] = sections;
    }
    if (self.content) dict[@"content"] = self.content;
    return [dict copy];
}

@end

#pragma mark - CPUserInputImageAttachmentConstraints

@implementation CPUserInputImageAttachmentConstraints

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        NSMutableArray<NSString *> *mimeTypes = [NSMutableArray array];
        id allowed = dict[@"allowedMimeTypes"];
        if ([allowed isKindOfClass:[NSArray class]]) {
            for (id mimeType in (NSArray *)allowed) {
                if ([mimeType isKindOfClass:[NSString class]]) [mimeTypes addObject:mimeType];
            }
        }
        _allowedMimeTypes = [mimeTypes copy];
        _maxBytesPerImage = [CPNumberOrNil(dict[@"maxBytesPerImage"]) integerValue];
        _maxCount = [CPNumberOrNil(dict[@"maxCount"]) integerValue];
    }
    return self;
}

@end

#pragma mark - CPUserInputImageAttachment

@implementation CPUserInputImageAttachment

+ (instancetype)attachmentWithData:(NSString *)data mimeType:(NSString *)mimeType {
    CPUserInputImageAttachment *attachment = [[CPUserInputImageAttachment alloc] init];
    attachment.data = data;
    attachment.mimeType = mimeType;
    return attachment;
}

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"type"] = @"image";
    dict[@"data"] = self.data ?: @"";
    dict[@"mimeType"] = self.mimeType ?: @"";
    if (self.imageDescription) dict[@"description"] = self.imageDescription;
    if (self.displayName) dict[@"displayName"] = self.displayName;
    return [dict copy];
}

@end

#pragma mark - CPUserInputRequest

@implementation CPUserInputRequest

- (instancetype)initWithDictionary:(NSDictionary<NSString *, id> *)dict {
    self = [super init];
    if (self) {
        _question = [dict[@"question"] copy];
        _choices = [dict[@"choices"] copy];
        _allowFreeform = dict[@"allowFreeform"];
        _requestId = [CPStringOrNil(dict[@"requestId"]) copy];
        id constraints = dict[@"imageAttachmentConstraints"];
        if ([constraints isKindOfClass:[NSDictionary class]]) {
            _imageAttachmentConstraints = [[CPUserInputImageAttachmentConstraints alloc] initWithDictionary:constraints];
        }
        id meta = dict[@"_meta"];
        if ([meta isKindOfClass:[NSDictionary class]]) _meta = [meta copy];
    }
    return self;
}

@end

#pragma mark - CPUserInputResponse

@implementation CPUserInputResponse

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"answer"] = self.answer ?: @"";
    dict[@"wasFreeform"] = @(self.wasFreeform);
    if (self.attachments) {
        NSMutableArray *attachments = [NSMutableArray arrayWithCapacity:self.attachments.count];
        for (CPUserInputImageAttachment *attachment in self.attachments) {
            [attachments addObject:[attachment toDictionary]];
        }
        dict[@"attachments"] = attachments;
    }
    return [dict copy];
}

@end

#pragma mark - CPImageOptions

@implementation CPImageOptions

- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    if (self.size) dict[@"size"] = self.size;
    if (self.quality) dict[@"quality"] = self.quality;
    if (self.style) dict[@"style"] = self.style;
    return [dict copy];
}

@end

#pragma mark - CPSessionEvent

@implementation CPSessionEvent

- (instancetype)initWithType:(NSString *)type data:(nullable NSDictionary<NSString *, id> *)data {
    self = [super init];
    if (self) {
        _type = [type copy];
        [self applyData:data];
    }
    return self;
}

- (instancetype)initWithEnvelope:(NSDictionary<NSString *, id> *)envelope {
    self = [super init];
    if (self) {
        _type = [(CPStringOrNil(envelope[@"type"]) ?: @"unknown") copy];
        _eventId = [CPStringOrNil(envelope[@"id"]) copy];
        _timestamp = [CPStringOrNil(envelope[@"timestamp"]) copy];
        _parentId = [CPStringOrNil(envelope[@"parentId"]) copy];
        _agentId = [CPStringOrNil(envelope[@"agentId"]) copy];
        _ephemeral = [CPNumberOrNil(envelope[@"ephemeral"]) boolValue];
        id data = envelope[@"data"];
        [self applyData:[data isKindOfClass:[NSDictionary class]] ? data : nil];
    }
    return self;
}

- (void)applyData:(nullable NSDictionary<NSString *, id> *)data {
    _rawData = [data copy];
    if (!data) return;
    _content = [CPStringOrNil(data[@"content"]) copy];
    _deltaContent = [(CPStringOrNil(data[@"deltaContent"]) ?: CPStringOrNil(data[@"delta"])) copy];
    _message = [CPStringOrNil(data[@"message"]) copy];
    _toolName = [CPStringOrNil(data[@"toolName"]) copy];
    _toolCallId = [CPStringOrNil(data[@"toolCallId"]) copy];
    if ([_type isEqualToString:CPEventTypeToolShellOutput]) {
        _shellOutput = [[CPToolShellOutputData alloc] initWithDictionary:data];
    }
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
    if (self.requestHeaders) dict[@"requestHeaders"] = self.requestHeaders;
    if (self.agentMode) dict[@"agentMode"] = self.agentMode;
    if (self.displayPrompt) dict[@"displayPrompt"] = self.displayPrompt;
    if (self.source) dict[@"source"] = self.source;
    if (self.clientTurnId) dict[@"clientTurnId"] = self.clientTurnId;
    if (self.responseSchema) {
        dict[@"responseFormat"] = @{
            @"type": @"json_schema",
            @"jsonSchema": @{@"name": @"response", @"strict": @YES, @"schema": self.responseSchema},
        };
    }
    return [dict copy];
}

@end

#pragma mark - CPSessionConfig

/// Tool definitions as sent on the wire: SDK-private keys (such as the `_handler` block
/// added by `CPDefineTool`) are dropped.
static NSArray *CPWireToolDefinitions(NSArray *tools) {
    NSMutableArray *definitions = [NSMutableArray arrayWithCapacity:tools.count];
    for (id tool in tools) {
        if (![tool isKindOfClass:[NSDictionary class]]) continue;
        NSMutableDictionary *definition = [NSMutableDictionary dictionary];
        [(NSDictionary *)tool enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
            if ([key isKindOfClass:[NSString class]] && ![(NSString *)key hasPrefix:@"_"]) {
                definition[key] = obj;
            }
        }];
        [definitions addObject:definition];
    }
    return definitions;
}

@implementation CPSessionConfig

/// The flat (SDK protocol 3 style) session options. The client converts them to the
/// grouped SDK protocol 4 payload with `CPGroupSessionParams` before sending.
- (NSDictionary<NSString *, id> *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];

    if (self.sessionId) dict[@"sessionId"] = self.sessionId;
    if (self.name) dict[@"name"] = self.name;
    if (self.model) dict[@"model"] = self.model;
    if (self.reasoningEffort) dict[@"reasoningEffort"] = self.reasoningEffort;
    if (self.workingDirectory) dict[@"workingDirectory"] = self.workingDirectory;
    dict[@"streaming"] = @(self.streaming);
    if (self.tools) dict[@"tools"] = CPWireToolDefinitions(self.tools);
    if (self.availableTools) dict[@"availableTools"] = self.availableTools;
    if (self.excludedTools) dict[@"excludedTools"] = self.excludedTools;
    if (self.skillDirectories) dict[@"skillDirectories"] = self.skillDirectories;
    if (self.disabledSkills) dict[@"disabledSkills"] = self.disabledSkills;
    if (self.includeSubAgentStreamingEvents) dict[@"includeSubAgentStreamingEvents"] = @YES;
    if (self.enableConfigDiscovery) dict[@"enableConfigDiscovery"] = @YES;
    if (self.mcpServers) dict[@"mcpServers"] = self.mcpServers;
    if (self.commands) dict[@"commands"] = self.commands;
    if (self.systemMessage) dict[@"systemMessage"] = self.systemMessage;
    if (self.githubToken) dict[@"gitHubToken"] = self.githubToken;
    if (self.instructionDirectories) dict[@"instructionDirectories"] = self.instructionDirectories;
    if (self.permissionHandler) dict[@"requestPermission"] = @YES;
    if (self.exitPlanModeHandler) dict[@"requestExitPlanMode"] = @YES;
    if (self.enableSessionTelemetry) dict[@"enableSessionTelemetry"] = @YES;

    // Model capabilities, with maxOutputTokens merged into limits.max_output_tokens
    NSMutableDictionary *capabilities = [self.modelCapabilities mutableCopy];
    if (self.maxOutputTokens) {
        if (!capabilities) capabilities = [NSMutableDictionary dictionary];
        id existingLimits = capabilities[@"limits"];
        NSMutableDictionary *limits = [existingLimits isKindOfClass:[NSDictionary class]]
            ? [existingLimits mutableCopy] : [NSMutableDictionary dictionary];
        limits[@"max_output_tokens"] = self.maxOutputTokens;
        capabilities[@"limits"] = limits;
    }
    if (capabilities) dict[@"modelCapabilities"] = capabilities;

    if (self.hooks && [self.hooks hasAnyHandler]) {
        dict[@"hooks"] = @YES;
        dict[@"hookTypes"] = [self.hooks hookTypes];
    }
    if (self.enableCitations) dict[@"enableCitations"] = self.enableCitations;
    if (self.excludedBuiltinAgents) dict[@"excludedBuiltinAgents"] = self.excludedBuiltinAgents;
    if (self.sessionLimits) dict[@"sessionLimits"] = [self.sessionLimits toDictionary];
    if (self.memory) dict[@"memory"] = [self.memory toDictionary];
    if (self.otlpProtocol) dict[@"otlpProtocol"] = self.otlpProtocol;
    if (self.expAssignments) dict[@"expAssignments"] = self.expAssignments;
    if (self.onMcpAuthRequest) dict[@"mcpAuthHandler"] = @YES;
    // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
    if (self.rewindEnabled) dict[@"rewindEnabled"] = self.rewindEnabled;
    if (self.additionalDirectories) dict[@"additionalDirectories"] = self.additionalDirectories;
    if (self.disabledMcpServers) dict[@"disabledMcpServers"] = self.disabledMcpServers;
    if (self.githubMcpToolConfig) dict[@"githubMcpToolConfig"] = self.githubMcpToolConfig;
    if (self.canvasProvider) dict[@"canvasProvider"] = self.canvasProvider;
    if (self.customAgentsLocalOnly) dict[@"customAgentsLocalOnly"] = self.customAgentsLocalOnly;
    if (self.toolSearch) dict[@"toolSearch"] = self.toolSearch;
    if (self.experimentalMode) dict[@"enableExperimentalMode"] = self.experimentalMode;
    if (self.contentExclusion) dict[@"contentExclusion"] = self.contentExclusion;
    if (self.argsSchema) dict[@"argsSchema"] = self.argsSchema;

    // --- v2.6.0 session options ---
    if (self.askUserVariant) dict[@"askUserVariant"] = self.askUserVariant;
    if (self.sandboxConfig) dict[@"sandboxConfig"] = [self.sandboxConfig toDictionary];
    if (self.featureFlags) dict[@"featureFlags"] = self.featureFlags;
    if (self.allowedModels) dict[@"allowedModels"] = self.allowedModels;
    if (self.authClientIdMetadataUrl) dict[@"authClientIdMetadataUrl"] = self.authClientIdMetadataUrl;
    if (self.agentDirectories) dict[@"agentDirectories"] = self.agentDirectories;
    if (self.hookDirectories) dict[@"hookDirectories"] = self.hookDirectories;
    if (self.interactiveUser) dict[@"interactiveUser"] = self.interactiveUser;
    if (self.excludedManagedSettings) dict[@"excludedManagedSettings"] = self.excludedManagedSettings;
    if (self.enforceManagedModelDefaults) dict[@"enforceManagedModelDefaults"] = self.enforceManagedModelDefaults;
    if (self.trustWorkingDirectory) dict[@"trustWorkingDirectory"] = self.trustWorkingDirectory;
    if (self.refreshCustomInstructions) dict[@"refreshCustomInstructions"] = self.refreshCustomInstructions;
    if (self.allowTranscriptRecovery) dict[@"allowTranscriptRecovery"] = self.allowTranscriptRecovery;
    if (self.imageGeneration) dict[@"imageGeneration"] = [self.imageGeneration toDictionary];
    if (self.autopilot) dict[@"autopilot"] = [self.autopilot toDictionary];

    // --- v3.0.0 session options ---
    if (self.suppressResumeEvent) dict[@"suppressResumeEvent"] = self.suppressResumeEvent;
    if (self.providers) {
        NSMutableArray *providers = [NSMutableArray arrayWithCapacity:self.providers.count];
        for (CPNamedProviderConfig *provider in self.providers) [providers addObject:[provider toDictionary]];
        dict[@"providers"] = providers;
    }
    if (self.models) {
        NSMutableArray *models = [NSMutableArray arrayWithCapacity:self.models.count];
        for (CPProviderModelConfig *model in self.models) [models addObject:[model toDictionary]];
        dict[@"models"] = models;
    }
    if (self.defaultProvider) {
        dict[@"defaultProvider"] = self.defaultProvider;
    } else if (self.clearDefaultProvider) {
        // An explicit null clears the default provider (session.resume only).
        dict[@"defaultProvider"] = [NSNull null];
    }
    // Copilot API options: capi.enabled, the Auto routing tier, and the WebSocket switch
    NSMutableDictionary *capi = [NSMutableDictionary dictionary];
    if (self.capi.enabled) capi[@"enabled"] = self.capi.enabled;
    if (self.autoTier) capi[@"autoTier"] = self.autoTier;
    if (self.enableWebSocketResponses) capi[@"enableWebSocketResponses"] = self.enableWebSocketResponses;
    if (capi.count > 0) dict[@"capi"] = capi;
    if (self.provider) dict[@"provider"] = self.provider;
    if (self.permissionMode) dict[@"permissionMode"] = self.permissionMode;
    if (self.onShellAuthentication) dict[@"requestShellAuthentication"] = @YES;
    if (self.onMemoryStore) dict[@"hasMemoryStore"] = @YES;

    // --- v3.1.0 session options ---
    if (self.userInputHandler) dict[@"requestUserInput"] = @YES;

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
