/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <XCTest/XCTest.h>
#import "../src/CPCopilotClient.h"
#import "../src/CPCopilotSession.h"
#import "../src/CPDefineTool.h"
#import "../src/CPJsonRpcClient.h"
#import "../src/CPSdkProtocolVersion.h"
#import "../src/CPSessionParams.h"
#import "../src/CPTypes.h"

#pragma mark - Type Tests

@interface CPTypeTests : XCTestCase
@end

@implementation CPTypeTests

- (void)testDefaultClientOptions {
    CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
    XCTAssertTrue(opts.autoStart);
    XCTAssertTrue(opts.autoRestart);
    XCTAssertTrue(opts.useLoggedInUser);
    XCTAssertEqualObjects(opts.logLevel, @"info");
    XCTAssertNil(opts.cliPath);
    XCTAssertNil(opts.cliUrl);
}

- (void)testProtocolVersion {
    XCTAssertEqual(CPSdkProtocolVersion, 4);
}

- (void)testToolResultSuccess {
    CPToolResult *result = [CPToolResult successWithText:@"Hello"];
    XCTAssertEqualObjects(result.textResultForLlm, @"Hello");
    XCTAssertEqual(result.resultType, CPToolResultTypeSuccess);
    XCTAssertNil(result.error);

    NSDictionary *dict = [result toDictionary];
    XCTAssertEqualObjects(dict[@"textResultForLlm"], @"Hello");
    XCTAssertEqualObjects(dict[@"resultType"], @"success");
}

- (void)testToolResultFailure {
    CPToolResult *result = [CPToolResult failureWithError:@"Something went wrong"];
    XCTAssertEqual(result.resultType, CPToolResultTypeFailure);
    XCTAssertEqualObjects(result.error, @"Something went wrong");

    NSDictionary *dict = [result toDictionary];
    XCTAssertEqualObjects(dict[@"resultType"], @"failure");
    XCTAssertEqualObjects(dict[@"error"], @"Something went wrong");
}

- (void)testToolInvocation {
    NSDictionary *args = @{@"city": @"Seattle"};
    CPToolInvocation *inv = [[CPToolInvocation alloc] initWithSessionId:@"s1"
                                                            toolCallId:@"tc1"
                                                              toolName:@"weather"
                                                             arguments:args];
    XCTAssertEqualObjects(inv.sessionId, @"s1");
    XCTAssertEqualObjects(inv.toolCallId, @"tc1");
    XCTAssertEqualObjects(inv.toolName, @"weather");
    XCTAssertEqualObjects(inv.arguments[@"city"], @"Seattle");
}

- (void)testSessionEvent {
    NSDictionary *data = @{@"content": @"Hello world", @"toolName": @"test_tool"};
    CPSessionEvent *event = [[CPSessionEvent alloc] initWithType:@"assistant.message" data:data];
    XCTAssertEqualObjects(event.type, @"assistant.message");
    XCTAssertEqualObjects(event.content, @"Hello world");
    XCTAssertEqualObjects(event.toolName, @"test_tool");
    XCTAssertNil(event.deltaContent);
}

- (void)testSessionMetadata {
    NSDictionary *dict = @{
        @"sessionId": @"abc123",
        @"startTime": @"2024-01-01T00:00:00Z",
        @"summary": @"Test session",
        @"isRemote": @NO,
    };
    CPSessionMetadata *meta = [[CPSessionMetadata alloc] initWithDictionary:dict];
    XCTAssertEqualObjects(meta.sessionId, @"abc123");
    XCTAssertEqualObjects(meta.startTime, @"2024-01-01T00:00:00Z");
    XCTAssertEqualObjects(meta.summary, @"Test session");
    XCTAssertFalse(meta.isRemote);
}

- (void)testAttachment {
    CPAttachment *att = [[CPAttachment alloc] init];
    att.type = CPAttachmentTypeFile;
    att.path = @"/path/to/file.txt";
    att.displayName = @"file.txt";

    NSDictionary *dict = [att toDictionary];
    XCTAssertEqualObjects(dict[@"type"], @"file");
    XCTAssertEqualObjects(dict[@"path"], @"/path/to/file.txt");
    XCTAssertEqualObjects(dict[@"displayName"], @"file.txt");
}

- (void)testMessageOptions {
    CPMessageOptions *opts = [[CPMessageOptions alloc] init];
    opts.prompt = @"Hello Copilot";

    NSDictionary *dict = [opts toDictionary];
    XCTAssertEqualObjects(dict[@"prompt"], @"Hello Copilot");
    XCTAssertNil(dict[@"attachments"]);
}

- (void)testSessionConfig {
    CPSessionConfig *config = [[CPSessionConfig alloc] init];
    config.model = @"gpt-4o";
    config.streaming = YES;
    config.workingDirectory = @"/Users/test";

    NSDictionary *dict = [config toDictionary];
    XCTAssertEqualObjects(dict[@"model"], @"gpt-4o");
    XCTAssertEqualObjects(dict[@"streaming"], @YES);
    XCTAssertEqualObjects(dict[@"workingDirectory"], @"/Users/test");
}

- (void)testSessionFsConfig {
    CPSessionFsConfig *fs = [[CPSessionFsConfig alloc] init];
    fs.initialCwd = @"/home/user";
    fs.conventions = @"posix";

    NSDictionary *dict = [fs toDictionary];
    XCTAssertEqualObjects(dict[@"initialCwd"], @"/home/user");
    XCTAssertEqualObjects(dict[@"conventions"], @"posix");
}

- (void)testMessageOptionsV3Fields {
    CPMessageOptions *opts = [[CPMessageOptions alloc] init];
    opts.prompt = @"Summarize";
    opts.source = CPMessageSourceAgent(@"reviewer");
    opts.clientTurnId = @"turn-1";
    opts.responseSchema = @{@"type": @"object"};

    NSDictionary *dict = [opts toDictionary];
    XCTAssertEqualObjects(dict[@"source"], @"agent-reviewer");
    XCTAssertEqualObjects(dict[@"clientTurnId"], @"turn-1");
    NSDictionary *expectedFormat = @{
        @"type": @"json_schema",
        @"jsonSchema": @{@"name": @"response", @"strict": @YES, @"schema": @{@"type": @"object"}},
    };
    XCTAssertEqualObjects(dict[@"responseFormat"], expectedFormat);
}

- (void)testShellOutputEventEnvelope {
    CPSessionEvent *event = [[CPSessionEvent alloc] initWithEnvelope:@{
        @"id": @"e1",
        @"type": CPEventTypeToolShellOutput,
        @"ephemeral": @YES,
        @"data": @{@"toolCallId": @"c1", @"text": @"hi\n", @"sequence": @3, @"stream": @"stderr"},
    }];
    XCTAssertEqualObjects(event.eventId, @"e1");
    XCTAssertTrue(event.ephemeral);
    XCTAssertEqualObjects(event.shellOutput.toolCallId, @"c1");
    XCTAssertEqualObjects(event.shellOutput.text, @"hi\n");
    XCTAssertEqual(event.shellOutput.sequence, 3);
    XCTAssertEqualObjects(event.shellOutput.stream, @"stderr");
}

- (void)testPolicyHelperFailureFromErrorData {
    NSDictionary *data = @{@"policyHelperFailure": @{
        @"category": @"helper_failed",
        @"failureMode": @"fail_closed",
        @"message": @"Policy helper exited",
        @"previousPolicyRetained": @YES,
        @"reason": @"exit_code",
        @"helpLink": @{@"url": @"https://example.com/help"},
    }};
    NSError *error = [NSError errorWithDomain:CPJsonRpcErrorDomain
                                         code:-32603
                                     userInfo:@{CPJsonRpcErrorDataKey: data}];
    CPPolicyHelperFailure *failure = [CPPolicyHelperFailure failureFromError:error];
    XCTAssertEqualObjects(failure.category, @"helper_failed");
    XCTAssertEqualObjects(failure.failureMode, @"fail_closed");
    XCTAssertTrue(failure.previousPolicyRetained);
    XCTAssertEqualObjects(failure.helpLinkUrl, @"https://example.com/help");
    XCTAssertNil([CPPolicyHelperFailure failureFromDictionary:@{@"category": @"x"}]);
}

@end

#pragma mark - Session Params (SDK protocol 4) Tests

@interface CPSessionParamsTests : XCTestCase
@end

@implementation CPSessionParamsTests

- (void)testGroupsFlatOptionsOnResume {
    NSDictionary *flat = @{
        @"model": @"m",
        @"streaming": @YES,
        @"requestPermission": @YES,
        @"tools": @[@{@"name": @"t"}],
        @"hooks": @YES,
        @"disableResume": @YES,
        @"trustWorkingDirectory": @"session",
        @"otlpProtocol": @"grpc",
    };
    NSDictionary *expected = @{
        @"model": @"m",
        @"events": @{@"streaming": @YES},
        @"permissions": @{@"request": @YES},
        @"tools": @{@"definitions": @[@{@"name": @"t"}]},
        @"hooks": @{@"hasHandlers": @YES},
        @"suppressResumeEvent": @YES,
        @"otlpProtocol": @"grpc",
    };
    XCTAssertEqualObjects(CPGroupSessionParams(flat, YES), expected);
}

- (void)testLegacyProviderBecomesDefaultNamedProvider {
    NSDictionary *flat = @{
        @"model": @"gpt",
        @"capi": @{@"autoTier": @"fast"},
        @"provider": @{@"type": @"openai", @"baseUrl": @"http://x", @"apiKeyCommand": @"get-key", @"maxOutputTokens": @100},
    };
    NSDictionary *expected = @{
        @"model": @"gpt",
        @"github": @{@"capi": @{@"autoTier": @"fast", @"enabled": @NO}},
        @"inference": @{
            @"providers": @[@{@"name": @"default", @"type": @"openai", @"baseUrl": @"http://x", @"apiKeyCommand": @"get-key"}],
            @"defaultProvider": @"default",
            @"models": @[@{@"provider": @"default", @"id": @"gpt", @"maxOutputTokens": @100}],
        },
    };
    XCTAssertEqualObjects(CPGroupSessionParams(flat, NO), expected);
}

- (void)testSessionConfigBuildsGroupedPayload {
    CPSessionConfig *config = [[CPSessionConfig alloc] init];
    config.streaming = YES;
    config.maxOutputTokens = @1000;
    config.autoTier = CPAutoTierFast;
    config.trustWorkingDirectory = CPTrustWorkingDirectorySession;
    config.clearDefaultProvider = YES;
    config.onMemoryStore = ^(CPStoredMemory *memory, void (^completion)(NSError *error)) {
        completion(nil);
    };
    config.tools = @[CPIdempotentTool(CPDefineSimpleTool(@"lookup", nil,
        ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
            done([CPToolResult successWithText:@"ok"]);
        }))];

    NSDictionary *created = CPGroupSessionParams([config toDictionary], NO);
    XCTAssertNil(created[@"streaming"]);
    XCTAssertEqualObjects(created[@"events"][@"streaming"], @YES);
    XCTAssertEqualObjects(created[@"inference"][@"capabilities"][@"limits"][@"max_output_tokens"], @1000);
    XCTAssertEqualObjects(created[@"github"][@"capi"][@"autoTier"], @"fast");
    XCTAssertEqualObjects(created[@"filesystem"][@"trustWorkingDirectory"], @"session");
    XCTAssertEqualObjects(created[@"state"][@"hasMemoryStore"], @YES);
    XCTAssertNil(created[@"inference"][@"defaultProvider"]);
    NSDictionary *tool = created[@"tools"][@"definitions"][0];
    XCTAssertEqualObjects(tool[@"idempotent"], @YES);
    XCTAssertNil(tool[@"_handler"]);

    NSDictionary *resumed = CPGroupSessionParams([config toDictionary], YES);
    XCTAssertNil(resumed[@"filesystem"]);
    XCTAssertEqualObjects(resumed[@"inference"][@"defaultProvider"], [NSNull null]);
}

- (void)testEmptyModeDefaultsKeepCallerValues {
    NSDictionary *grouped = CPGroupSessionParams(@{@"availableTools": @[], @"enableExperimentalMode": @YES}, NO);
    NSDictionary *params = CPApplyEmptyModeDefaults(grouped);
    XCTAssertEqualObjects(params[@"tools"][@"availableTools"], @[]);
    XCTAssertEqualObjects(params[@"experiments"][@"enableExperimentalMode"], @YES);
    XCTAssertEqualObjects(params[@"state"][@"memory"], @{@"enabled": @NO});
    XCTAssertEqualObjects(params[@"mcp"][@"oauthTokenStorage"], @"in-memory");
    XCTAssertEqualObjects(params[@"git"][@"enableHostGitOperations"], @NO);
}

@end

#pragma mark - Define Tool Tests

@interface CPDefineToolTests : XCTestCase
@end

@implementation CPDefineToolTests

- (void)testDefineToolWithParameters {
    NSDictionary *params = @{
        @"type": @"object",
        @"properties": @{
            @"query": @{@"type": @"string"}
        },
    };

    NSDictionary *tool = CPDefineTool(@"search", @"Search for items", params,
        ^(CPToolInvocation *inv, void (^done)(CPToolResult *)) {
            done([CPToolResult successWithText:@"found"]);
        });

    XCTAssertEqualObjects(tool[@"name"], @"search");
    XCTAssertEqualObjects(tool[@"description"], @"Search for items");
    XCTAssertNotNil(tool[@"parameters"]);
    XCTAssertNotNil(tool[@"_handler"]);
}

- (void)testDefineSimpleTool {
    NSDictionary *tool = CPDefineSimpleTool(@"ping", @"Ping the system",
        ^(CPToolInvocation *inv, void (^done)(CPToolResult *)) {
            done([CPToolResult successWithText:@"pong"]);
        });

    XCTAssertEqualObjects(tool[@"name"], @"ping");
    XCTAssertNil(tool[@"parameters"]);
}

@end

#pragma mark - Client Tests

@interface CPClientTests : XCTestCase
@end

@implementation CPClientTests

- (void)testClientInitialization {
    CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
    CPCopilotClient *client = [[CPCopilotClient alloc] initWithOptions:opts];
    XCTAssertNotNil(client);
    XCTAssertEqual(client.connectionState, CPConnectionStateDisconnected);
}

- (void)testClientInitWithNilOptions {
    CPCopilotClient *client = [[CPCopilotClient alloc] initWithOptions:nil];
    XCTAssertNotNil(client);
    XCTAssertEqual(client.connectionState, CPConnectionStateDisconnected);
}

@end

#pragma mark - JSON-RPC Client Tests

@interface CPJsonRpcClientTests : XCTestCase
@end

@implementation CPJsonRpcClientTests

- (void)testJsonRpcClientInit {
    CPJsonRpcClient *rpc = [[CPJsonRpcClient alloc] init];
    XCTAssertNotNil(rpc);
}

- (void)testRegisterRequestHandler {
    CPJsonRpcClient *rpc = [[CPJsonRpcClient alloc] init];
    [rpc registerRequestHandler:@"test/method"
                        handler:^(NSDictionary *params, void (^respond)(NSDictionary *, NSDictionary *)) {
        respond(@{@"ok": @YES}, nil);
    }];
    // Should not crash - handler registered successfully
    XCTAssertNotNil(rpc);
}

@end
