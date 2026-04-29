/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <XCTest/XCTest.h>
#import "../src/CPCopilotClient.h"
#import "../src/CPCopilotSession.h"
#import "../src/CPDefineTool.h"
#import "../src/CPJsonRpcClient.h"
#import "../src/CPSdkProtocolVersion.h"
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
    XCTAssertEqual(CPSdkProtocolVersion, 3);
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
