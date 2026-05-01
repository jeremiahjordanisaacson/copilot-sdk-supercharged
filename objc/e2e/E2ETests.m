/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <XCTest/XCTest.h>
#import "CPCapiProxy.h"
#import "CPCopilotClient.h"
#import "CPCopilotSession.h"
#import "CPTypes.h"

/// Resolve the repository root from this source file (objc/e2e/ -> ../..).
static NSString *RepoRoot(void) {
    NSString *e2eDir = [[NSString stringWithUTF8String:__FILE__] stringByDeletingLastPathComponent];
    return [[e2eDir stringByAppendingPathComponent:@"../.."] stringByStandardizingPath];
}

#pragma mark - E2ETests

@interface E2ETests : XCTestCase
@property (nonatomic, strong) CPCapiProxy *proxy;
@property (nonatomic, copy) NSString *repoRoot;
@end

@implementation E2ETests

- (void)setUp {
    [super setUp];
    self.repoRoot = RepoRoot();
    self.proxy = [[CPCapiProxy alloc] init];
    [self.proxy start];
}

- (void)tearDown {
    [self.proxy stop];
    [super tearDown];
}

#pragma mark - Helpers

/// Configures the proxy with a snapshot path relative to the repo root.
- (void)loadSnapshot:(NSString *)relativePath {
    [self.proxy configureWithFilePath:relativePath workDir:self.repoRoot];
}

/// Creates a `CPCopilotClient` pointed at the running proxy.
- (CPCopilotClient *)makeClient {
    CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
    opts.cliUrl = self.proxy.proxyUrl;
    return [[CPCopilotClient alloc] initWithOptions:opts];
}

/// Creates a client with a custom `CPSessionFsConfig`.
- (CPCopilotClient *)makeClientWithSessionFs:(CPSessionFsConfig *)fsConfig {
    CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
    opts.cliUrl = self.proxy.proxyUrl;
    opts.sessionFs = fsConfig;
    return [[CPCopilotClient alloc] initWithOptions:opts];
}

#pragma mark - Session create / disconnect

- (void)testSessionCreateAndDisconnect {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"session lifecycle"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr, @"Client start failed: %@", startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr, @"Create session failed: %@", sessErr);
            XCTAssertNotNil(session, @"Session must not be nil");
            XCTAssertTrue(session.sessionId.length > 0, @"Session ID must be non-empty");

            [session disconnectWithCompletion:^(NSError *discErr) {
                XCTAssertNil(discErr, @"Disconnect failed: %@", discErr);

                [client stopWithCompletion:^(NSError *stopErr) {
                    XCTAssertNil(stopErr, @"Client stop failed: %@", stopErr);
                    [exp fulfill];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Send message

- (void)testSendMessage {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"send message"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr, @"Client start failed: %@", startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr, @"Create session failed: %@", sessErr);
            XCTAssertNotNil(session);

            CPMessageOptions *msgOpts = [[CPMessageOptions alloc] init];
            msgOpts.prompt = @"What is 1+1?";

            [session sendAndWait:msgOpts timeout:30000 completion:^(NSString *content, NSError *sendErr) {
                XCTAssertNil(sendErr, @"Send failed: %@", sendErr);
                XCTAssertNotNil(content, @"Response content must not be nil");
                XCTAssertTrue(content.length > 0, @"Response must not be empty");

                [client stopWithCompletion:^(NSError *stopErr) {
                    [exp fulfill];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Stateful conversation

- (void)testStatefulConversation {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"stateful conversation"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);

            CPMessageOptions *msg1 = [[CPMessageOptions alloc] init];
            msg1.prompt = @"What is 1+1?";

            [session sendAndWait:msg1 timeout:30000 completion:^(NSString *first, NSError *err1) {
                XCTAssertNil(err1);
                XCTAssertNotNil(first);
                XCTAssertTrue([first containsString:@"2"],
                              @"Expected '2' in response, got: %@", first);

                CPMessageOptions *msg2 = [[CPMessageOptions alloc] init];
                msg2.prompt = @"Now if you double that, what do you get?";

                [session sendAndWait:msg2 timeout:30000 completion:^(NSString *second, NSError *err2) {
                    XCTAssertNil(err2);
                    XCTAssertNotNil(second);
                    XCTAssertTrue([second containsString:@"4"],
                                  @"Expected '4' in response, got: %@", second);

                    [client stopWithCompletion:^(NSError *stopErr) {
                        [exp fulfill];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Session Fs config

- (void)testSessionFsConfig {
    [self loadSnapshot:@"test/snapshots/session_fs/should_route_file_operations_through_the_session_fs_provider.yaml"];

    CPSessionFsConfig *fsConfig = [[CPSessionFsConfig alloc] init];
    fsConfig.initialCwd = @"/";
    fsConfig.sessionStatePath = @"/session-state";
    fsConfig.conventions = @"posix";

    CPCopilotClient *client = [self makeClientWithSessionFs:fsConfig];

    XCTestExpectation *exp = [self expectationWithDescription:@"session fs config"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr, @"Client start with sessionFs failed: %@", startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr, @"Create session failed: %@", sessErr);
            XCTAssertNotNil(session);
            XCTAssertTrue(session.sessionId.length > 0);

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Session with model config

- (void)testSessionWithModelConfig {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"model config"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        config.model = @"claude-sonnet-4.5";

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);
            XCTAssertTrue(session.sessionId.length > 0);

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Abort session

- (void)testAbortSession {
    [self loadSnapshot:@"test/snapshots/session/should_abort_a_session.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"abort session"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);

            // Fire-and-forget a message so there's something to abort
            CPMessageOptions *opts = [[CPMessageOptions alloc] init];
            opts.prompt = @"Write a very long essay about the history of computing.";

            [session send:opts completion:^(NSString *messageId, NSError *sendErr) {
                // Now abort
                [session abortWithCompletion:^(NSError *abortErr) {
                    XCTAssertNil(abortErr, @"Abort failed: %@", abortErr);

                    [client stopWithCompletion:^(NSError *stopErr) {
                        [exp fulfill];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

@end
