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

#pragma mark - Session resume

- (void)testSessionResume {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"session resume"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);

            NSString *savedId = session.sessionId;

            [client stopWithCompletion:^(NSError *stopErr) {
                // Create a new client and resume the session
                CPCopilotClient *client2 = [self makeClient];

                [client2 startWithCompletion:^(NSError *start2Err) {
                    XCTAssertNil(start2Err);

                    CPSessionConfig *config = [[CPSessionConfig alloc] init];
                    config.sessionId = savedId;

                    [client2 createSession:config completion:^(CPCopilotSession *resumed, NSError *resErr) {
                        XCTAssertNil(resErr);
                        XCTAssertNotNil(resumed);
                        XCTAssertTrue(resumed.sessionId.length > 0,
                                      @"Resumed session ID must be non-empty");

                        [client2 stopWithCompletion:^(NSError *stop2Err) {
                            [exp fulfill];
                        }];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Session list

- (void)testSessionList {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"session list"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *s1, NSError *err1) {
            XCTAssertNil(err1);

            [client createSession:nil completion:^(CPCopilotSession *s2, NSError *err2) {
                XCTAssertNil(err2);

                [client listSessionsWithCompletion:^(NSArray *sessions, NSError *listErr) {
                    XCTAssertNil(listErr);
                    XCTAssertNotNil(sessions);
                    XCTAssertTrue(sessions.count >= 2,
                                  @"Expected at least 2 sessions, got %lu",
                                  (unsigned long)sessions.count);

                    [client stopWithCompletion:^(NSError *stopErr) {
                        [exp fulfill];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Session metadata

- (void)testSessionMetadata {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"session metadata"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);

            [client getSessionMetadata:session.sessionId completion:^(NSDictionary *meta, NSError *metaErr) {
                XCTAssertNil(metaErr);
                XCTAssertNotNil(meta, @"Session metadata must not be nil");

                [client stopWithCompletion:^(NSError *stopErr) {
                    [exp fulfill];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Session delete

- (void)testSessionDelete {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"session delete"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            NSString *sessionId = session.sessionId;

            [client deleteSession:sessionId completion:^(NSError *delErr) {
                XCTAssertNil(delErr);

                [client listSessionsWithCompletion:^(NSArray *sessions, NSError *listErr) {
                    XCTAssertNil(listErr);
                    BOOL found = NO;
                    for (id s in sessions) {
                        if ([[s valueForKey:@"sessionId"] isEqualToString:sessionId]) {
                            found = YES;
                        }
                    }
                    XCTAssertFalse(found, @"Deleted session should not appear in list");

                    [client stopWithCompletion:^(NSError *stopErr) {
                        [exp fulfill];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Model list

- (void)testModelList {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"model list"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client listModelsWithCompletion:^(NSArray *models, NSError *modelsErr) {
            XCTAssertNil(modelsErr);
            XCTAssertNotNil(models);
            XCTAssertTrue(models.count > 0, @"Models list must not be empty");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Ping

- (void)testPing {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"ping"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client pingWithCompletion:^(NSDictionary *resp, NSError *pingErr) {
            XCTAssertNil(pingErr);
            XCTAssertNotNil(resp, @"Ping response must not be nil");
            XCTAssertNotNil(resp[@"message"], @"Ping should have message field");
            XCTAssertNotNil(resp[@"timestamp"], @"Ping should have timestamp field");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Auth status

- (void)testAuthStatus {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"auth status"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client getAuthStatusWithCompletion:^(NSDictionary *status, NSError *authErr) {
            XCTAssertNil(authErr);
            XCTAssertNotNil(status, @"Auth status must not be nil");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Client lifecycle

- (void)testClientLifecycle {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"client lifecycle"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);
        XCTAssertTrue(client.isConnected, @"Client should be connected after start");

        [client stopWithCompletion:^(NSError *stopErr) {
            XCTAssertNil(stopErr);
            XCTAssertFalse(client.isConnected, @"Client should be disconnected after stop");
            [exp fulfill];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Foreground session

- (void)testForegroundSession {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"foreground session"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            NSString *sessionId = session.sessionId;

            [client setForegroundSessionId:sessionId completion:^(NSError *setErr) {
                XCTAssertNil(setErr);

                [client getForegroundSessionIdWithCompletion:^(NSString *fgId, NSError *getErr) {
                    XCTAssertNil(getErr);
                    XCTAssertEqualObjects(fgId, sessionId,
                                          @"Foreground session ID should match");

                    [client stopWithCompletion:^(NSError *stopErr) {
                        [exp fulfill];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Tools

- (void)testTools {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"tools"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        CPToolDefinition *tool = [[CPToolDefinition alloc] init];
        tool.name = @"test_tool";
        tool.toolDescription = @"A test tool for E2E";
        tool.inputSchema = @{@"type": @"object",
                             @"properties": @{@"input": @{@"type": @"string"}}};
        config.tools = @[tool];

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);

            CPMessageOptions *msgOpts = [[CPMessageOptions alloc] init];
            msgOpts.prompt = @"Use the test_tool with input hello";

            [session sendAndWait:msgOpts timeout:30000 completion:^(NSString *content, NSError *sendErr) {
                XCTAssertNil(sendErr);
                XCTAssertNotNil(content, @"Should receive a response");

                [client stopWithCompletion:^(NSError *stopErr) {
                    [exp fulfill];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Streaming

- (void)testStreaming {
    [self loadSnapshot:@"test/snapshots/session/should_receive_session_events.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"streaming"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        config.streaming = YES;

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);

            CPMessageOptions *msgOpts = [[CPMessageOptions alloc] init];
            msgOpts.prompt = @"Hello streaming";

            [session sendAndWait:msgOpts timeout:30000 completion:^(NSString *content, NSError *sendErr) {
                XCTAssertNil(sendErr);
                XCTAssertNotNil(content, @"Streaming response must not be nil");
                XCTAssertTrue(content.length > 0, @"Streaming response must not be empty");

                [client stopWithCompletion:^(NSError *stopErr) {
                    [exp fulfill];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - System message customization

- (void)testSystemMessageCustomization {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"system message"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        config.systemMessage = @{@"content": @"You are a helpful test assistant.",
                                 @"mode": @"append"};

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);
            XCTAssertTrue(session.sessionId.length > 0,
                          @"Session with system message should have valid ID");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - SessionFs provider

- (void)testSessionFsProvider {
    [self loadSnapshot:@"test/snapshots/session_fs/should_route_file_operations_through_the_session_fs_provider.yaml"];

    CPSessionFsConfig *fsConfig = [[CPSessionFsConfig alloc] init];
    fsConfig.initialCwd = @"/";
    fsConfig.sessionStatePath = @"/session-state";
    fsConfig.conventions = @"posix";

    CPCopilotClient *client = [self makeClientWithSessionFs:fsConfig];

    XCTestExpectation *exp = [self expectationWithDescription:@"session fs provider"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr, @"Client with sessionFs should start OK: %@", startErr);

        [client stopWithCompletion:^(NSError *stopErr) {
            [exp fulfill];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - MCP servers config

- (void)testMcpServersConfig {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"mcp servers config"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        config.mcpServers = @{@"testServer": @{@"command": @"echo",
                                                @"args": @[@"hello"]}};

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);
            XCTAssertTrue(session.sessionId.length > 0,
                          @"Session with MCP config should have valid ID");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Skills config

- (void)testSkillsConfig {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"skills config"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        CPSessionConfig *config = [[CPSessionConfig alloc] init];
        config.skills = @{@"directories": @[@"/path/to/skills"]};

        [client createSession:config completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);
            XCTAssertNotNil(session);
            XCTAssertTrue(session.sessionId.length > 0,
                          @"Session with skills config should have valid ID");

            [client stopWithCompletion:^(NSError *stopErr) {
                [exp fulfill];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:30 handler:nil];
}

#pragma mark - Compaction

- (void)testCompaction {
    [self loadSnapshot:@"test/snapshots/session/should_have_stateful_conversation.yaml"];

    CPCopilotClient *client = [self makeClient];

    XCTestExpectation *exp = [self expectationWithDescription:@"compaction"];

    [client startWithCompletion:^(NSError *startErr) {
        XCTAssertNil(startErr);

        [client createSession:nil completion:^(CPCopilotSession *session, NSError *sessErr) {
            XCTAssertNil(sessErr);

            // Send first message
            CPMessageOptions *msg1 = [[CPMessageOptions alloc] init];
            msg1.prompt = @"Message 1: Tell me something interesting.";

            [session sendAndWait:msg1 timeout:30000 completion:^(NSString *r1, NSError *e1) {
                XCTAssertNil(e1);
                XCTAssertNotNil(r1);

                CPMessageOptions *msg2 = [[CPMessageOptions alloc] init];
                msg2.prompt = @"Message 2: Tell me more.";

                [session sendAndWait:msg2 timeout:30000 completion:^(NSString *r2, NSError *e2) {
                    XCTAssertNil(e2);
                    XCTAssertNotNil(r2);

                    CPMessageOptions *msg3 = [[CPMessageOptions alloc] init];
                    msg3.prompt = @"Message 3: Continue please.";

                    [session sendAndWait:msg3 timeout:30000 completion:^(NSString *r3, NSError *e3) {
                        XCTAssertNil(e3);
                        XCTAssertNotNil(r3);

                        CPMessageOptions *msg4 = [[CPMessageOptions alloc] init];
                        msg4.prompt = @"Message 4: Keep going.";

                        [session sendAndWait:msg4 timeout:30000 completion:^(NSString *r4, NSError *e4) {
                            XCTAssertNil(e4);
                            XCTAssertNotNil(r4);

                            CPMessageOptions *msg5 = [[CPMessageOptions alloc] init];
                            msg5.prompt = @"Message 5: One more thing.";

                            [session sendAndWait:msg5 timeout:30000 completion:^(NSString *r5, NSError *e5) {
                                XCTAssertNil(e5);
                                XCTAssertNotNil(r5);

                                [client stopWithCompletion:^(NSError *stopErr) {
                                    [exp fulfill];
                                }];
                            }];
                        }];
                    }];
                }];
            }];
        }];
    }];

    [self waitForExpectationsWithTimeout:60 handler:nil];
}

@end
