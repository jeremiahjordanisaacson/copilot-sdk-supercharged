/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * BasicExample.m - Demonstrates using the Copilot Objective-C SDK
 *
 * Build and run (macOS):
 *   clang -framework Foundation -fobjc-arc \
 *     -I../src ../src/*.m BasicExample.m -o basic_example
 *   ./basic_example
 *
 * Requires the Copilot CLI to be installed and on PATH.
 */

#import <Foundation/Foundation.h>
#import "../src/CPCopilotClient.h"
#import "../src/CPCopilotSession.h"
#import "../src/CPDefineTool.h"
#import "../src/CPTypes.h"

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        dispatch_semaphore_t done = dispatch_semaphore_create(0);

        // 1. Create client options
        CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
        // opts.cliPath = @"/usr/local/bin/copilot";  // Uncomment to set custom path

        // 2. Create client
        CPCopilotClient *client = [[CPCopilotClient alloc] initWithOptions:opts];

        // 3. Start the client
        [client startWithCompletion:^(NSError *error) {
            if (error) {
                NSLog(@"Failed to start client: %@", error.localizedDescription);
                dispatch_semaphore_signal(done);
                return;
            }

            NSLog(@"Client started successfully!");

            // 4. Define a custom tool
            NSDictionary *weatherTool = CPDefineTool(
                @"get_weather",
                @"Get weather for a city",
                @{
                    @"type": @"object",
                    @"properties": @{
                        @"city": @{@"type": @"string", @"description": @"City name"}
                    },
                    @"required": @[@"city"]
                },
                ^(CPToolInvocation *invocation, void (^complete)(CPToolResult *)) {
                    NSString *city = invocation.arguments[@"city"] ?: @"unknown";
                    NSString *result = [NSString stringWithFormat:@"Weather in %@: 22C, sunny", city];
                    complete([CPToolResult successWithText:result]);
                }
            );

            // 5. Create session config
            CPSessionConfig *config = [[CPSessionConfig alloc] init];
            config.tools = @[weatherTool];
            config.streaming = YES;

            // 6. Create a session
            [client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
                if (error) {
                    NSLog(@"Failed to create session: %@", error.localizedDescription);
                    dispatch_semaphore_signal(done);
                    return;
                }

                NSLog(@"Session created: %@", session.sessionId);

                // 7. Register event handlers
                [session onEventType:@"assistant.message_delta" handler:^(CPSessionEvent *event) {
                    if (event.deltaContent) {
                        printf("%s", [event.deltaContent UTF8String]);
                        fflush(stdout);
                    }
                }];

                // 8. Send a message and wait for completion
                CPMessageOptions *msgOpts = [[CPMessageOptions alloc] init];
                msgOpts.prompt = @"What is the weather like in San Francisco?";

                [session sendAndWait:msgOpts timeout:30000 completion:^(NSString *content, NSError *error) {
                    if (error) {
                        NSLog(@"\nError: %@", error.localizedDescription);
                    } else {
                        NSLog(@"\n\nFinal response: %@", content);
                    }

                    // 9. Clean up
                    [session disconnectWithCompletion:^(NSError *error) {
                        [client stopWithCompletion:^(NSError *error) {
                            NSLog(@"Client stopped.");
                            dispatch_semaphore_signal(done);
                        }];
                    }];
                }];
            }];
        }];

        // Wait for everything to complete
        dispatch_semaphore_wait(done, dispatch_time(DISPATCH_TIME_NOW, 120 * NSEC_PER_SEC));
    }
    return 0;
}
