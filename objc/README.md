# GitHub Copilot SDK for Objective-C

The official Objective-C SDK for the GitHub Copilot CLI. Communicate with the Copilot CLI server via JSON-RPC 2.0 over stdio using idiomatic Objective-C patterns -- blocks, `NSError**`, GCD queues, and Foundation types.

## Architecture

```
Your App (Objective-C)
    |
    v
CPCopilotClient  -->  CPJsonRpcClient  -->  Copilot CLI (server mode)
    |                    (JSON-RPC 2.0       (NSTask + NSPipe stdio)
    v                     Content-Length
CPCopilotSession          framing)
    |
    v
Event Handlers (blocks)
```

## Requirements

- macOS 13+ / iOS 16+ / tvOS 16+ / watchOS 9+
- Xcode 15+
- Copilot CLI installed (or reachable via `cliUrl`)

## Installation

### Swift Package Manager

Add this to your `Package.swift`:

```swift
dependencies: [
    .package(path: "../objc")  // or remote URL
]
```

Or in Xcode: File > Add Package Dependencies, then point to this directory.

### CocoaPods

Add to your `Podfile`:

```ruby
pod 'CopilotObjCSDK', :path => '../objc'
```

Then run:

```bash
pod install
```

### Manual Integration

Copy the files from `src/` into your Xcode project:

- `CPCopilotClient.h` / `.m`
- `CPCopilotSession.h` / `.m`
- `CPJsonRpcClient.h` / `.m`
- `CPTypes.h` / `.m`
- `CPDefineTool.h` / `.m`
- `CPSdkProtocolVersion.h`

## Quick Start

```objc
#import "CPCopilotClient.h"
#import "CPCopilotSession.h"
#import "CPTypes.h"

// 1. Create and start the client
CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
CPCopilotClient *client = [[CPCopilotClient alloc] initWithOptions:opts];

[client startWithCompletion:^(NSError *error) {
    if (error) {
        NSLog(@"Start failed: %@", error);
        return;
    }

    // 2. Create a session
    CPSessionConfig *config = [[CPSessionConfig alloc] init];
    config.streaming = YES;

    [client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
        if (error) {
            NSLog(@"Session error: %@", error);
            return;
        }

        // 3. Listen for events
        [session onEventType:@"assistant.message_delta" handler:^(CPSessionEvent *event) {
            if (event.deltaContent) {
                printf("%s", [event.deltaContent UTF8String]);
            }
        }];

        // 4. Send a message
        CPMessageOptions *msg = [[CPMessageOptions alloc] init];
        msg.prompt = @"Explain the Objective-C runtime in 3 sentences.";

        [session sendAndWait:msg timeout:30000 completion:^(NSString *content, NSError *error) {
            NSLog(@"Response: %@", content);
        }];
    }];
}];
```

## API Reference

### CPCopilotClient

The main client manages the CLI process lifecycle and session creation.

| Method | Description |
|--------|-------------|
| `initWithOptions:` | Create a client with the given options |
| `startWithCompletion:` | Start the CLI server process |
| `stopWithCompletion:` | Stop the client and kill the CLI process |
| `createSession:completion:` | Create a new conversation session |
| `resumeSession:config:completion:` | Resume an existing session by ID |
| `pingWithMessage:completion:` | Ping the server |
| `getMetadataWithCompletion:` | Get server metadata |
| `setForeground:completion:` | Set the foreground session hint |
| `listSessionsWithCompletion:` | List all known sessions |
| `deleteSession:completion:` | Delete a session |

### CPCopilotClientOptions

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `cliPath` | `NSString*` | `nil` (uses "copilot") | Path to CLI executable |
| `cliUrl` | `NSString*` | `nil` | URL for external CLI server |
| `cwd` | `NSString*` | `nil` (inherit) | Working directory for CLI |
| `logLevel` | `NSString*` | `@"info"` | Log level |
| `autoStart` | `BOOL` | `YES` | Auto-start on first session |
| `autoRestart` | `BOOL` | `YES` | Auto-restart on crash |
| `githubToken` | `NSString*` | `nil` | GitHub token |
| `useLoggedInUser` | `BOOL` | `YES` | Use stored OAuth tokens |
| `extraArgs` | `NSArray*` | `nil` | Extra CLI arguments |
| `sessionFs` | `CPSessionFsConfig*` | `nil` | Session filesystem config |

### CPCopilotSession

A conversation session created via the client.

| Method | Description |
|--------|-------------|
| `onEvent:` | Register a handler for all events |
| `onEventType:handler:` | Register a handler for a specific event type |
| `removeHandler:` | Remove an event handler by ID |
| `send:completion:` | Send a message (async, events via handlers) |
| `sendAndWait:timeout:completion:` | Send and wait for idle |
| `abortWithCompletion:` | Abort current message processing |
| `disconnectWithCompletion:` | Disconnect the session |
| `getMetadataWithCompletion:` | Get session metadata |

### CPSessionConfig

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `sessionId` | `NSString*` | `nil` (auto) | Custom session ID |
| `model` | `NSString*` | `nil` (default) | Model identifier |
| `reasoningEffort` | `NSString*` | `nil` | `"low"`, `"medium"`, `"high"`, `"xhigh"` |
| `workingDirectory` | `NSString*` | `nil` | Working directory for tools |
| `streaming` | `BOOL` | `NO` | Enable streaming deltas |
| `tools` | `NSArray*` | `nil` | Array of tool definitions |
| `systemMessage` | `NSDictionary*` | `nil` | System message config |
| `githubToken` | `NSString*` | `nil` | Session-level GitHub token |
| `permissionHandler` | `CPPermissionHandler` | `nil` | Permission request handler |

### CPMessageOptions

| Property | Type | Description |
|----------|------|-------------|
| `prompt` | `NSString*` | The message text (required) |
| `attachments` | `NSArray<CPAttachment*>*` | File attachments |
| `mode` | `NSString*` | `"enqueue"` (default) or `"immediate"` |

### Session Events

Events are delivered via blocks registered with `onEvent:` or `onEventType:handler:`.

| Event Type | Description |
|-----------|-------------|
| `assistant.message` | Final assistant message |
| `assistant.message_delta` | Streaming text delta |
| `assistant.reasoning` | Final reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning delta |
| `session.idle` | Session finished processing |
| `session.error` | Session error occurred |
| `tool.executing` | Tool is being executed |
| `tool.executed` | Tool execution completed |
| `session.compaction_start` | Infinite session compaction started |
| `session.compaction_complete` | Compaction finished |

## Defining Tools

Use `CPDefineTool()` to create tools with JSON Schema parameters:

```objc
#import "CPDefineTool.h"

NSDictionary *tool = CPDefineTool(
    @"get_weather",
    @"Get the current weather for a city",
    @{
        @"type": @"object",
        @"properties": @{
            @"city": @{@"type": @"string", @"description": @"City name"},
            @"unit": @{@"type": @"string", @"enum": @[@"celsius", @"fahrenheit"]}
        },
        @"required": @[@"city"]
    },
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        NSString *city = invocation.arguments[@"city"];
        NSString *unit = invocation.arguments[@"unit"] ?: @"celsius";
        NSString *result = [NSString stringWithFormat:@"Weather in %@: 22 %@", city, unit];
        done([CPToolResult successWithText:result]);
    }
);

// For tools with no parameters:
NSDictionary *timeTool = CPDefineSimpleTool(
    @"get_time",
    @"Get the current time",
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        NSDateFormatter *fmt = [[NSDateFormatter alloc] init];
        fmt.dateFormat = @"yyyy-MM-dd'T'HH:mm:ssZ";
        done([CPToolResult successWithText:[fmt stringFromDate:[NSDate date]]]);
    }
);

// Pass tools in session config
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.tools = @[tool, timeTool];
```

## Streaming

Enable streaming to receive incremental text as it is generated:

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.streaming = YES;

[client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
    // Receive deltas
    [session onEventType:@"assistant.message_delta" handler:^(CPSessionEvent *event) {
        printf("%s", [event.deltaContent UTF8String]);
        fflush(stdout);
    }];

    // Know when complete
    [session onEventType:@"session.idle" handler:^(CPSessionEvent *event) {
        NSLog(@"\nDone!");
    }];

    CPMessageOptions *msg = [[CPMessageOptions alloc] init];
    msg.prompt = @"Write a haiku about Objective-C";
    [session send:msg completion:^(NSString *messageId, NSError *error) {
        NSLog(@"Message sent: %@", messageId);
    }];
}];
```

## Error Handling

All async methods use the standard Objective-C error pattern with completion blocks:

```objc
[client createSession:nil completion:^(CPCopilotSession *session, NSError *error) {
    if (error) {
        NSLog(@"Error domain: %@", error.domain);
        NSLog(@"Error code: %ld", (long)error.code);
        NSLog(@"Description: %@", error.localizedDescription);
        return;
    }
    // Use session...
}];
```

## Session Persistence

Sessions support persistence through infinite sessions (enabled by default):

```objc
// Sessions are persisted automatically
// Resume a previous session by ID:
[client resumeSession:@"previous-session-id" config:nil
           completion:^(CPCopilotSession *session, NSError *error) {
    // Conversation history is restored
}];

// List all available sessions:
[client listSessionsWithCompletion:^(NSArray<CPSessionMetadata *> *sessions, NSError *error) {
    for (CPSessionMetadata *meta in sessions) {
        NSLog(@"Session: %@ - %@", meta.sessionId, meta.summary);
    }
}];
```

## Permission Handling

Handle permission requests from the Copilot server:

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.permissionHandler = ^(NSDictionary *request, NSString *sessionId,
                              void (^completion)(CPPermissionKind kind)) {
    NSString *permKind = request[@"kind"];
    NSLog(@"Permission requested: %@ for session %@", permKind, sessionId);

    // Auto-approve read operations, deny others
    if ([permKind isEqualToString:@"read"]) {
        completion(CPPermissionKindApproved);
    } else {
        completion(CPPermissionKindDeniedByUser);
    }
};
```

## System Messages

Customize the system prompt:

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.systemMessage = @{
    @"mode": @"append",
    @"content": @"You are a helpful assistant that specializes in Objective-C development."
};
```

## File Structure

```
objc/
  src/
    CPCopilotClient.h/.m      - Main client (process lifecycle, sessions)
    CPCopilotSession.h/.m      - Session (send, events, abort)
    CPJsonRpcClient.h/.m       - JSON-RPC 2.0 transport
    CPTypes.h/.m               - Type definitions
    CPDefineTool.h/.m          - Tool definition helpers
    CPSdkProtocolVersion.h     - Protocol version constant
  tests/
    CPClientTests.m            - XCTest unit tests
  examples/
    BasicExample.m             - Complete working example
  cookbook/
    README.md                  - Cookbook index
    error-handling.md          - Error handling recipes
    multiple-sessions.md       - Multiple session patterns
    persisting-sessions.md     - Session persistence
    tools-and-skills.md        - Custom tools guide
    advanced-features.md       - Advanced configuration
  Package.swift                - Swift Package Manager support
  CopilotSDK.podspec           - CocoaPods spec
  README.md                    - This file
```

## Running Tests

```bash
# Using Swift Package Manager
cd objc
swift test

# Or via Xcode
# Open Package.swift, select the test target, and run (Cmd+U)
```

## Building the Example

```bash
cd objc/examples
clang -framework Foundation -fobjc-arc \
  -I../src ../src/*.m BasicExample.m -o basic_example
./basic_example
```

## Cookbook

See the [cookbook/](cookbook/) directory for detailed recipes:

- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced Features](cookbook/advanced-features.md)

## License

See [LICENSE](../LICENSE) for details.
