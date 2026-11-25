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
- Copilot CLI 1.0.113 or newer installed (`cliPath` may point at the binary, at `node_modules/.bin/copilot`, or at `@github/copilot/npm-loader.js`, which is run with `node`)

## Installation

### Swift Package Manager

The Objective-C package is not published to a registry, and the `Package.swift` at the repository root only provides the Swift SDK, so add it as a local package from a clone of this repository. In your `Package.swift`:

```swift
dependencies: [
    .package(path: "../objc")  // path to the objc/ directory of your clone
]
```

Then add the product to your target:

```swift
.target(
    name: "MyApp",
    dependencies: [
        .product(name: "CopilotObjCSDK", package: "objc")
    ]
)
```

Or in Xcode: File > Add Package Dependencies > Add Local, then choose the `objc/` directory.

### CocoaPods

The pod is not on CocoaPods trunk. Its podspec file is named `CopilotSDK.podspec`, so point `:path` at the file itself (from a directory, CocoaPods only looks for `CopilotObjCSDK.podspec`). Add to your `Podfile`:

```ruby
pod 'CopilotObjCSDK', :path => '../objc/CopilotSDK.podspec'
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
- `CPSessionParams.h` / `.m`
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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, delivered to the completion block as an `NSError` in `CPJsonRpcErrorDomain` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store (`mcp.secrets.save`, `mcp.secrets.delete`, `mcp.secrets.deleteServer`). Pass `nil` for `configDir` to use the runtime's default configuration directory. The secret value is never logged.

```objc
NSString *token = NSProcessInfo.processInfo.environment[@"MY_SERVER_TOKEN"];
[client mcpSecretsSave:@"my-server-token" value:token configDir:nil
            completion:^(CPMcpSecretsSaveResult *result, NSError *error) {
    if (result) NSLog(@"%@", result.usedKeychain ? @"Stored in the OS keychain" : @"Stored in the file backend");
}];

[client mcpSecretsDelete:@"my-server-token" configDir:nil completion:^(NSError *error) {}];
// Every credential indexed for the server
[client mcpSecretsDeleteServer:@"my-server" configDir:nil completion:^(NSError *error) {}];
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions (`permissions.extensionApprovals.grant` / `.list` / `.revoke`). The list and revoke methods take an optional `sessionId` to use an attached session's configuration profile.

```objc
// Persist a pending extension permission request before answering it normally
[client extensionApprovalsGrantForSession:session.sessionId
                                requestId:requestId
                               completion:^(CPPermissionsExtensionApprovalsGrantResult *result, NSError *error) {
    if (result.granted) NSLog(@"Approved %@", result.approval.extensionName);
}];

[client extensionApprovalsListForSession:nil
                              completion:^(CPPermissionsExtensionApprovalsListResult *result, NSError *error) {
    for (CPPermissionsExtensionApproval *approval in result.approvals) {
        // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
        [client extensionApprovalsRevoke:approval.extensionName
                         installationKey:approval.installationKey
                               sessionId:nil
                              completion:^(CPPermissionsExtensionApprovalsRevokeResult *revoke, NSError *revokeError) {}];
    }
}];
```

### Ask-user image attachments

`CPSessionConfig.userInputHandler` answers the agent's ask_user questions (`userInput.request`); setting it enables the ask_user tool. When the runtime accepts images in an answer, the `CPUserInputRequest` carries `imageAttachmentConstraints` (`CPUserInputImageAttachmentConstraints` with `allowedMimeTypes`, `maxBytesPerImage`, `maxCount`); it is nil otherwise. The request also exposes `requestId` and `meta` (the opaque `_meta` object) when sent. Answer with `CPUserInputResponse.attachments`, an array of `CPUserInputImageAttachment` (base64 `data`, `mimeType`, optional `imageDescription` and `displayName`; the wire `type` is always `"image"`). `attachments` is sent only when set. Image-only answers use an empty `answer` and `wasFreeform = YES`; the runtime enforces the limits.

```objc
config.userInputHandler = ^(CPUserInputRequest *request, NSString *sessionId,
                            void (^completion)(CPUserInputResponse *response)) {
    CPUserInputResponse *response = [[CPUserInputResponse alloc] init];
    response.wasFreeform = YES;
    NSData *png = [NSData dataWithContentsOfFile:@"screenshot.png"];
    if (png && [request.imageAttachmentConstraints.allowedMimeTypes containsObject:@"image/png"]) {
        CPUserInputImageAttachment *image =
            [CPUserInputImageAttachment attachmentWithData:[png base64EncodedStringWithOptions:0]
                                                  mimeType:@"image/png"];
        image.displayName = @"screenshot.png";
        response.answer = @"";
        response.attachments = @[image];
    } else {
        response.answer = @"No screenshot available";
    }
    completion(response);
};
```

## What's New in v3.0.0

The Objective-C SDK now speaks **SDK protocol 4** and requires **Copilot CLI 1.0.113 or newer**. Older CLIs are rejected at start with an "SDK protocol version mismatch" error.

**Existing options keep working.** `CPSessionConfig` still produces the same flat options (`-toDictionary`), and the client converts them into the grouped protocol 4 payload with `CPGroupSessionParams` right before `session.create` and `session.resume` (for example `streaming` becomes `events.streaming`). A legacy single `provider` dictionary is sent as the named provider "default" with Copilot API models disabled. Failures of `session.abort`, `session.delete`, and `session.setForeground` arrive as errors.

This release also repairs the transport: the CLI is spawned with `--headless --stdio`, `.js` entry points run under `node`, session RPCs use their real names (`session.send`, `session.abort`, `session.getMetadata`), `session.event` notifications reach their sessions, tools defined with `CPDefineTool` are answered through `external_tool.requested`, `permissionHandler` answers `permission.requested`, and the type declarations lost from `CPTypes.h` in an earlier merge are restored.

### Client options: client info, client mode, network callbacks

```objc
CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];

CPClientInfo *info = [[CPClientInfo alloc] init];   // sent on the `connect` handshake
info.editorName = @"MyEditor";
info.editorVersion = @"2.1.0";
opts.clientInfo = info;

// CPClientModeCodingAgent (default) or CPClientModeEmpty: minimal defaults, and every
// session must set availableTools
opts.mode = CPClientModeCodingAgent;

CPNetworkOptions *network = [[CPNetworkOptions alloc] init];   // registered with network.setProvider
network.scope = CPNetworkScopeConnection;
network.resolveProxy = ^(NSString *url, void (^completion)(NSString *proxy)) {
    completion([url hasPrefix:@"https://internal."] ? @"PROXY proxy.corp:8080" : nil);
};
network.authenticateBasic = ^(CPNetworkBasicAuthRequest *request,
                              void (^completion)(CPNetworkBasicCredentials *credentials)) {
    CPNetworkBasicCredentials *credentials = [[CPNetworkBasicCredentials alloc] init];
    credentials.username = @"svc-account";
    credentials.password = @"load-from-keychain";   // never log it
    completion(credentials);
};
opts.network = network;
```

`authenticateNegotiate` returns a base64 SPNEGO token for a Kerberos service principal.

### Session options

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.availableTools = @[@"view", @"edit"];
config.askUserVariant = CPAskUserVariantElicitation;
config.allowedModels = @[@"gpt-5", @"claude-sonnet-4.5"];
config.featureFlags = @{@"my_flag": @YES};
config.authClientIdMetadataUrl = @"https://example.com/oauth/client.json";
config.agentDirectories = @[@"./agents"];
config.hookDirectories = @[@"./hooks"];
config.workingDirectory = @"/repo";
config.trustWorkingDirectory = CPTrustWorkingDirectorySession;   // session.create only
config.maxOutputTokens = @4096;   // inference.capabilities.limits.max_output_tokens
config.autoTier = CPAutoTierFast;
config.permissionMode = CPPermissionModeAssisted;

CPSandboxConfig *sandbox = [[CPSandboxConfig alloc] init];
sandbox.enabled = YES;
sandbox.allowBypass = @YES;
config.sandboxConfig = sandbox;

CPImageGenerationConfig *images = [[CPImageGenerationConfig alloc] init];
images.enabled = @YES;
images.saveToDisk = @NO;
config.imageGeneration = images;

CPAutopilotOptions *autopilot = [[CPAutopilotOptions alloc] init];
autopilot.continuationEnabled = @YES;
autopilot.maxContinues = @3;
config.autopilot = autopilot;

// Resume-only options
CPSessionConfig *resumeConfig = [[CPSessionConfig alloc] init];
resumeConfig.allowTranscriptRecovery = @YES;
resumeConfig.suppressResumeEvent = @YES;
resumeConfig.clearDefaultProvider = YES;   // sends defaultProvider: null
```

### GitHub token provider

```objc
config.gitHubTokenProvider = ^(CPGitHubTokenProviderArgs *args, void (^completion)(CPGitHubToken *token)) {
    // args.reason is "initial" or "refresh"; call completion(nil) to cancel
    completion([CPGitHubToken tokenWithAccessToken:@"gho_from_your_keychain" expiresIn:3600]);
};
```

The client registers the provider under a generated ID, answers `gitHubToken.getToken`, and drops the registration when the session detaches. It cannot be combined with `githubToken`.

### Named BYOK providers

```objc
CPNamedProviderConfig *ollama = [[CPNamedProviderConfig alloc] init];
ollama.name = @"ollama";
ollama.type = @"openai";
ollama.baseUrl = @"http://localhost:11434/v1";
ollama.apiKeyCommand = @"my-key-helper";

CPProviderModelConfig *llama = [[CPProviderModelConfig alloc] init];
llama.provider = @"ollama";
llama.identifier = @"llama3";
llama.maxOutputTokens = @2048;

CPCapiConfig *capi = [[CPCapiConfig alloc] init];
capi.enabled = @NO;

config.model = @"ollama/llama3";   // "<provider>/<identifier>"
config.providers = @[ollama];
config.models = @[llama];
config.defaultProvider = @"ollama";
config.capi = capi;
```

### Tools, messages, and events

```objc
config.tools = @[CPIdempotentTool(CPDefineSimpleTool(@"get_time", @"Current time",
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        done([CPToolResult successWithText:@"12:00"]);
    }))];

CPMessageOptions *msg = [[CPMessageOptions alloc] init];
msg.prompt = @"List the open issues as JSON";
msg.source = CPMessageSourceAgent(@"reviewer");   // "agent-reviewer"
msg.clientTurnId = @"turn-42";
msg.responseSchema = @{@"type": @"object", @"properties": @{@"issues": @{@"type": @"array"}}};
[session send:msg completion:^(NSString *messageId, NSError *error) {
    NSLog(@"sent %@", messageId);
}];

// Live shell output (`tool.shell_output`)
[session onEventType:CPEventTypeToolShellOutput handler:^(CPSessionEvent *event) {
    printf("[%s] %s", (event.shellOutput.stream ?: @"stdout").UTF8String, event.shellOutput.text.UTF8String);
}];
```

`responseSchema` is sent as `responseFormat: {type: "json_schema", jsonSchema: {name: "response", strict: true, schema}}`.

### Shell authentication, memory store, and sub-agent hooks

```objc
config.onShellAuthentication = ^(CPShellAuthenticationRequest *request,
                                 void (^completion)(CPShellAuthenticationResponse *response)) {
    if ([request.inputKind isEqualToString:CPShellAuthenticationInputKindConfirm]) {
        completion([CPShellAuthenticationResponse acceptWithValue:@"yes"]);
    } else {
        completion([CPShellAuthenticationResponse cancel]);
    }
};

config.onMemoryStore = ^(CPStoredMemory *memory, void (^completion)(NSError *error)) {
    NSLog(@"remember %@: %@", memory.subject, memory.fact);
    completion(nil);
};

CPSessionHooks *hooks = [[CPSessionHooks alloc] init];   // delivered through hooks.invoke
hooks.onSubagentStart = ^(NSDictionary *input, NSString *sessionId, void (^completion)(NSDictionary *output)) {
    NSLog(@"sub-agent starting in %@", sessionId);
    completion(nil);
};
config.hooks = hooks;
```

### Session and client methods

```objc
[session setAutoTier:CPAutoTierBalance completion:^(NSError *error) {}];   // session.model.switchAutoTier
[session setPermissionMode:CPPermissionModeAllowAll
                completion:^(CPPermissionModeResult *result, NSError *error) {
    if (result && !result.success) NSLog(@"refused: %@", result.reason);   // "managed_policy"
}];
[session recheckPolicyHelperWithCompletion:^(NSDictionary *result, NSError *error) {}];
[client clearManagedSettingsCacheWithCompletion:^(NSError *error) {}];   // managedSettings.clearCache
[session detachWithCompletion:^(NSError *error) {}];   // session.detach; the session stays resumable

[client createSession:config completion:^(CPCopilotSession *created, NSError *error) {
    CPPolicyHelperFailure *failure = [CPPolicyHelperFailure failureFromError:error];
    if (failure) NSLog(@"%@: %@", failure.category, failure.message);   // untrusted text: show as plain text
}];
```

`+[CPPolicyHelperFailure failureFromDictionary:]` also reads the failure from a `getManagedSettingsWithCompletion:` snapshot.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the SDK keeps spawning the CLI. The existing `inProcess` option is unchanged.
- `cliUrl` (connecting to an already running server) is not implemented yet; `startWithCompletion:` reports an error.

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
| `clearManagedSettingsCacheWithCompletion:` | Clear cached managed settings (`managedSettings.clearCache`) |
| `mcpSecretsSave:value:configDir:completion:` | Save an MCP credential (`mcp.secrets.save`, CLI 1.0.116+); returns `CPMcpSecretsSaveResult` |
| `mcpSecretsDelete:configDir:completion:` | Delete an MCP credential (`mcp.secrets.delete`, CLI 1.0.116+) |
| `mcpSecretsDeleteServer:configDir:completion:` | Delete an MCP server's credentials (`mcp.secrets.deleteServer`, CLI 1.0.116+) |
| `extensionApprovalsGrantForSession:requestId:completion:` | Persist a pending plugin extension approval (`permissions.extensionApprovals.grant`, CLI 1.0.116+) |
| `extensionApprovalsListForSession:completion:` | List plugin extension approvals (`permissions.extensionApprovals.list`, CLI 1.0.116+) |
| `extensionApprovalsRevoke:installationKey:sessionId:completion:` | Revoke a plugin extension approval (`permissions.extensionApprovals.revoke`, CLI 1.0.116+) |

### CPCopilotClientOptions

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `cliPath` | `NSString*` | `nil` (uses "copilot") | Path to CLI executable (`.js` paths run with `node`) |
| `cliUrl` | `NSString*` | `nil` | URL for external CLI server (not implemented yet) |
| `cwd` | `NSString*` | `nil` (inherit) | Working directory for CLI |
| `logLevel` | `NSString*` | `@"info"` | Log level |
| `autoStart` | `BOOL` | `YES` | Auto-start on first session |
| `autoRestart` | `BOOL` | `YES` | Auto-restart on crash |
| `githubToken` | `NSString*` | `nil` | GitHub token |
| `useLoggedInUser` | `BOOL` | `YES` | Use stored OAuth tokens |
| `extraArgs` | `NSArray*` | `nil` | Extra CLI arguments |
| `sessionFs` | `CPSessionFsConfig*` | `nil` | Session filesystem config |
| `onGetTraceContext` | `CPTraceContextProvider` | `nil` | W3C trace context for create/resume |
| `clientInfo` | `CPClientInfo*` | `nil` | Integration identity sent on `connect` |
| `mode` | `NSString*` | `nil` (coding agent) | `CPClientModeCodingAgent` or `CPClientModeEmpty` |
| `network` | `CPNetworkOptions*` | `nil` | Proxy and authentication callbacks |

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
| `disconnectWithCompletion:` | Disconnect the session (`session.detach`) |
| `detachWithCompletion:` | Release the session on the server; it stays resumable |
| `getMetadataWithCompletion:` | Get session metadata |
| `setAutoTier:completion:` | Change the Auto routing tier (`session.model.switchAutoTier`) |
| `setPermissionMode:completion:` | Set the permission mode; returns `CPPermissionModeResult` |
| `getManagedSettingsWithCompletion:` | Effective managed settings snapshot |
| `recheckPolicyHelperWithCompletion:` | Re-run the managed-settings Policy Helper |

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
| `userInputHandler` | `CPUserInputHandler` | `nil` | ask_user handler (`userInput.request`); enables the ask_user tool |

### CPMessageOptions

| Property | Type | Description |
|----------|------|-------------|
| `prompt` | `NSString*` | The message text (required) |
| `attachments` | `NSArray<CPAttachment*>*` | File attachments |
| `mode` | `NSString*` | `"enqueue"` (default) or `"immediate"` |
| `source` | `NSString*` | `CPMessageSourceUser`, `CPMessageSourceSystem`, or `CPMessageSourceAgent(agentId)` |
| `responseSchema` | `NSDictionary*` | JSON Schema for structured output |
| `clientTurnId` | `NSString*` | Opaque turn ID echoed on this turn's events |

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
| `tool.shell_output` | Live shell output (`event.shellOutput`); not persisted |

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

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk`. These properties live on
`CPSessionConfig` (session-scoped), `CPCopilotClientOptions` (client-scoped),
`CPMessageOptions` (per turn), or `CPSessionHooks`.

### v2.5

- **Reasoning effort** — `CPSessionConfig.reasoningEffort` (`@"low"`, `@"medium"`, `@"high"`, `@"xhigh"`).
- **Session rewind** — `CPSessionConfig.rewindEnabled` rolls the conversation back to an earlier turn.
- **Additional directories** — `CPSessionConfig.additionalDirectories`.
- **Content exclusion** — `CPSessionConfig.contentExclusion`.
- **Tool search** — `CPSessionConfig.toolSearch` discovers tools on demand.
- **Disabled MCP servers** — `CPSessionConfig.disabledMcpServers`.
- **GitHub MCP tool config** — `CPSessionConfig.githubMcpToolConfig`.
- **Canvas provider** — `CPSessionConfig.canvasProvider`.
- **Custom agents local-only** — `CPSessionConfig.customAgentsLocalOnly`.
- **Experimental mode** — `CPSessionConfig.experimentalMode`.
- **User-prompt-transformed hook** — `CPSessionHooks.onUserPromptTransformed`.
- **Permission decision context** — `CPSessionConfig.decisionContext`.
- **Agent-factory args schema** — `CPSessionConfig.argsSchema`.
- **Built-in plugin dirs / in-process FFI** — `CPCopilotClientOptions.builtinPluginDirectories`, `CPCopilotClientOptions.inProcess`.

### v2.4

- **BYOK bearer token provider** — `CPCopilotClientOptions.bearerTokenProvider` (`CPBearerTokenProvider`).
- **MCP OAuth token handler** — `CPSessionConfig.onMcpAuthRequest` (`CPMcpAuthHandler`).
- **HTTP request handler** — `CPCopilotClientOptions.requestHandler` (`CPCopilotRequestHandler`).
- **Session citations** — `CPSessionConfig.enableCitations`.
- **Excluded built-in agents** — `CPSessionConfig.excludedBuiltinAgents`.
- **Spending limits** — `CPSessionConfig.sessionLimits` with `CPSessionLimitsConfig.maxAiCredits`.
- **Session memory** — `CPSessionConfig.memory` with `CPMemoryConfiguration`.
- **OTLP protocol / WebSocket transport** — `CPSessionConfig.otlpProtocol`, `CPSessionConfig.enableWebSocketResponses`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `CPSessionHooks.onPostToolUse`, `CPSessionHooks.onPreMcpToolCall`.
- **Message agent mode / display prompt** — `CPMessageOptions.agentMode`, `CPMessageOptions.displayPrompt`.
- **Tool defer loading** — `CPToolDeferAuto` / `CPToolDeferNever`.
- **System message sections** — `CPSystemMessageSectionPreamble`, `CPSystemMessageSectionPreserve`.
- **GitHub attachments** — `CPGitHubAttachmentCommit`, `CPGitHubAttachmentRepository`.
- **Experiment assignments** — `CPSessionConfig.expAssignments`.

```objc
// Reasoning effort + session rewind + extra workspace roots
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.reasoningEffort = @"high";
config.rewindEnabled = @YES;
config.additionalDirectories = @[@"/repo/packages/core", @"/repo/packages/api"];
config.contentExclusion = @YES;             // honor content-exclusion rules
config.toolSearch = @{@"mode": @"auto"};    // discover tools on demand
```

```objc
// Bring-your-own-key: supply bearer tokens dynamically
CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
opts.bearerTokenProvider = ^(CPProviderTokenArgs *args, void (^completion)(NSString *token)) {
    completion(@"ghs_exampletoken");
};
```

```objc
// Cap AI-credit spend and emit citations
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.enableCitations = @YES;
CPSessionLimitsConfig *limits = [[CPSessionLimitsConfig alloc] init];
limits.maxAiCredits = @5;
config.sessionLimits = limits;
```

```objc
// Per-message agent mode and display prompt
CPMessageOptions *msg = [[CPMessageOptions alloc] init];
msg.prompt = @"Refactor this module";
msg.agentMode = @"plan";
msg.displayPrompt = @"Refactor (planning)";
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
    CPSessionParams.h/.m       - SDK protocol 4 session payload grouping
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
