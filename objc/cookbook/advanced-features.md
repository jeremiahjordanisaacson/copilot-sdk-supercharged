# Advanced Features

Advanced configuration options for the Copilot Objective-C SDK.

## Streaming Responses

Enable streaming to receive text as it is generated:

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.streaming = YES;

[client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
    __block NSMutableString *fullResponse = [NSMutableString string];

    [session onEventType:@"assistant.message_delta" handler:^(CPSessionEvent *event) {
        if (event.deltaContent) {
            [fullResponse appendString:event.deltaContent];
            // Update UI incrementally
            dispatch_async(dispatch_get_main_queue(), ^{
                self.textView.string = fullResponse;
            });
        }
    }];

    [session onEventType:@"assistant.message" handler:^(CPSessionEvent *event) {
        NSLog(@"Final message: %@", event.content);
    }];

    [session onEventType:@"session.idle" handler:^(CPSessionEvent *event) {
        NSLog(@"Response complete. Total length: %lu", (unsigned long)fullResponse.length);
    }];

    CPMessageOptions *msg = [[CPMessageOptions alloc] init];
    msg.prompt = @"Write a detailed guide to Objective-C memory management.";
    [session send:msg completion:^(NSString *messageId, NSError *error) {
        if (error) NSLog(@"Send error: %@", error);
    }];
}];
```

## Reasoning Events

Monitor the assistant's reasoning process:

```objc
[session onEventType:@"assistant.reasoning_delta" handler:^(CPSessionEvent *event) {
    NSLog(@"Reasoning: %@", event.deltaContent);
}];

[session onEventType:@"assistant.reasoning" handler:^(CPSessionEvent *event) {
    NSLog(@"Full reasoning: %@", event.content);
}];
```

## Custom System Messages

### Append Mode (default)

```objc
config.systemMessage = @{
    @"mode": @"append",
    @"content": @"Always respond in Markdown format."
};
```

### Replace Mode

```objc
config.systemMessage = @{
    @"mode": @"replace",
    @"content": @"You are an Objective-C expert. Only answer questions about Objective-C."
};
```

### Customize Mode

Fine-grained control over individual system prompt sections:

```objc
config.systemMessage = @{
    @"mode": @"customize",
    @"sectionOverrides": @{
        @"identity": @{@"action": @"replace", @"content": @"You are CodeBot."},
        @"tone": @{@"action": @"append", @"content": @"Be concise and technical."},
        @"safety": @{@"action": @"remove"},
    }
};
```

## Model Selection

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.model = @"gpt-4o";
config.reasoningEffort = @"high";  // "low", "medium", "high", "xhigh"
```

## File Attachments

Attach files to messages for context:

```objc
CPAttachment *attachment = [[CPAttachment alloc] init];
attachment.type = CPAttachmentTypeFile;
attachment.path = @"/path/to/source.m";
attachment.displayName = @"source.m";

CPMessageOptions *msg = [[CPMessageOptions alloc] init];
msg.prompt = @"Review this file for memory leaks.";
msg.attachments = @[attachment];

[session send:msg completion:^(NSString *messageId, NSError *error) {
    // File content is sent along with the message
}];
```

## Custom Working Directory

Set a working directory for tool operations:

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.workingDirectory = @"/Users/dev/my-project";
```

## Session Filesystem Configuration

Configure session filesystem paths:

```objc
CPSessionFsConfig *fs = [[CPSessionFsConfig alloc] init];
fs.initialCwd = @"/Users/dev/project";
fs.sessionStatePath = @"/Users/dev/.copilot-state";
fs.conventions = @"posix";

CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
opts.sessionFs = fs;
```

## GitHub Token Authentication

```objc
// Client-level token (all sessions)
CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
opts.githubToken = @"ghp_...";

// Session-level override
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.githubToken = @"ghp_different_token";
```

## Extra CLI Arguments

Pass additional arguments to the Copilot CLI process:

```objc
CPCopilotClientOptions *opts = [CPCopilotClientOptions defaultOptions];
opts.extraArgs = @[@"--debug", @"--no-color"];
```

## Server Ping

Verify server connectivity:

```objc
[client pingWithMessage:@"health-check" completion:^(NSDictionary *response, NSError *error) {
    if (error) {
        NSLog(@"Server unreachable: %@", error);
        return;
    }
    NSLog(@"Server message: %@", response[@"message"]);
    NSLog(@"Timestamp: %@", response[@"timestamp"]);
    NSLog(@"Protocol version: %@", response[@"protocolVersion"]);
}];
```

## Server Metadata

```objc
[client getMetadataWithCompletion:^(NSDictionary *metadata, NSError *error) {
    NSLog(@"Server metadata: %@", metadata);
}];
```
