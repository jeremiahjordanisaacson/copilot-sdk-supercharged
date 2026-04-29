# Multiple Sessions

Run parallel conversation sessions for different tasks.

## Creating Multiple Sessions

Each session is independent and can have its own configuration:

```objc
CPSessionConfig *codeConfig = [[CPSessionConfig alloc] init];
codeConfig.model = @"gpt-4o";
codeConfig.systemMessage = @{
    @"mode": @"append",
    @"content": @"You are a code review assistant."
};

CPSessionConfig *docsConfig = [[CPSessionConfig alloc] init];
docsConfig.model = @"gpt-4o-mini";
docsConfig.systemMessage = @{
    @"mode": @"append",
    @"content": @"You are a documentation writer."
};

__block CPCopilotSession *codeSession = nil;
__block CPCopilotSession *docsSession = nil;

dispatch_group_t group = dispatch_group_create();

dispatch_group_enter(group);
[client createSession:codeConfig completion:^(CPCopilotSession *session, NSError *error) {
    codeSession = session;
    dispatch_group_leave(group);
}];

dispatch_group_enter(group);
[client createSession:docsConfig completion:^(CPCopilotSession *session, NSError *error) {
    docsSession = session;
    dispatch_group_leave(group);
}];

dispatch_group_notify(group, dispatch_get_main_queue(), ^{
    NSLog(@"Both sessions ready: %@, %@", codeSession.sessionId, docsSession.sessionId);
});
```

## Routing Messages

Use separate sessions for different concerns:

```objc
- (void)reviewCode:(NSString *)code session:(CPCopilotSession *)session
        completion:(void (^)(NSString *))completion {
    CPMessageOptions *msg = [[CPMessageOptions alloc] init];
    msg.prompt = [NSString stringWithFormat:@"Review this code:\n```\n%@\n```", code];

    [session sendAndWait:msg timeout:30000 completion:^(NSString *content, NSError *error) {
        completion(content);
    }];
}

- (void)generateDocs:(NSString *)code session:(CPCopilotSession *)session
          completion:(void (^)(NSString *))completion {
    CPMessageOptions *msg = [[CPMessageOptions alloc] init];
    msg.prompt = [NSString stringWithFormat:@"Generate documentation for:\n```\n%@\n```", code];

    [session sendAndWait:msg timeout:30000 completion:^(NSString *content, NSError *error) {
        completion(content);
    }];
}
```

## Foreground Session

Set the foreground session hint for the server:

```objc
[client setForeground:activeSession.sessionId completion:^(NSError *error) {
    if (!error) {
        NSLog(@"Foreground session set to %@", activeSession.sessionId);
    }
}];
```

## Session Cleanup

Clean up sessions when no longer needed:

```objc
NSArray<CPCopilotSession *> *allSessions = @[codeSession, docsSession];

dispatch_group_t cleanupGroup = dispatch_group_create();
for (CPCopilotSession *session in allSessions) {
    dispatch_group_enter(cleanupGroup);
    [session disconnectWithCompletion:^(NSError *error) {
        dispatch_group_leave(cleanupGroup);
    }];
}

dispatch_group_notify(cleanupGroup, dispatch_get_main_queue(), ^{
    [client stopWithCompletion:^(NSError *error) {
        NSLog(@"All sessions cleaned up");
    }];
});
```
