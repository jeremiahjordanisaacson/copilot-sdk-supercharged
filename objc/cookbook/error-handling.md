# Error Handling

Robust error handling patterns for the Copilot Objective-C SDK.

## Basic Error Handling

All async SDK methods deliver errors via completion blocks:

```objc
[client startWithCompletion:^(NSError *error) {
    if (error) {
        NSLog(@"Domain: %@", error.domain);
        NSLog(@"Code: %ld", (long)error.code);
        NSLog(@"Message: %@", error.localizedDescription);
        return;
    }
    // Proceed...
}];
```

## Error Domains

| Domain | Description |
|--------|-------------|
| `CPCopilotClientErrorDomain` | Client lifecycle errors (start, stop, spawn) |
| `CPCopilotSessionErrorDomain` | Session-level errors (timeout, disconnect) |
| `CPJsonRpcErrorDomain` | JSON-RPC transport errors (parse, protocol) |

## Retry Pattern

```objc
- (void)startClientWithRetries:(CPCopilotClient *)client
                    maxRetries:(NSInteger)maxRetries
                    completion:(void (^)(NSError *))completion {
    __block NSInteger attempts = 0;

    void (^__block tryStart)(void) = ^{
        attempts++;
        [client startWithCompletion:^(NSError *error) {
            if (!error) {
                completion(nil);
                return;
            }

            if (attempts < maxRetries) {
                NSLog(@"Start attempt %ld failed, retrying...", (long)attempts);
                dispatch_after(
                    dispatch_time(DISPATCH_TIME_NOW,
                                  (int64_t)(pow(2, attempts) * NSEC_PER_SEC)),
                    dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                    tryStart
                );
            } else {
                completion(error);
            }
        }];
    };

    tryStart();
}
```

## Session Error Events

Listen for session-level errors alongside the completion block:

```objc
[session onEventType:@"session.error" handler:^(CPSessionEvent *event) {
    NSLog(@"Session error: %@", event.message);
}];
```

## Timeout Handling

`sendAndWait:timeout:completion:` returns a timeout error if the session
does not become idle within the specified duration:

```objc
CPMessageOptions *msg = [[CPMessageOptions alloc] init];
msg.prompt = @"Complex question...";

[session sendAndWait:msg timeout:60000 completion:^(NSString *content, NSError *error) {
    if (error && error.code == -5) {
        NSLog(@"Timed out waiting for response");
        [session abortWithCompletion:^(NSError *abortErr) {
            // Message processing aborted
        }];
        return;
    }
    // Handle response...
}];
```

## Connection State Monitoring

Check the client connection state before operations:

```objc
if (client.connectionState != CPConnectionStateConnected) {
    NSLog(@"Client not connected (state: %ld)", (long)client.connectionState);
    [client startWithCompletion:^(NSError *error) {
        // Re-attempt operation...
    }];
    return;
}
```
