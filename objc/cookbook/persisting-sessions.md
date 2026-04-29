# Persisting Sessions

Save and resume conversations across app launches.

## How Infinite Sessions Work

Infinite sessions are enabled by default. The Copilot CLI automatically persists
session state to `~/.copilot/session-state/{sessionId}`. When you resume a session,
the full conversation context is restored.

## Saving Session IDs

Store the session ID so you can resume later:

```objc
// After creating a session, save its ID
[client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
    if (!error) {
        [[NSUserDefaults standardUserDefaults] setObject:session.sessionId
                                                 forKey:@"lastSessionId"];
        NSLog(@"Session saved: %@", session.sessionId);
    }
}];
```

## Resuming a Session

```objc
NSString *savedId = [[NSUserDefaults standardUserDefaults] stringForKey:@"lastSessionId"];

if (savedId) {
    [client resumeSession:savedId config:nil completion:^(CPCopilotSession *session, NSError *error) {
        if (error) {
            NSLog(@"Could not resume session, creating new one...");
            [client createSession:nil completion:^(CPCopilotSession *session, NSError *error) {
                // Use new session
            }];
            return;
        }
        NSLog(@"Resumed session: %@", session.sessionId);
    }];
} else {
    [client createSession:nil completion:^(CPCopilotSession *session, NSError *error) {
        // First launch - new session
    }];
}
```

## Listing Available Sessions

```objc
[client listSessionsWithCompletion:^(NSArray<CPSessionMetadata *> *sessions, NSError *error) {
    if (error) {
        NSLog(@"Failed to list sessions: %@", error);
        return;
    }

    for (CPSessionMetadata *meta in sessions) {
        NSLog(@"Session: %@", meta.sessionId);
        NSLog(@"  Started:  %@", meta.startTime);
        NSLog(@"  Modified: %@", meta.modifiedTime);
        NSLog(@"  Summary:  %@", meta.summary ?: @"(none)");
        NSLog(@"  Remote:   %@", meta.isRemote ? @"YES" : @"NO");
    }
}];
```

## Deleting Old Sessions

```objc
[client deleteSession:@"old-session-id" completion:^(NSError *error) {
    if (!error) {
        NSLog(@"Session deleted");
    }
}];
```

## Workspace Path

When infinite sessions are enabled, a workspace path is available:

```objc
[client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
    if (session.workspacePath) {
        NSLog(@"Workspace path: %@", session.workspacePath);
        // Use for reading/writing session-scoped files
    }
}];
```

## Compaction Events

Monitor compaction events for infinite sessions:

```objc
[session onEventType:@"session.compaction_start" handler:^(CPSessionEvent *event) {
    NSLog(@"Session compaction started...");
}];

[session onEventType:@"session.compaction_complete" handler:^(CPSessionEvent *event) {
    NSLog(@"Session compaction complete");
}];
```
