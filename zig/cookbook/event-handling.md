# Event Handling

Subscribe to session events for real-time notifications.

## Event Types

| Event Type | Description |
|---|---|
| `session.start` | Session started |
| `session.resume` | Session resumed |
| `session.idle` | Session finished processing |
| `session.error` | Error occurred |
| `session.info` | Informational message |
| `session.model_change` | Model changed |
| `session.truncation` | Context truncated |
| `session.compaction_start` | Context compaction started |
| `session.compaction_complete` | Context compaction finished |
| `user.message` | User message sent |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `assistant.turn_start` | Assistant turn started |
| `assistant.turn_end` | Assistant turn ended |
| `assistant.usage` | Token usage information |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution finished |
| `tool.execution_progress` | Tool execution progress |
| `abort` | Session aborted |

## Subscribe to All Events

```zig
const sub = session.on(struct {
    fn handler(event: copilot.SessionEvent) void {
        std.debug.print("[{s}] {s}\n", .{ event.event_type, event.id });
    }
}.handler);
```

## Subscribe to a Specific Event Type

```zig
const sub = session.onEvent("assistant.message", struct {
    fn handler(event: copilot.SessionEvent) void {
        if (event.assistantMessageContent()) |content| {
            std.debug.print("Message: {s}\n", .{content});
        }
    }
}.handler);
```

## Unsubscribe

Subscriptions return an `EventSubscription` handle:

```zig
const sub = session.on(myHandler);

// Later, remove the subscription:
sub.unsubscribe();
```

## Lifecycle Events

Monitor client connection state changes:

```zig
client.onLifecycle(struct {
    fn handler(state: copilot.ConnectionState) void {
        std.debug.print("Connection: {any}\n", .{state});
    }
}.handler);
```
