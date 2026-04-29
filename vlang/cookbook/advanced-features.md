# Advanced Features

Agents, custom system prompts, attachments, and other advanced patterns.

## Custom System Prompt

Set a system message to shape the model's behaviour.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        system_message: 'You are a senior V developer. Answer only in V code with comments. Be concise.'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'Write a concurrent web scraper'
    }, 60_000) or { panic(err) }
    println(result)
}
```

## File Attachments

Attach file content for the model to reference.

```v
import copilot
import os

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    // Read a local file to attach
    source_code := os.read_file('src/client.v') or {
        eprintln('Cannot read file: ${err}')
        exit(1)
    }

    mut session := client.create_session(copilot.SessionConfig{
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'Review this code for bugs and suggest improvements.'
        attachments: [
            copilot.Attachment{
                file_path: 'src/client.v'
                language: 'vlang'
                content: source_code
            },
        ]
    }, 60_000) or { panic(err) }
    println(result)
}
```

## Using Agents

Specify an agent name for scoped behaviour.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        agent: 'code-reviewer'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'Analyze my project for security issues'
    }, 120_000) or { panic(err) }
    println(result)
}
```

## with_session Helper

Use the convenience wrapper for automatic cleanup.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    client.with_session(copilot.SessionConfig{
        model: 'gpt-4'
        on_permission_request: copilot.approve_all
    }, fn (mut session copilot.CopilotSession) ! {
        result := session.send_and_wait(copilot.SendOptions{
            prompt: 'What are V closures?'
        }, 30_000)!
        println(result)
    }) or { panic(err) }
    // Session is automatically disconnected here
}
```

## Event Filtering

Subscribe to specific event types only.

```v
import copilot

fn on_messages_only(handler fn (string)) fn (copilot.SessionEvent) {
    return fn [handler] (ev copilot.SessionEvent) {
        if ev.event_type == 'assistant.message' {
            handler(ev.raw_data)
        }
    }
}

fn main() {
    // Use the wrapper to only handle final messages:
    // session.on(on_messages_only(fn (content string) { println(content) }))
}
```
