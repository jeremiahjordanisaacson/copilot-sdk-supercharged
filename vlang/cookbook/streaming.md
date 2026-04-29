# Streaming

Receive assistant output token-by-token as it is generated.

## Basic Streaming

Enable streaming (on by default) and handle delta events.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        streaming: true  // default, shown for clarity
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    // Print tokens as they arrive
    session.on(fn (ev copilot.SessionEvent) {
        match ev.event_type {
            'assistant.message_delta' {
                // Delta contains a chunk of text
                print(ev.raw_data)
            }
            'assistant.message' {
                // Final event -- full message is available
                println('\n--- done ---')
            }
            'assistant.reasoning_delta' {
                // Model is thinking (if reasoning is enabled)
                print('[thinking] ${ev.raw_data}')
            }
            else {}
        }
    })

    session.send(copilot.SendOptions{
        prompt: 'Write a haiku about V programming'
    }) or { panic(err) }

    // Keep the program alive to receive events
    import time
    time.sleep(10 * time.second)
}
```

## Collecting Streaming Output

Accumulate deltas into a complete string.

```v
import copilot
import sync

fn collect_response(mut session copilot.CopilotSession, prompt string) !string {
    mut result := shared string('')
    done := chan bool{cap: 1}

    unsub := session.on(fn [mut result, done] (ev copilot.SessionEvent) {
        match ev.event_type {
            'assistant.message_delta' {
                lock result {
                    result = result + ev.raw_data
                }
            }
            'assistant.message' {
                done <- true
            }
            else {}
        }
    })
    defer { unsub() }

    session.send(copilot.SendOptions{ prompt: prompt })!

    // Wait for completion
    _ = <-done

    rlock result {
        return result
    }
}
```

## Streaming with Progress Indicator

Show a spinner while the model is generating.

```v
import copilot
import time

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    mut token_count := 0
    done := chan bool{cap: 1}

    session.on(fn [mut token_count, done] (ev copilot.SessionEvent) {
        match ev.event_type {
            'assistant.message_delta' {
                token_count++
                print(ev.raw_data)
            }
            'assistant.message' {
                done <- true
            }
            else {}
        }
    })

    print('Assistant: ')
    session.send(copilot.SendOptions{
        prompt: 'List 5 features of V language'
    }) or { panic(err) }

    _ = <-done
    println('\n(${token_count} tokens received)')
}
```
