# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in V.

## Basic Error Handling

Use `!` result types and `or` blocks for clean error propagation.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{})
    client.start() or {
        eprintln('startup failed: ${err}')
        exit(1)
    }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        model: 'gpt-4'
        on_permission_request: copilot.approve_all
    }) or {
        eprintln('session creation failed: ${err}')
        exit(1)
    }
    defer { session.disconnect() or {} }

    response := session.send_and_wait(copilot.SendOptions{
        prompt: 'Explain V error handling'
    }, 30_000) or {
        eprintln('send failed: ${err}')
        exit(1)
    }

    println(response)
}
```

## Matching on Error Kinds

Use the `CopilotError` struct to inspect failure categories.

```v
import copilot

fn safe_send(mut session copilot.CopilotSession, prompt string) string {
    return session.send_and_wait(copilot.SendOptions{
        prompt: prompt
    }, 30_000) or {
        // Log and return a fallback
        eprintln('Error: ${err}')
        return '[no response]'
    }
}
```

## Retry with Backoff

Wrap operations in a retry loop for transient failures.

```v
import copilot
import time

fn send_with_retry(mut session copilot.CopilotSession, prompt string, max_retries int) !string {
    mut last_err := ''
    for attempt in 0 .. max_retries {
        result := session.send_and_wait(copilot.SendOptions{
            prompt: prompt
        }, 30_000) or {
            last_err = err.msg()
            wait_ms := 500 * (attempt + 1)
            eprintln('Attempt ${attempt + 1} failed, retrying in ${wait_ms}ms...')
            time.sleep(wait_ms * time.millisecond)
            continue
        }
        return result
    }
    return error('all ${max_retries} attempts failed. Last error: ${last_err}')
}
```

## Graceful Client Shutdown

Use `defer` for cleanup to ensure resources are freed.

```v
import copilot

fn run_chat() ! {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start()!
    defer { client.stop() or { eprintln('stop error: ${err}') } }

    mut session := client.create_session(copilot.SessionConfig{})!
    defer { session.disconnect() or { eprintln('disconnect error: ${err}') } }

    response := session.send_and_wait(copilot.SendOptions{
        prompt: 'Hello'
    }, 10_000)!
    println(response)
}

fn main() {
    run_chat() or {
        eprintln('Fatal: ${err}')
        exit(1)
    }
}
```
