# Multiple Sessions

Run parallel conversations using the same client.

## Two Independent Sessions

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    // Session 1: code review assistant
    mut session1 := client.create_session(copilot.SessionConfig{
        model: 'gpt-4'
        system_message: 'You are a code review assistant. Be concise.'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session1.disconnect() or {} }

    // Session 2: documentation writer
    mut session2 := client.create_session(copilot.SessionConfig{
        model: 'gpt-4'
        system_message: 'You are a documentation writer. Be thorough.'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session2.disconnect() or {} }

    // Send to both in parallel using spawn
    ch1 := chan string{cap: 1}
    ch2 := chan string{cap: 1}

    spawn fn [mut session1, ch1] () {
        result := session1.send_and_wait(copilot.SendOptions{
            prompt: 'Review this: fn add(a int, b int) int { return a + b }'
        }, 30_000) or {
            ch1 <- 'Error: ${err}'
            return
        }
        ch1 <- result
    }()

    spawn fn [mut session2, ch2] () {
        result := session2.send_and_wait(copilot.SendOptions{
            prompt: 'Write docs for: fn add(a int, b int) int'
        }, 30_000) or {
            ch2 <- 'Error: ${err}'
            return
        }
        ch2 <- result
    }()

    review := <-ch1
    docs := <-ch2

    println('=== Code Review ===')
    println(review)
    println('')
    println('=== Documentation ===')
    println(docs)
}
```

## Session Pool Pattern

Manage a pool of sessions for high-throughput workloads.

```v
import copilot

struct SessionPool {
mut:
    sessions []&copilot.CopilotSession
    index    int
}

fn new_pool(mut client copilot.CopilotClient, size int) !SessionPool {
    mut sessions := []&copilot.CopilotSession{cap: size}
    for _ in 0 .. size {
        s := client.create_session(copilot.SessionConfig{
            on_permission_request: copilot.approve_all
        })!
        sessions << s
    }
    return SessionPool{ sessions: sessions }
}

fn (mut p SessionPool) next() &copilot.CopilotSession {
    s := p.sessions[p.index % p.sessions.len]
    p.index++
    return s
}

fn (mut p SessionPool) close() {
    for mut s in p.sessions {
        s.disconnect() or {}
    }
}
```
