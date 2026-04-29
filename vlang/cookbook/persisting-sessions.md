# Persisting Sessions

Resume conversations across program restarts by saving and restoring session IDs.

## Save and Restore a Session ID

```v
import copilot
import os

const session_file = '.copilot_session_id'

fn save_session_id(id string) {
    os.write_file(session_file, id) or {
        eprintln('Could not save session ID: ${err}')
    }
}

fn load_session_id() ?string {
    content := os.read_file(session_file) or { return none }
    trimmed := content.trim_space()
    if trimmed.len == 0 {
        return none
    }
    return trimmed
}

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    // Check for a previously saved session
    prev_id := load_session_id()
    if prev_id != none {
        println('Found previous session: ${prev_id}')
        // The CLI server may support resuming -- attempt it
    }

    mut session := client.create_session(copilot.SessionConfig{
        model: 'gpt-4'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    // Persist the new session ID
    save_session_id(session.id())
    println('Session ID: ${session.id()}')

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'Remember: my favourite language is V.'
    }, 30_000) or { panic(err) }
    println(result)
}
```

## Session History Seeding

Pre-fill a session with conversation history.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    // Seed with prior conversation turns
    history := [
        copilot.HistoryEntry{ role: 'user', content: 'My name is Alice.' },
        copilot.HistoryEntry{ role: 'assistant', content: 'Nice to meet you, Alice!' },
        copilot.HistoryEntry{ role: 'user', content: 'I work on compilers.' },
        copilot.HistoryEntry{ role: 'assistant', content: 'That is fascinating work!' },
    ]

    mut session := client.create_session(copilot.SessionConfig{
        history: history
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    // The model now has context from the seeded history
    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'What is my name and what do I work on?'
    }, 30_000) or { panic(err) }
    println(result)
    // Expected: something like "Your name is Alice and you work on compilers."
}
```

## Cleanup Old Sessions

Remove stale session files on startup.

```v
import os

fn cleanup_stale_sessions(dir string, max_age_hours int) {
    files := os.ls(dir) or { return }
    now := os.now()
    for file in files {
        path := os.join_path(dir, file)
        info := os.stat(path) or { continue }
        age_hours := (now - info.mtime) / 3600
        if age_hours > max_age_hours {
            os.rm(path) or {}
            println('Removed stale session file: ${file}')
        }
    }
}
```
