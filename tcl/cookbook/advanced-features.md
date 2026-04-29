# Advanced v2.0 Features - Tcl

Recipes for v2.0 SDK features in Tcl: per-session auth, idle timeouts, system prompts, streaming, and model selection.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth instead of a global token.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set session_a [::copilot::client::create_session $client \
    github_token $::env(GITHUB_TOKEN_USER_A) \
    system_prompt "You are a helpful assistant."]

set resp [::copilot::client::send_and_wait $session_a \
    [dict create message "Summarize my recent pull requests."]]
puts "User A response: $resp"

set session_b [::copilot::client::create_session $client \
    github_token $::env(GITHUB_TOKEN_USER_B) \
    system_prompt "You are a code reviewer."]

::copilot::client::stop $client
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    session_idle_timeout_seconds 300 \
    system_prompt "You are a helpful assistant."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "Hello!"]]

puts "Response: $response"

# Session automatically expires after 300s of inactivity.
::copilot::client::stop $client
```

## Streaming Responses

**Scenario:** Enable streaming to receive incremental response deltas as they arrive.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    streaming 1 \
    system_prompt "You are a helpful assistant."]

# Register a listener for streaming events
::copilot::session::on_event $session [list apply {{event} {
    set type [dict get $event type]
    if {$type eq "assistant.message_delta"} {
        set data [dict get $event data]
        if {[dict exists $data content]} {
            puts -nonewline [dict get $data content]
        }
    } elseif {$type eq "response"} {
        puts ""
        puts "--- Final response received ---"
    }
}}]

set response [::copilot::client::send_and_wait $session \
    [dict create message "Tell me a short story about a Tcl programmer."]]

::copilot::client::stop $client
```

## Model Selection

**Scenario:** Choose a specific model for a session.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    model "gpt-4o" \
    system_prompt "You are a helpful assistant."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "Explain Tcl namespaces in 3 sentences."]]

puts "Response: $response"
::copilot::client::stop $client
```

## Connecting to an External Server

**Scenario:** Connect to a remote Copilot CLI server instead of spawning a local process.

```tcl
package require copilot::client

set client [::copilot::client::new cli_url "http://localhost:3000"]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "Hello from a remote connection!"]]

puts "Response: $response"
::copilot::client::stop $client
```

## Checking Server Status

**Scenario:** Query the server status and auth state before starting a session.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set status [::copilot::client::get_status $client]
puts "Server status: $status"

set auth [::copilot::client::get_auth_status $client]
puts "Auth status: $auth"

if {[dict exists $auth authenticated] && [dict get $auth authenticated] eq "true"} {
    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant."]
    set response [::copilot::client::send_and_wait $session \
        [dict create message "Hello!"]]
    puts "Response: $response"
} else {
    puts "Not authenticated. Please sign in."
}

::copilot::client::stop $client
```

## Best Practices

1. **Use per-session tokens** for multi-user applications to scope access correctly.
2. **Set idle timeouts** for long-running applications to free resources automatically.
3. **Enable streaming** for interactive UIs where users expect incremental output.
4. **Check status before creating sessions** to catch connection and auth issues early.
5. **Use cli_url** for shared server deployments instead of spawning one CLI per client.
