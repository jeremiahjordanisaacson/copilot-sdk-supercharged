# Error Handling - Tcl

Patterns for handling errors gracefully in the Copilot SDK for Tcl, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with catch

**Scenario:** Catch errors from SDK calls without crashing your application.

```tcl
lappend auto_path [file join [file dirname [info script]] .. lib]
package require copilot::client

set client [::copilot::client::new]

if {[catch {::copilot::client::start $client} err]} {
    puts stderr "Failed to start client: $err"
    exit 1
}

if {[catch {
    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant."]
} err]} {
    puts stderr "Session creation failed: $err"
    ::copilot::client::stop $client
    exit 1
}
```

## Extended Error Handling with try/on

**Scenario:** Use Tcl 8.6 try/on/finally for structured error handling with guaranteed cleanup.

```tcl
package require copilot::client

set client [::copilot::client::new]

try {
    ::copilot::client::start $client

    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant."]

    set response [::copilot::client::send_and_wait $session \
        [dict create message "Hello!"]]

    puts "Response: $response"

} on error {err opts} {
    puts stderr "Error: $err"
    puts stderr "Stack: [dict get $opts -errorinfo]"

} finally {
    catch {::copilot::client::stop $client}
}
```

## Retry Logic

**Scenario:** Transient failures should be retried automatically with delay.

```tcl
package require copilot::client

proc retry {body max_attempts delay_ms} {
    set last_err ""
    for {set attempt 1} {$attempt <= $max_attempts} {incr attempt} {
        if {![catch {uplevel 1 $body} result]} {
            return $result
        }
        set last_err $result
        if {$attempt < $max_attempts} {
            puts "Attempt $attempt/$max_attempts failed: $last_err. Retrying..."
            after $delay_ms
        }
    }
    error "All $max_attempts attempts failed. Last error: $last_err"
}

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant."]

set response [retry {
    ::copilot::client::send_and_wait $session \
        [dict create message "Summarize the benefits of Tcl."]
} 3 2000]

puts "Response: $response"
::copilot::client::stop $client
```

## Cleanup with a Finally Pattern

**Scenario:** Ensure the client is always stopped, even if an error occurs mid-session.

```tcl
package require copilot::client

proc with_client {opts body} {
    set client [::copilot::client::new {*}$opts]
    ::copilot::client::start $client

    try {
        uplevel 1 [list set _client $client]
        uplevel 1 $body
    } finally {
        catch {::copilot::client::stop $client}
    }
}

with_client {} {
    set session [::copilot::client::create_session $_client \
        system_prompt "You are a helpful assistant."]

    set response [::copilot::client::send_and_wait $session \
        [dict create message "What is error handling?"]]

    puts "Response: $response"
}
```

## Handling Specific Error Types

**Scenario:** React differently to connection errors, timeouts, and auth errors.

```tcl
package require copilot::client

proc classify_error {err_msg} {
    set msg [string tolower $err_msg]
    if {[string match "*timeout*" $msg]} { return "timeout" }
    if {[string match "*connection*" $msg] || [string match "*refused*" $msg]} { return "connection" }
    if {[string match "*auth*" $msg] || [string match "*token*" $msg]} { return "auth" }
    return "unknown"
}

set client [::copilot::client::new]

if {[catch {::copilot::client::start $client} err]} {
    switch -- [classify_error $err] {
        "connection" { puts "Cannot connect to Copilot CLI. Is it installed?" }
        "auth"       { puts "Authentication error. Check your credentials." }
        "timeout"    { puts "Connection timed out. Try again later." }
        default      { puts "Unexpected error: $err" }
    }
    exit 1
}

::copilot::client::stop $client
```

## Best Practices

1. **Use try/on/finally** (Tcl 8.6+) for structured error handling with guaranteed cleanup.
2. **Use catch** for simpler one-off error checks where try/finally is overkill.
3. **Implement a cleanup wrapper** (like `with_client`) to guarantee resource release.
4. **Retry transient errors** with configurable delay for production systems.
5. **Classify errors** by pattern matching on the error message to take targeted recovery actions.
6. **Log errors to stderr** using `puts stderr` to keep stdout clean for program output.
