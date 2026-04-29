# Persisting Sessions - Tcl

Patterns for saving and resuming Copilot sessions across application restarts in Tcl.

## Basic Save and Load with JSON

**Scenario:** Save session state to a JSON file so it can be restored later.

```tcl
package require copilot::client
package require copilot::jsonrpc

proc save_session_state {session filepath} {
    set state [::copilot::session::get_state $session]
    set json [::copilot::jsonrpc::json_encode $state]
    set fd [open $filepath w]
    puts -nonewline $fd $json
    close $fd
    puts "Session saved to $filepath"
}

proc load_session_state {filepath} {
    if {![file exists $filepath]} {
        return ""
    }
    set fd [open $filepath r]
    set content [read $fd]
    close $fd
    return [::copilot::jsonrpc::json_decode $content]
}

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant."]

::copilot::client::send_and_wait $session \
    [dict create message "Remember that my name is Alice."]

save_session_state $session "session_state.json"
::copilot::client::stop $client
```

## Resuming a Saved Session

**Scenario:** Restore a previously saved session and continue the conversation.

```tcl
package require copilot::client
package require copilot::jsonrpc

proc load_session_state {filepath} {
    if {![file exists $filepath]} { return "" }
    set fd [open $filepath r]
    set content [read $fd]
    close $fd
    return [::copilot::jsonrpc::json_decode $content]
}

set client [::copilot::client::new]
::copilot::client::start $client

set saved [load_session_state "session_state.json"]

if {$saved ne ""} {
    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant." \
        state $saved]
    puts "Resumed existing session"
} else {
    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant."]
    puts "Started new session"
}

set response [::copilot::client::send_and_wait $session \
    [dict create message "What is my name?"]]
puts "Response: $response"

::copilot::client::stop $client
```

## Session Store with Multiple Named Sessions

**Scenario:** Persist multiple named sessions to disk, each in its own file.

```tcl
package require copilot::client
package require copilot::jsonrpc

namespace eval SessionStore {
    variable directory ""
}

proc SessionStore::init {dir} {
    variable directory $dir
    file mkdir $dir
}

proc SessionStore::filepath {name} {
    variable directory
    return [file join $directory "${name}.json"]
}

proc SessionStore::save {name session} {
    set state [::copilot::session::get_state $session]
    set json [::copilot::jsonrpc::json_encode $state]
    set fd [open [filepath $name] w]
    puts -nonewline $fd $json
    close $fd
}

proc SessionStore::load {name} {
    set path [filepath $name]
    if {![file exists $path]} { return "" }
    set fd [open $path r]
    set content [read $fd]
    close $fd
    return [::copilot::jsonrpc::json_decode $content]
}

proc SessionStore::delete {name} {
    file delete -force [filepath $name]
}

proc SessionStore::list_sessions {} {
    variable directory
    set names {}
    foreach f [glob -nocomplain -directory $directory *.json] {
        lappend names [file rootname [file tail $f]]
    }
    return $names
}

# Usage
set client [::copilot::client::new]
::copilot::client::start $client
SessionStore::init "./sessions"

set session [::copilot::client::create_session $client \
    system_prompt "You are a project planning assistant."]

::copilot::client::send_and_wait $session \
    [dict create message "We are building a REST API in Tcl."]

SessionStore::save "project-alpha" $session

# List all saved sessions
puts "Saved: [join [SessionStore::list_sessions] {, }]"

::copilot::client::stop $client
```

## Auto-Save on Every Turn

**Scenario:** Automatically save session state after every message exchange.

```tcl
package require copilot::client
package require copilot::jsonrpc

proc auto_saving_send {session opts save_path} {
    set response [::copilot::client::send_and_wait $session $opts]

    set state [::copilot::session::get_state $session]
    set json [::copilot::jsonrpc::json_encode $state]
    set fd [open $save_path w]
    puts -nonewline $fd $json
    close $fd

    return $response
}

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant."]

set save_path "./current_session.json"

auto_saving_send $session [dict create message "Hello!"] $save_path
auto_saving_send $session [dict create message "What can you help with?"] $save_path

puts "Session auto-saved to $save_path"
::copilot::client::stop $client
```

## Session State with Metadata

**Scenario:** Save extra metadata alongside the session state.

```tcl
package require copilot::client
package require copilot::jsonrpc

proc save_with_metadata {session filepath metadata} {
    set envelope [dict create \
        metadata  $metadata \
        state     [::copilot::session::get_state $session] \
        saved_at  [clock format [clock seconds] -format "%Y-%m-%dT%H:%M:%SZ" -gmt 1] \
    ]
    set fd [open $filepath w]
    puts -nonewline $fd [::copilot::jsonrpc::json_encode $envelope]
    close $fd
}

proc load_with_metadata {filepath} {
    if {![file exists $filepath]} { return "" }
    set fd [open $filepath r]
    set content [read $fd]
    close $fd
    return [::copilot::jsonrpc::json_decode $content]
}

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant."]

::copilot::client::send_and_wait $session [dict create message "Hello!"]

save_with_metadata $session "session_with_meta.json" [dict create \
    description "Test session for cookbook example" \
    created_by  "cookbook" \
    turn_count  1 \
]

set envelope [load_with_metadata "session_with_meta.json"]
if {$envelope ne ""} {
    puts "Saved at: [dict get $envelope saved_at]"
    puts "Description: [dict get [dict get $envelope metadata] description]"
}

::copilot::client::stop $client
```

## Best Practices

1. **Use the built-in JSON encoder** from `copilot::jsonrpc` or a Tcllib JSON package for serialization.
2. **Save after every meaningful exchange** to minimize data loss on crashes.
3. **Store metadata** (timestamps, descriptions, turn counts) alongside session state.
4. **Use a directory-based store** for multi-session applications.
5. **Handle missing files gracefully** by returning empty string from load procs.
6. **Clean up old session files** periodically using `file delete`.
