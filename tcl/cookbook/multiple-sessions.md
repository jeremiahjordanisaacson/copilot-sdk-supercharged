# Multiple Sessions - Tcl

Patterns for managing multiple independent Copilot conversations simultaneously in Tcl.

## Basic Multi-Session Setup

**Scenario:** Two or more independent conversations running at the same time, each with its own context.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set code_session [::copilot::client::create_session $client \
    system_prompt "You are an expert Tcl programmer. Provide concise code examples."]

set docs_session [::copilot::client::create_session $client \
    system_prompt "You are a technical writer. Write clear, concise documentation."]

set code_resp [::copilot::client::send_and_wait $code_session \
    [dict create message "Write a proc to reverse a list in Tcl."]]
puts "Code:\n$code_resp"

set docs_resp [::copilot::client::send_and_wait $docs_session \
    [dict create message "Write docs for a list reversal proc."]]
puts "Docs:\n$docs_resp"

::copilot::client::stop $client
```

## Session Registry

**Scenario:** Manage a dynamic pool of named sessions that can be looked up, created on demand, and cleaned up together.

```tcl
package require copilot::client

namespace eval SessionRegistry {
    variable sessions [dict create]
    variable client ""
}

proc SessionRegistry::init {c} {
    variable client $c
}

proc SessionRegistry::get_or_create {name config} {
    variable sessions
    variable client
    if {![dict exists $sessions $name]} {
        set sess [::copilot::client::create_session $client {*}$config]
        dict set sessions $name $sess
        puts "Created session: $name"
    }
    return [dict get $sessions $name]
}

proc SessionRegistry::send {name message} {
    variable sessions
    if {![dict exists $sessions $name]} {
        error "No session found with name: $name"
    }
    return [::copilot::client::send_and_wait \
        [dict get $sessions $name] [dict create message $message]]
}

proc SessionRegistry::list_names {} {
    variable sessions
    return [dict keys $sessions]
}

# Usage
set client [::copilot::client::new]
::copilot::client::start $client
SessionRegistry::init $client

SessionRegistry::get_or_create "frontend" \
    [list system_prompt "You are a frontend expert."]
SessionRegistry::get_or_create "backend" \
    [list system_prompt "You are a backend expert."]

set resp [SessionRegistry::send "frontend" "How do I lazy-load images?"]
puts $resp

puts "Active sessions: [join [SessionRegistry::list_names] {, }]"

::copilot::client::stop $client
```

## Per-User Sessions

**Scenario:** In a multi-user application, each user gets their own isolated session.

```tcl
package require copilot::client

namespace eval UserSessions {
    variable client ""
    variable user_sessions [dict create]
    variable default_prompt "You are a helpful assistant."
}

proc UserSessions::init {c {prompt ""}} {
    variable client $c
    if {$prompt ne ""} {
        variable default_prompt $prompt
    }
}

proc UserSessions::chat {user_id message} {
    variable client
    variable user_sessions
    variable default_prompt

    if {![dict exists $user_sessions $user_id]} {
        set prompt "$default_prompt\nYou are assisting user: $user_id"
        set sess [::copilot::client::create_session $client \
            system_prompt $prompt]
        dict set user_sessions $user_id $sess
    }

    set sess [dict get $user_sessions $user_id]
    return [::copilot::client::send_and_wait $sess \
        [dict create message $message]]
}

proc UserSessions::end_session {user_id} {
    variable user_sessions
    if {[dict exists $user_sessions $user_id]} {
        dict unset user_sessions $user_id
    }
}

# Usage
set client [::copilot::client::new]
::copilot::client::start $client
UserSessions::init $client "You are a helpful coding assistant."

set resp1 [UserSessions::chat "alice" "How do I read a file in Tcl?"]
puts "Alice got: $resp1"

set resp2 [UserSessions::chat "bob" "How do I write to a file in Tcl?"]
puts "Bob got: $resp2"

UserSessions::end_session "alice"
::copilot::client::stop $client
```

## Round-Robin Across Sessions

**Scenario:** Distribute requests across multiple sessions to compare responses or balance load.

```tcl
package require copilot::client

namespace eval RoundRobin {
    variable sessions {}
    variable index 0
}

proc RoundRobin::init {client configs} {
    variable sessions {}
    foreach cfg $configs {
        lappend sessions [::copilot::client::create_session $client {*}$cfg]
    }
}

proc RoundRobin::send {message} {
    variable sessions
    variable index
    set index [expr {($index % [llength $sessions]) }]
    set sess [lindex $sessions $index]
    incr index
    return [::copilot::client::send_and_wait $sess \
        [dict create message $message]]
}

# Usage
set client [::copilot::client::new]
::copilot::client::start $client

RoundRobin::init $client [list \
    [list system_prompt "You are concise. Keep answers under 50 words."] \
    [list system_prompt "You are detailed. Provide thorough explanations."] \
    [list system_prompt "You are creative. Use analogies and examples."] \
]

foreach question {"What is a coroutine?" "Explain namespaces." "What is upvar?"} {
    set resp [RoundRobin::send $question]
    puts "Q: $question\nA: $resp\n"
}

::copilot::client::stop $client
```

## Best Practices

1. **One client, many sessions.** Create a single client and spawn sessions from it to share the underlying connection.
2. **Use a registry or pool pattern** to manage session lifecycles and avoid losing references.
3. **Give each session a distinct system prompt** so it stays focused on its purpose.
4. **Wrap session operations in catch/try** to isolate failures in one session from affecting others.
5. **Avoid unbounded session creation.** Track active sessions and set a maximum limit for production use.
