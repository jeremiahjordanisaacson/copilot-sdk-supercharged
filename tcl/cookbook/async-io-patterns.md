# Async I/O Patterns - Tcl

Patterns for non-blocking I/O with fileevent and coroutines in the Copilot SDK for Tcl.

## Non-Blocking Reads with fileevent

**Scenario:** Use Tcl's event loop and `fileevent` to process server messages without blocking the main thread.

```tcl
package require copilot::client
package require copilot::jsonrpc

# After starting the client, configure the read channel for non-blocking I/O
# and use fileevent to react to incoming data.

proc on_readable {read_ch} {
    if {[eof $read_ch]} {
        puts "Connection closed."
        fileevent $read_ch readable {}
        return
    }

    set msg [::copilot::jsonrpc::read_message $read_ch]
    if {$msg ne ""} {
        puts "Received: $msg"
    }
}

set client [::copilot::client::new]
::copilot::client::start $client

# Note: This pattern is useful when you want to integrate SDK message
# handling into a Tk GUI event loop or a custom event-driven server.
# The SDK's synchronous send_and_wait handles its own read loop, so
# fileevent is primarily for advanced use cases.

::copilot::client::stop $client
```

## Coroutine-Based Session (Tcl 8.6+)

**Scenario:** Use a coroutine to yield control while waiting for responses, enabling cooperative multitasking.

```tcl
package require copilot::client

proc chat_coro {client prompts} {
    set session [::copilot::client::create_session $client \
        system_prompt "You are a helpful assistant."]

    foreach prompt $prompts {
        puts "Sending: $prompt"
        set response [::copilot::client::send_and_wait $session \
            [dict create message $prompt]]
        puts "Got: $response"
        yield $response
    }

    return "done"
}

set client [::copilot::client::new]
::copilot::client::start $client

set prompts [list "Hello!" "What is Tcl?" "Thanks, goodbye."]

# Create the coroutine
coroutine my_chat chat_coro $client $prompts

# Drive it step by step
while {![info commands my_chat] eq ""} {
    if {[catch {my_chat} result]} {
        break
    }
    if {$result eq "done"} {
        break
    }
    puts "Yielded response received in main loop."
}

::copilot::client::stop $client
```

## Timer-Based Polling with after

**Scenario:** Poll for work items on a schedule using Tcl's `after` command.

```tcl
package require copilot::client

proc poll_loop {client session interval_ms} {
    set response [::copilot::client::send_and_wait $session \
        [dict create message "Any new tasks for me?"]]
    puts "[clock format [clock seconds]]: $response"

    # Schedule the next poll
    after $interval_ms [list poll_loop $client $session $interval_ms]
}

set client [::copilot::client::new]
::copilot::client::start $client

set session [::copilot::client::create_session $client \
    system_prompt "You are a task manager. Report any pending items."]

# Poll every 60 seconds
after 0 [list poll_loop $client $session 60000]

# Enter the event loop (in a Tk app this happens automatically)
# vwait ::forever

# For non-Tk scripts, run for a limited time then clean up:
after 180000 {set ::done 1}
vwait ::done

::copilot::client::stop $client
```

## Parallel Session Queries with Coroutines

**Scenario:** Fire off multiple session queries concurrently using coroutines and collect results.

```tcl
package require copilot::client

proc parallel_query {client sessions questions} {
    set results [dict create]

    # Launch coroutines for each query
    set coro_names {}
    for {set i 0} {$i < [llength $sessions]} {incr i} {
        set sess [lindex $sessions $i]
        set q [lindex $questions $i]
        set coro_name "query_coro_$i"
        lappend coro_names $coro_name

        coroutine $coro_name apply [list {sess q idx results_var} {
            upvar #0 $results_var res
            set response [::copilot::client::send_and_wait $sess \
                [dict create message $q]]
            dict set res $idx $response
        }] $sess $q $i "::parallel_results"
    }

    return $::parallel_results
}

set client [::copilot::client::new]
::copilot::client::start $client

set ::parallel_results [dict create]

set s1 [::copilot::client::create_session $client system_prompt "Be brief."]
set s2 [::copilot::client::create_session $client system_prompt "Be detailed."]

set results [parallel_query $client [list $s1 $s2] \
    [list "What is Tcl?" "What is Tcl?"]]

dict for {idx resp} $results {
    puts "Session $idx: $resp"
}

::copilot::client::stop $client
```

## Best Practices

1. **Use fileevent for GUI integration** (Tk) where blocking calls freeze the UI.
2. **Use coroutines for cooperative multitasking** when you need interleaved session work.
3. **Use after for scheduled polling** instead of busy-wait loops.
4. **Always enter the event loop** (via `vwait` or Tk's mainloop) when using `after` or `fileevent`.
5. **Keep coroutines lightweight** -- avoid deep call stacks inside yielded coroutines.
6. **Configure channels as non-blocking** (`fconfigure $ch -blocking 0`) before using fileevent.
