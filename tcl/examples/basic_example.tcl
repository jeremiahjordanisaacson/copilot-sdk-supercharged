#!/usr/bin/env tclsh
# basic_example.tcl - Basic usage of the Copilot SDK for Tcl
#
# This example shows how to create a client, start a session, send a message,
# and clean up. Run with: tclsh basic_example.tcl
#
# Copyright (c) Microsoft Corporation. All rights reserved.

# Add the SDK library to the package search path
set script_dir [file dirname [file normalize [info script]]]
lappend auto_path [file join $script_dir .. lib]

package require copilot::client

# Create a new client (uses copilot CLI on PATH by default)
set client [::copilot::client::new]

# Start the client (spawns the CLI process)
puts "Starting Copilot client..."
::copilot::client::start $client

# Create a session with a system prompt
set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant. Be concise."]

puts "Session created."

# Register an event listener
::copilot::session::on_event $session [list apply {{event} {
    set type [dict get $event type]
    puts "Event: $type"
}}]

# Send a message and wait for the response
puts "Sending message..."
set response [::copilot::client::send_and_wait $session \
    [dict create message "What is 2+2? Reply with just the number."]]

if {[dict exists $response content]} {
    puts "Response: [dict get $response content]"
} elseif {[dict exists $response message]} {
    puts "Response: [dict get $response message]"
} else {
    puts "Response: $response"
}

# Clean up
::copilot::client::destroy_session $client $session
::copilot::client::stop $client
puts "Done."
