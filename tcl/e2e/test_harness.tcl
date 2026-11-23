# test_harness.tcl - E2E test harness for the Tcl Copilot SDK.
#
# Spawns the shared replay proxy (npx tsx test/harness/server.ts),
# parses the `Listening: http://...` URL from stdout, and provides
# helpers for E2E tests.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require http

namespace eval ::e2e::harness {
    variable proxy_pid ""
    variable proxy_url ""
    variable proxy_chan ""
    # Directory of this harness (captured while the file is being sourced)
    variable harness_dir [file dirname [file normalize [info script]]]
}

# Start the replay proxy and return the listening URL.
proc ::e2e::harness::start_proxy {} {
    variable proxy_pid
    variable proxy_url
    variable proxy_chan

    # Path from tcl/e2e to repo-root/test/harness/server.ts
    set script_dir [file dirname [info script]]
    set server_path [file normalize [file join $script_dir .. .. test harness server.ts]]
    set cwd [file normalize [file join $script_dir .. .. test harness]]

    # Spawn the proxy
    set proxy_chan [open "|npx tsx $server_path 2>@stderr" r]

    # Read the first line to get the URL
    gets $proxy_chan line

    if {[regexp {Listening: (http://[^ \r\n]+)} $line -> url]} {
        set proxy_url $url
    } else {
        catch {close $proxy_chan}
        error "Could not parse proxy URL from: $line"
    }

    # Set environment variable
    set ::env(COPILOT_API_URL) $proxy_url

    puts "\[tcl-e2e\] Proxy listening at $proxy_url"
    return $proxy_url
}

# Stop the replay proxy.
proc ::e2e::harness::stop_proxy {} {
    variable proxy_chan
    catch {close $proxy_chan}
}

# Return the proxy URL.
proc ::e2e::harness::url {} {
    variable proxy_url
    return $proxy_url
}

# Locate the Copilot CLI for E2E tests: COPILOT_CLI_PATH (when the file exists),
# then the CLI 1.0.113+ npm-loader.js entry point, then the legacy index.js entry
# points under nodejs/node_modules/@github. The client runs a .js path with node
# and executes any other path (such as node_modules/.bin/copilot) directly.
proc ::e2e::harness::cli_path {} {
    variable harness_dir
    if {[info exists ::env(COPILOT_CLI_PATH)] && [file exists $::env(COPILOT_CLI_PATH)]} {
        return $::env(COPILOT_CLI_PATH)
    }
    set github_dir [file normalize [file join $harness_dir .. .. nodejs node_modules @github]]
    set candidates [list [file join $github_dir copilot npm-loader.js]]
    foreach legacy [lsort [glob -nocomplain -directory $github_dir copilot-*]] {
        lappend candidates [file join $legacy index.js]
    }
    lappend candidates [file join $github_dir copilot index.js]
    foreach candidate $candidates {
        if {[file exists $candidate]} {
            return $candidate
        }
    }
    error "Copilot CLI not found for tests. Run 'npm install' in the nodejs directory."
}

# Configure the proxy for a specific test snapshot.
proc ::e2e::harness::configure {filePath workDir} {
    variable proxy_url
    set body [format {{"filePath":"%s","workDir":"%s"}} $filePath $workDir]
    set tok [::http::geturl "$proxy_url/config" \
        -method POST \
        -type "application/json" \
        -query $body]
    ::http::cleanup $tok
}

# HTTP GET against the proxy.
proc ::e2e::harness::http_get {path} {
    variable proxy_url
    set tok [::http::geturl "$proxy_url$path"]
    set body [::http::data $tok]
    ::http::cleanup $tok
    return $body
}

# HTTP POST against the proxy.
proc ::e2e::harness::http_post {path body} {
    variable proxy_url
    set tok [::http::geturl "$proxy_url$path" \
        -method POST \
        -type "application/json" \
        -headers [list Authorization "Bearer fake-token-for-e2e-tests"] \
        -query $body]
    set result [::http::data $tok]
    ::http::cleanup $tok
    return $result
}
