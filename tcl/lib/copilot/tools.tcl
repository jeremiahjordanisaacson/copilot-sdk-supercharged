# tools.tcl - Tool definition and registry for the Copilot SDK for Tcl
#
# Provides a convenience proc for defining tools and a registry namespace
# for managing tool handlers that the Copilot model can invoke.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require copilot::types
package require copilot::jsonrpc

namespace eval ::copilot::tools {
    variable registry [dict create]
    namespace export define_tool get_tool list_tools invoke_tool \
                     remove_tool clear_tools tools_to_wire tools_to_wire_json
}

# define_tool name description handler ?parameters? ?idempotent? ?skip_permission?
# idempotent ("1"/"0") marks a tool as safe to call repeatedly with the same
# arguments; it is sent as "idempotent" in the tool definition. skip_permission
# ("1") lets the runtime run the tool without a permission prompt; otherwise the
# session needs an on_permission_request handler that approves it.
proc ::copilot::tools::define_tool {name description handler args} {
    variable registry

    set parameters {}
    if {[llength $args] > 0} {
        set parameters [lindex $args 0]
    }
    set idempotent ""
    if {[llength $args] > 1} {
        set idempotent [lindex $args 1]
    }
    set skip_permission ""
    if {[llength $args] > 2} {
        set skip_permission [lindex $args 2]
    }

    set tool [::copilot::types::make_tool $name $description $handler $parameters \
        $idempotent $skip_permission]
    dict set registry $name $tool
    return $tool
}

proc ::copilot::tools::get_tool {name} {
    variable registry
    if {![dict exists $registry $name]} {
        error "Tool not found: $name"
    }
    return [dict get $registry $name]
}

proc ::copilot::tools::list_tools {} {
    variable registry
    return [dict keys $registry]
}

proc ::copilot::tools::invoke_tool {name params} {
    variable registry
    if {![dict exists $registry $name]} {
        return [::copilot::types::make_error "Unknown tool: $name"]
    }
    set tool [dict get $registry $name]
    set handler [dict get $tool handler]

    if {[catch {set result [uplevel #0 [list {*}$handler $params]]} err]} {
        return [::copilot::types::make_error "Tool error ($name): $err"]
    }
    return [::copilot::types::make_ok $result]
}

proc ::copilot::tools::remove_tool {name} {
    variable registry
    if {[dict exists $registry $name]} {
        dict unset registry $name
        return 1
    }
    return 0
}

proc ::copilot::tools::clear_tools {} {
    variable registry
    set registry [dict create]
}

# Convert registered tools to the wire format expected by JSON-RPC
proc ::copilot::tools::tools_to_wire {} {
    variable registry
    set wire_tools {}
    dict for {name tool} $registry {
        set wire_tool [dict create \
            name        [dict get $tool name] \
            description [dict get $tool description] \
        ]
        set params [dict get $tool parameters]
        if {$params ne ""  && [llength $params] > 0} {
            dict set wire_tool parameters $params
        }
        if {[dict exists $tool idempotent] && [dict get $tool idempotent] ne ""} {
            dict set wire_tool idempotent [dict get $tool idempotent]
        }
        if {[dict exists $tool skip_permission] && [string is true -strict [dict get $tool skip_permission]]} {
            dict set wire_tool skipPermission true
        }
        lappend wire_tools $wire_tool
    }
    return $wire_tools
}

# Registered tools as a JSON array of tool definitions (tools.definitions on the
# wire). Parameters may be raw JSON (::copilot::jsonrpc::json_raw) or a Tcl dict.
proc ::copilot::tools::tools_to_wire_json {} {
    variable registry
    set texts [list]
    dict for {name tool} $registry {
        set members [dict create \
            name        [::copilot::jsonrpc::json_string [dict get $tool name]] \
            description [::copilot::jsonrpc::json_string [dict get $tool description]] \
        ]
        set params [dict get $tool parameters]
        if {$params ne ""} {
            dict set members parameters [::copilot::jsonrpc::params_json $params]
        }
        if {[dict exists $tool idempotent] && [dict get $tool idempotent] ne ""} {
            dict set members idempotent [::copilot::jsonrpc::json_bool [dict get $tool idempotent]]
        }
        if {[dict exists $tool skip_permission] && [string is true -strict [dict get $tool skip_permission]]} {
            dict set members skipPermission true
        }
        lappend texts [::copilot::jsonrpc::json_object $members]
    }
    return [::copilot::jsonrpc::json_array $texts]
}

package provide copilot::tools 2.0.0
