# tools.tcl - Tool definition and registry for the Copilot SDK for Tcl
#
# Provides a convenience proc for defining tools and a registry namespace
# for managing tool handlers that the Copilot model can invoke.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require copilot::types

namespace eval ::copilot::tools {
    variable registry [dict create]
    namespace export define_tool get_tool list_tools invoke_tool \
                     remove_tool clear_tools tools_to_wire
}

proc ::copilot::tools::define_tool {name description handler args} {
    variable registry

    set parameters {}
    if {[llength $args] > 0} {
        set parameters [lindex $args 0]
    }

    set tool [::copilot::types::make_tool $name $description $handler $parameters]
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
        lappend wire_tools $wire_tool
    }
    return $wire_tools
}

package provide copilot::tools 2.0.0
