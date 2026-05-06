# version.tcl - Version constants for the Copilot SDK for Tcl
#
# Copyright (c) Microsoft Corporation. All rights reserved.

namespace eval ::copilot::version {
    variable sdk_version "2.0.0"
    variable sdk_name    "copilot-sdk-tcl"
    variable min_protocol_version 3
}

proc ::copilot::version::get {} {
    variable sdk_version
    return $sdk_version
}

proc ::copilot::version::get_name {} {
    variable sdk_name
    return $sdk_name
}

proc ::copilot::version::get_min_protocol {} {
    variable min_protocol_version
    return $min_protocol_version
}

package provide copilot::version 2.0.0
