# version.tcl - Version constants for the Copilot SDK for Tcl
#
# Copyright (c) Microsoft Corporation. All rights reserved.

namespace eval ::copilot::version {
    variable sdk_version "3.1.1"
    variable sdk_name    "copilot-sdk-tcl"
    # SDK protocol version spoken by this SDK; the server must report exactly this.
    variable sdk_protocol_version 4
    variable min_protocol_version 4
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

proc ::copilot::version::get_protocol {} {
    variable sdk_protocol_version
    return $sdk_protocol_version
}

package provide copilot::version 3.1.1
