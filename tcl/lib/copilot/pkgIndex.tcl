# pkgIndex.tcl - Package index for the Copilot SDK for Tcl
#
# Auto-generated package index. Source this or add the parent directory
# to your auto_path to make the copilot:: packages available.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

set dir [file dirname [info script]]

package ifneeded copilot::version 2.0.0 [list source [file join $dir version.tcl]]
package ifneeded copilot::types   2.0.0 [list source [file join $dir types.tcl]]
package ifneeded copilot::jsonrpc 2.0.0 [list source [file join $dir jsonrpc.tcl]]
package ifneeded copilot::tools   2.0.0 [list source [file join $dir tools.tcl]]
package ifneeded copilot::session 2.0.0 [list source [file join $dir session.tcl]]
package ifneeded copilot::client  2.0.0 [list source [file join $dir client.tcl]]
