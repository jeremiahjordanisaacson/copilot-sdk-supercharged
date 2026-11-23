# jsonrpc.tcl - JSON-RPC 2.0 transport for the Copilot SDK for Tcl
#
# Handles Content-Length framed JSON-RPC messages over stdio pipes.
# Uses the Tcllib json and json::write packages for serialization.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6

namespace eval ::copilot::jsonrpc {
    variable next_id 0
    variable pending_requests [dict create]
    variable notification_handlers [dict create]

    namespace export send_request send_notification read_message \
                     register_handler unregister_handler \
                     encode_message decode_message next_request_id
}

# ---- JSON helpers (pure Tcl, no Tcllib dependency) --------------------------

# Raw JSON values: json_raw tags exactly-typed JSON text so json_encode (and the
# message framing) emits it verbatim. Tcl values are untyped, so payloads whose
# shape matters (session options, server-request replies) are built as raw JSON.
proc ::copilot::jsonrpc::json_raw {text} {
    return [list ::copilot::jsonrpc::RAW $text]
}

proc ::copilot::jsonrpc::is_raw {value} {
    if {![string match {::copilot::jsonrpc::RAW *} $value]} { return 0 }
    if {[catch {llength $value} n] || $n != 2} { return 0 }
    return [expr {[lindex $value 0] eq "::copilot::jsonrpc::RAW"}]
}

# JSON text of a value: raw values verbatim, anything else via json_encode.
proc ::copilot::jsonrpc::to_json {value} {
    if {[is_raw $value]} {
        return [lindex $value 1]
    }
    return [json_encode $value]
}

# Typed JSON text builders.
proc ::copilot::jsonrpc::json_string {s} {
    set s [string map {\\ \\\\ \" \\\" \n \\n \r \\r \t \\t \b \\b \f \\f} $s]
    return "\"$s\""
}

proc ::copilot::jsonrpc::json_bool {value} {
    if {[string is true -strict $value]} { return "true" }
    return "false"
}

proc ::copilot::jsonrpc::json_number {value} {
    if {![string is integer -strict $value] && ![string is double -strict $value]} {
        error "Not a number: $value"
    }
    return $value
}

# JSON array of strings from a Tcl list.
proc ::copilot::jsonrpc::json_string_list {items} {
    set parts [list]
    foreach item $items {
        lappend parts [json_string $item]
    }
    return "\[[join $parts ,]\]"
}

# JSON array from a Tcl list of JSON texts.
proc ::copilot::jsonrpc::json_array {texts} {
    return "\[[join $texts ,]\]"
}

# JSON object from a dict of key -> JSON text (insertion order is kept).
proc ::copilot::jsonrpc::json_object {members} {
    set parts [list]
    dict for {key text} $members {
        lappend parts "[json_string $key]:$text"
    }
    set lbrace \{
    set rbrace \}
    return "${lbrace}[join $parts ,]${rbrace}"
}

# Split a JSON object text into a dict of key -> member JSON text, without
# decoding the member values (so their JSON types are preserved).
proc ::copilot::jsonrpc::json_object_members {text} {
    set text [string trim $text]
    if {[string index $text 0] ne "\{" || [string index $text end] ne "\}"} {
        error "Not a JSON object: $text"
    }
    set members [dict create]
    set pos 1
    while {1} {
        set pos [_jd_skip_ws $text $pos]
        set ch [string index $text $pos]
        if {$ch eq "\}" || $ch eq ""} {
            break
        }
        if {$ch eq ","} {
            incr pos
            continue
        }
        lassign [_jd_parse_string $text $pos] key pos
        set pos [_jd_skip_ws $text $pos]
        incr pos ;# skip colon
        set pos [_jd_skip_ws $text $pos]
        set end [_json_value_end $text $pos]
        dict set members $key [string trim [string range $text $pos [expr {$end - 1}]]]
        set pos $end
    }
    return $members
}

# Index just past the JSON value that starts at pos.
proc ::copilot::jsonrpc::_json_value_end {text pos} {
    set len [string length $text]
    set depth 0
    set in_string 0
    set i $pos
    while {$i < $len} {
        set ch [string index $text $i]
        if {$in_string} {
            if {$ch eq "\\"} {
                incr i 2
                continue
            }
            if {$ch eq "\""} {
                set in_string 0
                if {$depth == 0} {
                    return [expr {$i + 1}]
                }
            }
            incr i
            continue
        }
        if {$ch eq "\""} {
            set in_string 1
        } elseif {$ch eq "\{" || $ch eq "\["} {
            incr depth
        } elseif {$ch eq "\}" || $ch eq "\]"} {
            if {$depth == 0} {
                return $i
            }
            incr depth -1
            if {$depth == 0} {
                return [expr {$i + 1}]
            }
        } elseif {$ch eq "," && $depth == 0} {
            return $i
        }
        incr i
    }
    return $len
}

# JSON text of request params / results: raw values verbatim, an empty value as
# {}, a Tcl dict as an object (each member via to_json), anything else via
# json_encode.
proc ::copilot::jsonrpc::params_json {params} {
    if {[is_raw $params]} {
        return [lindex $params 1]
    }
    if {$params eq ""} {
        return "\{\}"
    }
    if {![catch {dict size $params} sz] && $sz > 0} {
        set members [dict create]
        dict for {key val} $params {
            dict set members $key [to_json $val]
        }
        return [json_object $members]
    }
    return [json_encode $params]
}

# Minimal JSON encoder that handles dicts, lists, strings, numbers, and bools.
# Uses a _tag metadata dict to distinguish JSON arrays from dicts when needed.
proc ::copilot::jsonrpc::json_encode {value} {
    if {[is_raw $value]} {
        return [lindex $value 1]
    }
    if {$value eq "null"} {
        return "null"
    }
    if {$value eq "true" || $value eq "false"} {
        return $value
    }
    # Number check (must come before dict/list to avoid treating "42" as a list)
    if {[string is integer -strict $value] || [string is double -strict $value]} {
        return $value
    }
    # If the string representation contains spaces or special chars that
    # make it "look" like a sentence rather than a structured value, encode
    # as a JSON string directly. This prevents "hello world" from being
    # treated as {hello: world}.
    if {[string first " " $value] >= 0 && [string index $value 0] ne "\{"} {
        return [_je_encode_string $value]
    }
    # Check for dict (even-length list where dict operations work)
    if {[_je_is_dict $value]} {
        return [_je_encode_dict $value]
    }
    # Check for list that should be a JSON array
    if {[_je_is_array $value]} {
        return [_je_encode_list $value]
    }
    # Default: treat as string
    return [_je_encode_string $value]
}

proc ::copilot::jsonrpc::_je_is_dict {value} {
    if {[catch {dict size $value} sz]} { return 0 }
    if {$sz == 0} { return 0 }
    if {[llength $value] % 2 != 0} { return 0 }
    # If any key contains a space, this is unlikely to be a dict.
    # Also reject if the value looks like a plain sentence (keys are words).
    set has_nonword_key 0
    dict for {k v} $value {
        if {![string is alpha -strict $k] && ![string match {*_*} $k]} {
            set has_nonword_key 1
            break
        }
    }
    # Heuristic: if all keys are simple alpha words AND there's only one key,
    # it could be either. Prefer dict if keys look like identifiers.
    return 1
}

proc ::copilot::jsonrpc::_je_is_array {value} {
    if {[llength $value] <= 1} { return 0 }
    if {[_je_is_dict $value]} { return 0 }
    return 1
}

proc ::copilot::jsonrpc::_je_encode_string {s} {
    set s [string map {\\ \\\\ \" \\\" \n \\n \r \\r \t \\t} $s]
    return "\"$s\""
}

proc ::copilot::jsonrpc::_je_encode_dict {d} {
    set parts [list]
    dict for {key val} $d {
        lappend parts "[_je_encode_string $key]:[json_encode $val]"
    }
    set lbrace \{
    set rbrace \}
    return "${lbrace}[join $parts ,]${rbrace}"
}

proc ::copilot::jsonrpc::_je_encode_list {lst} {
    set parts [list]
    foreach item $lst {
        lappend parts [json_encode $item]
    }
    return "\[[join $parts ,]\]"
}

# Minimal JSON decoder returning nested Tcl dicts/lists.
proc ::copilot::jsonrpc::json_decode {json_str} {
    set json_str [string trim $json_str]
    lassign [_jd_parse_value $json_str 0] value pos
    return $value
}

proc ::copilot::jsonrpc::_jd_skip_ws {s pos} {
    while {$pos < [string length $s]} {
        set ch [string index $s $pos]
        if {$ch ne " " && $ch ne "\t" && $ch ne "\n" && $ch ne "\r"} {
            break
        }
        incr pos
    }
    return $pos
}

proc ::copilot::jsonrpc::_jd_parse_value {s pos} {
    set pos [_jd_skip_ws $s $pos]
    set ch [string index $s $pos]
    if {$ch eq "\{"} {
        return [_jd_parse_object $s $pos]
    }
    if {$ch eq "\["} {
        return [_jd_parse_array $s $pos]
    }
    if {$ch eq "\""} {
        return [_jd_parse_string $s $pos]
    }
    if {$ch eq "t"} {
        return [list true [expr {$pos + 4}]]
    }
    if {$ch eq "f"} {
        return [list false [expr {$pos + 5}]]
    }
    if {$ch eq "n"} {
        return [list null [expr {$pos + 4}]]
    }
    return [_jd_parse_number $s $pos]
}

proc ::copilot::jsonrpc::_jd_parse_string {s pos} {
    incr pos ;# skip opening quote
    set result ""
    set len [string length $s]
    while {$pos < $len} {
        set ch [string index $s $pos]
        if {$ch eq "\\"} {
            incr pos
            set esc [string index $s $pos]
            if {$esc eq "n"}       { append result "\n"
            } elseif {$esc eq "r"} { append result "\r"
            } elseif {$esc eq "t"} { append result "\t"
            } elseif {$esc eq "\""} { append result "\""
            } elseif {$esc eq "\\"} { append result "\\"
            } elseif {$esc eq "/"}  { append result "/"
            } elseif {$esc eq "u"} {
                set hex [string range $s [expr {$pos+1}] [expr {$pos+4}]]
                append result [format %c 0x$hex]
                incr pos 4
            } else {
                append result $esc
            }
        } elseif {$ch eq "\""} {
            incr pos
            return [list $result $pos]
        } else {
            append result $ch
        }
        incr pos
    }
    error "Unterminated string in JSON"
}

proc ::copilot::jsonrpc::_jd_parse_number {s pos} {
    set start $pos
    set len [string length $s]
    while {$pos < $len} {
        set ch [string index $s $pos]
        if {[string is digit $ch] || $ch eq "." || $ch eq "e" || $ch eq "E" || $ch eq "+" || $ch eq "-"} {
            incr pos
        } else {
            break
        }
    }
    set numstr [string range $s $start [expr {$pos - 1}]]
    if {[string is integer -strict $numstr]} {
        return [list [expr {int($numstr)}] $pos]
    }
    return [list [expr {double($numstr)}] $pos]
}

proc ::copilot::jsonrpc::_jd_parse_object {s pos} {
    incr pos ;# skip opening brace
    set result [dict create]
    set pos [_jd_skip_ws $s $pos]
    if {[string index $s $pos] eq "\}"} {
        return [list $result [expr {$pos + 1}]]
    }
    while {1} {
        set pos [_jd_skip_ws $s $pos]
        lassign [_jd_parse_string $s $pos] key pos
        set pos [_jd_skip_ws $s $pos]
        incr pos ;# skip colon
        lassign [_jd_parse_value $s $pos] val pos
        dict set result $key $val
        set pos [_jd_skip_ws $s $pos]
        set ch [string index $s $pos]
        if {$ch eq "\}"} {
            return [list $result [expr {$pos + 1}]]
        }
        incr pos ;# skip comma
    }
}

proc ::copilot::jsonrpc::_jd_parse_array {s pos} {
    incr pos ;# skip opening bracket
    set result [list]
    set pos [_jd_skip_ws $s $pos]
    if {[string index $s $pos] eq "\]"} {
        return [list $result [expr {$pos + 1}]]
    }
    while {1} {
        lassign [_jd_parse_value $s $pos] val pos
        lappend result $val
        set pos [_jd_skip_ws $s $pos]
        set ch [string index $s $pos]
        if {$ch eq "\]"} {
            return [list $result [expr {$pos + 1}]]
        }
        incr pos ;# skip comma
    }
}

# ---- Message framing (Content-Length header) ---------------------------------

proc ::copilot::jsonrpc::encode_message {body_dict} {
    set json [json_encode $body_dict]
    set len [string length $json]
    return "Content-Length: $len\r\n\r\n$json"
}

proc ::copilot::jsonrpc::read_message {channel} {
    # Read Content-Length header
    while {1} {
        if {[eof $channel]} {
            return ""
        }
        gets $channel line
        set line [string trimright $line "\r"]
        if {[string match "Content-Length:*" $line]} {
            set content_length [string trim [string range $line 15 end]]
            break
        }
        if {$line eq ""} {
            continue
        }
    }

    # Skip blank line separator
    while {1} {
        if {[eof $channel]} { return "" }
        gets $channel line
        set line [string trimright $line "\r"]
        if {$line eq ""} { break }
    }

    # Read exactly content_length bytes (the body is UTF-8 encoded)
    set body [read $channel $content_length]
    if {$body eq ""} {
        return ""
    }

    return [json_decode [encoding convertfrom utf-8 $body]]
}

# Write one Content-Length framed message whose body is the given JSON text.
proc ::copilot::jsonrpc::write_frame {channel json} {
    set bytes [encoding convertto utf-8 $json]
    puts -nonewline $channel "Content-Length: [string length $bytes]\r\n\r\n$bytes"
    flush $channel
}

# ---- Request / notification sending -----------------------------------------

proc ::copilot::jsonrpc::next_request_id {} {
    variable next_id
    incr next_id
    return $next_id
}

proc ::copilot::jsonrpc::send_request {channel method params} {
    set id [next_request_id]
    write_frame $channel [json_object [dict create \
        jsonrpc [json_string "2.0"] \
        id      $id \
        method  [json_string $method] \
        params  [params_json $params] \
    ]]
    return $id
}

proc ::copilot::jsonrpc::send_notification {channel method params} {
    write_frame $channel [json_object [dict create \
        jsonrpc [json_string "2.0"] \
        method  [json_string $method] \
        params  [params_json $params] \
    ]]
}

# The id is echoed exactly as received (a JSON number or string).
proc ::copilot::jsonrpc::_id_json {id} {
    if {[string is integer -strict $id]} {
        return $id
    }
    return [json_string $id]
}

proc ::copilot::jsonrpc::send_response {channel id result} {
    write_frame $channel [json_object [dict create \
        jsonrpc [json_string "2.0"] \
        id      [_id_json $id] \
        result  [params_json $result] \
    ]]
}

proc ::copilot::jsonrpc::send_error_response {channel id code message} {
    write_frame $channel [json_object [dict create \
        jsonrpc [json_string "2.0"] \
        id      [_id_json $id] \
        error   [json_object [dict create \
            code    [json_number $code] \
            message [json_string $message] \
        ]] \
    ]]
}

# ---- Notification handler registry ------------------------------------------

proc ::copilot::jsonrpc::register_handler {method callback} {
    variable notification_handlers
    dict set notification_handlers $method $callback
}

proc ::copilot::jsonrpc::unregister_handler {method} {
    variable notification_handlers
    if {[dict exists $notification_handlers $method]} {
        dict unset notification_handlers $method
    }
}

proc ::copilot::jsonrpc::dispatch_notification {method params} {
    variable notification_handlers
    if {[dict exists $notification_handlers $method]} {
        set cb [dict get $notification_handlers $method]
        uplevel #0 [list {*}$cb $params]
    }
}

# ---- Pending request tracking -----------------------------------------------

proc ::copilot::jsonrpc::register_pending {id} {
    variable pending_requests
    dict set pending_requests $id [dict create resolved 0 result "" error ""]
}

proc ::copilot::jsonrpc::resolve_pending {id result} {
    variable pending_requests
    if {[dict exists $pending_requests $id]} {
        dict set pending_requests $id resolved 1
        dict set pending_requests $id result $result
    }
}

proc ::copilot::jsonrpc::reject_pending {id error_info} {
    variable pending_requests
    if {[dict exists $pending_requests $id]} {
        dict set pending_requests $id resolved 1
        dict set pending_requests $id error $error_info
    }
}

proc ::copilot::jsonrpc::is_resolved {id} {
    variable pending_requests
    if {![dict exists $pending_requests $id]} { return 1 }
    return [dict get [dict get $pending_requests $id] resolved]
}

proc ::copilot::jsonrpc::get_result {id} {
    variable pending_requests
    if {![dict exists $pending_requests $id]} {
        error "No pending request with id $id"
    }
    set entry [dict get $pending_requests $id]
    if {[dict get $entry error] ne ""} {
        error [dict get $entry error]
    }
    return [dict get $entry result]
}

proc ::copilot::jsonrpc::remove_pending {id} {
    variable pending_requests
    if {[dict exists $pending_requests $id]} {
        dict unset pending_requests $id
    }
}

package provide copilot::jsonrpc 2.0.0
