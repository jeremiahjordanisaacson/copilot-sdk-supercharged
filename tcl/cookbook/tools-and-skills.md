# Tools and Skills - Tcl

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Tcl.

## Defining a Simple Tool

**Scenario:** Expose a Tcl proc as a tool that the Copilot model can call during a conversation.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

::copilot::client::define_tool "get_current_time" \
    "Returns the current date and time" \
    [list apply {{params} {
        return [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
    }}]

set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant with access to tools."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "What is the current date and time?"]]

puts "Response: $response"
::copilot::client::stop $client
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured input parameters from the model.

```tcl
package require copilot::client
package require copilot::jsonrpc

set client [::copilot::client::new]
::copilot::client::start $client

::copilot::client::define_tool "calculate" \
    "Performs basic arithmetic operations" \
    [list apply {{params} {
        set a [dict get $params a]
        set b [dict get $params b]
        set op [dict get $params operation]

        switch -- $op {
            add      { set result [expr {$a + $b}] }
            subtract { set result [expr {$a - $b}] }
            multiply { set result [expr {$a * $b}] }
            divide   {
                if {$b == 0} {
                    return [::copilot::jsonrpc::json_encode [dict create error "Division by zero"]]
                }
                set result [expr {double($a) / $b}]
            }
            default  {
                return [::copilot::jsonrpc::json_encode [dict create error "Unknown operation: $op"]]
            }
        }
        return [::copilot::jsonrpc::json_encode [dict create result $result]]
    }}]

set session [::copilot::client::create_session $client \
    system_prompt "You are a calculator assistant. Use the calculate tool for math."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "What is 42 multiplied by 17?"]]

puts "Response: $response"
::copilot::client::stop $client
```

## File System Tool

**Scenario:** Give the model the ability to read files from the local file system.

```tcl
package require copilot::client
package require copilot::jsonrpc

set client [::copilot::client::new]
::copilot::client::start $client

::copilot::client::define_tool "read_file" \
    "Reads and returns the contents of a file" \
    [list apply {{params} {
        set filepath [dict get $params path]
        if {![file exists $filepath]} {
            return [::copilot::jsonrpc::json_encode [dict create error "File not found: $filepath"]]
        }

        set fd [open $filepath r]
        set content [read $fd]
        close $fd

        set max_length 10000
        if {[string length $content] > $max_length} {
            set content "[string range $content 0 [expr {$max_length - 1}]]\n... (truncated)"
        }

        return [::copilot::jsonrpc::json_encode [dict create \
            path $filepath content $content size [string length $content]]]
    }}]

::copilot::client::define_tool "list_files" \
    "Lists files in a directory" \
    [list apply {{params} {
        set dir "."
        catch { set dir [dict get $params directory] }
        set files [glob -nocomplain -directory $dir *]
        set names {}
        foreach f $files {
            lappend names [file tail $f]
        }
        return [::copilot::jsonrpc::json_encode [dict create directory $dir files $names]]
    }}]

set session [::copilot::client::create_session $client \
    system_prompt "You are a file explorer assistant. Use your tools to read and list files."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "List the files in the current directory."]]

puts $response
::copilot::client::stop $client
```

## Multiple Tools Working Together

**Scenario:** Register several related tools that the model can combine to accomplish complex tasks.

```tcl
package require copilot::client
package require copilot::jsonrpc

set client [::copilot::client::new]
::copilot::client::start $client

# In-memory key-value store
set ::kv_store [dict create]

::copilot::client::define_tool "store_set" "Stores a value under a key" \
    [list apply {{params} {
        dict set ::kv_store [dict get $params key] [dict get $params value]
        return [::copilot::jsonrpc::json_encode [dict create status "ok" key [dict get $params key]]]
    }}]

::copilot::client::define_tool "store_get" "Retrieves a value by key" \
    [list apply {{params} {
        set key [dict get $params key]
        if {![dict exists $::kv_store $key]} {
            return [::copilot::jsonrpc::json_encode [dict create error "Key not found: $key"]]
        }
        return [::copilot::jsonrpc::json_encode [dict create key $key value [dict get $::kv_store $key]]]
    }}]

::copilot::client::define_tool "store_list" "Lists all keys in the store" \
    [list apply {{params} {
        return [::copilot::jsonrpc::json_encode [dict create keys [dict keys $::kv_store]]]
    }}]

::copilot::client::define_tool "store_delete" "Deletes a key from the store" \
    [list apply {{params} {
        set key [dict get $params key]
        dict unset ::kv_store $key
        return [::copilot::jsonrpc::json_encode [dict create status "deleted" key $key]]
    }}]

set session [::copilot::client::create_session $client \
    system_prompt "You are a key-value store assistant. Use the store tools to manage data."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "Store my name as Alice, my age as 30, then list all keys."]]

puts $response
::copilot::client::stop $client
```

## Tool with Error Handling

**Scenario:** Handle errors inside tool handlers gracefully so the model gets useful error messages.

```tcl
package require copilot::client
package require copilot::jsonrpc

set client [::copilot::client::new]
::copilot::client::start $client

::copilot::client::define_tool "safe_divide" \
    "Divides two numbers with error handling" \
    [list apply {{params} {
        if {[catch {
            set a [dict get $params numerator]
            set b [dict get $params denominator]
            if {$b == 0} { error "Cannot divide by zero" }
            set result [expr {double($a) / $b}]
        } err]} {
            return [::copilot::jsonrpc::json_encode [dict create error $err]]
        }
        return [::copilot::jsonrpc::json_encode [dict create result $result]]
    }}]

set session [::copilot::client::create_session $client \
    system_prompt "You are a math assistant. Use safe_divide for division."]

set response [::copilot::client::send_and_wait $session \
    [dict create message "What is 100 divided by 0?"]]

puts $response
::copilot::client::stop $client
```

## Orchestrating Sub-Agents

**Scenario:** Use one session to coordinate work across multiple specialized sessions.

```tcl
package require copilot::client

set client [::copilot::client::new]
::copilot::client::start $client

set code_agent [::copilot::client::create_session $client \
    system_prompt "You generate Tcl code. Only output code, no explanations."]

set review_agent [::copilot::client::create_session $client \
    system_prompt "You review Tcl code for bugs and style issues. Be concise."]

::copilot::client::define_tool "generate_code" \
    "Asks the code agent to generate Tcl code" \
    [list apply [list {params} {
        upvar #0 ::code_agent_handle agent
        set response [::copilot::client::send_and_wait $agent \
            [dict create message [dict get $params request]]]
        return $response
    }]]

::copilot::client::define_tool "review_code" \
    "Asks the review agent to review Tcl code" \
    [list apply [list {params} {
        upvar #0 ::review_agent_handle agent
        set response [::copilot::client::send_and_wait $agent \
            [dict create message "Review this code:\n[dict get $params code]"]]
        return $response
    }]]

set ::code_agent_handle $code_agent
set ::review_agent_handle $review_agent

set orchestrator [::copilot::client::create_session $client \
    system_prompt "You orchestrate tasks. Use generate_code to create code and review_code to review it."]

set response [::copilot::client::send_and_wait $orchestrator \
    [dict create message "Generate a Tcl proc to merge two sorted lists, then review it."]]

puts $response
::copilot::client::stop $client
```

## Best Practices

1. **Return JSON from tool handlers** for structured data that the model can parse reliably.
2. **Always handle errors inside tool handlers** using `catch` so the model receives an error message rather than causing a crash.
3. **Truncate large outputs** to stay within token limits and keep responses fast.
4. **Keep tool descriptions clear and specific** so the model knows exactly when and how to use each tool.
5. **Combine related tools** into a coherent set (CRUD operations, file tools) rather than making one monolithic tool.
6. **Use sub-agent orchestration** for complex multi-step workflows where different sessions specialize in different tasks.
