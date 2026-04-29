# Tools and Skills

Define custom tools, register them with a session, and let the model invoke them.

## Defining a Simple Tool

Use `define_tool` to expose a function the model can call.

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    // Define parameters
    mut params := map[string]copilot.ToolParam{}
    params['city'] = copilot.ToolParam{
        param_type: 'string'
        description: 'City name to look up'
        required: true
    }

    // Define the tool with a handler
    weather_tool := copilot.define_tool('get_weather', 'Get current weather for a city', params,
        fn (args map[string]string) !string {
            city := args['city'] or { return error('missing city argument') }
            // In production, call a real weather API here
            return '{"city":"${city}","temp":"72F","condition":"sunny"}'
        }
    )

    mut session := client.create_session(copilot.SessionConfig{
        tools: [weather_tool]
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'What is the weather in Seattle?'
    }, 30_000) or { panic(err) }

    println(result)
}
```

## Multiple Tools

Register several tools in one session.

```v
import copilot

fn make_search_tool() copilot.Tool {
    mut params := map[string]copilot.ToolParam{}
    params['query'] = copilot.ToolParam{
        param_type: 'string'
        description: 'Search query'
        required: true
    }
    return copilot.define_tool('web_search', 'Search the web', params,
        fn (args map[string]string) !string {
            query := args['query'] or { return error('no query') }
            return '{"results":["Result for: ${query}"]}'
        }
    )
}

fn make_calculator_tool() copilot.Tool {
    mut params := map[string]copilot.ToolParam{}
    params['expression'] = copilot.ToolParam{
        param_type: 'string'
        description: 'Math expression'
        required: true
    }
    return copilot.define_tool('calculator', 'Evaluate a math expression', params,
        fn (args map[string]string) !string {
            expr := args['expression'] or { return error('no expression') }
            return '{"result":"${expr} = computed"}'
        }
    )
}

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    search := make_search_tool()
    calc := make_calculator_tool()

    mut session := client.create_session(copilot.SessionConfig{
        tools: [search, calc]
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    result := session.send_and_wait(copilot.SendOptions{
        prompt: 'Search for V language then calculate 6 * 7'
    }, 60_000) or { panic(err) }
    println(result)
}
```

## Tool Error Handling

If a tool handler returns an error, the SDK sends it back to the model.

```v
import copilot

fn main() {
    mut params := map[string]copilot.ToolParam{}
    params['path'] = copilot.ToolParam{
        param_type: 'string'
        description: 'File path'
        required: true
    }

    file_tool := copilot.define_tool('read_file', 'Read a file', params,
        fn (args map[string]string) !string {
            path := args['path'] or { return error('missing path') }
            if path.contains('..') {
                return error('path traversal not allowed')
            }
            return '{"content":"file contents for ${path}"}'
        }
    )

    // The model receives the error message and can adjust its approach
    _ = file_tool
}
```
