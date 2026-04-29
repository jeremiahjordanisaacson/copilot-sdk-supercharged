# Cookbook: Custom Tools
# Demonstrates registering tools that the assistant can call.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function custom_tools_example()
    # Define a calculator tool using the do-block syntax
    calc_tool = define_tool("calculate", "Evaluate a math expression";
        parameters=Dict{String, Any}(
            "type" => "object",
            "properties" => Dict{String, Any}(
                "expression" => Dict{String, Any}(
                    "type" => "string",
                    "description" => "A Julia math expression to evaluate",
                ),
            ),
            "required" => ["expression"],
        ),
    ) do inv
        expr = get(inv.arguments, "expression", "0")
        try
            result = eval(Meta.parse(expr))
            ToolResult(text_result_for_llm="Result: $result")
        catch e
            ToolResult(
                text_result_for_llm="Error: $(sprint(showerror, e))",
                result_type=TOOL_FAILURE,
            )
        end
    end

    # Define a greeting tool using positional syntax
    greet_tool = define_tool("greet", "Greet a user by name",
        inv -> ToolResult(text_result_for_llm="Hello, $(get(inv.arguments, "name", "World"))!"))

    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    session = create_session(client;
        model="gpt-4",
        tools=[calc_tool, greet_tool],
        on_permission_request=approve_all,
    )

    done = Channel{Bool}(1)
    on(session) do event
        if event.type == "assistant.message"
            println("Assistant: ", get(event.data, "content", ""))
        elseif event.type == "session.idle"
            try put!(done, true) catch; end
        end
    end

    send(session, "Calculate 42 * 17 + 3, then greet Julia.")

    CopilotSDK.timedwait(60.0) do
        isready(done)
    end

    disconnect(session)
    stop!(client)
end

custom_tools_example()
