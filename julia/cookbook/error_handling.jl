# Cookbook: Error Handling
# Demonstrates robust error handling patterns with the SDK.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function error_handling_example()
    # 1. Handle client connection errors
    client = CopilotClient(
        cli_path="/nonexistent/path",
        auto_start=false,
    )

    try
        start!(client)
    catch e
        println("Expected connection error: ", sprint(showerror, e))
    end

    # 2. Handle tool errors gracefully
    error_tool = define_tool("risky_op", "An operation that might fail") do inv
        action = get(inv.arguments, "action", "")
        if action == "crash"
            error("Simulated failure!")
        end
        ToolResult(text_result_for_llm="Success: $action")
    end

    # The session's tool dispatch catches exceptions and returns failure results.
    # You can also handle errors inside your tool:
    safe_tool = define_tool("safe_op", "Safely handles errors") do inv
        try
            result = parse(Int, get(inv.arguments, "number", "abc"))
            ToolResult(text_result_for_llm="Parsed: $result")
        catch e
            ToolResult(
                text_result_for_llm="Could not parse number: $(sprint(showerror, e))",
                result_type=TOOL_FAILURE,
                error=sprint(showerror, e),
            )
        end
    end

    # 3. Test the safe tool locally (no server needed)
    inv = ToolInvocation(
        session_id="test",
        tool_call_id="tc1",
        tool_name="safe_op",
        arguments=Dict{String, Any}("number" => "not_a_number"),
    )
    result = safe_tool.handler(inv)
    println("Safe tool result: $(result.result_type) - $(result.text_result_for_llm)")

    inv2 = ToolInvocation(
        session_id="test",
        tool_call_id="tc2",
        tool_name="safe_op",
        arguments=Dict{String, Any}("number" => "42"),
    )
    result2 = safe_tool.handler(inv2)
    println("Safe tool result: $(result2.result_type) - $(result2.text_result_for_llm)")

    # 4. Timeout handling with send_and_wait
    println("\nAll error handling patterns demonstrated successfully.")
end

error_handling_example()
