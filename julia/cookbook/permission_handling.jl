# Cookbook: Permission Handling
# Shows different strategies for handling permission requests.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

# Strategy 1: approve everything (for trusted environments)
function run_with_approve_all()
    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    session = create_session(client;
        model="gpt-4",
        on_permission_request=approve_all,
    )

    done = Channel{Bool}(1)
    on(session) do event
        event.type == "session.idle" && (try put!(done, true) catch; end)
    end

    send(session, "List files in the current directory.")
    CopilotSDK.timedwait(30.0) do; isready(done); end

    disconnect(session)
    stop!(client)
end

# Strategy 2: selective approval based on tool name
function selective_handler(req::PermissionRequest)
    allowed = Set(["read_file", "list_directory", "search"])
    if req.tool_name in allowed
        println("  [APPROVED] $(req.tool_name): $(req.description)")
        return "allow"
    else
        println("  [DENIED]   $(req.tool_name): $(req.description)")
        return "deny"
    end
end

function run_with_selective()
    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    session = create_session(client;
        model="gpt-4",
        on_permission_request=selective_handler,
    )

    done = Channel{Bool}(1)
    on(session) do event
        event.type == "session.idle" && (try put!(done, true) catch; end)
    end

    send(session, "Read the README.md file and summarize it.")
    CopilotSDK.timedwait(30.0) do; isready(done); end

    disconnect(session)
    stop!(client)
end

# Strategy 3: interactive prompt
function interactive_handler(req::PermissionRequest)
    println("\nPermission requested:")
    println("  Tool: $(req.tool_name)")
    println("  Description: $(req.description)")
    print("  Allow? [y/N]: ")
    answer = strip(readline())
    return lowercase(answer) in ("y", "yes") ? "allow" : "deny"
end

println("=== Approve All ===")
run_with_approve_all()

println("\n=== Selective ===")
run_with_selective()
