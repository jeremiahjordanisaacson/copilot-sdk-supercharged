# Cookbook: External CLI Server
# Connect to an already-running Copilot CLI server via TCP.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function external_server_example()
    # Connect to a CLI server running on localhost:3000
    # Start the server separately:  copilot-cli --server --port 3000
    client = CopilotClient(cli_url="localhost:3000")

    try
        start!(client)
        println("Connected to external server.")

        # Query server status
        status = get_status(client)
        println("Server version: $(status.version)")
        println("Protocol version: $(status.protocol_version)")
        println("Authenticated: $(status.authenticated)")

        # List available models
        models = get_models(client)
        for m in models
            default_marker = m.is_default ? " (default)" : ""
            println("  Model: $(m.name)$default_marker")
        end

        # Create a session
        session = create_session(client;
            model="gpt-4",
            on_permission_request=approve_all,
        )

        done = Channel{Bool}(1)
        on(session) do event
            if event.type == "assistant.message_delta"
                print(get(event.data, "content", ""))
            elseif event.type == "session.idle"
                println()
                try put!(done, true) catch; end
            end
        end

        send(session, "What is Julia's multiple dispatch?")
        CopilotSDK.timedwait(30.0) do; isready(done); end

        disconnect(session)
    catch e
        println("Error: ", sprint(showerror, e))
        println("Make sure a CLI server is running: copilot-cli --server --port 3000")
    finally
        stop!(client)
    end
end

external_server_example()
