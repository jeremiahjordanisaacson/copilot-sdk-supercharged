# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# Basic example: connect to Copilot CLI, create a session, send a prompt, print the reply.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function main()
    # Create a client (set cli_path or COPILOT_CLI_PATH env var)
    client = CopilotClient(log_level=LOG_ERROR)

    try
        start!(client)
        println("Client connected.")

        # Create a session with automatic permission approval
        session = create_session(client;
            model="gpt-4",
            on_permission_request=approve_all,
        )
        println("Session created: $(session.session_id)")

        # Collect assistant messages
        done = Channel{Bool}(1)
        on(session) do event
            if event.type == "assistant.message_delta"
                content = get(event.data, "content", "")
                print(content)
            elseif event.type == "session.idle"
                println()
                try put!(done, true) catch; end
            end
        end

        # Send a prompt
        println("Sending prompt...")
        send(session, "Explain the Julia type system in three sentences.")

        # Wait for the response
        result = CopilotSDK.timedwait(60.0) do
            isready(done)
        end
        if result === :timed_out
            println("\nTimed out waiting for response.")
        end

        disconnect(session)
        println("Session disconnected.")
    finally
        stop!(client)
        println("Client stopped.")
    end
end

main()
