# Cookbook: Streaming Chat
# Shows how to handle streaming deltas for a real-time chat experience.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function streaming_chat()
    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    session = create_session(client;
        model="gpt-4",
        streaming=true,
        on_permission_request=approve_all,
    )

    done = Channel{Bool}(1)
    full_response = IOBuffer()

    on(session) do event
        if event.type == "assistant.message_delta"
            chunk = get(event.data, "content", "")
            print(chunk)                 # stream to terminal in real time
            write(full_response, chunk)
        elseif event.type == "assistant.message"
            # Final consolidated message
            content = get(event.data, "content", "")
            println("\n\n--- Full response ($(length(content)) chars) ---")
        elseif event.type == "session.idle"
            try put!(done, true) catch; end
        end
    end

    send(session, "Write a haiku about Julia programming.")

    CopilotSDK.timedwait(30.0) do
        isready(done)
    end

    disconnect(session)
    stop!(client)
end

streaming_chat()
