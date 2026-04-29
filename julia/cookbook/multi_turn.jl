# Cookbook: Multi-turn Conversation
# Demonstrates sending multiple prompts in sequence within one session.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function multi_turn()
    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    session = create_session(client;
        model="gpt-4",
        on_permission_request=approve_all,
    )

    prompts = [
        "What is the capital of France?",
        "What language do they speak there?",
        "Translate 'Hello, world!' into that language.",
    ]

    for (i, prompt) in enumerate(prompts)
        println("--- Turn $i: $prompt ---")

        done = Channel{Bool}(1)
        on(session) do event
            if event.type == "assistant.message_delta"
                print(get(event.data, "content", ""))
            elseif event.type == "session.idle"
                println()
                try put!(done, true) catch; end
            end
        end

        send(session, prompt)

        CopilotSDK.timedwait(30.0) do
            isready(done)
        end
    end

    disconnect(session)
    stop!(client)
end

multi_turn()
