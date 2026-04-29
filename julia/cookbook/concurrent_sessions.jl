# Cookbook: Concurrent Sessions
# Run multiple sessions in parallel using Julia's @async / @sync.

push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

function concurrent_sessions()
    client = CopilotClient(log_level=LOG_ERROR)
    start!(client)

    prompts = [
        "Explain parametric types in Julia in two sentences.",
        "What is the difference between abstract and concrete types?",
        "Give a one-line example of multiple dispatch.",
    ]

    results = Dict{Int, String}()
    results_lock = ReentrantLock()

    @sync begin
        for (i, prompt) in enumerate(prompts)
            @async begin
                session = create_session(client;
                    model="gpt-4",
                    on_permission_request=approve_all,
                )

                buf = IOBuffer()
                done = Channel{Bool}(1)

                on(session) do event
                    if event.type == "assistant.message_delta"
                        write(buf, get(event.data, "content", ""))
                    elseif event.type == "session.idle"
                        try put!(done, true) catch; end
                    end
                end

                send(session, prompt)

                CopilotSDK.timedwait(30.0) do
                    isready(done)
                end

                lock(results_lock) do
                    results[i] = String(take!(buf))
                end

                disconnect(session)
            end
        end
    end

    # Print results in order
    for i in sort(collect(keys(results)))
        println("--- Session $i ---")
        println("Q: $(prompts[i])")
        println("A: $(results[i])")
        println()
    end

    stop!(client)
end

concurrent_sessions()
