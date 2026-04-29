# Fibers and Channels

Crystal-specific concurrency patterns for the Copilot SDK.

## Channel-Based Event Collection

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

session = client.create_session(
  CopilotSDK::SessionConfig.new(streaming: true)
)

# Collect all events into a channel
events = Channel(CopilotSDK::SessionEvent).new(256)

session.on do |event|
  events.send(event)
end

# Process events in a dedicated fiber
spawn do
  while event = events.receive?
    case event.type
    when "assistant.message_delta"
      print event.delta_text
    when "assistant.message"
      puts "\n--- Message complete ---"
    when "turn.end"
      puts "--- Turn complete ---"
    end
  end
end

session.send(CopilotSDK::MessageOptions.new(prompt: "Explain Crystal fibers"))
sleep 10 # wait for events to arrive

session.destroy
client.stop
```

## Timeout with Select

```crystal
# Crystal's select statement works well for timeouts
result_ch = Channel(String).new(1)

spawn do
  result = session.send_and_wait(
    CopilotSDK::MessageOptions.new(prompt: "Tell me about Crystal"),
    timeout: 30
  )
  result_ch.send(result || "")
end

select
when result = result_ch.receive
  puts "Got result: #{result}"
when timeout(45.seconds)
  puts "Overall timeout exceeded"
end
```

## Fan-Out / Fan-In

```crystal
# Send multiple prompts in parallel and collect results
prompts = [
  "What is Crystal?",
  "What are fibers?",
  "What are channels?",
]

results = Channel(Tuple(Int32, String?)).new(prompts.size)

prompts.each_with_index do |prompt, index|
  spawn do
    result = session.send_and_wait(
      CopilotSDK::MessageOptions.new(prompt: prompt),
      timeout: 30
    )
    results.send({index, result})
  end
end

# Collect results in any order
collected = Hash(Int32, String?).new
prompts.size.times do
  idx, result = results.receive
  collected[idx] = result
end

# Print in original order
collected.keys.sort.each do |idx|
  puts "Q: #{prompts[idx]}"
  puts "A: #{collected[idx]}"
  puts
end
```

## Background Heartbeat

```crystal
# Run a periodic health check in the background
heartbeat_done = Channel(Nil).new(1)

spawn do
  loop do
    select
    when heartbeat_done.receive
      break
    when timeout(30.seconds)
      begin
        status = client.get_status
        puts "Heartbeat OK: #{status.status}"
      rescue ex
        STDERR.puts "Heartbeat failed: #{ex.message}"
      end
    end
  end
end

# ... use the client ...

# Stop the heartbeat
heartbeat_done.send(nil)
```
