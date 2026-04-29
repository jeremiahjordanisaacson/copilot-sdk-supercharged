# Multiple Sessions

Running multiple concurrent conversations with the Copilot CLI.

## Parallel Sessions

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

# Create several sessions for different tasks
code_session = client.create_session(
  CopilotSDK::SessionConfig.new(model: "gpt-4", instructions: "You are a code reviewer.")
)

docs_session = client.create_session(
  CopilotSDK::SessionConfig.new(model: "gpt-4", instructions: "You are a technical writer.")
)

# Use channels to collect results from concurrent sends
results = Channel(Tuple(String, String?)).new(2)

spawn do
  result = code_session.send_and_wait(
    CopilotSDK::MessageOptions.new(prompt: "Review this function: def add(a, b); a + b; end")
  )
  results.send({"code_review", result})
end

spawn do
  result = docs_session.send_and_wait(
    CopilotSDK::MessageOptions.new(prompt: "Write docs for an add function that takes two numbers")
  )
  results.send({"docs", result})
end

# Collect both results
2.times do
  label, content = results.receive
  puts "=== #{label} ==="
  puts content || "(no response)"
  puts
end

# Clean up
code_session.destroy
docs_session.destroy
client.stop
```

## Session Pool

```crystal
class SessionPool
  @sessions : Array(CopilotSDK::CopilotSession)
  @available : Channel(CopilotSDK::CopilotSession)

  def initialize(@client : CopilotSDK::CopilotClient, size : Int32 = 3)
    @sessions = Array(CopilotSDK::CopilotSession).new(size)
    @available = Channel(CopilotSDK::CopilotSession).new(size)

    size.times do
      session = @client.create_session(CopilotSDK::SessionConfig.new)
      @sessions << session
      @available.send(session)
    end
  end

  def acquire(&block : CopilotSDK::CopilotSession ->)
    session = @available.receive
    begin
      yield session
    ensure
      @available.send(session)
    end
  end

  def shutdown : Nil
    @sessions.each(&.destroy)
  end
end

# Usage
pool = SessionPool.new(client, size: 3)

prompts = ["Hello", "What is Crystal?", "Tell me a joke", "Explain fibers"]
done = Channel(Nil).new(prompts.size)

prompts.each do |prompt|
  spawn do
    pool.acquire do |session|
      result = session.send_and_wait(CopilotSDK::MessageOptions.new(prompt: prompt))
      puts "[#{prompt}] => #{result}"
    end
    done.send(nil)
  end
end

prompts.size.times { done.receive }
pool.shutdown
```
