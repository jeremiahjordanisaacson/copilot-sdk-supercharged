# Error Handling

Patterns for handling errors, timeouts, and retries in Crystal.

## Basic Error Handling

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)

begin
  client.start
rescue ex : CopilotSDK::CopilotError
  STDERR.puts "Failed to connect: #{ex.message}"
  exit 1
end
```

## Retry Logic

```crystal
def with_retry(max_attempts = 3, delay = 1.0, &block)
  attempts = 0
  loop do
    attempts += 1
    begin
      return yield
    rescue ex : CopilotSDK::CopilotError
      raise ex if attempts >= max_attempts
      STDERR.puts "Attempt #{attempts} failed: #{ex.message}. Retrying in #{delay}s..."
      sleep delay
      delay *= 2 # exponential backoff
    end
  end
end

# Usage
result = with_retry do
  session.send_and_wait(
    CopilotSDK::MessageOptions.new(prompt: "Hello"),
    timeout: 30
  )
end
```

## Timeout Handling

```crystal
# send_and_wait returns nil on timeout
result = session.send_and_wait(
  CopilotSDK::MessageOptions.new(prompt: "Complex question..."),
  timeout: 120
)

if result.nil?
  puts "Request timed out - try a simpler prompt or increase the timeout"
end
```

## JSON-RPC Errors

```crystal
begin
  session.send(CopilotSDK::MessageOptions.new(prompt: "Hello"))
rescue ex : CopilotSDK::JsonRpcError
  STDERR.puts "RPC error (code #{ex.code}): #{ex.message}"
  if data = ex.data
    STDERR.puts "Details: #{data.to_json}"
  end
end
```

## Graceful Shutdown with at_exit

```crystal
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)

at_exit do
  client.stop
  puts "Cleaned up Copilot client"
end

Signal::INT.trap do
  puts "\nShutting down..."
  exit 0
end

client.start
# ... use the client
```
