# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Ruby.

## Basic Error Handling

Use `begin/rescue/ensure` to catch errors and guarantee cleanup.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start
  session = client.create_session(Copilot::SessionConfig.new)
  response = session.send_and_wait(Copilot::SendOptions.new(message: "Explain Ruby blocks"))
  puts response.message
rescue StandardError => e
  warn "SDK error: #{e.message}"
ensure
  client.stop
end
```

## Handling Specific Error Types

Rescue specific exception classes before falling through to the generic handler.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start
  session = client.create_session(Copilot::SessionConfig.new)
  response = session.send_and_wait(Copilot::SendOptions.new(message: "Hello"))
  puts response.message
rescue Copilot::ConnectionError => e
  warn "Connection failed. Is the Copilot CLI running? #{e.message}"
rescue Copilot::TimeoutError => e
  warn "Request timed out: #{e.message}"
rescue StandardError => e
  warn "Unexpected error: #{e.message}"
ensure
  client.stop
end
```

## Timeout Handling

Use the `Timeout` module or SDK-level options to cap request duration.

```ruby
require "copilot_sdk"
require "timeout"

client = Copilot::Client.new

begin
  client.start
  session = client.create_session(Copilot::SessionConfig.new)

  Timeout.timeout(30) do
    response = session.send_and_wait(
      Copilot::SendOptions.new(message: "Summarize this large codebase")
    )
    puts response.message
  end
rescue Timeout::Error
  warn "Request timed out after 30 seconds"
rescue StandardError => e
  warn "Error: #{e.message}"
ensure
  client.stop
end
```

## Aborting a Request

Use a background thread and `Thread#kill` or a shared flag to cancel a request.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start
  session = client.create_session(Copilot::SessionConfig.new)

  worker = Thread.new do
    response = session.send_and_wait(
      Copilot::SendOptions.new(message: "Write a long essay")
    )
    puts response.message
  end

  # Cancel after 5 seconds
  sleep 5
  worker.kill
  puts "Request was cancelled."
rescue StandardError => e
  warn "Error: #{e.message}"
ensure
  client.stop
end
```

## Graceful Shutdown

Use `at_exit` or signal traps to ensure the client stops on exit.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

at_exit do
  puts "Shutting down..."
  client.stop
end

Signal.trap("INT") do
  puts "\nReceived interrupt, exiting..."
  exit
end

client.start
session = client.create_session(Copilot::SessionConfig.new)
response = session.send_and_wait(
  Copilot::SendOptions.new(message: "Hello from Ruby")
)
puts response.message
```

## Best Practices

- Always use `ensure` to call `client.stop` so resources are released on every code path.
- Rescue specific error classes before rescuing `StandardError`.
- Set timeouts on all production requests to avoid indefinite blocking.
- Use `at_exit` or signal traps for graceful shutdown in long-running processes.
- Log errors with context (session ID, prompt snippet) for easier debugging.
- Avoid rescuing `Exception` directly; prefer `StandardError` to let system signals propagate.
