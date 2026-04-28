# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in Ruby.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  # Create a session with a custom ID
  config = Copilot::SessionConfig.new(session_id: "project-alpha-session-001")
  session = client.create_session(config)

  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "Let's plan the auth module")
  )
  puts response.message
ensure
  client.stop
end
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```ruby
require "copilot_sdk"

# First run: start a session
client1 = Copilot::Client.new
begin
  client1.start
  config = Copilot::SessionConfig.new(session_id: "persistent-session-42")
  session = client1.create_session(config)
  session.send_and_wait(Copilot::SendOptions.new(message: "We are building a payment gateway"))
ensure
  client1.stop
end

# Later run: resume the same session
client2 = Copilot::Client.new
begin
  client2.start
  config = Copilot::SessionConfig.new(session_id: "persistent-session-42")
  session = client2.create_session(config)

  # The session remembers previous context
  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "What were we building?")
  )
  puts response.message # Refers to the payment gateway
ensure
  client2.stop
end
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  config = Copilot::SessionConfig.new(
    session_id: "long-running-agent",
    infinite_session: true
  )
  session = client.create_session(config)

  # Listen for compaction events
  session.on_event("session.compaction_start") do |_event|
    puts "Compaction started: conversation history is being summarized..."
  end
  session.on_event("session.compaction_complete") do |_event|
    puts "Compaction complete: history has been summarized."
  end

  # Long-running loop that may trigger compaction
  tasks = [
    "Analyze the auth module",
    "Review the database schema",
    "Check the API endpoints",
    "Suggest performance improvements",
    "Write unit tests for the service layer"
  ]

  tasks.each do |task|
    response = session.send_and_wait(Copilot::SendOptions.new(message: task))
    puts "Task: #{task}"
    puts "Response: #{response.message}"
    puts
  end
ensure
  client.stop
end
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  config = Copilot::SessionConfig.new(session_id: "my-app-session")
  session = client.create_session(config)

  puts "Session ID: #{config.session_id}"
  puts "State stored at: ~/.copilot/session-state/my-app-session/"
ensure
  client.stop
end
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`infinite_session: true`) for long-running agents or interactive tools.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.
