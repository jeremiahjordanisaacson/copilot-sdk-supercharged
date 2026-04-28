# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Ruby.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  # Create two independent sessions
  code_review = client.create_session(Copilot::SessionConfig.new)
  documentation = client.create_session(Copilot::SessionConfig.new)

  # Each session tracks its own context
  review = code_review.send_and_wait(
    Copilot::SendOptions.new(message: "Review this function for bugs")
  )
  puts "Review: #{review.message}"

  docs = documentation.send_and_wait(
    Copilot::SendOptions.new(message: "Write API docs for UserService")
  )
  puts "Docs: #{docs.message}"
ensure
  client.stop
end
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  session1 = client.create_session(Copilot::SessionConfig.new)
  session2 = client.create_session(Copilot::SessionConfig.new)

  # Session 1 conversation
  session1.send_and_wait(Copilot::SendOptions.new(message: "I'm working on a Rails app"))
  follow_up1 = session1.send_and_wait(Copilot::SendOptions.new(message: "How should I handle auth?"))
  puts "Session 1: #{follow_up1.message}"

  # Session 2 conversation (knows nothing about session1)
  session2.send_and_wait(Copilot::SendOptions.new(message: "I'm building a CLI gem"))
  follow_up2 = session2.send_and_wait(Copilot::SendOptions.new(message: "How should I handle auth?"))
  puts "Session 2: #{follow_up2.message}"
ensure
  client.stop
end
```

## Parallel Requests with Threads

Run multiple sessions concurrently using Ruby threads.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  sessions = 3.times.map { client.create_session(Copilot::SessionConfig.new) }
  prompts = ["Explain blocks", "Explain procs", "Explain lambdas"]

  threads = sessions.zip(prompts).map do |session, prompt|
    Thread.new do
      response = session.send_and_wait(Copilot::SendOptions.new(message: prompt))
      { prompt: prompt, response: response.message }
    end
  end

  results = threads.map(&:value)
  results.each do |r|
    puts "#{r[:prompt]}: #{r[:response]}"
  end
ensure
  client.stop
end
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up with `client.stop`.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  # Short-lived session for a one-off task
  temp = client.create_session(Copilot::SessionConfig.new)
  result = temp.send_and_wait(Copilot::SendOptions.new(message: "Format this JSON"))
  puts result.message

  # Long-lived session for interactive use
  main_session = client.create_session(Copilot::SessionConfig.new)
  main_session.send_and_wait(Copilot::SendOptions.new(message: "Let's build an API"))
  main_session.send_and_wait(Copilot::SendOptions.new(message: "Add a /users endpoint"))
  main_session.send_and_wait(Copilot::SendOptions.new(message: "Add pagination support"))
ensure
  client.stop # All sessions are cleaned up
end
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Use `Thread.new` to run independent sessions concurrently.
- Keep session references in variables so you can send follow-ups to the right context.
- Call `client.stop` once when done; this cleans up all active sessions.
