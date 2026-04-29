# Persisting Sessions

How to resume sessions across application restarts.

## Save and Restore Session ID

```crystal
require "copilot_sdk"
require "json"

SESSION_FILE = ".copilot_session.json"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

# Try to resume an existing session
session = if File.exists?(SESSION_FILE)
            data = JSON.parse(File.read(SESSION_FILE))
            session_id = data["session_id"].as_s
            puts "Resuming session: #{session_id}"
            begin
              client.resume_session(
                CopilotSDK::ResumeSessionConfig.new(session_id: session_id)
              )
            rescue ex
              puts "Could not resume (#{ex.message}), creating new session"
              client.create_session(CopilotSDK::SessionConfig.new)
            end
          else
            puts "Creating new session"
            client.create_session(CopilotSDK::SessionConfig.new)
          end

# Persist the session ID
File.write(SESSION_FILE, %({"session_id": "#{session.session_id}"}))
puts "Session ID saved: #{session.session_id}"

# Use the session
result = session.send_and_wait(
  CopilotSDK::MessageOptions.new(prompt: "Remember that my name is Crystal Developer.")
)
puts result

client.stop
```

## Session State Directory

The Copilot CLI stores session state in `~/.copilot/session-state/{sessionId}`.
Infinite sessions are enabled by default, so session history persists automatically.

```crystal
# Check if session state exists on disk
def session_state_exists?(session_id : String) : Bool
  home = ENV["HOME"]? || ENV["USERPROFILE"]? || "~"
  state_dir = File.join(home, ".copilot", "session-state", session_id)
  Dir.exists?(state_dir)
end

# List all persisted sessions
def list_persisted_sessions : Array(String)
  home = ENV["HOME"]? || ENV["USERPROFILE"]? || "~"
  state_dir = File.join(home, ".copilot", "session-state")
  return [] of String unless Dir.exists?(state_dir)

  Dir.children(state_dir).select { |name|
    File.directory?(File.join(state_dir, name))
  }
end
```
