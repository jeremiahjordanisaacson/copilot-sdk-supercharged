# Copyright (c) Microsoft Corporation. All rights reserved.

# Basic example showing how to use the Copilot SDK for Crystal.

require "../src/copilot_sdk"

# Create a client pointing at the Copilot CLI
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(
    cli_path: ENV["COPILOT_CLI_PATH"]? || "/usr/local/bin/copilot-cli"
  )
)

begin
  # Start the connection
  client.start
  puts "Connected to Copilot CLI"

  # Create a new session
  session = client.create_session(
    CopilotSDK::SessionConfig.new(model: "gpt-4")
  )
  puts "Session created: #{session.session_id}"

  # Register a simple tool
  session.define_tool("get_time") do |t|
    t.description = "Get the current date and time"
    t.handler do |_args, _invocation|
      JSON::Any.new(Time.utc.to_s)
    end
  end

  # Approve all permission requests
  session.on_permission do |_request, _session_id|
    CopilotSDK::PermissionRequestResult.new(allowed: true)
  end

  # Subscribe to streaming events
  session.on("assistant.message_delta") do |event|
    if delta = event.delta_text
      print delta
    end
  end

  session.on("assistant.message") do |_event|
    puts # newline after streaming completes
  end

  # Send a message and wait for the full response
  result = session.send_and_wait(
    CopilotSDK::MessageOptions.new(prompt: "What time is it? Use the get_time tool."),
    timeout: 30
  )

  if result
    puts "\nFinal response: #{result}"
  else
    puts "\nNo response received (timeout)"
  end

  # Clean up
  session.destroy
  puts "Session destroyed"
ensure
  client.stop
  puts "Client stopped"
end
