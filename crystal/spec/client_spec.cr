# Copyright (c) Microsoft Corporation. All rights reserved.

require "./spec_helper"

describe CopilotSDK do
  describe "VERSION" do
    it "has a version string" do
      CopilotSDK::VERSION.should_not be_empty
      CopilotSDK::VERSION.should match(/^\d+\.\d+\.\d+$/)
    end
  end

  describe "MIN_PROTOCOL_VERSION" do
    it "is at least 2" do
      CopilotSDK::MIN_PROTOCOL_VERSION.should be >= 2
    end
  end
end

describe CopilotSDK::ConnectionState do
  it "has expected enum values" do
    CopilotSDK::ConnectionState::Disconnected.value.should eq(0)
    CopilotSDK::ConnectionState::Connected.value.should eq(2)
  end
end

describe CopilotSDK::ToolResultType do
  it "has expected enum values" do
    CopilotSDK::ToolResultType::Success.to_s.should eq("Success")
    CopilotSDK::ToolResultType::Failure.to_s.should eq("Failure")
    CopilotSDK::ToolResultType::Rejected.to_s.should eq("Rejected")
    CopilotSDK::ToolResultType::Denied.to_s.should eq("Denied")
  end
end

describe CopilotSDK::ToolDefinition do
  it "serializes to JSON" do
    tool = CopilotSDK::ToolDefinition.new(
      name: "test_tool",
      description: "A test tool"
    )
    json = tool.to_json
    parsed = JSON.parse(json)
    parsed["name"].as_s.should eq("test_tool")
    parsed["description"].as_s.should eq("A test tool")
  end

  it "deserializes from JSON" do
    json = %Q({"name":"my_tool","description":"desc"})
    tool = CopilotSDK::ToolDefinition.from_json(json)
    tool.name.should eq("my_tool")
    tool.description.should eq("desc")
    tool.parameters.should be_nil
  end

  it "round-trips with parameters" do
    params = JSON.parse(%Q({"type":"object","properties":{"x":{"type":"string"}}}))
    tool = CopilotSDK::ToolDefinition.new(
      name: "param_tool",
      description: "Has params",
      parameters: params
    )
    restored = CopilotSDK::ToolDefinition.from_json(tool.to_json)
    restored.name.should eq("param_tool")
    restored.parameters.should_not be_nil
  end
end

describe CopilotSDK::SessionEvent do
  it "extracts assistant message content" do
    event = CopilotSDK::SessionEvent.new(
      type: "assistant.message",
      data: JSON.parse(%Q({"content":"Hello there"}))
    )
    event.assistant_message_content.should eq("Hello there")
  end

  it "returns nil for non-message events" do
    event = CopilotSDK::SessionEvent.new(type: "turn.end")
    event.assistant_message_content.should be_nil
  end

  it "extracts delta text" do
    event = CopilotSDK::SessionEvent.new(
      type: "assistant.message_delta",
      data: JSON.parse(%Q({"delta":"chunk"}))
    )
    event.delta_text.should eq("chunk")
  end

  it "returns nil for non-delta event types" do
    event = CopilotSDK::SessionEvent.new(
      type: "assistant.message",
      data: JSON.parse(%Q({"delta":"chunk"}))
    )
    event.delta_text.should be_nil
  end
end

describe CopilotSDK::SessionConfig do
  it "serializes with defaults" do
    config = CopilotSDK::SessionConfig.new
    json = config.to_json
    parsed = JSON.parse(json)
    parsed["model"].as_s?.should be_nil
    parsed["streaming"].as_bool?.should be_nil
  end

  it "serializes with all fields" do
    config = CopilotSDK::SessionConfig.new(
      model: "gpt-4",
      streaming: true,
      system_message: "Be helpful",
      agent_mode: "full"
    )
    parsed = JSON.parse(config.to_json)
    parsed["model"].as_s.should eq("gpt-4")
    parsed["streaming"].as_bool.should be_true
    parsed["systemMessage"].as_s.should eq("Be helpful")
    parsed["agentMode"].as_s.should eq("full")
  end
end

describe CopilotSDK::MessageOptions do
  it "serializes prompt" do
    opts = CopilotSDK::MessageOptions.new(prompt: "Hello")
    parsed = JSON.parse(opts.to_json)
    parsed["prompt"].as_s.should eq("Hello")
  end
end

describe CopilotSDK::ToolResultObject do
  it "serializes with camelCase keys" do
    result = CopilotSDK::ToolResultObject.new(
      text_result_for_llm: "output",
      result_type: "success"
    )
    parsed = JSON.parse(result.to_json)
    parsed["textResultForLlm"].as_s.should eq("output")
    parsed["resultType"].as_s.should eq("success")
  end
end

describe CopilotSDK::PermissionRequestResult do
  it "serializes allowed" do
    result = CopilotSDK::PermissionRequestResult.new(allowed: true)
    parsed = JSON.parse(result.to_json)
    parsed["allowed"].as_bool.should be_true
  end

  it "serializes denied with reason" do
    result = CopilotSDK::PermissionRequestResult.new(allowed: false, reason: "nope")
    parsed = JSON.parse(result.to_json)
    parsed["allowed"].as_bool.should be_false
    parsed["reason"].as_s.should eq("nope")
  end
end

describe CopilotSDK::ModelInfo do
  it "deserializes from JSON" do
    json = %Q({"id":"gpt-4","displayName":"GPT-4","vendor":"openai"})
    model = CopilotSDK::ModelInfo.from_json(json)
    model.id.should eq("gpt-4")
    model.display_name.should eq("GPT-4")
    model.vendor.should eq("openai")
  end
end

describe CopilotSDK::GetStatusResponse do
  it "deserializes protocol version" do
    json = %Q({"version":"1.0","protocolVersion":3,"status":"ok"})
    status = CopilotSDK::GetStatusResponse.from_json(json)
    status.version.should eq("1.0")
    status.protocol_version.should eq(3)
    status.status.should eq("ok")
  end
end

describe "Permission helpers" do
  it "deny_all returns denied" do
    request = CopilotSDK::PermissionRequest.new(
      session_id: "s1", resource: "file", action: "read"
    )
    result = CopilotSDK.deny_all_permissions(request, "s1")
    result.allowed.should be_false
  end

  it "approve_all returns allowed" do
    request = CopilotSDK::PermissionRequest.new(
      session_id: "s1", resource: "file", action: "write"
    )
    result = CopilotSDK.approve_all_permissions(request, "s1")
    result.allowed.should be_true
  end
end

describe CopilotSDK::ToolBuilder do
  it "builds a tool definition" do
    builder = CopilotSDK::ToolBuilder.new("my_tool")
    builder.description = "A sample tool"
    builder.parameter("name", "string", "User name", required: true)
    builder.parameter("verbose", "boolean", "Enable verbose output")

    defn = builder.build_definition
    defn.name.should eq("my_tool")
    defn.description.should eq("A sample tool")
    defn.parameters.should_not be_nil

    params = defn.parameters.not_nil!
    params["type"].as_s.should eq("object")
    params["properties"]["name"]["type"].as_s.should eq("string")
    params["required"].as_a.map(&.as_s).should contain("name")
    params["required"].as_a.map(&.as_s).should_not contain("verbose")
  end

  it "raises if no handler set" do
    builder = CopilotSDK::ToolBuilder.new("no_handler")
    expect_raises(Exception, /No handler/) do
      builder.build_handler
    end
  end
end

describe "CopilotSDK.define_tool" do
  it "returns definition and handler" do
    defn, handler = CopilotSDK.define_tool("echo") do |t|
      t.description = "Echoes input"
      t.parameter("text", "string", "Text to echo", required: true)
      t.handler do |args, _invocation|
        JSON::Any.new(args["text"].as_s)
      end
    end

    defn.name.should eq("echo")
    defn.description.should eq("Echoes input")

    invocation = CopilotSDK::ToolInvocation.new(
      session_id: "s1", tool_call_id: "tc1", tool_name: "echo",
      arguments: JSON.parse(%Q({"text":"hello"}))
    )
    result = handler.call(JSON.parse(%Q({"text":"hello"})), invocation)
    result.as_s.should eq("hello")
  end
end

describe CopilotSDK::CopilotSession do
  it "can register and fire event handlers" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    received_events = [] of String
    sub_id = session.on do |event|
      received_events << event.type
    end

    session.dispatch_event(CopilotSDK::SessionEvent.new(type: "assistant.message"))
    session.dispatch_event(CopilotSDK::SessionEvent.new(type: "turn.end"))

    received_events.should eq(["assistant.message", "turn.end"])

    session.off(sub_id)
    session.dispatch_event(CopilotSDK::SessionEvent.new(type: "after.unsub"))
    received_events.size.should eq(2) # no new events after unsub

    reader.close
    writer.close
  end

  it "filters events by type" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    messages = [] of String
    session.on("assistant.message") do |event|
      messages << (event.assistant_message_content || "")
    end

    session.dispatch_event(CopilotSDK::SessionEvent.new(
      type: "assistant.message",
      data: JSON.parse(%Q({"content":"hi"}))
    ))
    session.dispatch_event(CopilotSDK::SessionEvent.new(type: "turn.end"))

    messages.should eq(["hi"])

    reader.close
    writer.close
  end

  it "handles tool calls" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    session.define_tool("greet") do |t|
      t.description = "Greet a user"
      t.parameter("name", "string", required: true)
      t.handler do |args, _invocation|
        JSON::Any.new("Hello, #{args["name"].as_s}!")
      end
    end

    payload = CopilotSDK::ToolCallRequestPayload.new(
      session_id: "test-session",
      tool_call_id: "tc-1",
      tool_name: "greet",
      arguments: JSON.parse(%Q({"name":"Crystal"}))
    )

    result = session.handle_tool_call(payload)
    result.as_s.should eq("Hello, Crystal!")

    reader.close
    writer.close
  end

  it "returns error for unknown tools" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    payload = CopilotSDK::ToolCallRequestPayload.new(
      session_id: "test-session",
      tool_call_id: "tc-1",
      tool_name: "nonexistent",
      arguments: JSON.parse("{}")
    )

    result = session.handle_tool_call(payload)
    result["error"].as_s.should contain("Unknown tool")

    reader.close
    writer.close
  end

  it "uses default deny permission handler" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    request = CopilotSDK::PermissionRequest.new(
      session_id: "test-session", resource: "file", action: "write"
    )
    result = session.handle_permission_request(request)
    result.allowed.should be_false

    reader.close
    writer.close
  end

  it "uses custom permission handler" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    session.on_permission do |_req, _sid|
      CopilotSDK::PermissionRequestResult.new(allowed: true, reason: "approved")
    end

    request = CopilotSDK::PermissionRequest.new(
      session_id: "test-session", resource: "file", action: "write"
    )
    result = session.handle_permission_request(request)
    result.allowed.should be_true
    result.reason.should eq("approved")

    reader.close
    writer.close
  end

  it "tracks destroyed state" do
    reader, writer = IO.pipe
    rpc = CopilotSDK::JsonRpcClient.new(reader: reader, writer: writer)
    session = CopilotSDK::CopilotSession.new("test-session", rpc)

    session.destroyed?.should be_false
    # Calling destroy without a real server connection is best-effort
    session.destroy
    session.destroyed?.should be_true

    reader.close
    writer.close
  end
end

describe CopilotSDK::CopilotClientOptions do
  it "has sensible defaults" do
    opts = CopilotSDK::CopilotClientOptions.new
    opts.cli_path.should be_nil
    opts.cli_url.should be_nil
    opts.auto_start.should be_true
    opts.request_timeout.should eq(30)
  end

  it "accepts custom values" do
    opts = CopilotSDK::CopilotClientOptions.new(
      cli_path: "/bin/cli",
      cli_url: "localhost:9000",
      auto_start: false,
      request_timeout: 60
    )
    opts.cli_path.should eq("/bin/cli")
    opts.cli_url.should eq("localhost:9000")
    opts.auto_start.should be_false
    opts.request_timeout.should eq(60)
  end
end

describe CopilotSDK::CopilotClient do
  it "starts in disconnected state" do
    client = CopilotSDK::CopilotClient.new(
      CopilotSDK::CopilotClientOptions.new(auto_start: false)
    )
    client.state.should eq(CopilotSDK::ConnectionState::Disconnected)
    client.connected?.should be_false
  end

  it "raises when operations called while disconnected" do
    client = CopilotSDK::CopilotClient.new(
      CopilotSDK::CopilotClientOptions.new(auto_start: false)
    )
    expect_raises(CopilotSDK::CopilotError, /not connected/) do
      client.get_status
    end
  end

  it "returns nil for unknown session" do
    client = CopilotSDK::CopilotClient.new(
      CopilotSDK::CopilotClientOptions.new(auto_start: false)
    )
    client.get_session("nonexistent").should be_nil
  end
end

describe CopilotSDK::JsonRpcClient do
  it "formats Content-Length framed messages" do
    # Verify the client can write and read framed messages through pipes
    server_reader, client_writer = IO.pipe
    client_reader, server_writer = IO.pipe

    rpc = CopilotSDK::JsonRpcClient.new(reader: client_reader, writer: client_writer)

    # Write a notification and verify the framing on the other end
    rpc.send_notification("test/hello", JSON.parse(%Q({"msg":"hi"})))

    raw = server_reader.gets('\n', chomp: false)
    raw.should_not be_nil
    raw.not_nil!.should start_with("Content-Length:")

    server_reader.close
    server_writer.close
    client_reader.close
    client_writer.close
  end
end
