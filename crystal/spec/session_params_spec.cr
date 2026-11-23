# Copyright (c) Microsoft Corporation. All rights reserved.

require "./spec_helper"

describe "CopilotSDK.group_session_params" do
  it "groups a flat resume payload for SDK protocol 4" do
    flat = {
      "sessionId"             => JSON::Any.new("s1"),
      "model"                 => JSON::Any.new("m"),
      "name"                  => JSON::Any.new("create-only"),
      "streaming"             => JSON::Any.new(true),
      "hooks"                 => JSON::Any.new(true),
      "disableResume"         => JSON::Any.new(true),
      "configDir"             => JSON::Any.new("/cfg"),
      "trustWorkingDirectory" => JSON::Any.new("session"),
      "tools"                 => JSON.parse(%([{"name":"t"}])),
      "mcpServers"            => JSON.parse(%({"srv":{"command":"x"}})),
      "unknownKey"            => JSON::Any.new(1_i64),
    }

    payload = CopilotSDK.group_session_params(flat, true)

    payload["sessionId"].as_s.should eq("s1")
    payload["model"].as_s.should eq("m")
    payload["suppressResumeEvent"].as_bool.should be_true
    payload["configDirectory"].as_s.should eq("/cfg")
    payload["unknownKey"].as_i.should eq(1)
    payload.has_key?("name").should be_false
    payload.has_key?("streaming").should be_false
    payload.has_key?("filesystem").should be_false
    payload["events"]["streaming"].as_bool.should be_true
    payload["hooks"]["hasHandlers"].as_bool.should be_true
    payload["tools"]["definitions"][0]["name"].as_s.should eq("t")
    payload["mcp"]["envValueMode"].as_s.should eq("direct")
  end

  it "translates a legacy provider into a default named provider" do
    flat = {
      "model"    => JSON::Any.new("gpt-x"),
      "provider" => JSON.parse(%({"type":"openai","baseUrl":"https://example.test","maxOutputTokens":100})),
    }

    payload = CopilotSDK.group_session_params(flat, false)

    payload.has_key?("provider").should be_false
    connection = payload["inference"]["providers"][0]
    connection["name"].as_s.should eq("default")
    connection["baseUrl"].as_s.should eq("https://example.test")
    connection.as_h.has_key?("maxOutputTokens").should be_false
    payload["inference"]["defaultProvider"].as_s.should eq("default")
    payload["inference"]["models"][0]["id"].as_s.should eq("gpt-x")
    payload["inference"]["models"][0]["maxOutputTokens"].as_i.should eq(100)
    payload["github"]["capi"]["enabled"].as_bool.should be_false
  end
end
