/// Unit tests for the Copilot D SDK.
///
/// Tests exercise JSON serialization, type helpers, tool definitions,
/// the JSON-RPC framing logic, and session event dispatch.
module copilot.client_test;

import std.json;
import std.typecons : Nullable;
import std.conv : to;

import copilot.types;
import copilot.jsonrpc;
import copilot.session;
import copilot.tools;

// ---------------------------------------------------------------------------
// Type serialization tests
// ---------------------------------------------------------------------------

unittest
{
    // SessionConfig default serialization.
    auto cfg = SessionConfig.init;
    auto j = cfg.toJson();
    assert(j["streaming"].type == JSONType.false_);
    assert(j["infiniteSessions"].type == JSONType.true_);
    assert("systemMessage" !in j);
    assert("provider" !in j);
    assert("tools" !in j);
}

unittest
{
    // SessionConfig with all fields populated.
    auto cfg = SessionConfig(
        true,                                       // streaming
        Nullable!string("You are a helpful assistant."), // systemMessage
        Nullable!ProviderConfig(ProviderConfig(
            Nullable!string("gpt-4"),
            Nullable!string("https://api.example.com"),
        )),
        [Tool("greet", "Say hello", JSONValue(["type": "object"]))],
        false,                                      // infiniteSessions
    );
    auto j = cfg.toJson();
    assert(j["streaming"].type == JSONType.true_);
    assert(j["infiniteSessions"].type == JSONType.false_);
    assert(j["systemMessage"].str == "You are a helpful assistant.");
    assert(j["provider"]["model"].str == "gpt-4");
    assert(j["provider"]["baseUrl"].str == "https://api.example.com");
    assert(j["tools"].array.length == 1);
    assert(j["tools"][0]["name"].str == "greet");
}

unittest
{
    // ResumeSessionConfig serialization.
    auto cfg = ResumeSessionConfig("sess-42", true);
    auto j = cfg.toJson();
    assert(j["sessionId"].str == "sess-42");
    assert(j["streaming"].type == JSONType.true_);
}

// ---------------------------------------------------------------------------
// Tool and ToolResult tests
// ---------------------------------------------------------------------------

unittest
{
    // Tool.toJson round-trip.
    auto t = Tool("calculator", "Do math", parseJSON(`{"type":"object","properties":{"expr":{"type":"string"}}}`));
    auto j = t.toJson();
    assert(j["name"].str == "calculator");
    assert(j["description"].str == "Do math");
    assert(j["parameters"]["type"].str == "object");
}

unittest
{
    // ToolResult success serialization.
    auto r = ToolResult("call-1", "42", false);
    auto j = r.toJson();
    assert(j["id"].str == "call-1");
    assert(j["result"].str == "42");
    assert("isError" !in j);
}

unittest
{
    // ToolResult error serialization.
    auto r = ToolResult("call-2", "boom", true);
    auto j = r.toJson();
    assert(j["isError"].type == JSONType.true_);
}

// ---------------------------------------------------------------------------
// defineTool helper tests
// ---------------------------------------------------------------------------

unittest
{
    auto binding = defineTool(
        "echo",
        "Echo input",
        `{"type":"object","properties":{"text":{"type":"string"}}}`,
        (string callId, JSONValue args) @safe {
            return successResult(callId, "echoed");
        },
    );
    assert(binding.tool.name == "echo");
    assert(binding.tool.description == "Echo input");

    auto result = binding.handler("id-1", JSONValue(null));
    assert(result.result == "echoed");
    assert(!result.isError);
}

unittest
{
    // errorResult helper.
    auto r = errorResult("id-x", "something failed");
    assert(r.isError);
    assert(r.result == "something failed");
}

// ---------------------------------------------------------------------------
// SessionEvent tests
// ---------------------------------------------------------------------------

unittest
{
    // parseEventType known values.
    assert(parseEventType("assistant.message") == SessionEventType.assistantMessage);
    assert(parseEventType("session.idle") == SessionEventType.sessionIdle);
    assert(parseEventType("session.compaction_start") == SessionEventType.compactionStart);
    assert(parseEventType("not.real") == SessionEventType.unknown);
}

unittest
{
    // SessionEvent.stringField extraction.
    auto payload = parseJSON(`{"content":"hello","number":42}`);
    auto ev = SessionEvent(SessionEventType.assistantMessage, payload);
    assert(!ev.stringField("content").isNull);
    assert(ev.stringField("content").get == "hello");
    assert(ev.stringField("number").isNull); // not a string
    assert(ev.stringField("missing").isNull);
}

// ---------------------------------------------------------------------------
// SessionInfo / CopilotStatus from JSON
// ---------------------------------------------------------------------------

unittest
{
    auto j = parseJSON(`{"sessionId":"abc-123"}`);
    auto info = SessionInfo.fromJson(j);
    assert(info.sessionId == "abc-123");
}

unittest
{
    auto j = parseJSON(`{"status":"ready","version":"1.2.3"}`);
    auto s = CopilotStatus.fromJson(j);
    assert(s.status == "ready");
    assert(!s.version_.isNull);
    assert(s.version_.get == "1.2.3");
}

unittest
{
    // Missing fields should not crash.
    auto j = parseJSON(`{}`);
    auto info = SessionInfo.fromJson(j);
    assert(info.sessionId == "");
    auto s = CopilotStatus.fromJson(j);
    assert(s.status == "");
    assert(s.version_.isNull);
}

// ---------------------------------------------------------------------------
// ProviderConfig serialization
// ---------------------------------------------------------------------------

unittest
{
    auto p = ProviderConfig(Nullable!string("gpt-4"), Nullable!string.init);
    auto j = p.toJson();
    assert(j["model"].str == "gpt-4");
    assert("baseUrl" !in j);
}

// ---------------------------------------------------------------------------
// CopilotClientOptions defaults
// ---------------------------------------------------------------------------

unittest
{
    auto o = CopilotClientOptions.init;
    assert(o.cliPath == "github-copilot");
    assert(o.useStdio == true);
    assert(o.logLevel == "info");
    assert(o.autoStart == true);
    assert(o.autoRestart == false);
    assert(o.useLoggedInUser == true);
    assert(o.cliUrl.isNull);
    assert(o.port.isNull);
    assert(o.gitHubToken.isNull);
}

// ---------------------------------------------------------------------------
// MessageOptions defaults
// ---------------------------------------------------------------------------

unittest
{
    auto m = MessageOptions.init;
    assert(m.attachments.length == 0);
    assert(m.responseFormat.isNull);
}

// ---------------------------------------------------------------------------
// JsonRpcError
// ---------------------------------------------------------------------------

unittest
{
    bool caught = false;
    try
    {
        throw new JsonRpcError("test error");
    }
    catch (JsonRpcError e)
    {
        assert(e.msg == "test error");
        caught = true;
    }
    assert(caught);
}

// ---------------------------------------------------------------------------
// Main for running unit tests from DUB.
// ---------------------------------------------------------------------------

version (unittest)
{
    void main() {}
}
