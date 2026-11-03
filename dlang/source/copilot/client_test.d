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
// v2.6.0 upstream-sync features
// ---------------------------------------------------------------------------

unittest
{
    // JsonRpcError carries the JSON-RPC error code.
    auto e = new JsonRpcError("Method not found", methodNotFoundCode);
    assert(e.code == -32601);
    assert(new JsonRpcError("plain").code == 0);
}

unittest
{
    // capi carries the auto tier and WebSocket responses; nothing new by default.
    auto plain = SessionConfig.init.toJson();
    assert("capi" !in plain);
    assert("enableWebSocketResponses" !in plain);
    assert("hooks" !in plain);
    assert("featureFlags" !in plain);
    assert("gitHubTokenProviderRegistrationId" !in plain);

    SessionConfig cfg;
    cfg.autoTier = AutoTier.intelligence;
    cfg.enableWebSocketResponses = true;
    auto j = cfg.toJson();
    assert(j["capi"]["autoTier"].str == "intelligence");
    assert(j["capi"]["enableWebSocketResponses"].type == JSONType.true_);
    assert("enableWebSocketResponses" !in j);
}

unittest
{
    // Session create options.
    SessionConfig cfg;
    cfg.askUserVariant = AskUserVariant.elicitation;
    cfg.featureFlags = (bool[string]).init;
    cfg.allowedModels = ["gpt-5", "claude-sonnet-4.6"];
    cfg.authClientIdMetadataUrl = "https://example.com/client.json";
    cfg.name = "triage";
    cfg.agentDirectories = ["/agents"];
    cfg.hookDirectories = ["/hooks"];
    cfg.interactiveUser = true;
    cfg.excludedManagedSettings = ["model"];
    cfg.enforceManagedModelDefaults = true;
    cfg.trustWorkingDirectory = TrustWorkingDirectory.session;
    cfg.refreshCustomInstructions = true;

    SandboxConfig sandbox;
    sandbox.enabled = true;
    sandbox.allowBypass = true;
    sandbox.sandboxMcpServers = false;
    cfg.sandboxConfig = sandbox;

    ImageGenerationConfig images;
    images.enabled = true;
    images.saveToDisk = false;
    cfg.imageGeneration = images;

    AutopilotConfig autopilot;
    autopilot.continuationEnabled = true;
    autopilot.maxContinues = 3;
    cfg.autopilot = autopilot;

    auto j = cfg.toJson();
    assert(j["askUserVariant"].str == "elicitation");
    assert(j["featureFlags"].type == JSONType.object);
    assert(j["featureFlags"].toString() == "{}");
    assert(j["allowedModels"][1].str == "claude-sonnet-4.6");
    assert(j["authClientIdMetadataUrl"].str == "https://example.com/client.json");
    assert(j["name"].str == "triage");
    assert(j["agentDirectories"][0].str == "/agents");
    assert(j["hookDirectories"][0].str == "/hooks");
    assert(j["interactiveUser"].type == JSONType.true_);
    assert(j["excludedManagedSettings"][0].str == "model");
    assert(j["enforceManagedModelDefaults"].type == JSONType.true_);
    assert(j["trustWorkingDirectory"].str == "session");
    assert(j["refreshCustomInstructions"].type == JSONType.true_);
    assert(j["sandboxConfig"]["enabled"].type == JSONType.true_);
    assert(j["sandboxConfig"]["allowBypass"].type == JSONType.true_);
    assert(j["sandboxConfig"]["sandboxMcpServers"].type == JSONType.false_);
    assert("addCurrentWorkingDirectory" !in j["sandboxConfig"]);
    assert(j["imageGeneration"]["enabled"].type == JSONType.true_);
    assert(j["imageGeneration"]["saveToDisk"].type == JSONType.false_);
    assert("watermark" !in j["imageGeneration"]);
    assert(j["autopilot"]["continuationEnabled"].type == JSONType.true_);
    assert(j["autopilot"]["maxContinues"].integer == 3);
}

unittest
{
    // Session resume options.
    ResumeSessionConfig cfg;
    cfg.sessionId = "sess-1";
    cfg.autoTier = AutoTier.fast;
    cfg.allowTranscriptRecovery = true;
    cfg.featureFlags = ["beta": true];
    auto j = cfg.toJson();
    assert(j["capi"]["autoTier"].str == "fast");
    assert("enableWebSocketResponses" !in j["capi"]);
    assert(j["allowTranscriptRecovery"].type == JSONType.true_);
    assert(j["featureFlags"]["beta"].type == JSONType.true_);
    assert("name" !in j);
    assert("trustWorkingDirectory" !in j);
}

unittest
{
    // Sub-agent hooks enable the hooks flag and use typed inputs/outputs.
    SessionConfig cfg;
    cfg.onSubagentStart = (SubagentStartHookInput input) {
        SubagentStartHookOutput output;
        output.additionalContext = "context for " ~ input.agentName;
        return output;
    };
    assert(cfg.toJson()["hooks"].type == JSONType.true_);

    auto start = SubagentStartHookInput.fromJson(parseJSON(
        `{"sessionId":"s1","timestamp":1700000000000,"workingDirectory":"/w",` ~
        `"transcriptPath":"/t","agentName":"explore","agentDisplayName":"Explore"}`));
    assert(start.sessionId == "s1");
    assert(start.timestamp == 1700000000000);
    assert(start.agentName == "explore");
    assert(start.agentDisplayName.get == "Explore");
    assert(start.agentDescription.isNull);
    assert(cfg.onSubagentStart(start).toJson()["additionalContext"].str == "context for explore");

    auto stop = SubagentStopHookInput.fromJson(parseJSON(
        `{"sessionId":"s1","agentName":"explore","agentType":"explore",` ~
        `"stopReason":"end_turn","response":"done","agentId":"a-1"}`));
    assert(stop.agentType == "explore");
    assert(stop.stopReason == "end_turn");
    assert(stop.response == "done");
    assert(stop.agentId.get == "a-1");

    SubagentStopHookOutput out_;
    out_.decision = "block";
    out_.reason = "keep going";
    auto oj = out_.toJson();
    assert(oj["decision"].str == "block");
    assert(oj["reason"].str == "keep going");
    assert("modifiedResponse" !in oj);
}

unittest
{
    // Message source, client info, token results, BYOK provider, model limits.
    assert(MessageSource.user == "user");
    assert(MessageSource.system == "system");
    assert(MessageSource.agent("abc") == "agent-abc");

    auto m = MessageOptions.init;
    assert(m.source.isNull);
    assert(m.responseSchema.type == JSONType.null_);

    assert(ClientInfo.init.toWire().type == JSONType.null_);
    auto wire = ClientInfo("acme-portal", "2.4.0", "", "").toWire();
    assert(wire["editorName"].str == "acme-portal");
    assert(wire["editorVersion"].str == "2.4.0");
    assert("extensionName" !in wire);

    auto tok = GitHubTokenProviderResult.token("gho_x", 28_800).toJson();
    assert(tok["kind"].str == "token");
    assert(tok["accessToken"].str == "gho_x");
    assert(tok["expiresIn"].integer == 28_800);
    assert("tokenType" !in tok);
    auto cancelled = GitHubTokenProviderResult.cancelled().toJson();
    assert(cancelled["kind"].str == "cancelled");
    assert("accessToken" !in cancelled);

    ProviderConfig provider;
    provider.baseUrl = "http://localhost:11434/v1";
    provider.modelProvider = ModelProvider.ollama;
    provider.apiKeyCommand = "print-key";
    auto pj = provider.toJson();
    assert(pj["modelProvider"].str == "ollama");
    assert(pj["apiKeyCommand"].str == "print-key");

    ModelLimitsOverride limits;
    limits.maxOutputTokens = 4096;
    auto caps = limits.toModelCapabilities();
    assert(caps["limits"]["max_output_tokens"].integer == 4096);
    assert("max_prompt_tokens" !in caps["limits"]);
}

// ---------------------------------------------------------------------------
// Main for running unit tests from DUB.
// ---------------------------------------------------------------------------

version (unittest)
{
    void main() {}
}
