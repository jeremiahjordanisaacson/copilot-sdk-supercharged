/// Cookbook: Tool Usage
///
/// Demonstrates defining custom tools, registering them with a session,
/// and handling tool invocations from the assistant.
module cookbook.tool_usage;

import copilot;
import copilot.tools;
import std.stdio : writeln, writefln;
import std.json : JSONValue, parseJSON;
import std.conv : to;
import std.datetime : Clock;

void main()
{
    auto client = new CopilotClient();
    scope(exit) client.stop();

    // Define a calculator tool.
    auto calculator = defineTool(
        "calculate",
        "Evaluate a simple arithmetic expression",
        `{
            "type": "object",
            "properties": {
                "expression": {
                    "type": "string",
                    "description": "Arithmetic expression like '2 + 2'"
                }
            },
            "required": ["expression"]
        }`,
        (string callId, JSONValue args) @safe {
            auto expr = args["expression"].str;
            // In a real implementation, parse and evaluate the expression.
            return successResult(callId, "Result of " ~ expr ~ " = 42");
        },
    );

    // Define a current-time tool.
    auto timeTool = defineTool(
        "current_time",
        "Get the current date and time",
        `{
            "type": "object",
            "properties": {}
        }`,
        (string callId, JSONValue args) @safe {
            auto now = (() @trusted => Clock.currTime().toISOExtString())();
            return successResult(callId, now);
        },
    );

    // Create a session with both tools registered.
    auto cfg = SessionConfig.init;
    cfg.systemMessage = "You have access to a calculator and clock. Use them when asked.";

    auto session = client.createSession(cfg, [calculator, timeTool]);

    // Listen for responses.
    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto c = ev.stringField("content");
            if (!c.isNull)
                writeln("Assistant: ", c.get);
        }
        else if (ev.type == SessionEventType.toolCall)
        {
            auto name = ev.stringField("name");
            writeln("[tool called: ", name.isNull ? "?" : name.get, "]");
        }
    });

    session.sendAndWait("What time is it, and what is 123 * 456?");
}
