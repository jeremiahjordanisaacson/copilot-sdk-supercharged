/// Cookbook: Multiple Sessions
///
/// Demonstrates running multiple conversation sessions in parallel
/// from a single CopilotClient instance.
module cookbook.multiple_sessions;

import copilot;
import std.stdio : writeln, writefln;
import std.json : JSONValue;
import core.thread : Thread;

void main()
{
    auto client = new CopilotClient();
    scope(exit) client.stop();

    // Create two independent sessions with different system prompts.
    auto configA = SessionConfig.init;
    configA.systemMessage = "You are a pirate. Speak accordingly.";

    auto configB = SessionConfig.init;
    configB.systemMessage = "You are a formal British butler.";

    auto sessionA = client.createSession(configA);
    auto sessionB = client.createSession(configB);

    // Collect responses from both.
    string responseA, responseB;

    sessionA.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto c = ev.stringField("content");
            if (!c.isNull) responseA = c.get;
        }
    });

    sessionB.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto c = ev.stringField("content");
            if (!c.isNull) responseB = c.get;
        }
    });

    // Send to both sessions. In a real app you could use std.parallelism
    // or std.concurrency to run these concurrently.
    sessionA.sendAndWait("Greet me.");
    sessionB.sendAndWait("Greet me.");

    writeln("Pirate says: ", responseA);
    writeln("Butler says: ", responseB);
    writefln!"Session IDs: %s, %s"(sessionA.sessionId, sessionB.sessionId);
}
