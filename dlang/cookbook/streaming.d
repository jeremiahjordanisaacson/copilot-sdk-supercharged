/// Cookbook: Streaming
///
/// Demonstrates handling streaming responses where the assistant sends
/// incremental deltas before the final message.
module cookbook.streaming;

import copilot;
import std.stdio : write, writeln, stdout;
import std.json : JSONValue;
import std.array : Appender;

void main()
{
    auto client = new CopilotClient();
    scope(exit) client.stop();

    // Enable streaming in the session config.
    auto cfg = SessionConfig.init;
    cfg.streaming = true;
    cfg.systemMessage = "You are a storyteller. Tell short fables.";

    auto session = client.createSession(cfg);

    // Accumulate the full response from deltas.
    Appender!string fullResponse;
    bool reasoningPhase = false;

    session.addEventListener((SessionEvent ev) {
        final switch (ev.type) with (SessionEventType)
        {
            case assistantMessageDelta:
                // Incremental content chunk.
                auto delta = ev.stringField("content");
                if (!delta.isNull)
                {
                    write(delta.get);  // Print without newline for streaming effect.
                    stdout.flush();
                    fullResponse ~= delta.get;
                }
                break;

            case assistantMessage:
                // Final assembled message (also sent after all deltas).
                writeln();  // End the streaming line.
                writeln("--- Full message received ---");
                break;

            case assistantReasoningDelta:
                if (!reasoningPhase)
                {
                    writeln("[reasoning...]");
                    reasoningPhase = true;
                }
                break;

            case assistantReasoning:
                reasoningPhase = false;
                writeln("[reasoning complete]");
                break;

            case sessionIdle:
                writeln("[session idle]");
                break;

            case toolCall, sessionError, sessionEnd,
                 compactionStart, compactionComplete, unknown:
                break;
        }
    });

    session.sendAndWait("Tell me a short fable about a turtle and a hawk.");
    writeln("Total length: ", fullResponse[].length, " characters");
}
