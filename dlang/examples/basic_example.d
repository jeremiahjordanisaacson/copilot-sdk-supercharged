/// Basic example demonstrating the Copilot D SDK.
///
/// Build with:
///   cd dlang && dub run --config=library -- examples/basic_example.d
///
/// Or compile directly:
///   dmd -I../dlang/source examples/basic_example.d
module basic_example;

import copilot;
import std.stdio : writeln, writefln;
import std.json : JSONValue;

void main()
{
    // 1. Configure the client.
    auto opts = CopilotClientOptions.init;
    opts.autoStart = true;
    opts.logLevel = "warn";

    auto client = new CopilotClient(opts);
    scope(exit) client.stop();

    // 2. Create a session with a system message.
    auto config = SessionConfig.init;
    config.systemMessage = "You are a concise assistant.";
    config.streaming = false;

    auto session = client.createSession(config);

    // 3. Listen for events.
    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto content = ev.stringField("content");
            if (!content.isNull)
                writeln("Assistant: ", content.get);
        }
    });

    // 4. Send a message and wait for the response.
    auto events = session.sendAndWait("What is the D programming language?");

    writefln!"Received %d events."(events.length);
}
