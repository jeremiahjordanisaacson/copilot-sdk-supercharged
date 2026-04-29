/// Cookbook: Error Handling
///
/// Demonstrates robust error handling patterns using D's scope guards,
/// exception chaining, and Nullable checks.
module cookbook.error_handling;

import copilot;
import std.stdio : writeln, writefln;
import std.json : JSONValue;
import std.typecons : Nullable;

void main()
{
    // Use scope(exit) to guarantee cleanup regardless of exceptions.
    CopilotClient client;
    try
    {
        auto opts = CopilotClientOptions.init;
        opts.autoStart = false; // Manual start for explicit error handling.
        client = new CopilotClient(opts);
    }
    catch (Exception e)
    {
        writeln("Failed to create client: ", e.msg);
        return;
    }
    scope(exit)
    {
        if (client !is null)
            client.stop();
        writeln("Client cleaned up.");
    }

    // Start with retry logic.
    enum MAX_RETRIES = 3;
    foreach (attempt; 0 .. MAX_RETRIES)
    {
        try
        {
            client.start();
            break;
        }
        catch (Exception e)
        {
            writefln!"Start attempt %d failed: %s"(attempt + 1, e.msg);
            if (attempt == MAX_RETRIES - 1)
            {
                writeln("All start attempts exhausted.");
                return;
            }
        }
    }

    // Ping to verify connectivity.
    if (!client.ping())
    {
        writeln("CLI not reachable after start.");
        return;
    }

    // Create a session with error handling.
    CopilotSession session;
    try
    {
        session = client.createSession();
    }
    catch (Exception e)
    {
        writeln("Session creation failed: ", e.msg);
        return;
    }

    // Listen for error events.
    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.sessionError)
        {
            auto msg = ev.stringField("message");
            writeln("Session error: ", msg.isNull ? "unknown" : msg.get);
        }
    });

    // Send with per-message error handling.
    try
    {
        auto events = session.sendAndWait("Hello");
        writefln!"Got %d events."(events.length);
    }
    catch (Exception e)
    {
        writeln("Send failed: ", e.msg);
    }
}
