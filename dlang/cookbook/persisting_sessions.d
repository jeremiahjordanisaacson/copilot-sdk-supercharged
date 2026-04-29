/// Cookbook: Persisting Sessions
///
/// Shows how to create a session, save its ID, then resume it later
/// using the infinite sessions feature and sessionFs.
module cookbook.persisting_sessions;

import copilot;
import std.stdio : writeln, writefln, File;
import std.json : JSONValue;
import std.file : exists, readText, write;

void main()
{
    enum SESSION_FILE = "session_id.txt";

    auto opts = CopilotClientOptions.init;
    // Point sessionFs at a local directory for persistence.
    opts.sessionFs = ".copilot-sessions";

    auto client = new CopilotClient(opts);
    scope(exit) client.stop();

    CopilotSession session;

    if (exists(SESSION_FILE))
    {
        // Resume an existing session.
        string savedId = readText(SESSION_FILE);
        writeln("Resuming session: ", savedId);

        auto resumeCfg = ResumeSessionConfig(savedId, false);
        session = client.resumeSession(resumeCfg);
    }
    else
    {
        // Create a fresh session with infinite sessions enabled.
        auto cfg = SessionConfig.init;
        cfg.infiniteSessions = true;
        cfg.systemMessage = "You are a persistent helper. Remember our history.";

        session = client.createSession(cfg);

        // Save the session ID to disk.
        write(SESSION_FILE, session.sessionId);
        writeln("Created session: ", session.sessionId);
    }

    // Attach a listener.
    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto c = ev.stringField("content");
            if (!c.isNull) writeln("Assistant: ", c.get);
        }
        if (ev.type == SessionEventType.compactionStart)
            writeln("[compaction started]");
        if (ev.type == SessionEventType.compactionComplete)
            writeln("[compaction complete]");
    });

    session.sendAndWait("What did we talk about last time?");
}
