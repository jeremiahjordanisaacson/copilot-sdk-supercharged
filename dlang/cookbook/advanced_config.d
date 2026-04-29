/// Cookbook: Advanced Configuration
///
/// Demonstrates provider overrides, custom system messages, idle timeout,
/// environment variables, and session filesystem configuration.
module cookbook.advanced_config;

import copilot;
import std.stdio : writeln, writefln;
import std.json : JSONValue;
import std.typecons : Nullable;

void main()
{
    // Configure the client with advanced options.
    auto opts = CopilotClientOptions.init;
    opts.logLevel = "debug";
    opts.autoStart = false;

    // Custom environment variables passed to the CLI.
    opts.env["COPILOT_LOCALE"] = "en-US";
    opts.env["COPILOT_WORKSPACE"] = "/home/user/project";

    // Session filesystem for persistent state.
    opts.sessionFs = ".copilot-state";

    // Idle timeout: sessions are reaped after 10 minutes of inactivity.
    opts.sessionIdleTimeoutSeconds = 600;

    auto client = new CopilotClient(opts);
    scope(exit) client.stop();

    // Start explicitly since autoStart is off.
    client.start();

    // Check connectivity and version.
    if (client.ping())
    {
        auto status = client.getStatus();
        writeln("CLI status: ", status.status);
        if (!status.version_.isNull)
            writeln("CLI version: ", status.version_.get);
    }

    // List available models.
    auto models = client.listModels();
    writeln("Models: ", models.toString());

    // Create a session with a custom provider.
    auto cfg = SessionConfig.init;
    cfg.streaming = true;

    // Override the model and endpoint.
    cfg.provider = ProviderConfig(
        Nullable!string("gpt-4"),
        Nullable!string.init,  // Use default endpoint.
    );

    // Multi-line system message with role instructions.
    cfg.systemMessage =
        "You are a senior D programmer and code reviewer.\n"
        ~ "Always provide idiomatic D code.\n"
        ~ "Prefer @safe and scope(exit) patterns.";

    // Disable infinite sessions for a one-shot conversation.
    cfg.infiniteSessions = false;

    auto session = client.createSession(cfg);

    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto c = ev.stringField("content");
            if (!c.isNull) writeln("Review: ", c.get);
        }
    });

    session.sendAndWait("Review this code: `auto x = new int[](100); delete x;`");

    // Auth status check.
    auto auth = client.getAuthStatus();
    writeln("Auth status: ", auth.toString());
}
