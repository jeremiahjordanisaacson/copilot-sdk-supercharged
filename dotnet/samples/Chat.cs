using GitHub.Copilot.SDK;

await using var client = new CopilotClient(new CopilotClientOptions
{
    SessionIdleTimeoutSeconds = 600,
});
await using var session = await client.CreateSessionAsync(new SessionConfig
{
    OnPermissionRequest = PermissionHandler.ApproveAll
});

using var _ = session.On(evt =>
{
    Console.ForegroundColor = ConsoleColor.Blue;
    switch (evt)
    {
        case AssistantReasoningEvent reasoning:
            Console.WriteLine($"[reasoning: {reasoning.Data.Content}]");
            break;
        case ToolExecutionStartEvent tool:
            Console.WriteLine($"[tool: {tool.Data.ToolName}]");
            break;
    }
    Console.ResetColor();
});

Console.WriteLine("Chat with Copilot (Ctrl+C to exit)\n");

while (true)
{
    Console.Write("You: ");
    var input = Console.ReadLine()?.Trim();
    if (string.IsNullOrEmpty(input)) continue;
    Console.WriteLine();

    var reply = await session.SendAndWaitAsync(new MessageOptions { Prompt = input });
    Console.WriteLine($"\nAssistant: {reply?.Data.Content}\n");
}

// --- v2.0 Features ---

// Session Metadata
var meta = await client.GetSessionMetadataAsync(session.SessionId);
if (meta is not null)
{
    Console.WriteLine($"Session ID: {meta.SessionId}, Summary: {meta.Summary}");
}

// Skills (uncomment to use)
// var skillSession = await client.CreateSessionAsync(new SessionConfig
// {
//     OnPermissionRequest = PermissionHandler.ApproveAll,
//     SkillDirectories = ["./skills"],
//     IncludeSubAgentStreamingEvents = true,
// });
