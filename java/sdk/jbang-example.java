///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS com.microsoft:copilot-sdk:1.0.28
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.generated.AssistantMessageEvent;
import com.microsoft.copilot.generated.SessionUsageInfoEvent;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;

import static java.lang.System.out;

class CopilotSDK {
    public static void main(String[] args) throws Exception {
        // Create and start client
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            client.start().get();

            // Create a session
            var session = client.createSession(
                new CreateSessionOptions().setPermissions(new SessionPermissionOptions()
                    .setOnRequest(PermissionHandler.APPROVE_ALL)).setModel("claude-sonnet-4.5")).get();

            // Handle assistant message events
            session.on(AssistantMessageEvent.class, msg -> {
                out.println(msg.getData().content());
            });

            // Handle session usage info events
            session.on(SessionUsageInfoEvent.class, usage -> {
                var data = usage.getData();
                out.println("\n--- Usage Metrics ---");
                out.println("Current tokens: " + data.currentTokens().intValue());
                out.println("Token limit: " + data.tokenLimit().intValue());
                out.println("Messages count: " + data.messagesLength().intValue());
            });

            // Send a message
            var completable = session.sendAndWait(new MessageOptions().setPrompt("What is 2+2?"));
            // and wait for completion
            completable.get();
        }
    }
}
