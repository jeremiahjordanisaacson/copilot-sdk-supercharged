/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.RuntimeConnection;

import static org.junit.jupiter.api.Assertions.*;

import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;

/**
 * Tests for {@link CopilotClient#updateSessionOptionsForMode}.
 */
class UpdateSessionOptionsForModeTest {

    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();

    /**
     * A connected socket pair where the "server" side auto-replies to every
     * JSON-RPC request with {@code {"success": true}}.
     */
    private static final class AutoReplyPair implements AutoCloseable {

        final Socket clientSocket;
        final Socket serverSocket;
        final JsonRpcClient rpcClient;
        private volatile boolean running = true;
        private final Thread replyThread;
        /** The last JSON-RPC params node received by the stub server. */
        volatile JsonNode lastParams;
        /** The last JSON-RPC method received by the stub server. */
        volatile String lastMethod;

        AutoReplyPair() throws Exception {
            var loopback = InetAddress.getLoopbackAddress();
            try (var ss = new ServerSocket(0, 1, loopback)) {
                clientSocket = new Socket(loopback, ss.getLocalPort());
                serverSocket = ss.accept();
            }
            rpcClient = JsonRpcClient.fromSocket(clientSocket);

            // Background thread that reads requests and sends back success responses
            replyThread = new Thread(() -> {
                try {
                    var in = serverSocket.getInputStream();
                    var out = serverSocket.getOutputStream();
                    while (running) {
                        // Read Content-Length header
                        var header = new StringBuilder();
                        int b;
                        while ((b = in.read()) != -1) {
                            if (b == '\n' && header.toString().endsWith("\r")) {
                                break;
                            }
                            header.append((char) b);
                        }
                        if (b == -1)
                            break;
                        // Skip blank line
                        in.read(); // '\r'
                        in.read(); // '\n'

                        String hdr = header.toString().trim();
                        int colon = hdr.indexOf(':');
                        int len = Integer.parseInt(hdr.substring(colon + 1).trim());
                        byte[] body = in.readNBytes(len);
                        JsonNode msg = MAPPER.readTree(body);

                        lastMethod = msg.get("method").asText();
                        lastParams = msg.get("params");
                        long id = msg.get("id").asLong();

                        // Send back a success response
                        String response = MAPPER.writeValueAsString(MAPPER.createObjectNode().put("jsonrpc", "2.0")
                                .put("id", id).set("result", MAPPER.createObjectNode().put("success", true)));
                        sendRpcMessage(out, response);
                    }
                } catch (Exception e) {
                    if (running) {
                        // Ignore expected exceptions on shutdown
                    }
                }
            });
            replyThread.setDaemon(true);
            replyThread.start();
        }

        private static void sendRpcMessage(OutputStream out, String json) throws Exception {
            byte[] bytes = json.getBytes(StandardCharsets.UTF_8);
            String header = "Content-Length: " + bytes.length + "\r\n\r\n";
            out.write(header.getBytes(StandardCharsets.UTF_8));
            out.write(bytes);
            out.flush();
        }

        @Override
        public void close() throws Exception {
            running = false;
            rpcClient.close();
            clientSocket.close();
            serverSocket.close();
            replyThread.join(3000);
        }
    }

    // ── COPILOT_CLI mode tests ────────────────────────────────────────────────

    @Test
    void codingAgentMode_noFieldsSet_noPatchSent() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(
                    new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));

            client.updateSessionOptionsForMode(session, null, null, null, null).get();

            assertNull(pair.lastMethod, "No RPC call should be made when no fields are set in COPILOT_CLI mode");
            client.close();
        }
    }

    @Test
    void codingAgentMode_skipCustomInstructionsSet_patchContainsOnlyThatField() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(
                    new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));

            client.updateSessionOptionsForMode(session, true, null, null, null).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertEquals("sess-1", pair.lastParams.path("sessionId").asText());
            assertTrue(pair.lastParams.path("instructions").get("skipCustomInstructions").asBoolean());
            assertTrue(pair.lastParams.path("agents").path("localDiscoveryOnly").isMissingNode(),
                    "customAgentsLocalOnly should be absent");
            assertTrue(pair.lastParams.path("git").path("coauthorEnabled").isMissingNode(),
                    "coauthorEnabled should be absent");
            assertTrue(pair.lastParams.path("tools").path("manageScheduleEnabled").isMissingNode(),
                    "manageScheduleEnabled should be absent");
            assertTrue(pair.lastParams.path("plugins").path("installed").isMissingNode(),
                    "installedPlugins should be absent");
            assertTrue(pair.lastParams.path("skills").path("includedBuiltinSkills").isMissingNode(),
                    "includedBuiltinSkills should be absent in COPILOT_CLI mode");
            assertTrue(pair.lastParams.path("tools").path("imageGeneration").isMissingNode(),
                    "Mode patches must preserve the session's image-generation consent");
            client.close();
        }
    }

    @Test
    void codingAgentMode_allFieldsSet_allPropagated() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(
                    new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));

            client.updateSessionOptionsForMode(session, false, true, true, false, List.of("code-review")).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertFalse(pair.lastParams.path("instructions").get("skipCustomInstructions").asBoolean());
            assertTrue(pair.lastParams.path("agents").get("localDiscoveryOnly").asBoolean());
            assertTrue(pair.lastParams.path("git").get("coauthorEnabled").asBoolean());
            assertFalse(pair.lastParams.path("tools").get("manageScheduleEnabled").asBoolean());
            assertEquals("code-review", pair.lastParams.path("skills").get("includedBuiltinSkills").get(0).asText());
            client.close();
        }
    }

    @Test
    void codingAgentMode_onlyCoauthorEnabled_patchSent() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(
                    new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));

            client.updateSessionOptionsForMode(session, null, null, true, null).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertTrue(pair.lastParams.path("git").get("coauthorEnabled").asBoolean());
            client.close();
        }
    }

    // ── EMPTY mode tests ──────────────────────────────────────────────────────

    @Test
    void emptyMode_noFieldsSet_safeDefaultsSent() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                    .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/tmp/copilot-home"))
                    .setAutoStart(false));

            client.updateSessionOptionsForMode(session, null, null, null, null).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertTrue(pair.lastParams.path("instructions").get("skipCustomInstructions").asBoolean(),
                    "default: skip custom instructions");
            assertTrue(pair.lastParams.path("agents").get("localDiscoveryOnly").asBoolean(),
                    "default: local agents only");
            assertFalse(pair.lastParams.path("git").get("coauthorEnabled").asBoolean(), "default: coauthor disabled");
            assertFalse(pair.lastParams.path("tools").get("manageScheduleEnabled").asBoolean(),
                    "default: schedule disabled");
            assertTrue(pair.lastParams.path("plugins").get("installed").isArray(),
                    "installedPlugins should be empty array");
            assertEquals(0, pair.lastParams.path("plugins").get("installed").size());
            assertTrue(pair.lastParams.path("skills").get("includedBuiltinSkills").isArray(),
                    "includedBuiltinSkills should be empty array");
            assertEquals(0, pair.lastParams.path("skills").get("includedBuiltinSkills").size());
            assertTrue(pair.lastParams.path("interaction").path("imageGeneration").isMissingNode(),
                    "Mode defaults must preserve the session's image-generation consent");
            client.close();
        }
    }

    @Test
    void emptyMode_callerOverridesWin() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                    .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/tmp/copilot-home"))
                    .setAutoStart(false));

            client.updateSessionOptionsForMode(session, false, false, true, true).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertFalse(pair.lastParams.path("instructions").get("skipCustomInstructions").asBoolean(),
                    "caller override: don't skip");
            assertFalse(pair.lastParams.path("agents").get("localDiscoveryOnly").asBoolean(),
                    "caller override: not local only");
            assertTrue(pair.lastParams.path("git").get("coauthorEnabled").asBoolean(),
                    "caller override: coauthor enabled");
            assertTrue(pair.lastParams.path("tools").get("manageScheduleEnabled").asBoolean(),
                    "caller override: schedule enabled");
            assertTrue(pair.lastParams.path("plugins").get("installed").isArray(),
                    "installedPlugins always empty in EMPTY mode");
            assertEquals(0, pair.lastParams.path("plugins").get("installed").size());
            assertTrue(pair.lastParams.path("skills").get("includedBuiltinSkills").isArray(),
                    "includedBuiltinSkills always empty in EMPTY mode");
            assertEquals(0, pair.lastParams.path("skills").get("includedBuiltinSkills").size());
            client.close();
        }
    }

    @Test
    void emptyMode_explicitBuiltinSkillAllowlistWins() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                    .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/tmp/copilot-home"))
                    .setAutoStart(false));

            client.updateSessionOptionsForMode(session, null, null, null, null, List.of("code-review")).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertEquals(1, pair.lastParams.path("skills").get("includedBuiltinSkills").size());
            assertEquals("code-review", pair.lastParams.path("skills").get("includedBuiltinSkills").get(0).asText());
            client.close();
        }
    }

    @Test
    void emptyMode_partialOverrides_restGetDefaults() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("sess-1", pair.rpcClient);
            var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                    .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/tmp/copilot-home"))
                    .setAutoStart(false));

            // Only override coauthorEnabled, rest should use safe defaults
            client.updateSessionOptionsForMode(session, null, null, true, null).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertTrue(pair.lastParams.path("instructions").get("skipCustomInstructions").asBoolean(), "default: skip");
            assertTrue(pair.lastParams.path("agents").get("localDiscoveryOnly").asBoolean(), "default: local only");
            assertTrue(pair.lastParams.path("git").get("coauthorEnabled").asBoolean(), "override: coauthor enabled");
            assertFalse(pair.lastParams.path("tools").get("manageScheduleEnabled").asBoolean(),
                    "default: schedule disabled");
            client.close();
        }
    }

    // ── SessionId injection ───────────────────────────────────────────────────

    @Test
    void sessionIdInjectedBySessionOptionsApi() throws Exception {
        try (var pair = new AutoReplyPair()) {
            var session = new CopilotSession("my-session-id", pair.rpcClient);
            var client = new CopilotClient(
                    new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));

            client.updateSessionOptionsForMode(session, true, null, null, null).get();

            assertEquals("session.options.update", pair.lastMethod);
            assertEquals("my-session-id", pair.lastParams.get("sessionId").asText(),
                    "SessionOptionsApi should inject sessionId");
            client.close();
        }
    }
}
