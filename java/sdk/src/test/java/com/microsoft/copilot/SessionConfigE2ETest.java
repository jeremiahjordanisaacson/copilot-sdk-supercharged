/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;
import com.microsoft.copilot.rpc.CapiSessionOptions;
import com.microsoft.copilot.rpc.SessionGitHubOptions;

import com.microsoft.copilot.rpc.SessionAgentOptions;
import com.microsoft.copilot.rpc.SessionExecutionOptions;
import com.microsoft.copilot.rpc.SessionInferenceOptions;
import com.microsoft.copilot.rpc.SessionInstructionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;

import org.junit.jupiter.api.Tag;

import static com.microsoft.copilot.CopilotRequestTestSupport.SYNTHETIC_TEXT;
import static com.microsoft.copilot.CopilotRequestTestSupport.newLlmClient;
import static com.microsoft.copilot.CopilotRequestTestSupport.setupCapiAuth;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
import com.microsoft.copilot.rpc.BlobAttachment;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ProviderConfig;
import com.microsoft.copilot.rpc.ResumeSessionOptions;

/**
 * E2E tests for session configuration features.
 */
@Tag("e2e")
public class SessionConfigE2ETest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static E2ETestContext ctx;

    @BeforeAll
    static void setup() throws Exception {
        ctx = E2ETestContext.create();
    }

    @AfterAll
    static void teardown() throws Exception {
        if (ctx != null) {
            ctx.close();
        }
    }

    @Test
    void testShouldApplyInstructionDirectoriesOnCreate() throws Exception {
        ctx.configureForTest("session_config", "should_apply_instructiondirectories_on_create");

        // Set up instruction directory with a custom instruction file
        Path projectDir = ctx.getWorkDir().resolve("instruction-create-project");
        Path instructionDir = ctx.getWorkDir().resolve("extra-create-instructions");
        Path instructionFilesDir = instructionDir.resolve(".github").resolve("instructions");
        String sentinel = "JAVA_CREATE_INSTRUCTION_DIRECTORIES_SENTINEL";
        Files.createDirectories(projectDir);
        Files.createDirectories(instructionFilesDir);
        Files.writeString(instructionFilesDir.resolve("extra.instructions.md"), "Always include " + sentinel + ".");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setWorkingDirectory(projectDir.toString())
                    .setInstructions(
                            ctx.newSessionConfig().getInstructions().setDirectories(List.of(instructionDir.toString())))
                    .setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            session.sendAndWait(new MessageOptions().setPrompt("What is 1+1?")).get(60, TimeUnit.SECONDS);

            List<Map<String, Object>> exchanges = ctx.getExchanges();
            assertFalse(exchanges.isEmpty(), "Should have at least one exchange");
            String systemMessage = getSystemMessage(exchanges.get(0));
            assertNotNull(systemMessage, "System message should not be null");
            assertTrue(systemMessage.contains(sentinel),
                    "System message should contain the instruction sentinel: " + sentinel);
        }
    }

    @Test
    void testShouldApplyInstructionDirectoriesOnResume() throws Exception {
        ctx.configureForTest("session_config", "should_apply_instructiondirectories_on_resume");

        // Set up instruction directory with a custom instruction file
        Path projectDir = ctx.getWorkDir().resolve("instruction-resume-project");
        Path instructionDir = ctx.getWorkDir().resolve("extra-resume-instructions");
        Path instructionFilesDir = instructionDir.resolve(".github").resolve("instructions");
        String sentinel = "JAVA_RESUME_INSTRUCTION_DIRECTORIES_SENTINEL";
        Files.createDirectories(projectDir);
        Files.createDirectories(instructionFilesDir);
        Files.writeString(instructionFilesDir.resolve("extra.instructions.md"), "Always include " + sentinel + ".");

        try (CopilotClient client = ctx.createClient()) {
            // Create a session first
            CopilotSession session1 = client
                    .createSession(ctx.newSessionConfig().setWorkingDirectory(projectDir.toString()).setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            // Resume with instructionDirectories
            CopilotSession session2 = client
                    .resumeSession(session1.getSessionId(), new ResumeSessionOptions()
                            .setWorkingDirectory(projectDir.toString())
                            .setInstructions(
                                    new SessionInstructionOptions().setDirectories(List.of(instructionDir.toString())))
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            session2.sendAndWait(new MessageOptions().setPrompt("What is 1+1?")).get(60, TimeUnit.SECONDS);

            List<Map<String, Object>> exchanges = ctx.getExchanges();
            assertFalse(exchanges.isEmpty(), "Should have at least one exchange");
            String systemMessage = getSystemMessage(exchanges.get(0));
            assertNotNull(systemMessage, "System message should not be null");
            assertTrue(systemMessage.contains(sentinel),
                    "System message should contain the instruction sentinel: " + sentinel);
        }
    }

    @Test
    void testShouldForwardProviderWireModel() throws Exception {
        ctx.configureForTest("session_config", "should_forward_provider_wire_model");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig().setModel("claude-sonnet-5")
                    .setInference(ByokTestOptions.inference(ctx.newSessionConfig().getInference(),
                            new ProviderConfig().setType("openai").setBaseUrl(ctx.getProxyUrl())
                                    .setApiKey("test-provider-key").setWireModel("test-wire-model")
                                    .setMaxOutputTokens(1024)))
                    .setGitHub(new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false)))
                    .setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            session.sendAndWait(new MessageOptions().setPrompt("What is 1+1?")).get(30, TimeUnit.SECONDS);

            List<Map<String, Object>> exchanges = ctx.getExchanges();
            assertFalse(exchanges.isEmpty(), "Should have at least one exchange");
            @SuppressWarnings("unchecked")
            Map<String, Object> request = (Map<String, Object>) exchanges.get(0).get("request");
            assertEquals("test-wire-model", request.get("model"));
        }
    }

    @Test
    void testShouldUseProviderModelIdAsWireModel() throws Exception {
        ctx.configureForTest("session_config", "should_use_provider_model_id_as_wire_model");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig().setModel("claude-sonnet-5")
                    .setInference(ByokTestOptions.inference(ctx.newSessionConfig().getInference(),
                            new ProviderConfig().setType("openai").setBaseUrl(ctx.getProxyUrl())
                                    .setApiKey("test-provider-key").setModelId("claude-sonnet-5")))
                    .setGitHub(new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false)))
                    .setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            session.sendAndWait(new MessageOptions().setPrompt("What is 1+1?")).get(30, TimeUnit.SECONDS);

            List<Map<String, Object>> exchanges = ctx.getExchanges();
            assertFalse(exchanges.isEmpty(), "Should have at least one exchange");
            @SuppressWarnings("unchecked")
            Map<String, Object> request = (Map<String, Object>) exchanges.get(0).get("request");
            assertEquals("claude-sonnet-5", request.get("model"));
        }
    }

    @Test
    void testShouldApplySessionLimitsOnCreate() throws Exception {
        ctx.configureForTest("session_config", "should_apply_session_limits_on_create");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setExecution(ctx.newSessionConfig().getExecution()
                            .setLimits(SessionLimitsConfig.builder().maxAiCredits(30.0).build()))
                    .setPermissions(ctx.newSessionConfig()
                            .setExecution(ctx.newSessionConfig().getExecution()
                                    .setLimits(SessionLimitsConfig.builder().maxAiCredits(30.0).build()))
                            .getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                sendAndGetNextExchange(session, "Acknowledge the current session limits.");

                assertSessionLimitsApplied(session, 30.0);
            } finally {
                session.close();
            }
        }
    }

    @Test
    void testShouldApplySessionLimitsOnResume() throws Exception {
        ctx.configureForTest("session_config", "should_apply_session_limits_on_resume");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session1 = client.createSession(ctx.newSessionConfig().setPermissions(
                    ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))).get();
            CopilotSession session2 = client.resumeSession(session1.getSessionId(),
                    new ResumeSessionOptions()
                            .setExecution(new SessionExecutionOptions()
                                    .setLimits(SessionLimitsConfig.builder().maxAiCredits(30.0).build()))
                            .setPermissions(new ResumeSessionOptions()
                                    .setExecution(new SessionExecutionOptions()
                                            .setLimits(SessionLimitsConfig.builder().maxAiCredits(30.0).build()))
                                    .getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                sendAndGetNextExchange(session2, "Acknowledge the current session limits.");

                assertSessionLimitsApplied(session2, 30.0);
            } finally {
                session2.close();
                session1.close();
            }
        }
    }

    @Test
    void testShouldApplyExcludedBuiltInAgentsOnCreate() throws Exception {
        ctx.configureForTest("session_config", "should_apply_excluded_built_in_agents_on_create");

        final String excludedAgent = "explore";
        final String prompt = "What is 1+1?";

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession baselineSession = client.createSession(ctx.newSessionConfig().setPermissions(
                    ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))).get();
            try {
                Map<String, Object> baselineExchange = sendAndGetNextExchange(baselineSession, prompt);
                assertTrue(getTaskAgentTypes(baselineExchange).contains(excludedAgent));
            } finally {
                baselineSession.close();
            }

            CopilotSession excludedSession = client.createSession(ctx.newSessionConfig()
                    .setAgents(ctx.newSessionConfig().getAgents().setExcludedBuiltInAgents(List.of(excludedAgent)))
                    .setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                List<String> agentTypes = getTaskAgentTypes(sendAndGetNextExchange(excludedSession, prompt));

                assertFalse(agentTypes.isEmpty(), "Expected task tool agent types");
                assertFalse(agentTypes.contains(excludedAgent), "Expected excluded built-in agent to be omitted");
            } finally {
                excludedSession.close();
            }
        }
    }

    @Test
    void testShouldApplyExcludedBuiltInAgentsOnResume() throws Exception {
        ctx.configureForTest("session_config", "should_apply_excluded_built_in_agents_on_resume");

        final String excludedAgent = "explore";

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session1 = client.createSession(ctx.newSessionConfig().setPermissions(
                    ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))).get();
            CopilotSession session2 = client
                    .resumeSession(session1.getSessionId(), new ResumeSessionOptions()
                            .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(List.of(excludedAgent)))
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                List<String> agentTypes = getTaskAgentTypes(sendAndGetNextExchange(session2, "What is 1+1?"));

                assertFalse(agentTypes.isEmpty(), "Expected task tool agent types");
                assertFalse(agentTypes.contains(excludedAgent), "Expected excluded built-in agent to be omitted");
            } finally {
                session2.close();
                session1.close();
            }
        }
    }

    @Test
    void testShouldEnableCitationsForAnthropicFileAttachmentsOnCreate() throws Exception {
        setupCapiAuth(ctx);
        var handler = new CopilotRequestTestSupport.RecordingRequestHandler(SYNTHETIC_TEXT);

        try (CopilotClient client = newLlmClient(ctx, handler)) {
            CopilotSession session = client.createSession(ctx.newSessionConfig().setModel("claude-sonnet-5")
                    .setInference(ByokTestOptions.inference(
                            ctx.newSessionConfig().getInference().setEnableCitations(true), createAnthropicProvider()))
                    .setGitHub(new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false)))
                    .setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                session.sendAndWait(new MessageOptions().setPrompt("Summarize the attached PDF with citations enabled.")
                        .setAttachments(List.of(createPdfAttachment()))).get(60, TimeUnit.SECONDS);

                assertAnthropicDocumentCitationsEnabled(singleInferenceRequestBody(handler));
            } finally {
                session.close();
            }
        }
    }

    @Test
    void testShouldEnableCitationsForAnthropicFileAttachmentsOnResume() throws Exception {
        setupCapiAuth(ctx);
        var handler = new CopilotRequestTestSupport.RecordingRequestHandler(SYNTHETIC_TEXT);

        try (CopilotClient client = newLlmClient(ctx, handler)) {
            CopilotSession session1 = client.createSession(ctx.newSessionConfig().setPermissions(
                    ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))).get();
            CopilotSession session2 = client
                    .resumeSession(session1.getSessionId(), new ResumeSessionOptions().setModel("claude-sonnet-5")
                            .setInference(ByokTestOptions.inference(
                                    new SessionInferenceOptions().setEnableCitations(true), createAnthropicProvider()))
                            .setGitHub(new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false)))
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            try {
                session2.sendAndWait(
                        new MessageOptions().setPrompt("Summarize the attached PDF with citations enabled.")
                                .setAttachments(List.of(createPdfAttachment())))
                        .get(60, TimeUnit.SECONDS);

                assertAnthropicDocumentCitationsEnabled(singleInferenceRequestBody(handler));
            } finally {
                session2.close();
                session1.close();
            }
        }
    }

    private Map<String, Object> sendAndGetNextExchange(CopilotSession session, String prompt) throws Exception {
        int existingCount = ctx.getExchanges().size();
        session.sendAndWait(new MessageOptions().setPrompt(prompt)).get(60, TimeUnit.SECONDS);

        List<Map<String, Object>> exchanges = ctx.getExchanges();
        assertTrue(exchanges.size() > existingCount, "Expected at least one new exchange");
        return exchanges.get(existingCount);
    }

    private static void assertSessionLimitsApplied(CopilotSession session, double maxAiCredits) throws Exception {
        SessionLimitsConfig limits = session.getRpc().metadata.snapshot().get(30, TimeUnit.SECONDS).sessionLimits();
        assertNotNull(limits, "Expected active session limits");
        assertEquals(maxAiCredits, limits.maxAiCredits());
    }

    private static List<String> getTaskAgentTypes(Map<String, Object> exchange) {
        Object toolsObj = getRequest(exchange).get("tools");
        assertInstanceOf(List.class, toolsObj, "Expected request tools");

        JsonNode parameters = null;
        for (Object toolObj : (List<?>) toolsObj) {
            if (toolObj instanceof Map<?, ?> toolMap && toolMap.get("function") instanceof Map<?, ?> functionMap
                    && "task".equals(functionMap.get("name"))) {
                parameters = MAPPER.valueToTree(functionMap.get("parameters"));
                break;
            }
        }

        assertNotNull(parameters, "Expected task tool parameters");
        JsonNode enumValues = parameters.path("properties").path("agent_type").path("enum");
        assertTrue(enumValues.isArray(), "Expected task agent_type enum");

        List<String> values = new ArrayList<>();
        enumValues.forEach(value -> {
            if (value.isTextual()) {
                values.add(value.asText());
            }
        });
        return values;
    }

    private static Map<?, ?> getRequest(Map<String, Object> exchange) {
        Object request = exchange.get("request");
        assertInstanceOf(Map.class, request, "Expected exchange request");
        return (Map<?, ?>) request;
    }

    private static BlobAttachment createPdfAttachment() {
        String pdfText = "%PDF-1.4\n1 0 obj\n<< /Type /Catalog >>\nendobj\ntrailer\n<< /Root 1 0 R >>\n%%EOF\n";
        return new BlobAttachment()
                .setData(Base64.getEncoder().encodeToString(pdfText.getBytes(StandardCharsets.US_ASCII)))
                .setDisplayName("citation-source.pdf").setMimeType("application/pdf");
    }

    private static ProviderConfig createAnthropicProvider() {
        return new ProviderConfig().setType("anthropic").setBaseUrl("https://anthropic-citations.invalid/v1")
                .setApiKey("test-provider-key").setModelId("claude-sonnet-5").setWireModel("claude-sonnet-5");
    }

    private static String singleInferenceRequestBody(CopilotRequestTestSupport.RecordingRequestHandler handler) {
        List<CopilotRequestTestSupport.InterceptedRequest> requests = handler.inferenceRequests();
        assertEquals(1, requests.size(), "Expected one intercepted inference request");
        return requests.get(0).body();
    }

    private static void assertAnthropicDocumentCitationsEnabled(String requestBody) throws Exception {
        JsonNode root = MAPPER.readTree(requestBody);
        List<JsonNode> documentBlocks = new ArrayList<>();
        for (JsonNode message : root.path("messages")) {
            for (JsonNode block : message.path("content")) {
                if ("document".equals(block.path("type").asText())) {
                    documentBlocks.add(block);
                }
            }
        }

        assertEquals(1, documentBlocks.size(), "Expected one Anthropic document block");
        JsonNode documentBlock = documentBlocks.get(0);
        assertEquals("citation-source.pdf", documentBlock.path("title").asText());
        assertTrue(documentBlock.path("citations").path("enabled").asBoolean(false));
    }

    @SuppressWarnings("unchecked")
    private static String getSystemMessage(Map<String, Object> exchange) {
        // The exchange structure is: { request: { messages: [...] }, response: ...,
        // requestHeaders: ... }
        Object requestObj = exchange.get("request");
        if (!(requestObj instanceof Map<?, ?> request)) {
            return null;
        }
        Object messagesObj = request.get("messages");
        if (messagesObj instanceof List<?> messages) {
            for (Object msg : messages) {
                if (msg instanceof Map<?, ?> msgMap) {
                    if ("system".equals(msgMap.get("role"))) {
                        Object content = msgMap.get("content");
                        return content != null ? content.toString() : null;
                    }
                }
            }
        }
        return null;
    }
}
