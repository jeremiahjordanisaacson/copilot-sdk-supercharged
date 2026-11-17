/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;
import com.microsoft.copilot.rpc.CapiSessionOptions;

import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.ChildProcessRuntimeConnection;
import com.microsoft.copilot.rpc.ClientGitHubOptions;
import com.microsoft.copilot.rpc.ClientObservabilityOptions;
import com.microsoft.copilot.rpc.CreateSessionFilesystemOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SessionAgentOptions;
import com.microsoft.copilot.rpc.SessionExecutionOptions;
import com.microsoft.copilot.rpc.SessionGitHubOptions;
import com.microsoft.copilot.rpc.SessionInferenceOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionToolOptions;
import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
import com.microsoft.copilot.generated.AgentModelPolicy;
import com.microsoft.copilot.rpc.CustomAgentConfig;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ProviderConfig;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedStdioRuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedTcpRuntimeConnection;
import com.microsoft.copilot.rpc.WorkingDirectoryTrust;

class ClientOptionsE2ETest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void spawnedConnectionsForwardProcessOptionsWithoutMutatingConfiguration(boolean tcp) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var environment = new HashMap<>(Map.of("JAVA_SDK_TEST_ENV", "configured", "NODE_DEBUG", "removed",
                    "COPILOT_HOME", "overridden-by-client", "copilot_sdk_auth_token", "environment-token"));
            var expectedEnvironment = new HashMap<>(environment);
            var connection = fake.connection(tcp, environment);
            environment.put("JAVA_SDK_TEST_ENV", "mutated-source");
            if (connection instanceof SpawnedStdioRuntimeConnection stdio) {
                stdio.getEnvironment().put("JAVA_SDK_TEST_ENV", "mutated-getter");
            } else if (connection instanceof SpawnedTcpRuntimeConnection spawned) {
                spawned.getEnvironment().put("JAVA_SDK_TEST_ENV", "mutated-getter");
            }
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(connection.setBaseDirectory(fake.path("home").toString()))
                    .setGitHub(new ClientGitHubOptions().setToken("owned-token"))
                    .setObservability(new ClientObservabilityOptions().setLogLevel("debug"));
            var original = MAPPER.valueToTree(options);
            try (var client = new CopilotClient(options)) {
                client.start().get(30, TimeUnit.SECONDS);
            }
            assertEquals(original, MAPPER.valueToTree(options), "Client construction must not rewrite options");
            var configuredEnvironment = connection instanceof SpawnedStdioRuntimeConnection stdio
                    ? stdio.getEnvironment()
                    : ((SpawnedTcpRuntimeConnection) connection).getEnvironment();
            var configuredArguments = connection instanceof SpawnedStdioRuntimeConnection stdio
                    ? stdio.getArgs()
                    : ((SpawnedTcpRuntimeConnection) connection).getArgs();
            assertEquals(expectedEnvironment, configuredEnvironment, "Startup must not rewrite the connection");
            assertEquals(List.of(fake.script().toString(), "--capture-file", fake.capture().toString()),
                    configuredArguments, "Managed flags must not be added to caller-owned arguments");

            var startup = fake.startup();
            assertEquals(fake.workDir().toRealPath(), Path.of(startup.path("cwd").asText()).toRealPath());
            assertEquals("configured", startup.path("environment").path("JAVA_SDK_TEST_ENV").asText());
            assertFalse(startup.path("hasPath").asBoolean(), "A supplied environment must replace the parent");
            assertFalse(startup.path("environment").has("NODE_DEBUG"));
            assertEquals(connection.getBaseDirectory(), startup.path("environment").path("COPILOT_HOME").asText());
            assertEquals("owned-token", startup.path("environment").path("COPILOT_SDK_AUTH_TOKEN").asText());
            var expectedArgs = new ArrayList<>(List.of("--capture-file", fake.capture().toString(), "--server",
                    "--no-auto-update", "--log-level", "debug"));
            if (!tcp) {
                expectedArgs.add("--stdio");
            }
            expectedArgs.addAll(List.of("--auth-token-env", "COPILOT_SDK_AUTH_TOKEN", "--no-auto-login"));
            assertEquals(MAPPER.valueToTree(expectedArgs), startup.path("arguments"));
            if (tcp) {
                var spawned = (SpawnedTcpRuntimeConnection) connection;
                assertNull(spawned.getConnectionToken(), "Generating a token must not mutate the connection");
                assertFalse(startup.path("environment").path("COPILOT_CONNECTION_TOKEN").asText().isEmpty());
            } else {
                assertFalse(startup.path("environment").has("COPILOT_CONNECTION_TOKEN"));
            }
        }
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"info", "debug"})
    void spawnedConnectionsOnlyForwardConfiguredLogLevel(String logLevel) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(fake.connection(false, Map.of()));
            if (logLevel != null) {
                options.getObservability().setLogLevel(logLevel);
            }
            try (var client = new CopilotClient(options)) {
                client.start().get(30, TimeUnit.SECONDS);
            }
            var arguments = new ArrayList<String>();
            fake.startup().path("arguments").forEach(argument -> arguments.add(argument.asText()));
            int index = arguments.indexOf("--log-level");
            if (logLevel == null) {
                assertEquals(-1, index, "Unset log levels must use the runtime's default");
            } else {
                assertTrue(index >= 0, "Explicit log levels must be forwarded");
                assertEquals(logLevel, arguments.get(index + 1));
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void spawnedConnectionsHonorExplicitlyEmptyEnvironment(boolean tcp) throws Exception {
        try (var fake = FakeStdioCli.create();
                var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                        .setRuntime(fake.connection(tcp, Map.of()))
                        .setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false)))) {
            client.start().get(30, TimeUnit.SECONDS);
            var startup = fake.startup();
            assertFalse(startup.path("hasPath").asBoolean(), "An empty map must not inherit PATH");
            assertFalse(startup.path("environment").has("JAVA_SDK_TEST_ENV"));
            assertEquals("1", startup.path("environment").path("COPILOT_RUNTIME_PROCESS_FILE_LOGGING").asText());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void runtimeUsesExecutablesFromTheChildPath(boolean javascript) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var node = TestUtil.findExecutableInPath("node");
            assertNotNull(node, "The fixture requires Node.js");
            var childNode = fake.path("node-bin").resolve(Path.of(node).getFileName());
            var copy = new ProcessBuilder(node, "-e",
                    "require('node:fs').copyFileSync(process.execPath, process.argv[1])", childNode.toString()).start();
            try {
                assertTrue(copy.waitFor(30, TimeUnit.SECONDS), "Interpreter copy did not finish");
                assertEquals(0, copy.exitValue(), "Interpreter copy failed");
            } finally {
                copy.destroyForcibly();
            }
            var arguments = new ArrayList<>(List.of("--capture-file", fake.capture().toString()));
            if (!javascript) {
                arguments.add(0, fake.script().toString());
            }
            var connection = RuntimeConnection.forSpawnedStdio(javascript ? fake.script().toString() : "node")
                    .setArgs(arguments).setEnvironment(Map.of("PATH", childNode.getParent().toString()))
                    .setWorkingDirectory(fake.workDir().toString());
            try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(connection).setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false)))) {
                client.start().get(30, TimeUnit.SECONDS);
            }
            assertEquals(childNode.toRealPath(), Path.of(fake.startup().path("execPath").asText()).toRealPath());
        }
    }

    @Test
    void emptyModeRequiredSettingsOverrideEnvironmentAliases() throws Exception {
        try (var fake = FakeStdioCli.create();
                var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                        .setRuntime(fake.connection(false, Map.of("copilot_disable_keytar", "0"))
                                .setBaseDirectory(fake.path("home").toString()))
                        .setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false)))) {
            client.start().get(30, TimeUnit.SECONDS);
            assertEquals("1", fake.startup().path("environment").path("COPILOT_DISABLE_KEYTAR").asText());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void spawnedConnectionsCanClearProcessOverrides(boolean tcp) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var connection = fake.connection(tcp, Map.of("JAVA_SDK_TEST_ENV", "discarded"));
            if (connection instanceof SpawnedStdioRuntimeConnection stdio) {
                stdio.setEnvironment(null).setWorkingDirectory(null).setPath(fake.script().toString())
                        .setArgs(List.of("--discarded")).setArgs(null);
            } else if (connection instanceof SpawnedTcpRuntimeConnection spawned) {
                spawned.setEnvironment(null).setWorkingDirectory(null).setPath(fake.script().toString())
                        .setArgs(List.of("--discarded")).setArgs(null);
            }
            try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(connection).setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false)))) {
                client.start().get(30, TimeUnit.SECONDS);
            }
            var startup = fake.startup();
            assertTrue(startup.path("hasPath").asBoolean(), "Null must restore the inherited environment");
            assertNotEquals("discarded", startup.path("environment").path("JAVA_SDK_TEST_ENV").asText());
            assertEquals(Path.of("").toRealPath().toString(), startup.path("cwd").asText());
            var expectedArgs = new ArrayList<>(List.of("--server", "--no-auto-update"));
            if (!tcp) {
                expectedArgs.add("--stdio");
            }
            expectedArgs.add("--no-auto-login");
            assertEquals(MAPPER.valueToTree(expectedArgs), startup.path("arguments"));
        }
    }

    @ParameterizedTest
    @NullSource
    @EnumSource(WorkingDirectoryTrust.class)
    void testShouldForwardTrustAndCustomAgentModelPolicy(WorkingDirectoryTrust trust) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var policy = trust == null
                    ? null
                    : trust == WorkingDirectoryTrust.SESSION ? AgentModelPolicy.PREFERRED : AgentModelPolicy.REQUIRED;
            var config = new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setFilesystem(new CreateSessionFilesystemOptions().setWorkingDirectoryTrust(trust))
                    .setAgents(new SessionAgentOptions().setDefinitions(java.util.List
                            .of(new CustomAgentConfig().setName("policy-agent").setPrompt("Use the authored model.")
                                    .setModel("claude-haiku-4.5").setModelPolicy(policy))));
            try (var client = fake.createClient()) {
                client.createSession(config).get(30, TimeUnit.SECONDS).close();
            }
            var create = fake.capturedRequest("session.create").path("params");
            var agent = create.path("agents").path("definitions").get(0);
            if (trust == null) {
                assertFalse(create.path("filesystem").has("trustWorkingDirectory"), create.toString());
                assertFalse(agent.has("modelPolicy"), agent.toString());
            } else {
                assertEquals(trust.getValue(), create.path("filesystem").path("trustWorkingDirectory").asText());
                assertEquals(policy.getValue(), agent.path("modelPolicy").asText());
            }
        }
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(booleans = {true, false})
    void testShouldForwardRefreshCustomInstructionsOnlyOnSessionCreation(Boolean refresh) throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var config = new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            if (refresh != null) {
                config.setInstructions(config.getInstructions().setRefreshCustomInstructions(refresh));
            }

            try (var client = fake.createClient()) {
                var session = client.createSession(config).get(30, TimeUnit.SECONDS);
                session.close();
                var resumed = client
                        .resumeSession(session.getSessionId(),
                                new ResumeSessionOptions().setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(30, TimeUnit.SECONDS);
                resumed.close();
            }

            var create = fake.capturedRequest("session.create").path("params");
            if (refresh == null) {
                assertFalse(create.path("instructions").has("refreshCustomInstructions"), create.toString());
            } else {
                assertEquals(MAPPER.valueToTree(refresh), create.path("instructions").get("refreshCustomInstructions"));
            }
            var resume = fake.capturedRequest("session.resume").path("params");
            assertFalse(resume.path("instructions").has("refreshCustomInstructions"), resume.toString());
        }
    }

    @Test
    void testRefreshCustomInstructionsIsNotExposedOnResumeConfig() {
        assertThrows(NoSuchMethodException.class,
                () -> ResumeSessionOptions.class.getMethod("getRefreshCustomInstructions"));
        assertThrows(NoSuchMethodException.class,
                () -> ResumeSessionOptions.class.getMethod("setRefreshCustomInstructions", boolean.class));
        assertThrows(NoSuchMethodException.class,
                () -> ResumeSessionOptions.class.getMethod("clearRefreshCustomInstructions"));
    }

    @Test
    void testShouldForwardAdvancedSessionCreationOptionsToTheCli() throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var workDir = fake.path("create-work");
            var configDir = fake.path("create-config");

            try (var client = fake.createClient()) {
                var session = client
                        .createSession(new CreateSessionOptions().setSessionId("java-create-session")
                                .setName("java named at create").setClientName("java-e2e-client").setModel("gpt-5-mini")
                                .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                        .setReasoningSummary("none").setContextTier("long_context")
                                        .setEnableCitations(true))
                                .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                        .setExcludedTools(java.util.List.of("grep")))
                                .setAgents(new SessionAgentOptions()
                                        .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                .setExecution(new SessionExecutionOptions()
                                        .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                .setWorkingDirectory(workDir.toString())
                                .setEvents(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getEvents().setStreaming(true).setIncludeSubAgentStreamingEvents(true))
                                .setConfigDirectory(configDir.toString()).setEnableConfigDiscovery(false)
                                .setState(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getState().setSkipEmbeddingRetrieval(true).setEnableCrossSessionStore(true)
                                        .setEmbeddingCacheStorage("in-memory"))
                                .setInstructions(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getInstructions().setOrganizationText("Use Java parity instructions.")
                                        .setOnDemandDiscovery(false).setSkipCustomInstructions(true))
                                .setHooks(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getHooks().setEnableRepositoryHooks(true))
                                .setGit(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getGit().setEnableHostGitOperations(false).setCoauthorEnabled(true))
                                .setSkills(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getSkills().setEnabled(false))
                                .setGitHub(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getGitHub().setToken("java-session-token"))
                                .setAgents(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getAgents().setLocalDiscoveryOnly(false))
                                .setTools(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getTools().setManageScheduleEnabled(true))
                                .setPermissions(new CreateSessionOptions().setSessionId("java-create-session")
                                        .setName("java named at create").setClientName("java-e2e-client")
                                        .setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("low")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setTools(new SessionToolOptions().setAvailableTools(java.util.List.of("bash"))
                                                .setExcludedTools(java.util.List.of("grep")))
                                        .setAgents(new SessionAgentOptions()
                                                .setExcludedBuiltInAgents(java.util.List.of("explore")))
                                        .setGitHub(new SessionGitHubOptions().setTelemetryEnabled(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(42.0).build()))
                                        .getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(30, TimeUnit.SECONDS);
                session.close();
            }

            var create = fake.capturedRequest("session.create").path("params");
            assertEquals("java-create-session", create.path("sessionId").asText());
            assertEquals("java named at create", create.path("name").asText());
            assertEquals("java-e2e-client", create.path("clientName").asText());
            assertEquals("gpt-5-mini", create.path("model").asText());
            assertEquals("low", create.path("inference").path("reasoningEffort").asText());
            assertEquals("none", create.path("inference").path("reasoningSummary").asText());
            assertEquals("long_context", create.path("inference").path("contextTier").asText());
            assertEquals("bash", create.path("tools").path("availableTools").get(0).asText());
            assertEquals("grep", create.path("tools").path("excludedTools").get(0).asText());
            assertEquals("explore", create.path("agents").path("excludedBuiltinAgents").get(0).asText());
            assertEquals(MAPPER.valueToTree(true), create.path("github").get("telemetryEnabled"));
            assertEquals(MAPPER.valueToTree(true), create.path("inference").get("enableCitations"));
            assertEquals(42.0, create.path("execution").path("limits").path("maxAiCredits").asDouble());
            assertEquals(workDir.toString(), create.path("workingDirectory").asText());
            assertEquals(MAPPER.valueToTree(true), create.path("events").get("streaming"));
            assertEquals(MAPPER.valueToTree(true), create.path("events").get("includeSubAgentStreamingEvents"));
            assertEquals(configDir.toString(), create.path("configDirectory").asText());
            assertEquals(MAPPER.valueToTree(false), create.get("enableConfigDiscovery"));
            assertEquals(MAPPER.valueToTree(true), create.path("state").get("skipEmbeddingRetrieval"));
            assertEquals("Use Java parity instructions.",
                    create.path("instructions").path("organizationText").asText());
            assertEquals(MAPPER.valueToTree(false), create.path("instructions").get("onDemandDiscovery"));
            assertEquals(MAPPER.valueToTree(true), create.path("hooks").get("enableRepositoryHooks"));
            assertEquals(MAPPER.valueToTree(false), create.path("git").get("enableHostGitOperations"));
            assertEquals(MAPPER.valueToTree(true), create.path("state").get("enableCrossSessionStore"));
            assertEquals(MAPPER.valueToTree(false), create.path("skills").get("enabled"));
            assertEquals("in-memory", create.path("state").path("embeddingCacheStorage").asText());
            assertEquals("java-session-token", create.path("github").path("token").asText());
            assertFalse(create.has("remoteSession"));
            assertEquals("direct", create.path("mcp").path("envValueMode").asText());
            assertEquals(MAPPER.valueToTree(true), create.path("permissions").get("request"));

            var update = fake.capturedRequest("session.options.update").path("params");
            assertEquals("java-create-session", update.path("sessionId").asText());
            assertEquals(MAPPER.valueToTree(true), update.path("instructions").get("skipCustomInstructions"));
            assertEquals(MAPPER.valueToTree(false), update.path("agents").get("localDiscoveryOnly"));
            assertEquals(MAPPER.valueToTree(true), update.path("git").get("coauthorEnabled"));
            assertEquals(MAPPER.valueToTree(true), update.path("tools").get("manageScheduleEnabled"));
        }
    }

    @Test
    void testShouldForwardNamedProviderConfigurationOnSessionCreation() throws Exception {
        try (var fake = FakeStdioCli.create()) {
            try (var client = fake.createClient()) {
                var session = client
                        .createSession(new CreateSessionOptions()
                                .setInference(ByokTestOptions.inference(new SessionInferenceOptions(),
                                        new ProviderConfig().setType("openai").setWireApi("responses")
                                                .setTransport("websockets").setBaseUrl("https://models.example.test/v1")
                                                .setApiKey("provider-key").setModelId("base-model")
                                                .setWireModel("wire-model").setMaxPromptTokens(1000)
                                                .setMaxOutputTokens(2000).setHeaders(Map.of("x-provider", "java"))))
                                .setGitHub(
                                        new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false)))
                                .setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(30, TimeUnit.SECONDS);
                session.close();
            }

            var inference = fake.capturedRequest("session.create").path("params").path("inference");
            var provider = inference.path("providers").get(0);
            var model = inference.path("models").get(0);
            assertEquals("openai", provider.path("type").asText());
            assertEquals("responses", provider.path("wireApi").asText());
            assertEquals("websockets", provider.path("transport").asText());
            assertEquals("https://models.example.test/v1", provider.path("baseUrl").asText());
            assertEquals("provider-key", provider.path("apiKey").asText());
            assertEquals("base-model", model.path("modelId").asText());
            assertEquals("wire-model", model.path("wireModel").asText());
            assertEquals(1000, model.path("maxPromptTokens").asInt());
            assertEquals(2000, model.path("maxOutputTokens").asInt());
            assertEquals("java", provider.path("headers").path("x-provider").asText());
        }
    }

    @Test
    void testDefaultProviderPresenceOnCreateAndResume() throws Exception {
        for (boolean resume : new boolean[]{false, true}) {
            for (int presence = 0; presence < 3; presence++) {
                try (var fake = FakeStdioCli.create(); var client = fake.createClient()) {
                    var inference = new SessionInferenceOptions();
                    if (presence != 0)
                        inference.setDefaultProvider(presence == 1 ? null : "named");
                    var github = new SessionGitHubOptions().setToken("identity")
                            .setCapi(new CapiSessionOptions().setEnabled(false));
                    var permissions = new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL);
                    var session = resume
                            ? client.resumeSession("routing",
                                    new ResumeSessionOptions().setInference(inference).setGitHub(github)
                                            .setPermissions(permissions).clone())
                                    .get(30, TimeUnit.SECONDS)
                            : client.createSession(new CreateSessionOptions().setInference(inference).setGitHub(github)
                                    .setPermissions(permissions).clone()).get(30, TimeUnit.SECONDS);
                    session.close();
                    var params = fake.capturedRequest(resume ? "session.resume" : "session.create").path("params");
                    var value = params.path("inference").path("defaultProvider");
                    assertEquals(presence == 0, value.isMissingNode());
                    if (presence == 1)
                        assertTrue(value.isNull());
                    if (presence == 2)
                        assertEquals("named", value.asText());
                    assertEquals(false, params.path("github").path("capi").get("enabled").booleanValue());
                    assertEquals("identity", params.path("github").get("token").asText());
                }
            }
        }
    }

    @Test
    void testShouldForwardAdvancedSessionResumeOptionsToTheCli() throws Exception {
        try (var fake = FakeStdioCli.create()) {
            var workDir = fake.path("resume-work");
            var configDir = fake.path("resume-config");

            try (var client = fake.createClient()) {
                client.createSession(new CreateSessionOptions().setSessionId("java-resume-session")
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(30, TimeUnit.SECONDS);
                var session = client
                        .resumeSession("java-resume-session",
                                new ResumeSessionOptions().setClientName("java-resume-client").setModel("gpt-5-mini")
                                        .setInference(new SessionInferenceOptions().setReasoningEffort("medium")
                                                .setReasoningSummary("none").setContextTier("long_context")
                                                .setEnableCitations(true))
                                        .setExecution(new SessionExecutionOptions()
                                                .setLimits(SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                        .setWorkingDirectory(workDir.toString())
                                        .setConfigDirectory(configDir.toString()).setEnableConfigDiscovery(false)
                                        .setState(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getState().setSkipEmbeddingRetrieval(true)
                                                .setEnableCrossSessionStore(true).setEmbeddingCacheStorage("in-memory"))
                                        .setInstructions(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getInstructions().setOrganizationText("Use resumed Java instructions.")
                                                .setOnDemandDiscovery(false).setSkipCustomInstructions(false))
                                        .setHooks(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions().setReasoningEffort("medium")
                                                        .setReasoningSummary("none").setContextTier("long_context")
                                                        .setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getHooks().setEnableRepositoryHooks(true))
                                        .setGit(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getGit().setEnableHostGitOperations(false).setCoauthorEnabled(false))
                                        .setSkills(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getSkills().setEnabled(false))
                                        .setGitHub(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getGitHub().setToken("java-resume-token"))
                                        .setAgents(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getAgents().setLocalDiscoveryOnly(true))
                                        .setTools(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions().setReasoningEffort("medium")
                                                        .setReasoningSummary("none").setContextTier("long_context")
                                                        .setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getTools().setManageScheduleEnabled(true))
                                        .setPermissions(new ResumeSessionOptions().setClientName("java-resume-client")
                                                .setModel("gpt-5-mini")
                                                .setInference(new SessionInferenceOptions()
                                                        .setReasoningEffort("medium").setReasoningSummary("none")
                                                        .setContextTier("long_context").setEnableCitations(true))
                                                .setExecution(new SessionExecutionOptions().setLimits(
                                                        SessionLimitsConfig.builder().maxAiCredits(84.0).build()))
                                                .getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(30, TimeUnit.SECONDS);
                session.close();
            }

            var resume = fake.capturedRequest("session.resume").path("params");
            assertEquals("java-resume-session", resume.path("sessionId").asText());
            assertEquals("java-resume-client", resume.path("clientName").asText());
            assertEquals("gpt-5-mini", resume.path("model").asText());
            assertEquals("medium", resume.path("inference").path("reasoningEffort").asText());
            assertEquals("none", resume.path("inference").path("reasoningSummary").asText());
            assertEquals("long_context", resume.path("inference").path("contextTier").asText());
            assertEquals(MAPPER.valueToTree(true), resume.path("inference").get("enableCitations"));
            assertEquals(84.0, resume.path("execution").path("limits").path("maxAiCredits").asDouble());
            assertEquals(workDir.toString(), resume.path("workingDirectory").asText());
            assertEquals(configDir.toString(), resume.path("configDirectory").asText());
            assertEquals(MAPPER.valueToTree(false), resume.get("enableConfigDiscovery"));
            assertEquals(MAPPER.valueToTree(true), resume.path("state").get("skipEmbeddingRetrieval"));
            assertEquals("Use resumed Java instructions.",
                    resume.path("instructions").path("organizationText").asText());
            assertEquals(MAPPER.valueToTree(false), resume.path("instructions").get("onDemandDiscovery"));
            assertEquals(MAPPER.valueToTree(true), resume.path("hooks").get("enableRepositoryHooks"));
            assertEquals(MAPPER.valueToTree(false), resume.path("git").get("enableHostGitOperations"));
            assertEquals(MAPPER.valueToTree(true), resume.path("state").get("enableCrossSessionStore"));
            assertEquals(MAPPER.valueToTree(false), resume.path("skills").get("enabled"));
            assertEquals("in-memory", resume.path("state").path("embeddingCacheStorage").asText());
            assertEquals("java-resume-token", resume.path("github").path("token").asText());
            assertFalse(resume.has("remoteSession"));
            assertEquals("direct", resume.path("mcp").path("envValueMode").asText());
            assertEquals(MAPPER.valueToTree(true), resume.path("permissions").get("request"));

            var update = fake.capturedRequest("session.options.update").path("params");
            assertEquals("java-resume-session", update.path("sessionId").asText());
            assertEquals(MAPPER.valueToTree(false), update.path("instructions").get("skipCustomInstructions"));
            assertEquals(MAPPER.valueToTree(true), update.path("agents").get("localDiscoveryOnly"));
            assertEquals(MAPPER.valueToTree(false), update.path("git").get("coauthorEnabled"));
            assertEquals(MAPPER.valueToTree(true), update.path("tools").get("manageScheduleEnabled"));
        }
    }

    private record FakeStdioCli(Path dir, Path script, Path capture, Path workDir) implements AutoCloseable {

        static FakeStdioCli create() throws IOException {
            var dir = Files.createTempDirectory("java-fake-copilot-cli-");
            var script = dir.resolve("fake-copilot-cli.js");
            var capture = dir.resolve("capture.json");
            var workDir = dir.resolve("work");
            Files.createDirectories(workDir);
            Files.writeString(capture, "{\"requests\":[]}");
            Files.writeString(script, FAKE_STDIO_CLI_SCRIPT);
            return new FakeStdioCli(dir, script, capture, workDir);
        }

        CopilotClient createClient() {
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(connection(false, null))
                    .setGitHub(new com.microsoft.copilot.rpc.ClientGitHubOptions().setUseLoggedInUser(false));
            return new CopilotClient(options);
        }

        ChildProcessRuntimeConnection<?> connection(boolean tcp, Map<String, String> environment) {
            var node = TestUtil.findExecutableInPath("node");
            assertNotNull(node, "Node.js was not found in PATH");
            var arguments = new ArrayList<>(List.of(script.toString(), "--capture-file", capture.toString()));
            ChildProcessRuntimeConnection<?> connection = tcp
                    ? RuntimeConnection.forSpawnedTcp().setPath(node).setArgs(arguments).setEnvironment(environment)
                            .setWorkingDirectory(workDir.toString())
                    : RuntimeConnection.forSpawnedStdio(node).setArgs(arguments).setEnvironment(environment)
                            .setWorkingDirectory(workDir.toString());
            arguments.add("--mutated-source");
            return connection;
        }

        JsonNode startup() throws IOException {
            return MAPPER.readTree(Files.readString(capture)).path("startup");
        }

        Path path(String name) throws IOException {
            var path = workDir.resolve(name);
            Files.createDirectories(path);
            return path;
        }

        JsonNode capturedRequest(String method) throws IOException {
            for (JsonNode request : MAPPER.readTree(Files.readString(capture)).path("requests")) {
                if (method.equals(request.path("method").asText())) {
                    return request;
                }
            }
            fail("Expected captured request for " + method + " in " + Files.readString(capture));
            return null;
        }

        @Override
        public void close() throws IOException {
            if (Files.exists(dir)) {
                try (var paths = Files.walk(dir)) {
                    paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException ignored) {
                        }
                    });
                }
            }
        }
    }

    private static final String FAKE_STDIO_CLI_SCRIPT = """
            const fs = require('fs');

            const captureFileIndex = process.argv.indexOf('--capture-file');
            const captureFile = captureFileIndex < 0
              ? require('node:path').join(__dirname, 'capture.json')
              : process.argv[captureFileIndex + 1];
            const environmentKeys = ['JAVA_SDK_TEST_ENV', 'NODE_DEBUG', 'COPILOT_HOME',
              'COPILOT_SDK_AUTH_TOKEN', 'COPILOT_CONNECTION_TOKEN', 'COPILOT_RUNTIME_PROCESS_FILE_LOGGING',
              'COPILOT_DISABLE_KEYTAR'];
            const capture = { requests: [], startup: {
              execPath: process.execPath,
              cwd: process.cwd(), arguments: process.argv.slice(2),
              hasPath: Object.keys(process.env).some(key => key.toUpperCase() === 'PATH'),
              environment: Object.fromEntries(environmentKeys
                .filter(key => process.env[key] !== undefined).map(key => [key, process.env[key]]))
            }};
            fs.writeFileSync(captureFile, JSON.stringify(capture));

            let buffer = Buffer.alloc(0);
            let output = process.stdout;

            function persist() {
              fs.writeFileSync(captureFile, JSON.stringify(capture));
            }

            function send(message) {
              const body = Buffer.from(JSON.stringify(message), 'utf8');
              output.write(`Content-Length: ${body.length}\\r\\n\\r\\n`);
              output.write(body);
            }

            function resultFor(message) {
              switch (message.method) {
                case 'connect':
                  return { ok: true, protocolVersion: 4, version: 'fake' };
                case 'llmInference.setProvider':
                  return {};
                case 'session.create':
                  return { sessionId: message.params?.sessionId ?? 'fake-session', openCanvases: [] };
                case 'session.resume':
                  return { sessionId: message.params?.sessionId ?? 'fake-session', openCanvases: [] };
                case 'session.detach':
                  return { success: true };
                case 'session.options.update':
                  return { success: true };
                default:
                  return {};
              }
            }

            function handle(message) {
              capture.requests.push({ method: message.method, params: message.params ?? null });
              persist();
              send({ jsonrpc: '2.0', id: message.id, result: resultFor(message) });
            }

            function attach(input, stream) {
              output = stream;
              input.on('data', chunk => {
                buffer = Buffer.concat([buffer, chunk]);
                while (true) {
                  const headerEnd = buffer.indexOf('\\r\\n\\r\\n');
                  if (headerEnd < 0) {
                    return;
                  }
                  const header = buffer.subarray(0, headerEnd).toString('utf8');
                  const match = /Content-Length:\\s*(\\d+)/i.exec(header);
                  if (!match) {
                    throw new Error(`Missing Content-Length in ${header}`);
                  }
                  const length = Number(match[1]);
                  const bodyStart = headerEnd + 4;
                  if (buffer.length < bodyStart + length) {
                    return;
                  }
                  const body = buffer.subarray(bodyStart, bodyStart + length).toString('utf8');
                  buffer = buffer.subarray(bodyStart + length);
                  handle(JSON.parse(body));
                }
              });
            }

            if (process.argv.includes('--stdio')) {
              attach(process.stdin, process.stdout);
            } else {
              const server = require('node:net').createServer(socket => {
                attach(socket, socket);
                socket.on('close', () => server.close());
              });
              server.listen(0, () => {
                process.stdout.write(`listening on port ${server.address().port}\\n`);
              });
            }
            """;
}
