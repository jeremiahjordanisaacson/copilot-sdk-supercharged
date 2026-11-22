/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.microsoft.copilot.generated.rpc.CatalogClientContract;
import com.microsoft.copilot.generated.rpc.AuthLoginStepAwaiting;
import com.microsoft.copilot.generated.rpc.AutopilotObjectiveState;
import com.microsoft.copilot.generated.rpc.AutopilotObjectiveStatus;
import com.microsoft.copilot.generated.rpc.AutopilotObjectiveCreditLimit;
import com.microsoft.copilot.generated.rpc.CatalogSearchParams;
import com.microsoft.copilot.generated.rpc.McpPlanInstallParams;
import com.microsoft.copilot.generated.rpc.McpPlanScope;
import com.microsoft.copilot.generated.rpc.RpcCaller;
import com.microsoft.copilot.generated.rpc.ServerRpc;
import com.microsoft.copilot.generated.rpc.FeatureFlagServiceSnapshot;
import com.microsoft.copilot.generated.rpc.Extension;
import com.microsoft.copilot.generated.rpc.ExtensionSource;
import com.microsoft.copilot.generated.rpc.ExtensionStatus;
import com.microsoft.copilot.generated.rpc.SessionExtensionsListResult;
import com.microsoft.copilot.generated.rpc.SessionExtensionsReconcileResult;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsReadParams;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsReadResult;
import com.microsoft.copilot.generated.rpc.SessionOpenOptions;
import com.microsoft.copilot.generated.rpc.SessionUpdateAgentOptions;
import com.microsoft.copilot.generated.rpc.SessionAccountsLogoutParams;
import com.microsoft.copilot.generated.rpc.SessionAccountsLoginBeginResult;
import com.microsoft.copilot.generated.rpc.SessionOptionsUpdateParams;
import com.microsoft.copilot.generated.rpc.SessionSettingsPatch;
import com.microsoft.copilot.generated.rpc.SessionPermissionsSetRequiredParams;
import com.microsoft.copilot.generated.rpc.SessionRpc;
import com.microsoft.copilot.generated.rpc.SessionMetadataSnapshotResult;
import com.microsoft.copilot.generated.rpc.MetadataSnapshotRemoteMetadata;
import com.microsoft.copilot.generated.rpc.MetadataSnapshotCurrentMode;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
import com.microsoft.copilot.generated.rpc.IndexedSearchState;

/**
 * Generated request builders preserve the protocol's wire methods.
 */
class LegacyRequestCompatibilityTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void objectiveConstructorPreservesPublishedDescriptorAndCreditArguments() throws Exception {
        var constructor = AutopilotObjectiveState.class.getConstructor(Long.class, String.class,
                AutopilotObjectiveStatus.class, Long.class, String.class, String.class, String.class,
                AutopilotObjectiveCreditLimit.class);
        var limit = AutopilotObjectiveCreditLimit.builder().credits(5.0).creditsUsed(0.125)
                .creditsUsedNanoAiu("125000000").build();
        var objective = constructor.newInstance(7L, "Ship the change", AutopilotObjectiveStatus.PAUSED, 3L, "user",
                "Work saved", "123456789", limit);
        assertEquals(MAPPER.readTree("""
                {"id":7,"objective":"Ship the change","status":"paused","turnCount":3,
                 "pauseReason":"user","completionSummary":"Work saved","creditCountNanoAiu":"123456789",
                 "creditLimit":{"credits":5.0,"creditsUsed":0.125,"creditsUsedNanoAiu":"125000000"}}
                """), MAPPER.readTree(MAPPER.writeValueAsBytes(objective)));

        var current = AutopilotObjectiveState.builder().id(7L).objective("Ship the change")
                .status(AutopilotObjectiveStatus.PAUSED).turnCount(3L).pauseReason("user")
                .completionSummary("Work saved").creditCountNanoAiu("123456789").creditLimit(limit)
                .createdAt("2026-01-02T03:04:05Z").resumeCount(2L).build();
        ObjectNode currentWire = MAPPER.valueToTree(objective);
        currentWire.put("createdAt", "2026-01-02T03:04:05Z").put("resumeCount", 2L);
        assertEquals(currentWire, MAPPER.valueToTree(current));
    }

    @Test
    void metadataConstructorPreservesPublishedDescriptorAndArgumentPositions() throws Exception {
        var constructor = SessionMetadataSnapshotResult.class.getConstructor(String.class,
                java.time.OffsetDateTime.class, java.time.OffsetDateTime.class, Boolean.class, Boolean.class,
                String.class, String.class, String.class, MetadataSnapshotRemoteMetadata.class, String.class,
                String.class, MetadataSnapshotCurrentMode.class, String.class, SessionLimitsConfig.class,
                IndexedSearchState.class, SessionMetadataSnapshotResult.SessionMetadataSnapshotResultWorkspace.class);
        var start = java.time.OffsetDateTime.parse("2026-01-02T03:04:05Z");
        var modified = java.time.OffsetDateTime.parse("2026-01-02T04:05:06Z");
        var snapshot = constructor.newInstance("snapshot-session", start, modified, false, true, "/workspace",
                "snapshot-name", "snapshot-client", null, "snapshot-summary", "/work", MetadataSnapshotCurrentMode.PLAN,
                "snapshot-model", null, null, null);
        var mapper = MAPPER.copy().registerModule(new JavaTimeModule())
                .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        assertEquals(mapper.readTree("""
                {"sessionId":"snapshot-session","startTime":"2026-01-02T03:04:05Z",
                 "modifiedTime":"2026-01-02T04:05:06Z","isRemote":false,"alreadyInUse":true,
                 "workspacePath":"/workspace","initialName":"snapshot-name","clientName":"snapshot-client",
                 "summary":"snapshot-summary","workingDirectory":"/work","currentMode":"plan",
                 "selectedModel":"snapshot-model","sessionLimits":null,"workspace":null}
                """), mapper.valueToTree(snapshot));
    }

    @Test
    void extensionConstructorsPreservePublishedDescriptorsAndWireValues() throws Exception {
        var constructor = Extension.class.getConstructor(String.class, String.class, ExtensionSource.class,
                ExtensionStatus.class, Long.class);
        var extension = constructor.newInstance("user:probe", "probe", ExtensionSource.USER, ExtensionStatus.RUNNING,
                42L);
        var expected = MAPPER.createObjectNode().put("id", "user:probe").put("name", "probe").put("source", "user")
                .put("status", "running").put("pid", 42L);
        assertEquals(expected, MAPPER.valueToTree(extension));
        var expectedList = MAPPER.valueToTree(Map.of("extensions", List.of(extension)));
        for (var type : List.of(SessionExtensionsListResult.class, SessionExtensionsReconcileResult.class)) {
            var result = type.getConstructor(List.class).newInstance(List.of(extension));
            assertEquals(expectedList, MAPPER.valueToTree(result));
        }

        var current = Extension.class.getConstructor(String.class, String.class, ExtensionSource.class,
                ExtensionStatus.class, Long.class, String.class, String.class).newInstance("user:probe", "probe",
                        ExtensionSource.USER, ExtensionStatus.RUNNING, 42L, "/extensions/probe.mjs", "/logs/probe.log");
        var currentWire = expected.deepCopy();
        currentWire.put("modulePath", "/extensions/probe.mjs");
        currentWire.put("logPath", "/logs/probe.log");
        assertEquals(currentWire, MAPPER.valueToTree(current));
        var currentList = MAPPER
                .valueToTree(Map.of("extensions", List.of(current), "configured", true, "explicitOptOut", false));
        for (var type : List.of(SessionExtensionsListResult.class, SessionExtensionsReconcileResult.class)) {
            var result = type.getConstructor(List.class, Boolean.class, Boolean.class).newInstance(List.of(current),
                    true, false);
            assertEquals(currentList, MAPPER.valueToTree(result));
        }
    }

    @Test
    void loginBeginAcceptsThePublishedObjectConstructorAndKeepsTypedSteps() throws Exception {
        var wireStep = Map.of("kind", "awaiting");
        var constructor = SessionAccountsLoginBeginResult.class.getConstructor(String.class, Object.class);
        var result = constructor.newInstance("flow", wireStep);
        assertInstanceOf(AuthLoginStepAwaiting.class, result.step());
        assertEquals(MAPPER.valueToTree(Map.of("flowId", "flow", "step", wireStep)), MAPPER.valueToTree(result));

        var typed = AuthLoginStepAwaiting.builder().build();
        assertSame(typed, constructor.newInstance("flow", typed).step());
        assertNull(constructor.newInstance("flow", null).step());
    }

    @Test
    void featureFlagReadBuildersPreserveDefaultWireMode() throws Exception {
        var params = SessionFeatureFlagsReadParams.builder().sessionId("legacy").waitForExpResponse(true)
                .flags(List.of("CONTROLLED")).overrides(List.of()).afterRevision(4L).build();
        assertEquals(
                MAPPER.valueToTree(Map.of("sessionId", "legacy", "waitForExpResponse", true, "flags",
                        List.of("CONTROLLED"), "overrides", List.of(), "afterRevision", 4L)),
                MAPPER.valueToTree(params));

        var snapshot = MAPPER.readValue("""
                {"serviceId":"local","revision":1,"active":true,"expResponseResolved":true,"flags":{"CONTROLLED":false},
                 "expFlags":{},"response":{},"autoApprovalTargeted":false,
                 "reasoningSummariesArm":{"offByDefault":false,"hintEnabled":false},
                 "gptDefaultModelEnabled":false}
                """, FeatureFlagServiceSnapshot.class);
        var result = SessionFeatureFlagsReadResult.builder().snapshot(snapshot).flags(Map.of("CONTROLLED", false))
                .overrides(List.of(false)).build();
        assertEquals(MAPPER.valueToTree(
                Map.of("snapshot", snapshot, "flags", Map.of("CONTROLLED", false), "overrides", List.of(false))),
                MAPPER.valueToTree(result));
    }

    @Test
    void permissionRequiredBuilderOmitsProviderIdentity() throws Exception {
        var params = SessionPermissionsSetRequiredParams.builder().sessionId("legacy").required(true).build();
        assertEquals("legacy", params.sessionId());
        assertTrue(params.required());
        assertNull(params.providerId());
        assertEquals(MAPPER.valueToTree(Map.of("sessionId", "legacy", "required", true)), MAPPER.valueToTree(params));
    }

    @Test
    void permissionProviderUsesTheSameMethodAndPreservesBothBooleanValues() {
        var caller = new RecordingCaller();
        var session = new SessionRpc(caller, "permission-session");
        for (boolean required : new boolean[]{false, true}) {
            session.permissions.setRequired(SessionPermissionsSetRequiredParams.builder()
                    .sessionId("permission-session").required(required).providerId("provider").build());
            var call = caller.calls.get(caller.calls.size() - 1);
            assertEquals("session.permissions.setRequired", call.method());
            assertEquals(
                    MAPPER.valueToTree(
                            Map.of("sessionId", "permission-session", "required", required, "providerId", "provider")),
                    call.params());
        }
    }

    private static final class RecordingCaller implements RpcCaller {
        record Call(String method, JsonNode params) {
        }

        final List<Call> calls = new ArrayList<>();

        @Override
        public <T> CompletableFuture<T> invoke(String method, Object params, Class<T> resultType) {
            calls.add(new Call(method, MAPPER.valueToTree(params)));
            return CompletableFuture.completedFuture(null);
        }
    }

    private static CatalogClientContract contract() {
        return CatalogClientContract.builder().protocolVersion(3L).requiredCapabilities(List.of("mcp-install-planning"))
                .build();
    }

    private static Map<String, String> candidate() {
        return Map.of("kind", "candidate", "candidateHandle", "candidate", "searchId", "search");
    }

    @Test
    void sessionOptionsBuilderOmitsFeatureFlagConfiguration() {
        var options = SessionOpenOptions.builder().sessionId("session").build();

        assertEquals("session", options.sessionId());
        assertNull(options.experiments());
        assertFalse(MAPPER.valueToTree(options).has("experiments"));
    }

    @Test
    void sessionOptionsCanonicalConstructorPreservesExplicitNullPresence() throws ReflectiveOperationException {
        assertCanonicalPresence(com.microsoft.copilot.generated.rpc.SessionOpenExperimentOptions.class, "assignments",
                "configuration");
    }

    @Test
    void sessionUpdatesCanonicalConstructorPreservesExplicitNullPresence() throws ReflectiveOperationException {
        assertCanonicalPresence(SessionUpdateAgentOptions.class, "includedBuiltinAgents", "directories");
    }

    @Test
    void sessionUpdatesChangeSettingsWithoutChangingDiscovery() {
        for (boolean explicitNull : new boolean[]{true, false}) {
            var agents = SessionUpdateAgentOptions.builder();
            if (explicitNull) {
                agents.includedBuiltinAgents(null);
            }
            var group = agents.build();
            var value = SessionOptionsUpdateParams.builder().sessionId("session")
                    .settings(SessionSettingsPatch.builder().memory(false).build()).agents(group).build();
            var expected = MAPPER.createObjectNode().put("sessionId", "session");
            expected.set("settings", MAPPER.valueToTree(Map.of("memory", false)));
            var expectedAgents = expected.putObject("agents");
            if (explicitNull) {
                expectedAgents.putNull("includedBuiltinAgents");
            }
            assertEquals(expected, MAPPER.valueToTree(value));
            assertEquals(explicitNull ? Set.of("includedBuiltinAgents") : Set.of(), group.$explicitNulls());
        }
    }

    private static void assertCanonicalPresence(Class<?> type, String nullableField, String addedField)
            throws ReflectiveOperationException {
        var components = type.getRecordComponents();
        var canonicalConstructor = type.getConstructor(
                Arrays.stream(components).map(component -> component.getType()).toArray(Class<?>[]::new));
        int presenceIndex = Arrays.stream(components).map(component -> component.getName()).toList()
                .indexOf("$explicitNulls");
        assertTrue(presenceIndex >= 0);
        var arguments = new Object[components.length];
        int sessionIdIndex = Arrays.stream(components).map(component -> component.getName()).toList()
                .indexOf("sessionId");
        if (sessionIdIndex >= 0) {
            arguments[sessionIdIndex] = "session";
        }

        for (Set<String> explicitNulls : Arrays.<Set<String>>asList(new HashSet<>(Set.of(nullableField)), null)) {
            arguments[presenceIndex] = explicitNulls;
            var value = canonicalConstructor.newInstance(arguments);
            assertEquals(explicitNulls == null ? Set.of() : Set.of(nullableField),
                    type.getMethod("$explicitNulls").invoke(value));
            var wire = MAPPER.valueToTree(value);
            assertFalse(wire.has(addedField));
            if (explicitNulls == null) {
                assertFalse(wire.has(nullableField));
            } else {
                assertEquals(Set.of(nullableField), explicitNulls);
                assertTrue(wire.has(nullableField));
                assertTrue(wire.get(nullableField).isNull());
            }
        }
    }

    @Test
    void accountLogoutRequestsPreserveConcreteWireValues() throws Exception {
        var constructor = SessionAccountsLogoutParams.class.getConstructor(String.class, String.class, String.class);
        var wire = Map.of("sessionId", "session", "accountId", "account");
        for (Object value : List.<Object>of(wire, MAPPER.valueToTree(wire))) {
            var params = MAPPER.convertValue(value, SessionAccountsLogoutParams.class);
            assertEquals("account", params.accountId());
            assertEquals(MAPPER.valueToTree(wire), MAPPER.valueToTree(params));
            assertEquals(MAPPER.valueToTree(params),
                    MAPPER.valueToTree(constructor.newInstance("session", "account", null)));
        }
        var selected = SessionAccountsLogoutParams.builder().sessionId("session").selectionId("selected").build();
        assertEquals(MAPPER.valueToTree(Map.of("sessionId", "session", "selectionId", "selected")),
                MAPPER.valueToTree(selected));
        assertNull(MAPPER.readValue("{\"sessionId\":\"session\",\"accountId\":null}", SessionAccountsLogoutParams.class)
                .accountId());
    }

    @Test
    void existingRecordCallsOmitTheAddedFields() {
        var caller = new RecordingCaller();
        var server = new ServerRpc(caller);

        server.mcp.planInstall(McpPlanInstallParams.builder().contract(contract()).source(candidate())
                .scope(McpPlanScope.USER).build());
        server.catalog
                .search(CatalogSearchParams.builder().contract(contract()).query("catalogue query").limit(4L).build());

        assertEquals(2, caller.calls.size());
        var plan = caller.calls.get(0);
        assertEquals("mcp.planInstall", plan.method());
        assertEquals("user", plan.params().path("scope").asText());
        assertFalse(plan.params().has("policySessionId"));
        var search = caller.calls.get(1);
        assertEquals("catalog.search", search.method());
        assertEquals(4, search.params().path("limit").asInt());
        assertFalse(search.params().has("policySessionId"));
    }

    @Test
    void requestBuildersUseTheSameWireMethodWithFlatParams() {
        var caller = new RecordingCaller();
        var server = new ServerRpc(caller);

        server.mcp.planInstall(McpPlanInstallParams.builder().contract(contract()).source(candidate())
                .scope(McpPlanScope.USER).policySessionId("s").build());
        server.catalog.search(CatalogSearchParams.builder().contract(contract()).query("catalogue query")
                .policySessionId("s").build());

        var plan = caller.calls.get(0);
        assertEquals("mcp.planInstall", plan.method());
        assertEquals(MAPPER.valueToTree(Map.of("contract",
                Map.of("protocolVersion", 3L, "requiredCapabilities", List.of("mcp-install-planning")), "source",
                candidate(), "scope", "user", "policySessionId", "s")), plan.params());
        var search = caller.calls.get(1);
        assertEquals("catalog.search", search.method());
        assertEquals("catalogue query", search.params().path("query").asText());
        assertEquals("s", search.params().path("policySessionId").asText());
        assertFalse(search.params().has("limit"));
    }

    @Test
    void requestBuildersRequireTheirMandatoryInputs() {
        assertThrows(IllegalStateException.class, () -> McpPlanInstallParams.builder().source(candidate()).build());
        assertThrows(IllegalStateException.class, () -> McpPlanInstallParams.builder().contract(contract()).build());
        assertThrows(IllegalStateException.class, () -> CatalogSearchParams.builder().contract(contract()).build());
    }
}
