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
import com.microsoft.copilot.generated.rpc.CatalogClientContract;
import com.microsoft.copilot.generated.rpc.AuthWriteLogout;
import com.microsoft.copilot.generated.rpc.CatalogSearchParams;
import com.microsoft.copilot.generated.rpc.McpPlanInstallParams;
import com.microsoft.copilot.generated.rpc.McpPlanScope;
import com.microsoft.copilot.generated.rpc.RpcCaller;
import com.microsoft.copilot.generated.rpc.ServerRpc;
import com.microsoft.copilot.generated.rpc.FeatureFlagServiceSnapshot;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsReadParams;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsReadResult;
import com.microsoft.copilot.generated.rpc.SessionOpenOptions;
import com.microsoft.copilot.generated.rpc.SessionUpdateAgentOptions;
import com.microsoft.copilot.generated.rpc.SessionAccountsSetParams;
import com.microsoft.copilot.generated.rpc.SessionPermissionsSetRequiredParams;
import com.microsoft.copilot.generated.rpc.SessionRpc;

/**
 * Generated request builders preserve the protocol's wire methods.
 */
class LegacyRequestCompatibilityTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

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
    void accountCommandsDeserializeJsonAndPreserveTypedBuilderValues() throws Exception {
        var command = Map.of("kind", "logout", "accountId", "account");
        for (Object legacy : List.<Object>of(command, MAPPER.valueToTree(command))) {
            var params = MAPPER.treeToValue(MAPPER.valueToTree(Map.of("sessionId", "session", "command", legacy)),
                    SessionAccountsSetParams.class);

            assertEquals("account", assertInstanceOf(AuthWriteLogout.class, params.command()).getAccountId());
            assertEquals(MAPPER.valueToTree(Map.of("sessionId", "session", "command", command)),
                    MAPPER.valueToTree(params));
        }

        var typed = AuthWriteLogout.builder().accountId("account").build();
        assertSame(typed, SessionAccountsSetParams.builder().sessionId("session").command(typed).build().command());
        assertNull(MAPPER.readValue("{\"sessionId\":\"session\",\"command\":null}", SessionAccountsSetParams.class)
                .command());
        assertThrows(com.fasterxml.jackson.databind.JsonMappingException.class,
                () -> MAPPER.readValue("{\"sessionId\":\"session\",\"command\":{\"kind\":\"unknown\"}}",
                        SessionAccountsSetParams.class));
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
