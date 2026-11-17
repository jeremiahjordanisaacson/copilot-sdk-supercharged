/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionGitHubOptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.JsonNode;

import com.microsoft.copilot.rpc.AutoTier;
import com.microsoft.copilot.rpc.CapiSessionOptions;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.WorkingDirectoryTrust;
import com.microsoft.copilot.rpc.CreateSessionFilesystemOptions;

/**
 * Tests for CAPI provider-scoped session options.
 */
class CapiSessionOptionsTest {

    @Test
    void workingDirectoryTrustRetainsTheStringApiAndAnUnambiguousNullSetter() throws Exception {
        var config = new CreateSessionOptions();
        assertEquals(String.class,
                CreateSessionFilesystemOptions.class.getMethod("getTrustWorkingDirectory").getReturnType());
        assertEquals(CreateSessionFilesystemOptions.class, CreateSessionFilesystemOptions.class
                .getMethod("setTrustWorkingDirectory", String.class).getReturnType());
        assertSame(config, config.setFilesystem(config.getFilesystem().setTrustWorkingDirectory("session")));
        assertEquals("session", config.getFilesystem().getTrustWorkingDirectory());
        assertEquals(WorkingDirectoryTrust.SESSION, config.getFilesystem().getWorkingDirectoryTrust());
        assertEquals("session",
                SessionRequestBuilder.buildCreateRequest(config, "test").getFilesystem().getTrustWorkingDirectory());
        assertSame(config, config.setFilesystem(config.getFilesystem().setTrustWorkingDirectory(null)));
        assertNull(config.getFilesystem().getTrustWorkingDirectory());
        assertNull(config.getFilesystem().getWorkingDirectoryTrust());
        assertNull(SessionRequestBuilder.buildCreateRequest(config, "test").getFilesystem().getTrustWorkingDirectory());
        assertSame(config.getFilesystem(), config.getFilesystem().setWorkingDirectoryTrust(WorkingDirectoryTrust.USER));
        assertEquals("user", config.getFilesystem().getTrustWorkingDirectory());
        assertEquals("user",
                SessionRequestBuilder.buildCreateRequest(config, "test").getFilesystem().getTrustWorkingDirectory());
        var copy = config.clone();
        assertEquals(WorkingDirectoryTrust.USER, copy.getFilesystem().getWorkingDirectoryTrust());
        var mapper = JsonRpcClient.getObjectMapper();
        JsonNode serialized = mapper.valueToTree(config);
        assertEquals("user", serialized.get("filesystem").get("trustWorkingDirectory").asText());
        assertTrue(serialized.path("filesystem").path("workingDirectoryTrust").isMissingNode());
        assertEquals("user",
                mapper.treeToValue(serialized, CreateSessionOptions.class).getFilesystem().getTrustWorkingDirectory());
    }

    @Test
    void defaultsAreNull() {
        var capi = new CapiSessionOptions();

        assertNull(capi.getEnableWebSocketResponses());
        assertNull(capi.getAutoTier());
    }

    @Test
    void fluentSetterReturnsSameInstance() {
        var capi = new CapiSessionOptions();

        assertSame(capi, capi.setEnableWebSocketResponses(true));
        assertEquals(Boolean.TRUE, capi.getEnableWebSocketResponses());
        assertSame(capi, capi.setAutoTier(AutoTier.BALANCE));
        assertEquals(AutoTier.BALANCE, capi.getAutoTier());
    }

    @Test
    void serializesEnableWebSocketResponses() {
        var capi = new CapiSessionOptions().setEnableWebSocketResponses(true);

        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(capi);

        assertTrue(json.get("enableWebSocketResponses").asBoolean());
        assertTrue(json.path("autoTier").isMissingNode());
    }

    @Test
    void omitsUnsetEnableWebSocketResponses() {
        var capi = new CapiSessionOptions();

        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(capi);

        assertTrue(json.path("enableWebSocketResponses").isMissingNode());
        assertTrue(json.path("autoTier").isMissingNode());
        assertEquals(0, json.size());
    }

    @ParameterizedTest
    @MethodSource("canonicalAutoTiers")
    void autoTierCanonicalValuesRoundTripAndForward(AutoTier tier, String value) throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var capi = new CapiSessionOptions().setAutoTier(tier);
        JsonNode json = mapper.valueToTree(capi);
        assertEquals(value, json.get("autoTier").asText());
        assertEquals(1, json.size());
        assertEquals(tier, mapper.treeToValue(json, CapiSessionOptions.class).getAutoTier());

        capi.setEnableWebSocketResponses(false);
        var create = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setModel("auto").setGitHub(new SessionGitHubOptions().setCapi(capi)),
                "session-1");
        var resume = SessionRequestBuilder.buildResumeRequest("session-1",
                new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setCapi(capi)));
        for (Object request : new Object[]{create, resume}) {
            JsonNode requestJson = mapper.valueToTree(request);
            assertEquals(value, requestJson.path("github").get("capi").get("autoTier").asText());
            assertFalse(requestJson.path("github").get("capi").get("enableWebSocketResponses").asBoolean());
            assertEquals(2, requestJson.path("github").get("capi").size());
        }
    }

    @Test
    void autoTierPreservesServerIdentifiersAndRejectsMalformedValues() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (String value : new String[]{"balanced", "Balance", "unknown"}) {
            var tier = AutoTier.fromValue(value);
            assertEquals(value, tier.getValue());
            assertEquals(value, mapper.valueToTree(tier).asText());
            assertEquals(tier, mapper.readValue("\"" + value + "\"", AutoTier.class));
        }
        for (String value : new String[]{"", "two words", "\n"}) {
            assertThrows(IllegalArgumentException.class, () -> AutoTier.fromValue(value));
        }
        assertNull(AutoTier.fromValue(null));
    }

    private static Stream<Arguments> canonicalAutoTiers() {
        return Stream.of(Arguments.of(AutoTier.EFFICIENCY, "efficiency"), Arguments.of(AutoTier.BALANCE, "balance"),
                Arguments.of(AutoTier.INTELLIGENCE, "intelligence"), Arguments.of(AutoTier.FAST, "fast"));
    }

    @Test
    void clearingAutoTierOmitsIt() {
        var capi = new CapiSessionOptions().setAutoTier(AutoTier.BALANCE).setAutoTier(null);
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(capi);
        assertEquals(0, json.size());
    }

    @Test
    void createRequestIncludesCapiWhenSet() {
        var config = new CreateSessionOptions().setGitHub(
                new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnableWebSocketResponses(true)));

        var request = SessionRequestBuilder.buildCreateRequest(config, "session-1");
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);

        assertNotNull(request.getGitHub().getCapi());
        assertTrue(json.path("github").get("capi").get("enableWebSocketResponses").asBoolean());
        assertTrue(json.path("github").get("capi").path("autoTier").isMissingNode());
    }

    @Test
    void createRequestOmitsCapiWhenUnset() {
        var config = new CreateSessionOptions();

        var request = SessionRequestBuilder.buildCreateRequest(config, "session-1");
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);

        assertNull(request.getGitHub().getCapi());
        assertTrue(json.path("github").path("capi").isMissingNode());
    }

    @Test
    void resumeRequestIncludesCapiWhenSet() {
        var config = new ResumeSessionOptions().setGitHub(
                new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnableWebSocketResponses(true)));

        var request = SessionRequestBuilder.buildResumeRequest("session-1", config);
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);

        assertNotNull(request.getGitHub().getCapi());
        assertTrue(json.path("github").get("capi").get("enableWebSocketResponses").asBoolean());
        assertTrue(json.path("github").get("capi").path("autoTier").isMissingNode());
    }

    @Test
    void resumeRequestOmitsCapiWhenUnset() {
        var config = new ResumeSessionOptions();

        var request = SessionRequestBuilder.buildResumeRequest("session-1", config);
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);

        assertNull(request.getGitHub().getCapi());
        assertTrue(json.path("github").path("capi").isMissingNode());
    }

    @Test
    void sessionConfigCloneCopiesCapiReference() {
        var capi = new CapiSessionOptions().setEnableWebSocketResponses(true);

        var clone = new CreateSessionOptions().setGitHub(new SessionGitHubOptions().setCapi(capi)).clone();

        assertSame(capi, clone.getGitHub().getCapi());
    }

    @Test
    void resumeSessionConfigCloneCopiesCapiReference() {
        var capi = new CapiSessionOptions().setEnableWebSocketResponses(true);

        var clone = new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setCapi(capi)).clone();

        assertSame(capi, clone.getGitHub().getCapi());
    }

    @Test
    void falseValueIsSerializedWhenExplicitlySet() {
        var capi = new CapiSessionOptions().setEnableWebSocketResponses(false);

        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(capi);

        assertFalse(json.get("enableWebSocketResponses").asBoolean());
    }
}
