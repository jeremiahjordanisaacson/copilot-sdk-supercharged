/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.rpc.CreateSessionRequest;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.ResumeSessionRequest;
import com.microsoft.copilot.rpc.CreateSessionOptions;

/**
 * High-level configs omit legacy remote control; raw requests retain wire
 * compatibility.
 */
class RemoteSessionTest {
    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();

    @Test
    void highLevelConfigsDoNotExposeRemoteControl() {
        for (var type : new Class<?>[]{CreateSessionOptions.class, ResumeSessionOptions.class}) {
            assertThrows(NoSuchMethodException.class, () -> type.getMethod("getRemoteSession"));
            assertThrows(NoSuchMethodException.class, () -> type.getMethod("setRemoteSession", String.class));
        }
    }

    @Test
    void createAndResumeOmitLegacyRemoteMode() throws Exception {
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "local-session");
        var resume = SessionRequestBuilder.buildResumeRequest("local-session", new ResumeSessionOptions());
        assertFalse(MAPPER.valueToTree(create).has("remoteSession"));
        assertFalse(MAPPER.valueToTree(resume).has("remoteSession"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"off", "export", "on"})
    void rawCreateRequestRetainsLegacyRemoteMode(String mode) throws Exception {
        var request = new CreateSessionRequest();
        request.setRemoteSession(mode);
        var tree = MAPPER.valueToTree(request);
        assertEquals(mode, tree.get("remoteSession").asText());
        assertEquals(mode, MAPPER.treeToValue(tree, CreateSessionRequest.class).getRemoteSession());
    }

    @ParameterizedTest
    @ValueSource(strings = {"off", "export", "on"})
    void rawResumeRequestRetainsLegacyRemoteMode(String mode) throws Exception {
        var request = new ResumeSessionRequest();
        request.setRemoteSession(mode);
        var tree = MAPPER.valueToTree(request);
        assertEquals(mode, tree.get("remoteSession").asText());
        assertEquals(mode, MAPPER.treeToValue(tree, ResumeSessionRequest.class).getRemoteSession());
    }
}
