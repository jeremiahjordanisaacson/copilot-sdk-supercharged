/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.generated.UserMessageDelivery;
import com.microsoft.copilot.generated.UserMessageEvent.UserMessageEventData;
import com.microsoft.copilot.generated.rpc.RpcCaller;
import com.microsoft.copilot.generated.rpc.SessionRpc;
import com.microsoft.copilot.generated.rpc.SessionSendMessagesParams;
import com.microsoft.copilot.generated.rpc.SessionSendParams;

class ClientTurnIdCompatibilityTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void userMessageBuilderKeepsWireFieldsWithoutClientTurnId() {
        var data = UserMessageEventData.builder().content("hi").messageId("message-1").source("user")
                .delivery(UserMessageDelivery.STEERING).turnId("turn-1").build();

        assertEquals("message-1", data.messageId());
        assertEquals(UserMessageDelivery.STEERING, data.delivery());
        assertEquals("turn-1", data.turnId());
        assertNull(data.clientTurnId());
        assertFalse(MAPPER.valueToTree(data).has("clientTurnId"));
    }

    @Test
    void userMessageConstructorWithClientTurnIdKeepsArgumentMeaning() {
        var data = new UserMessageEventData(null, "hi", null, "message-1", "host-turn-1", null, null, null, null, null,
                "user", UserMessageDelivery.STEERING, null, null, null, "turn-1", null);

        assertEquals("message-1", data.messageId());
        assertEquals("host-turn-1", data.clientTurnId());
        assertEquals(UserMessageDelivery.STEERING, data.delivery());
        assertEquals("turn-1", data.turnId());
        assertNull(data.hasSubmittedTask());
        assertFalse(MAPPER.valueToTree(data).has("hasSubmittedTask"));
    }

    @Test
    void userMessageClientTurnIdRoundTripsThroughTheCanonicalRecord() throws Exception {
        JsonNode wire = MAPPER.readTree("{\"content\":\"hi\",\"clientTurnId\":\"host-turn-1\"}");
        var data = MAPPER.treeToValue(wire, UserMessageEventData.class);

        assertEquals("host-turn-1", data.clientTurnId());
        assertEquals(wire, MAPPER.valueToTree(data));
    }

    @Test
    void sendBuildersKeepTheirShapeAndRequestsCarryClientTurnId() {
        var sent = new ArrayList<ObjectNode>();
        var session = new SessionRpc(capturing(sent), "sess-1");

        session.send(SessionSendParams.builder().sessionId("sess-1").prompt("hi").billable(true).build());
        session.send(SessionSendParams.builder().prompt("hi").clientTurnId("host-turn-1").build());

        assertEquals(MAPPER.createObjectNode().put("sessionId", "sess-1").put("prompt", "hi").put("billable", true),
                sent.get(0));
        assertEquals(MAPPER.createObjectNode().put("prompt", "hi").put("clientTurnId", "host-turn-1").put("sessionId",
                "sess-1"), sent.get(1));
    }

    @Test
    void sendMessagesBuildersKeepTheirShapeAndRequestsCarryClientTurnId() {
        var sent = new ArrayList<ObjectNode>();
        var session = new SessionRpc(capturing(sent), "sess-1");

        session.sendMessages(
                SessionSendMessagesParams.builder().sessionId("sess-1").messages(List.of()).prepend(true).build());
        session.sendMessages(
                SessionSendMessagesParams.builder().messages(List.of()).clientTurnId("host-turn-1").build());

        var legacy = MAPPER.createObjectNode().put("sessionId", "sess-1");
        legacy.putArray("messages");
        legacy.put("prepend", true);
        var extended = MAPPER.createObjectNode();
        extended.putArray("messages");
        extended.put("clientTurnId", "host-turn-1").put("sessionId", "sess-1");
        assertEquals(legacy, sent.get(0));
        assertEquals(extended, sent.get(1));
    }

    private static RpcCaller capturing(List<ObjectNode> sent) {
        return new RpcCaller() {
            @Override
            public <T> CompletableFuture<T> invoke(String method, Object params, Class<T> resultType) {
                sent.add((ObjectNode) params);
                return CompletableFuture.completedFuture(null);
            }
        };
    }
}
