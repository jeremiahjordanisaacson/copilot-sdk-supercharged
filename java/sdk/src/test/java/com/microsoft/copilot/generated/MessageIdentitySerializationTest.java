/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.generated;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.UserMessageEvent.UserMessageEventData;
import com.microsoft.copilot.generated.rpc.QueuePendingItems;
import com.microsoft.copilot.generated.rpc.SessionSendParams;

@com.microsoft.copilot.AllowCopilotExperimental
class MessageIdentitySerializationTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void testPublishedSendSourceAccessorsPreserveWireProvenance() throws Exception {
        var request = SessionSendParams.builder().prompt("hello").source("command-review").build();

        assertEquals("command-review", request.source());
        assertEquals(MAPPER.readTree("""
                {"prompt": "hello", "source": "command-review"}
                """), MAPPER.valueToTree(request));
        assertFalse(java.util.Arrays.stream(SessionSendParams.class.getMethods())
                .anyMatch(method -> method.getName().equals("getSendAdmissionId")
                        || method.getName().equals("setSendAdmissionId")
                        || method.getName().equals("sendAdmissionId")));
    }

    @Test
    void testQueuePendingMessageIdUsesCamelCaseAndIsOptional() throws Exception {
        var item = MAPPER.readValue("""
                {
                    "id": "queue-1",
                    "messageId": "message-1",
                    "kind": "message",
                    "displayText": "hello",
                    "agentMode": "interactive"
                }
                """, QueuePendingItems.class);

        assertEquals("message-1", item.messageId());
        assertEquals("message-1", MAPPER.valueToTree(item).get("messageId").textValue());

        var olderItem = MAPPER.readValue("""
                {
                    "id": "queue-2",
                    "kind": "command",
                    "displayText": "/help",
                    "agentMode": "interactive"
                }
                """, QueuePendingItems.class);

        assertNull(olderItem.messageId());
        assertFalse(MAPPER.<JsonNode>valueToTree(olderItem).has("messageId"));
    }

    @Test
    void testUserMessageIdUsesCamelCaseAndIsOptional() throws Exception {
        var message = MAPPER.readValue("""
                {"content": "hello", "messageId": "message-1"}
                """, UserMessageEventData.class);

        assertEquals("message-1", message.messageId());
        assertEquals("message-1", MAPPER.valueToTree(message).get("messageId").textValue());

        var olderMessage = MAPPER.readValue("""
                {"content": "hello"}
                """, UserMessageEventData.class);

        assertNull(olderMessage.messageId());
        assertFalse(MAPPER.<JsonNode>valueToTree(olderMessage).has("messageId"));
    }
}
