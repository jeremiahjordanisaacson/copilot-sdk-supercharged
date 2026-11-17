/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.McpServer;
import com.microsoft.copilot.generated.rpc.McpServerMetadata;
import com.microsoft.copilot.generated.rpc.McpServerSource;
import com.microsoft.copilot.generated.rpc.McpServerStatus;

class McpServerWireCompatibilityTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void builderOmitsTheOwnedMarker() {
        var metadata = McpServerMetadata.builder().instructions("use it").build();
        var server = McpServer.builder().name("server").status(McpServerStatus.CONNECTED).source(McpServerSource.USER)
                .sourcePlugin("plugin").sourcePluginVersion("1.0.0").displayName("Server").error("failed")
                .serverMetadata(metadata).build();

        assertEquals("server", server.name());
        assertEquals(metadata, server.serverMetadata());
        assertNull(server.owned());
        assertFalse(MAPPER.valueToTree(server).has("owned"));
    }

    @Test
    void ownedMarkerDeserialisesThroughTheCanonicalRecord() throws Exception {
        var server = MAPPER.readValue("""
                {"name":"owned","status":"stopped","owned":{"installationId":"installation"}}
                """, McpServer.class);

        assertEquals("installation", server.owned().installationId());
        JsonNode json = MAPPER.valueToTree(server);
        assertEquals("installation", json.at("/owned/installationId").asText());
    }
}
