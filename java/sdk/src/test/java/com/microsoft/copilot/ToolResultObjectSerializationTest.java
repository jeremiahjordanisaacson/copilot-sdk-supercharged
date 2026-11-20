/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.microsoft.copilot.rpc.ToolResultObject;

/**
 * Verifies optional tool result metadata on {@link ToolResultObject}, including
 * null omission, explicit false values, and backward-compatible constructors.
 */
class ToolResultObjectSerializationTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @ParameterizedTest
    @NullSource
    @ValueSource(booleans = {false, true})
    void preservesLargeOutputPolicy(Boolean skip) throws Exception {
        var result = new ToolResultObject("success", "large output", null, null, null, null, null, skip);
        JsonNode node = MAPPER.readTree(MAPPER.writeValueAsString(result));
        if (skip == null) {
            assertFalse(node.has("skipLargeOutputProcessing"));
        } else {
            assertEquals(skip, node.get("skipLargeOutputProcessing").booleanValue());
        }
        assertEquals(skip, MAPPER.treeToValue(node, ToolResultObject.class).skipLargeOutputProcessing());
    }

    @Test
    void legacyConstructorsLeaveLargeOutputPolicyUnset() {
        for (var result : List.of(new ToolResultObject("success", "ok", null, null, null, null),
                new ToolResultObject("success", "ok", null, null, null, null, List.of("view")),
                ToolResultObject.success("ok"))) {
            assertFalse(MAPPER.valueToTree(result).has("skipLargeOutputProcessing"));
        }
    }

    @Test
    void serializesToolReferences() {
        var result = new ToolResultObject("success", "found 2 tools", null, null, null, null,
                List.of("get_weather", "check_status"));

        JsonNode node = MAPPER.valueToTree(result);

        assertEquals("found 2 tools", node.get("textResultForLlm").asText());
        JsonNode refs = node.get("toolReferences");
        assertNotNull(refs);
        assertTrue(refs.isArray());
        assertEquals(2, refs.size());
        assertEquals("get_weather", refs.get(0).asText());
        assertEquals("check_status", refs.get(1).asText());
    }

    @Test
    void omitsToolReferencesWhenNull() {
        JsonNode node = MAPPER.valueToTree(ToolResultObject.success("ok"));

        assertFalse(node.has("toolReferences"));
    }

    @Test
    void sixArgConstructorLeavesToolReferencesNull() {
        var result = new ToolResultObject("success", "ok", null, null, null, null);

        assertNull(result.toolReferences());
        assertFalse(MAPPER.valueToTree(result).has("toolReferences"));
    }

    @Test
    void deserializesToolReferences() throws Exception {
        String json = "{\"resultType\":\"success\",\"textResultForLlm\":\"x\","
                + "\"toolReferences\":[\"alpha\",\"beta\"]}";

        ToolResultObject result = MAPPER.readValue(json, ToolResultObject.class);

        assertEquals(List.of("alpha", "beta"), result.toolReferences());
    }
}
