/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.Model;
import com.microsoft.copilot.generated.rpc.ModelCapabilities;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesLimits;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesSupports;
import com.microsoft.copilot.rpc.SessionMetadata;

/**
 * Tests for generated model metadata and session metadata.
 */
class ModelInfoTest {

    @Test
    void modelInfoPreservesOptionalNativeProvider() throws Exception {
        var mapper = new ObjectMapper();
        for (var provider : List.of("{\"id\":\"copilot\",\"kind\":\"copilot\",\"label\":\"GitHub Copilot\"}",
                "{\"id\":\"loki:opaque\",\"kind\":\"loki\",\"label\":\"Microsoft 365 Copilot\"}")) {
            var model = mapper
                    .readValue("{\"id\":\"shared-model\",\"name\":\"Shared Model\",\"capabilities\":{},\"provider\":"
                            + provider + "}", Model.class);
            assertEquals(mapper.readTree(provider), mapper.valueToTree(model.provider()));
            assertEquals(mapper.readTree(provider), mapper.valueToTree(model).get("provider"));
        }
        var unattributed = mapper.readValue("{\"id\":\"configured/model\"}", Model.class);
        assertNull(unattributed.provider());
        assertFalse(mapper.valueToTree(unattributed).has("provider"));
    }

    @Test
    void modelSupportsReasoningEffortGetterSetter() {
        var supports = ModelCapabilitiesSupports.builder().reasoningEffort(true).build();
        assertTrue(supports.reasoningEffort());
    }

    @Test
    void modelSupportsFluentChaining() {
        var supports = ModelCapabilitiesSupports.builder().vision(true).reasoningEffort(true).build();
        assertTrue(supports.vision());
        assertTrue(supports.reasoningEffort());
    }

    @Test
    void modelInfoSupportedReasoningEffortsGetterSetter() {
        var model = modelBuilder().supportedReasoningEfforts(List.of("low", "medium", "high")).build();
        assertEquals(List.of("low", "medium", "high"), model.supportedReasoningEfforts());
    }

    @Test
    void modelInfoDefaultReasoningEffortGetterSetter() {
        var model = modelBuilder().defaultReasoningEffort("medium").build();
        assertEquals("medium", model.defaultReasoningEffort());
    }

    private static Model.Builder modelBuilder() {
        return Model.builder().id("test").name("Test")
                .capabilities(ModelCapabilities.builder().supports(ModelCapabilitiesSupports.builder().build())
                        .limits(ModelCapabilitiesLimits.builder().maxContextWindowTokens(8192L).build()).build());
    }

    @Test
    void sessionMetadataGettersAndSetters() {
        var meta = new SessionMetadata();
        assertNull(meta.getStartTime());
        assertNull(meta.getModifiedTime());
        assertNull(meta.getSummary());
        assertFalse(meta.isRemote());

        meta.setRemote(true);
        assertTrue(meta.isRemote());
    }
}
