/*
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License.
 */

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionToolOptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.JsonNode;
import com.microsoft.copilot.generated.rpc.ProtocolImageGenerationOptions;
import com.microsoft.copilot.rpc.ImageGenerationConfig;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;

class ImageGenerationConfigTest {
    @ParameterizedTest
    @NullSource
    @ValueSource(booleans = {true, false})
    void protocolOptionsBuilderPreservesOmittedFields(Boolean enabled) {
        var options = ProtocolImageGenerationOptions.builder().enabled(enabled).build();
        assertProtocolOptionsOnlyContainEnabled(options, enabled);
    }

    private static void assertProtocolOptionsOnlyContainEnabled(ProtocolImageGenerationOptions options,
            Boolean enabled) {
        assertEquals(enabled, options.enabled());
        assertNull(options.watermark());
        assertNull(options.saveToDisk());
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(options);
        assertFalse(json.has("watermark"));
        assertFalse(json.has("saveToDisk"));
        if (enabled == null) {
            assertFalse(json.has("enabled"));
        } else {
            assertEquals(enabled.booleanValue(), json.get("enabled").booleanValue());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void protocolOptionsForwardExplicitWatermark(boolean watermark) {
        var options = ProtocolImageGenerationOptions.builder().enabled(null).watermark(watermark).build();
        assertEquals(watermark, options.watermark());
        assertNull(options.saveToDisk());
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(options);
        assertEquals(watermark, json.get("watermark").booleanValue());
        assertFalse(json.has("enabled"));
        assertFalse(json.has("saveToDisk"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void protocolOptionsForwardExplicitSaveToDisk(boolean saveToDisk) {
        var options = ProtocolImageGenerationOptions.builder().enabled(null).watermark(null).saveToDisk(saveToDisk)
                .build();
        assertEquals(saveToDisk, options.saveToDisk());
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(options);
        assertEquals(saveToDisk, json.get("saveToDisk").booleanValue());
        assertEquals(1, json.size());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void createAndResumeForwardExplicitOptIn(boolean enabled) {
        var imageGeneration = new ImageGenerationConfig().setEnabled(enabled);
        var createConfig = new CreateSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone();
        var resumeConfig = new ResumeSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone();
        assertSame(imageGeneration, createConfig.getTools().getImageGeneration());
        assertSame(imageGeneration, resumeConfig.getTools().getImageGeneration());
        var create = SessionRequestBuilder.buildCreateRequest(createConfig, "image-test");
        var resume = SessionRequestBuilder.buildResumeRequest("image-test", resumeConfig);
        for (Object request : new Object[]{create, resume}) {
            JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);
            assertEquals(enabled, json.path("tools").get("imageGeneration").get("enabled").booleanValue());
            assertEquals(1, json.path("tools").get("imageGeneration").size());
        }
    }

    @Test
    void createAndResumeOmitUnsetOptIn() {
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "image-test");
        var resume = SessionRequestBuilder.buildResumeRequest("image-test", new ResumeSessionOptions());
        for (Object request : new Object[]{create, resume}) {
            JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);
            assertFalse(json.path("tools").has("imageGeneration"));
        }
        JsonNode empty = JsonRpcClient.getObjectMapper().valueToTree(new ImageGenerationConfig());
        assertEquals(0, empty.size());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void createAndResumeForwardWatermarkWithoutEnablingImages(boolean watermark) {
        var imageGeneration = new ImageGenerationConfig().setWatermark(watermark);
        assertEquals(watermark, imageGeneration.getWatermark());
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone(), "image-test");
        var resume = SessionRequestBuilder.buildResumeRequest("image-test", new ResumeSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone());
        for (Object request : new Object[]{create, resume}) {
            JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);
            assertEquals(watermark, json.path("tools").get("imageGeneration").get("watermark").booleanValue());
            assertEquals(1, json.path("tools").get("imageGeneration").size());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void createAndResumeForwardSaveToDiskWithoutEnablingImages(boolean saveToDisk) {
        var imageGeneration = new ImageGenerationConfig().setSaveToDisk(saveToDisk);
        assertEquals(saveToDisk, imageGeneration.getSaveToDisk());
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone(), "image-test");
        var resume = SessionRequestBuilder.buildResumeRequest("image-test", new ResumeSessionOptions()
                .setTools(new SessionToolOptions().setImageGeneration(imageGeneration)).clone());
        for (Object request : new Object[]{create, resume}) {
            JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);
            assertEquals(saveToDisk, json.path("tools").get("imageGeneration").get("saveToDisk").booleanValue());
            assertEquals(1, json.path("tools").get("imageGeneration").size());
        }
    }
}
