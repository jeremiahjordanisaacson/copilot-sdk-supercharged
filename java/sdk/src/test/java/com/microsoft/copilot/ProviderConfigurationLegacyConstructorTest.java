/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.NamedProviderConfig;
import com.microsoft.copilot.generated.rpc.ProviderModelConfig;

class ProviderConfigurationLegacyConstructorTest {

    @Test
    void providerOmitsUnspecifiedRuntimeOptions() {
        var provider = NamedProviderConfig.builder().name("saved").baseUrl("https://example.test").apiKey("fixture-key")
                .bearerToken("fixture-bearer").headers(Map.of("X-Fixture", "kept")).hasBearerTokenProvider(false)
                .build();
        assertEquals("saved", provider.name());
        assertEquals("fixture-key", provider.apiKey());
        assertEquals("fixture-bearer", provider.bearerToken());
        assertEquals(Map.of("X-Fixture", "kept"), provider.headers());
        assertNull(provider.localModel());
        assertFalse(new ObjectMapper().valueToTree(provider).has("localModel"));
    }

    @Test
    void modelOmitsUnspecifiedAutoMetadata() {
        var model = ProviderModelConfig.builder().id("model").provider("saved").wireModel("wire").modelId("base")
                .name("Display").maxPromptTokens(4096L).maxContextWindowTokens(8192L).maxOutputTokens(1024L)
                .metadata(Map.of("kept", true)).build();
        assertEquals("wire", model.wireModel());
        assertEquals(4096L, model.maxPromptTokens());
        assertEquals(Map.of("kept", true), model.metadata());
        assertNull(model.autoLocal());
        assertFalse(new ObjectMapper().valueToTree(model).has("autoLocal"));
    }

    @Test
    void canonicalRecordsPreservePublishedRuntimeOptionsAndAutoMetadata() throws Exception {
        var mapper = new ObjectMapper();
        var providerJson = mapper.readTree("""
                {"name":"managed","type":"winml","baseUrl":"model.gguf",
                 "localModel":{"ctxSize":8192,"args":["--jinja"],"futureOption":true}}
                """);
        var provider = mapper.treeToValue(providerJson, NamedProviderConfig.class);
        assertEquals(providerJson, mapper.valueToTree(provider));
        var modelJson = mapper.readTree("""
                {"id":"model","provider":"managed","autoLocal":{"capiId":"base","maxConcurrent":2,"future":true}}
                """);
        var model = mapper.treeToValue(modelJson, ProviderModelConfig.class);
        assertEquals(modelJson, mapper.valueToTree(model));
    }
}
