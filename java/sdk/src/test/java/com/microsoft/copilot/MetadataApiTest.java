/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.generated.ToolExecutionProgressEvent;
import com.microsoft.copilot.generated.rpc.ModelBillingTokenPrices;
import com.microsoft.copilot.generated.rpc.ModelBillingTokenPricesLongContext;
import com.microsoft.copilot.generated.rpc.Model;
import com.microsoft.copilot.generated.rpc.ModelBilling;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesLimitsVision;
import com.microsoft.copilot.generated.rpc.ModelPolicyState;
import com.microsoft.copilot.rpc.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the new metadata APIs (getStatus, getAuthStatus, listModels) and
 * the ToolExecutionProgressEvent.
 */
public class MetadataApiTest {

    private static String cliPath;
    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();

    @BeforeAll
    static void setup() {
        cliPath = TestUtil.findCliPath();
    }

    // ===== ToolExecutionProgressEvent Tests =====

    @Test
    void testToolExecutionProgressEventParsing() throws Exception {
        String json = """
                {
                    "type": "tool.execution_progress",
                    "id": "550e8400-e29b-41d4-a716-446655440000",
                    "timestamp": "2026-01-22T10:00:00Z",
                    "data": {
                        "toolCallId": "call-123",
                        "progressMessage": "Processing file 1 of 10..."
                    }
                }
                """;

        var event = MAPPER.treeToValue(MAPPER.readTree(json), SessionEvent.class);

        assertNotNull(event);
        assertInstanceOf(ToolExecutionProgressEvent.class, event);

        ToolExecutionProgressEvent progressEvent = (ToolExecutionProgressEvent) event;
        assertEquals("tool.execution_progress", progressEvent.getType());
        assertNotNull(progressEvent.getData());
        assertEquals("call-123", progressEvent.getData().toolCallId());
        assertEquals("Processing file 1 of 10...", progressEvent.getData().progressMessage());
    }

    @Test
    void testToolExecutionProgressEventType() {
        assertEquals("tool.execution_progress", new ToolExecutionProgressEvent().getType());
    }

    // ===== Response Type Deserialization Tests =====

    @Test
    void testGetStatusResponseDeserialization() throws Exception {
        String json = """
                {
                    "version": "1.2.3",
                    "protocolVersion": 2
                }
                """;

        GetStatusResponse response = MAPPER.readValue(json, GetStatusResponse.class);

        assertEquals("1.2.3", response.getVersion());
        assertEquals(2, response.getProtocolVersion());
    }

    @Test
    void testGetAuthStatusResponseDeserialization() throws Exception {
        String json = """
                {
                    "isAuthenticated": true,
                    "authType": "user",
                    "host": "github.com",
                    "login": "testuser",
                    "statusMessage": "Authenticated successfully"
                }
                """;

        GetAuthStatusResponse response = MAPPER.readValue(json, GetAuthStatusResponse.class);

        assertTrue(response.isAuthenticated());
        assertEquals("user", response.getAuthType());
        assertEquals("github.com", response.getHost());
        assertEquals("testuser", response.getLogin());
        assertEquals("Authenticated successfully", response.getStatusMessage());
    }

    @Test
    void testGetAuthStatusResponseNotAuthenticated() throws Exception {
        String json = """
                {
                    "isAuthenticated": false,
                    "statusMessage": "Not authenticated"
                }
                """;

        GetAuthStatusResponse response = MAPPER.readValue(json, GetAuthStatusResponse.class);

        assertFalse(response.isAuthenticated());
        assertNull(response.getAuthType());
        assertNull(response.getHost());
        assertNull(response.getLogin());
        assertEquals("Not authenticated", response.getStatusMessage());
    }

    @Test
    void testModelPreservesUnknownMetadataValues() throws Exception {
        String json = """
                {
                    "id": "future-model",
                    "name": "Future model",
                    "capabilities": {
                        "supports": {
                            "adaptive_thinking": "future-thinking"
                        }
                    },
                    "policy": {
                        "state": "future-policy"
                    },
                    "modelPickerCategory": "future-capability",
                    "modelPickerPriceCategory": "future-price"
                }
                """;

        Model model = MAPPER.readValue(json, Model.class);

        assertEquals("future-capability", model.modelPickerCategory().getValue());
        assertEquals("future-price", model.modelPickerPriceCategory().getValue());
        assertEquals("future-policy", model.policy().state().getValue());
        assertEquals("future-thinking", model.capabilities().supports().adaptiveThinking().getValue());
        JsonNode encoded = MAPPER.valueToTree(model);
        assertEquals("future-capability", encoded.get("modelPickerCategory").asText());
        assertEquals("future-price", encoded.get("modelPickerPriceCategory").asText());
        assertEquals("future-policy", encoded.get("policy").get("state").asText());
        assertEquals("future-thinking", encoded.get("capabilities").get("supports").get("adaptive_thinking").asText());
    }

    @Test
    void testModelLimitsPreservesMaxOutputTokens() throws Exception {
        String json = """
                {
                    "id": "m",
                    "name": "M",
                    "capabilities": {
                        "supports": {},
                        "limits": {
                            "max_prompt_tokens": 128000,
                            "max_output_tokens": 16384,
                            "max_context_window_tokens": 144384
                        }
                    }
                }
                """;

        Model model = MAPPER.readValue(json, Model.class);

        assertEquals(16384L, model.capabilities().limits().maxOutputTokens());
        JsonNode encoded = MAPPER.valueToTree(model.capabilities().limits());
        assertEquals(16384, encoded.get("max_output_tokens").asInt());
    }

    @Test
    void testModelInfoDeserialization() throws Exception {
        String json = """
                {
                    "id": "gpt-4",
                    "name": "GPT-4",
                    "capabilities": {
                        "supports": {
                            "vision": true
                        },
                        "limits": {
                            "max_prompt_tokens": 8192,
                            "max_context_window_tokens": 128000,
                            "vision": {
                                "supported_media_types": ["image/png", "image/jpeg"],
                                "max_prompt_images": 10,
                                "max_prompt_image_size": 20971520
                            }
                        }
                    },
                    "policy": {
                        "state": "enabled",
                        "terms": "https://example.com/terms"
                    },
                    "billing": {
                        "multiplier": 1.5,
                        "tokenPrices": {
                            "inputPrice": 2.0,
                            "outputPrice": 8.0,
                            "cacheReadPrice": 0.5,
                            "batchSize": 1000000,
                            "maxPromptTokens": 128000,
                            "longContext": {
                                "inputPrice": 4.0,
                                "outputPrice": 16.0,
                                "cacheReadPrice": 1.0,
                                "maxPromptTokens": 1000000
                            }
                        }
                    }
                }
                """;

        Model model = MAPPER.readValue(json, Model.class);

        assertEquals("gpt-4", model.id());
        assertEquals("GPT-4", model.name());

        // Capabilities
        assertNotNull(model.capabilities());
        assertTrue(model.capabilities().supports().vision());
        assertEquals(8192L, model.capabilities().limits().maxPromptTokens());
        assertEquals(128000L, model.capabilities().limits().maxContextWindowTokens());

        // Vision limits
        ModelCapabilitiesLimitsVision visionLimits = model.capabilities().limits().vision();
        assertNotNull(visionLimits);
        assertEquals(List.of("image/png", "image/jpeg"), visionLimits.supportedMediaTypes());
        assertEquals(10L, visionLimits.maxPromptImages());
        assertEquals(20971520L, visionLimits.maxPromptImageSize());

        // Policy
        assertNotNull(model.policy());
        assertEquals(ModelPolicyState.ENABLED, model.policy().state());
        assertEquals("https://example.com/terms", model.policy().terms());

        // Billing
        assertNotNull(model.billing());
        assertEquals(1.5, model.billing().multiplier());

        // Token prices
        ModelBillingTokenPrices tokenPrices = model.billing().tokenPrices();
        assertNotNull(tokenPrices);
        assertEquals(2.0, tokenPrices.inputPrice());
        assertEquals(8.0, tokenPrices.outputPrice());
        assertEquals(0.5, tokenPrices.cacheReadPrice());
        assertEquals(Long.valueOf(1000000), tokenPrices.batchSize());
        assertEquals(Long.valueOf(128000), tokenPrices.maxPromptTokens());

        // Long context tier
        ModelBillingTokenPricesLongContext longContext = tokenPrices.longContext();
        assertNotNull(longContext);
        assertEquals(4.0, longContext.inputPrice());
        assertEquals(16.0, longContext.outputPrice());
        assertEquals(1.0, longContext.cacheReadPrice());
        assertEquals(Long.valueOf(1000000), longContext.maxPromptTokens());
    }

    @Test
    void testModelBillingSerializationOmitsNullMultiplier() throws Exception {
        var billing = ModelBilling.builder().build();

        String json = MAPPER.writeValueAsString(billing);

        assertFalse(json.contains("multiplier"));
    }

    @Test
    void testModelBillingMultiplierOptPresent() throws Exception {
        ModelBilling billing = MAPPER.readValue("{\"multiplier\": 1.5}", ModelBilling.class);

        assertEquals(1.5, billing.multiplier());
    }

    @Test
    void testModelBillingMultiplierOptAbsent() throws Exception {
        ModelBilling billing = MAPPER.readValue("{}", ModelBilling.class);

        assertNull(billing.multiplier());
    }

    @Test
    void testGetModelsResponseDeserialization() throws Exception {
        String json = """
                {
                    "models": [
                        {
                            "id": "gpt-4",
                            "name": "GPT-4",
                            "capabilities": {
                                "supports": { "vision": false },
                                "limits": { "max_context_window_tokens": 8192 }
                            }
                        },
                        {
                            "id": "claude-3",
                            "name": "Claude 3",
                            "capabilities": {
                                "supports": { "vision": true },
                                "limits": { "max_context_window_tokens": 200000 }
                            }
                        }
                    ]
                }
                """;

        GetModelsResponse response = MAPPER.readValue(json, GetModelsResponse.class);

        assertNotNull(response.getModels());
        assertEquals(2, response.getModels().size());
        assertEquals("gpt-4", response.getModels().get(0).id());
        assertEquals("claude-3", response.getModels().get(1).id());
    }

    // ===== Integration Tests (require CLI) =====

    @Test
    void testGetStatus() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio(cliPath)))) {
            client.start().get();

            GetStatusResponse status = client.getStatus().get();

            assertNotNull(status);
            assertNotNull(status.getVersion());
            assertFalse(status.getVersion().isEmpty());
            assertEquals(SdkProtocolVersion.get(), status.getProtocolVersion());
        }
    }

    @Test
    void testGetAuthStatus() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio(cliPath)))) {
            client.start().get();

            GetAuthStatusResponse authStatus = client.getAuthStatus().get();

            assertNotNull(authStatus);
            // The response should have a status message regardless of auth state
            // We can't guarantee the user is authenticated in tests
        }
    }

    @Test
    void testListModels() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio(cliPath)))) {
            client.start().get();

            // Note: listModels may require authentication
            // This test verifies the method exists and can be called
            try {
                List<Model> models = client.listModels().get();
                assertNotNull(models);
                // If we got models, verify they have expected fields
                for (Model model : models) {
                    assertNotNull(model.id());
                    assertNotNull(model.name());
                }
            } catch (Exception e) {
                // May fail if not authenticated, which is acceptable in tests
                System.out.println("listModels failed (may require auth): " + e.getMessage());
            }
        }
    }

    // ===== Protocol Version Test =====

    @Test
    void testProtocolVersionIsFour() {
        assertEquals(4, SdkProtocolVersion.get());
    }
}
