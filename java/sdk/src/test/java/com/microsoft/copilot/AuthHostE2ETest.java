/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CopilotClientMode;
import org.junit.jupiter.api.Tag;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;

@Tag("e2e")
class AuthHostE2ETest {

    @ParameterizedTest
    @CsvSource({"'', fallback.ghe.example, https://fallback.ghe.example",
            "tenant.ghe.example, fallback.ghe.example, https://tenant.ghe.example", "'', '', https://github.com"})
    void usesConfiguredGitHubHostForAuthentication(String copilotHost, String ghHost, String expectedHost)
            throws Exception {
        try (var ctx = E2ETestContext.create()) {
            ctx.configureForTest("client", "should_get_authenticated_status");
            Map<String, String> environment = ctx.getEnvironment();
            environment.put("COPILOT_GH_HOST", copilotHost);
            environment.put("GH_HOST", ghHost);

            // This exercises process-environment precedence, not embedded configuration.
            try (var client = ctx.createClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(RuntimeConnection.forSpawnedStdio().setEnvironment(environment))
                    .setGitHub(new com.microsoft.copilot.rpc.ClientGitHubOptions()
                            .setToken(environment.get("GH_TOKEN"))))) {
                client.start().get(30, TimeUnit.SECONDS);
                var status = client.getAuthStatus().get(30, TimeUnit.SECONDS);
                assertTrue(status.isAuthenticated(), status.getStatusMessage());
                assertEquals(expectedHost, status.getHost());
            }
        }
    }
}
