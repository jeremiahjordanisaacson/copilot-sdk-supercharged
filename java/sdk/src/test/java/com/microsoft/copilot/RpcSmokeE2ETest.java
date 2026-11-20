/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import org.junit.jupiter.api.Tag;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.PingParams;
import com.microsoft.copilot.generated.rpc.UserSettingsSetParams;

@Tag("e2e")
class RpcSmokeE2ETest {

    private static E2ETestContext ctx;

    @BeforeAll
    static void setup() throws Exception {
        ctx = E2ETestContext.create();
    }

    @AfterAll
    static void teardown() throws Exception {
        if (ctx != null) {
            ctx.close();
        }
    }

    @Test
    void testShouldCallRpcPingWithTypedParamsAndResult() throws Exception {
        ctx.configureForTest("rpc_server", "should_call_rpc_ping_with_typed_params_and_result");

        try (var client = ctx.createClient()) {
            client.start().get(30, TimeUnit.SECONDS);

            var result = client.getRpc().ping(PingParams.builder().message("typed rpc test").build()).get(30,
                    TimeUnit.SECONDS);

            assertEquals("pong: typed rpc test", result.message());
            assertNotNull(result.timestamp());
            assertNotNull(result.protocolVersion());
            assertTrue(result.protocolVersion() >= 0);
        }
    }

    @Test
    void testShouldRejectOversizedSettingsIntegerWithoutAWrite() throws Exception {
        try (var context = E2ETestContext.create(); var client = context.createClient()) {
            var file = context.getHomeDir().resolve("settings.json");
            var original = "{\"statusLine\":{\"command\":\"echo before\",\"padding\":3}}";
            Files.writeString(file, original);
            var oversized = new BigInteger("18446744073709551617");
            var settings = Map.of("statusLine", Map.of("command", "echo after", "padding", oversized));
            var params = UserSettingsSetParams.builder().settings(settings).build();
            client.start().get(30, TimeUnit.SECONDS);

            var error = assertThrows(ExecutionException.class,
                    () -> client.getRpc().user.settings.set(params).get(30, TimeUnit.SECONDS));
            Throwable cause = error;
            while (cause.getCause() != null) {
                cause = cause.getCause();
            }
            assertInstanceOf(IOException.class, cause);
            assertTrue(cause.getMessage().contains("integer"));
            assertEquals(original, Files.readString(file));
            assertSame(settings, params.settings());
            assertEquals(oversized, settings.get("statusLine").get("padding"));

            var maximum = new BigInteger("18446744073709551615");
            client.getRpc().user.settings.set(UserSettingsSetParams.builder()
                    .settings(Map.of("statusLine", Map.of("command", "echo boundary", "padding", maximum))).build())
                    .get(30, TimeUnit.SECONDS);
            var stored = new ObjectMapper().readTree(Files.readString(file));
            assertEquals(maximum, stored.path("statusLine").path("padding").bigIntegerValue());
        }
    }
}
