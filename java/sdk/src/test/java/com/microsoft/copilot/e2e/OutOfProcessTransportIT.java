/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.e2e;

import com.microsoft.copilot.rpc.CopilotClientMode;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import com.microsoft.copilot.AllowCopilotExperimental;
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.PingResponse;
import com.microsoft.copilot.rpc.RuntimeConnection;

/**
 * Failsafe smoke test for the managed out-of-process runtime wrapper.
 */
@AllowCopilotExperimental
@RequireInProcess
class OutOfProcessTransportIT {

    @Test
    void shouldStartPingAndStopOverStdio() throws Exception {
        CopilotClientOptions options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio());
        try (CopilotClient client = new CopilotClient(options)) {
            client.start().get();

            PingResponse pong = client.ping("wrapper message").get();
            assertEquals("pong: wrapper message", pong.message());
            assertNotNull(pong.timestamp());

            client.stop().get();
        }
    }
}
