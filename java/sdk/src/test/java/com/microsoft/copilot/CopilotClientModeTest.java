/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.RuntimeConnection;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;

/**
 * Tests for {@link CopilotClientMode} and Empty mode validation.
 */
public class CopilotClientModeTest {

    @Test
    void testRequiresExplicitMode() {
        var opts = new CopilotClientOptions();
        assertNull(opts.getMode());
        var ex = assertThrows(IllegalArgumentException.class, () -> new CopilotClient(opts));
        assertTrue(ex.getMessage().contains("mode is required"));
        assertThrows(IllegalArgumentException.class, () -> new CopilotClient(null));
    }

    @Test
    void testCodingAgentModeAndClone() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        assertEquals(CopilotClientMode.CODING_AGENT, opts.clone().getMode());
        try (var client = new CopilotClient(opts)) {
            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testClonePreservesEmptyMode() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.EMPTY);
        assertEquals(CopilotClientMode.EMPTY, opts.clone().getMode());
    }

    @Test
    void testSetModeEmpty() {
        var opts = new CopilotClientOptions();
        opts.setMode(CopilotClientMode.EMPTY);
        assertEquals(CopilotClientMode.EMPTY, opts.getMode());
    }

    @Test
    void testEmptyModeRequiresCopilotHome() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.EMPTY).setAutoStart(false);
        // Empty mode without copilotHome should throw
        var ex = assertThrows(IllegalArgumentException.class, () -> new CopilotClient(opts));
        assertTrue(ex.getMessage().contains("Empty mode"));
    }

    @Test
    void testEmptyModeWithCopilotHome() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/tmp/copilot-home")).setAutoStart(false);
        // Should not throw - copilotHome is set
        var client = new CopilotClient(opts);
        assertEquals(ConnectionState.DISCONNECTED, client.getState());
        client.close();
    }

    @Test
    void testEmptyModeAttachedRuntimeDoesNotRequireHome() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.EMPTY)
                .setRuntime(RuntimeConnection.forTcp("localhost", 4321)).setAutoStart(false);
        try (var client = new CopilotClient(options)) {
            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testCopilotClientModeEnumValues() {
        assertEquals(2, CopilotClientMode.values().length);
        assertEquals(CopilotClientMode.EMPTY, CopilotClientMode.valueOf("EMPTY"));
        assertEquals(CopilotClientMode.CODING_AGENT, CopilotClientMode.valueOf("CODING_AGENT"));
    }

    @Test
    void testEnumSerializationNames() {
        // CopilotClientMode is a plain enum; verify the values exist
        assertNotNull(CopilotClientMode.EMPTY);
        assertNotNull(CopilotClientMode.CODING_AGENT);
    }
}
