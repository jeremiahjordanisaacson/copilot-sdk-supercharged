/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.InstalledPluginInfo;
import com.microsoft.copilot.generated.rpc.MarketplacePluginInfo;
import com.microsoft.copilot.generated.rpc.Plugin;

class PluginWireCompatibilityTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void buildersOmitTheAssignment() {
        var plugin = Plugin.builder().name("plugin").marketplace("market").version("1.0.0").enabled(true).managed(false)
                .installed(true).build();
        var installed = InstalledPluginInfo.builder().name("plugin").marketplace("market").version("1.0.0")
                .enabled(true).managed(false).installed(true).description("description").build();
        var marketplace = MarketplacePluginInfo.builder().name("plugin").description("description").build();

        assertEquals("plugin", plugin.name());
        assertEquals("plugin", installed.name());
        assertEquals("plugin", marketplace.name());
        assertNull(plugin.assignment());
        assertNull(installed.assignment());
        assertNull(marketplace.assignment());
        assertFalse(MAPPER.valueToTree(plugin).has("assignment"));
        assertFalse(MAPPER.valueToTree(installed).has("assignment"));
        assertFalse(MAPPER.valueToTree(marketplace).has("assignment"));
    }

    @Test
    void assignmentDeserializesThroughTheCanonicalRecords() throws Exception {
        var json = """
                {"name":"plugin","marketplace":"market","enabled":true,
                 "assignment":{"state":"defaultEnabled","reason":"Company setup"}}
                """;
        var plugin = MAPPER.readValue(json, Plugin.class);
        var installed = MAPPER.readValue(json, InstalledPluginInfo.class);
        var marketplace = MAPPER.readValue(json, MarketplacePluginInfo.class);

        assertEquals("defaultEnabled", plugin.assignment().state());
        assertEquals("Company setup", installed.assignment().reason());
        assertEquals("defaultEnabled", marketplace.assignment().state());
    }
}
