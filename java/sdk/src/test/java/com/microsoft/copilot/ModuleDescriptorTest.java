/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.module.ModuleDescriptor;
import org.junit.jupiter.api.Test;

class ModuleDescriptorTest {

    @Test
    void sdkHasExplicitModuleDescriptor() {
        Module module = CopilotClient.class.getModule();
        assertTrue(module.isNamed());
        assertEquals("com.microsoft.copilot.java", module.getName());

        ModuleDescriptor descriptor = module.getDescriptor();
        assertTrue(descriptor.exports().stream().anyMatch(export -> export.source().equals("com.microsoft.copilot")));
        assertTrue(
                descriptor.exports().stream().anyMatch(export -> export.source().equals("com.microsoft.copilot.rpc")));
        assertTrue(descriptor.requires().stream()
                .anyMatch(require -> require.name().equals("com.fasterxml.jackson.databind")));
    }
}
