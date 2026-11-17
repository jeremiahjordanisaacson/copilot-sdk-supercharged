/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc.fixtures;

import com.microsoft.copilot.tool.CopilotTool;
import com.microsoft.copilot.tool.CopilotToolParam;

/**
 * Fixture testing tool override flag.
 */
public class OverrideTools {

    @CopilotTool(value = "Custom grep implementation", name = "grep", overridesBuiltInTool = true)
    public String customGrep(@CopilotToolParam(value = "Search pattern", required = true) String pattern) {
        return "Found: " + pattern;
    }
}
