/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc.fixtures;

import com.microsoft.copilot.tool.CopilotTool;
import com.microsoft.copilot.tool.CopilotToolParam;

/**
 * Fixture testing argument coercion with multiple types including an enum.
 */
public class ArgCoercionTools {

    public enum Color {
        RED, GREEN, BLUE
    }

    @CopilotTool("Method with mixed argument types")
    public String mixedArgs(@CopilotToolParam("Text input") String text, @CopilotToolParam("A count") int count,
            @CopilotToolParam("A flag") boolean flag, @CopilotToolParam("A color") Color color) {
        return text + "-" + count + "-" + flag + "-" + color.name();
    }
}
