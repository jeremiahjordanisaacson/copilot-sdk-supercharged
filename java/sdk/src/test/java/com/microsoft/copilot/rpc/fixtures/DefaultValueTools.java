/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc.fixtures;

import com.microsoft.copilot.tool.CopilotTool;
import com.microsoft.copilot.tool.CopilotToolParam;

/**
 * Fixture testing default parameter values.
 */
public class DefaultValueTools {

    @CopilotTool("Method with a default value parameter")
    public String withDefault(@CopilotToolParam(value = "A label", required = true) String label,
            @CopilotToolParam(value = "A count", required = false, defaultValue = "42") int count) {
        return label + ":" + count;
    }
}
