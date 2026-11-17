/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc.fixtures;

import com.microsoft.copilot.rpc.ToolInvocation;
import com.microsoft.copilot.tool.CopilotTool;
import com.microsoft.copilot.tool.CopilotToolParam;

/**
 * Static tool fixture for {@link ToolInvocation} runtime context injection.
 */
public class StaticInvocationTools {

    @CopilotTool("Returns invocation context from a static tool")
    public static String reportStatic(@CopilotToolParam("Current phase") String phase, ToolInvocation invocation) {
        return "phase=" + phase + ",sessionId=" + invocation.getSessionId() + ",toolCallId="
                + invocation.getToolCallId() + ",toolName=" + invocation.getToolName();
    }
}
