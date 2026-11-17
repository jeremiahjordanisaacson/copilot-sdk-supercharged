/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc.fixtures;

import com.microsoft.copilot.tool.CopilotTool;
import com.microsoft.copilot.tool.CopilotToolParam;

/**
 * Simple tool fixture with basic String-returning methods.
 */
public class SimpleTools {

    @CopilotTool(value = "Greets a user by name", metadata = {
            @CopilotTool.MetadataEntry(key = "github.com/copilot:safeForTelemetry", value = @CopilotTool.MetadataValue(flags = {
                    @CopilotTool.MetadataFlag(name = "name", value = true),
                    @CopilotTool.MetadataFlag(name = "inputsNames", value = false)}))})
    public String greetUser(@CopilotToolParam(value = "The user's name", required = true) String name) {
        return "Hello, " + name + "!";
    }

    @CopilotTool("Adds two numbers together")
    public String addNumbers(@CopilotToolParam(value = "First number") int a,
            @CopilotToolParam(value = "Second number") int b) {
        return String.valueOf(a + b);
    }
}
