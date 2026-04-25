/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

/**
 * Known system prompt section identifiers for the "customize" mode.
 */
public final class SystemPromptSection {
    public static final String IDENTITY = "identity";
    public static final String TONE = "tone";
    public static final String TOOL_EFFICIENCY = "tool_efficiency";
    public static final String ENVIRONMENT_CONTEXT = "environment_context";
    public static final String CODE_CHANGE_RULES = "code_change_rules";
    public static final String GUIDELINES = "guidelines";
    public static final String SAFETY = "safety";
    public static final String TOOL_INSTRUCTIONS = "tool_instructions";
    public static final String CUSTOM_INSTRUCTIONS = "custom_instructions";
    public static final String LAST_INSTRUCTIONS = "last_instructions";

    private SystemPromptSection() {}
}
