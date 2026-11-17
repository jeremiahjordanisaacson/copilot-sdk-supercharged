/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
/** Grouped SessionTool configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionToolOptions {

    private List<ToolDefinition> definitions;
    private List<String> availableTools;
    private List<String> excludedTools;
    private Boolean manageScheduleEnabled;
    private LargeToolOutputConfig largeOutput;
    private ToolSearchConfig search;
    private ImageGenerationConfig imageGeneration;

    /**
     * Gets the custom definitions for this session.
     *
     * @return the list of tool definitions
     */
    public List<ToolDefinition> getDefinitions() {
        return definitions == null ? null : Collections.unmodifiableList(definitions);
    }

    /**
     * Sets custom definitions that the assistant can invoke during the session.
     *
     * @param definitions
     *            the list of tool definitions
     * @return this config for method chaining
     * @see ToolDefinition
     */
    public SessionToolOptions setDefinitions(List<ToolDefinition> definitions) {
        this.definitions = definitions;
        return this;
    }

    /**
     * Gets the list of allowed tool names.
     *
     * @return the list of available tool names
     */
    public List<String> getAvailableTools() {
        return availableTools == null ? null : Collections.unmodifiableList(availableTools);
    }

    /**
     * Sets the list of tool names that are allowed in this session.
     * <p>
     * When specified, only tools in this list will be available to the assistant.
     * If a tool appears in both this list and the excluded list, the exclusion
     * wins.
     *
     * @param availableTools
     *            the list of allowed tool names
     * @return this config for method chaining
     */
    public SessionToolOptions setAvailableTools(List<String> availableTools) {
        this.availableTools = availableTools;
        return this;
    }

    /**
     * Gets the list of excluded tool names.
     *
     * @return the list of excluded tool names
     */
    public List<String> getExcludedTools() {
        return excludedTools == null ? null : Collections.unmodifiableList(excludedTools);
    }

    /**
     * Sets the list of tool names to exclude from this session.
     * <p>
     * Tools in this list will not be available to the assistant. Exclusions take
     * precedence over available tools.
     *
     * @param excludedTools
     *            the list of tool names to exclude
     * @return this config for method chaining
     */
    public SessionToolOptions setExcludedTools(List<String> excludedTools) {
        this.excludedTools = excludedTools;
        return this;
    }

    /**
     * Gets whether the manage_schedule tool is enabled.
     *
     * @return the manage schedule flag, or empty if not explicitly set
     * @since 1.3.0
     */
    @JsonIgnore
    public Optional<Boolean> getManageScheduleEnabled() {
        return Optional.ofNullable(manageScheduleEnabled);
    }

    /**
     * Sets whether to enable the {@code manage_schedule} tool.
     * <p>
     * This option is sent to the server via a {@code session.options.update}
     * JSON-RPC call immediately after session resume. In
     * {@link CopilotClientMode#EMPTY EMPTY} mode the default is {@code false}
     * (disabled); in {@link CopilotClientMode#COPILOT_CLI COPILOT_CLI} mode the
     * value is forwarded only when explicitly set.
     *
     * @param manageScheduleEnabled
     *            whether manage schedule is enabled
     * @return this config instance for method chaining
     * @since 1.3.0
     */
    public SessionToolOptions setManageScheduleEnabled(boolean manageScheduleEnabled) {
        this.manageScheduleEnabled = manageScheduleEnabled;
        return this;
    }

    /**
     * Gets the configuration for large tool output handling.
     *
     * @return the large output config, or {@code null} for default
     */
    public LargeToolOutputConfig getLargeOutput() {
        return largeOutput;
    }

    /**
     * Sets the configuration for large tool output handling.
     *
     * @param largeOutput
     *            the large output config
     * @return this config for method chaining
     */
    public SessionToolOptions setLargeOutput(LargeToolOutputConfig largeOutput) {
        this.largeOutput = largeOutput;
        return this;
    }

    /**
     * Gets the tool-search configuration.
     *
     * @return the tool-search config, or {@code null} for the runtime default
     */
    public ToolSearchConfig getSearch() {
        return search;
    }

    /**
     * Gets the non-persisted image generation opt-in.
     *
     * @return the image generation config, or null when unset
     */
    @CopilotExperimental
    public ImageGenerationConfig getImageGeneration() {
        return imageGeneration;
    }

    /**
     * Sets the image generation opt-in. Re-supply after runtime restart.
     *
     * @param imageGeneration
     *            the image generation config
     * @return this config for method chaining
     */
    @CopilotExperimental
    public SessionToolOptions setImageGeneration(ImageGenerationConfig imageGeneration) {
        this.imageGeneration = imageGeneration;
        return this;
    }

    /**
     * Sets the tool-search configuration.
     *
     * @param search
     *            the tool-search config
     * @return this config for method chaining
     */
    public SessionToolOptions setSearch(ToolSearchConfig search) {
        this.search = search;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionToolOptions clone() {
        SessionToolOptions copy = new SessionToolOptions();
        copy.definitions = this.definitions != null ? new ArrayList<>(this.definitions) : null;
        copy.availableTools = this.availableTools != null ? new ArrayList<>(this.availableTools) : null;
        copy.excludedTools = this.excludedTools != null ? new ArrayList<>(this.excludedTools) : null;
        copy.largeOutput = this.largeOutput;
        copy.search = this.search;
        copy.imageGeneration = this.imageGeneration;
        copy.manageScheduleEnabled = this.manageScheduleEnabled;
        return copy;
    }

    /**
     * Clears the manageScheduleEnabled setting.
     *
     * @return this instance for method chaining
     * @since 1.3.0
     */
    public SessionToolOptions clearManageScheduleEnabled() {
        this.manageScheduleEnabled = null;
        return this;
    }
}
