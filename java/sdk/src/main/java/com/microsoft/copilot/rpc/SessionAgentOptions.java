/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
/** Grouped SessionAgent configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionAgentOptions {

    private List<String> excludedBuiltinAgents;
    private List<String> includedBuiltinAgents;
    private Boolean localDiscoveryOnly;
    private List<CustomAgentConfig> definitions;
    private DefaultAgentConfig defaultAgent;
    private String selected;
    private List<String> directories;

    /**
     * Gets the built-in agent names excluded from the resumed session.
     *
     * @return the list of excluded built-in agent names
     */
    @JsonProperty("excludedBuiltinAgents")
    public List<String> getExcludedBuiltInAgents() {
        return excludedBuiltinAgents == null ? null : Collections.unmodifiableList(excludedBuiltinAgents);
    }

    /**
     * Sets the built-in agent names to exclude from the resumed session.
     * <p>
     * Excluded built-in agents are hidden from discovery and cannot be selected or
     * invoked unless a custom agent with the same name is configured.
     *
     * @param excludedBuiltinAgents
     *            the built-in agent names to exclude
     * @return this config instance for method chaining
     */
    public SessionAgentOptions setExcludedBuiltInAgents(List<String> excludedBuiltinAgents) {
        this.excludedBuiltinAgents = excludedBuiltinAgents != null ? new ArrayList<>(excludedBuiltinAgents) : null;
        return this;
    }

    /** Gets the built-in subagent allowlist. @return the allowlist, or null */
    @JsonProperty("includedBuiltinAgents")
    public List<String> getIncludedBuiltInAgents() {
        return includedBuiltinAgents == null ? null : Collections.unmodifiableList(includedBuiltinAgents);
    }

    /**
     * Allows only the named built-in subagents, subject to availability and
     * exclusions. An empty list disables all built-in subagents; custom agents
     * remain available, including same-name agents. Null uses defaults on cold
     * resume and preserves the current allowlist on warm resume. Not persisted;
     * re-supply on resume after restart.
     *
     * @param includedBuiltinAgents
     *            the built-in subagent allowlist, or null to omit
     * @return this config instance for method chaining
     */
    public SessionAgentOptions setIncludedBuiltInAgents(List<String> includedBuiltinAgents) {
        this.includedBuiltinAgents = includedBuiltinAgents != null ? new ArrayList<>(includedBuiltinAgents) : null;
        return this;
    }

    /**
     * Gets whether custom-agent discovery is restricted to local only.
     *
     * @return {@code true} for local only, or empty if not explicitly set
     * @since 1.3.0
     */
    @JsonIgnore
    public Optional<Boolean> getLocalDiscoveryOnly() {
        return Optional.ofNullable(localDiscoveryOnly);
    }

    /**
     * Sets whether custom-agent discovery is restricted to the session's local
     * working directory.
     * <p>
     * This option is sent with the initial resume request and maintained via
     * {@code session.options.update}. In {@link CopilotClientMode#EMPTY EMPTY} mode
     * the default is {@code true} (local only); in
     * {@link CopilotClientMode#COPILOT_CLI COPILOT_CLI} mode the value is forwarded
     * only when explicitly set.
     *
     * @param localDiscoveryOnly
     *            whether to restrict to local agents
     * @return this config instance for method chaining
     * @since 1.3.0
     */
    public SessionAgentOptions setLocalDiscoveryOnly(boolean localDiscoveryOnly) {
        this.localDiscoveryOnly = localDiscoveryOnly;
        return this;
    }

    /**
     * Gets the custom agent configurations.
     *
     * @return the list of custom agent configurations
     */
    public List<CustomAgentConfig> getDefinitions() {
        return definitions == null ? null : Collections.unmodifiableList(definitions);
    }

    /**
     * Sets custom agent configurations.
     *
     * @param definitions
     *            the list of custom agent configurations
     * @return this config for method chaining
     * @see CustomAgentConfig
     */
    public SessionAgentOptions setDefinitions(List<CustomAgentConfig> definitions) {
        this.definitions = definitions;
        return this;
    }

    /**
     * Gets the default agent configuration.
     *
     * @return the default agent configuration, or {@code null} if not set
     */
    public DefaultAgentConfig getDefault() {
        return defaultAgent;
    }

    /**
     * Sets the default agent configuration.
     * <p>
     * Use {@link DefaultAgentConfig#setExcludedTools(List)} to hide specific tools
     * from the default agent while keeping them available to custom sub-agents.
     *
     * @param default
     *            the default agent configuration
     * @return this config for method chaining
     * @see DefaultAgentConfig
     */
    public SessionAgentOptions setDefault(DefaultAgentConfig defaultAgent) {
        this.defaultAgent = defaultAgent;
        return this;
    }

    /**
     * Gets the name of the custom selected to activate at session start.
     *
     * @return the selected name, or {@code null} if not set
     */
    public String getSelected() {
        return selected;
    }

    /**
     * Sets the name of the custom selected to activate when the session starts.
     * <p>
     * Must match the name of one of the agents in {@link #setDefinitions(List)}.
     *
     * @param selected
     *            the selected name to pre-select
     * @return this config for method chaining
     */
    public SessionAgentOptions setSelected(String selected) {
        this.selected = selected;
        return this;
    }

    /**
     * Gets explicit discovery-only agent roots, separate from custom agent objects.
     * No general filesystem grant. Available without configuration discovery,
     * subject to source/managed disables. Relative paths resolve once when supplied
     * against the request's effective working directory; retained roots do not
     * move. Null and omission are equivalent; an empty list clears only the SDK
     * contribution.
     *
     * @return the agent directory paths
     */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets explicit discovery-only agent roots. See {@link #getDirectories()}.
     *
     * @param directories
     *            the agent directory paths
     * @return this config for method chaining
     */
    public SessionAgentOptions setDirectories(List<String> directories) {
        this.directories = directories == null ? null : new ArrayList<>(directories);
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionAgentOptions clone() {
        SessionAgentOptions copy = new SessionAgentOptions();
        copy.excludedBuiltinAgents = this.excludedBuiltinAgents != null
                ? new ArrayList<>(this.excludedBuiltinAgents)
                : null;
        copy.includedBuiltinAgents = this.includedBuiltinAgents != null
                ? new ArrayList<>(this.includedBuiltinAgents)
                : null;
        copy.definitions = this.definitions != null ? new ArrayList<>(this.definitions) : null;
        copy.defaultAgent = this.defaultAgent;
        copy.selected = this.selected;
        copy.localDiscoveryOnly = this.localDiscoveryOnly;
        copy.directories = this.directories != null ? new ArrayList<>(this.directories) : null;
        return copy;
    }

    /**
     * Clears the localDiscoveryOnly setting.
     *
     * @return this instance for method chaining
     * @since 1.3.0
     */
    public SessionAgentOptions clearLocalDiscoveryOnly() {
        this.localDiscoveryOnly = null;
        return this;
    }
}
