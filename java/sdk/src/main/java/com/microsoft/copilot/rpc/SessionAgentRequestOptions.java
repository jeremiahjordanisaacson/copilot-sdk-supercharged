/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionAgentRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionAgentRequestOptions {

    @JsonProperty("excludedBuiltinAgents")
    private List<String> excludedBuiltinAgents;
    @JsonProperty("includedBuiltinAgents")
    private List<String> includedBuiltinAgents;

    @JsonProperty("definitions")
    private List<CustomAgentConfig> definitions;

    @JsonProperty("localDiscoveryOnly")
    private Boolean localDiscoveryOnly;

    @JsonProperty("default")
    private DefaultAgentConfig defaultAgent;

    @JsonProperty("selected")
    private String selected;

    @JsonProperty("directories")
    private List<String> directories;

    /** Gets excluded built-in agents. @return the built-in agent names */
    @JsonProperty("excludedBuiltinAgents")
    public List<String> getExcludedBuiltInAgents() {
        return excludedBuiltinAgents == null ? null : Collections.unmodifiableList(excludedBuiltinAgents);
    }

    /**
     * Sets excluded built-in agents. @param excludedBuiltinAgents the agent names
     */
    public SessionAgentRequestOptions setExcludedBuiltInAgents(List<String> excludedBuiltinAgents) {
        this.excludedBuiltinAgents = excludedBuiltinAgents;
        return this;
    }

    /** Gets the built-in subagent allowlist. @return the allowlist, or null */
    @JsonProperty("includedBuiltinAgents")
    public List<String> getIncludedBuiltInAgents() {
        return includedBuiltinAgents == null ? null : Collections.unmodifiableList(includedBuiltinAgents);
    }

    /**
     * Sets the built-in subagent allowlist. @param includedBuiltinAgents the
     * allowlist
     */
    public SessionAgentRequestOptions setIncludedBuiltInAgents(List<String> includedBuiltinAgents) {
        this.includedBuiltinAgents = includedBuiltinAgents;
        return this;
    }

    /** Gets custom agents. @return the agents */
    public List<CustomAgentConfig> getDefinitions() {
        return definitions == null ? null : Collections.unmodifiableList(definitions);
    }

    /** Sets custom agents. @param definitions the agents */
    public SessionAgentRequestOptions setDefinitions(List<CustomAgentConfig> definitions) {
        this.definitions = definitions;
        return this;
    }

    /** Gets whether custom agents are local only. @return the flag */
    public Boolean getLocalDiscoveryOnly() {
        return localDiscoveryOnly;
    }

    /**
     * Sets whether custom agents are local only. @param localDiscoveryOnly the flag
     */
    public SessionAgentRequestOptions setLocalDiscoveryOnly(Boolean localDiscoveryOnly) {
        this.localDiscoveryOnly = localDiscoveryOnly;
        return this;
    }

    /** Gets the default agent config. @return the default agent config */
    public DefaultAgentConfig getDefault() {
        return defaultAgent;
    }

    /**
     * Sets the default agent config. @param default the default agent config
     */
    public SessionAgentRequestOptions setDefault(DefaultAgentConfig defaultAgent) {
        this.defaultAgent = defaultAgent;
        return this;
    }

    /** Gets the pre-selected selected name. @return the selected name */
    public String getSelected() {
        return selected;
    }

    /** Sets the pre-selected selected name. @param selected the selected name */
    public SessionAgentRequestOptions setSelected(String selected) {
        this.selected = selected;
        return this;
    }

    /** Gets explicit agent discovery roots. @return the directories */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets explicit agent discovery roots. @param directories the directories
     */
    public SessionAgentRequestOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }
}
