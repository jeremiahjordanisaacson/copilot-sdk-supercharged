/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionInstructionRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionInstructionRequestOptions {

    @JsonProperty("directories")
    private List<String> directories;

    @JsonProperty("organizationText")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String organizationText;

    @JsonProperty("onDemandDiscovery")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean onDemandDiscovery;

    @JsonProperty("refreshCustomInstructions")
    private Boolean refreshCustomInstructions;

    /** Gets instruction directories. @return the instruction directories */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets instruction directories. @param directories the directories
     */
    public SessionInstructionRequestOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }

    /** Gets organization custom instructions. @return the instructions */
    public String getOrganizationText() {
        return organizationText;
    }

    /**
     * Sets organization custom instructions. @param organizationText the
     * instructions
     */
    public SessionInstructionRequestOptions setOrganizationText(String organizationText) {
        this.organizationText = organizationText;
        return this;
    }

    /** Gets enable on-demand instruction discovery flag. @return the flag */
    @JsonIgnore
    public Boolean getOnDemandDiscovery() {
        return onDemandDiscovery;
    }

    /**
     * Sets enable on-demand instruction discovery flag. @param onDemandDiscovery
     * the flag
     */
    public SessionInstructionRequestOptions setOnDemandDiscovery(boolean onDemandDiscovery) {
        this.onDemandDiscovery = onDemandDiscovery;
        return this;
    }

    /**
     * Gets the instruction discovery cache invalidation flag.
     *
     * @return the flag
     */
    public Boolean getRefreshCustomInstructions() {
        return refreshCustomInstructions;
    }

    /**
     * Sets the instruction discovery cache invalidation flag.
     *
     * @param refreshCustomInstructions
     *            the flag
     * @see CreateSessionInstructionOptions#setRefreshCustomInstructions(boolean)
     */
    public SessionInstructionRequestOptions setRefreshCustomInstructions(boolean refreshCustomInstructions) {
        this.refreshCustomInstructions = refreshCustomInstructions;
        return this;
    }

    /**
     * Clears the onDemandDiscovery setting, reverting to the default behavior.
     */
    public void clearOnDemandDiscovery() {
        this.onDemandDiscovery = null;
    }

    /** Clears the instruction discovery cache invalidation flag. */
    public void clearRefreshCustomInstructions() {
        this.refreshCustomInstructions = null;
    }
}
