/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
/** Grouped CreateSessionInstruction configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CreateSessionInstructionOptions {

    private Boolean skipCustomInstructions;
    private List<String> directories;
    private String organizationText;
    private Boolean onDemandDiscovery;
    private Boolean refreshCustomInstructions;

    /**
     * Gets whether custom instruction file loading is suppressed.
     *
     * @return {@code true} to suppress, or empty if not explicitly set
     * @since 1.3.0
     */
    @JsonIgnore
    public Optional<Boolean> getSkipCustomInstructions() {
        return Optional.ofNullable(skipCustomInstructions);
    }

    /**
     * Sets whether to suppress loading of custom instruction files (e.g.
     * {@code .github/copilot-instructions.md}, {@code AGENTS.md}) from the working
     * directory.
     * <p>
     * This option is sent to the server via a {@code session.options.update}
     * JSON-RPC call immediately after session creation. In
     * {@link CopilotClientMode#EMPTY EMPTY} mode the default is {@code true}
     * (skip); in {@link CopilotClientMode#COPILOT_CLI COPILOT_CLI} mode the value
     * is forwarded only when explicitly set.
     *
     * @param skipCustomInstructions
     *            whether to skip custom instructions
     * @return this config instance for method chaining
     * @since 1.3.0
     */
    public CreateSessionInstructionOptions setSkipCustomInstructions(boolean skipCustomInstructions) {
        this.skipCustomInstructions = skipCustomInstructions;
        return this;
    }

    /**
     * Gets explicit discovery-only instruction roots using existing SDK formats. No
     * general filesystem grant. Available without configuration discovery, subject
     * to instruction/managed disables. Relative paths resolve once when supplied
     * against the request's effective working directory; retained roots do not
     * move. Null and omission are equivalent; an empty list clears only the SDK
     * contribution.
     *
     * @return the list of instruction directory paths
     */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets additional directories to search for custom instruction files.
     *
     * @param directories
     *            the list of instruction directory paths
     * @return this config instance for method chaining
     */
    public CreateSessionInstructionOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }

    /**
     * Gets the organization-level custom instructions.
     *
     * @return the organization-level custom instructions, or {@code null} if not
     *         set
     */
    public String getOrganizationText() {
        return organizationText;
    }

    /**
     * Sets organization-level custom instructions.
     *
     * @param organizationText
     *            the organization-level custom instructions
     * @return this config instance for method chaining
     */
    public CreateSessionInstructionOptions setOrganizationText(String organizationText) {
        this.organizationText = organizationText;
        return this;
    }

    /**
     * Gets whether on-demand instruction file discovery is enabled.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable
     *         on-demand discovery or {@code false} to disable it, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getOnDemandDiscovery() {
        return Optional.ofNullable(onDemandDiscovery);
    }

    /**
     * Sets whether instruction files are discovered on demand.
     *
     * @param onDemandDiscovery
     *            {@code true} to enable on-demand instruction discovery,
     *            {@code false} to disable it
     * @return this config instance for method chaining
     */
    public CreateSessionInstructionOptions setOnDemandDiscovery(boolean onDemandDiscovery) {
        this.onDemandDiscovery = onDemandDiscovery;
        return this;
    }

    /**
     * Gets whether to invalidate process-wide instruction discovery caches before
     * constructing the new session.
     *
     * @return an optional flag; {@code false} or empty reuses cached discovery
     */
    @JsonIgnore
    public Optional<Boolean> getRefreshCustomInstructions() {
        return Optional.ofNullable(refreshCustomInstructions);
    }

    /**
     * Sets whether to invalidate process-wide instruction discovery caches before
     * constructing the new session. Defaults to {@code false}, reusing cached
     * discovery. Other sessions in this runtime may observe updated instructions on
     * later turns or discovery. This does not watch files or override instruction
     * loading policy.
     *
     * @param refreshCustomInstructions
     *            {@code true} to invalidate caches, {@code false} to reuse them
     * @return this config instance for method chaining
     */
    public CreateSessionInstructionOptions setRefreshCustomInstructions(boolean refreshCustomInstructions) {
        this.refreshCustomInstructions = refreshCustomInstructions;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public CreateSessionInstructionOptions clone() {
        CreateSessionInstructionOptions copy = new CreateSessionInstructionOptions();
        copy.skipCustomInstructions = this.skipCustomInstructions;
        copy.directories = this.directories != null ? new ArrayList<>(this.directories) : null;
        copy.organizationText = this.organizationText;
        copy.onDemandDiscovery = this.onDemandDiscovery;
        copy.refreshCustomInstructions = this.refreshCustomInstructions;
        return copy;
    }

    /**
     * Clears the skipCustomInstructions setting.
     *
     * @return this instance for method chaining
     * @since 1.3.0
     */
    public CreateSessionInstructionOptions clearSkipCustomInstructions() {
        this.skipCustomInstructions = null;
        return this;
    }

    /**
     * Clears the onDemandDiscovery setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public CreateSessionInstructionOptions clearOnDemandDiscovery() {
        this.onDemandDiscovery = null;
        return this;
    }

    /**
     * Clears the refreshCustomInstructions setting, restoring default cache reuse.
     *
     * @return this config instance for method chaining
     */
    public CreateSessionInstructionOptions clearRefreshCustomInstructions() {
        this.refreshCustomInstructions = null;
        return this;
    }
}
