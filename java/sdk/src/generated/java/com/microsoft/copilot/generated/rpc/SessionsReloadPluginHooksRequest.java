/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import java.util.Objects;
import javax.annotation.processing.Generated;

/**
 * Active session ID and an optional flag for deferring repo-level hooks until folder trust.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class SessionsReloadPluginHooksRequest {

    /** Active session ID to reload hooks for */
    @JsonProperty("sessionId")
    private final String sessionId;

    /** When true, skip repo-level hooks. Use before folder trust is confirmed; loadDeferredRepoHooks loads them post-trust. */
    @JsonProperty("deferRepoHooks")
    private Boolean deferRepoHooks;

    /** Managed plugin specifications whose hooks must load before queue release. */
    @JsonProperty("requiredManagedPluginSpecs")
    private List<String> requiredManagedPluginSpecs;

    /** Whether hook reload failures must retain the managed-policy queue pause. */
    @JsonProperty("strictManagedPolicy")
    private Boolean strictManagedPolicy;

    /**
     * Creates a request with its required inputs.
     *
     * @param sessionId Active session ID to reload hooks for
     */
    public SessionsReloadPluginHooksRequest(String sessionId) {
        this.sessionId = Objects.requireNonNull(sessionId, "sessionId");
    }

    /**
     * Returns the {@code sessionId} property.
     *
     * @return Active session ID to reload hooks for
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * Returns the {@code deferRepoHooks} property.
     *
     * @return When true, skip repo-level hooks. Use before folder trust is confirmed; loadDeferredRepoHooks loads them post-trust.
     */
    public Boolean getDeferRepoHooks() {
        return deferRepoHooks;
    }

    /**
     * Returns the {@code requiredManagedPluginSpecs} property.
     *
     * @return Managed plugin specifications whose hooks must load before queue release.
     */
    public List<String> getRequiredManagedPluginSpecs() {
        return requiredManagedPluginSpecs;
    }

    /**
     * Returns the {@code strictManagedPolicy} property.
     *
     * @return Whether hook reload failures must retain the managed-policy queue pause.
     */
    public Boolean getStrictManagedPolicy() {
        return strictManagedPolicy;
    }

    /**
     * Sets the {@code deferRepoHooks} property.
     *
     * @param value When true, skip repo-level hooks. Use before folder trust is confirmed; loadDeferredRepoHooks loads them post-trust.
     * @return this request
     */
    public SessionsReloadPluginHooksRequest setDeferRepoHooks(Boolean value) {
        this.deferRepoHooks = value;
        return this;
    }

    /**
     * Sets the {@code requiredManagedPluginSpecs} property.
     *
     * @param value Managed plugin specifications whose hooks must load before queue release.
     * @return this request
     */
    public SessionsReloadPluginHooksRequest setRequiredManagedPluginSpecs(List<String> value) {
        this.requiredManagedPluginSpecs = value;
        return this;
    }

    /**
     * Sets the {@code strictManagedPolicy} property.
     *
     * @param value Whether hook reload failures must retain the managed-policy queue pause.
     * @return this request
     */
    public SessionsReloadPluginHooksRequest setStrictManagedPolicy(Boolean value) {
        this.strictManagedPolicy = value;
        return this;
    }
}
