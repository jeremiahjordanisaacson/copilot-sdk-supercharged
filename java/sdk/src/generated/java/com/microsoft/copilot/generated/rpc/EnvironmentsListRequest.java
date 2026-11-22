/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.Objects;
import javax.annotation.processing.Generated;

/**
 * Optional discovery filters supported by GitHub Mission Control.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class EnvironmentsListRequest {

    /** Restrict discovery to this compute kind. */
    @JsonProperty("kind")
    private EnvironmentKind kind;

    /** Operational status, such as online, offline, degraded, waking, or draining. */
    @JsonProperty("status")
    private String status;

    /** Use the server's configured account instead of the connection's foreground account. Omit or set false to retain connection authentication. */
    @JsonProperty("useConfiguredAccount")
    private Boolean useConfiguredAccount;

    /**
     * Creates a request with its required inputs.
     */
    public EnvironmentsListRequest() {
    }

    /**
     * Returns the {@code kind} property.
     *
     * @return Restrict discovery to this compute kind.
     */
    public EnvironmentKind getKind() {
        return kind;
    }

    /**
     * Returns the {@code status} property.
     *
     * @return Operational status, such as online, offline, degraded, waking, or draining.
     */
    public String getStatus() {
        return status;
    }

    /**
     * Returns the {@code useConfiguredAccount} property.
     *
     * @return Use the server's configured account instead of the connection's foreground account. Omit or set false to retain connection authentication.
     */
    Boolean getUseConfiguredAccount() {
        return useConfiguredAccount;
    }

    /**
     * Sets the {@code kind} property.
     *
     * @param value Restrict discovery to this compute kind.
     * @return this request
     */
    public EnvironmentsListRequest setKind(EnvironmentKind value) {
        this.kind = value;
        return this;
    }

    /**
     * Sets the {@code status} property.
     *
     * @param value Operational status, such as online, offline, degraded, waking, or draining.
     * @return this request
     */
    public EnvironmentsListRequest setStatus(String value) {
        this.status = value;
        return this;
    }

    /**
     * Sets the {@code useConfiguredAccount} property.
     *
     * @param value Use the server's configured account instead of the connection's foreground account. Omit or set false to retain connection authentication.
     * @return this request
     */
    EnvironmentsListRequest setUseConfiguredAccount(Boolean value) {
        this.useConfiguredAccount = value;
        return this;
    }
}
