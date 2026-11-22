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
 * Session ID whose pending events should be flushed to disk.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class SessionsSaveRequest {
    @com.fasterxml.jackson.annotation.JsonIgnore
    private final java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();

    /** Session ID whose pending events should be flushed to disk */
    @JsonProperty("sessionId")
    private final String sessionId;

    /** Persist an unnamed session even before its first prompt. Omitted or null defaults to false. */
    @JsonProperty("force")
    private Boolean force;

    /**
     * Creates a request with its required inputs.
     *
     * @param sessionId Session ID whose pending events should be flushed to disk
     */
    public SessionsSaveRequest(String sessionId) {
        this.sessionId = Objects.requireNonNull(sessionId, "sessionId");
    }

    /**
     * Returns the {@code sessionId} property.
     *
     * @return Session ID whose pending events should be flushed to disk
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * Returns the {@code force} property.
     *
     * @return Persist an unnamed session even before its first prompt. Omitted or null defaults to false.
     */
    public Boolean getForce() {
        return force;
    }

    /**
     * Sets the {@code force} property.
     *
     * @param value Persist an unnamed session even before its first prompt. Omitted or null defaults to false.
     * @return this request
     */
    public SessionsSaveRequest setForce(Boolean value) {
        this.force = value;
        if (value == null) $explicitNulls.add("force");
        else $explicitNulls.remove("force");
        return this;
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("force") && force == null) values.put("force", null);
        return values;
    }
}
