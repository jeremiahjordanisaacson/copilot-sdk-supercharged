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
 * A single top-level setting subtree to validate.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
final class UserSettingsValidateRequest {
    @com.fasterxml.jackson.annotation.JsonIgnore
    private final java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();

    /** Top-level setting name to validate. */
    @JsonProperty("topKey")
    private final String topKey;

    /** Value to validate. Omitted when `delete` is true. */
    @JsonProperty("value")
    private Object value;

    /** True to validate a removal of this key. The current settings file is not read. */
    @JsonProperty("delete")
    private Boolean delete;

    /** Optional trusted-host context whose environment controls hook validation. */
    @JsonProperty("context")
    private UserSettingsHostContext context;

    /**
     * Creates a request with its required inputs.
     *
     * @param topKey Top-level setting name to validate.
     */
    public UserSettingsValidateRequest(String topKey) {
        this.topKey = Objects.requireNonNull(topKey, "topKey");
    }

    /**
     * Returns the {@code topKey} property.
     *
     * @return Top-level setting name to validate.
     */
    public String getTopKey() {
        return topKey;
    }

    /**
     * Returns the {@code value} property.
     *
     * @return Value to validate. Omitted when `delete` is true.
     */
    public Object getValue() {
        return value;
    }

    /**
     * Returns the {@code delete} property.
     *
     * @return True to validate a removal of this key. The current settings file is not read.
     */
    public Boolean getDelete() {
        return delete;
    }

    /**
     * Returns the {@code context} property.
     *
     * @return Optional trusted-host context whose environment controls hook validation.
     */
    public UserSettingsHostContext getContext() {
        return context;
    }

    /**
     * Sets the {@code value} property.
     *
     * @param value Value to validate. Omitted when `delete` is true.
     * @return this request
     */
    public UserSettingsValidateRequest setValue(Object value) {
        this.value = value;
        if (value == null) $explicitNulls.add("value");
        else $explicitNulls.remove("value");
        return this;
    }

    /**
     * Sets the {@code delete} property.
     *
     * @param value True to validate a removal of this key. The current settings file is not read.
     * @return this request
     */
    public UserSettingsValidateRequest setDelete(Boolean value) {
        this.delete = value;
        return this;
    }

    /**
     * Sets the {@code context} property.
     *
     * @param value Optional trusted-host context whose environment controls hook validation.
     * @return this request
     */
    public UserSettingsValidateRequest setContext(UserSettingsHostContext value) {
        this.context = value;
        return this;
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("value") && value == null) values.put("value", null);
        return values;
    }
}
