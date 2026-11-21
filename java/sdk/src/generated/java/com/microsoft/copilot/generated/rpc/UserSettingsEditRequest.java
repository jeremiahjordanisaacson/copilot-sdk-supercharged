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
 * One atomic edit of a canonical setting in settings.json.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
final class UserSettingsEditRequest {
    @com.fasterxml.jackson.annotation.JsonIgnore
    private final java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();

    /** Nonempty canonical path segments of the setting to edit, starting with its top-level key. */
    @JsonProperty("canonicalPath")
    private final List<String> canonicalPath;

    /** Set a scalar, add or remove a string list item, or remove the setting. The handler rejects a missing or incompatible value. */
    @JsonProperty("operation")
    private final SettingsEditOperation operation;

    /** Configuration directory to edit. Omitted or empty resolves the server's own home, as in `user.settings.loadForConfigDir`. */
    @JsonProperty("configDir")
    private String configDir;

    /** Required boolean, string or number for set, required string for add-item and remove-item, and forbidden for remove. The runtime rejects all other JSON values. */
    @JsonProperty("value")
    private Object value;

    /** Optional trusted-host paths and hook environment. Cannot be combined with configDir. */
    @JsonProperty("context")
    private UserSettingsHostContext context;

    /**
     * Creates a request with its required inputs.
     *
     * @param canonicalPath Nonempty canonical path segments of the setting to edit, starting with its top-level key.
     * @param operation Set a scalar, add or remove a string list item, or remove the setting. The handler rejects a missing or incompatible value.
     */
    public UserSettingsEditRequest(List<String> canonicalPath, SettingsEditOperation operation) {
        this.canonicalPath = Objects.requireNonNull(canonicalPath, "canonicalPath");
        this.operation = Objects.requireNonNull(operation, "operation");
    }

    /**
     * Returns the {@code canonicalPath} property.
     *
     * @return Nonempty canonical path segments of the setting to edit, starting with its top-level key.
     */
    public List<String> getCanonicalPath() {
        return canonicalPath;
    }

    /**
     * Returns the {@code operation} property.
     *
     * @return Set a scalar, add or remove a string list item, or remove the setting. The handler rejects a missing or incompatible value.
     */
    public SettingsEditOperation getOperation() {
        return operation;
    }

    /**
     * Returns the {@code configDir} property.
     *
     * @return Configuration directory to edit. Omitted or empty resolves the server's own home, as in `user.settings.loadForConfigDir`.
     */
    public String getConfigDir() {
        return configDir;
    }

    /**
     * Returns the {@code value} property.
     *
     * @return Required boolean, string or number for set, required string for add-item and remove-item, and forbidden for remove. The runtime rejects all other JSON values.
     */
    public Object getValue() {
        return value;
    }

    /**
     * Returns the {@code context} property.
     *
     * @return Optional trusted-host paths and hook environment. Cannot be combined with configDir.
     */
    public UserSettingsHostContext getContext() {
        return context;
    }

    /**
     * Sets the {@code configDir} property.
     *
     * @param value Configuration directory to edit. Omitted or empty resolves the server's own home, as in `user.settings.loadForConfigDir`.
     * @return this request
     */
    public UserSettingsEditRequest setConfigDir(String value) {
        this.configDir = value;
        return this;
    }

    /**
     * Sets the {@code value} property.
     *
     * @param value Required boolean, string or number for set, required string for add-item and remove-item, and forbidden for remove. The runtime rejects all other JSON values.
     * @return this request
     */
    public UserSettingsEditRequest setValue(Object value) {
        this.value = value;
        if (value == null) $explicitNulls.add("value");
        else $explicitNulls.remove("value");
        return this;
    }

    /**
     * Sets the {@code context} property.
     *
     * @param value Optional trusted-host paths and hook environment. Cannot be combined with configDir.
     * @return this request
     */
    public UserSettingsEditRequest setContext(UserSettingsHostContext value) {
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
