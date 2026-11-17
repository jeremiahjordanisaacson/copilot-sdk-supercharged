/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Current policy state for this model
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ModelPolicyState {
    /** The {@code enabled} variant. */
    public static final ModelPolicyState ENABLED = new ModelPolicyState("enabled");
    /** The {@code disabled} variant. */
    public static final ModelPolicyState DISABLED = new ModelPolicyState("disabled");
    /** The {@code unconfigured} variant. */
    public static final ModelPolicyState UNCONFIGURED = new ModelPolicyState("unconfigured");
    /** The default value when no recognized variant is available. */
    public static final ModelPolicyState UNKNOWN = new ModelPolicyState("unknown");

    private final String value;
    private ModelPolicyState(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ModelPolicyState fromValue(String value) {
        if (ENABLED.value.equals(value)) return ENABLED;
        if (DISABLED.value.equals(value)) return DISABLED;
        if (UNCONFIGURED.value.equals(value)) return UNCONFIGURED;
        if (UNKNOWN.value.equals(value)) return UNKNOWN;
        if (value != null) return new ModelPolicyState(value);
        throw new IllegalArgumentException("Unknown ModelPolicyState value: " + value);
    }
    @Override
    public boolean equals(Object other) {
        return other instanceof ModelPolicyState that && value.equals(that.value);
    }
    @Override
    public int hashCode() { return value.hashCode(); }
}
