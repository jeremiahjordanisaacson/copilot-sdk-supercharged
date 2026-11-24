/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Non-prompting host-policy decision. Defer preserves the runtime's existing judge and permission-handler path.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum HostPolicyPreCheckDecision {
    /** The {@code approve} variant. */
    APPROVE("approve"),
    /** The {@code defer} variant. */
    DEFER("defer");

    private final String value;
    HostPolicyPreCheckDecision(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static HostPolicyPreCheckDecision fromValue(String value) {
        for (HostPolicyPreCheckDecision v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown HostPolicyPreCheckDecision value: " + value);
    }
}
