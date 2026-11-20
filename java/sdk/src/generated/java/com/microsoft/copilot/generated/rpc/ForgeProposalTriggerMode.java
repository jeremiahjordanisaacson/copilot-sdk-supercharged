/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Original proposal generation trigger.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum ForgeProposalTriggerMode {
    /** The {@code auto_close} variant. */
    AUTO_CLOSE("auto_close"),
    /** The {@code on_demand} variant. */
    ON_DEMAND("on_demand");

    private final String value;
    ForgeProposalTriggerMode(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ForgeProposalTriggerMode fromValue(String value) {
        for (ForgeProposalTriggerMode v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown ForgeProposalTriggerMode value: " + value);
    }
}
