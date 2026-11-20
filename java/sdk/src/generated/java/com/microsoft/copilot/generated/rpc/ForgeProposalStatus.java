/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Persisted proposal lifecycle state.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum ForgeProposalStatus {
    /** The {@code generating} variant. */
    GENERATING("generating"),
    /** The {@code ready} variant. */
    READY("ready"),
    /** The {@code reviewing} variant. */
    REVIEWING("reviewing"),
    /** The {@code accepted} variant. */
    ACCEPTED("accepted"),
    /** The {@code rejected} variant. */
    REJECTED("rejected"),
    /** The {@code superseded} variant. */
    SUPERSEDED("superseded"),
    /** The {@code failed} variant. */
    FAILED("failed");

    private final String value;
    ForgeProposalStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ForgeProposalStatus fromValue(String value) {
        for (ForgeProposalStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown ForgeProposalStatus value: " + value);
    }
}
