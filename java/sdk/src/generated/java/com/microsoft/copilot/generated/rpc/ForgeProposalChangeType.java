/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Existing persisted file-change classification.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum ForgeProposalChangeType {
    /** The {@code added} variant. */
    ADDED("added"),
    /** The {@code modified} variant. */
    MODIFIED("modified"),
    /** The {@code deleted} variant. */
    DELETED("deleted"),
    /** The {@code renamed} variant. */
    RENAMED("renamed");

    private final String value;
    ForgeProposalChangeType(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ForgeProposalChangeType fromValue(String value) {
        for (ForgeProposalChangeType v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown ForgeProposalChangeType value: " + value);
    }
}
