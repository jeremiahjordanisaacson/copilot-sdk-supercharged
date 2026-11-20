/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * How the provider resolved the sensitivity label for one model request
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum SensitivityLabelOutcome {
    /** The {@code inherited} variant. */
    INHERITED("inherited"),
    /** The {@code no_source_labels} variant. */
    NO_SOURCE_LABELS("no_source_labels"),
    /** The {@code no_applicable_label} variant. */
    NO_APPLICABLE_LABEL("no_applicable_label"),
    /** The {@code indeterminate} variant. */
    INDETERMINATE("indeterminate");

    private final String value;
    SensitivityLabelOutcome(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SensitivityLabelOutcome fromValue(String value) {
        for (SensitivityLabelOutcome v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SensitivityLabelOutcome value: " + value);
    }
}
