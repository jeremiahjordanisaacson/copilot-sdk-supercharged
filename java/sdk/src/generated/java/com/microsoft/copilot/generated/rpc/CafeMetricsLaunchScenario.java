/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Initial requested CLI launch intent, not the eventual selection.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum CafeMetricsLaunchScenario {
    /** The {@code fresh} variant. */
    FRESH("fresh"),
    /** The {@code resume} variant. */
    RESUME("resume"),
    /** The {@code picker} variant. */
    PICKER("picker"),
    /** The {@code remote} variant. */
    REMOTE("remote"),
    /** The {@code restart} variant. */
    RESTART("restart"),
    /** The {@code unknown} variant. */
    UNKNOWN("unknown");

    private final String value;
    CafeMetricsLaunchScenario(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static CafeMetricsLaunchScenario fromValue(String value) {
        for (CafeMetricsLaunchScenario v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown CafeMetricsLaunchScenario value: " + value);
    }
}
