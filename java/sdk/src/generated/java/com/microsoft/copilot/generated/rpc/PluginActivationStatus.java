/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * The result of a built-in plugin activation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum PluginActivationStatus {
    /** The {@code activated} variant. */
    ACTIVATED("activated"),
    /** The {@code notFound} variant. */
    NOTFOUND("notFound"),
    /** The {@code disabled} variant. */
    DISABLED("disabled"),
    /** The {@code noHooks} variant. */
    NOHOOKS("noHooks"),
    /** The {@code failed} variant. */
    FAILED("failed"),
    /** The {@code timedOut} variant. */
    TIMEDOUT("timedOut");

    private final String value;
    PluginActivationStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static PluginActivationStatus fromValue(String value) {
        for (PluginActivationStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown PluginActivationStatus value: " + value);
    }
}
