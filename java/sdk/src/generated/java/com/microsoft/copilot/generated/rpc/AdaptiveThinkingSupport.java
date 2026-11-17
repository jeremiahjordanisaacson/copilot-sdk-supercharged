/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Resolved Anthropic adaptive-thinking capability for a model.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class AdaptiveThinkingSupport {
    /** The {@code unsupported} variant. */
    public static final AdaptiveThinkingSupport UNSUPPORTED = new AdaptiveThinkingSupport("unsupported");
    /** The {@code optional} variant. */
    public static final AdaptiveThinkingSupport OPTIONAL = new AdaptiveThinkingSupport("optional");
    /** The {@code required} variant. */
    public static final AdaptiveThinkingSupport REQUIRED = new AdaptiveThinkingSupport("required");
    /** The {@code adaptive_only} variant. */
    public static final AdaptiveThinkingSupport ADAPTIVE_ONLY = new AdaptiveThinkingSupport("adaptive_only");
    /** The default value when no recognized variant is available. */
    public static final AdaptiveThinkingSupport UNKNOWN = new AdaptiveThinkingSupport("unknown");

    private final String value;
    private AdaptiveThinkingSupport(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static AdaptiveThinkingSupport fromValue(String value) {
        if (UNSUPPORTED.value.equals(value)) return UNSUPPORTED;
        if (OPTIONAL.value.equals(value)) return OPTIONAL;
        if (REQUIRED.value.equals(value)) return REQUIRED;
        if (ADAPTIVE_ONLY.value.equals(value)) return ADAPTIVE_ONLY;
        if (UNKNOWN.value.equals(value)) return UNKNOWN;
        if (value != null) return new AdaptiveThinkingSupport(value);
        throw new IllegalArgumentException("Unknown AdaptiveThinkingSupport value: " + value);
    }
    @Override
    public boolean equals(Object other) {
        return other instanceof AdaptiveThinkingSupport that && value.equals(that.value);
    }
    @Override
    public int hashCode() { return value.hashCode(); }
}
