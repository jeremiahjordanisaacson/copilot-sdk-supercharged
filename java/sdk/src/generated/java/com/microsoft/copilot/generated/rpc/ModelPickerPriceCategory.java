/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Relative cost tier for token-based billing users
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ModelPickerPriceCategory {
    /** The {@code low} variant. */
    public static final ModelPickerPriceCategory LOW = new ModelPickerPriceCategory("low");
    /** The {@code medium} variant. */
    public static final ModelPickerPriceCategory MEDIUM = new ModelPickerPriceCategory("medium");
    /** The {@code high} variant. */
    public static final ModelPickerPriceCategory HIGH = new ModelPickerPriceCategory("high");
    /** The {@code very_high} variant. */
    public static final ModelPickerPriceCategory VERY_HIGH = new ModelPickerPriceCategory("very_high");
    /** The default value when no recognized variant is available. */
    public static final ModelPickerPriceCategory UNKNOWN = new ModelPickerPriceCategory("unknown");

    private final String value;
    private ModelPickerPriceCategory(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ModelPickerPriceCategory fromValue(String value) {
        if (LOW.value.equals(value)) return LOW;
        if (MEDIUM.value.equals(value)) return MEDIUM;
        if (HIGH.value.equals(value)) return HIGH;
        if (VERY_HIGH.value.equals(value)) return VERY_HIGH;
        if (UNKNOWN.value.equals(value)) return UNKNOWN;
        if (value != null) return new ModelPickerPriceCategory(value);
        throw new IllegalArgumentException("Unknown ModelPickerPriceCategory value: " + value);
    }
    @Override
    public boolean equals(Object other) {
        return other instanceof ModelPickerPriceCategory that && value.equals(that.value);
    }
    @Override
    public int hashCode() { return value.hashCode(); }
}
