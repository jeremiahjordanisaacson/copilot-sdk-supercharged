/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Model capability category for grouping in the model picker
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ModelPickerCategory {
    /** The {@code lightweight} variant. */
    public static final ModelPickerCategory LIGHTWEIGHT = new ModelPickerCategory("lightweight");
    /** The {@code versatile} variant. */
    public static final ModelPickerCategory VERSATILE = new ModelPickerCategory("versatile");
    /** The {@code powerful} variant. */
    public static final ModelPickerCategory POWERFUL = new ModelPickerCategory("powerful");
    /** The default value when no recognized variant is available. */
    public static final ModelPickerCategory UNKNOWN = new ModelPickerCategory("unknown");

    private final String value;
    private ModelPickerCategory(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static ModelPickerCategory fromValue(String value) {
        if (LIGHTWEIGHT.value.equals(value)) return LIGHTWEIGHT;
        if (VERSATILE.value.equals(value)) return VERSATILE;
        if (POWERFUL.value.equals(value)) return POWERFUL;
        if (UNKNOWN.value.equals(value)) return UNKNOWN;
        if (value != null) return new ModelPickerCategory(value);
        throw new IllegalArgumentException("Unknown ModelPickerCategory value: " + value);
    }
    @Override
    public boolean equals(Object other) {
        return other instanceof ModelPickerCategory that && value.equals(that.value);
    }
    @Override
    public int hashCode() { return value.hashCode(); }
}
