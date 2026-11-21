/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * The removed skill's storage tier.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum SkillResourceRemoveKind {
    /** The {@code directory} variant. */
    DIRECTORY("directory"),
    /** The {@code personal} variant. */
    PERSONAL("personal"),
    /** The {@code project} variant. */
    PROJECT("project");

    private final String value;
    SkillResourceRemoveKind(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SkillResourceRemoveKind fromValue(String value) {
        for (SkillResourceRemoveKind v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SkillResourceRemoveKind value: " + value);
    }
}
