/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * The source of an installed or registered skill.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum SkillResourceInstallKind {
    /** The {@code file} variant. */
    FILE("file"),
    /** The {@code url} variant. */
    URL("url"),
    /** The {@code directory} variant. */
    DIRECTORY("directory");

    private final String value;
    SkillResourceInstallKind(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SkillResourceInstallKind fromValue(String value) {
        for (SkillResourceInstallKind v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SkillResourceInstallKind value: " + value);
    }
}
