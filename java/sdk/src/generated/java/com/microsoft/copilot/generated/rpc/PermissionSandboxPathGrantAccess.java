/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Access a sandbox path grant confers
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum PermissionSandboxPathGrantAccess {
    /** The {@code read} variant. */
    READ("read"),
    /** The {@code readWrite} variant. */
    READWRITE("readWrite");

    private final String value;
    PermissionSandboxPathGrantAccess(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static PermissionSandboxPathGrantAccess fromValue(String value) {
        for (PermissionSandboxPathGrantAccess v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown PermissionSandboxPathGrantAccess value: " + value);
    }
}
