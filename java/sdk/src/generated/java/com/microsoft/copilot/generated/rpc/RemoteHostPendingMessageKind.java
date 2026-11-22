/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * AHP message queue selection.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum RemoteHostPendingMessageKind {
    /** The {@code steering} variant. */
    STEERING("steering"),
    /** The {@code queued} variant. */
    QUEUED("queued");

    private final String value;
    RemoteHostPendingMessageKind(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static RemoteHostPendingMessageKind fromValue(String value) {
        for (RemoteHostPendingMessageKind v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown RemoteHostPendingMessageKind value: " + value);
    }
}
