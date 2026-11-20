/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Private transition in an admitted host's credential lifecycle.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum HostCredentialLifecycleAction {
    /** The {@code begin} variant. */
    BEGIN("begin"),
    /** The {@code bind} variant. */
    BIND("bind"),
    /** The {@code adopt} variant. */
    ADOPT("adopt"),
    /** The {@code restore} variant. */
    RESTORE("restore"),
    /** The {@code discard} variant. */
    DISCARD("discard"),
    /** The {@code accept} variant. */
    ACCEPT("accept"),
    /** The {@code auth} variant. */
    AUTH("auth"),
    /** The {@code session} variant. */
    SESSION("session"),
    /** The {@code release} variant. */
    RELEASE("release"),
    /** The {@code restoreSession} variant. */
    RESTORESESSION("restoreSession");

    private final String value;
    HostCredentialLifecycleAction(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static HostCredentialLifecycleAction fromValue(String value) {
        for (HostCredentialLifecycleAction v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown HostCredentialLifecycleAction value: " + value);
    }
}
