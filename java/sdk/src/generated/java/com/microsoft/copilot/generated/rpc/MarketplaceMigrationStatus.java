/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * The result of the guarded retired marketplace migration.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum MarketplaceMigrationStatus {
    /** The {@code alreadyRan} variant. */
    ALREADYRAN("alreadyRan"),
    /** The {@code notApplicable} variant. */
    NOTAPPLICABLE("notApplicable"),
    /** The {@code alreadyRegistered} variant. */
    ALREADYREGISTERED("alreadyRegistered"),
    /** The {@code blockedByManagedPolicy} variant. */
    BLOCKEDBYMANAGEDPOLICY("blockedByManagedPolicy"),
    /** The {@code migrated} variant. */
    MIGRATED("migrated");

    private final String value;
    MarketplaceMigrationStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static MarketplaceMigrationStatus fromValue(String value) {
        for (MarketplaceMigrationStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown MarketplaceMigrationStatus value: " + value);
    }
}
