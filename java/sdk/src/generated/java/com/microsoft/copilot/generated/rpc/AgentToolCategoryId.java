/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * A tool group that an agent file can enable.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum AgentToolCategoryId {
    /** The {@code shell} variant. */
    SHELL("shell"),
    /** The {@code read} variant. */
    READ("read"),
    /** The {@code edit} variant. */
    EDIT("edit"),
    /** The {@code subagents} variant. */
    SUBAGENTS("subagents"),
    /** The {@code skills} variant. */
    SKILLS("skills"),
    /** The {@code web} variant. */
    WEB("web"),
    /** The {@code ask-user} variant. */
    ASK_USER("ask-user");

    private final String value;
    AgentToolCategoryId(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static AgentToolCategoryId fromValue(String value) {
        for (AgentToolCategoryId v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown AgentToolCategoryId value: " + value);
    }
}
