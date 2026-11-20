/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.Objects;
import javax.annotation.processing.Generated;

/**
 * Adds or removes a single skill from the global disabled list, leaving every other entry untouched.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class SkillsConfigSetSkillDisabledRequest {

    /** Name of the skill to add to or remove from the disabled list */
    @JsonProperty("name")
    private final String name;

    /** True to disable the skill, false to enable it */
    @JsonProperty("disabled")
    private final Boolean disabled;

    /** Configuration directory to update. Omit to use the server's configured directory. */
    @JsonProperty("configDir")
    private String configDir;

    /**
     * Creates a request with its required inputs.
     *
     * @param name Name of the skill to add to or remove from the disabled list
     * @param disabled True to disable the skill, false to enable it
     */
    public SkillsConfigSetSkillDisabledRequest(String name, Boolean disabled) {
        this.name = Objects.requireNonNull(name, "name");
        this.disabled = Objects.requireNonNull(disabled, "disabled");
    }

    /**
     * Returns the {@code name} property.
     *
     * @return Name of the skill to add to or remove from the disabled list
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the {@code disabled} property.
     *
     * @return True to disable the skill, false to enable it
     */
    public Boolean getDisabled() {
        return disabled;
    }

    /**
     * Returns the {@code configDir} property.
     *
     * @return Configuration directory to update. Omit to use the server's configured directory.
     */
    public String getConfigDir() {
        return configDir;
    }

    /**
     * Sets the {@code configDir} property.
     *
     * @param value Configuration directory to update. Omit to use the server's configured directory.
     * @return this request
     */
    public SkillsConfigSetSkillDisabledRequest setConfigDir(String value) {
        this.configDir = value;
        return this;
    }
}
