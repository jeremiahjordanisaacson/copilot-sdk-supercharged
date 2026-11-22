/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import javax.annotation.processing.Generated;

/**
 * A form or URL elicitation prompt.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "mode", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = UIElicitationPromptForm.class, name = "form"),
    @JsonSubTypes.Type(value = UIElicitationPromptUrl.class, name = "url")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public abstract class UIElicitationPrompt {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the mode discriminator
     */
    public abstract String getMode();
}
