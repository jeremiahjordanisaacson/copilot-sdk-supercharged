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
 * A pre-session login action. Every action after open retains the same credential and persistence owner.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "operation", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = AccountLoginProcessParamsOpen.class, name = "open"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsToken.class, name = "token"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsStartDevice.class, name = "startDevice"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsStartWeb.class, name = "startWeb"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsComplete.class, name = "complete"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsOpenBrowser.class, name = "openBrowser"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsContinue.class, name = "continue"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsPolicy.class, name = "policy"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsDiscard.class, name = "discard"),
    @JsonSubTypes.Type(value = AccountLoginProcessParamsClose.class, name = "close")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
abstract class AccountLoginProcessParams {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the operation discriminator
     */
    public abstract String getOperation();
}
