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
 * The result of one connection-owned pre-session login action.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "kind", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = AccountLoginProcessResultOpened.class, name = "opened"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultDeviceCode.class, name = "deviceCode"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultWeb.class, name = "web"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultPersistence.class, name = "persistence"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultPolicy.class, name = "policy"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultDiscarded.class, name = "discarded"),
    @JsonSubTypes.Type(value = AccountLoginProcessResultDone.class, name = "done")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
abstract class AccountLoginProcessResult {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the kind discriminator
     */
    public abstract String getKind();
}
