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
 * Reply from the original controller's process-launch delegate.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "kind", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = EmbedderAgentRegistrySpawnResultResult.class, name = "result"),
    @JsonSubTypes.Type(value = EmbedderAgentRegistrySpawnResultResponse.class, name = "response"),
    @JsonSubTypes.Type(value = EmbedderAgentRegistrySpawnResultMessage.class, name = "message"),
    @JsonSubTypes.Type(value = EmbedderAgentRegistrySpawnResultUnexpected.class, name = "unexpected")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
abstract class EmbedderAgentRegistrySpawnResult {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the kind discriminator
     */
    public abstract String getKind();
}
