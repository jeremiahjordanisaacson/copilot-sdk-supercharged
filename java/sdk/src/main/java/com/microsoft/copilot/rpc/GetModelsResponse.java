/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import com.microsoft.copilot.generated.rpc.Model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * Response from the models.list RPC call.
 * <p>
 * Contains a list of available models with their metadata.
 *
 * @since 1.0.1
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class GetModelsResponse {

    @JsonProperty("models")
    private List<Model> models;

    public List<Model> getModels() {
        return models;
    }

    public GetModelsResponse setModels(List<Model> models) {
        this.models = models;
        return this;
    }
}
