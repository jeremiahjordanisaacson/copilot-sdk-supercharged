/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotRequestHandler;
import com.microsoft.copilot.generated.rpc.Model;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;
/** Grouped ClientModel configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientModelOptions {

    private Supplier<CompletableFuture<List<Model>>> onListModels;
    private CopilotRequestHandler requestHandler;

    /**
     * Gets the custom handler for listing available models.
     *
     * @return the handler, or {@code null} if not set
     */
    @JsonIgnore
    public Supplier<CompletableFuture<List<Model>>> getOnListModels() {
        return onListModels;
    }

    /**
     * Sets a custom handler for listing available models.
     * <p>
     * When provided, {@code listModels()} calls this handler instead of querying
     * the CLI server. Useful in BYOK (Bring Your Own Key) mode to return models
     * available from your custom provider.
     *
     * @param onListModels
     *            the handler that returns the list of available models (must not be
     *            {@code null})
     * @return this options instance for method chaining
     * @throws IllegalArgumentException
     *             if {@code onListModels} is {@code null}
     */
    public ClientModelOptions setOnListModels(Supplier<CompletableFuture<List<Model>>> onListModels) {
        this.onListModels = Objects.requireNonNull(onListModels, "onListModels must not be null");
        return this;
    }

    /**
     * Gets the connection-level LLM inference request handler.
     *
     * @return the request handler, or {@code null} if not set
     */
    @JsonIgnore
    public CopilotRequestHandler getRequestHandler() {
        return requestHandler;
    }

    /**
     * Sets a connection-level LLM inference request handler.
     * <p>
     * When provided, the client registers as the runtime's LLM inference provider
     * on connect, and the runtime routes its model-layer HTTP and WebSocket traffic
     * (both BYOK and CAPI) through the handler instead of issuing the calls itself.
     *
     * @param requestHandler
     *            the request handler (must not be {@code null})
     * @return this options instance for method chaining
     * @throws IllegalArgumentException
     *             if {@code requestHandler} is {@code null}
     */
    public ClientModelOptions setRequestHandler(CopilotRequestHandler requestHandler) {
        this.requestHandler = Objects.requireNonNull(requestHandler, "requestHandler must not be null");
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientModelOptions clone() {
        ClientModelOptions copy = new ClientModelOptions();
        copy.onListModels = this.onListModels;
        copy.requestHandler = this.requestHandler;
        return copy;
    }

}
