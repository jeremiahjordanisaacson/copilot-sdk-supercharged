/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.Optional;
/** Grouped SessionState configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionStateOptions {

    private InfiniteSessionConfig infiniteSessions;
    private MemoryConfiguration memory;
    private Boolean skipEmbeddingRetrieval;
    private Boolean enableCrossSessionStore;
    private String embeddingCacheStorage;

    /**
     * Gets the infinite session configuration.
     *
     * @return the infinite session config
     */
    public InfiniteSessionConfig getInfiniteSessions() {
        return infiniteSessions;
    }

    /**
     * Sets the infinite session configuration for persistent workspaces and
     * automatic compaction.
     *
     * @param infiniteSessions
     *            the infinite session configuration
     * @return this config for method chaining
     * @see InfiniteSessionConfig
     */
    public SessionStateOptions setInfiniteSessions(InfiniteSessionConfig infiniteSessions) {
        this.infiniteSessions = infiniteSessions;
        return this;
    }

    /**
     * Gets the configuration for session memory.
     *
     * @return the memory config, or {@code null} for default
     */
    public MemoryConfiguration getMemory() {
        return memory;
    }

    /**
     * Sets the configuration for session memory.
     *
     * @param memory
     *            the memory config
     * @return this config for method chaining
     */
    public SessionStateOptions setMemory(MemoryConfiguration memory) {
        this.memory = memory;
        return this;
    }

    /**
     * Gets whether embedding-based retrieval is skipped.
     *
     * @return an {@link java.util.Optional} containing {@code true} to skip
     *         embedding retrieval or {@code false} to force it, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getSkipEmbeddingRetrieval() {
        return Optional.ofNullable(skipEmbeddingRetrieval);
    }

    /**
     * Sets whether to skip embedding-based retrieval.
     *
     * @param skipEmbeddingRetrieval
     *            {@code true} to skip embedding retrieval, {@code false} to keep it
     *            enabled
     * @return this config for method chaining
     */
    public SessionStateOptions setSkipEmbeddingRetrieval(boolean skipEmbeddingRetrieval) {
        this.skipEmbeddingRetrieval = skipEmbeddingRetrieval;
        return this;
    }

    /**
     * Gets whether the cross-session store is enabled.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable the
     *         session store or {@code false} to disable it, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getEnableCrossSessionStore() {
        return Optional.ofNullable(enableCrossSessionStore);
    }

    /**
     * Sets whether the cross-session store is enabled.
     *
     * @param enableCrossSessionStore
     *            {@code true} to enable the session store, {@code false} to disable
     *            it
     * @return this config for method chaining
     */
    public SessionStateOptions setEnableCrossSessionStore(boolean enableCrossSessionStore) {
        this.enableCrossSessionStore = enableCrossSessionStore;
        return this;
    }

    /**
     * Gets the embedding cache storage mode.
     *
     * @return the embedding cache storage mode ({@code "persistent"} or
     *         {@code "in-memory"}), or {@code null} to use the default behavior
     */
    public String getEmbeddingCacheStorage() {
        return embeddingCacheStorage;
    }

    /**
     * Sets the embedding cache storage mode.
     *
     * @param embeddingCacheStorage
     *            {@code "persistent"} to persist embeddings across sessions, or
     *            {@code "in-memory"} for session-scoped storage
     * @return this config for method chaining
     */
    public SessionStateOptions setEmbeddingCacheStorage(String embeddingCacheStorage) {
        this.embeddingCacheStorage = embeddingCacheStorage;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionStateOptions clone() {
        SessionStateOptions copy = new SessionStateOptions();
        copy.skipEmbeddingRetrieval = this.skipEmbeddingRetrieval;
        copy.enableCrossSessionStore = this.enableCrossSessionStore;
        copy.embeddingCacheStorage = this.embeddingCacheStorage;
        copy.memory = this.memory;
        copy.infiniteSessions = this.infiniteSessions;
        return copy;
    }

    /**
     * Clears the skipEmbeddingRetrieval setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionStateOptions clearSkipEmbeddingRetrieval() {
        this.skipEmbeddingRetrieval = null;
        return this;
    }

    /**
     * Clears the enableCrossSessionStore setting, reverting to the default
     * behavior.
     *
     * @return this instance for method chaining
     */
    public SessionStateOptions clearEnableCrossSessionStore() {
        this.enableCrossSessionStore = null;
        return this;
    }

    /**
     * Clears the embeddingCacheStorage setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionStateOptions clearEmbeddingCacheStorage() {
        this.embeddingCacheStorage = null;
        return this;
    }
}
