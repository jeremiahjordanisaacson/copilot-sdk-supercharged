/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
/** Grouped SessionStateRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionStateRequestOptions {

    @JsonProperty("infiniteSessions")
    private InfiniteSessionConfig infiniteSessions;

    @JsonProperty("memory")
    private MemoryConfiguration memory;

    @JsonProperty("skipEmbeddingRetrieval")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean skipEmbeddingRetrieval;

    @JsonProperty("enableCrossSessionStore")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enableCrossSessionStore;

    @JsonProperty("hasMemoryStore")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean hasMemoryStore;

    @JsonProperty("embeddingCacheStorage")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String embeddingCacheStorage;

    /** Gets infinite sessions config. @return the infinite sessions config */
    public InfiniteSessionConfig getInfiniteSessions() {
        return infiniteSessions;
    }

    /**
     * Sets infinite sessions config. @param infiniteSessions the infinite sessions
     * config
     */
    public SessionStateRequestOptions setInfiniteSessions(InfiniteSessionConfig infiniteSessions) {
        this.infiniteSessions = infiniteSessions;
        return this;
    }

    /** Gets memory config. @return the memory config */
    public MemoryConfiguration getMemory() {
        return memory;
    }

    /** Sets memory config. @param memory the memory config */
    public SessionStateRequestOptions setMemory(MemoryConfiguration memory) {
        this.memory = memory;
        return this;
    }

    /** Gets skip embedding retrieval flag. @return the flag */
    public Boolean getSkipEmbeddingRetrieval() {
        return skipEmbeddingRetrieval;
    }

    /**
     * Sets skip embedding retrieval flag. @param skipEmbeddingRetrieval the flag
     */
    public SessionStateRequestOptions setSkipEmbeddingRetrieval(boolean skipEmbeddingRetrieval) {
        this.skipEmbeddingRetrieval = skipEmbeddingRetrieval;
        return this;
    }

    /** Gets enable session store flag. @return the flag */
    public Boolean getEnableCrossSessionStore() {
        return enableCrossSessionStore;
    }

    /** Sets enable session store flag. @param enableCrossSessionStore the flag */
    public SessionStateRequestOptions setEnableCrossSessionStore(boolean enableCrossSessionStore) {
        this.enableCrossSessionStore = enableCrossSessionStore;
        return this;
    }

    /** Gets memory-store availability flag. @return the flag */
    public Boolean getHasMemoryStore() {
        return hasMemoryStore;
    }

    /** Sets memory-store availability flag. @param hasMemoryStore the flag */
    public SessionStateRequestOptions setHasMemoryStore(boolean hasMemoryStore) {
        this.hasMemoryStore = hasMemoryStore;
        return this;
    }

    /** Gets embedding cache storage mode. @return the mode */
    public String getEmbeddingCacheStorage() {
        return embeddingCacheStorage;
    }

    /** Sets embedding cache storage mode. @param embeddingCacheStorage the mode */
    public SessionStateRequestOptions setEmbeddingCacheStorage(String embeddingCacheStorage) {
        this.embeddingCacheStorage = embeddingCacheStorage;
        return this;
    }

    /**
     * Clears the skipEmbeddingRetrieval setting, reverting to the default behavior.
     */
    public void clearSkipEmbeddingRetrieval() {
        this.skipEmbeddingRetrieval = null;
    }

    /**
     * Clears the enableCrossSessionStore setting, reverting to the default
     * behavior.
     */
    public void clearEnableCrossSessionStore() {
        this.enableCrossSessionStore = null;
    }

    /** Clears the hasMemoryStore setting, reverting to the default behavior. */
    public void clearHasMemoryStore() {
        this.hasMemoryStore = null;
    }

    /**
     * Clears the embeddingCacheStorage setting, reverting to the default behavior.
     */
    public void clearEmbeddingCacheStorage() {
        this.embeddingCacheStorage = null;
    }
}
