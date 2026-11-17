/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionHookRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionHookRequestOptions {

    @JsonProperty("hasHandlers")
    private Boolean hasHandlers;

    @JsonProperty("directories")
    private List<String> directories;

    @JsonProperty("enableRepositoryHooks")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enableRepositoryHooks;

    /** Gets hasHandlers flag. @return the flag */
    public Boolean getHasHandlers() {
        return hasHandlers;
    }

    /** Sets hasHandlers flag. @param hasHandlers the flag */
    public SessionHookRequestOptions setHasHandlers(boolean hasHandlers) {
        this.hasHandlers = hasHandlers;
        return this;
    }

    /** Gets explicit file-hook discovery roots. @return the directories */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets explicit file-hook discovery roots. @param directories the directories
     */
    public SessionHookRequestOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }

    /** Gets enable file hooks flag. @return the flag */
    public Boolean getEnableRepositoryHooks() {
        return enableRepositoryHooks;
    }

    /** Sets enable file hooks flag. @param enableRepositoryHooks the flag */
    public SessionHookRequestOptions setEnableRepositoryHooks(boolean enableRepositoryHooks) {
        this.enableRepositoryHooks = enableRepositoryHooks;
        return this;
    }

    /**
     * Clears the hasHandlers setting, reverting to the default behavior.
     */
    public void clearHasHandlers() {
        this.hasHandlers = null;
    }

    /**
     * Clears the enableRepositoryHooks setting, reverting to the default behavior.
     */
    public void clearEnableRepositoryHooks() {
        this.enableRepositoryHooks = null;
    }
}
