/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
/** Grouped SessionFilesystem configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionFilesystemOptions {

    private Boolean trackFileChanges;
    private List<String> additionalDirectories;

    /**
     * Gets whether file change tracking is enabled for rewind and cumulative
     * session diff.
     *
     * @return an {@link java.util.Optional} containing the setting, or
     *         {@link java.util.Optional#empty()} for the default
     */
    @JsonIgnore
    public Optional<Boolean> getTrackFileChanges() {
        return Optional.ofNullable(trackFileChanges);
    }

    /**
     * Enables or disables file change tracking when the resumed session has a valid
     * baseline. Earlier untracked changes cannot be reconstructed.
     *
     * @param trackFileChanges
     *            whether to enable file change tracking
     * @return this config instance for method chaining
     */
    public SessionFilesystemOptions setTrackFileChanges(boolean trackFileChanges) {
        this.trackFileChanges = trackFileChanges;
        return this;
    }

    /**
     * Gets the directories the agent may access beyond the working directory.
     *
     * @return the additional directory paths
     */
    public List<String> getAdditionalDirectories() {
        return additionalDirectories;
    }

    /**
     * Sets directories the agent may access beyond the working directory.
     *
     * @param additionalDirectories
     *            the additional directory paths
     * @return this config for method chaining
     */
    public SessionFilesystemOptions setAdditionalDirectories(List<String> additionalDirectories) {
        this.additionalDirectories = additionalDirectories;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionFilesystemOptions clone() {
        SessionFilesystemOptions copy = new SessionFilesystemOptions();
        copy.trackFileChanges = this.trackFileChanges;
        copy.additionalDirectories = this.additionalDirectories != null
                ? new ArrayList<>(this.additionalDirectories)
                : null;
        return copy;
    }

    /**
     * Clears the file change tracking setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionFilesystemOptions clearTrackFileChanges() {
        this.trackFileChanges = null;
        return this;
    }
}
