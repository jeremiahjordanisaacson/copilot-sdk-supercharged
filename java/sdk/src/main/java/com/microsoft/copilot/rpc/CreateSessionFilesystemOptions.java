/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
/** Grouped CreateSessionFilesystem configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CreateSessionFilesystemOptions {

    private Boolean trackFileChanges;
    private List<String> additionalDirectories;
    private WorkingDirectoryTrust trustWorkingDirectory;

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
     * Enables or disables file change tracking from the first turn.
     *
     * @param trackFileChanges
     *            whether to enable file change tracking
     * @return this config instance for method chaining
     */
    public CreateSessionFilesystemOptions setTrackFileChanges(boolean trackFileChanges) {
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
     * @return this config instance for method chaining
     */
    public CreateSessionFilesystemOptions setAdditionalDirectories(List<String> additionalDirectories) {
        this.additionalDirectories = additionalDirectories;
        return this;
    }

    /**
     * Gets the working-directory trust scope.
     *
     * @return {@code "session"}, {@code "user"}, or {@code null} to leave trust
     *         unchanged
     */
    public String getTrustWorkingDirectory() {
        return trustWorkingDirectory != null ? trustWorkingDirectory.getValue() : null;
    }

    /**
     * Trusts the working directory for this session or persistently for the user.
     *
     * @param trustWorkingDirectory
     *            {@code "session"}, {@code "user"}, or {@code null} to leave trust
     *            unchanged
     * @return this config for method chaining
     */
    public CreateSessionFilesystemOptions setTrustWorkingDirectory(String trustWorkingDirectory) {
        this.trustWorkingDirectory = trustWorkingDirectory != null
                ? WorkingDirectoryTrust.fromValue(trustWorkingDirectory)
                : null;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public CreateSessionFilesystemOptions clone() {
        CreateSessionFilesystemOptions copy = new CreateSessionFilesystemOptions();
        copy.trackFileChanges = this.trackFileChanges;
        copy.additionalDirectories = this.additionalDirectories != null
                ? new ArrayList<>(this.additionalDirectories)
                : null;
        copy.trustWorkingDirectory = this.trustWorkingDirectory;
        return copy;
    }

    /**
     * Clears the file change tracking setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public CreateSessionFilesystemOptions clearTrackFileChanges() {
        this.trackFileChanges = null;
        return this;
    }

    /**
     * Gets the typed working-directory trust scope.
     *
     * @return the trust scope, or null if not specified
     */
    @JsonIgnore
    public WorkingDirectoryTrust getWorkingDirectoryTrust() {
        return trustWorkingDirectory;
    }

    /**
     * Sets the typed working-directory trust scope.
     *
     * @param trustWorkingDirectory
     *            the trust scope, or null to clear it
     * @return this config for method chaining
     */
    public CreateSessionFilesystemOptions setWorkingDirectoryTrust(WorkingDirectoryTrust trustWorkingDirectory) {
        this.trustWorkingDirectory = trustWorkingDirectory;
        return this;
    }
}
