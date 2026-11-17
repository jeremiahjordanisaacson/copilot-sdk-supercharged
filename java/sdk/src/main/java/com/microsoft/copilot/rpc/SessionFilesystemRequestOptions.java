/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
/** Grouped SessionFilesystemRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionFilesystemRequestOptions {

    @JsonProperty("trackFileChanges")
    private Boolean trackFileChanges;

    @JsonProperty("additionalDirectories")
    private List<String> additionalDirectories;

    @JsonProperty("trustWorkingDirectory")
    private String trustWorkingDirectory;

    /** Gets the file change tracking flag. @return the flag */
    public Boolean getTrackFileChanges() {
        return trackFileChanges;
    }

    /**
     * Sets the file change tracking flag.
     *
     * @param trackFileChanges
     *            the flag
     */
    public SessionFilesystemRequestOptions setTrackFileChanges(boolean trackFileChanges) {
        this.trackFileChanges = trackFileChanges;
        return this;
    }

    /** Gets additional directories. @return the additional directories */
    public List<String> getAdditionalDirectories() {
        return additionalDirectories;
    }

    /**
     * Sets additional directories.
     *
     * @param additionalDirectories
     *            the additional directories
     */
    public SessionFilesystemRequestOptions setAdditionalDirectories(List<String> additionalDirectories) {
        this.additionalDirectories = additionalDirectories;
        return this;
    }

    /**
     * Gets the working-directory trust scope. @return the scope, or {@code null}
     */
    public String getTrustWorkingDirectory() {
        return trustWorkingDirectory;
    }

    /**
     * Sets the working-directory trust scope. @param trustWorkingDirectory the
     * scope
     */
    public SessionFilesystemRequestOptions setTrustWorkingDirectory(String trustWorkingDirectory) {
        this.trustWorkingDirectory = trustWorkingDirectory;
        return this;
    }
}
