/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionPluginRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPluginRequestOptions {

    @JsonProperty("directories")
    private List<String> directories;

    /** Gets plugin directories. @return the plugin directories */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /** Sets plugin directories. @param directories the directories */
    public SessionPluginRequestOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }
}
