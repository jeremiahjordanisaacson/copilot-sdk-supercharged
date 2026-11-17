/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
/** Grouped SessionPlugin configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPluginOptions {

    private List<String> directories;

    /**
     * Gets the plugin directories to load Open Plugin definitions from.
     *
     * @return the list of plugin directory paths
     */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets the plugin directories to load Open Plugin definitions from.
     *
     * @param directories
     *            the list of plugin directory paths
     * @return this config for method chaining
     */
    public SessionPluginOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionPluginOptions clone() {
        SessionPluginOptions copy = new SessionPluginOptions();
        copy.directories = this.directories != null ? new ArrayList<>(this.directories) : null;
        return copy;
    }

}
