/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
/** Grouped ClientPlugin configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientPluginOptions {

    private List<Path> builtinDirectories;

    /**
     * Gets the trusted plugin directories bundled by the host.
     *
     * @return a copy of the configured absolute paths, or {@code null}
     */
    public List<Path> getBuiltinDirectories() {
        return builtinDirectories != null ? new ArrayList<>(builtinDirectories) : null;
    }

    /**
     * Sets trusted plugin directories bundled by the host. Every path must be
     * absolute. When non-empty, the complete set is registered during startup
     * before sessions can be created.
     *
     * @param paths
     *            absolute plugin directory paths, or {@code null}/empty to disable
     * @return this options instance for method chaining
     */
    public ClientPluginOptions setBuiltinDirectories(List<Path> paths) {
        if (paths == null || paths.isEmpty()) {
            this.builtinDirectories = null;
            return this;
        }
        for (Path path : paths) {
            Objects.requireNonNull(path, "builtin plugin directory path must not be null");
            if (!path.isAbsolute()) {
                throw new IllegalArgumentException("BuiltinDirectories must contain only absolute paths: " + path);
            }
        }
        this.builtinDirectories = new ArrayList<>(paths);
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientPluginOptions clone() {
        ClientPluginOptions copy = new ClientPluginOptions();
        copy.builtinDirectories = this.builtinDirectories != null ? new ArrayList<>(this.builtinDirectories) : null;
        return copy;
    }

}
