/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

/**
 * Override action for a system prompt section.
 */
public final class SectionOverrideAction {
    public static final String REPLACE = "replace";
    public static final String REMOVE = "remove";
    public static final String APPEND = "append";
    public static final String PREPEND = "prepend";

    private SectionOverrideAction() {}
}
