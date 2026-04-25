/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

/**
 * Override operation for a single system prompt section.
 */
public class SectionOverride {
    private String action;
    private String content;

    public SectionOverride() {}

    public SectionOverride(String action) { this.action = action; }

    public SectionOverride(String action, String content) {
        this.action = action;
        this.content = content;
    }

    public String getAction() { return action; }
    public SectionOverride action(String action) { this.action = action; return this; }

    public String getContent() { return content; }
    public SectionOverride content(String content) { this.content = content; return this; }
}
