/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import java.util.Map;

/**
 * System message configuration for session creation.
 *
 * Supports three modes:
 * - "append" (default): SDK foundation + optional custom content
 * - "replace": Full control, caller provides entire system message
 * - "customize": Section-level overrides with graceful fallback
 */
public class SystemMessageConfig {
    private String mode;
    private String content;
    private Map<String, SectionOverride> sections;

    public SystemMessageConfig() {}

    // Builder-style setters
    public SystemMessageConfig mode(String mode) { this.mode = mode; return this; }
    public SystemMessageConfig content(String content) { this.content = content; return this; }
    public SystemMessageConfig sections(Map<String, SectionOverride> sections) { this.sections = sections; return this; }

    // Getters
    public String getMode() { return mode; }
    public String getContent() { return content; }
    public Map<String, SectionOverride> getSections() { return sections; }

    // Factory methods
    public static SystemMessageConfig append() { return new SystemMessageConfig().mode("append"); }
    public static SystemMessageConfig append(String content) { return new SystemMessageConfig().mode("append").content(content); }
    public static SystemMessageConfig replace(String content) { return new SystemMessageConfig().mode("replace").content(content); }
    public static SystemMessageConfig customize(Map<String, SectionOverride> sections) {
        return new SystemMessageConfig().mode("customize").sections(sections);
    }
    public static SystemMessageConfig customize(Map<String, SectionOverride> sections, String content) {
        return new SystemMessageConfig().mode("customize").sections(sections).content(content);
    }
}
