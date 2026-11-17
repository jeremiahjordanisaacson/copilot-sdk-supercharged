/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionSkillRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionSkillRequestOptions {

    @JsonProperty("directories")
    private List<String> directories;

    @JsonProperty("disabledSkills")
    private List<String> disabledSkills;

    @JsonProperty("enabled")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enabled;

    @JsonProperty("hasProvider")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean hasProvider;

    /** Gets skill directories. @return the directories */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /** Sets skill directories. @param directories the directories */
    public SessionSkillRequestOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }

    /** Gets disabled skills. @return the disabled skill names */
    public List<String> getDisabledSkills() {
        return disabledSkills == null ? null : Collections.unmodifiableList(disabledSkills);
    }

    /** Sets disabled skills. @param disabledSkills the skill names to disable */
    public SessionSkillRequestOptions setDisabledSkills(List<String> disabledSkills) {
        this.disabledSkills = disabledSkills;
        return this;
    }

    /** Gets enable skills flag. @return the flag */
    public Boolean getEnabled() {
        return enabled;
    }

    /** Sets enable skills flag. @param enabled the flag */
    public SessionSkillRequestOptions setEnabled(boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /** Gets skill-provider availability flag. @return the flag */
    public Boolean getHasProvider() {
        return hasProvider;
    }

    /** Sets skill-provider availability flag. @param hasProvider the flag */
    public SessionSkillRequestOptions setHasProvider(boolean hasProvider) {
        this.hasProvider = hasProvider;
        return this;
    }

    /** Clears the enabled setting, reverting to the default behavior. */
    public void clearEnabled() {
        this.enabled = null;
    }

    /**
     * Clears the hasProvider setting, reverting to the default behavior.
     */
    public void clearHasProvider() {
        this.hasProvider = null;
    }
}
