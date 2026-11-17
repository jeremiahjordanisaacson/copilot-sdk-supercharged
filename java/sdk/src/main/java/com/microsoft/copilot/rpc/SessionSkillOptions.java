/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.SkillProvider;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
/** Grouped SessionSkill configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionSkillOptions {

    private List<String> directories;
    private List<String> includedBuiltinSkills;
    private List<String> disabledSkills;
    private Boolean enabled;
    private SkillProvider provider;

    /**
     * Gets explicit discovery-only skill roots, not general filesystem grants.
     * Available without configuration discovery, subject to skill/managed disables.
     * Relative paths resolve once when supplied against the request's effective
     * working directory; retained roots do not move when it changes. Null and
     * omission are equivalent; an empty list clears only the SDK contribution.
     *
     * @return the list of skill directory paths
     */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets directories containing skill definitions.
     *
     * @param directories
     *            the list of skill directory paths
     * @return this config for method chaining
     */
    public SessionSkillOptions setDirectories(List<String> directories) {
        this.directories = directories;
        return this;
    }

    /**
     * Gets the runtime-bundled skill allowlist.
     *
     * @return the built-in skill names, or {@code null} when unspecified
     */
    public List<String> getIncludedBuiltinSkills() {
        return includedBuiltinSkills == null ? null : Collections.unmodifiableList(includedBuiltinSkills);
    }

    /**
     * Sets the runtime-bundled skill allowlist. In empty mode, omitting this option
     * excludes all built-in skills; specifying names opts those built-ins back in.
     *
     * @param includedBuiltinSkills
     *            the built-in skill names to allow
     * @return this config for method chaining
     */
    public SessionSkillOptions setIncludedBuiltinSkills(List<String> includedBuiltinSkills) {
        this.includedBuiltinSkills = includedBuiltinSkills;
        return this;
    }

    /**
     * Gets the disabled skills.
     *
     * @return the list of disabled skill names
     */
    public List<String> getDisabledSkills() {
        return disabledSkills == null ? null : Collections.unmodifiableList(disabledSkills);
    }

    /**
     * Sets skills that should be disabled for this session.
     *
     * @param disabledSkills
     *            the list of skill names to disable
     * @return this config for method chaining
     */
    public SessionSkillOptions setDisabledSkills(List<String> disabledSkills) {
        this.disabledSkills = disabledSkills;
        return this;
    }

    /**
     * Gets whether skill loading is enabled.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable skill
     *         loading or {@code false} to disable it, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getEnabled() {
        return Optional.ofNullable(enabled);
    }

    /**
     * Sets whether skill loading is enabled.
     *
     * @param enabled
     *            {@code true} to enable skill loading, {@code false} to disable it
     * @return this config for method chaining
     */
    public SessionSkillOptions setEnabled(boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /**
     * Gets the session-scoped skill provider for the resumed session.
     *
     * @return the skill provider, or {@code null} when not configured
     * @apiNote This API is experimental and may change in a future version.
     */
    @CopilotExperimental
    @JsonIgnore
    public SkillProvider getProvider() {
        return provider;
    }

    /**
     * Sets the session-scoped skill provider for the resumed session.
     * <p>
     * Skill provider registrations are not persisted; re-supply the provider when
     * resuming.
     *
     * @param provider
     *            the skill provider to expose to the runtime
     * @return this config for method chaining
     * @apiNote This API is experimental and may change in a future version.
     */
    @CopilotExperimental
    public SessionSkillOptions setProvider(SkillProvider provider) {
        this.provider = provider;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionSkillOptions clone() {
        SessionSkillOptions copy = new SessionSkillOptions();
        copy.enabled = this.enabled;
        copy.provider = this.provider;
        copy.directories = this.directories != null ? new ArrayList<>(this.directories) : null;
        copy.includedBuiltinSkills = this.includedBuiltinSkills != null
                ? new ArrayList<>(this.includedBuiltinSkills)
                : null;
        copy.disabledSkills = this.disabledSkills != null ? new ArrayList<>(this.disabledSkills) : null;
        return copy;
    }

    /**
     * Clears the enabled setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionSkillOptions clearEnabled() {
        this.enabled = null;
        return this;
    }
}
