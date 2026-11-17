/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Project directory access and change tracking.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenFilesystemOptions(
    /** Opt in to capturing file changes for session rewind and session diff. Capture cannot reconstruct changes made before it was enabled. On create it starts capture from the first turn. It is also honored on resume: for a session that already has tracked prior turns, tracking continues automatically even if this is omitted; passing it on resume additionally enables tracking for an eligible session that has no prior root turn yet. Resuming a session whose prior root turns were never tracked has no restorable baseline, so tracking stays disabled for it and rewind reports file change tracking as unavailable; the resume itself still succeeds, so sessions that predate tracking remain loadable. The opt-in is only rejected when the session can never track (a subagent session, or one without local session storage). It is intentionally absent from the mutable options update because enabling it after edits have occurred would create an incomplete, misleading baseline. Subagents share the parent session's capture store and are not tracked as separate rewind points: a file a subagent writes is attributed to whichever root user turn was open when the capture was staged, just before the tool body ran. A turn cannot open while a staged capture is still in flight, so a subagent tool that staged under the spawning turn stays attributed to it however late the write lands, while a capture it stages after the user's next message belongs to that later turn. Attribution decides which turn's rewind point counts and file preview include that write; it does not narrow which rewinds revert it, because a rewind restores every capture from the selected turn onward, so the earlier spawning turn reverts it as well. */
    @JsonProperty("trackFileChanges") Boolean trackFileChanges,
    /** Additional directories the agent may access beyond the working directory. Each entry is granted to the session's file-access allow-list and surfaced to the model (system prompt context and `@`-mention completion). Conventional `.github/skills/` and `.github/agents/` definitions under each directory also join the session's project catalogs when their existing subsystem gates are enabled: added-root skills require both `enableConfigDiscovery` and effective `enableSkills`; added-root agents require `enableConfigDiscovery`. Supplying a directory therefore activates configuration from it and should be treated as a trust decision. Absolute paths are recommended; a relative path is resolved against the session's working directory. Nonexistent or unresolvable entries are skipped with a warning. This is applied during session creation and cold resume and is not persisted, so a cold resume must re-supply the directories. */
    @JsonProperty("additionalDirectories") List<String> additionalDirectories,
    /** Pre-resolved working-directory context for session startup. */
    @JsonProperty("workingDirectoryContext") SessionContext workingDirectoryContext
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenFilesystemOptions {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    public static final class Builder {
        private Builder() {}
        private Boolean trackFileChanges;
        private List<String> additionalDirectories;
        private SessionContext workingDirectoryContext;

        /**
         * Sets the {@code trackFileChanges} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trackFileChanges(Boolean value) {
            this.trackFileChanges = value;
            return this;
        }

        /**
         * Sets the {@code additionalDirectories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalDirectories(List<String> value) {
            this.additionalDirectories = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectoryContext} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectoryContext(SessionContext value) {
            this.workingDirectoryContext = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenFilesystemOptions build() {
            return new SessionOpenFilesystemOptions(trackFileChanges, additionalDirectories, workingDirectoryContext);
        }
    }
}
