/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Options scoped to the built-in CAPI (Copilot API) provider.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record CapiSessionOptions(
    /** Enables the session's built-in Copilot model catalog, authentication and
inference, including subagent and ancillary model work. Defaults to true
on create; omission on resume preserves the policy. Does not disable
GitHub identity, tools, MCP, managed settings or separate telemetry. */
    @JsonProperty("enabled") Boolean enabled,
    /** Routing preference for sessions whose model is `auto`. On create or cold resume, this establishes the preference sent as `tier` on CAPI `/auto` requests; when omitted on cold resume, the runtime restores the last committed preference. On resident resume, a different value requests a safe switch after resume succeeds and cannot change an in-flight turn. Successful switches are persisted for later cold resume. When no preference is supplied or restored, CAPI default routing is used. `fast` is an integrator-only latency preset, not a first-party GitHub Copilot product preference. For Copilot Fusion, prefer `fusionTier`: the `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces remain accepted but are deprecated. */
    @JsonProperty("autoTier") AutoTier autoTier,
    /** Copilot Fusion tier for sessions whose model is `fusion` or `auto`, matching the CLI's `--fusion-tier`. The runtime resolves the tier to the routing preference it stands for and then applies it exactly like `autoTier`, so `session.model.getCurrent` and `session.model_change` report that preference as `autoTier`; `session.model.getCurrent` also reports the resolved tier as `fusionTier` once the session's Auto tier metadata is known. Cannot be combined with `autoTier`. */
    @JsonProperty("fusionTier") FusionTier fusionTier,
    /** Whether to use WebSocket transport for the CAPI Responses API. Enabled by default when the model advertises `ws:/responses` support; set to `false` to force the HTTP Responses transport in environments where WebSockets are blocked (e.g. behind a proxy). Setting this to `false` is equivalent to the `COPILOT_CLI_DISABLE_WEBSOCKET_RESPONSES` environment variable. */
    @JsonProperty("enableWebSocketResponses") Boolean enableWebSocketResponses
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public CapiSessionOptions {}

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
        private Boolean enabled;
        private AutoTier autoTier;
        private FusionTier fusionTier;
        private Boolean enableWebSocketResponses;

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code autoTier} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder autoTier(AutoTier value) {
            this.autoTier = value;
            return this;
        }

        /**
         * Sets the {@code fusionTier} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fusionTier(FusionTier value) {
            this.fusionTier = value;
            return this;
        }

        /**
         * Sets the {@code enableWebSocketResponses} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableWebSocketResponses(Boolean value) {
            this.enableWebSocketResponses = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public CapiSessionOptions build() {
            return new CapiSessionOptions(enabled, autoTier, fusionTier, enableWebSocketResponses);
        }
    }
}
