/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * A bounded catalog search. Both the query length and the result count are capped by the schema so a caller cannot request an unbounded scan.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record CatalogSearchParams(
    /** Protocol version and capabilities the caller requires. */
    @JsonProperty("contract") CatalogClientContract contract,
    /** Select an existing attached local session. Requires authenticated, session-bound search.
The runtime never creates, resumes or reconfigures a session to honour this selector. */
    @JsonProperty("policySessionId") String policySessionId,
    /** Free-text search query. Persisted as tool input for session continuity, but omitted from telemetry. */
    @JsonProperty("query") String query,
    /** Maximum number of candidates to return. Defaults to 10 when omitted. */
    @JsonProperty("limit") Long limit,
    /** Restrict results to these candidate kinds. Agent Plugins are opt-in and require the `agent-plugin-discovery` capability so protocol-v3 clients generated before that variant cannot receive an unknown result; when omitted, the backwards-compatible MCP server and AI skill kinds are searched. */
    @JsonProperty("kinds") List<CatalogCandidateKind> kinds,
    /** Restrict results to resources in at least one of these server-defined workflow categories, such as `design` or `test`. Requires the `catalog-search-categories` capability. Each value is a lowercase slug; the known set is not exhaustive, and a category the server does not recognise matches nothing. Omit to search every category. */
    @JsonProperty("categories") List<String> categories,
    /** Numbered navigation using metadata from an earlier response. Requires catalog-search-pagination and the same query, kinds, categories and effective limit. Omit for a fresh first-page search. */
    @JsonProperty("page") CatalogSearchPage page
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public CatalogSearchParams {}

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
        private CatalogClientContract contract;
        private String policySessionId;
        private String query;
        private Long limit;
        private List<CatalogCandidateKind> kinds;
        private List<String> categories;
        private CatalogSearchPage page;

        /**
         * Sets the {@code contract} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder contract(CatalogClientContract value) {
            this.contract = value;
            return this;
        }

        /**
         * Sets the {@code policySessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder policySessionId(String value) {
            this.policySessionId = value;
            return this;
        }

        /**
         * Sets the {@code query} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder query(String value) {
            this.query = value;
            return this;
        }

        /**
         * Sets the {@code limit} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder limit(Long value) {
            this.limit = value;
            return this;
        }

        /**
         * Sets the {@code kinds} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kinds(List<CatalogCandidateKind> value) {
            this.kinds = value;
            return this;
        }

        /**
         * Sets the {@code categories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder categories(List<String> value) {
            this.categories = value;
            return this;
        }

        /**
         * Sets the {@code page} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder page(CatalogSearchPage value) {
            this.page = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public CatalogSearchParams build() {
            if (contract == null) throw new IllegalStateException("Missing required property: contract");
            if (query == null) throw new IllegalStateException("Missing required property: query");
            return new CatalogSearchParams(contract, policySessionId, query, limit, kinds, categories, page);
        }
    }
}
