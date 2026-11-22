/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * An MCP form elicitation prompt with its default mode made explicit.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class UIElicitationPromptForm extends UIElicitationPrompt {

    @JsonProperty("mode")
    private final String mode = "form";

    @Override
    public String getMode() { return mode; }

    /** Message that explains the requested information. */
    @JsonProperty("message")
    private String message;

    /** Schema of the form fields. */
    @JsonProperty("requestedSchema")
    private UIElicitationSnapshotSchema requestedSchema;

    /** MCP request metadata. */
    @JsonProperty("_meta")
    private Map<String, Object> meta;

    /** MCP task metadata. */
    @JsonProperty("task")
    private McpTaskMetadata task;

    public String getMessage() { return message; }
    public void setMessage(String message) { this.message = message; }

    public UIElicitationSnapshotSchema getRequestedSchema() { return requestedSchema; }
    public void setRequestedSchema(UIElicitationSnapshotSchema requestedSchema) { this.requestedSchema = requestedSchema; }

    public Map<String, Object> getMeta() { return meta; }
    public void setMeta(Map<String, Object> meta) { this.meta = meta; }

    public McpTaskMetadata getTask() { return task; }
    public void setTask(McpTaskMetadata task) { this.task = task; }

    private final Map<String, JsonNode> additionalProperties = new LinkedHashMap<>();

    @JsonAnySetter
    public void setAdditionalProperty(String name, JsonNode value) {
        additionalProperties.put(name, value);
    }

    @JsonAnyGetter
    public Map<String, JsonNode> getAdditionalProperties() { return additionalProperties; }


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
        private String message;
        private UIElicitationSnapshotSchema requestedSchema;
        private Map<String, Object> meta;
        private McpTaskMetadata task;

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Sets the {@code requestedSchema} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestedSchema(UIElicitationSnapshotSchema value) {
            this.requestedSchema = value;
            return this;
        }

        /**
         * Sets the {@code _meta} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder meta(Map<String, Object> value) {
            this.meta = value;
            return this;
        }

        /**
         * Sets the {@code task} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder task(McpTaskMetadata value) {
            this.task = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIElicitationPromptForm build() {
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (requestedSchema == null) throw new IllegalStateException("Missing required property: requestedSchema");
            UIElicitationPromptForm instance = new UIElicitationPromptForm();
            instance.setMessage(message);
            instance.setRequestedSchema(requestedSchema);
            instance.setMeta(meta);
            instance.setTask(task);
            return instance;
        }
    }
}
