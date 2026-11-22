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
 * Current state of a long-lived service.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIServiceTask(
    /** Task kind, always service. */
    @JsonProperty("type") String type,
    /** Registry task identity. */
    @JsonProperty("id") String id,
    /** Service kind, such as LSP. */
    @JsonProperty("serviceKind") String serviceKind,
    /** Identity of the service instance. */
    @JsonProperty("serviceId") String serviceId,
    /** Display description. */
    @JsonProperty("description") String description,
    /** Current lifecycle status. */
    @JsonProperty("status") String status,
    /** Whether the service can accept work. */
    @JsonProperty("ready") Boolean ready,
    /** Current initialization phase. */
    @JsonProperty("phase") String phase,
    /** Current completion percentage. */
    @JsonProperty("percentage") Double percentage,
    /** Start time in milliseconds since the Unix epoch. */
    @JsonProperty("startedAt") Double startedAt,
    /** Completion time in milliseconds since the Unix epoch. */
    @JsonProperty("completedAt") Double completedAt,
    /** Failure text, when present. */
    @JsonProperty("error") String error,
    /** Recent display messages. */
    @JsonProperty("log") List<UIServiceLogEntry> log
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIServiceTask {}

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
        private String type;
        private String id;
        private String serviceKind;
        private String serviceId;
        private String description;
        private String status;
        private Boolean ready;
        private String phase;
        private Double percentage;
        private Double startedAt;
        private Double completedAt;
        private String error;
        private List<UIServiceLogEntry> log;

        /**
         * Sets the {@code type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder type(String value) {
            this.type = value;
            return this;
        }

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code serviceKind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder serviceKind(String value) {
            this.serviceKind = value;
            return this;
        }

        /**
         * Sets the {@code serviceId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder serviceId(String value) {
            this.serviceId = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code ready} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ready(Boolean value) {
            this.ready = value;
            return this;
        }

        /**
         * Sets the {@code phase} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder phase(String value) {
            this.phase = value;
            return this;
        }

        /**
         * Sets the {@code percentage} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder percentage(Double value) {
            this.percentage = value;
            return this;
        }

        /**
         * Sets the {@code startedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder startedAt(Double value) {
            this.startedAt = value;
            return this;
        }

        /**
         * Sets the {@code completedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder completedAt(Double value) {
            this.completedAt = value;
            return this;
        }

        /**
         * Sets the {@code error} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder error(String value) {
            this.error = value;
            return this;
        }

        /**
         * Sets the {@code log} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder log(List<UIServiceLogEntry> value) {
            this.log = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIServiceTask build() {
            if (type == null) throw new IllegalStateException("Missing required property: type");
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (serviceKind == null) throw new IllegalStateException("Missing required property: serviceKind");
            if (serviceId == null) throw new IllegalStateException("Missing required property: serviceId");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (ready == null) throw new IllegalStateException("Missing required property: ready");
            if (startedAt == null) throw new IllegalStateException("Missing required property: startedAt");
            if (log == null) throw new IllegalStateException("Missing required property: log");
            return new UIServiceTask(type, id, serviceKind, serviceId, description, status, ready, phase, percentage, startedAt, completedAt, error, log);
        }
    }
}
