/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.Objects;
/** Grouped ClientObservability configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientObservabilityOptions {

    private String logLevel;
    private TelemetryConfig telemetry;

    /**
     * Gets the log level for the CLI process.
     *
     * @return the configured log level, or {@code null} for the runtime default
     */
    public String getLogLevel() {
        return logLevel;
    }

    /**
     * Sets the log level for the CLI process.
     * <p>
     * Valid levels include: "none", "error", "warning", "info", "debug", "all".
     *
     * @param logLevel
     *            the log level, or {@code null} for the runtime default
     * @return this options instance for method chaining
     */
    public ClientObservabilityOptions setLogLevel(String logLevel) {
        this.logLevel = logLevel;
        return this;
    }

    /**
     * Gets the OpenTelemetry configuration for the CLI server.
     *
     * @return the telemetry config, or {@code null}
     * @since 1.0.0
     */
    public TelemetryConfig getTelemetry() {
        return telemetry;
    }

    /**
     * Sets the OpenTelemetry configuration for the CLI server.
     * <p>
     * When set, the CLI server is started with OpenTelemetry instrumentation
     * enabled using the provided settings.
     *
     * @param telemetry
     *            the telemetry configuration
     * @return this options instance for method chaining
     * @since 1.0.0
     */
    public ClientObservabilityOptions setTelemetry(TelemetryConfig telemetry) {
        this.telemetry = Objects.requireNonNull(telemetry, "telemetry must not be null");
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientObservabilityOptions clone() {
        ClientObservabilityOptions copy = new ClientObservabilityOptions();
        copy.logLevel = this.logLevel;
        copy.telemetry = this.telemetry;
        return copy;
    }

}
