/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;
import java.util.concurrent.Executor;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.EntraAuthConfig;

/**
 * Configuration options for creating a
 * {@link com.microsoft.copilot.CopilotClient}.
 * <p>
 * This class provides a fluent API for configuring how the client connects to
 * and manages the Copilot CLI server. All setter methods return {@code this}
 * for method chaining.
 *
 * <h2>Example Usage</h2>
 *
 * <pre>{@code
 * var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
 * 		.setRuntime(RuntimeConnection.forSpawnedStdio("/usr/local/bin/copilot"))
 * 		.setObservability(new ClientObservabilityOptions().setLogLevel("debug")).setAutoStart(true);
 *
 * var client = new CopilotClient(options);
 * }</pre>
 *
 * @see com.microsoft.copilot.CopilotClient
 * @since 1.0.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CopilotClientOptions {
    private InternalTestOptions internalTestOptions;

    /**
     * @return unsupported SDK test infrastructure configuration
     * @hidden
     */
    @CopilotExperimental
    public InternalTestOptions getInternalTestOptions() {
        return internalTestOptions;
    }

    /**
     * @param value
     *            test configuration for runtimes owned by this client
     * @return this options instance
     * @hidden
     */
    @CopilotExperimental
    public CopilotClientOptions setInternalTestOptions(InternalTestOptions value) {
        internalTestOptions = value;
        return this;
    }

    private boolean autoStart = true;
    private RuntimeConnection runtime;
    private ClientInfo clientInfo;
    private EntraAuthOptions entra;
    private Executor executor;
    private CopilotClientMode mode;
    private NetworkConfig network;
    private boolean remote;

    /**
     * Returns whether the client should automatically start the server.
     *
     * @return {@code true} to auto-start (default), {@code false} for manual start
     */
    public boolean isAutoStart() {
        return autoStart;
    }

    /**
     * Sets whether the client should automatically start the CLI server when the
     * first request is made.
     *
     * @param autoStart
     *            {@code true} to auto-start, {@code false} for manual start
     * @return this options instance for method chaining
     */
    public CopilotClientOptions setAutoStart(boolean autoStart) {
        this.autoStart = autoStart;
        return this;
    }

    /**
     * Gets the network callbacks through which the SDK host takes over parts of the
     * runtime's networking.
     *
     * @return the configuration, or {@code null} if not set
     */
    @JsonIgnore
    @CopilotExperimental
    public NetworkConfig getNetwork() {
        return network;
    }

    /**
     * Sets the network callbacks through which the SDK host decides the runtime's
     * proxies and supplies its Kerberos tokens and proxy credentials.
     * <p>
     * When the configuration has at least one callback, the client handles the
     * runtime's {@code network.resolveProxy}, {@code network.authenticateNegotiate}
     * and {@code network.authenticateBasic} requests for the configured callbacks
     * and registers them during startup, before any session can be created. Startup
     * fails if registration fails. The callbacks and scope are read when the client
     * starts.
     *
     * @param network
     *            the configuration (must not be {@code null})
     * @return this options instance for method chaining
     * @see NetworkConfig
     */
    @CopilotExperimental
    public CopilotClientOptions setNetwork(NetworkConfig network) {
        this.network = Objects.requireNonNull(network, "network must not be null");
        return this;
    }

    /**
     * Gets the runtime that selects how the client reaches the Copilot runtime.
     *
     * @return the runtime, or {@code null} to resolve
     *         {@link RuntimeConnection#forDefault()} when constructing the client
     */
    @JsonIgnore
    public RuntimeConnection getRuntime() {
        return runtime;
    }

    /**
     * Sets the runtime that selects how the client reaches the Copilot runtime.
     * <p>
     * When set, the runtime takes precedence over the ambient
     * {@code COPILOT_SDK_DEFAULT_CONNECTION} override. Executable path, arguments,
     * environment and process working directory are configured only on
     * {@link SpawnedStdioRuntimeConnection} or {@link SpawnedTcpRuntimeConnection}.
     *
     * @param runtime
     *            the runtime, or {@code null} to use
     *            {@link RuntimeConnection#forDefault()}
     * @return this options instance for method chaining
     */
    public CopilotClientOptions setRuntime(RuntimeConnection runtime) {
        this.runtime = runtime;
        return this;
    }

    /**
     * Gets the executor used for internal asynchronous operations.
     * <p>
     * Returns {@code null} if no executor has been explicitly set, indicating that
     * the SDK should use its default executor strategy.
     *
     * @return the executor, or {@code null} if using SDK defaults
     */
    public Executor getExecutor() {
        return executor;
    }

    /**
     * Sets the executor used for internal asynchronous operations.
     * <p>
     * When provided, the SDK uses this executor for all internal
     * {@code CompletableFuture} combinators. This allows callers to isolate SDK
     * work onto a dedicated thread pool or integrate with container-managed
     * threading.
     * <p>
     * The SDK will not shut down a user-provided executor. If you pass a custom
     * {@code ExecutorService}, you remain responsible for shutting it down.
     * <p>
     * If not set (or set to {@code null}), the SDK uses its default executor:
     * virtual threads on JDK 25+, {@code ForkJoinPool.commonPool()} on older JDKs.
     *
     * @param executor
     *            the executor to use, or {@code null} for SDK defaults
     * @return this options instance for fluent chaining
     */
    public CopilotClientOptions setExecutor(Executor executor) {
        this.executor = executor;
        return this;
    }

    /**
     * Gets the SDK defaulting strategy.
     *
     * @return the client mode, or {@code null} if not yet selected
     * @since 1.3.0
     */
    public CopilotClientMode getMode() {
        return mode;
    }

    /**
     * Sets the required SDK defaulting strategy.
     * <p>
     * When set to {@link CopilotClientMode#EMPTY}, the SDK validates that the app
     * has supplied the required configuration (e.g.
     * {@link OwnedRuntimeConnection#setBaseDirectory(String)}).
     *
     * @param mode
     *            the client mode
     * @return this options instance for method chaining
     * @since 1.3.0
     */
    public CopilotClientOptions setMode(CopilotClientMode mode) {
        this.mode = Objects.requireNonNull(mode, "mode must not be null");
        return this;
    }

    /**
     * Returns whether remote session support (Mission Control integration) is
     * enabled.
     * <p>
     * When {@code true}, sessions in a GitHub repository working directory are
     * accessible from GitHub web and mobile.
     *
     * @return {@code true} if remote sessions are enabled
     */
    public boolean isRemote() {
        return remote;
    }

    /**
     * Enables remote session support (Mission Control integration).
     * <p>
     * When {@code true}, sessions in a GitHub repository working directory are
     * accessible from GitHub web and mobile.
     * <p>
     * Enabling this option requires a runtime owned by this client; it is rejected
     * with {@link RuntimeConnection#forTcp(String, int)}.
     *
     * @param remote
     *            {@code true} to enable remote sessions
     * @return this options instance for method chaining
     */
    public CopilotClientOptions setRemote(boolean remote) {
        this.remote = remote;
        return this;
    }

    /**
     * Gets the integrating application's declared identity.
     *
     * @return the client info, or {@code null}
     * @since 1.6.0
     */
    public ClientInfo getClientInfo() {
        return clientInfo;
    }

    /**
     * Gets the host's Entra registration, or null when built-in Entra sign-in and
     * token renewal are disabled.
     *
     * @return the Entra configuration
     */
    @CopilotExperimental
    public EntraAuthOptions getEntra() {
        return entra;
    }

    /**
     * Sets the host application's Entra registration for its sessions.
     *
     * @param entra
     *            the Entra configuration
     * @return this options instance
     */
    @CopilotExperimental
    public CopilotClientOptions setEntra(EntraAuthConfig entra) {
        Objects.requireNonNull(entra, "entra must not be null");
        this.entra = new EntraAuthOptions().setClientId(entra.clientId()).setMatsAppName(entra.matsAppName())
                .setOneAuthApplicationId(entra.oneAuthApplicationId());
        return this;
    }

    /**
     * Sets the Entra application and optional host-owned acquisition callback.
     *
     * @param entra
     *            the host's Entra options
     * @return this options instance
     */
    @CopilotExperimental
    public CopilotClientOptions setEntra(EntraAuthOptions entra) {
        this.entra = Objects.requireNonNull(entra, "entra must not be null");
        return this;
    }

    /**
     * Declares the integrating application's identity, forwarded to the runtime on
     * the {@code server.connect} handshake.
     * <p>
     * Declaring it lets the telemetry the runtime emits on this connection be
     * attributed to a consistent surface (the application and its Copilot
     * integration) instead of the runtime's own build. All fields on
     * {@link ClientInfo} are optional; leave this unset to keep the runtime's
     * default attribution.
     *
     * @param clientInfo
     *            the application identity to declare
     * @return this options instance for method chaining
     * @since 1.6.0
     */
    public CopilotClientOptions setClientInfo(ClientInfo clientInfo) {
        this.clientInfo = Objects.requireNonNull(clientInfo, "clientInfo must not be null");
        return this;
    }

    @JsonProperty("plugins")
    private ClientPluginOptions plugins = new ClientPluginOptions();
    /**
     * Returns the plugins options.
     *
     * @return the options
     */
    public ClientPluginOptions getPlugins() {
        return plugins;
    }
    /**
     * Sets the plugins options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setPlugins(ClientPluginOptions value) {
        plugins = value == null ? new ClientPluginOptions() : value;
        return this;
    }

    @JsonProperty("github")
    private ClientGitHubOptions github = new ClientGitHubOptions();
    /**
     * Returns the github options.
     *
     * @return the options
     */
    @JsonProperty("github")
    public ClientGitHubOptions getGitHub() {
        return github;
    }
    /**
     * Sets the github options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setGitHub(ClientGitHubOptions value) {
        github = value == null ? new ClientGitHubOptions() : value;
        return this;
    }

    @JsonProperty("observability")
    private ClientObservabilityOptions observability = new ClientObservabilityOptions();
    /**
     * Returns the observability options.
     *
     * @return the options
     */
    public ClientObservabilityOptions getObservability() {
        return observability;
    }
    /**
     * Sets the observability options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setObservability(ClientObservabilityOptions value) {
        observability = value == null ? new ClientObservabilityOptions() : value;
        return this;
    }

    @JsonProperty("extensions")
    private ClientExtensionOptions extensions = new ClientExtensionOptions();
    /**
     * Returns the extensions options.
     *
     * @return the options
     */
    public ClientExtensionOptions getExtensions() {
        return extensions;
    }
    /**
     * Sets the extensions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setExtensions(ClientExtensionOptions value) {
        extensions = value == null ? new ClientExtensionOptions() : value;
        return this;
    }

    @JsonProperty("installations")
    private ClientInstallationOptions installations = new ClientInstallationOptions();
    /**
     * Returns the installations options.
     *
     * @return the options
     */
    public ClientInstallationOptions getInstallations() {
        return installations;
    }
    /**
     * Sets the installations options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setInstallations(ClientInstallationOptions value) {
        installations = value == null ? new ClientInstallationOptions() : value;
        return this;
    }

    @JsonProperty("models")
    private ClientModelOptions models = new ClientModelOptions();
    /**
     * Returns the models options.
     *
     * @return the options
     */
    public ClientModelOptions getModels() {
        return models;
    }
    /**
     * Sets the models options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CopilotClientOptions setModels(ClientModelOptions value) {
        models = value == null ? new ClientModelOptions() : value;
        return this;
    }

    /**
     * Copies options and their mutable group collections.
     *
     * @return the independent options copy
     */
    @Override
    public CopilotClientOptions clone() {
        CopilotClientOptions copy = new CopilotClientOptions();
        copy.autoStart = this.autoStart;
        copy.runtime = this.runtime == null ? null : this.runtime.clone();
        copy.clientInfo = this.clientInfo;
        copy.entra = this.entra;
        copy.internalTestOptions = this.internalTestOptions;
        copy.executor = this.executor;
        copy.network = this.network;
        copy.mode = this.mode;
        copy.remote = this.remote;
        copy.plugins = this.plugins.clone();
        copy.github = this.github.clone();
        copy.observability = this.observability.clone();
        copy.extensions = this.extensions.clone();
        copy.installations = this.installations.clone();
        copy.models = this.models.clone();
        return copy;
    }

}
