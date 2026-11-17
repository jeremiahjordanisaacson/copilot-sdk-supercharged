// Copyright (c) Microsoft Corporation. All rights reserved.

//! Runtime ownership and grouped client startup options.
use super::*;

/// Settings for a client. Omitted runtime selection is resolved when starting the client.
pub struct ClientOptions {
    /// Runtime selection. Use `RuntimeConnection::for_default` to capture a deployment default now.
    pub runtime: Option<RuntimeConnection>,
    /// Unsupported SDK test infrastructure configuration for an owned runtime.
    /// Supplied values override startup configuration without changing the host environment.
    #[doc(hidden)]
    pub internal_test_options: Option<InternalTestOptions>,
    /// Network callbacks through which the SDK host decides the runtime's
    /// proxies and supplies its Kerberos tokens.
    ///
    /// When set with at least one callback, the SDK handles the configured
    /// `network.resolveProxy` and `network.authenticateNegotiate` requests
    /// and registers with the runtime via
    /// `network.setProvider` during [`Client::start`], before any session can
    /// be created.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub network: Option<crate::network::NetworkConfig>,
    /// Custom session filesystem provider configuration.
    ///
    /// When set, the SDK calls `sessionFs.setProvider` during
    /// [`Client::start`] to register a virtualizable filesystem layer with
    /// the CLI. Each session created on this client must supply its own
    /// [`SessionFsProvider`] via
    /// [`SessionStateOptions::with_session_fs_provider`](crate::SessionStateOptions::with_session_fs_provider).
    pub session_fs: Option<SessionFsConfig>,
    /// SDK-level mode controlling whether sessions get CLI-style defaults
    /// or are stripped to a minimal/safe baseline. A mode is required. See
    /// [`ClientMode`] for the contract and trade-offs.
    pub mode: ClientMode,
    /// Declares the integrating application's identity, forwarded to the runtime on
    /// the `server.connect` handshake. Declaring it lets the telemetry the
    /// runtime emits on this connection be attributed to a consistent surface
    /// (the application and its Copilot integration) instead of the runtime's own
    /// build. All fields are optional; leave it `None` to keep the runtime's
    /// default attribution.
    pub client_info: Option<ClientInfo>,
    /// Host's Entra registration, shared by its sessions. Omission disables
    /// built-in Entra sign-in and token renewal; other auth methods remain available.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub entra: Option<types::EntraAuthOptions>,
    /// Plugins settings.
    pub plugins: ClientPluginOptions,
    /// Extensions settings.
    pub extensions: ClientExtensionOptions,
    /// Installations settings.
    pub installations: ClientInstallationOptions,
    /// Github settings.
    pub github: ClientGitHubOptions,
    /// Observability settings.
    pub observability: ClientObservabilityOptions,
    /// Models settings.
    pub models: ClientModelOptions,
}
impl std::fmt::Debug for ClientOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientOptions").finish_non_exhaustive()
    }
}
impl ClientOptions {
    /// Construct client settings with an explicitly selected [`ClientMode`].
    ///
    /// ```
    /// # use copilot_sdk::{ClientMode, ClientOptions, LogLevel};
    /// let options = ClientOptions::new(ClientMode::CodingAgent)
    ///     .with_observability(|options| options.with_log_level(LogLevel::Debug));
    /// ```
    ///
    /// Omitting the mode is a compile-time error:
    ///
    /// ```compile_fail
    /// # use copilot_sdk::ClientOptions;
    /// let options = ClientOptions::new();
    /// ```
    ///
    /// There is no implicit default mode:
    ///
    /// ```compile_fail
    /// # use copilot_sdk::ClientOptions;
    /// let options = ClientOptions::default();
    /// ```
    pub fn new(mode: ClientMode) -> Self {
        Self {
            runtime: None,
            internal_test_options: None,
            network: None,
            session_fs: None,
            mode,
            client_info: None,
            entra: None,
            plugins: ClientPluginOptions::default(),
            extensions: ClientExtensionOptions::default(),
            installations: ClientInstallationOptions::default(),
            github: ClientGitHubOptions::default(),
            observability: ClientObservabilityOptions::default(),
            models: ClientModelOptions::default(),
        }
    }
    /// Select an explicit runtime connection.
    pub fn with_runtime(mut self, runtime: RuntimeConnection) -> Self {
        self.runtime = Some(runtime);
        self
    }

    /// Configure unsupported SDK test infrastructure overrides.
    #[doc(hidden)]
    pub fn with_internal_test_options(mut self, options: InternalTestOptions) -> Self {
        self.internal_test_options = Some(options);
        self
    }

    /// Custom session filesystem provider configuration.
    pub fn with_session_fs(mut self, config: SessionFsConfig) -> Self {
        self.session_fs = Some(config);
        self
    }

    /// Take over parts of the runtime's networking from the SDK host; see
    /// [`NetworkConfig`](crate::network::NetworkConfig).
    ///
    /// The configured callbacks are registered with the runtime before
    /// [`Client::start`] returns. By default they serve only requests made for
    /// sessions this client creates or resumes; use
    /// [`NetworkConfig::with_scope`](crate::network::NetworkConfig::with_scope)
    /// with [`NetworkScope::Runtime`](crate::network::NetworkScope::Runtime) to
    /// also serve the runtime's shared background traffic. A client created with
    /// [`Client::from_streams`] registers them with [`Client::register_network`].
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub fn with_network(mut self, config: crate::network::NetworkConfig) -> Self {
        self.network = Some(config);
        self
    }

    /// Set the SDK [`ClientMode`]. Use [`ClientMode::Empty`] for any
    /// scenario where CLI-like ambient behavior is unsafe (e.g. multi-user
    /// servers). Owned runtimes in empty mode require [`OwnedRuntimeOptions::base_directory`]
    /// or [`Self::session_fs`], validated at [`Client::start`].
    pub fn with_mode(mut self, mode: ClientMode) -> Self {
        self.mode = mode;
        self
    }

    /// Declare the integrating application's identity, forwarded to the runtime on
    /// the `server.connect` handshake so its telemetry is attributed to a
    /// consistent surface. See [`Self::client_info`].
    pub fn with_client_info(mut self, client_info: ClientInfo) -> Self {
        self.client_info = Some(client_info);
        self
    }

    /// Configure the host's Microsoft Entra application for its sessions.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub fn with_entra(mut self, entra: impl Into<types::EntraAuthOptions>) -> Self {
        self.entra = Some(entra.into());
        self
    }
    /// Update the plugins option group.
    pub fn with_plugins(
        mut self,
        configure: impl FnOnce(ClientPluginOptions) -> ClientPluginOptions,
    ) -> Self {
        self.plugins = configure(self.plugins);
        self
    }
    /// Update the extensions option group.
    pub fn with_extensions(
        mut self,
        configure: impl FnOnce(ClientExtensionOptions) -> ClientExtensionOptions,
    ) -> Self {
        self.extensions = configure(self.extensions);
        self
    }
    /// Update the installations option group.
    pub fn with_installations(
        mut self,
        configure: impl FnOnce(ClientInstallationOptions) -> ClientInstallationOptions,
    ) -> Self {
        self.installations = configure(self.installations);
        self
    }
    /// Update the github option group.
    pub fn with_github(
        mut self,
        configure: impl FnOnce(ClientGitHubOptions) -> ClientGitHubOptions,
    ) -> Self {
        self.github = configure(self.github);
        self
    }
    /// Update the observability option group.
    pub fn with_observability(
        mut self,
        configure: impl FnOnce(ClientObservabilityOptions) -> ClientObservabilityOptions,
    ) -> Self {
        self.observability = configure(self.observability);
        self
    }
    /// Update the models option group.
    pub fn with_models(
        mut self,
        configure: impl FnOnce(ClientModelOptions) -> ClientModelOptions,
    ) -> Self {
        self.models = configure(self.models);
        self
    }

    #[cfg(feature = "runtime")]
    pub(crate) fn into_startup(self) -> Result<StartupOptions> {
        let runtime = match self.runtime {
            Some(runtime) => runtime,
            None => RuntimeConnection::for_default(OwnedRuntimeOptions::default())?,
        };
        let mut startup = StartupOptions {
            internal_test_options: self.internal_test_options,
            network: self.network,
            session_fs: self.session_fs,
            mode: self.mode,
            client_info: self.client_info,
            entra: self.entra,
            builtin_plugin_directories: self.plugins.builtin_directories,
            extension_launch_provider: self.extensions.launch_provider,
            installation_confirmation_handler: self.installations.confirmation_handler,
            github_token: self.github.token,
            use_logged_in_user: self.github.use_logged_in_user,
            on_github_telemetry: self.github.on_telemetry,
            log_level: self.observability.log_level,
            telemetry: self.observability.telemetry,
            on_get_trace_context: self.observability.on_get_trace_context,
            disable_client_telemetry: self.observability.disable_client_telemetry,
            on_list_models: self.models.on_list_models,
            request_handler: self.models.request_handler,
            ..StartupOptions::default()
        };

        match runtime {
            RuntimeConnection::SpawnedStdio(mut process) => {
                std::mem::take(&mut process.owned).apply(&mut startup);
                startup.transport = Transport::SpawnedStdio(process);
            }
            RuntimeConnection::SpawnedTcp(mut tcp) => {
                std::mem::take(&mut tcp.process.owned).apply(&mut startup);
                startup.transport = Transport::SpawnedTcp {
                    process: tcp.process,
                    port: tcp.port,
                    connection_token: tcp.connection_token,
                };
            }
            RuntimeConnection::InProcess(inprocess) => {
                startup.transport = Transport::InProcess;
                inprocess.owned.apply(&mut startup);
                startup.run_on_ui_thread = inprocess.run_on_ui_thread;
                startup.in_process_runtime_library_path = inprocess.runtime_library_path;
                startup.in_process_auth_owner = inprocess.auth_owner;
            }
            RuntimeConnection::AttachedInProcess(attached) => {
                startup.transport = Transport::InProcess;
                startup.in_process_server_id = Some(attached.server_id);
                startup.in_process_runtime_library_path = Some(attached.runtime_library_path);
                startup.in_process_session_binding = attached.session_binding;
                startup.run_on_ui_thread = attached.run_on_ui_thread;
            }
            RuntimeConnection::Tcp(tcp) => {
                startup.transport = Transport::Tcp {
                    host: tcp.host,
                    port: tcp.port,
                    connection_token: tcp.connection_token,
                };
            }
        }
        Ok(startup)
    }
}
/// Plugins settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientPluginOptions {
    /// Absolute paths to trusted plugin directories bundled by the host.
    ///
    /// When non-empty, [`Client::start`] replaces the runtime's complete
    /// trusted built-in plugin directory set before sessions can be created.
    pub builtin_directories: Vec<PathBuf>,
}
impl std::fmt::Debug for ClientPluginOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientPluginOptions")
            .finish_non_exhaustive()
    }
}
impl ClientPluginOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set trusted plugin directories bundled by the host.
    ///
    /// Every path must be absolute; invalid paths are rejected by
    /// [`Client::start`].
    pub fn with_builtin_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.builtin_directories = paths.into_iter().map(Into::into).collect();
        self
    }
}
/// Extensions settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientExtensionOptions {
    /// Connection-level extension launch profile provider.
    ///
    /// When set, the SDK registers itself with the runtime during
    /// [`Client::start`] before any session can be created. Incoming
    /// `extensionLaunchProvider.resolve` requests are dispatched independently
    /// of sessions.
    pub launch_provider: Option<Arc<dyn crate::extension_launch_provider::ExtensionLaunchProvider>>,
}
impl std::fmt::Debug for ClientExtensionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientExtensionOptions")
            .finish_non_exhaustive()
    }
}
impl ClientExtensionOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a connection-level extension launch profile provider.
    ///
    /// The provider is wrapped in [`Arc`] internally and registered with the
    /// runtime before [`Client::start`] returns.
    pub fn with_launch_provider<P>(mut self, provider: P) -> Self
    where
        P: crate::extension_launch_provider::ExtensionLaunchProvider,
    {
        self.launch_provider = Some(Arc::new(provider));
        self
    }
}
/// Installations settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientInstallationOptions {
    /// Connection-global human review handler for `installations.confirm` (experimental).
    ///
    /// This receives requests without session routing or inferred operation authority.
    /// It does not register or enable installation capabilities on the runtime.
    pub confirmation_handler:
        Option<Arc<dyn crate::installation_confirmation::InstallationConfirmationHandler>>,
}
impl std::fmt::Debug for ClientInstallationOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientInstallationOptions")
            .finish_non_exhaustive()
    }
}
impl ClientInstallationOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install a connection-global human confirmation handler (experimental).
    pub fn with_confirmation_handler<H>(mut self, handler: H) -> Self
    where
        H: crate::installation_confirmation::InstallationConfirmationHandler,
    {
        self.confirmation_handler = Some(Arc::new(handler));
        self
    }
}
/// Github settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientGitHubOptions {
    /// GitHub token for authentication. When set, the SDK passes the token
    /// to the CLI via `--auth-token-env COPILOT_SDK_AUTH_TOKEN` and exports
    /// the token in that env var. When set, the CLI defaults to *not*
    /// using the logged-in user (override with [`Self::use_logged_in_user`]).
    pub token: Option<String>,
    /// Whether the CLI should fall back to the logged-in `gh` user when no
    /// token is provided. `None` means use the runtime default (true unless
    /// [`Self::token`] is set, in which case false). When false, only
    /// explicit tokens such as [`Self::token`] are used; environment
    /// variable tokens (`COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, `GITHUB_TOKEN`)
    /// are ignored.
    pub use_logged_in_user: Option<bool>,
    /// Connection-level GitHub telemetry forwarding callback (experimental).
    ///
    /// When set, every session created or resumed on this client opts into
    /// telemetry forwarding (`enableGitHubTelemetryForwarding`) and the
    /// callback is invoked for each `gitHubTelemetry.event` notification the
    /// runtime forwards. `#[doc(hidden)]`, consistent with the experimental
    /// telemetry payload types.
    #[doc(hidden)]
    pub on_telemetry: Option<crate::github_telemetry::GitHubTelemetryCallback>,
}
impl std::fmt::Debug for ClientGitHubOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientGitHubOptions")
            .finish_non_exhaustive()
    }
}
impl ClientGitHubOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// GitHub token for authentication. The SDK passes the token to the
    /// CLI via `--auth-token-env COPILOT_SDK_AUTH_TOKEN`.
    pub fn with_token(mut self, token: impl Into<String>) -> Self {
        self.token = Some(token.into());
        self
    }

    /// Whether the CLI should fall back to the logged-in `gh` user when
    /// no token is provided. See the field docs for default semantics.
    pub fn with_use_logged_in_user(mut self, use_logged_in: bool) -> Self {
        self.use_logged_in_user = Some(use_logged_in);
        self
    }

    /// Register a connection-level GitHub telemetry forwarding callback
    /// (internal/experimental). Registering a callback auto-enables telemetry
    /// forwarding on every session created or resumed on this client; the
    /// callback fires for each forwarded `gitHubTelemetry.event` notification.
    /// The callback is wrapped in `Arc` internally.
    #[doc(hidden)]
    pub fn with_on_telemetry<F>(mut self, callback: F) -> Self
    where
        F: Fn(crate::github_telemetry::GitHubTelemetryNotification) + Send + Sync + 'static,
    {
        self.on_telemetry = Some(Arc::new(callback));
        self
    }
}
/// Observability settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientObservabilityOptions {
    /// Log level passed to the CLI server via `--log-level`. When `None`,
    /// the SDK does not pass `--log-level` to the runtime at all and the
    /// CLI uses its built-in default.
    pub log_level: Option<LogLevel>,
    /// OpenTelemetry startup config for the owned runtime, including in-process hosts. See
    /// [`TelemetryConfig`] for the env-var mapping. The SDK takes no
    /// OpenTelemetry dependency — this configures the runtime's exporter.
    pub telemetry: Option<TelemetryConfig>,
    /// Optional [`TraceContextProvider`] used to inject W3C Trace Context
    /// headers (`traceparent` / `tracestate`) on outbound `session.create`,
    /// `session.resume`, and `session.send` requests.
    ///
    /// When [`MessageOptions`] carries a per-turn override (set via
    /// [`MessageOptions::with_trace_context`](crate::types::MessageOptions::with_trace_context)
    /// or the underlying fields), it takes precedence over this provider.
    ///
    /// [`MessageOptions`]: crate::types::MessageOptions
    pub on_get_trace_context: Option<Arc<dyn TraceContextProvider>>,
    /// Disable Copilot-owned GitHub and Entra authentication telemetry at startup.
    ///
    /// Applies to owned runtimes, including [`RuntimeConnection::InProcess`], without
    /// changing the host process environment. User-configured OpenTelemetry and
    /// [`ClientGitHubOptions::on_telemetry`] forwarding remain independent.
    pub disable_client_telemetry: bool,
}
impl std::fmt::Debug for ClientObservabilityOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientObservabilityOptions")
            .finish_non_exhaustive()
    }
}
impl ClientObservabilityOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Disable Copilot-owned uploads without affecting independently configured OpenTelemetry.
    pub fn with_disable_client_telemetry(mut self, disabled: bool) -> Self {
        self.disable_client_telemetry = disabled;
        self
    }

    /// Log level passed to the CLI server via `--log-level`.
    pub fn with_log_level(mut self, level: LogLevel) -> Self {
        self.log_level = Some(level);
        self
    }

    /// Set the [`TraceContextProvider`] used to inject W3C Trace Context
    /// headers on outbound `session.create` / `session.resume` /
    /// `session.send` requests. The provider is wrapped in `Arc` internally.
    pub fn with_trace_context_provider<P>(mut self, provider: P) -> Self
    where
        P: TraceContextProvider + 'static,
    {
        self.on_get_trace_context = Some(Arc::new(provider));
        self
    }

    /// OpenTelemetry startup config for the owned runtime.
    pub fn with_telemetry(mut self, config: TelemetryConfig) -> Self {
        self.telemetry = Some(config);
        self
    }
}
/// Models settings. Unset values retain their existing defaults.
#[derive(Default)]
pub struct ClientModelOptions {
    /// Optional override for [`Client::list_models`].
    ///
    /// When set, [`Client::list_models`] returns the handler's result
    /// without making a `models.list` RPC. This is the BYOK escape hatch
    /// for environments where the model catalog is provisioned separately
    /// from the GitHub Copilot CLI (e.g. external inference servers selected via
    /// [`RuntimeConnection::Tcp`]).
    pub on_list_models: Option<Arc<dyn ListModelsHandler>>,
    /// Connection-level Copilot request handler configuration.
    ///
    /// When set, the SDK registers itself as the runtime's request handler
    /// during [`Client::start`], so the runtime routes its model-layer HTTP and
    /// WebSocket traffic — for both CAPI and BYOK sessions — through the
    /// configured
    /// [`CopilotRequestHandler`]
    /// instead of issuing the calls itself.
    pub request_handler: Option<Arc<dyn crate::copilot_request_handler::CopilotRequestHandler>>,
}
impl std::fmt::Debug for ClientModelOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClientModelOptions").finish_non_exhaustive()
    }
}
impl ClientModelOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Override [`Client::list_models`] with a caller-supplied handler.
    /// The handler is wrapped in `Arc` internally.
    pub fn with_list_models_handler<H>(mut self, handler: H) -> Self
    where
        H: ListModelsHandler + 'static,
    {
        self.on_list_models = Some(Arc::new(handler));
        self
    }

    /// Register a connection-level Copilot request handler. The runtime will
    /// route its model-layer HTTP and WebSocket traffic through the handler
    /// configured here instead of issuing the calls itself. The handler is
    /// wrapped in `Arc` internally.
    pub fn with_request_handler<H>(mut self, handler: H) -> Self
    where
        H: crate::copilot_request_handler::CopilotRequestHandler,
    {
        self.request_handler = Some(Arc::new(handler));
        self
    }
}
/// Owned settings. Unset values retain their existing defaults.
#[derive(Clone, Default)]
pub struct OwnedRuntimeOptions {
    /// Override the directory where the CLI persists its state (sessions,
    /// auth, telemetry buffers). When set, exported as `COPILOT_HOME` to
    /// the spawned CLI process. Useful for sandboxing test runs or
    /// running multiple isolated SDK instances side-by-side.
    /// In-process hosts also keep their runtime cache in its `cache` subdirectory.
    pub base_directory: Option<PathBuf>,
    /// Server-wide idle timeout for sessions, in seconds. When set to a
    /// positive value, the SDK passes `--session-idle-timeout <secs>` to
    /// the CLI; sessions without activity for this duration are
    /// automatically cleaned up. `None` or `Some(0)` leaves sessions
    /// running indefinitely (the CLI default).
    pub session_idle_timeout_seconds: Option<u64>,
    /// Override the directory where the bundled CLI binary is extracted on
    /// first use.
    ///
    /// When `None` (the default), the SDK extracts the embedded CLI to
    /// `<platform cache dir>/github-copilot-sdk/cli/<version>/copilot[.exe]`,
    /// where the cache dir is the OS's LocalAppData known folder on
    /// Windows, `~/Library/Caches/` on macOS,
    /// `$XDG_CACHE_HOME` (or `~/.cache/`) on Linux. Use this knob to
    /// redirect the extraction (e.g. to a session-scoped temp directory in
    /// CI runners) without changing the global cache layout.
    ///
    /// Only applies when the `bundled-cli` cargo feature is on (the
    /// default). With `bundled-cli` disabled (`default-features = false`)
    /// there is no archive to re-extract at runtime — the binary lives
    /// at a build-time-known conventional path. To relocate that
    /// extraction, set `COPILOT_CLI_EXTRACT_DIR` (honored symmetrically
    /// at build and runtime); to point the runtime at a different
    /// binary altogether, use [`CliProgram::Path`] or `COPILOT_CLI_PATH`.
    pub bundled_cli_extract_directory: Option<PathBuf>,
}
impl std::fmt::Debug for OwnedRuntimeOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OwnedRuntimeOptions")
            .finish_non_exhaustive()
    }
}
impl OwnedRuntimeOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Server-wide idle timeout for sessions (seconds). Pass `0` to leave
    /// sessions running indefinitely (the CLI default).
    pub fn with_session_idle_timeout_seconds(mut self, seconds: u64) -> Self {
        self.session_idle_timeout_seconds = Some(seconds);
        self
    }

    /// Override the directory where the CLI persists its state. Set as
    /// `COPILOT_HOME` on the spawned CLI process.
    pub fn with_base_directory(mut self, dir: impl Into<PathBuf>) -> Self {
        self.base_directory = Some(dir.into());
        self
    }

    /// Override the directory where bundled CLI and runtime artifacts are
    /// extracted on first use. See [`Self::bundled_cli_extract_directory`].
    ///
    /// Only applies when the `bundled-cli` cargo feature is on. With
    /// `bundled-cli` disabled (`default-features = false`), set
    /// `COPILOT_CLI_EXTRACT_DIR` to relocate the build-time extraction
    /// (honored symmetrically at build and runtime), or use
    /// [`CliProgram::Path`] / `COPILOT_CLI_PATH` to point at a different
    /// binary at runtime.
    pub fn with_bundled_cli_extract_directory(mut self, dir: impl Into<PathBuf>) -> Self {
        self.bundled_cli_extract_directory = Some(dir.into());
        self
    }
    #[cfg(feature = "runtime")]
    fn apply(self, startup: &mut StartupOptions) {
        startup.base_directory = self.base_directory;
        startup.session_idle_timeout_seconds = self.session_idle_timeout_seconds;
        startup.bundled_cli_extract_directory = self.bundled_cli_extract_directory;
    }
}
/// Owned in-process settings. Child-process environment overrides are not applicable.
///
/// ```compile_fail
/// use copilot_sdk::InProcessRuntimeOptions;
/// let options = InProcessRuntimeOptions {
///     env: vec![("GITHUB_TOKEN".into(), "token".into())],
///     ..Default::default()
/// };
/// ```
#[derive(Default)]
pub struct InProcessRuntimeOptions {
    /// Settings for this client-owned runtime.
    pub owned: OwnedRuntimeOptions,
    /// Experimental UI scheduling for [`RuntimeConnection::InProcess`] only.
    ///
    /// Execute the supplied native action exactly once on the host's UI thread
    /// (the process main thread on macOS), completing only after it returns.
    /// Other in-process clients may share this process-wide capability.
    ///
    /// Required for OneAuth interactive authentication on macOS. This general-purpose
    /// scheduler may also be used for other runtime-owned native UI operations.
    /// Temporary OneAuth workaround; this API may be removed if no longer needed.
    pub run_on_ui_thread: Option<Arc<dyn UiThreadHandler>>,
    /// Native image the embedding host already loaded, required for an attached server.
    /// Otherwise defaults to the SDK's ordinary runtime resolver.
    #[doc(hidden)]
    pub runtime_library_path: Option<PathBuf>,
    /// Original live process auth owner for SDK-owned in-process startup.
    ///
    /// The owner binds a one-use startup reservation through its existing
    /// native binder. The SDK retains it until its host closes. This cannot
    /// be combined with an attached server, a borrowed session, or [`ClientGitHubOptions::token`].
    pub auth_owner: Option<Arc<dyn InProcessAuthOwner>>,
}
impl std::fmt::Debug for InProcessRuntimeOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InProcessRuntimeOptions")
            .finish_non_exhaustive()
    }
}
impl InProcessRuntimeOptions {
    /// Construct default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Update shared owned-runtime settings.
    pub fn with_owned(
        mut self,
        configure: impl FnOnce(OwnedRuntimeOptions) -> OwnedRuntimeOptions,
    ) -> Self {
        self.owned = configure(self.owned);
        self
    }
}

/// An SDK-owned TCP child process.
#[derive(Default)]
pub struct SpawnedTcpRuntimeOptions {
    /// Child process and owned-runtime settings.
    pub process: SpawnedProcessOptions,
    /// Listener port. Zero asks the operating system to select it.
    pub port: u16,
    /// Optional token; omission generates a fresh token before starting the process.
    pub connection_token: Option<String>,
}

impl std::fmt::Debug for SpawnedTcpRuntimeOptions {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("SpawnedTcpRuntimeOptions")
            .field("port", &self.port)
            .finish_non_exhaustive()
    }
}

/// A connection to an externally owned runtime.
///
/// ```compile_fail
/// use copilot_sdk::TcpRuntimeOptions;
/// let mut runtime = TcpRuntimeOptions::default();
/// runtime.auth_owner = None;
/// ```
#[derive(Default)]
pub struct TcpRuntimeOptions {
    /// Hostname or IP address without a scheme, port, path, or IPv6 brackets.
    pub host: String,
    /// TCP port, from 1 through 65535.
    pub port: u16,
    /// Token required by the external server.
    pub connection_token: Option<String>,
}

/// Borrow an existing in-process server without changing its startup settings.
///
/// ```compile_fail
/// use copilot_sdk::AttachedInProcessRuntimeOptions;
/// let options = AttachedInProcessRuntimeOptions {
///     server_id: 1,
///     runtime_library_path: "runtime.node".into(),
///     session_binding: None,
///     run_on_ui_thread: None,
///     auth_owner: None,
/// };
/// ```
#[doc(hidden)]
pub struct AttachedInProcessRuntimeOptions {
    /// Existing server retained by the embedding host.
    pub server_id: u32,
    /// Already-loaded image that owns that server.
    pub runtime_library_path: PathBuf,
    /// Original session callback authority, when borrowing a session.
    pub session_binding: Option<InProcessSessionBinding>,
    /// Scheduling shared with the owning host.
    pub run_on_ui_thread: Option<Arc<dyn UiThreadHandler>>,
}

/// Runtime selection with ownership-specific settings.
pub enum RuntimeConnection {
    /// Start a child process and communicate over its standard streams.
    SpawnedStdio(SpawnedProcessOptions),
    /// Start and host a runtime in this process.
    InProcess(InProcessRuntimeOptions),
    /// Start a child process and connect over TCP.
    SpawnedTcp(SpawnedTcpRuntimeOptions),
    /// Connect to a server owned by another host.
    Tcp(TcpRuntimeOptions),
    /// Borrow an existing native server with its original ownership.
    #[doc(hidden)]
    AttachedInProcess(AttachedInProcessRuntimeOptions),
}

impl std::fmt::Debug for RuntimeConnection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::SpawnedStdio(_) => "SpawnedStdio",
            Self::InProcess(_) => "InProcess",
            Self::SpawnedTcp(_) => "SpawnedTcp",
            Self::Tcp(_) => "Tcp",
            Self::AttachedInProcess(_) => "AttachedInProcess",
        };
        formatter.debug_struct(name).finish_non_exhaustive()
    }
}

impl RuntimeConnection {
    /// Settings for an owned runtime, or `None` for an external or borrowed server.
    pub fn owned_options_mut(&mut self) -> Option<&mut OwnedRuntimeOptions> {
        match self {
            Self::SpawnedStdio(child) => Some(&mut child.owned),
            Self::SpawnedTcp(tcp) => Some(&mut tcp.process.owned),
            Self::InProcess(inprocess) => Some(&mut inprocess.owned),
            Self::Tcp(_) | Self::AttachedInProcess(_) => None,
        }
    }

    /// Child-process settings, absent for in-process and external connections.
    pub fn child_process_options_mut(&mut self) -> Option<&mut SpawnedProcessOptions> {
        match self {
            Self::SpawnedStdio(child) => Some(child),
            Self::SpawnedTcp(tcp) => Some(&mut tcp.process),
            _ => None,
        }
    }

    /// Configure an owned standard-stream child process without starting it.
    pub fn for_spawned_stdio(options: SpawnedProcessOptions) -> Self {
        Self::SpawnedStdio(options)
    }
    /// Configure an owned TCP child process without starting it.
    pub fn for_spawned_tcp(options: SpawnedTcpRuntimeOptions) -> Self {
        Self::SpawnedTcp(options)
    }
    /// Configure an owned in-process runtime without starting it.
    pub fn for_in_process(options: InProcessRuntimeOptions) -> Self {
        Self::InProcess(options)
    }
    /// Configure an externally owned TCP server without connecting.
    pub fn for_tcp(host: impl Into<String>, port: u16) -> Self {
        Self::Tcp(TcpRuntimeOptions {
            host: host.into(),
            port,
            connection_token: None,
        })
    }
    /// Capture the deployment default at this call, without starting a runtime.
    ///
    /// `COPILOT_SDK_DEFAULT_CONNECTION` accepts `stdio` and `inprocess`,
    /// case-insensitively. Unset or empty uses the installed SDK default (in-process).
    /// An invalid or non-Unicode value is an error. Only shared owned-runtime
    /// settings are accepted; use a concrete factory for child-process settings.
    pub fn for_default(options: OwnedRuntimeOptions) -> Result<Self> {
        let configured = match std::env::var(DEFAULT_CONNECTION_ENV_VAR) {
            Ok(value) => Some(value),
            Err(std::env::VarError::NotPresent) => None,
            Err(std::env::VarError::NotUnicode(_)) => {
                return Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "COPILOT_SDK_DEFAULT_CONNECTION must be Unicode",
                ));
            }
        };
        Self::default_for_value(configured.as_deref(), options)
    }

    pub(crate) fn default_for_value(
        value: Option<&str>,
        options: OwnedRuntimeOptions,
    ) -> Result<Self> {
        match value {
            None | Some("") => Ok(Self::InProcess(InProcessRuntimeOptions {
                owned: options,
                ..Default::default()
            })),
            Some(value) if value.eq_ignore_ascii_case("stdio") => {
                Ok(Self::SpawnedStdio(SpawnedProcessOptions {
                    owned: options,
                    ..Default::default()
                }))
            }
            Some(value) if value.eq_ignore_ascii_case("inprocess") => {
                Ok(Self::InProcess(InProcessRuntimeOptions {
                    owned: options,
                    ..Default::default()
                }))
            }
            Some(value) => Err(Error::with_message(
                ErrorKind::InvalidConfig,
                format!(
                    "Invalid {DEFAULT_CONNECTION_ENV_VAR} value '{value}': expected stdio, inprocess, empty, or unset"
                ),
            )),
        }
    }
}
