defmodule Copilot.Client do
  @moduledoc """
  Main client for interacting with the Copilot CLI.

  `Copilot.Client` is a GenServer that manages the lifecycle of the CLI
  subprocess, maintains a JSON-RPC connection over stdio, and provides
  session management (create, resume, delete, list).

  ## Example

      {:ok, client} = Copilot.Client.start_link()
      {:ok, session} = Copilot.Client.create_session(client, %Copilot.Types.SessionConfig{})
      :ok = Copilot.Session.send(session, %Copilot.Types.MessageOptions{prompt: "Hello!"})
      Copilot.Session.destroy(session)
      Copilot.Client.stop(client)
  """

  use GenServer
  require Logger

  alias Copilot.JsonRpcClient
  alias Copilot.SdkProtocolVersion
  alias Copilot.Session
  alias Copilot.Types
  alias Copilot.Types.{
    CopilotClientOptions,
    ModelInfo,
    PingResponse,
    GetStatusResponse,
    GetAuthStatusResponse,
    SessionConfig,
    ResumeSessionConfig,
    SessionMetadata,
    Tool,
    ToolResult,
    ToolInvocation,
    PermissionRequest,
    PermissionRequestResult,
    UserInputRequest,
    UserInputResponse,
    SessionHooks,
    ExitPlanModeRequest,
    ExitPlanModeResponse
  }

  # ---------------------------------------------------------------------------
  # State
  # ---------------------------------------------------------------------------

  defmodule State do
    @moduledoc false
    @type t :: %__MODULE__{
            options: CopilotClientOptions.t(),
            rpc: pid() | nil,
            rpc_monitor: reference() | nil,
            sessions: %{String.t() => pid()},
            connection_state: :disconnected | :connecting | :connected | :error,
            models_cache: [ModelInfo.t()] | nil,
            lifecycle_handlers: [fun()],
            mode: :coding_agent | :empty,
            token_providers: :ets.tid() | nil,
            shell_auth_requests: :ets.tid() | nil
          }
    defstruct options: %CopilotClientOptions{},
              # JSON-RPC client; it owns the CLI port
              rpc: nil,
              rpc_monitor: nil,
              sessions: %{},
              connection_state: :disconnected,
              models_cache: nil,
              lifecycle_handlers: [],
              # Client mode: :coding_agent (default) or :empty
              mode: :coding_agent,
              # GitHub token providers: {registration_id, provider, session_id | nil}
              token_providers: nil,
              # Pending shell authentication prompts: {{session_id, request_id}, waiter, ref}
              shell_auth_requests: nil
  end

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Start the Copilot client GenServer.

  ## Options

  All fields from `Copilot.Types.CopilotClientOptions` are supported.
  Pass `name: <name>` in `gen_opts` to register the process.
  """
  @spec start_link(CopilotClientOptions.t(), keyword()) :: GenServer.on_start()
  def start_link(options \\ %CopilotClientOptions{}, gen_opts \\ []) do
    GenServer.start_link(__MODULE__, options, gen_opts)
  end

  @doc "Start the CLI server and establish a JSON-RPC connection."
  @spec start(GenServer.server()) :: :ok | {:error, any()}
  def start(client) do
    GenServer.call(client, :start, 30_000)
  end

  @doc "Stop the CLI server and clean up all sessions."
  @spec stop(GenServer.server()) :: :ok
  def stop(client) do
    GenServer.call(client, :stop, 15_000)
  catch
    :exit, _ -> :ok
  end

  @doc """
  Send a ping to the server.

  Returns `{:ok, %PingResponse{}}` or `{:error, reason}`.
  """
  @spec ping(GenServer.server(), String.t() | nil) :: {:ok, PingResponse.t()} | {:error, any()}
  def ping(client, message \\ nil) do
    GenServer.call(client, {:ping, message}, 10_000)
  end

  @doc "Get CLI status including version and protocol information."
  @spec get_status(GenServer.server()) :: {:ok, GetStatusResponse.t()} | {:error, any()}
  def get_status(client) do
    GenServer.call(client, :get_status, 10_000)
  end

  @doc "Get current authentication status."
  @spec get_auth_status(GenServer.server()) ::
          {:ok, GetAuthStatusResponse.t()} | {:error, any()}
  def get_auth_status(client) do
    GenServer.call(client, :get_auth_status, 10_000)
  end

  @doc "List available models. Results are cached after the first call."
  @spec list_models(GenServer.server()) :: {:ok, [ModelInfo.t()]} | {:error, any()}
  def list_models(client) do
    GenServer.call(client, :list_models, 30_000)
  end

  @doc """
  Create a new conversation session.

  Returns `{:ok, session_pid}` where `session_pid` is a `Copilot.Session` GenServer.
  """
  @spec create_session(GenServer.server(), SessionConfig.t()) ::
          {:ok, pid()} | {:error, any()}
  def create_session(client, config \\ %SessionConfig{}) do
    GenServer.call(client, {:create_session, config}, 30_000)
  end

  @doc """
  Resume an existing session by ID.

  Returns `{:ok, session_pid}`.
  """
  @spec resume_session(GenServer.server(), String.t(), ResumeSessionConfig.t()) ::
          {:ok, pid()} | {:error, any()}
  def resume_session(client, session_id, config \\ %ResumeSessionConfig{}) do
    GenServer.call(client, {:resume_session, session_id, config}, 30_000)
  end

  @doc "Delete a session and its data from disk."
  @spec delete_session(GenServer.server(), String.t()) :: :ok | {:error, any()}
  def delete_session(client, session_id) do
    GenServer.call(client, {:delete_session, session_id}, 10_000)
  end

  @doc "List all available sessions."
  @spec list_sessions(GenServer.server()) :: {:ok, [SessionMetadata.t()]} | {:error, any()}
  def list_sessions(client) do
    GenServer.call(client, :list_sessions, 10_000)
  end

  @doc "Get metadata for a session by ID."
  @spec get_session_metadata(GenServer.server(), String.t()) :: {:ok, map()} | {:error, any()}
  def get_session_metadata(client, session_id) do
    GenServer.call(client, {:get_session_metadata, session_id}, 10_000)
  end

  @doc "Get the last session ID."
  @spec get_last_session_id(GenServer.server()) :: {:ok, String.t() | nil} | {:error, any()}
  def get_last_session_id(client) do
    GenServer.call(client, :get_last_session_id, 10_000)
  end

  @doc "Get the foreground session ID."
  @spec get_foreground_session_id(GenServer.server()) :: {:ok, String.t() | nil} | {:error, any()}
  def get_foreground_session_id(client) do
    GenServer.call(client, :get_foreground_session_id, 10_000)
  end

  @doc "Set the foreground session ID."
  @spec set_foreground_session_id(GenServer.server(), String.t()) :: :ok | {:error, any()}
  def set_foreground_session_id(client, session_id) do
    GenServer.call(client, {:set_foreground_session_id, session_id}, 10_000)
  end

  @doc """
  Sets the session filesystem provider configuration.

  ## Options

    * `:initial_cwd` - Optional initial working directory.
    * `:session_state_path` - Optional path for session state persistence.
    * `:conventions` - Optional list of convention strings.
  """
  @spec set_session_fs_provider(GenServer.server(), map()) :: :ok | {:error, any()}
  def set_session_fs_provider(client, config \\ %{}) do
    GenServer.call(client, {:set_session_fs_provider, config}, 10_000)
  end

  @doc "Get the current connection state."
  @spec get_state(GenServer.server()) :: :disconnected | :connecting | :connected | :error
  def get_state(client) do
    GenServer.call(client, :get_state)
  end

  @doc """
  Subscribe to session lifecycle events.

  Returns a reference that can be used with `unsubscribe_lifecycle/2`.
  """
  @spec subscribe_lifecycle(GenServer.server(), (Types.SessionLifecycleEvent.t() -> any())) ::
          reference()
  def subscribe_lifecycle(client, handler) do
    GenServer.call(client, {:subscribe_lifecycle, handler})
  end

  @doc "Clear the runtime's cached managed (enterprise) settings (`managedSettings.clearCache`)."
  @spec clear_managed_settings_cache(GenServer.server()) :: :ok | {:error, any()}
  def clear_managed_settings_cache(client) do
    GenServer.call(client, :clear_managed_settings_cache, 10_000)
  end

  @doc """
  Save an MCP server secret in the runtime-owned secret store
  (`mcp.secrets.save`). `value` is a credential: it is sent to the runtime and
  never logged. Pass `config_dir: dir` to select the configuration directory
  (the runtime's default otherwise).

  Returns `{:ok, %Types.McpSecretsSaveResult{used_keychain: bool}}`:
  `used_keychain` is `true` when the OS keychain stored the secret and `false`
  for the file backend. Requires Copilot CLI 1.0.116 or newer; older runtimes
  answer with a JSON-RPC method-not-found error, returned as `{:error, error}`.
  """
  @spec mcp_secrets_save(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, Types.McpSecretsSaveResult.t()} | {:error, any()}
  def mcp_secrets_save(client, secret_id, value, opts \\ []) do
    GenServer.call(client, {:mcp_secrets_save, secret_id, value, opts}, 30_000)
  end

  @doc """
  Delete one secret from the runtime-owned MCP secret store
  (`mcp.secrets.delete`). Pass `config_dir: dir` to select the configuration
  directory. Requires Copilot CLI 1.0.116 or newer.
  """
  @spec mcp_secrets_delete(GenServer.server(), String.t(), keyword()) :: :ok | {:error, any()}
  def mcp_secrets_delete(client, secret_id, opts \\ []) do
    GenServer.call(client, {:mcp_secrets_delete, secret_id, opts}, 30_000)
  end

  @doc """
  Delete every stored secret of one MCP server (`mcp.secrets.deleteServer`).
  Pass `config_dir: dir` to select the configuration directory. Requires
  Copilot CLI 1.0.116 or newer.
  """
  @spec mcp_secrets_delete_server(GenServer.server(), String.t(), keyword()) ::
          :ok | {:error, any()}
  def mcp_secrets_delete_server(client, server_name, opts \\ []) do
    GenServer.call(client, {:mcp_secrets_delete_server, server_name, opts}, 30_000)
  end

  @doc """
  Persist a pending plugin extension permission request as a user-scoped
  approval, before answering that request normally
  (`permissions.extensionApprovals.grant`).

  Returns `{:ok, %Types.PermissionsExtensionApprovalsGrantResult{granted: bool,
  approval: %Types.PermissionsExtensionApproval{} | nil}}`. Requires Copilot CLI
  1.0.116 or newer.
  """
  @spec extension_approvals_grant(GenServer.server(), String.t(), String.t()) ::
          {:ok, Types.PermissionsExtensionApprovalsGrantResult.t()} | {:error, any()}
  def extension_approvals_grant(client, session_id, request_id) do
    GenServer.call(client, {:extension_approvals_grant, session_id, request_id}, 30_000)
  end

  @doc """
  List the persisted plugin extension approvals
  (`permissions.extensionApprovals.list`). Pass `session_id: id` to list the
  configuration profile of that attached session.

  Returns `{:ok, %Types.PermissionsExtensionApprovalsListResult{approvals: [...]}}`.
  Requires Copilot CLI 1.0.116 or newer.
  """
  @spec extension_approvals_list(GenServer.server(), keyword()) ::
          {:ok, Types.PermissionsExtensionApprovalsListResult.t()} | {:error, any()}
  def extension_approvals_list(client, opts \\ []) do
    GenServer.call(client, {:extension_approvals_list, opts}, 30_000)
  end

  @doc """
  Revoke a persisted plugin extension approval
  (`permissions.extensionApprovals.revoke`). Pass the approval's
  `extension_name` and `installation_key` unchanged; `session_id: id` selects
  the attached session's configuration profile.

  Returns `{:ok, %Types.PermissionsExtensionApprovalsRevokeResult{revoked: bool}}`.
  Requires Copilot CLI 1.0.116 or newer.
  """
  @spec extension_approvals_revoke(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, Types.PermissionsExtensionApprovalsRevokeResult.t()} | {:error, any()}
  def extension_approvals_revoke(client, extension_name, installation_key, opts \\ []) do
    GenServer.call(
      client,
      {:extension_approvals_revoke, extension_name, installation_key, opts},
      30_000
    )
  end

  # ---------------------------------------------------------------------------
  # GenServer Callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(options) do
    case Types.ClientMode.parse(Map.get(options, :mode)) do
      {:ok, mode} ->
        {:ok,
         %State{
           options: options,
           mode: mode,
           token_providers: :ets.new(:copilot_github_token_providers, [:set, :public]),
           shell_auth_requests: :ets.new(:copilot_shell_auth_requests, [:set, :public])
         }}

      {:error, reason} ->
        {:stop, {:invalid_config, reason}}
    end
  end

  @impl true
  def handle_call(:start, _from, %State{connection_state: :connected} = state) do
    {:reply, :ok, state}
  end

  def handle_call(:start, _from, state) do
    case do_start(state) do
      {:ok, new_state} -> {:reply, :ok, new_state}
      {:error, reason} -> {:reply, {:error, reason}, %{state | connection_state: :error}}
    end
  end

  def handle_call(:stop, _from, state) do
    new_state = do_stop(state)
    {:reply, :ok, new_state}
  end

  def handle_call({:ping, message}, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "ping", %{"message" => message}) do
      {:ok, result} -> {:reply, {:ok, PingResponse.from_map(result)}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:get_status, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "status.get", %{}) do
      {:ok, result} -> {:reply, {:ok, GetStatusResponse.from_map(result)}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:get_auth_status, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "auth.getStatus", %{}) do
      {:ok, result} -> {:reply, {:ok, GetAuthStatusResponse.from_map(result)}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:list_models, _from, %State{models_cache: cache} = state)
      when not is_nil(cache) do
    {:reply, {:ok, cache}, state}
  end

  def handle_call(:list_models, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "models.list", %{}, 30_000) do
      {:ok, %{"models" => models_raw}} ->
        models = Enum.map(models_raw, &ModelInfo.from_map/1)
        {:reply, {:ok, models}, %{state | models_cache: models}}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call({:create_session, config}, _from, state) do
    state = maybe_auto_start(state)

    case validate_session_config(config, state) do
      :ok ->
        registration_id =
          register_github_token_provider(state, config.github_token_provider, config.session_id)

        params =
          config
          |> build_create_params()
          |> put_if("gitHubTokenProviderRegistrationId", registration_id)
          |> group_session_params(:create)
          |> apply_mode_defaults(state.mode)

        case JsonRpcClient.request(state.rpc, "session.create", params, 30_000) do
          {:ok, %{"sessionId" => session_id} = resp} ->
            workspace_path = resp["workspacePath"]
            assign_github_token_provider(state, registration_id, session_id)

            {:ok, session_pid} =
              Session.start_link(
                session_id: session_id,
                rpc: state.rpc,
                workspace_path: workspace_path,
                tools: config.tools,
                on_permission_request: config.on_permission_request,
                on_user_input_request: config.on_user_input_request,
                hooks: config.hooks,
                trace_context_provider: state.options.on_get_trace_context,
                on_shell_authentication_request: config.on_shell_authentication_request,
                store_memory: config.store_memory,
                github_token_registration: registration_id,
                token_providers: state.token_providers
              )

            sessions = Map.put(state.sessions, session_id, session_pid)
            {:reply, {:ok, session_pid}, %{state | sessions: sessions}}

          {:error, _} = err ->
            # A refusing managed-settings policy helper is available through
            # Copilot.Types.PolicyHelperFailure.from_error(err).
            unregister_github_token_provider(state, registration_id)
            {:reply, err, state}
        end

      {:error, _} = invalid ->
        {:reply, invalid, state}
    end
  end

  def handle_call({:resume_session, session_id, config}, _from, state) do
    state = maybe_auto_start(state)

    case validate_session_config(config, state) do
      :ok ->
        registration_id =
          register_github_token_provider(state, config.github_token_provider, session_id)

        params =
          session_id
          |> build_resume_params(config)
          |> put_if("gitHubTokenProviderRegistrationId", registration_id)
          |> group_session_params(:resume)
          |> apply_mode_defaults(state.mode)

        case JsonRpcClient.request(state.rpc, "session.resume", params, 30_000) do
          {:ok, %{"sessionId" => sid} = resp} ->
            workspace_path = resp["workspacePath"]
            assign_github_token_provider(state, registration_id, sid)

            {:ok, session_pid} =
              Session.start_link(
                session_id: sid,
                rpc: state.rpc,
                workspace_path: workspace_path,
                tools: config.tools,
                on_permission_request: config.on_permission_request,
                on_user_input_request: config.on_user_input_request,
                hooks: config.hooks,
                trace_context_provider: state.options.on_get_trace_context,
                on_shell_authentication_request: config.on_shell_authentication_request,
                store_memory: config.store_memory,
                github_token_registration: registration_id,
                token_providers: state.token_providers
              )

            sessions = Map.put(state.sessions, sid, session_pid)
            {:reply, {:ok, session_pid}, %{state | sessions: sessions}}

          {:error, _} = err ->
            unregister_github_token_provider(state, registration_id)
            {:reply, err, state}
        end

      {:error, _} = invalid ->
        {:reply, invalid, state}
    end
  end

  def handle_call({:delete_session, session_id}, _from, state) do
    case JsonRpcClient.request(state.rpc, "session.delete", %{"sessionId" => session_id}) do
      {:ok, %{"success" => false} = resp} ->
        {:reply, {:error, resp["error"] || "Failed to delete session #{session_id}"}, state}

      {:ok, _} ->
        sessions = Map.delete(state.sessions, session_id)
        {:reply, :ok, %{state | sessions: sessions}}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call(:list_sessions, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "session.list", %{}) do
      {:ok, %{"sessions" => raw}} ->
        sessions = Enum.map(raw, &SessionMetadata.from_map/1)
        {:reply, {:ok, sessions}, state}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call({:get_session_metadata, session_id}, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "session.getMetadata", %{"sessionId" => session_id}) do
      {:ok, metadata} -> {:reply, {:ok, metadata}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:get_last_session_id, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "session.getLastId", %{}) do
      {:ok, %{"sessionId" => sid}} -> {:reply, {:ok, sid}, state}
      {:ok, _} -> {:reply, {:ok, nil}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:get_foreground_session_id, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "session.getForeground", %{}) do
      {:ok, %{"sessionId" => sid}} -> {:reply, {:ok, sid}, state}
      {:ok, _} -> {:reply, {:ok, nil}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call({:set_foreground_session_id, session_id}, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "session.setForeground", %{"sessionId" => session_id}) do
      {:ok, %{"success" => true}} ->
        {:reply, :ok, state}

      {:ok, %{"success" => false, "error" => err}} ->
        {:reply, {:error, "Failed to set foreground session: " <> err}, state}

      {:ok, _} ->
        {:reply, {:error, "Failed to set foreground session: Unknown error"}, state}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call({:set_session_fs_provider, config}, _from, state) do
    state = maybe_auto_start(state)

    params =
      %{}
      |> then(fn p -> if config[:initial_cwd], do: Map.put(p, "initialCwd", config[:initial_cwd]), else: p end)
      |> then(fn p -> if config[:session_state_path], do: Map.put(p, "sessionStatePath", config[:session_state_path]), else: p end)
      |> then(fn p -> if config[:conventions], do: Map.put(p, "conventions", config[:conventions]), else: p end)

    case JsonRpcClient.request(state.rpc, "sessionFs.setProvider", params, 10_000) do
      {:ok, _} -> {:reply, :ok, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state.connection_state, state}
  end

  def handle_call({:subscribe_lifecycle, handler}, _from, state) do
    ref = make_ref()
    handlers = [{ref, handler} | state.lifecycle_handlers]
    {:reply, ref, %{state | lifecycle_handlers: handlers}}
  end

  def handle_call(:clear_managed_settings_cache, _from, state) do
    state = maybe_auto_start(state)

    case JsonRpcClient.request(state.rpc, "managedSettings.clearCache", %{}) do
      {:ok, _} -> {:reply, :ok, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call({:mcp_secrets_save, secret_id, value, opts}, _from, state) do
    state = maybe_auto_start(state)

    params =
      Types.compact(%{
        "secretId" => secret_id,
        "value" => value,
        "configDir" => Keyword.get(opts, :config_dir)
      })

    case JsonRpcClient.request(state.rpc, "mcp.secrets.save", params) do
      {:ok, result} -> {:reply, {:ok, Types.McpSecretsSaveResult.from_map(result)}, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call({:mcp_secrets_delete, secret_id, opts}, _from, state) do
    state = maybe_auto_start(state)

    params =
      Types.compact(%{"secretId" => secret_id, "configDir" => Keyword.get(opts, :config_dir)})

    case JsonRpcClient.request(state.rpc, "mcp.secrets.delete", params) do
      {:ok, _} -> {:reply, :ok, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call({:mcp_secrets_delete_server, server_name, opts}, _from, state) do
    state = maybe_auto_start(state)

    params =
      Types.compact(%{"serverName" => server_name, "configDir" => Keyword.get(opts, :config_dir)})

    case JsonRpcClient.request(state.rpc, "mcp.secrets.deleteServer", params) do
      {:ok, _} -> {:reply, :ok, state}
      {:error, _} = err -> {:reply, err, state}
    end
  end

  def handle_call({:extension_approvals_grant, session_id, request_id}, _from, state) do
    state = maybe_auto_start(state)
    params = %{"sessionId" => session_id, "requestId" => request_id}

    case JsonRpcClient.request(state.rpc, "permissions.extensionApprovals.grant", params) do
      {:ok, result} ->
        {:reply, {:ok, Types.PermissionsExtensionApprovalsGrantResult.from_map(result)}, state}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call({:extension_approvals_list, opts}, _from, state) do
    state = maybe_auto_start(state)
    params = Types.compact(%{"sessionId" => Keyword.get(opts, :session_id)})

    case JsonRpcClient.request(state.rpc, "permissions.extensionApprovals.list", params) do
      {:ok, result} ->
        {:reply, {:ok, Types.PermissionsExtensionApprovalsListResult.from_map(result)}, state}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call(
        {:extension_approvals_revoke, extension_name, installation_key, opts},
        _from,
        state
      ) do
    state = maybe_auto_start(state)

    params =
      Types.compact(%{
        "extensionName" => extension_name,
        "installationKey" => installation_key,
        "sessionId" => Keyword.get(opts, :session_id)
      })

    case JsonRpcClient.request(state.rpc, "permissions.extensionApprovals.revoke", params) do
      {:ok, result} ->
        {:reply, {:ok, Types.PermissionsExtensionApprovalsRevokeResult.from_map(result)}, state}

      {:error, _} = err ->
        {:reply, err, state}
    end
  end

  def handle_call(:get_sessions, _from, state) do
    {:reply, state.sessions, state}
  end

  @impl true
  def handle_info({:copilot_notification, method, params}, state) do
    handle_notification(method, params, state)
    {:noreply, state}
  end

  # The JSON-RPC client stopped because the CLI exited or its port closed. A
  # later call starts the CLI again when auto_start is set.
  def handle_info({:DOWN, ref, :process, _pid, _reason}, %State{rpc_monitor: ref} = state) do
    {:noreply,
     %{state | rpc: nil, rpc_monitor: nil, connection_state: :disconnected, models_cache: nil}}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # ---------------------------------------------------------------------------
  # Internal: Start / Stop
  # ---------------------------------------------------------------------------

  defp do_start(state) do
    state = %{state | connection_state: :connecting}

    case cli_launch(state.options) do
      {:ok, {:spawn_executable, executable, _port_settings} = launch} ->
        # The JSON-RPC client opens and owns the CLI port, so it receives
        # everything the CLI writes to stdout.
        case JsonRpcClient.start_link(launch) do
          {:ok, rpc} ->
            connect(rpc, state)

          {:error, reason} ->
            {:error, "Failed to start the Copilot CLI (#{executable}): #{inspect(reason)}"}
        end

      {:error, _} = error ->
        error
    end
  end

  defp connect(rpc, state) do
    opts = state.options

    new_state = %{
      state
      | rpc: rpc,
        rpc_monitor: Process.monitor(rpc),
        connection_state: :connected
    }

    # Register all server-to-client request handlers
    register_rpc_handlers(rpc, new_state)

    # Route notifications through this GenServer so they are dispatched with
    # the current sessions map (events raised while session.create is in
    # flight are delivered once the session is registered).
    client_pid = self()

    JsonRpcClient.set_notification_handler(rpc, fn method, params ->
      send(client_pid, {:copilot_notification, method, params})
      :ok
    end)

    # Optional connect handshake (clientInfo), protocol check, then network callbacks
    result =
      try do
        with :ok <- send_client_info(rpc, opts.client_info),
             :ok <- verify_protocol_version(rpc),
             do: register_network_provider(rpc, opts.network)
      rescue
        e -> {:error, Exception.message(e)}
      end

    case result do
      :ok ->
        {:ok, new_state}

      {:error, reason} ->
        do_stop(new_state)
        {:error, reason}
    end
  end

  # How the CLI is started: {:spawn_executable, executable, port_settings} for
  # JsonRpcClient.start_link/2. Arguments are passed one by one, so paths with
  # spaces work.
  defp cli_launch(opts) do
    cli_path = opts.cli_path || find_cli_path()
    cwd = opts.cwd || File.cwd!()
    log_level = opts.log_level || "info"

    args =
      (opts.cli_args || []) ++
        [
          "--headless",
          "--no-auto-update",
          "--log-level",
          log_level,
          "--stdio"
        ]

    args =
      if opts.github_token do
        args ++ ["--auth-token-env", "COPILOT_SDK_AUTH_TOKEN"]
      else
        args
      end

    use_logged_in =
      case opts.use_logged_in_user do
        nil -> if opts.github_token, do: false, else: true
        val -> val
      end

    args = if use_logged_in, do: args, else: args ++ ["--no-auto-login"]

    env =
      if opts.github_token do
        [{"COPILOT_SDK_AUTH_TOKEN", opts.github_token} | opts.env || []]
      else
        opts.env || []
      end

    # JavaScript entry points (npm-loader.js, legacy index.js) run under node;
    # any other path (a native binary or the node_modules/.bin/copilot shim) is
    # executed directly.
    {executable, launch_args} =
      if String.downcase(Path.extname(cli_path)) in [".js", ".mjs", ".cjs"] do
        {System.find_executable("node") || "node", [cli_path | args]}
      else
        {cli_path, args}
      end

    port_settings = [
      {:args, launch_args},
      {:cd, String.to_charlist(cwd)},
      {:env, Enum.map(env, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end)}
    ]

    {:ok, {:spawn_executable, String.to_charlist(executable), port_settings}}
  rescue
    e -> {:error, Exception.message(e)}
  end

  defp do_stop(state) do
    # Destroy all sessions
    for {_id, session_pid} <- state.sessions do
      try do
        Session.destroy(session_pid)
      rescue
        _ -> :ok
      end
    end

    if state.rpc_monitor, do: Process.demonitor(state.rpc_monitor, [:flush])

    # Stopping the JSON-RPC client closes the CLI port.
    if state.rpc, do: JsonRpcClient.stop(state.rpc)

    # Drop GitHub token provider registrations and pending shell prompts
    for table <- [state.token_providers, state.shell_auth_requests], table != nil do
      :ets.delete_all_objects(table)
    end

    %{
      state
      | rpc: nil,
        rpc_monitor: nil,
        sessions: %{},
        connection_state: :disconnected,
        models_cache: nil
    }
  end

  defp maybe_auto_start(%State{connection_state: :connected} = state), do: state

  defp maybe_auto_start(%State{options: %{auto_start: true}} = state) do
    case do_start(state) do
      {:ok, new_state} -> new_state
      {:error, _} -> state
    end
  end

  defp maybe_auto_start(state), do: state

  # ---------------------------------------------------------------------------
  # Internal: Protocol version verification
  # ---------------------------------------------------------------------------

  defp verify_protocol_version(rpc) do
    expected = SdkProtocolVersion.get()

    case JsonRpcClient.request(rpc, "ping", %{"message" => "version-check"}) do
      {:ok, %{"protocolVersion" => ^expected}} ->
        :ok

      {:ok, %{"protocolVersion" => server_version}} when not is_nil(server_version) ->
        {:error,
         "SDK protocol version mismatch: SDK supports version #{expected}, " <>
           "but server reports version #{server_version}. " <>
           "Please update your SDK or server to ensure compatibility."}

      {:ok, _} ->
        {:error,
         "SDK protocol version mismatch: SDK supports version #{expected}, " <>
           "but server does not report a protocol version. " <>
           "Please update your server to ensure compatibility."}

      {:error, reason} ->
        {:error, "Failed to verify protocol version: #{inspect(reason)}"}
    end
  end

  # Declares the integrating application (clientInfo) on a `connect` request
  # before the ping handshake. Skipped when every clientInfo field is empty;
  # older CLIs without `connect` (-32601 / "Unhandled method connect") are tolerated.
  defp send_client_info(rpc, client_info) do
    case Types.ClientInfo.to_wire(client_info) do
      nil ->
        :ok

      wire ->
        case JsonRpcClient.request(rpc, "connect", %{"clientInfo" => wire}) do
          {:ok, _} ->
            :ok

          {:error, %{"code" => -32601}} ->
            :ok

          {:error, %{"message" => message} = error} when is_binary(message) ->
            if String.contains?(message, "Unhandled method connect"),
              do: :ok,
              else: {:error, "connect failed: #{inspect(error)}"}

          {:error, reason} ->
            {:error, "connect failed: #{inspect(reason)}"}
        end
    end
  end

  # Registers the client network callbacks after the protocol check.
  defp register_network_provider(rpc, network) do
    case Types.NetworkConfig.set_provider_params(network) do
      nil ->
        :ok

      params ->
        case JsonRpcClient.request(rpc, "network.setProvider", params) do
          {:ok, _} -> :ok
          {:error, reason} -> {:error, "Failed to register network callbacks: #{inspect(reason)}"}
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: RPC request handlers
  # ---------------------------------------------------------------------------

  defp register_rpc_handlers(rpc, state) do
    # We use a closure over `self()` so the handler can ask the GenServer
    # for the current sessions map (which changes over time).
    client_pid = self()
    token_providers = state.token_providers
    shell_auth_requests = state.shell_auth_requests

    JsonRpcClient.set_request_handler(rpc, "tool.call", fn params ->
      sessions = get_sessions(client_pid)
      handle_tool_call(sessions, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "permission.request", fn params ->
      sessions = get_sessions(client_pid)
      handle_permission_request(sessions, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "userInput.request", fn params ->
      sessions = get_sessions(client_pid)
      handle_user_input_request(sessions, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "hooks.invoke", fn params ->
      sessions = get_sessions(client_pid)
      handle_hooks_invoke(sessions, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "exitPlanMode.request", fn params ->
      sessions = get_sessions(client_pid)
      handle_exit_plan_mode_request(sessions, params)
    end)

    # The token registry is read directly (no GenServer round-trip), so a
    # token request raised while session.create is in flight can be answered.
    JsonRpcClient.set_request_handler(rpc, "gitHubToken.getToken", fn params ->
      handle_github_token_request(token_providers, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "shellAuthentication.request", fn params ->
      sessions = get_sessions(client_pid)
      handle_shell_authentication_request(sessions, shell_auth_requests, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "shellAuthentication.cancel", fn params ->
      cancel_shell_authentication(shell_auth_requests, params)
    end)

    JsonRpcClient.set_request_handler(rpc, "memoryStore.store", fn params ->
      sessions = get_sessions(client_pid)
      handle_memory_store(sessions, params)
    end)

    register_network_handlers(rpc, state.options.network)
  end

  defp get_sessions(client_pid) do
    try do
      GenServer.call(client_pid, :get_sessions, 5_000)
    catch
      _, _ -> %{}
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Tool call handling
  # ---------------------------------------------------------------------------

  defp handle_tool_call(sessions, params) do
    session_id = params["sessionId"]
    tool_name = params["toolName"]
    tool_call_id = params["toolCallId"]
    arguments = params["arguments"]

    case Map.get(sessions, session_id) do
      nil ->
        %{
          "result" => %{
            "textResultForLlm" => "Unknown session #{session_id}",
            "resultType" => "failure",
            "error" => "unknown session"
          }
        }

      session_pid ->
        invocation = %ToolInvocation{
          session_id: session_id,
          tool_call_id: tool_call_id,
          tool_name: tool_name,
          arguments: arguments
        }

        case Session.handle_tool_call(session_pid, tool_name, arguments, invocation) do
          {:ok, result} -> %{"result" => result}
          {:error, msg} ->
            %{
              "result" => %{
                "textResultForLlm" =>
                  "Invoking this tool produced an error. Detailed information is not available.",
                "resultType" => "failure",
                "error" => msg,
                "toolTelemetry" => %{}
              }
            }
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Permission request handling
  # ---------------------------------------------------------------------------

  defp handle_permission_request(sessions, params) do
    session_id = params["sessionId"]
    perm_request = params["permissionRequest"]

    case Map.get(sessions, session_id) do
      nil ->
        %{"result" => %{"kind" => "denied-no-approval-rule-and-could-not-request-from-user"}}

      session_pid ->
        case Session.handle_permission_request(session_pid, perm_request) do
          {:ok, result} -> %{"result" => result}
          {:error, _} ->
            %{"result" => %{"kind" => "denied-no-approval-rule-and-could-not-request-from-user"}}
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: User input request handling
  # ---------------------------------------------------------------------------

  defp handle_user_input_request(sessions, params) do
    session_id = params["sessionId"]

    case Map.get(sessions, session_id) do
      nil ->
        raise "Session not found: #{session_id}"

      session_pid ->
        request = UserInputRequest.from_map(params)

        case Session.handle_user_input_request(session_pid, request) do
          {:ok, response} -> response
          {:error, reason} -> raise reason
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Hooks invocation
  # ---------------------------------------------------------------------------

  defp handle_hooks_invoke(sessions, params) do
    session_id = params["sessionId"]
    hook_type = params["hookType"]
    input = params["input"]

    case Map.get(sessions, session_id) do
      nil ->
        %{"output" => nil}

      session_pid ->
        case Session.handle_hooks_invoke(session_pid, hook_type, input) do
          {:ok, output} -> %{"output" => output}
          {:error, _} -> %{"output" => nil}
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Exit plan mode request handling
  # ---------------------------------------------------------------------------

  defp handle_exit_plan_mode_request(sessions, params) do
    session_id = params["sessionId"]

    case Map.get(sessions, session_id) do
      nil ->
        %{"result" => %{"approved" => true}}

      session_pid ->
        case Session.handle_exit_plan_mode(session_pid, params) do
          {:ok, result} -> %{"result" => result}
          {:error, _} -> %{"result" => %{"approved" => true}}
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: GitHub token provider (gitHubToken.getToken)
  # ---------------------------------------------------------------------------

  defp handle_github_token_request(table, params) do
    registration_id = params["registrationId"]

    case :ets.lookup(table, registration_id) do
      [{^registration_id, provider, registered_session_id}] ->
        args = %{
          host: params["host"],
          session_id: params["sessionId"] || registered_session_id,
          reason: params["reason"]
        }

        Types.GitHubToken.to_wire(provider.(args))

      [] ->
        raise "No GitHub token provider registered for registration ID #{inspect(registration_id)}"
    end
  end

  defp register_github_token_provider(_state, nil, _session_id), do: nil

  defp register_github_token_provider(state, provider, session_id) do
    registration_id = new_registration_id()
    :ets.insert(state.token_providers, {registration_id, provider, session_id})
    registration_id
  end

  defp assign_github_token_provider(_state, nil, _session_id), do: :ok

  defp assign_github_token_provider(state, registration_id, session_id) do
    :ets.update_element(state.token_providers, registration_id, {3, session_id})
    :ok
  end

  defp unregister_github_token_provider(_state, nil), do: :ok

  defp unregister_github_token_provider(state, registration_id) do
    :ets.delete(state.token_providers, registration_id)
    :ok
  end

  # Random (version 4) UUID used as the provider registration ID.
  defp new_registration_id do
    <<a::32, b::16, _::4, c::12, _::2, d::14, e::48>> = :rand.bytes(16)

    :io_lib.format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", [a, b, c, d + 0x8000, e])
    |> IO.iodata_to_binary()
  end

  # ---------------------------------------------------------------------------
  # Internal: Shell authentication (shellAuthentication.request / .cancel)
  # ---------------------------------------------------------------------------

  # The handler runs in a worker process so that a shellAuthentication.cancel
  # can answer the pending request with `cancel` while the handler still waits.
  defp handle_shell_authentication_request(sessions, pending, params) do
    session_id = params["sessionId"]

    handler =
      case Map.get(sessions, session_id) do
        nil -> nil
        session_pid -> Session.shell_authentication_handler(session_pid)
      end

    unless is_function(handler, 1) do
      raise "Shell authentication requested but no handler registered for session #{session_id}"
    end

    request = Types.ShellAuthenticationRequest.from_map(params)
    key = {session_id, params["requestId"]}
    ref = make_ref()
    waiter = self()
    :ets.insert(pending, {key, waiter, ref})

    {worker, monitor_ref} =
      spawn_monitor(fn -> send(waiter, {ref, {:answer, handler.(request)}}) end)

    reply =
      receive do
        {^ref, {:answer, answer}} ->
          Process.demonitor(monitor_ref, [:flush])
          {:ok, Types.ShellAuthenticationRequest.response_to_wire(answer)}

        {^ref, :cancel} ->
          Process.demonitor(monitor_ref, [:flush])
          Process.exit(worker, :kill)
          {:ok, Types.ShellAuthenticationRequest.response_to_wire(:cancel)}

        {:DOWN, ^monitor_ref, :process, ^worker, reason} ->
          {:failed, reason}
      end

    :ets.delete(pending, key)

    case reply do
      {:ok, wire} -> wire
      {:failed, reason} -> raise "Shell authentication handler failed: #{inspect(reason)}"
    end
  end

  defp cancel_shell_authentication(pending, params) do
    key = {params["sessionId"], params["requestId"]}

    case :ets.take(pending, key) do
      [{_key, waiter, ref}] -> send(waiter, {ref, :cancel})
      [] -> :ok
    end

    %{}
  end

  # ---------------------------------------------------------------------------
  # Internal: Memory store (memoryStore.store)
  # ---------------------------------------------------------------------------

  defp handle_memory_store(sessions, params) do
    session_id = params["sessionId"]

    store =
      case Map.get(sessions, session_id) do
        nil -> nil
        session_pid -> Session.memory_store(session_pid)
      end

    unless is_function(store, 1) do
      raise "Memory store requested but no store registered for session #{session_id}"
    end

    case store.(Types.StoredMemory.from_map(params)) do
      {:error, reason} -> raise "Memory store failed: #{inspect(reason)}"
      _ -> :null
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Network callbacks (network.resolveProxy / authenticate*)
  # ---------------------------------------------------------------------------

  defp register_network_handlers(_rpc, nil), do: :ok

  defp register_network_handlers(rpc, %Types.NetworkConfig{} = network) do
    if is_function(network.resolve_proxy, 1) do
      JsonRpcClient.set_request_handler(rpc, "network.resolveProxy", fn params ->
        case network.resolve_proxy.(%{url: params["url"]}) do
          proxy when is_binary(proxy) and proxy != "" -> %{"proxy" => proxy}
          _ -> %{}
        end
      end)
    end

    # Tokens and passwords are never logged.
    if is_function(network.authenticate_negotiate, 1) do
      JsonRpcClient.set_request_handler(rpc, "network.authenticateNegotiate", fn params ->
        case network.authenticate_negotiate.(%{spn: params["spn"]}) do
          token when is_binary(token) and token != "" -> %{"token" => token}
          _ -> %{}
        end
      end)
    end

    if is_function(network.authenticate_basic, 1) do
      JsonRpcClient.set_request_handler(rpc, "network.authenticateBasic", fn params ->
        request = %{
          proxy: params["proxy"],
          realm: params["realm"],
          previous_rejected: params["previousRejected"] == true
        }

        case network.authenticate_basic.(request) do
          %{username: username} = credentials when is_binary(username) ->
            Types.compact(%{"username" => username, "password" => Map.get(credentials, :password)})

          _ ->
            %{}
        end
      end)
    end

    :ok
  end

  # ---------------------------------------------------------------------------
  # Internal: Notification handling
  # ---------------------------------------------------------------------------

  defp handle_notification("session.event", params, state) do
    session_id = params["sessionId"]
    event = params["event"]

    case Map.get(state.sessions, session_id) do
      nil -> :ok
      session_pid -> Session.dispatch_event(session_pid, event)
    end
  end

  defp handle_notification("session.lifecycle", params, state) do
    event = Types.SessionLifecycleEvent.from_map(params)

    for {_ref, handler} <- state.lifecycle_handlers do
      try do
        handler.(event)
      rescue
        _ -> :ok
      end
    end
  end

  defp handle_notification(_method, _params, _state), do: :ok

  # ---------------------------------------------------------------------------
  # Internal: Helpers
  # ---------------------------------------------------------------------------

  defp find_cli_path do
    case System.find_executable("copilot") do
      nil -> "copilot"
      path -> path
    end
  end

  defp build_create_params(%SessionConfig{} = c) do
    params = %{}
    params = put_if(params, "sessionId", c.session_id)
    params = put_if(params, "model", c.model)
    params = put_if(params, "reasoningEffort", c.reasoning_effort)
    params = put_if(params, "configDir", c.config_dir)
    params = put_if(params, "workingDirectory", c.working_directory)
    params = put_if(params, "streaming", c.streaming)
    params = put_if(params, "availableTools", c.available_tools)
    params = put_if(params, "excludedTools", c.excluded_tools)
    params = put_if(params, "mcpServers", c.mcp_servers)
    params = put_if(params, "skillDirectories", c.skill_directories)
    params = put_if(params, "disabledSkills", c.disabled_skills)

    params =
      if c.tools do
        Map.put(params, "tools", Enum.map(c.tools, &Tool.to_wire/1))
      else
        params
      end

    params =
      if c.system_message do
        Map.put(params, "systemMessage", system_message_to_map(c.system_message))
      else
        params
      end

    params =
      if c.provider do
        Map.put(params, "provider", provider_to_map(c.provider))
      else
        params
      end

    params = Map.put(params, "requestPermission", c.on_permission_request != nil)
    params = Map.put(params, "requestUserInput", c.on_user_input_request != nil)

    params =
      Map.put(
        params,
        "hooks",
        c.hooks != nil and SessionHooks.any_set?(c.hooks)
      )

    params =
      if c.custom_agents do
        Map.put(params, "customAgents", Enum.map(c.custom_agents, &agent_to_map/1))
      else
        params
      end

    params =
      if c.infinite_sessions do
        Map.put(params, "infiniteSessions", infinite_session_to_map(c.infinite_sessions))
      else
        params
      end

    # Upstream-sync session options (parity with @github/copilot-sdk)
    params = put_if(params, "enableCitations", c.enable_citations)
    params = put_if(params, "excludedBuiltinAgents", c.excluded_builtin_agents)
    params = put_if(params, "otlpProtocol", c.otlp_protocol)
    params = put_if(params, "expAssignments", c.exp_assignments)
    params = put_if(params, "rewindEnabled", c.rewind_enabled)
    params = put_if(params, "additionalDirectories", c.additional_directories)
    params = put_if(params, "disabledMcpServers", c.disabled_mcp_servers)
    params = put_if(params, "githubMcpToolConfig", c.github_mcp_tool_config)
    params = put_if(params, "canvasProvider", c.canvas_provider)
    params = put_if(params, "customAgentsLocalOnly", c.custom_agents_local_only)
    params = put_if(params, "toolSearch", c.tool_search)
    params = put_if(params, "experimentalMode", c.experimental_mode)
    params = put_if(params, "contentExclusion", c.content_exclusion)

    params =
      if c.session_limits do
        Map.put(params, "sessionLimits", session_limits_to_map(c.session_limits))
      else
        params
      end

    params =
      if c.memory do
        Map.put(params, "memory", memory_config_to_map(c.memory))
      else
        params
      end

    params =
      if c.on_mcp_auth_request do
        Map.put(params, "mcpAuthHandler", true)
      else
        params
      end

    # Options accepted only by session.create
    params = put_if(params, "name", c.name)
    params = put_if(params, "trustWorkingDirectory", c.trust_working_directory)
    params = put_if(params, "refreshCustomInstructions", c.refresh_custom_instructions)
    params = put_if(params, "defaultProvider", c.default_provider)

    put_shared_options(params, c)
  end

  defp build_resume_params(session_id, %ResumeSessionConfig{} = c) do
    params = %{"sessionId" => session_id}
    params = put_if(params, "model", c.model)
    params = put_if(params, "reasoningEffort", c.reasoning_effort)
    params = put_if(params, "configDir", c.config_dir)
    params = put_if(params, "workingDirectory", c.working_directory)
    params = put_if(params, "streaming", c.streaming)
    params = put_if(params, "availableTools", c.available_tools)
    params = put_if(params, "excludedTools", c.excluded_tools)
    params = put_if(params, "mcpServers", c.mcp_servers)
    params = put_if(params, "skillDirectories", c.skill_directories)
    params = put_if(params, "disabledSkills", c.disabled_skills)
    params = put_if(params, "disableResume", c.disable_resume)

    params =
      if c.tools do
        Map.put(params, "tools", Enum.map(c.tools, &Tool.to_wire/1))
      else
        params
      end

    params =
      if c.system_message do
        Map.put(params, "systemMessage", system_message_to_map(c.system_message))
      else
        params
      end

    params =
      if c.provider do
        Map.put(params, "provider", provider_to_map(c.provider))
      else
        params
      end

    params = Map.put(params, "requestPermission", c.on_permission_request != nil)
    params = Map.put(params, "requestUserInput", c.on_user_input_request != nil)

    params =
      Map.put(
        params,
        "hooks",
        c.hooks != nil and SessionHooks.any_set?(c.hooks)
      )

    params =
      if c.custom_agents do
        Map.put(params, "customAgents", Enum.map(c.custom_agents, &agent_to_map/1))
      else
        params
      end

    params =
      if c.infinite_sessions do
        Map.put(params, "infiniteSessions", infinite_session_to_map(c.infinite_sessions))
      else
        params
      end

    # Options accepted only by session.resume
    params = put_if(params, "allowTranscriptRecovery", c.allow_transcript_recovery)

    # `default_provider: :clear` sends null, which clears the default connection.
    params =
      case c.default_provider do
        :clear -> Map.put(params, "defaultProvider", nil)
        provider -> put_if(params, "defaultProvider", provider)
      end

    put_shared_options(params, c)
  end

  # Flat options shared by SessionConfig and ResumeSessionConfig (v2.6.0 / v3.0.0).
  defp put_shared_options(params, c) do
    params
    |> put_if("modelCapabilities", c.model_capabilities)
    |> put_if("enableConfigDiscovery", c.enable_config_discovery)
    |> put_if("includeSubAgentStreamingEvents", c.include_sub_agent_streaming_events)
    |> put_if("gitHubToken", c.github_token)
    |> put_if("instructionDirectories", c.instruction_directories)
    |> put_if("askUserVariant", c.ask_user_variant)
    |> put_if("sandboxConfig", to_wire_map(c.sandbox_config))
    |> put_if("featureFlags", c.feature_flags)
    |> put_if("allowedModels", c.allowed_models)
    |> put_if("authClientIdMetadataUrl", c.auth_client_id_metadata_url)
    |> put_if("agentDirectories", c.agent_directories)
    |> put_if("hookDirectories", c.hook_directories)
    |> put_if("interactiveUser", c.interactive_user)
    |> put_if("excludedManagedSettings", c.excluded_managed_settings)
    |> put_if("enforceManagedModelDefaults", c.enforce_managed_model_defaults)
    |> put_if("imageGeneration", to_wire_map(c.image_generation))
    |> put_if("autopilot", to_wire_map(c.autopilot))
    |> put_if("providers", to_wire_list(c.providers))
    |> put_if("models", to_wire_list(c.models))
    |> put_if("capi", capi_to_map(c))
    |> put_if("requestShellAuthentication", if(c.on_shell_authentication_request, do: true))
    |> put_if("hasMemoryStore", if(c.store_memory, do: true))
  end

  defp to_wire_map(nil), do: nil
  defp to_wire_map(%Types.SandboxConfig{} = s), do: Types.SandboxConfig.to_map(s)
  defp to_wire_map(%Types.ImageGenerationConfig{} = i), do: Types.ImageGenerationConfig.to_map(i)
  defp to_wire_map(%Types.AutopilotOptions{} = a), do: Types.AutopilotOptions.to_map(a)
  defp to_wire_map(%Types.NamedProviderConfig{} = p), do: Types.NamedProviderConfig.to_map(p)
  defp to_wire_map(%Types.ProviderModelConfig{} = m), do: Types.ProviderModelConfig.to_map(m)
  defp to_wire_map(map) when is_map(map), do: map

  defp to_wire_list(nil), do: nil
  defp to_wire_list(items) when is_list(items), do: Enum.map(items, &to_wire_map/1)

  # Auto routing tier, WebSocket transport and the CAPI catalog switch travel
  # together in the `capi` object (grouped as github.capi).
  defp capi_to_map(c) do
    enabled =
      case c.capi do
        %Types.CapiConfig{enabled: enabled} -> enabled
        %{} = capi -> Map.get(capi, :enabled, Map.get(capi, "enabled"))
        _ -> nil
      end

    capi =
      Types.compact(%{
        "enabled" => enabled,
        "autoTier" => c.auto_tier,
        "enableWebSocketResponses" => c.enable_web_socket_responses
      })

    if map_size(capi) == 0, do: nil, else: capi
  end

  defp validate_session_config(config, state) do
    cond do
      config.github_token != nil and config.github_token_provider != nil ->
        {:error, "github_token and github_token_provider are mutually exclusive"}

      config.github_token_provider != nil and not is_function(config.github_token_provider, 1) ->
        {:error, "github_token_provider must be a function of arity 1"}

      state.mode == :empty and config.available_tools == nil ->
        {:error,
         "CopilotClient is in mode \"empty\" but the session config did not specify " <>
           "available_tools. Empty mode requires every session to explicitly opt into " <>
           "the tools it wants."}

      true ->
        :ok
    end
  end

  # ---------------------------------------------------------------------------
  # SDK protocol 4: grouped session payloads
  # ---------------------------------------------------------------------------

  @session_option_groups ~w(inference tools permissions interaction events agents skills mcp
                            github filesystem execution instructions hooks git state plugins
                            canvases extensions experiments policy)

  # Flat session option key => {group, member}. Keys not listed here (sessionId,
  # model, name, clientName, workingDirectory, configDirectory,
  # enableConfigDiscovery, systemMessage, allowTranscriptRecovery, ...) stay at
  # the root.
  @session_option_paths %{
    "reasoningEffort" => {"inference", "reasoningEffort"},
    "reasoningSummary" => {"inference", "reasoningSummary"},
    "contextTier" => {"inference", "contextTier"},
    "allowMultipleToolCalls" => {"inference", "allowMultipleToolCalls"},
    "enableCitations" => {"inference", "enableCitations"},
    "modelCapabilities" => {"inference", "capabilities"},
    "verbosity" => {"inference", "verbosity"},
    "providers" => {"inference", "providers"},
    "models" => {"inference", "models"},
    "defaultProvider" => {"inference", "defaultProvider"},
    "tools" => {"tools", "definitions"},
    "availableTools" => {"tools", "availableTools"},
    "excludedTools" => {"tools", "excludedTools"},
    "toolSearch" => {"tools", "search"},
    "largeOutput" => {"tools", "largeOutput"},
    "imageGeneration" => {"tools", "imageGeneration"},
    "manageScheduleEnabled" => {"tools", "manageScheduleEnabled"},
    "webSearch" => {"tools", "webSearch"},
    "shell" => {"tools", "shell"},
    "maxInlineBinaryBytes" => {"tools", "maxInlineBinaryBytes"},
    "requestPermission" => {"permissions", "request"},
    "sandboxConfig" => {"permissions", "sandbox"},
    "sandbox" => {"permissions", "sandbox"},
    "permissionMode" => {"permissions", "mode"},
    "sandboxConfigSource" => {"permissions", "sandboxSource"},
    "commands" => {"interaction", "commands"},
    "askUserVariant" => {"interaction", "askUserVariant"},
    "requestUserInput" => {"interaction", "requestUserInput"},
    "requestElicitation" => {"interaction", "requestElicitation"},
    "requestShellAuthentication" => {"interaction", "requestShellAuthentication"},
    "requestExitPlanMode" => {"interaction", "requestExitPlanMode"},
    "requestAutoModeSwitch" => {"interaction", "requestAutoModeSwitch"},
    "requestAutoModeRecovery" => {"interaction", "requestAutoModeRecovery"},
    "streaming" => {"events", "streaming"},
    "includeSubAgentStreamingEvents" => {"events", "includeSubAgentStreamingEvents"},
    "customAgents" => {"agents", "definitions"},
    "defaultAgent" => {"agents", "default"},
    "agent" => {"agents", "selected"},
    "includedBuiltinAgents" => {"agents", "includedBuiltinAgents"},
    "excludedBuiltinAgents" => {"agents", "excludedBuiltinAgents"},
    "customAgentsLocalOnly" => {"agents", "localDiscoveryOnly"},
    "agentDirectories" => {"agents", "directories"},
    "skillDirectories" => {"skills", "directories"},
    "enableSkills" => {"skills", "enabled"},
    "includedBuiltinSkills" => {"skills", "includedBuiltinSkills"},
    "disabledSkills" => {"skills", "disabledSkills"},
    "hasSkillProvider" => {"skills", "hasProvider"},
    "ignoredSkillsLocations" => {"skills", "ignoredLocations"},
    "mcpServers" => {"mcp", "servers"},
    "disabledMcpServers" => {"mcp", "disabledServers"},
    "requestMcpApps" => {"mcp", "enableApps"},
    "enableMcpApps" => {"mcp", "enableApps"},
    "mcpOAuthTokenStorage" => {"mcp", "oauthTokenStorage"},
    "authClientIdMetadataUrl" => {"mcp", "authClientIdMetadataUrl"},
    "envValueMode" => {"mcp", "envValueMode"},
    "managedMcpServers" => {"mcp", "managedServers"},
    "allowAllMcpServerInstructions" => {"mcp", "allowAllServerInstructions"},
    "gitHubToken" => {"github", "token"},
    "gitHubTokenProviderRegistrationId" => {"github", "tokenProviderRegistrationId"},
    "githubMcpToolConfig" => {"github", "mcp"},
    "capi" => {"github", "capi"},
    "enableSessionTelemetry" => {"github", "telemetryEnabled"},
    "enableManagedSettings" => {"github", "fetchManagedSettings"},
    "interactiveUser" => {"github", "interactiveUser"},
    "enableGitHubTelemetryForwarding" => {"github", "telemetryForwarding"},
    "authInfo" => {"github", "authInfo"},
    "copilotUrl" => {"github", "copilotUrl"},
    "additionalDirectories" => {"filesystem", "additionalDirectories"},
    "enableFileChangeTracking" => {"filesystem", "trackFileChanges"},
    "trustWorkingDirectory" => {"filesystem", "trustWorkingDirectory"},
    "sessionLimits" => {"execution", "limits"},
    "autopilot" => {"execution", "autopilot"},
    "autopilotContinuation" => {"execution", "autopilotContinuation"},
    "instructionDirectories" => {"instructions", "directories"},
    "organizationCustomInstructions" => {"instructions", "organizationText"},
    "enableOnDemandInstructionDiscovery" => {"instructions", "onDemandDiscovery"},
    "skipCustomInstructions" => {"instructions", "skipCustomInstructions"},
    "refreshCustomInstructions" => {"instructions", "refreshCustomInstructions"},
    "hooks" => {"hooks", "hasHandlers"},
    "hookDirectories" => {"hooks", "directories"},
    "enableFileHooks" => {"hooks", "enableRepositoryHooks"},
    "coauthorEnabled" => {"git", "coauthorEnabled"},
    "enableHostGitOperations" => {"git", "enableHostGitOperations"},
    "infiniteSessions" => {"state", "infiniteSessions"},
    "memory" => {"state", "memory"},
    "hasMemoryStore" => {"state", "hasMemoryStore"},
    "enableSessionStore" => {"state", "enableCrossSessionStore"},
    "skipEmbeddingRetrieval" => {"state", "skipEmbeddingRetrieval"},
    "embeddingCacheStorage" => {"state", "embeddingCacheStorage"},
    "pluginDirectories" => {"plugins", "directories"},
    "installedPlugins" => {"plugins", "installed"},
    "canvases" => {"canvases", "definitions"},
    "canvasProvider" => {"canvases", "provider"},
    "requestCanvasRenderer" => {"canvases", "requestRenderer"},
    "openCanvases" => {"canvases", "open"},
    "requestExtensions" => {"extensions", "enabled"},
    "extensionSdkPath" => {"extensions", "sdkPath"},
    "extensionInfo" => {"extensions", "identity"},
    "enableExperimentalMode" => {"experiments", "enableExperimentalMode"},
    "isExperimentalMode" => {"experiments", "enableExperimentalMode"},
    # This SDK historically sent the experimental-mode flag as experimentalMode.
    "experimentalMode" => {"experiments", "enableExperimentalMode"},
    "featureFlags" => {"experiments", "flags"},
    "expAssignments" => {"experiments", "assignments"},
    "featureFlagConfiguration" => {"experiments", "configuration"},
    "allowedModels" => {"policy", "allowedModels"},
    "managedSettings" => {"policy", "managedSettings"},
    "excludedManagedSettings" => {"policy", "excludedManagedSettings"},
    "enforceManagedModelDefaults" => {"policy", "enforceManagedModelDefaults"}
  }

  # Keys valid only on the other lifecycle method.
  @create_only_keys ~w(name cloud trustWorkingDirectory refreshCustomInstructions)
  @resume_only_keys ~w(suppressResumeEvent continuePendingWork allowTranscriptRecovery
                       requestedEnvironmentVariables openCanvases)

  # Legacy BYOK provider fields that describe the model, not the connection.
  @legacy_provider_model_fields ~w(modelId wireModel maxPromptTokens maxContextWindowTokens
                                   maxOutputTokens capabilities)

  @empty_mode_defaults [
    {"experiments", "enableExperimentalMode", false},
    {"github", "telemetryEnabled", false},
    {"mcp", "oauthTokenStorage", "in-memory"},
    {"state", "skipEmbeddingRetrieval", true},
    {"state", "embeddingCacheStorage", "in-memory"},
    {"state", "enableCrossSessionStore", false},
    {"state", "memory", %{"enabled" => false}},
    {"instructions", "onDemandDiscovery", false},
    {"hooks", "enableRepositoryHooks", false},
    {"git", "enableHostGitOperations", false}
  ]

  @doc false
  # Converts the flat session payload built by build_create_params/1 and
  # build_resume_params/2 into the SDK protocol 4 grouped payload sent with
  # session.create (`:create`) and session.resume (`:resume`).
  #
  # Protocol 4 runtimes reject the flat keys and unknown group members, while
  # unknown root keys are tolerated. Known flat keys therefore move to their
  # exact group member (for example "streaming" to events.streaming and
  # "hooks" => true to hooks.hasHandlers); root keys and unrecognized keys stay
  # at the top level; nil values are skipped (except an explicit
  # "defaultProvider" => nil on resume, which clears the default provider);
  # members invalid for the method are dropped; empty groups are not emitted.
  # Already-grouped objects are merged and their members win. The legacy
  # single "provider" becomes the named "default" connection unless providers,
  # defaultProvider or capi.enabled are given explicitly.
  @spec group_session_params(map(), :create | :resume) :: map()
  def group_session_params(flat, mode) when is_map(flat) and mode in [:create, :resume] do
    excluded = if mode == :resume, do: @create_only_keys, else: @resume_only_keys

    {root, groups, explicit, provider} =
      Enum.reduce(flat, {%{}, %{}, %{}, nil}, fn {key, value}, {root, groups, explicit, provider} = acc ->
        cond do
          is_nil(value) and not (mode == :resume and key == "defaultProvider") ->
            acc

          key in excluded ->
            acc

          key == "provider" ->
            {root, groups, explicit, value}

          key == "configDir" ->
            {Map.put_new(root, "configDirectory", value), groups, explicit, provider}

          key == "disableResume" and mode == :resume ->
            {Map.put_new(root, "suppressResumeEvent", value), groups, explicit, provider}

          key == "disableResume" ->
            acc

          key in @session_option_groups and is_map(value) ->
            {root, groups, Map.put(explicit, key, value), provider}

          Map.has_key?(@session_option_paths, key) ->
            {group, member} = Map.fetch!(@session_option_paths, key)
            {root, put_group_member(groups, group, member, value), explicit, provider}

          true ->
            {Map.put(root, key, value), groups, explicit, provider}
        end
      end)

    # Explicitly grouped members win over members moved from flat keys.
    groups =
      Enum.reduce(explicit, groups, fn {group, members}, acc ->
        kept =
          members
          |> Enum.reject(fn {member, value} ->
            is_nil(value) and
              not (mode == :resume and group == "inference" and member == "defaultProvider")
          end)
          |> Map.new()

        Map.update(acc, group, kept, &Map.merge(&1, kept))
      end)

    groups
    |> translate_legacy_provider(provider, Map.get(root, "model"))
    |> ensure_mcp_env_value_mode()
    |> drop_lifecycle_members(mode)
    |> Enum.reject(fn {_group, members} -> members == %{} end)
    |> Map.new()
    |> then(&Map.merge(root, &1))
  end

  @doc false
  # Applies the client mode defaults under the caller's grouped values: in
  # :empty mode ambient capabilities default off; :coding_agent leaves the
  # payload unchanged.
  @spec apply_mode_defaults(map(), :coding_agent | :empty) :: map()
  def apply_mode_defaults(params, :empty) do
    Enum.reduce(@empty_mode_defaults, params, fn {group, member, default}, acc ->
      members = Map.get(acc, group, %{})

      if is_nil(Map.get(members, member)) do
        Map.put(acc, group, Map.put(members, member, default))
      else
        acc
      end
    end)
  end

  def apply_mode_defaults(params, _mode), do: params

  defp put_group_member(groups, group, member, value) do
    Map.update(groups, group, %{member => value}, &Map.put(&1, member, value))
  end

  # The legacy single `provider` becomes the named "default" connection
  # (inference.providers), selected by inference.defaultProvider, with CAPI
  # disabled (github.capi.enabled = false) as v1 BYOK did. Model-specific
  # fields move to an inference.models entry for the session model.
  defp translate_legacy_provider(groups, provider, model) when is_map(provider) do
    inference = Map.get(groups, "inference", %{})
    github = Map.get(groups, "github", %{})

    explicit_capi =
      case Map.get(github, "capi") do
        %{} = capi -> Map.has_key?(capi, "enabled")
        _ -> false
      end

    if Map.has_key?(inference, "providers") or Map.has_key?(inference, "defaultProvider") or
         explicit_capi do
      groups
    else
      present =
        provider
        |> Enum.reject(fn {key, value} -> is_nil(value) or key == "name" end)
        |> Map.new()

      {model_fields, connection} = Map.split(present, @legacy_provider_model_fields)

      inference =
        inference
        |> Map.put("providers", [Map.put(connection, "name", "default")])
        |> Map.put("defaultProvider", "default")

      inference =
        if map_size(model_fields) > 0 and is_binary(model) do
          definition = model_fields |> Map.put("provider", "default") |> Map.put("id", model)
          Map.put(inference, "models", [definition])
        else
          inference
        end

      capi =
        case Map.get(github, "capi") do
          %{} = existing -> Map.put(existing, "enabled", false)
          _ -> %{"enabled" => false}
        end

      groups
      |> Map.put("inference", inference)
      |> Map.put("github", Map.put(github, "capi", capi))
    end
  end

  defp translate_legacy_provider(groups, _provider, _model), do: groups

  # mcp.envValueMode is "direct" whenever MCP servers are sent.
  defp ensure_mcp_env_value_mode(%{"mcp" => %{"servers" => _} = mcp} = groups),
    do: Map.put(groups, "mcp", Map.put_new(mcp, "envValueMode", "direct"))

  defp ensure_mcp_env_value_mode(groups), do: groups

  # Protocol 4 runtimes reject members that are invalid for the lifecycle method.
  defp drop_lifecycle_members(groups, :resume) do
    groups
    |> drop_group_member("filesystem", "trustWorkingDirectory")
    |> drop_group_member("instructions", "refreshCustomInstructions")
  end

  defp drop_lifecycle_members(groups, :create),
    do: drop_group_member(groups, "canvases", "open")

  defp drop_group_member(groups, group, member) do
    case groups do
      %{^group => %{} = members} -> Map.put(groups, group, Map.delete(members, member))
      _ -> groups
    end
  end

  defp put_if(map, _key, nil), do: map
  defp put_if(map, key, value), do: Map.put(map, key, value)

  defp system_message_to_map(%Types.SystemMessageAppendConfig{} = c),
    do: Types.SystemMessageAppendConfig.to_map(c)

  defp system_message_to_map(%Types.SystemMessageReplaceConfig{} = c),
    do: Types.SystemMessageReplaceConfig.to_map(c)

  defp provider_to_map(%Types.ProviderConfig{} = p) do
    m = %{}
    m = put_if(m, "type", p.type)
    m = put_if(m, "wireApi", p.wire_api)
    m = put_if(m, "baseUrl", p.base_url)
    m = put_if(m, "apiKey", p.api_key)
    m = put_if(m, "bearerToken", p.bearer_token)
    m = put_if(m, "azure", p.azure)
    m = put_if(m, "apiKeyCommand", p.api_key_command)
    m = put_if(m, "modelProvider", p.model_provider)
    m = put_if(m, "transport", p.transport)
    m = put_if(m, "headers", p.headers)
    m
  end

  defp agent_to_map(%Types.CustomAgentConfig{} = a) do
    m = %{"name" => a.name, "prompt" => a.prompt}
    m = put_if(m, "displayName", a.display_name)
    m = put_if(m, "description", a.description)
    m = put_if(m, "tools", a.tools)
    m = put_if(m, "mcpServers", a.mcp_servers)
    m = put_if(m, "infer", a.infer)
    m = put_if(m, "argsSchema", a.args_schema)
    m
  end

  defp infinite_session_to_map(%Types.InfiniteSessionConfig{} = c) do
    m = %{}
    m = put_if(m, "enabled", c.enabled)
    m = put_if(m, "backgroundCompactionThreshold", c.background_compaction_threshold)
    m = put_if(m, "bufferExhaustionThreshold", c.buffer_exhaustion_threshold)
    m
  end

  defp session_limits_to_map(%Types.SessionLimitsConfig{} = s) do
    put_if(%{}, "maxAiCredits", s.max_ai_credits)
  end

  defp memory_config_to_map(%Types.MemoryConfiguration{} = m) do
    put_if(%{}, "enabled", m.enabled)
  end
end
