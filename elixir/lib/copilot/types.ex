defmodule Copilot.Types do
  @moduledoc """
  Type definitions for the Copilot SDK.

  All types are represented as structs with typespecs, following the same patterns
  as the Node.js and Python SDKs.
  """

  # ---------------------------------------------------------------------------
  # Session Events
  # ---------------------------------------------------------------------------

  @typedoc """
  A session event from the Copilot CLI server.

  Events are maps with at least `type`, `id`, `timestamp`, and `data` keys.
  The `type` field discriminates the event kind (e.g. `"assistant.message"`,
  `"session.idle"`, `"tool.execution_complete"`, etc.).
  """
  @type session_event :: %{
          required(:type) => String.t(),
          required(:id) => String.t(),
          required(:timestamp) => String.t(),
          optional(:parentId) => String.t() | nil,
          optional(:ephemeral) => boolean(),
          optional(:agentId) => String.t() | nil,
          required(:data) => map()
        }

  @typedoc """
  All possible session event type strings.
  """
  @type session_event_type ::
          String.t()

  # ---------------------------------------------------------------------------
  # Tool Result Types
  # ---------------------------------------------------------------------------

  @type tool_result_type :: :success | :failure | :rejected | :denied

  defmodule ToolBinaryResult do
    @moduledoc "Binary result from a tool invocation."
    @type t :: %__MODULE__{
            data: String.t(),
            mime_type: String.t(),
            type: String.t(),
            description: String.t() | nil
          }
    defstruct [:data, :mime_type, :type, :description]
  end

  defmodule ToolResult do
    @moduledoc """
    Structured result of a tool invocation.

    Can also be a plain string, which the SDK wraps into this struct automatically.
    """
    @type t :: %__MODULE__{
            text_result_for_llm: String.t(),
            binary_results_for_llm: [Copilot.Types.ToolBinaryResult.t()] | nil,
            result_type: Copilot.Types.tool_result_type(),
            error: String.t() | nil,
            session_log: String.t() | nil,
            tool_telemetry: map() | nil
          }
    defstruct [
      :text_result_for_llm,
      :binary_results_for_llm,
      :result_type,
      :error,
      :session_log,
      :tool_telemetry
    ]

    @doc "Serialize to the JSON-compatible map expected by the server."
    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = r) do
      m = %{"textResultForLlm" => r.text_result_for_llm, "resultType" => to_string(r.result_type)}
      m = if r.error, do: Map.put(m, "error", r.error), else: m
      m = if r.session_log, do: Map.put(m, "sessionLog", r.session_log), else: m
      m = if r.tool_telemetry, do: Map.put(m, "toolTelemetry", r.tool_telemetry), else: m

      m =
        if r.binary_results_for_llm do
          Map.put(
            m,
            "binaryResultsForLlm",
            Enum.map(r.binary_results_for_llm, fn b ->
              %{
                "data" => b.data,
                "mimeType" => b.mime_type,
                "type" => b.type,
                "description" => b.description
              }
            end)
          )
        else
          m
        end

      m
    end
  end

  # ---------------------------------------------------------------------------
  # Tool Invocation / Definition
  # ---------------------------------------------------------------------------

  defmodule ToolInvocation do
    @moduledoc "Context passed to a tool handler when invoked."
    @type t :: %__MODULE__{
            session_id: String.t(),
            tool_call_id: String.t(),
            tool_name: String.t(),
            arguments: any()
          }
    defstruct [:session_id, :tool_call_id, :tool_name, :arguments]
  end

  @typedoc """
  A tool handler function.

  Receives the parsed arguments and a `ToolInvocation` struct. Must return
  a `ToolResult` struct, a plain string, or any term that will be
  JSON-encoded as a success result.
  """
  @type tool_handler :: (any(), ToolInvocation.t() -> any())

  defmodule Tool do
    @moduledoc """
    Definition of a tool exposed to the Copilot CLI.

    Set `idempotent: true` to mark a tool as safe to retry (repeated calls with
    the same arguments have no additional side effects).
    """
    @type t :: %__MODULE__{
            name: String.t(),
            description: String.t() | nil,
            parameters: map() | nil,
            handler: Copilot.Types.tool_handler(),
            idempotent: boolean() | nil
          }
    defstruct [:name, :description, :parameters, :handler, :idempotent]

    @doc "Serialize to the map sent in session.create / session.resume (`tools.definitions[]`)."
    @spec to_wire(t()) :: map()
    def to_wire(%__MODULE__{} = tool) do
      m = %{"name" => tool.name}
      m = if tool.description, do: Map.put(m, "description", tool.description), else: m
      m = if tool.parameters, do: Map.put(m, "parameters", tool.parameters), else: m
      m = if is_boolean(tool.idempotent), do: Map.put(m, "idempotent", tool.idempotent), else: m
      m
    end
  end

  # ---------------------------------------------------------------------------
  # System Message Configuration
  # ---------------------------------------------------------------------------

  defmodule SystemMessageAppendConfig do
    @moduledoc "Append mode: SDK foundation plus optional appended content."
    @type t :: %__MODULE__{mode: :append, content: String.t() | nil}
    defstruct mode: :append, content: nil

    def to_map(%__MODULE__{} = c) do
      m = %{"mode" => "append"}
      if c.content, do: Map.put(m, "content", c.content), else: m
    end
  end

  defmodule SystemMessageReplaceConfig do
    @moduledoc "Replace mode: caller provides entire system message."
    @type t :: %__MODULE__{mode: :replace, content: String.t()}
    defstruct mode: :replace, content: ""

    def to_map(%__MODULE__{} = c) do
      %{"mode" => "replace", "content" => c.content}
    end
  end

  defmodule SystemMessageCustomizeConfig do
    @moduledoc "Customize mode: override individual sections of the system prompt."
    @type t :: %__MODULE__{
            mode: :customize,
            sections: %{optional(String.t()) => SectionOverride.t()} | nil,
            content: String.t() | nil
          }
    defstruct mode: :customize, sections: nil, content: nil

    def to_map(%__MODULE__{} = c) do
      m = %{"mode" => "customize"}
      m = if c.sections do
        sections_map = c.sections
          |> Enum.map(fn {k, v} -> {k, SectionOverride.to_map(v)} end)
          |> Map.new()
        Map.put(m, "sections", sections_map)
      else
        m
      end
      if c.content, do: Map.put(m, "content", c.content), else: m
    end
  end

  defmodule SectionOverride do
    @moduledoc "Override operation for a single system prompt section."
    @type t :: %__MODULE__{action: String.t(), content: String.t() | nil}
    defstruct [:action, :content]

    def to_map(%__MODULE__{} = o) do
      m = %{"action" => o.action}
      if o.content, do: Map.put(m, "content", o.content), else: m
    end
  end

  @doc "Known system prompt section identifiers for the 'customize' mode."
  defmodule SystemPromptSection do
    @moduledoc false
    def identity, do: "identity"
    def tone, do: "tone"
    def tool_efficiency, do: "tool_efficiency"
    def environment_context, do: "environment_context"
    def code_change_rules, do: "code_change_rules"
    def guidelines, do: "guidelines"
    def safety, do: "safety"
    def tool_instructions, do: "tool_instructions"
    def custom_instructions, do: "custom_instructions"
    def last_instructions, do: "last_instructions"
  end

  @doc "Override action constants."
  defmodule SectionOverrideAction do
    @moduledoc false
    def replace, do: "replace"
    def remove, do: "remove"
    def append, do: "append"
    def prepend, do: "prepend"
  end

  @type system_message_config :: SystemMessageAppendConfig.t() | SystemMessageReplaceConfig.t() | SystemMessageCustomizeConfig.t()

  # ---------------------------------------------------------------------------
  # Permission Types
  # ---------------------------------------------------------------------------

  defmodule PermissionRequest do
    @moduledoc "Permission request from the server."
    @type t :: %__MODULE__{
            kind: String.t(),
            tool_call_id: String.t() | nil,
            extra: map()
          }
    defstruct [:kind, :tool_call_id, extra: %{}]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        kind: m["kind"],
        tool_call_id: m["toolCallId"],
        extra: Map.drop(m, ["kind", "toolCallId"])
      }
    end
  end

  defmodule PermissionRequestResult do
    @moduledoc "Result of a permission decision."
    @type kind ::
            :approved
            | :denied_by_rules
            | :denied_no_approval_rule_and_could_not_request_from_user
            | :denied_interactively_by_user

    @type t :: %__MODULE__{kind: kind(), rules: [any()] | nil, decision_context: map() | nil}
    defstruct [:kind, :rules, :decision_context]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = r) do
      kind_str =
        case r.kind do
          :approved -> "approved"
          :denied_by_rules -> "denied-by-rules"
          :denied_no_approval_rule_and_could_not_request_from_user -> "denied-no-approval-rule-and-could-not-request-from-user"
          :denied_interactively_by_user -> "denied-interactively-by-user"
          other when is_binary(other) -> other
        end

      m = %{"kind" => kind_str}
      m = if r.rules, do: Map.put(m, "rules", r.rules), else: m
      if r.decision_context, do: Map.put(m, "decisionContext", r.decision_context), else: m
    end
  end

  @typedoc "Permission handler callback."
  @type permission_handler ::
          (PermissionRequest.t(), %{session_id: String.t()} -> PermissionRequestResult.t())

  @typedoc "MCP OAuth host token handler: supplies an OAuth token for an MCP auth request."
  @type mcp_auth_handler :: (map() -> map() | String.t())

  @typedoc "BYOK bearer-token provider: mints a fresh bearer token per session."
  @type bearer_token_provider :: (ProviderTokenArgs.t() -> String.t())

  @typedoc "Intercepts outbound LLM inference HTTP/WebSocket requests."
  @type copilot_request_handler :: (map(), map() -> map())

  # ---------------------------------------------------------------------------
  # User Input Types
  # ---------------------------------------------------------------------------

  defmodule UserInputRequest do
    @moduledoc "Request for user input from the agent (enables ask_user tool)."
    @type t :: %__MODULE__{
            question: String.t(),
            choices: [String.t()] | nil,
            allow_freeform: boolean()
          }
    defstruct [:question, :choices, allow_freeform: true]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        question: m["question"],
        choices: m["choices"],
        allow_freeform: Map.get(m, "allowFreeform", true)
      }
    end
  end

  defmodule UserInputResponse do
    @moduledoc "Response to a user input request."
    @type t :: %__MODULE__{answer: String.t(), was_freeform: boolean()}
    defstruct [:answer, :was_freeform]

    def to_map(%__MODULE__{} = r) do
      %{"answer" => r.answer, "wasFreeform" => r.was_freeform}
    end
  end

  @typedoc "User input handler callback."
  @type user_input_handler ::
          (UserInputRequest.t(), %{session_id: String.t()} -> UserInputResponse.t())

  # ---------------------------------------------------------------------------
  # Hook Types
  # ---------------------------------------------------------------------------

  defmodule SessionHooks do
    @moduledoc """
    Configuration for session hooks.

    Each field is an optional callback that the server invokes at the
    corresponding lifecycle point.
    """
    @type t :: %__MODULE__{
            on_pre_tool_use: (map(), map() -> map() | nil) | nil,
            on_post_tool_use: (map(), map() -> map() | nil) | nil,
            on_pre_mcp_tool_call: (map(), map() -> map() | nil) | nil,
            on_user_prompt_submitted: (map(), map() -> map() | nil) | nil,
            on_user_prompt_transformed: (map(), map() -> map() | nil) | nil,
            on_session_start: (map(), map() -> map() | nil) | nil,
            on_session_end: (map(), map() -> map() | nil) | nil,
            on_error_occurred: (map(), map() -> map() | nil) | nil,
            on_subagent_start: (map(), map() -> map() | nil) | nil,
            on_subagent_stop: (map(), map() -> map() | nil) | nil
          }
    defstruct [
      :on_pre_tool_use,
      :on_post_tool_use,
      :on_pre_mcp_tool_call,
      :on_user_prompt_submitted,
      :on_user_prompt_transformed,
      :on_session_start,
      :on_session_end,
      :on_error_occurred,
      # Sub-agent lifecycle hooks (hook types "subagentStart" / "subagentStop").
      # subagentStart input: timestamp, cwd, transcriptPath, agentName,
      # agentDisplayName, agentDescription; output: additionalContext.
      # subagentStop input adds agentId, agentType, stopReason, response;
      # output: decision ("block" | "allow"), reason, modifiedResponse.
      :on_subagent_start,
      :on_subagent_stop
    ]

    @doc "Returns true if any hook handler is set."
    @spec any_set?(t()) :: boolean()
    def any_set?(%__MODULE__{} = h) do
      h.on_pre_tool_use != nil or
        h.on_post_tool_use != nil or
        h.on_pre_mcp_tool_call != nil or
        h.on_user_prompt_submitted != nil or
        h.on_user_prompt_transformed != nil or
        h.on_session_start != nil or
        h.on_session_end != nil or
        h.on_error_occurred != nil or
        h.on_subagent_start != nil or
        h.on_subagent_stop != nil
    end

    @doc "Look up a handler by hook type string."
    @spec handler_for(t(), String.t()) :: (map(), map() -> map() | nil) | nil
    def handler_for(%__MODULE__{} = h, hook_type) do
      case hook_type do
        "preToolUse" -> h.on_pre_tool_use
        "postToolUse" -> h.on_post_tool_use
        "preMcpToolCall" -> h.on_pre_mcp_tool_call
        "userPromptSubmitted" -> h.on_user_prompt_submitted
        "userPromptTransformed" -> h.on_user_prompt_transformed
        "sessionStart" -> h.on_session_start
        "sessionEnd" -> h.on_session_end
        "errorOccurred" -> h.on_error_occurred
        "subagentStart" -> h.on_subagent_start
        "subagentStop" -> h.on_subagent_stop
        _ -> nil
      end
    end
  end

  # ---------------------------------------------------------------------------
  # MCP Server Configuration
  # ---------------------------------------------------------------------------

  defmodule MCPLocalServerConfig do
    @moduledoc "Configuration for a local/stdio MCP server."
    @type t :: %__MODULE__{
            tools: [String.t()],
            type: String.t() | nil,
            timeout: non_neg_integer() | nil,
            command: String.t(),
            args: [String.t()],
            env: map() | nil,
            cwd: String.t() | nil
          }
    defstruct [:command, :args, :tools, :type, :timeout, :env, :cwd]
  end

  defmodule MCPRemoteServerConfig do
    @moduledoc "Configuration for a remote MCP server (HTTP or SSE)."
    @type t :: %__MODULE__{
            tools: [String.t()],
            type: String.t(),
            timeout: non_neg_integer() | nil,
            url: String.t(),
            headers: map() | nil
          }
    defstruct [:url, :tools, :type, :timeout, :headers]
  end

  @type mcp_server_config :: MCPLocalServerConfig.t() | MCPRemoteServerConfig.t()

  # ---------------------------------------------------------------------------
  # Custom Agent Configuration
  # ---------------------------------------------------------------------------

  defmodule CustomAgentConfig do
    @moduledoc "Configuration for a custom agent."
    @type t :: %__MODULE__{
            name: String.t(),
            display_name: String.t() | nil,
            description: String.t() | nil,
            tools: [String.t()] | nil,
            prompt: String.t(),
            mcp_servers: map() | nil,
            infer: boolean() | nil,
            skills: [String.t()] | nil,
            args_schema: map() | nil
          }
    defstruct [
      :name,
      :display_name,
      :description,
      :tools,
      :prompt,
      :mcp_servers,
      :infer,
      # List of skill names to preload
      :skills,
      # JSON schema describing the agent's arguments
      :args_schema
    ]
  end

  # ---------------------------------------------------------------------------
  # Infinite Session Configuration
  # ---------------------------------------------------------------------------

  defmodule InfiniteSessionConfig do
    @moduledoc "Configuration for infinite sessions with automatic context compaction."
    @type t :: %__MODULE__{
            enabled: boolean() | nil,
            background_compaction_threshold: float() | nil,
            buffer_exhaustion_threshold: float() | nil
          }
    defstruct [:enabled, :background_compaction_threshold, :buffer_exhaustion_threshold]
  end

  # ---------------------------------------------------------------------------
  # Upstream-sync Feature Types & Constants (parity with @github/copilot-sdk)
  # ---------------------------------------------------------------------------

  defmodule SessionLimitsConfig do
    @moduledoc "Per-session AI-credit budget; set `max_ai_credits` to cap spend."
    @type t :: %__MODULE__{max_ai_credits: number() | nil}
    defstruct [:max_ai_credits]
  end

  defmodule MemoryConfiguration do
    @moduledoc "Opt-in persistent session memory."
    @type t :: %__MODULE__{enabled: boolean() | nil}
    defstruct [:enabled]
  end

  defmodule ProviderTokenArgs do
    @moduledoc "Arguments passed to a BYOK bearer-token provider (per-session scoping)."
    @type t :: %__MODULE__{session_id: String.t()}
    defstruct [:session_id]
  end

  defmodule ToolDefer do
    @moduledoc ~S|Tool "defer" loading policy: `auto/0` (lazy via search) or `never/0` (eager pre-load).|
    @spec auto() :: String.t()
    def auto, do: "auto"
    @spec never() :: String.t()
    def never, do: "never"
  end

  defmodule SystemMessageSection do
    @moduledoc """
    System-message section identifiers. The `preamble/0` section targets only the
    identity preamble; `preserve/0` protects an addressable section from removal.
    """
    def preamble, do: "preamble"
    def identity, do: "identity"
    def tool_instructions, do: "tool_instructions"
    def preserve, do: "preserve"
  end

  defmodule GitHubAttachment do
    @moduledoc "GitHub-anchored attachment variants."
    def github_commit, do: "GitHubCommit"
    def github_release, do: "GitHubRelease"
    def github_actions_job, do: "GitHubActionsJob"
    def github_repository, do: "GitHubRepository"
    def github_file_diff, do: "GitHubFileDiff"
    def github_tree_comparison, do: "GitHubTreeComparison"
    def github_url, do: "GitHubUrl"
    def github_file, do: "GitHubFile"
    def github_snippet, do: "GitHubSnippet"
  end

  # ---------------------------------------------------------------------------
  # Provider Configuration
  # ---------------------------------------------------------------------------

  defmodule ProviderConfig do
    @moduledoc """
    Configuration for a custom API provider (legacy single BYOK provider).

    On SDK protocol 4 the SDK sends it as the named `"default"` connection in
    `inference.providers`, selects it with `inference.defaultProvider`, and
    disables Copilot API models (`github.capi.enabled: false`), preserving v1
    behavior. Prefer `providers` / `models` (`NamedProviderConfig`,
    `ProviderModelConfig`) for new code.

    `api_key_command` is a command whose stdout is used as the API key;
    `model_provider` is one of `"openai"`, `"anthropic"`, `"azure_openai"`,
    `"ollama"`, `"lm_studio"`, `"foundry_local"` or `"llama_cpp"`.
    """
    @type t :: %__MODULE__{
            type: String.t() | nil,
            wire_api: String.t() | nil,
            base_url: String.t(),
            api_key: String.t() | nil,
            bearer_token: String.t() | nil,
            azure: map() | nil,
            api_key_command: String.t() | nil,
            model_provider: String.t() | nil,
            transport: String.t() | nil,
            headers: map() | nil
          }
    defstruct [
      :type,
      :wire_api,
      :base_url,
      :api_key,
      :bearer_token,
      :azure,
      :api_key_command,
      :model_provider,
      :transport,
      :headers
    ]
  end

  # ---------------------------------------------------------------------------
  # v2.6.0 Session Option Types
  # ---------------------------------------------------------------------------

  defmodule ClientInfo do
    @moduledoc """
    Identity of the integrating application, declared on the `connect`
    handshake (`clientInfo`). Empty fields are dropped; when every field is
    empty, `connect` is not sent.
    """
    @type t :: %__MODULE__{
            editor_name: String.t() | nil,
            editor_version: String.t() | nil,
            extension_name: String.t() | nil,
            extension_version: String.t() | nil
          }
    defstruct [:editor_name, :editor_version, :extension_name, :extension_version]

    @doc "Wire map for `connect` (`clientInfo`); `nil` when every field is empty."
    @spec to_wire(t() | nil) :: map() | nil
    def to_wire(nil), do: nil

    def to_wire(%__MODULE__{} = info) do
      wire =
        [
          {"editorName", info.editor_name},
          {"editorVersion", info.editor_version},
          {"extensionName", info.extension_name},
          {"extensionVersion", info.extension_version}
        ]
        |> Enum.reject(fn {_key, value} -> value in [nil, ""] end)
        |> Map.new()

      if map_size(wire) == 0, do: nil, else: wire
    end
  end

  defmodule SandboxConfig do
    @moduledoc """
    Sandbox configuration (`permissions.sandbox`). `allow_bypass: true` lets the
    agent ask to run an individual command outside the sandbox; the request
    goes through the normal permission flow.
    """
    @type t :: %__MODULE__{
            enabled: boolean(),
            user_policy: map() | nil,
            add_current_working_directory: boolean() | nil,
            sandbox_mcp_servers: boolean() | nil,
            sandbox_lsp_servers: boolean() | nil,
            sandbox_extensions: boolean() | nil,
            allow_bypass: boolean() | nil
          }
    defstruct [
      enabled: true,
      user_policy: nil,
      add_current_working_directory: nil,
      sandbox_mcp_servers: nil,
      sandbox_lsp_servers: nil,
      sandbox_extensions: nil,
      allow_bypass: nil
    ]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = s) do
      Copilot.Types.compact(%{
        "enabled" => s.enabled,
        "userPolicy" => s.user_policy,
        "addCurrentWorkingDirectory" => s.add_current_working_directory,
        "sandboxMcpServers" => s.sandbox_mcp_servers,
        "sandboxLspServers" => s.sandbox_lsp_servers,
        "sandboxExtensions" => s.sandbox_extensions,
        "allowBypass" => s.allow_bypass
      })
    end
  end

  defmodule ImageGenerationConfig do
    @moduledoc "Image generation opt-in (`tools.imageGeneration`)."
    @type t :: %__MODULE__{
            enabled: boolean() | nil,
            watermark: boolean() | nil,
            save_to_disk: boolean() | nil
          }
    defstruct [:enabled, :watermark, :save_to_disk]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = c) do
      Copilot.Types.compact(%{
        "enabled" => c.enabled,
        "watermark" => c.watermark,
        "saveToDisk" => c.save_to_disk
      })
    end
  end

  defmodule AutopilotOptions do
    @moduledoc "Autopilot continuation options (`execution.autopilot`)."
    @type t :: %__MODULE__{
            continuation_enabled: boolean() | nil,
            max_continues: non_neg_integer() | nil
          }
    defstruct [:continuation_enabled, :max_continues]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = a) do
      Copilot.Types.compact(%{
        "continuationEnabled" => a.continuation_enabled,
        "maxContinues" => a.max_continues
      })
    end
  end

  defmodule MessageSource do
    @moduledoc ~S|Message provenance for the `source` message option: `"user"`, `"system"`, or `"agent-<id>"`.|
    @spec user() :: String.t()
    def user, do: "user"
    @spec system() :: String.t()
    def system, do: "system"
    @spec agent(String.t()) :: String.t()
    def agent(agent_id), do: "agent-" <> agent_id
  end

  defmodule GitHubToken do
    @moduledoc """
    A GitHub token returned by a session's `github_token_provider`
    (`gitHubToken.getToken` reply `{kind: "token", accessToken, tokenType?, expiresIn}`).
    A provider may also return `:cancelled` to decline.

    `expires_in` is the remaining lifetime in seconds. The runtime requires more
    than one hour; it defaults to 28800 (8 hours, the typical GitHub token
    lifetime).
    """
    @type t :: %__MODULE__{
            access_token: String.t(),
            token_type: String.t() | nil,
            expires_in: non_neg_integer()
          }
    defstruct [:access_token, :token_type, expires_in: 28_800]

    @doc "Encode a provider result as the `gitHubToken.getToken` reply."
    @spec to_wire(t() | :cancelled | map() | String.t()) :: map()
    def to_wire(:cancelled), do: %{"kind" => "cancelled"}

    def to_wire(token) when is_binary(token), do: to_wire(%__MODULE__{access_token: token})

    def to_wire(%__MODULE__{} = t) do
      Copilot.Types.compact(%{
        "kind" => "token",
        "accessToken" => t.access_token,
        "tokenType" => t.token_type,
        "expiresIn" => t.expires_in
      })
    end

    def to_wire(%{access_token: token} = m) do
      to_wire(%__MODULE__{
        access_token: token,
        token_type: Map.get(m, :token_type),
        expires_in: Map.get(m, :expires_in, 28_800)
      })
    end
  end

  # ---------------------------------------------------------------------------
  # v3.0.0 Types (SDK protocol 4)
  # ---------------------------------------------------------------------------

  defmodule ClientMode do
    @moduledoc """
    Client modes. `coding_agent/0` (`"coding-agent"`, the default) keeps the
    CLI-like coding tools and defaults; `empty/0` (`"empty"`) enables only what
    each session asks for. The legacy name `"copilot-cli"` is an alias of
    `"coding-agent"`.
    """
    @spec coding_agent() :: String.t()
    def coding_agent, do: "coding-agent"
    @spec empty() :: String.t()
    def empty, do: "empty"

    @doc "Normalize a mode option to `:coding_agent` or `:empty`."
    @spec parse(term()) :: {:ok, :coding_agent | :empty} | {:error, String.t()}
    def parse(nil), do: {:ok, :coding_agent}
    def parse(:coding_agent), do: {:ok, :coding_agent}
    def parse(:empty), do: {:ok, :empty}
    def parse("coding-agent"), do: {:ok, :coding_agent}
    def parse("copilot-cli"), do: {:ok, :coding_agent}
    def parse("empty"), do: {:ok, :empty}

    def parse(other),
      do: {:error, "invalid client mode #{inspect(other)}: expected \"coding-agent\" or \"empty\""}
  end

  defmodule NamedProviderConfig do
    @moduledoc """
    A named BYOK provider connection (`inference.providers[]`). Model
    definitions reference it by `name` (which cannot contain `/`).
    """
    @type t :: %__MODULE__{
            name: String.t(),
            type: String.t() | nil,
            base_url: String.t(),
            api_key: String.t() | nil,
            api_key_command: String.t() | nil,
            bearer_token: String.t() | nil,
            wire_api: String.t() | nil,
            transport: String.t() | nil,
            azure: map() | nil,
            headers: map() | nil,
            model_provider: String.t() | nil
          }
    defstruct [
      :name,
      :type,
      :base_url,
      :api_key,
      :api_key_command,
      :bearer_token,
      :wire_api,
      :transport,
      :azure,
      :headers,
      :model_provider
    ]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = p) do
      Copilot.Types.compact(%{
        "name" => p.name,
        "type" => p.type,
        "baseUrl" => p.base_url,
        "apiKey" => p.api_key,
        "apiKeyCommand" => p.api_key_command,
        "bearerToken" => p.bearer_token,
        "wireApi" => p.wire_api,
        "transport" => p.transport,
        "azure" => p.azure,
        "headers" => p.headers,
        "modelProvider" => p.model_provider
      })
    end
  end

  defmodule ProviderModelConfig do
    @moduledoc """
    A model served by a named provider (`inference.models[]`). Select it with
    the canonical `"<provider>/<id>"` model name.
    """
    @type t :: %__MODULE__{
            provider: String.t(),
            id: String.t(),
            model_id: String.t() | nil,
            wire_model: String.t() | nil,
            name: String.t() | nil,
            max_prompt_tokens: non_neg_integer() | nil,
            max_context_window_tokens: non_neg_integer() | nil,
            max_output_tokens: non_neg_integer() | nil,
            capabilities: map() | nil
          }
    defstruct [
      :provider,
      :id,
      :model_id,
      :wire_model,
      :name,
      :max_prompt_tokens,
      :max_context_window_tokens,
      :max_output_tokens,
      :capabilities
    ]

    @spec to_map(t()) :: map()
    def to_map(%__MODULE__{} = m) do
      Copilot.Types.compact(%{
        "provider" => m.provider,
        "id" => m.id,
        "modelId" => m.model_id,
        "wireModel" => m.wire_model,
        "name" => m.name,
        "maxPromptTokens" => m.max_prompt_tokens,
        "maxContextWindowTokens" => m.max_context_window_tokens,
        "maxOutputTokens" => m.max_output_tokens,
        "capabilities" => m.capabilities
      })
    end
  end

  defmodule CapiConfig do
    @moduledoc """
    Built-in Copilot API (CAPI) policy (`github.capi`). `enabled: false` keeps a
    BYOK-only model catalog.
    """
    @type t :: %__MODULE__{enabled: boolean() | nil}
    defstruct [:enabled]
  end

  defmodule ShellAuthenticationRequest do
    @moduledoc """
    A password or other answer that a shell command run by the agent asks for
    (`shellAuthentication.request`). `input_kind` is `"secret"` (mask the
    input), `"text"`, or `"confirm"` (answer `"yes"` or cancel). The handler
    returns `{:accept, value}` or `:cancel`.
    """
    @type t :: %__MODULE__{
            session_id: String.t(),
            request_id: String.t(),
            program: String.t(),
            prompt: String.t(),
            input_kind: String.t(),
            command: String.t(),
            cwd: String.t()
          }
    defstruct [:session_id, :request_id, :program, :prompt, :input_kind, :command, :cwd]

    @spec from_map(map()) :: t()
    def from_map(m) when is_map(m) do
      %__MODULE__{
        session_id: m["sessionId"],
        request_id: m["requestId"],
        program: m["program"],
        prompt: m["prompt"],
        input_kind: m["inputKind"],
        command: m["command"],
        cwd: m["cwd"]
      }
    end

    @doc "Encode a handler answer: `{:accept, value}` or anything else (`:cancel`)."
    @spec response_to_wire(term()) :: map()
    def response_to_wire({:accept, value}) when is_binary(value),
      do: %{"action" => "accept", "value" => value}

    def response_to_wire(_cancel), do: %{"action" => "cancel"}
  end

  defmodule StoredMemory do
    @moduledoc """
    A memory the agent asked to store (`memoryStore.store`), after the
    runtime's validation, secret filtering and approval. The `store_memory`
    callback returns `:ok` or `{:error, reason}`.
    """
    @type t :: %__MODULE__{
            session_id: String.t(),
            subject: String.t(),
            fact: String.t(),
            citations: String.t(),
            reason: String.t()
          }
    defstruct [:session_id, :subject, :fact, :citations, :reason]

    @spec from_map(map()) :: t()
    def from_map(m) when is_map(m) do
      %__MODULE__{
        session_id: m["sessionId"],
        subject: m["subject"],
        fact: m["fact"],
        citations: m["citations"],
        reason: m["reason"]
      }
    end
  end

  defmodule NetworkConfig do
    @moduledoc """
    Client network callbacks registered with `network.setProvider`:

      * `resolve_proxy` - `fn %{url: url} -> proxy_list | nil end` (PAC-style,
        for example `"PROXY proxy.example:8080; DIRECT"`; `nil` leaves the
        request to the runtime's own proxy resolution)
      * `authenticate_negotiate` - `fn %{spn: spn} -> base64_token | nil end`
      * `authenticate_basic` - `fn %{proxy: p, realm: r, previous_rejected: b} ->
        %{username: u, password: p} | nil end`

    `scope` is `"connection"` (runtime default) or `"runtime"`. Tokens and
    passwords are never logged.
    """
    @type t :: %__MODULE__{
            scope: String.t() | atom() | nil,
            resolve_proxy: (map() -> String.t() | nil) | nil,
            authenticate_negotiate: (map() -> String.t() | nil) | nil,
            authenticate_basic: (map() -> map() | nil) | nil
          }
    defstruct [:scope, :resolve_proxy, :authenticate_negotiate, :authenticate_basic]

    @doc "The `network.setProvider` params, or `nil` when no callback is set."
    @spec set_provider_params(t() | nil) :: map() | nil
    def set_provider_params(nil), do: nil

    def set_provider_params(%__MODULE__{} = n) do
      markers =
        [
          {"resolveProxy", n.resolve_proxy},
          {"authenticateNegotiate", n.authenticate_negotiate},
          {"authenticateBasic", n.authenticate_basic}
        ]
        |> Enum.filter(fn {_key, callback} -> is_function(callback, 1) end)
        |> Map.new(fn {key, _callback} -> {key, %{}} end)

      cond do
        map_size(markers) == 0 -> nil
        n.scope == nil -> markers
        true -> Map.put(markers, "scope", to_string(n.scope))
      end
    end
  end

  defmodule ToolShellOutput do
    @moduledoc """
    Data of a `tool.shell_output` event: one chunk of live shell output.
    `stream` is `"stdout"` (also when omitted), `"stderr"`, or `"terminal"`.
    """
    @type t :: %__MODULE__{
            tool_call_id: String.t(),
            text: String.t(),
            sequence: non_neg_integer(),
            stream: String.t() | nil
          }
    defstruct [:tool_call_id, :text, :sequence, :stream]

    @doc "Decode the `data` of a `tool.shell_output` event (or the whole event)."
    @spec from_map(map()) :: t()
    def from_map(%{"type" => "tool.shell_output", "data" => data}) when is_map(data),
      do: from_map(data)

    def from_map(m) when is_map(m) do
      %__MODULE__{
        tool_call_id: m["toolCallId"],
        text: m["text"],
        sequence: m["sequence"],
        stream: m["stream"]
      }
    end
  end

  defmodule SessionEventType do
    @moduledoc "Session event type identifiers. Unknown event types are delivered unchanged."
    def assistant_message, do: "assistant.message"
    def assistant_message_delta, do: "assistant.message_delta"
    def session_idle, do: "session.idle"
    def session_error, do: "session.error"
    def tool_execution_start, do: "tool.execution_start"
    def tool_execution_complete, do: "tool.execution_complete"
    @doc "Removed in SDK protocol 4; still delivered if an older runtime sends it."
    def tool_execution_partial_result, do: "tool.execution_partial_result"
    @doc "Live, append-only shell output (ephemeral)."
    def tool_shell_output, do: "tool.shell_output"
  end

  defmodule PolicyHelperFailure do
    @moduledoc """
    The latest unresolved managed-settings policy helper failure
    (`policyHelperFailure`), reported in the JSON-RPC error data of a refused
    `session.create` / `session.resume` and on managed settings snapshots.
    Helper-authored text is untrusted: render it as plain text.
    """
    @type t :: %__MODULE__{
            category: String.t(),
            failure_mode: String.t(),
            message: String.t(),
            previous_policy_retained: boolean(),
            reason: String.t(),
            authority: String.t() | nil,
            code: String.t() | nil,
            details: [map()] | nil,
            diagnostic: String.t() | nil,
            help_link: map() | nil
          }
    defstruct [
      :category,
      :failure_mode,
      :message,
      :reason,
      :authority,
      :code,
      :details,
      :diagnostic,
      :help_link,
      previous_policy_retained: false
    ]

    @doc """
    Extract the failure from a `{:error, rpc_error}` result, the RPC error map,
    its `"data"`, or a managed settings snapshot. Returns `nil` when absent.
    """
    @spec from_error(term()) :: t() | nil
    def from_error({:error, error}), do: from_error(error)
    def from_error(%{"data" => data}) when is_map(data), do: from_error(data)

    def from_error(%{"policyHelperFailure" => f}) when is_map(f) do
      %__MODULE__{
        category: f["category"],
        failure_mode: f["failureMode"],
        message: f["message"],
        previous_policy_retained: f["previousPolicyRetained"] == true,
        reason: f["reason"],
        authority: f["authority"],
        code: f["code"],
        details: f["details"],
        diagnostic: f["diagnostic"],
        help_link: f["helpLink"]
      }
    end

    def from_error(_other), do: nil
  end

  defmodule PermissionModeRefusal do
    @moduledoc "Known refusal reasons of `session.permissions.setMode`."
    @doc ~S|Managed (enterprise) policy does not allow the requested mode: `"managed_policy"`.|
    @spec managed_policy() :: String.t()
    def managed_policy, do: "managed_policy"
  end

  defmodule PermissionsSetModeResult do
    @moduledoc """
    Result of `Copilot.Session.set_permission_mode/2`. A refused change has
    `success: false` and, when known, `reason: "managed_policy"`.
    """
    @type t :: %__MODULE__{success: boolean(), mode: String.t() | nil, reason: String.t() | nil}
    defstruct [:mode, :reason, success: false]

    @spec from_map(map()) :: t()
    def from_map(m) when is_map(m) do
      %__MODULE__{success: m["success"] == true, mode: m["mode"], reason: m["reason"]}
    end
  end

  @doc false
  @spec compact(map()) :: map()
  def compact(map) when is_map(map) do
    map |> Enum.reject(fn {_key, value} -> is_nil(value) end) |> Map.new()
  end

  # ---------------------------------------------------------------------------
  # Commands
  # ---------------------------------------------------------------------------

  @typedoc "Command handler callback."
  @type command_handler :: (CommandContext.t() -> :ok | {:error, any()})

  defmodule CommandContext do
    @moduledoc "Context for a slash-command invocation."
    @type t :: %__MODULE__{
            session_id: String.t(),
            command: String.t(),
            command_name: String.t(),
            args: String.t()
          }
    defstruct [:session_id, :command, :command_name, args: ""]
  end

  defmodule CommandDefinition do
    @moduledoc "Definition of a slash command registered with the session."
    @type t :: %__MODULE__{
            name: String.t(),
            description: String.t() | nil,
            handler: Copilot.Types.command_handler()
          }
    defstruct [:name, :description, :handler]
  end

  # ---------------------------------------------------------------------------
  # UI Elicitation
  # ---------------------------------------------------------------------------

  @typedoc "Elicitation handler callback."
  @type elicitation_handler ::
          (ElicitationContext.t() -> ElicitationResult.t() | {:error, any()})

  defmodule ElicitationContext do
    @moduledoc "Context for an elicitation request from the server."
    @type t :: %__MODULE__{
            session_id: String.t(),
            message: String.t(),
            requested_schema: map() | nil,
            mode: String.t() | nil,
            elicitation_source: String.t() | nil,
            url: String.t() | nil
          }
    defstruct [:session_id, :message, :requested_schema, :mode, :elicitation_source, :url]
  end

  defmodule ElicitationResult do
    @moduledoc "Result returned from an elicitation handler."
    @type t :: %__MODULE__{
            action: String.t(),
            content: map() | nil
          }
    defstruct [:action, :content]
  end

  # ---------------------------------------------------------------------------
  # Session Configuration
  # ---------------------------------------------------------------------------

  defmodule SessionConfig do
    @moduledoc "Configuration for creating a session."
    @type t :: %__MODULE__{
            session_id: String.t() | nil,
            model: String.t() | nil,
            reasoning_effort: String.t() | nil,
            config_dir: String.t() | nil,
            tools: [Copilot.Types.Tool.t()] | nil,
            system_message: Copilot.Types.system_message_config() | nil,
            available_tools: [String.t()] | nil,
            excluded_tools: [String.t()] | nil,
            provider: Copilot.Types.ProviderConfig.t() | nil,
            on_permission_request: Copilot.Types.permission_handler() | nil,
            on_user_input_request: Copilot.Types.user_input_handler() | nil,
            hooks: Copilot.Types.SessionHooks.t() | nil,
            working_directory: String.t() | nil,
            streaming: boolean() | nil,
            mcp_servers: map() | nil,
            custom_agents: [Copilot.Types.CustomAgentConfig.t()] | nil,
            skill_directories: [String.t()] | nil,
            disabled_skills: [String.t()] | nil,
            infinite_sessions: Copilot.Types.InfiniteSessionConfig.t() | nil,
            model_capabilities: map() | nil,
            enable_config_discovery: boolean() | nil,
            include_sub_agent_streaming_events: boolean() | nil,
            github_token: String.t() | nil,
            commands: [Copilot.Types.CommandDefinition.t()] | nil,
            on_elicitation_request: Copilot.Types.elicitation_handler() | nil,
            session_fs: Copilot.Types.SessionFsConfig.t() | nil,
            instruction_directories: [String.t()] | nil,
            enable_citations: boolean() | nil,
            excluded_builtin_agents: [String.t()] | nil,
            session_limits: Copilot.Types.SessionLimitsConfig.t() | nil,
            memory: Copilot.Types.MemoryConfiguration.t() | nil,
            otlp_protocol: String.t() | nil,
            enable_web_socket_responses: boolean() | nil,
            exp_assignments: map() | nil,
            on_mcp_auth_request: Copilot.Types.mcp_auth_handler() | nil,
            rewind_enabled: boolean() | nil,
            additional_directories: [String.t()] | nil,
            disabled_mcp_servers: [String.t()] | nil,
            github_mcp_tool_config: map() | nil,
            canvas_provider: map() | nil,
            custom_agents_local_only: boolean() | nil,
            tool_search: map() | nil,
            experimental_mode: boolean() | nil,
            content_exclusion: boolean() | nil,
            auto_tier: String.t() | nil,
            ask_user_variant: String.t() | nil,
            github_token_provider: (map() -> Copilot.Types.GitHubToken.t() | :cancelled | map()) | nil,
            sandbox_config: Copilot.Types.SandboxConfig.t() | map() | nil,
            feature_flags: %{optional(String.t()) => boolean()} | nil,
            allowed_models: [String.t()] | nil,
            auth_client_id_metadata_url: String.t() | nil,
            name: String.t() | nil,
            agent_directories: [String.t()] | nil,
            hook_directories: [String.t()] | nil,
            interactive_user: boolean() | nil,
            excluded_managed_settings: [String.t()] | nil,
            enforce_managed_model_defaults: boolean() | nil,
            trust_working_directory: String.t() | nil,
            refresh_custom_instructions: boolean() | nil,
            image_generation: Copilot.Types.ImageGenerationConfig.t() | map() | nil,
            autopilot: Copilot.Types.AutopilotOptions.t() | map() | nil,
            providers: [Copilot.Types.NamedProviderConfig.t()] | nil,
            models: [Copilot.Types.ProviderModelConfig.t()] | nil,
            default_provider: String.t() | nil,
            capi: Copilot.Types.CapiConfig.t() | nil,
            on_shell_authentication_request:
              (Copilot.Types.ShellAuthenticationRequest.t() -> {:accept, String.t()} | :cancel)
              | nil,
            store_memory: (Copilot.Types.StoredMemory.t() -> :ok | {:error, any()}) | nil
          }
    defstruct [
      :session_id,
      :model,
      :reasoning_effort,
      :config_dir,
      :tools,
      :system_message,
      :available_tools,
      :excluded_tools,
      :provider,
      :on_permission_request,
      :on_user_input_request,
      :hooks,
      :working_directory,
      :streaming,
      :mcp_servers,
      :custom_agents,
      :skill_directories,
      :disabled_skills,
      :infinite_sessions,
      # Model capabilities overrides
      :model_capabilities,
      # Auto-discover MCP server configs (default: false)
      :enable_config_discovery,
      # Include sub-agent streaming events (default: true)
      :include_sub_agent_streaming_events,
      # GitHub token for authentication. Overrides client-level token for this session only.
      :github_token,
      # Slash commands registered for this session
      :commands,
      # Handler for elicitation requests from the server
      :on_elicitation_request,
      # Custom session filesystem provider configuration
      :session_fs,
      # Directories to search for instruction files
      :instruction_directories,
      # Emit source citations for grounded responses
      :enable_citations,
      # Built-in agents to exclude from this session
      :excluded_builtin_agents,
      # Per-session AI-credit spend limits
      :session_limits,
      # Opt-in persistent session memory
      :memory,
      # OTLP telemetry protocol ("grpc" or "http/protobuf")
      :otlp_protocol,
      # Stream model responses over a WebSocket transport
      :enable_web_socket_responses,
      # Experiment assignment overrides
      :exp_assignments,
      # MCP OAuth host token handler
      :on_mcp_auth_request,
      # Enable session rewind (rewind to a prior checkpoint)
      :rewind_enabled,
      # Additional workspace directories the session may access
      :additional_directories,
      # MCP servers to disable for this session
      :disabled_mcp_servers,
      # Configuration for the built-in GitHub MCP tool
      :github_mcp_tool_config,
      # Canvas provider configuration
      :canvas_provider,
      # Restrict custom agents to local-only definitions
      :custom_agents_local_only,
      # Tool search configuration
      :tool_search,
      # Enable experimental mode
      :experimental_mode,
      # Enable content exclusion enforcement
      :content_exclusion,
      # --- v2.6.0 session options ---
      # Auto routing tier ("efficiency" | "balance" | "intelligence" | "fast"), sent in github.capi
      :auto_tier,
      # Ask-user surface: "legacy" or "elicitation"
      :ask_user_variant,
      # GitHub token provider (answers gitHubToken.getToken); exclusive with github_token
      :github_token_provider,
      # Sandbox configuration (Copilot.Types.SandboxConfig)
      :sandbox_config,
      # Runtime feature-flag overrides
      :feature_flags,
      # Model allowlist
      :allowed_models,
      # OAuth client-ID metadata document URL for MCP auth
      :auth_client_id_metadata_url,
      # Human-readable session name (create only)
      :name,
      # Directories to load custom agents from
      :agent_directories,
      # Directories to load file-based hooks from
      :hook_directories,
      # Whether a human is interactively driving the session
      :interactive_user,
      # Managed settings to ignore for this session
      :excluded_managed_settings,
      # Enforce managed model defaults
      :enforce_managed_model_defaults,
      # Trust the working directory: "session" or "user" (create only)
      :trust_working_directory,
      # Reload custom instructions when the session starts (create only)
      :refresh_custom_instructions,
      # Image generation opt-in (Copilot.Types.ImageGenerationConfig)
      :image_generation,
      # Autopilot continuation options (Copilot.Types.AutopilotOptions)
      :autopilot,
      # --- v3.0.0 session options (SDK protocol 4) ---
      # Named BYOK provider connections (Copilot.Types.NamedProviderConfig)
      :providers,
      # Model definitions for the named providers (Copilot.Types.ProviderModelConfig)
      :models,
      # Provider connection used for unqualified model names
      :default_provider,
      # Built-in Copilot API policy (Copilot.Types.CapiConfig)
      :capi,
      # Shell authentication handler (shellAuthentication.request)
      :on_shell_authentication_request,
      # Application memory store callback (memoryStore.store)
      :store_memory
    ]
  end

  defmodule ResumeSessionConfig do
    @moduledoc "Configuration for resuming an existing session."
    @type t :: %__MODULE__{
            model: String.t() | nil,
            reasoning_effort: String.t() | nil,
            tools: [Copilot.Types.Tool.t()] | nil,
            system_message: Copilot.Types.system_message_config() | nil,
            available_tools: [String.t()] | nil,
            excluded_tools: [String.t()] | nil,
            provider: Copilot.Types.ProviderConfig.t() | nil,
            on_permission_request: Copilot.Types.permission_handler() | nil,
            on_user_input_request: Copilot.Types.user_input_handler() | nil,
            hooks: Copilot.Types.SessionHooks.t() | nil,
            working_directory: String.t() | nil,
            config_dir: String.t() | nil,
            streaming: boolean() | nil,
            mcp_servers: map() | nil,
            custom_agents: [Copilot.Types.CustomAgentConfig.t()] | nil,
            skill_directories: [String.t()] | nil,
            disabled_skills: [String.t()] | nil,
            infinite_sessions: Copilot.Types.InfiniteSessionConfig.t() | nil,
            disable_resume: boolean() | nil,
            model_capabilities: map() | nil,
            enable_config_discovery: boolean() | nil,
            include_sub_agent_streaming_events: boolean() | nil,
            github_token: String.t() | nil,
            commands: [Copilot.Types.CommandDefinition.t()] | nil,
            on_elicitation_request: Copilot.Types.elicitation_handler() | nil,
            instruction_directories: [String.t()] | nil,
            enable_web_socket_responses: boolean() | nil,
            auto_tier: String.t() | nil,
            ask_user_variant: String.t() | nil,
            github_token_provider: (map() -> Copilot.Types.GitHubToken.t() | :cancelled | map()) | nil,
            sandbox_config: Copilot.Types.SandboxConfig.t() | map() | nil,
            feature_flags: %{optional(String.t()) => boolean()} | nil,
            allowed_models: [String.t()] | nil,
            auth_client_id_metadata_url: String.t() | nil,
            agent_directories: [String.t()] | nil,
            hook_directories: [String.t()] | nil,
            interactive_user: boolean() | nil,
            excluded_managed_settings: [String.t()] | nil,
            enforce_managed_model_defaults: boolean() | nil,
            allow_transcript_recovery: boolean() | nil,
            image_generation: Copilot.Types.ImageGenerationConfig.t() | map() | nil,
            autopilot: Copilot.Types.AutopilotOptions.t() | map() | nil,
            providers: [Copilot.Types.NamedProviderConfig.t()] | nil,
            models: [Copilot.Types.ProviderModelConfig.t()] | nil,
            default_provider: String.t() | :clear | nil,
            capi: Copilot.Types.CapiConfig.t() | nil,
            on_shell_authentication_request:
              (Copilot.Types.ShellAuthenticationRequest.t() -> {:accept, String.t()} | :cancel)
              | nil,
            store_memory: (Copilot.Types.StoredMemory.t() -> :ok | {:error, any()}) | nil
          }
    defstruct [
      :model,
      :reasoning_effort,
      :tools,
      :system_message,
      :available_tools,
      :excluded_tools,
      :provider,
      :on_permission_request,
      :on_user_input_request,
      :hooks,
      :working_directory,
      :config_dir,
      :streaming,
      :mcp_servers,
      :custom_agents,
      :skill_directories,
      :disabled_skills,
      :infinite_sessions,
      :disable_resume,
      # Model capabilities overrides
      :model_capabilities,
      # Auto-discover MCP server configs (default: false)
      :enable_config_discovery,
      # Include sub-agent streaming events (default: true)
      :include_sub_agent_streaming_events,
      # GitHub token for authentication. Overrides client-level token for this session only.
      :github_token,
      # Slash commands registered for this session
      :commands,
      # Handler for elicitation requests from the server
      :on_elicitation_request,
      # Directories to search for instruction files
      :instruction_directories,
      # Stream model responses over a WebSocket transport (sent in github.capi)
      :enable_web_socket_responses,
      # --- v2.6.0 session options ---
      :auto_tier,
      :ask_user_variant,
      :github_token_provider,
      :sandbox_config,
      :feature_flags,
      :allowed_models,
      :auth_client_id_metadata_url,
      :agent_directories,
      :hook_directories,
      :interactive_user,
      :excluded_managed_settings,
      :enforce_managed_model_defaults,
      # Allow recovering the conversation from the transcript (resume only)
      :allow_transcript_recovery,
      :image_generation,
      :autopilot,
      # --- v3.0.0 session options (SDK protocol 4) ---
      :providers,
      :models,
      # Default provider connection; :clear sends null to clear it
      :default_provider,
      :capi,
      :on_shell_authentication_request,
      :store_memory
    ]
  end

  # ---------------------------------------------------------------------------
  # Image Generation / Response Format Types
  # ---------------------------------------------------------------------------

  @typedoc "The response format for a message."
  @type response_format :: :text | :image | :json_object

  @typedoc "Options for image generation."
  @type image_options :: %{
          optional(:size) => String.t(),
          optional(:quality) => String.t(),
          optional(:style) => String.t()
        }

  @typedoc "Image data returned by the assistant."
  @type assistant_image_data :: %{
          format: String.t(),
          base64: String.t(),
          url: String.t() | nil,
          revised_prompt: String.t() | nil,
          width: integer(),
          height: integer()
        }

  @typedoc "A content block in an assistant response (text or image)."
  @type content_block ::
          %{type: :text, text: String.t()}
          | %{type: :image, image: assistant_image_data()}

  @doc "Convert response format atom to JSON string."
  def response_format_to_string(:text), do: "text"
  def response_format_to_string(:image), do: "image"
  def response_format_to_string(:json_object), do: "json_object"

  # ---------------------------------------------------------------------------
  # Message Options
  # ---------------------------------------------------------------------------

  defmodule MessageOptions do
    @moduledoc """
    Options for sending a message to a session.

      * `source` - message provenance: `"user"`, `"system"`, or `"agent-<id>"`
        (see `Copilot.Types.MessageSource`)
      * `response_schema` - JSON Schema for structured output; sent as
        `responseFormat: %{type: "json_schema", jsonSchema: %{name: "response", strict: true, schema: ...}}`
      * `client_turn_id` - opaque caller-chosen turn identifier (`clientTurnId`)
    """
    @type t :: %__MODULE__{
            prompt: String.t(),
            attachments: [map()] | nil,
            mode: String.t() | nil,
            agent_mode: String.t() | nil,
            display_prompt: String.t() | nil,
            response_format: Copilot.Types.response_format() | nil,
            image_options: Copilot.Types.image_options() | nil,
            request_headers: %{optional(String.t()) => String.t()} | nil,
            source: String.t() | nil,
            response_schema: map() | nil,
            client_turn_id: String.t() | nil
          }
    defstruct [
      :prompt,
      :attachments,
      :mode,
      # Agent mode override for this message
      :agent_mode,
      # Prompt text to display in transcripts (may differ from the model prompt)
      :display_prompt,
      :response_format,
      :image_options,
      # Custom HTTP headers for outbound model requests
      :request_headers,
      # Message provenance ("user", "system", or "agent-<id>")
      :source,
      # JSON Schema for structured output (responseFormat json_schema)
      :response_schema,
      # Opaque caller-chosen turn identifier (clientTurnId)
      :client_turn_id
    ]
  end

  # ---------------------------------------------------------------------------
  # Session Filesystem Types
  # ---------------------------------------------------------------------------

  defmodule SessionFsConfig do
    @moduledoc "Configuration for a custom session filesystem provider."
    @type t :: %__MODULE__{
            initial_cwd: String.t(),
            session_state_path: String.t(),
            conventions: String.t()
          }
    defstruct [:initial_cwd, :session_state_path, :conventions]
  end

  defmodule SessionFsFileInfo do
    @moduledoc "File metadata returned by session filesystem operations."
    @type t :: %__MODULE__{
            name: String.t(),
            size: integer(),
            is_directory: boolean(),
            is_file: boolean(),
            created_at: String.t() | nil,
            modified_at: String.t() | nil
          }
    defstruct [:name, :size, :is_directory, :is_file, :created_at, :modified_at]
  end

  defmodule SessionFsProvider do
    @moduledoc "Behaviour for session filesystem providers."
    @callback read_file(session_id :: String.t(), path :: String.t()) :: {:ok, String.t()} | {:error, term()}
    @callback write_file(session_id :: String.t(), path :: String.t(), content :: String.t()) :: :ok | {:error, term()}
    @callback append_file(session_id :: String.t(), path :: String.t(), content :: String.t()) :: :ok | {:error, term()}
    @callback exists?(session_id :: String.t(), path :: String.t()) :: {:ok, boolean()} | {:error, term()}
    @callback stat(session_id :: String.t(), path :: String.t()) :: {:ok, SessionFsFileInfo.t()} | {:error, term()}
    @callback mkdir(session_id :: String.t(), path :: String.t(), recursive :: boolean()) :: :ok | {:error, term()}
    @callback readdir(session_id :: String.t(), path :: String.t()) :: {:ok, [String.t()]} | {:error, term()}
    @callback readdir_with_types(session_id :: String.t(), path :: String.t()) :: {:ok, [SessionFsFileInfo.t()]} | {:error, term()}
    @callback rm(session_id :: String.t(), path :: String.t(), recursive :: boolean()) :: :ok | {:error, term()}
    @callback rename(session_id :: String.t(), old_path :: String.t(), new_path :: String.t()) :: :ok | {:error, term()}
  end

  # ---------------------------------------------------------------------------
  # Client Options
  # ---------------------------------------------------------------------------

  defmodule CopilotClientOptions do
    @moduledoc "Options for creating a CopilotClient."
    @type t :: %__MODULE__{
            cli_path: String.t() | nil,
            cli_args: [String.t()],
            cwd: String.t() | nil,
            log_level: String.t(),
            auto_start: boolean(),
            auto_restart: boolean(),
            env: [{String.t(), String.t()}] | nil,
            github_token: String.t() | nil,
            use_logged_in_user: boolean() | nil,
            session_idle_timeout_seconds: integer() | nil,
            session_fs: Copilot.Types.SessionFsConfig.t() | nil,
            copilot_home: String.t() | nil,
            tcp_connection_token: String.t() | nil,
            request_handler: Copilot.Types.copilot_request_handler() | nil,
            bearer_token_provider: Copilot.Types.bearer_token_provider() | nil,
            builtin_plugin_directories: [String.t()] | nil,
            in_process: boolean() | nil,
            client_info: Copilot.Types.ClientInfo.t() | nil,
            mode: String.t() | atom(),
            network: Copilot.Types.NetworkConfig.t() | nil,
            on_get_trace_context: (-> map()) | nil
          }
    defstruct [
      cli_path: nil,
      cli_args: [],
      cwd: nil,
      log_level: "info",
      auto_start: true,
      auto_restart: true,
      env: nil,
      github_token: nil,
      use_logged_in_user: nil,
      # Server-wide idle timeout for sessions in seconds
      session_idle_timeout_seconds: nil,
      # Custom session filesystem provider configuration
      session_fs: nil,
      # Custom path to the copilot home directory
      copilot_home: nil,
      # Token for TCP connections
      tcp_connection_token: nil,
      # Intercept outbound LLM inference requests
      request_handler: nil,
      # BYOK bearer-token provider (per-session)
      bearer_token_provider: nil,
      # Directories to search for built-in plugins (wire: "builtinPluginDirectories")
      builtin_plugin_directories: nil,
      # Use in-process FFI transport instead of spawning a CLI (wire: "inProcess")
      in_process: nil,
      # Application identity sent on the `connect` handshake (clientInfo)
      client_info: nil,
      # "coding-agent" (default) or "empty" (see Copilot.Types.ClientMode)
      mode: "coding-agent",
      # Network callbacks registered with network.setProvider
      network: nil,
      # Returns %{traceparent: ..., tracestate: ...} for W3C trace propagation
      on_get_trace_context: nil
    ]
  end

  # ---------------------------------------------------------------------------
  # Ping / Status / Auth Responses
  # ---------------------------------------------------------------------------

  defmodule PingResponse do
    @moduledoc "Response from ping."
    @type t :: %__MODULE__{
            message: String.t(),
            timestamp: integer(),
            protocol_version: integer() | nil
          }
    defstruct [:message, :timestamp, :protocol_version]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        message: m["message"],
        timestamp: m["timestamp"],
        protocol_version: m["protocolVersion"]
      }
    end
  end

  defmodule GetStatusResponse do
    @moduledoc "Response from status.get."
    @type t :: %__MODULE__{version: String.t(), protocol_version: integer()}
    defstruct [:version, :protocol_version]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        version: m["version"],
        protocol_version: m["protocolVersion"]
      }
    end
  end

  defmodule GetAuthStatusResponse do
    @moduledoc "Response from auth.getStatus."
    @type t :: %__MODULE__{
            is_authenticated: boolean(),
            auth_type: String.t() | nil,
            host: String.t() | nil,
            login: String.t() | nil,
            status_message: String.t() | nil
          }
    defstruct [:is_authenticated, :auth_type, :host, :login, :status_message]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        is_authenticated: m["isAuthenticated"],
        auth_type: m["authType"],
        host: m["host"],
        login: m["login"],
        status_message: m["statusMessage"]
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Model Types
  # ---------------------------------------------------------------------------

  defmodule ModelVisionLimits do
    @moduledoc "Vision-specific limits."
    @type t :: %__MODULE__{
            supported_media_types: [String.t()] | nil,
            max_prompt_images: integer() | nil,
            max_prompt_image_size: integer() | nil
          }
    defstruct [:supported_media_types, :max_prompt_images, :max_prompt_image_size]

    def from_map(nil), do: nil

    def from_map(m) when is_map(m) do
      %__MODULE__{
        supported_media_types: m["supported_media_types"],
        max_prompt_images: m["max_prompt_images"],
        max_prompt_image_size: m["max_prompt_image_size"]
      }
    end
  end

  defmodule ModelLimits do
    @moduledoc """
    Model limits. To override the output limit for a session, set
    `model_capabilities: %{"limits" => %{"max_output_tokens" => n}}` (the wire
    keys inside capability limits are snake_case).
    """
    @type t :: %__MODULE__{
            max_prompt_tokens: integer() | nil,
            max_context_window_tokens: integer() | nil,
            max_output_tokens: integer() | nil,
            vision: ModelVisionLimits.t() | nil
          }
    defstruct [:max_prompt_tokens, :max_context_window_tokens, :max_output_tokens, :vision]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        max_prompt_tokens: m["max_prompt_tokens"],
        max_context_window_tokens: m["max_context_window_tokens"],
        max_output_tokens: m["max_output_tokens"],
        vision: ModelVisionLimits.from_map(m["vision"])
      }
    end
  end

  defmodule ModelSupports do
    @moduledoc "Model support flags."
    @type t :: %__MODULE__{vision: boolean(), reasoning_effort: boolean()}
    defstruct [:vision, reasoning_effort: false]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        vision: m["vision"],
        reasoning_effort: Map.get(m, "reasoningEffort", false)
      }
    end
  end

  defmodule ModelCapabilities do
    @moduledoc "Model capabilities and limits."
    @type t :: %__MODULE__{supports: ModelSupports.t(), limits: ModelLimits.t()}
    defstruct [:supports, :limits]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        supports: ModelSupports.from_map(m["supports"]),
        limits: ModelLimits.from_map(m["limits"])
      }
    end
  end

  defmodule ModelPolicy do
    @moduledoc "Model policy state."
    @type t :: %__MODULE__{state: String.t(), terms: String.t()}
    defstruct [:state, :terms]

    def from_map(nil), do: nil

    def from_map(m) when is_map(m) do
      %__MODULE__{state: m["state"], terms: m["terms"]}
    end
  end

  defmodule ModelBilling do
    @moduledoc "Model billing information."
    @type t :: %__MODULE__{multiplier: float()}
    defstruct [:multiplier]

    def from_map(nil), do: nil
    def from_map(m) when is_map(m), do: %__MODULE__{multiplier: m["multiplier"]}
  end

  defmodule ModelInfo do
    @moduledoc "Information about an available model."
    @type t :: %__MODULE__{
            id: String.t(),
            name: String.t(),
            capabilities: ModelCapabilities.t(),
            policy: ModelPolicy.t() | nil,
            billing: ModelBilling.t() | nil,
            supported_reasoning_efforts: [String.t()] | nil,
            default_reasoning_effort: String.t() | nil
          }
    defstruct [
      :id,
      :name,
      :capabilities,
      :policy,
      :billing,
      :supported_reasoning_efforts,
      :default_reasoning_effort
    ]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        id: m["id"],
        name: m["name"],
        capabilities: ModelCapabilities.from_map(m["capabilities"]),
        policy: ModelPolicy.from_map(m["policy"]),
        billing: ModelBilling.from_map(m["billing"]),
        supported_reasoning_efforts: m["supportedReasoningEfforts"],
        default_reasoning_effort: m["defaultReasoningEffort"]
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Session Metadata
  # ---------------------------------------------------------------------------

  defmodule SessionMetadata do
    @moduledoc "Metadata about a session."
    @type t :: %__MODULE__{
            session_id: String.t(),
            start_time: String.t(),
            modified_time: String.t(),
            is_remote: boolean(),
            summary: String.t() | nil
          }
    defstruct [:session_id, :start_time, :modified_time, :is_remote, :summary]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        session_id: m["sessionId"],
        start_time: m["startTime"],
        modified_time: m["modifiedTime"],
        is_remote: m["isRemote"],
        summary: m["summary"]
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Session Lifecycle Types
  # ---------------------------------------------------------------------------

  defmodule SessionLifecycleEvent do
    @moduledoc "Session lifecycle event notification."
    @type t :: %__MODULE__{
            type: String.t(),
            session_id: String.t(),
            metadata: map() | nil
          }
    defstruct [:type, :session_id, :metadata]

    def from_map(m) when is_map(m) do
      %__MODULE__{
        type: m["type"],
        session_id: m["sessionId"],
        metadata: m["metadata"]
      }
    end
  end
end
