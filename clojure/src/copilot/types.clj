;;;; ---------------------------------------------------------------------------
;;;;  Copyright (c) Microsoft Corporation. All rights reserved.
;;;; ---------------------------------------------------------------------------
;;;;
;;;; Type definitions for the Copilot SDK (Clojure).
;;;;
;;;; In idiomatic Clojure we represent most "types" as plain maps with
;;;; well-known keyword keys.  This namespace documents the canonical shapes,
;;;; provides constructor helpers, and defines the small number of records
;;;; that benefit from positional construction.

(ns copilot.types
  "Canonical data shapes used throughout the Copilot Clojure SDK.

  All maps use kebab-case keywords internally.  Conversion to/from
  camelCase JSON happens at the transport boundary (see copilot.json-rpc).")

;; ============================================================================
;; Tool types
;; ============================================================================

(defn tool-result
  "Construct a normalised tool result map.

  `text`        - string result for the LLM
  `result-type` - one of :success :failure :rejected :denied
  `opts`        - optional map of :error :session-log :tool-telemetry
                  :binary-results-for-llm"
  ([text result-type]
   (tool-result text result-type nil))
  ([text result-type opts]
   (merge {:textResultForLlm text
           :resultType       (name result-type)
           :toolTelemetry    {}}
          (when-let [e (:error opts)]             {:error e})
          (when-let [s (:session-log opts)]       {:sessionLog s})
          (when-let [t (:tool-telemetry opts)]    {:toolTelemetry t})
          (when-let [b (:binary-results-for-llm opts)] {:binaryResultsForLlm b}))))

(defn tool-binary-result
  "Construct a binary result entry.

  `data`      - base64-encoded string
  `mime-type` - MIME type string
  `type`      - result type string
  `desc`      - optional description"
  ([data mime-type type]
   (tool-binary-result data mime-type type nil))
  ([data mime-type type desc]
   (cond-> {:data data :mimeType mime-type :type type}
     desc (assoc :description desc))))

(defn tool
  "Construct a tool definition.

  `name`        - tool name string
  `description` - description for the LLM
  `parameters`  - JSON Schema map (or nil)
  `handler`     - (fn [args invocation] ...) -> any  (may return a promise)
  Optional: :idempotent true marks the tool as safe to retry (repeated calls
            with the same arguments have no additional side effects)."
  [name description parameters handler & {:keys [idempotent]}]
  (cond-> {:name        name
           :description description
           :parameters  parameters
           :handler     handler}
    (some? idempotent) (assoc :idempotent idempotent)))

(defn tool-invocation
  "Construct a tool invocation context map."
  [session-id tool-call-id tool-name arguments]
  {:session-id   session-id
   :tool-call-id tool-call-id
   :tool-name    tool-name
   :arguments    arguments})

;; ============================================================================
;; Commands
;; ============================================================================

(defn command-context
  "Construct a command context map."
  [session-id command command-name args]
  {:session-id   session-id
   :command      command
   :command-name command-name
   :args         args})

(defn command-definition
  "Define a slash command.  `handler` is a function of one arg (command-context map)."
  ([name handler] {:name name :handler handler})
  ([name description handler]
   {:name name :description description :handler handler}))

;; ============================================================================
;; UI Elicitation
;; ============================================================================

(defn elicitation-context
  "Construct an elicitation context map."
  [session-id message & {:keys [requested-schema mode elicitation-source url]}]
  (cond-> {:session-id session-id :message message}
    requested-schema  (assoc :requested-schema requested-schema)
    mode              (assoc :mode mode)
    elicitation-source (assoc :elicitation-source elicitation-source)
    url               (assoc :url url)))

(defn elicitation-result
  "Construct an elicitation result map.
  `action` is one of \"accept\", \"decline\", \"cancel\".
  `content` is an optional map of form values."
  ([action] {:action action})
  ([action content] {:action action :content content}))

;; ============================================================================
;; Session configuration
;; ============================================================================

(defn session-config
  "Create a session configuration map.  All keys are optional.

  Recognised keys (kebab-case):
    :session-id :model :reasoning-effort :config-dir
    :tools :system-message :available-tools :excluded-tools
    :provider :on-permission-request :on-user-input-request :hooks
    :working-directory :streaming :mcp-servers :custom-agents
    :skill-directories :disabled-skills :infinite-sessions
    :model-capabilities :enable-config-discovery
    :include-sub-agent-streaming-events
    :github-token :commands :on-elicitation-request
    :session-fs (session filesystem provider map)
    :instruction-directories (list of directories to search for instruction files)
    :enable-citations :excluded-builtin-agents :session-limits :memory-config
    :otlp-protocol :enable-web-socket-responses :exp-assignments
    :rewind-enabled :additional-directories :disabled-mcp-servers
    :github-mcp-tool-config :canvas-provider :custom-agents-local-only
    :tool-search :experimental-mode :content-exclusion
    :on-mcp-auth-request (fn invoked to supply an MCP OAuth host token)

  v2.6.0 / v3.0.0 keys:
    :ask-user-variant (\"legacy\" or \"elicitation\")
    :github-token-provider (fn [request] -> token string, token map, or
                            :cancelled; mutually exclusive with :github-token)
    :sandbox-config (see `sandbox-config`, includes :allow-bypass)
    :feature-flags (map of flag name -> value)
    :allowed-models (vector of model ids)
    :auth-client-id-metadata-url (string)
    :agent-directories :hook-directories (vectors of paths)
    :trust-working-directory (create only: \"session\" trusts the working
                              directory for this session, \"user\" persists it)
    :allow-transcript-recovery (resume only)
    :image-generation (see `image-generation-config`)
    :autopilot (see `autopilot-options`)
    :providers (see `named-provider-config`) :models (see `provider-model-config`)
    :default-provider (string; on resume an explicit nil clears it)
    :capi (see `capi-config`)
    :on-shell-authentication-request (fn [request] -> value string or nil)
    :store-memory (fn [memory] ...)"
  [& {:as opts}]
  (or opts {}))

(defn resume-session-config
  "Create a resume-session configuration map.  Accepts the same keys as
  `session-config` plus :disable-resume, :github-token,
  :commands, :on-elicitation-request, :instruction-directories, and
  :allow-transcript-recovery.  :trust-working-directory is create-only and is
  not sent on resume."
  [& {:as opts}]
  (or opts {}))

;; ============================================================================
;; System message configuration
;; ============================================================================

(defn system-message-append
  "Append-mode system message (default). Content is appended after SDK sections."
  ([] {:mode "append"})
  ([content] {:mode "append" :content content}))

(defn system-message-replace
  "Replace-mode system message. Caller provides entire system message."
  [content]
  {:mode "replace" :content content})

(defn system-message-customize
  "Customize-mode system message. Override individual sections."
  ([] {:mode "customize"})
  ([sections] {:mode "customize" :sections sections})
  ([sections content] {:mode "customize" :sections sections :content content}))

(defn section-override
  "Create a section override.
  `action` is one of :replace :remove :append :prepend.
  `content` is optional string."
  ([action] {:action (name action)})
  ([action content] {:action (name action) :content content}))

;; System prompt section constants
(def system-prompt-sections
  {:preamble            "preamble"
   :identity            "identity"
   :tone                "tone"
   :tool-efficiency     "tool_efficiency"
   :environment-context "environment_context"
   :code-change-rules   "code_change_rules"
   :guidelines          "guidelines"
   :safety              "safety"
   :tool-instructions   "tool_instructions"
   :custom-instructions "custom_instructions"
   :last-instructions   "last_instructions"})

;; Section override action constants
(def section-override-actions
  {:replace  "replace"
   :remove   "remove"
   :append   "append"
   :prepend  "prepend"
   :preserve "preserve"})

;; ============================================================================
;; Permission types
;; ============================================================================

(defn permission-request
  "Construct a permission request map.
  `kind` is one of :shell :write :mcp :read :url."
  [kind & {:as extra}]
  (merge {:kind (name kind)} extra))

(defn permission-result
  "Construct a permission result map.
  `kind` is one of :approved :denied-by-rules
   :denied-no-approval-rule-and-could-not-request-from-user
   :denied-interactively-by-user
  Optional :decision-context (opaque map emitted as decisionContext)."
  [kind & {:as extra}]
  (let [decision-context (:decision-context extra)
        extra            (dissoc extra :decision-context)]
    (cond-> (merge {:kind (name kind)} extra)
      decision-context (assoc :decisionContext decision-context))))

;; ============================================================================
;; User input types
;; ============================================================================

(defn user-input-request
  "Construct a user input request.
  `question` - string
  Optional: :choices (vec of strings), :allow-freeform (bool)"
  [question & {:as opts}]
  (merge {:question question}
         (when-let [c (:choices opts)]        {:choices c})
         (when-let [a (:allow-freeform opts)] {:allowFreeform a})))

(defn user-input-response
  "Construct a user input response.
  `answer`       - string
  `was-freeform` - boolean"
  [answer was-freeform]
  {:answer answer :wasFreeform was-freeform})

;; ============================================================================
;; Canvas / cloud session types
;; ============================================================================

(defn canvas-action
  "Create a canvas action.

  `name` and `description` are required.
  Optional: :input-schema (JSON-schema map)."
  [name description & {:as opts}]
  (cond-> {:name name :description description}
    (:input-schema opts) (assoc :inputSchema (:input-schema opts))))

(defn canvas-declaration
  "Create a canvas declaration.

  Required: `id`, `display-name`, `description`.
  Optional: :input-schema, :actions (vector of canvas-action maps)."
  [id display-name description & {:as opts}]
  (cond-> {:id id :displayName display-name :description description}
    (:input-schema opts) (assoc :inputSchema (:input-schema opts))
    (:actions opts)      (assoc :actions (:actions opts))))

(defn canvas-open-response
  "Create a canvas-open response.
  Optional keys: :url :title :status"
  [& {:as opts}]
  (cond-> {}
    (:url opts)    (assoc :url (:url opts))
    (:title opts)  (assoc :title (:title opts))
    (:status opts) (assoc :status (:status opts))))

(defn canvas-host-capabilities
  "Create a canvas-host-capabilities map.
  Optional: :canvases (defaults to false)."
  [& {:as opts}]
  (if (contains? opts :canvases)
    {:canvases (:canvases opts)}
    {:canvases false}))

(defn canvas-host-context
  "Create a canvas-host-context map. `capabilities` is required."
  [capabilities]
  {:capabilities capabilities})

(defn canvas-open-context
  "Create a canvas-open-context map.
  Optional: :host"
  [session-id extension-id canvas-id instance-id input & {:as opts}]
  (cond-> {:sessionId session-id
           :extensionId extension-id
           :canvasId canvas-id
           :instanceId instance-id
           :input input}
    (:host opts) (assoc :host (:host opts))))

(defn canvas-action-context
  "Create a canvas-action-context map.
  Optional: :host"
  [session-id extension-id canvas-id instance-id action-name input & {:as opts}]
  (cond-> {:sessionId session-id
           :extensionId extension-id
           :canvasId canvas-id
           :instanceId instance-id
           :actionName action-name
           :input input}
    (:host opts) (assoc :host (:host opts))))

(defn canvas-lifecycle-context
  "Create a canvas-lifecycle-context map.
  Optional: :host"
  [session-id extension-id canvas-id instance-id & {:as opts}]
  (cond-> {:sessionId session-id
           :extensionId extension-id
           :canvasId canvas-id
           :instanceId instance-id}
    (:host opts) (assoc :host (:host opts))))

(defn cloud-session-repository
  "Create cloud-session repository metadata.
  Optional: :branch"
  [owner name & {:as opts}]
  (cond-> {:owner owner :name name}
    (:branch opts) (assoc :branch (:branch opts))))

(defn cloud-session-options
  "Create cloud-session options.
  Optional: :repository"
  [& {:as opts}]
  (cond-> {}
    (:repository opts) (assoc :repository (:repository opts))))

;; ============================================================================
;; Provider configuration
;; ============================================================================

(defn provider-config
  "Create a provider configuration map for BYOK (Bring Your Own Key).

  Required: :base-url
  Optional: :type (:openai :azure :anthropic), :wire-api, :api-key,
            :bearer-token, :azure {:api-version ...},
            :api-key-command (command whose stdout is used as the API key),
            :model-provider (\"openai\" \"anthropic\" \"azure_openai\" \"ollama\"
                             \"lm_studio\" \"foundry_local\" \"llama_cpp\"),
            :transport, :headers

  On SDK protocol 4 this legacy single provider is sent as the named
  \"default\" connection (`inference.providers`), selected with
  `inference.defaultProvider`, with Copilot API models disabled
  (`github.capi.enabled` false), which preserves v1 behavior."
  [& {:as opts}]
  (let [{:keys [base-url type wire-api api-key bearer-token azure
                api-key-command model-provider transport headers]} opts]
    (cond-> {:baseUrl base-url}
      type            (assoc :type (name type))
      wire-api        (assoc :wireApi (name wire-api))
      api-key         (assoc :apiKey api-key)
      bearer-token    (assoc :bearerToken bearer-token)
      azure           (assoc :azure azure)
      api-key-command (assoc :apiKeyCommand api-key-command)
      model-provider  (assoc :modelProvider (name model-provider))
      transport       (assoc :transport (name transport))
      headers         (assoc :headers headers))))

;; ============================================================================
;; MCP server configuration
;; ============================================================================

(defn mcp-local-server-config
  "Create a local/stdio MCP server configuration.

  `command` - command string
  `args`    - vector of argument strings
  `tools`   - vector of tool name strings (or [\"*\"] for all)
  Optional: :type :timeout :env :cwd"
  [command args tools & {:as opts}]
  (cond-> {:command command :args args :tools tools}
    (:type opts)    (assoc :type (name (:type opts)))
    (:timeout opts) (assoc :timeout (:timeout opts))
    (:env opts)     (assoc :env (:env opts))
    (:cwd opts)     (assoc :cwd (:cwd opts))))

(defn mcp-remote-server-config
  "Create a remote MCP server configuration.

  `type`  - :http or :sse
  `url`   - remote server URL
  `tools` - vector of tool name strings
  Optional: :timeout :headers"
  [type url tools & {:as opts}]
  (cond-> {:type (name type) :url url :tools tools}
    (:timeout opts) (assoc :timeout (:timeout opts))
    (:headers opts) (assoc :headers (:headers opts))))

;; ============================================================================
;; Custom agent configuration
;; ============================================================================

(defn custom-agent-config
  "Create a custom agent configuration.

  `name`   - unique agent name
  `prompt` - agent prompt content
  Optional: :display-name :description :tools :mcp-servers :infer
            :skills (vector of skill name strings to preload)
            :args-schema (opaque map emitted as argsSchema)"
  [name prompt & {:as opts}]
  (cond-> {:name name :prompt prompt}
    (:display-name opts) (assoc :displayName (:display-name opts))
    (:description opts)  (assoc :description (:description opts))
    (contains? opts :tools) (assoc :tools (:tools opts))
    (:mcp-servers opts)  (assoc :mcpServers (:mcp-servers opts))
    (contains? opts :infer) (assoc :infer (:infer opts))
    (:skills opts)       (assoc :skills (:skills opts))
    (:args-schema opts)  (assoc :argsSchema (:args-schema opts))))

;; ============================================================================
;; Infinite session configuration
;; ============================================================================

(defn infinite-session-config
  "Create an infinite session configuration.

  Optional: :enabled (default true),
            :background-compaction-threshold (0.0-1.0, default 0.80),
            :buffer-exhaustion-threshold (0.0-1.0, default 0.95)"
  [& {:as opts}]
  (let [{:keys [enabled background-compaction-threshold
                buffer-exhaustion-threshold]} opts]
    (cond-> {}
      (some? enabled) (assoc :enabled enabled)
      background-compaction-threshold
      (assoc :backgroundCompactionThreshold background-compaction-threshold)
      buffer-exhaustion-threshold
      (assoc :bufferExhaustionThreshold buffer-exhaustion-threshold))))

;; ============================================================================
;; Ping response
;; ============================================================================

(defn ping-response
  "Parse a raw ping response map."
  [m]
  {:message          (:message m)
   :timestamp        (:timestamp m)
   :protocol-version (:protocolVersion m)})

;; ============================================================================
;; Model information
;; ============================================================================

(defn model-info
  "Parse a raw model info map from JSON-RPC response."
  [m]
  {:id                          (:id m)
   :name                        (:name m)
   :capabilities                (:capabilities m)
   :policy                      (:policy m)
   :billing                     (:billing m)
   :supported-reasoning-efforts (:supportedReasoningEfforts m)
   :default-reasoning-effort    (:defaultReasoningEffort m)})

;; ============================================================================
;; Session metadata
;; ============================================================================

(defn session-metadata
  "Parse a raw session metadata map."
  [m]
  {:session-id    (:sessionId m)
   :start-time    (:startTime m)
   :modified-time (:modifiedTime m)
   :summary       (:summary m)
   :is-remote     (:isRemote m)})

;; ============================================================================
;; Session lifecycle events
;; ============================================================================

(defn session-lifecycle-event
  "Parse a raw session lifecycle event from JSON-RPC notification."
  [m]
  {:type       (:type m)
   :session-id (:sessionId m)
   :metadata   (:metadata m)})

;; ============================================================================
;; Response format & image generation
;; ============================================================================

(def response-formats #{:text :image :json-object})

(defn response-format->str
  "Convert a response format keyword to its JSON string representation."
  [fmt]
  (case fmt
    :text "text"
    :image "image"
    :json-object "json_object"
    (name fmt)))

(defn image-options
  "Create image options map for image generation.
   Options: :size (e.g. \"1024x1024\"), :quality (\"hd\" or \"standard\"), :style (\"natural\" or \"vivid\")"
  [& {:keys [size quality style]}]
  (cond-> {}
    size    (assoc :size size)
    quality (assoc :quality quality)
    style   (assoc :style style)))

(defn parse-assistant-image-data
  "Parse assistant image data from a response map."
  [data]
  {:format         (:format data)
   :base64         (:base64 data)
   :url            (:url data)
   :revised-prompt (:revisedPrompt data)
   :width          (:width data)
   :height         (:height data)})

(defn parse-content-block
  "Parse a content block from a mixed text+image response."
  [block]
  (case (:type block)
    "text"  {:type :text :text (:text block)}
    "image" {:type :image :image (parse-assistant-image-data (:image block))}
    block))

;; ============================================================================
;; Message options
;; ============================================================================

(defn message-options
  "Construct message options for sending a message.

  `prompt` - the message string
  Optional: :attachments (vector of attachment maps), :mode (:enqueue or :immediate),
            :response-format (keyword from response-formats), :image-options (image options map),
            :agent-mode (string agent name to route the message to),
            :display-prompt (string shown in the UI in place of the raw prompt),
            :request-headers (map of string->string custom HTTP headers for outbound model requests),
            :source (message provenance: \"user\", \"system\", or \"agent-<id>\";
                     see `message-source`),
            :response-schema (JSON Schema map for structured output, sent as a
                              `json_schema` responseFormat),
            :client-turn-id (opaque caller-chosen turn identifier, wire `clientTurnId`)"
  [prompt & {:as opts}]
  (cond-> {:prompt prompt}
    (:attachments opts)     (assoc :attachments (:attachments opts))
    (:mode opts)            (assoc :mode (name (:mode opts)))
    (:response-format opts) (assoc :response-format (response-format->str (:response-format opts)))
    (:image-options opts)   (assoc :image-options (:image-options opts))
    (:agent-mode opts)      (assoc :agent-mode (:agent-mode opts))
    (:display-prompt opts)  (assoc :display-prompt (:display-prompt opts))
    (:request-headers opts) (assoc :request-headers (:request-headers opts))
    (:source opts)          (assoc :source (:source opts))
    (:response-schema opts) (assoc :response-schema (:response-schema opts))
    (:client-turn-id opts)  (assoc :client-turn-id (:client-turn-id opts))))

;; ============================================================================
;; Session hooks
;; ============================================================================

(defn session-hooks
  "Create a session hooks map.

  Optional keys:
    :on-pre-tool-use            (fn [input invocation] ...)
    :on-post-tool-use           (fn [input invocation] ...)
    :on-pre-mcp-tool-call       (fn [input invocation] ...)
    :on-user-prompt-submitted   (fn [input invocation] ...)
    :on-session-start           (fn [input invocation] ...)
    :on-session-end             (fn [input invocation] ...)
    :on-error-occurred          (fn [input invocation] ...)
    :on-subagent-start          (fn [input invocation] ...)  hook type \"subagentStart\":
                                input has :agentName :agentDisplayName :agentDescription;
                                may return {:additionalContext \"...\"}
    :on-subagent-stop           (fn [input invocation] ...)  hook type \"subagentStop\":
                                input adds :agentId :agentType :stopReason :response;
                                may return {:decision \"block\" :reason \"...\"} or
                                {:modifiedResponse \"...\"}"
  [& {:as opts}]
  (or opts {}))

;; ============================================================================
;; Upstream-sync feature constructors & constants
;; (parity with @github/copilot-sdk — camelCase JSON-RPC wire keys preserved)
;; ============================================================================

(defn session-limits
  "Construct a session spending-limits map.
  Optional: :max-ai-credits (number) → wire key `maxAiCredits`."
  [& {:keys [max-ai-credits]}]
  (cond-> {} (some? max-ai-credits) (assoc :maxAiCredits max-ai-credits)))

(defn memory-configuration
  "Construct a MemoryConfiguration map (opt-in persistent session memory).
  Optional: :enabled (boolean).  Maps to the camelCase wire key `memory`."
  [& {:keys [enabled]}]
  (cond-> {} (some? enabled) (assoc :enabled enabled)))

(defn bearer-token-provider
  "Wrap a token-producing function as a BYOK bearer-token provider.
  `f` is a (fn [args] ...) returning a bearer token string (or a promise)."
  [f]
  {:getBearerToken f})

(defn copilot-request-handler
  "Wrap a function as a CopilotRequestHandler for outbound HTTP requests.
  `f` is a (fn [request] ...) returning a response map."
  [f]
  {:requestHandler f})

;; Tool defer-loading policies (:auto loads lazily via search, :never pre-loads)
(def tool-defer-modes
  {:auto  "auto"
   :never "never"})

;; GitHub-anchored attachment type discriminators
(def github-attachment-types
  {:commit          "GitHubCommit"
   :release         "GitHubRelease"
   :actions-job     "GitHubActionsJob"
   :repository      "GitHubRepository"
   :file-diff       "GitHubFileDiff"
   :tree-comparison "GitHubTreeComparison"
   :url             "GitHubUrl"
   :file            "GitHubFile"
   :snippet         "GitHubSnippet"})

;; ============================================================================
;; v2.6.0 and v3.0.0 constructors (SDK protocol 4)
;; ============================================================================

(defn client-info
  "Construct the ClientInfo map declared on the `connect` handshake
  (client option :client-info).  All keys are optional strings; empty values
  are dropped, and `connect` is not sent when every field is empty.
  Keys: :editor-name :editor-version :extension-name :extension-version"
  [& {:keys [editor-name editor-version extension-name extension-version]}]
  (cond-> {}
    (seq editor-name)       (assoc :editorName editor-name)
    (seq editor-version)    (assoc :editorVersion editor-version)
    (seq extension-name)    (assoc :extensionName extension-name)
    (seq extension-version) (assoc :extensionVersion extension-version)))

(def auto-tiers
  "Auto routing tiers accepted by `copilot.session/switch-auto-tier!`."
  #{"fast" "efficiency" "balance" "intelligence"})

(def ask-user-variants
  "Values of the :ask-user-variant session option (wire askUserVariant)."
  #{"legacy" "elicitation"})

(defn sandbox-config
  "Construct a sandbox configuration map (session option :sandbox-config,
  wire permissions.sandbox).

  Optional: :enabled (default true), :user-policy, :add-current-working-directory,
            :sandbox-mcp-servers, :sandbox-lsp-servers, :sandbox-extensions,
            :allow-bypass (lets the agent ask to run an individual command
            outside the sandbox; the request goes through the normal
            permission flow)"
  [& {:keys [enabled user-policy add-current-working-directory sandbox-mcp-servers
             sandbox-lsp-servers sandbox-extensions allow-bypass]
      :or   {enabled true}}]
  (cond-> {:enabled enabled}
    (some? user-policy)                   (assoc :userPolicy user-policy)
    (some? add-current-working-directory) (assoc :addCurrentWorkingDirectory add-current-working-directory)
    (some? sandbox-mcp-servers)           (assoc :sandboxMcpServers sandbox-mcp-servers)
    (some? sandbox-lsp-servers)           (assoc :sandboxLspServers sandbox-lsp-servers)
    (some? sandbox-extensions)            (assoc :sandboxExtensions sandbox-extensions)
    (some? allow-bypass)                  (assoc :allowBypass allow-bypass)))

(defn image-generation-config
  "Construct an image generation opt-in map (session option :image-generation,
  wire tools.imageGeneration).
  Optional: :enabled, :watermark, :save-to-disk (booleans)"
  [& {:keys [enabled watermark save-to-disk]}]
  (cond-> {}
    (some? enabled)      (assoc :enabled enabled)
    (some? watermark)    (assoc :watermark watermark)
    (some? save-to-disk) (assoc :saveToDisk save-to-disk)))

(defn autopilot-options
  "Construct autopilot continuation options (session option :autopilot,
  wire execution.autopilot).
  Optional: :continuation-enabled (boolean), :max-continues (integer)"
  [& {:keys [continuation-enabled max-continues]}]
  (cond-> {}
    (some? continuation-enabled) (assoc :continuationEnabled continuation-enabled)
    (some? max-continues)        (assoc :maxContinues max-continues)))

(defn message-source
  "Construct a MessageSource value for the :source message option:
  (message-source :user) => \"user\", (message-source :system) => \"system\",
  (message-source :agent \"id\") => \"agent-id\"."
  ([kind]
   (case kind
     :user   "user"
     :system "system"
     (throw (ex-info (str "Unknown message source " (pr-str kind)) {:kind kind}))))
  ([kind agent-id]
   (if (= kind :agent)
     (str "agent-" agent-id)
     (throw (ex-info (str "Unknown message source " (pr-str kind)) {:kind kind})))))

(defn github-token-response
  "Encode a :github-token-provider result as the gitHubToken.getToken reply.

  `result` - a token string, a map {:access-token s :token-type s :expires-in secs}
             (camelCase keys are also accepted), or :cancelled / nil to decline.
             :expires-in is the remaining lifetime in seconds; the runtime
             requires more than one hour, and it defaults to 28800 (8 hours,
             the typical GitHub token lifetime)."
  [result]
  (let [token (cond
                (string? result) result
                (map? result)    (or (:access-token result) (:accessToken result)))]
    (if (string? token)
      (let [token-type (when (map? result) (or (:token-type result) (:tokenType result)))
            expires-in (or (when (map? result) (or (:expires-in result) (:expiresIn result)))
                           28800)]
        (cond-> {:kind "token" :accessToken token :expiresIn expires-in}
          token-type (assoc :tokenType token-type)))
      {:kind "cancelled"})))

(def client-modes
  "Client modes (client option :mode).  \"coding-agent\" (the default) keeps
  the CLI-like coding tools and defaults; \"empty\" enables only what each
  session asks for, so every create/resume must pass :available-tools.  The
  legacy name \"copilot-cli\" is accepted as an alias of \"coding-agent\"."
  {:coding-agent "coding-agent"
   :empty        "empty"})

(defn parse-client-mode
  "Normalize a :mode client option (string or keyword) to :coding-agent or
  :empty.  Throws ex-info for unknown values."
  [mode]
  (let [m (if (keyword? mode) (name mode) mode)]
    (cond
      (contains? #{nil "coding-agent" "copilot-cli"} m) :coding-agent
      (= "empty" m)                                     :empty
      :else (throw (ex-info (str "Invalid client mode " (pr-str mode)
                                 ": expected \"coding-agent\" or \"empty\"")
                            {:mode mode})))))

(defn named-provider-config
  "Construct a NamedProviderConfig map for the :providers session option
  (wire inference.providers[]).  Model definitions reference it by :name,
  which cannot contain '/'.

  Required: :name, :base-url
  Optional: :type, :api-key, :api-key-command, :bearer-token, :wire-api,
            :transport, :azure, :headers, :model-provider"
  [& {:keys [type base-url api-key api-key-command bearer-token wire-api
             transport azure headers model-provider]
      provider-name :name}]
  (cond-> {:name provider-name :baseUrl base-url}
    type            (assoc :type (name type))
    api-key         (assoc :apiKey api-key)
    api-key-command (assoc :apiKeyCommand api-key-command)
    bearer-token    (assoc :bearerToken bearer-token)
    wire-api        (assoc :wireApi (name wire-api))
    transport       (assoc :transport (name transport))
    azure           (assoc :azure azure)
    headers         (assoc :headers headers)
    model-provider  (assoc :modelProvider (name model-provider))))

(defn provider-model-config
  "Construct a ProviderModelConfig map for the :models session option
  (wire inference.models[]).  Select the model with \"<provider>/<id>\".

  Required: :provider, :id
  Optional: :model-id, :wire-model, :name, :max-prompt-tokens,
            :max-context-window-tokens, :max-output-tokens, :capabilities"
  [& {:keys [provider id model-id wire-model max-prompt-tokens
             max-context-window-tokens max-output-tokens capabilities]
      display-name :name}]
  (cond-> {:provider provider :id id}
    model-id                  (assoc :modelId model-id)
    wire-model                (assoc :wireModel wire-model)
    display-name              (assoc :name display-name)
    max-prompt-tokens         (assoc :maxPromptTokens max-prompt-tokens)
    max-context-window-tokens (assoc :maxContextWindowTokens max-context-window-tokens)
    max-output-tokens         (assoc :maxOutputTokens max-output-tokens)
    capabilities              (assoc :capabilities capabilities)))

(defn capi-config
  "Construct the built-in Copilot API policy (session option :capi, wire
  github.capi).  :enabled false keeps a BYOK-only model catalog."
  [& {:keys [enabled]}]
  (cond-> {} (some? enabled) (assoc :enabled enabled)))

(defn model-limits-override
  "Construct the :limits map of `model-capabilities-override`.  The wire keys
  inside capabilities limits are snake_case (max_output_tokens, ...).
  Optional: :max-prompt-tokens, :max-output-tokens, :max-context-window-tokens,
            :vision (map with snake_case keys)"
  [& {:keys [max-prompt-tokens max-output-tokens max-context-window-tokens vision]}]
  (cond-> {}
    max-prompt-tokens         (assoc :max_prompt_tokens max-prompt-tokens)
    max-output-tokens         (assoc :max_output_tokens max-output-tokens)
    max-context-window-tokens (assoc :max_context_window_tokens max-context-window-tokens)
    vision                    (assoc :vision vision)))

(defn shell-authentication-request
  "Parse shellAuthentication.request params into the map passed to an
  :on-shell-authentication-request handler.  :input-kind is \"secret\" (mask
  the input), \"text\", or \"confirm\" (answer \"yes\" or cancel)."
  [m]
  {:session-id (:sessionId m)
   :request-id (:requestId m)
   :program    (:program m)
   :prompt     (:prompt m)
   :input-kind (:inputKind m)
   :command    (:command m)
   :cwd        (:cwd m)})

(defn shell-authentication-response
  "Encode a shell authentication handler result as the reply: a string (or
  {:action \"accept\" :value s}) accepts with that value; nil, :cancel, or
  {:action \"cancel\"} cancels."
  [result]
  (let [value (cond
                (string? result) result
                (map? result)    (when-not (= "cancel" (some-> (:action result) name))
                                   (:value result)))]
    (if (string? value)
      {:action "accept" :value value}
      {:action "cancel"})))

(defn stored-memory
  "Parse memoryStore.store params into the map passed to a :store-memory
  callback: {:session-id :subject :fact :citations :reason}."
  [m]
  {:session-id (:sessionId m)
   :subject    (:subject m)
   :fact       (:fact m)
   :citations  (:citations m)
   :reason     (:reason m)})

(defn network-set-provider-params
  "Build the network.setProvider params for a :network client option, or nil
  when no callback is set.  Each registered callback is marked with an empty
  object; :scope is \"connection\" (runtime default) or \"runtime\"."
  [network]
  (when (map? network)
    (let [markers (cond-> {}
                    (:resolve-proxy network)          (assoc :resolveProxy {})
                    (:authenticate-negotiate network) (assoc :authenticateNegotiate {})
                    (:authenticate-basic network)     (assoc :authenticateBasic {}))]
      (when (seq markers)
        (cond-> markers
          (:scope network) (assoc :scope (name (:scope network))))))))

(def tool-shell-output-event
  "Event type of live, append-only shell output chunks (ephemeral)."
  "tool.shell_output")

(defn tool-shell-output
  "Parse the data of a tool.shell_output event (or the whole event) into
  {:tool-call-id :text :sequence :stream}.  :stream is \"stdout\" (also when
  omitted), \"stderr\", or \"terminal\"."
  [event-or-data]
  (let [d (if (= tool-shell-output-event (:type event-or-data))
            (:data event-or-data)
            event-or-data)]
    {:tool-call-id (:toolCallId d)
     :text         (:text d)
     :sequence     (:sequence d)
     :stream       (:stream d)}))

(defn policy-helper-failure
  "Extract the latest unresolved managed-settings policy helper failure
  (wire policyHelperFailure) from a refused session.create / session.resume
  (the thrown ex-info, its ex-data, or the JSON-RPC error :data) or from a
  managed settings snapshot.  Returns nil when absent.  Helper-authored text
  is untrusted: render it as plain text.

  Keys: :category :failure-mode :message :previous-policy-retained :reason
        :authority :code :details :diagnostic :help-link"
  [x]
  (let [m (if (map? x) x (ex-data x))
        f (or (:policyHelperFailure m)
              (get-in m [:data :policyHelperFailure]))]
    (when (map? f)
      {:category                 (:category f)
       :failure-mode             (:failureMode f)
       :message                  (:message f)
       :previous-policy-retained (true? (:previousPolicyRetained f))
       :reason                   (:reason f)
       :authority                (:authority f)
       :code                     (:code f)
       :details                  (:details f)
       :diagnostic               (:diagnostic f)
       :help-link                (:helpLink f)})))

(def permission-mode-refusals
  "Known refusal reasons of session.permissions.setMode.  :managed-policy
  (\"managed_policy\"): managed (enterprise) policy does not allow the mode."
  {:managed-policy "managed_policy"})

(defn permissions-set-mode-result
  "Parse a session.permissions.setMode result into {:success :mode :reason}.
  A refused change has :success false and, when known, :reason
  \"managed_policy\"."
  [m]
  {:success (true? (:success m))
   :mode    (:mode m)
   :reason  (:reason m)})

;; ============================================================================
;; Session filesystem types
;; ============================================================================

(defn session-fs-config
  "Create a session filesystem configuration.

  `initial-cwd`        - initial working directory
  `session-state-path` - path for session state storage
  Optional: :conventions (\"windows\" or \"posix\")"
  [initial-cwd session-state-path & {:keys [conventions]}]
  (cond-> {:initialCwd initial-cwd :sessionStatePath session-state-path}
    conventions (assoc :conventions conventions)))

(defn session-fs-file-info
  "Create a session filesystem file info map.

  `name`          - file or directory name
  `size`          - size in bytes
  `is-directory`  - boolean
  `is-file`       - boolean
  Optional: :created-at (ISO 8601 string), :modified-at (ISO 8601 string)"
  [name size is-directory is-file & {:as opts}]
  (cond-> {:name        name
           :size        size
           :isDirectory is-directory
           :isFile      is-file}
    (:created-at opts)  (assoc :createdAt (:created-at opts))
    (:modified-at opts) (assoc :modifiedAt (:modified-at opts))))

;; Session Filesystem Provider convention
;;
;; To implement a session filesystem provider in Clojure, supply a map with
;; the following function-valued keys:
;;
;;   :read-file           (fn [session-id path] ...) → string
;;   :write-file          (fn [session-id path content] ...) → nil
;;   :append-file         (fn [session-id path content] ...) → nil
;;   :exists?             (fn [session-id path] ...) → boolean
;;   :stat                (fn [session-id path] ...) → session-fs-file-info map
;;   :mkdir               (fn [session-id path recursive?] ...) → nil
;;   :readdir             (fn [session-id path] ...) → vector of strings
;;   :readdir-with-types  (fn [session-id path] ...) → vector of file-info maps
;;   :rm                  (fn [session-id path recursive?] ...) → nil
;;   :rename              (fn [session-id old-path new-path] ...) → nil

;; ============================================================================
;; Exit Plan Mode
;; ============================================================================

(defn exit-plan-mode-request
  "Construct an exit plan mode request map."
  [session-id]
  {:session-id session-id})

(defn exit-plan-mode-response
  "Construct an exit plan mode response map."
  [approved]
  {:approved approved})

;; ============================================================================
;; Trace Context
;; ============================================================================

(defn trace-context
  "Construct a trace context map."
  [& {:keys [traceparent tracestate]}]
  (cond-> {}
    traceparent (assoc :traceparent traceparent)
    tracestate  (assoc :tracestate tracestate)))

;; ============================================================================
;; Model Capabilities Override
;; ============================================================================

(defn model-capabilities-override
  "Construct a model capabilities override map.

  `supports` - map of capability flags (e.g. {:vision true})
  `limits`   - map of limits with snake_case wire keys, e.g.
               (model-limits-override :max-output-tokens 8192)
               => {:max_output_tokens 8192}
  `vision`   - optional vision limits map"
  [& {:keys [supports limits vision]}]
  (cond-> {}
    supports (assoc :supports supports)
    limits   (assoc :limits limits)
    vision   (assoc :vision vision)))

;; ============================================================================
;; Slash Command types
;; ============================================================================

(defn slash-command-input
  "Construct a slash command input map.

  `hint` - hint string for the command input
  Optional: :completion (\"directory\")"
  [hint & {:keys [completion]}]
  (cond-> {:hint hint}
    completion (assoc :completion completion)))

(defn slash-command-info
  "Construct a slash command info map.

  `allow-during-agent-execution` - boolean
  `description`                  - description string
  `kind`                         - one of \"builtin\" \"client\" \"skill\"
  `name`                         - command name
  Optional: :aliases (vector of strings), :experimental (boolean),
            :input (slash-command-input map)"
  [allow-during-agent-execution description kind name & {:as opts}]
  (cond-> {:allowDuringAgentExecution allow-during-agent-execution
           :description               description
           :kind                      kind
           :name                      name}
    (:aliases opts)      (assoc :aliases (:aliases opts))
    (:experimental opts) (assoc :experimental (:experimental opts))
    (:input opts)        (assoc :input (:input opts))))

;; ============================================================================
;; Command request types
;; ============================================================================

(defn commands-invoke-request
  "Construct a commands invoke request map.

  `name`  - command name
  Optional: :input (string)"
  [name & {:keys [input]}]
  (cond-> {:name name}
    input (assoc :input input)))

(defn commands-list-request
  "Construct a commands list request map.

  Optional: :include-builtins (boolean), :include-client-commands (boolean),
            :include-skills (boolean)"
  [& {:keys [include-builtins include-client-commands include-skills]}]
  (cond-> {}
    (some? include-builtins)        (assoc :includeBuiltins include-builtins)
    (some? include-client-commands) (assoc :includeClientCommands include-client-commands)
    (some? include-skills)          (assoc :includeSkills include-skills)))

;; ============================================================================
;; Model Billing types
;; ============================================================================

(defn model-billing-token-prices
  "Construct a model billing token prices map.

  Optional: :batch-size (int), :cache-price (int),
            :input-price (int), :output-price (int)"
  [& {:keys [batch-size cache-price input-price output-price]}]
  (cond-> {}
    batch-size   (assoc :batchSize batch-size)
    cache-price  (assoc :cachePrice cache-price)
    input-price  (assoc :inputPrice input-price)
    output-price (assoc :outputPrice output-price)))

(defn model-billing
  "Construct a model billing map.

  `multiplier` - billing multiplier (double)
  Optional: :token-prices (model-billing-token-prices map),
            :picker-price-category (one of \"high\" \"low\" \"medium\" \"very_high\")"
  [multiplier & {:keys [token-prices picker-price-category]}]
  (cond-> {:multiplier multiplier}
    token-prices          (assoc :tokenPrices token-prices)
    picker-price-category (assoc :pickerPriceCategory picker-price-category)))

;; Experimental
(defn skills-load-diagnostics
  "Construct a skills load diagnostics map.

  `errors`   - vector of error strings
  `warnings` - vector of warning strings"
  [errors warnings]
  {:errors errors :warnings warnings})

;; Experimental
(def remote-session-modes
  "Valid remote session mode values."
  #{"export" "off" "on"})

;; Experimental
(defn remote-enable-request
  "Construct a remote enable request map.

  Optional: :mode (one of \"export\" \"off\" \"on\")"
  [& {:keys [mode]}]
  (cond-> {}
    mode (assoc :mode mode)))

;; Experimental
(defn remote-enable-result
  "Parse a remote enable result map.

  `m` - raw JSON map with :remoteSteerable (boolean, required) and :url (string, optional)"
  [m]
  {:remote-steerable (:remoteSteerable m)
   :url              (:url m)})

;; ============================================================================
;; Client options
;; ============================================================================

(defn client-options
  "Create a client options map.

  Optional keys:
    :cli-path         - path to CLI executable (default: \"copilot\")
    :cli-args         - extra CLI arguments (vector of strings)
    :cwd              - working directory for CLI process
    :port             - TCP port (default 0 = random)
    :use-stdio        - use stdio transport (default true)
    :cli-url          - URL of existing server (mutually exclusive with :cli-path/:use-stdio)
    :log-level        - :none :error :warning :info :debug :all (default :info)
    :auto-start       - auto-start on first use (default true)
    :auto-restart     - auto-restart on crash (default true)
    :env              - environment variables map
    :github-token     - GitHub auth token
    :use-logged-in-user - use logged-in user auth (default true unless :github-token set)
    :session-idle-timeout-seconds - server-wide idle timeout for sessions in seconds
    :session-fs       - session filesystem provider map (see session-fs-config)
    :copilot-home     - custom path to the copilot home directory
    :tcp-connection-token - token for TCP connections
    :bearer-token-provider - BYOK bearer-token provider map (see bearer-token-provider)
    :builtin-plugin-directories - extra builtin plugin dirs (JSON: builtinPluginDirectories)
    :in-process       - use in-process FFI transport (JSON: inProcess)
    :request-handler  - CopilotRequestHandler map for outbound HTTP (see copilot-request-handler)
    :client-info      - application identity sent on the connect handshake (see client-info)
    :mode             - \"coding-agent\" (default) or \"empty\" (see client-modes)
    :network          - network callbacks registered with network.setProvider:
                        {:scope \"connection\"|\"runtime\"
                         :resolve-proxy (fn [{:keys [url]}] -> PAC proxy string or nil)
                         :authenticate-negotiate (fn [{:keys [spn]}] -> base64 token or nil)
                         :authenticate-basic (fn [{:keys [proxy realm previous-rejected]}]
                                               -> {:username u :password p} or nil)}"
  [& {:as opts}]
  (or opts {}))

;; ============================================================================
;; Auth / Status response types
;; ============================================================================

(defn get-status-response
  "Parse a status.get response."
  [m]
  {:version          (:version m)
   :protocol-version (:protocolVersion m)})

(defn get-auth-status-response
  "Parse an auth.getStatus response."
  [m]
  {:is-authenticated (:isAuthenticated m)
   :auth-type        (:authType m)
   :host             (:host m)
   :login            (:login m)
   :status-message   (:statusMessage m)})

;; ---------------------------------------------------------------------------
;; Session Event Envelope
;; ---------------------------------------------------------------------------
;; Session events are plain Clojure maps with these envelope keys:
;;   :type      - event type string (e.g. "assistant.message")
;;   :id        - unique event UUID
;;   :timestamp - ISO 8601 timestamp
;;   :parentId  - UUID of preceding event (or nil)
;;   :agentId   - sub-agent identifier (or nil for root agent)
;;   :ephemeral - boolean, true if transient
;;   :data      - event payload map
