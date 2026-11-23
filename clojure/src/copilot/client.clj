;;;; ---------------------------------------------------------------------------
;;;;  Copyright (c) Microsoft Corporation. All rights reserved.
;;;; ---------------------------------------------------------------------------
;;;;
;;;; Copilot CLI SDK Client - Main entry point for the Copilot SDK.
;;;;
;;;; Manages the connection to the Copilot CLI server and provides session
;;;; management capabilities.

(ns copilot.client
  "Main client for interacting with the Copilot CLI.

  The CopilotClient manages the connection to the Copilot CLI server and
  provides methods to create and manage conversation sessions.  It can
  either spawn a CLI server process or connect to an existing server."
  (:require [copilot.json-rpc :as rpc]
            [copilot.session :as session]
            [copilot.sdk-protocol-version :as proto]
            [copilot.types :as types])
  (:import [java.lang Process ProcessBuilder ProcessBuilder$Redirect]
           [java.net Socket]
           [java.io Closeable]
           [java.util UUID]))

;; ============================================================================
;; Internal helpers
;; ============================================================================

(defn- parse-cli-url
  "Parse a CLI URL string into {:host h :port p}.
  Supports formats: \"host:port\", \"http://host:port\", or just \"port\"."
  [^String url]
  (let [clean (.replaceAll url "^https?://" "")]
    (if (re-matches #"\d+" clean)
      {:host "localhost" :port (Integer/parseInt clean)}
      (let [parts (.split clean ":")]
        (when (not= 2 (alength parts))
          (throw (ex-info (str "Invalid cliUrl format: " url
                               ". Expected \"host:port\", \"http://host:port\", or \"port\"")
                          {:url url})))
        (let [host (if (empty? (aget parts 0)) "localhost" (aget parts 0))
              port (Integer/parseInt (aget parts 1))]
          (when (or (<= port 0) (> port 65535))
            (throw (ex-info (str "Invalid port in cliUrl: " url) {:url url})))
          {:host host :port port})))))

(defn- build-cli-args
  "Build the CLI argument list from options."
  [opts]
  (let [extra-args  (or (:cli-args opts) [])
        log-level   (or (:log-level opts) "info")
        use-stdio   (if (contains? opts :use-stdio) (:use-stdio opts) true)
        port        (or (:port opts) 0)
        github-token (:github-token opts)
        use-logged-in (if (contains? opts :use-logged-in-user)
                        (:use-logged-in-user opts)
                        (if github-token false true))]
    (cond-> (vec (concat extra-args
                         ["--headless" "--no-auto-update"
                          "--log-level" (name log-level)]))
      use-stdio          (conj "--stdio")
      (and (not use-stdio)
           (pos? port))  (conj "--port" (str port))
      github-token       (conj "--auth-token-env" "COPILOT_SDK_AUTH_TOKEN")
      (not use-logged-in) (conj "--no-auto-login")
      (and (:session-idle-timeout-seconds opts)
           (pos? (:session-idle-timeout-seconds opts)))
      (conj "--session-idle-timeout" (str (:session-idle-timeout-seconds opts))))))

(defn- spawn-cli-process
  "Spawn the Copilot CLI process. Returns a java.lang.Process.

  JavaScript entry points (npm-loader.js, legacy index.js) run under `node`;
  any other path (a native binary or the node_modules/.bin/copilot shim) is
  executed directly."
  [opts]
  (let [cli-path (or (:cli-path opts) "copilot")
        args     (build-cli-args opts)
        cwd      (or (:cwd opts) (System/getProperty "user.dir"))
        env      (:env opts)
        command  (if (re-find #"(?i)\.[mc]?js$" cli-path)
                   ["node" cli-path]
                   [cli-path])
        pb       (ProcessBuilder. ^java.util.List (into command args))]
    (.directory pb (java.io.File. ^String cwd))
    (.redirectErrorStream pb false)
    ;; Redirect stderr to inherit so logs are visible
    (.redirectError pb ProcessBuilder$Redirect/INHERIT)
    ;; Set environment
    (let [pb-env (.environment pb)]
      ;; Remove NODE_DEBUG to avoid polluting stdout
      (.remove pb-env "NODE_DEBUG")
      ;; Add custom env vars
      (when env
        (doseq [[k v] env]
          (.put pb-env k v)))
      ;; Add auth token to environment if provided
      (when-let [token (:github-token opts)]
        (.put pb-env "COPILOT_SDK_AUTH_TOKEN" token)))
    (.start pb)))

;; ============================================================================
;; Tool result normalisation
;; ============================================================================

(defn- tool-result-object?
  "Check if a value looks like a ToolResultObject (duck-type check)."
  [v]
  (and (map? v)
       (contains? v :textResultForLlm)
       (string? (:textResultForLlm v))
       (contains? v :resultType)))

(defn- normalize-tool-result
  "Normalize any value to a ToolResultObject map."
  [result]
  (cond
    (nil? result)
    {:textResultForLlm "Tool returned no result"
     :resultType       "failure"
     :error            "tool returned no result"
     :toolTelemetry    {}}

    (tool-result-object? result)
    result

    (string? result)
    {:textResultForLlm result
     :resultType       "success"
     :toolTelemetry    {}}

    :else
    {:textResultForLlm (pr-str result)
     :resultType       "success"
     :toolTelemetry    {}}))

;; ============================================================================
;; CopilotClient
;; ============================================================================

(defn- tool-definition
  "Wire definition of a tool map (the handler stays local)."
  [t]
  (cond-> {:name        (:name t)
           :description (:description t)
           :parameters  (:parameters t)}
    (some? (:idempotent t)) (assoc :idempotent (:idempotent t))))

(defn- capi-options
  "The flat `capi` object (grouped as github.capi): the :capi session option
  plus the WebSocket transport flag.  nil when empty."
  [capi enable-web-socket-responses]
  (let [m (cond-> (if (map? capi) capi {})
            (some? enable-web-socket-responses)
            (assoc :enableWebSocketResponses enable-web-socket-responses))]
    (when (seq m) m)))

(defn- enum-name
  "Wire string of an enum option given as a keyword or string."
  [v]
  (if (keyword? v) (name v) v))

(defn- make-session-create-params
  "Build the flat params map for session.create or session.resume.  The flat
  map is converted to the SDK protocol 4 grouped payload by
  `group-session-params` right before it is sent."
  [config]
  (let [{:keys [model session-id reasoning-effort tools system-message
                available-tools excluded-tools provider on-permission-request
                on-user-input-request hooks working-directory streaming
                mcp-servers custom-agents config-dir skill-directories
                disabled-skills infinite-sessions disable-resume
                model-capabilities enable-config-discovery
                include-sub-agent-streaming-events github-token
                idle-timeout
                enable-citations excluded-builtin-agents session-limits
                memory-config otlp-protocol enable-web-socket-responses
                exp-assignments on-mcp-auth-request
                rewind-enabled additional-directories disabled-mcp-servers
                github-mcp-tool-config canvas-provider custom-agents-local-only
                tool-search experimental-mode content-exclusion
                ask-user-variant sandbox-config feature-flags allowed-models
                auth-client-id-metadata-url agent-directories hook-directories
                trust-working-directory allow-transcript-recovery
                image-generation autopilot providers models default-provider
                capi on-shell-authentication-request store-memory]} config
        capi-wire (capi-options capi enable-web-socket-responses)]
    (cond-> {}
      model               (assoc :model model)
      session-id          (assoc :sessionId session-id)
      reasoning-effort    (assoc :reasoningEffort (name reasoning-effort))
      tools               (assoc :tools (mapv tool-definition tools))
      system-message      (assoc :systemMessage system-message)
      available-tools     (assoc :availableTools available-tools)
      excluded-tools      (assoc :excludedTools excluded-tools)
      provider            (assoc :provider provider)
      on-permission-request   (assoc :requestPermission true)
      on-user-input-request   (assoc :requestUserInput true)
      (and hooks (some some? (vals hooks))) (assoc :hooks true)
      working-directory   (assoc :workingDirectory working-directory)
      (some? streaming)   (assoc :streaming streaming)
      mcp-servers         (assoc :mcpServers mcp-servers)
      custom-agents       (assoc :customAgents custom-agents)
      config-dir          (assoc :configDir config-dir)
      skill-directories   (assoc :skillDirectories skill-directories)
      disabled-skills     (assoc :disabledSkills disabled-skills)
      infinite-sessions   (assoc :infiniteSessions infinite-sessions)
      (some? disable-resume) (assoc :disableResume disable-resume)
      model-capabilities  (assoc :modelCapabilities model-capabilities)
      (some? enable-config-discovery) (assoc :enableConfigDiscovery enable-config-discovery)
      (some? include-sub-agent-streaming-events) (assoc :includeSubAgentStreamingEvents include-sub-agent-streaming-events)
      github-token        (assoc :gitHubToken github-token)
      (some? enable-citations) (assoc :enableCitations enable-citations)
      excluded-builtin-agents  (assoc :excludedBuiltinAgents excluded-builtin-agents)
      session-limits           (assoc :sessionLimits session-limits)
      memory-config            (assoc :memory memory-config)
      otlp-protocol            (assoc :otlpProtocol otlp-protocol)
      capi-wire                (assoc :capi capi-wire)
      exp-assignments          (assoc :expAssignments exp-assignments)
      (some? rewind-enabled)   (assoc :rewindEnabled rewind-enabled)
      additional-directories   (assoc :additionalDirectories additional-directories)
      disabled-mcp-servers     (assoc :disabledMcpServers disabled-mcp-servers)
      github-mcp-tool-config   (assoc :githubMcpToolConfig github-mcp-tool-config)
      canvas-provider          (assoc :canvasProvider canvas-provider)
      (some? custom-agents-local-only) (assoc :customAgentsLocalOnly custom-agents-local-only)
      (some? tool-search)      (assoc :toolSearch tool-search)
      (some? experimental-mode) (assoc :experimentalMode experimental-mode)
      (some? content-exclusion) (assoc :contentExclusion content-exclusion)
      on-mcp-auth-request      (assoc :mcpAuthHandler true)
      idle-timeout        (assoc :idleTimeout idle-timeout)
      ask-user-variant         (assoc :askUserVariant (enum-name ask-user-variant))
      sandbox-config           (assoc :sandboxConfig sandbox-config)
      feature-flags            (assoc :featureFlags feature-flags)
      allowed-models           (assoc :allowedModels allowed-models)
      auth-client-id-metadata-url (assoc :authClientIdMetadataUrl auth-client-id-metadata-url)
      agent-directories        (assoc :agentDirectories agent-directories)
      hook-directories         (assoc :hookDirectories hook-directories)
      trust-working-directory  (assoc :trustWorkingDirectory (enum-name trust-working-directory))
      (some? allow-transcript-recovery) (assoc :allowTranscriptRecovery allow-transcript-recovery)
      image-generation         (assoc :imageGeneration image-generation)
      autopilot                (assoc :autopilot autopilot)
      providers                (assoc :providers providers)
      models                   (assoc :models models)
      ;; An explicit nil is kept so that resume can clear the default provider.
      (contains? config :default-provider) (assoc :defaultProvider default-provider)
      on-shell-authentication-request (assoc :requestShellAuthentication true)
      store-memory             (assoc :hasMemoryStore true))))

;; ============================================================================
;; SDK protocol 4: grouped session payloads
;; ============================================================================

(def ^:private session-option-groups
  "Option groups of the protocol 4 session.create / session.resume payloads."
  #{:inference :tools :permissions :interaction :events :agents :skills :mcp
    :github :filesystem :execution :instructions :hooks :git :state :plugins
    :canvases :extensions :experiments :policy})

(def ^:private create-only-session-keys
  "Flat keys valid only on session.create; dropped from resume payloads."
  #{:name :cloud :trustWorkingDirectory :refreshCustomInstructions})

(def ^:private resume-only-session-keys
  "Flat keys valid only on session.resume; dropped from create payloads."
  #{:suppressResumeEvent :continuePendingWork :allowTranscriptRecovery
    :requestedEnvironmentVariables :openCanvases})

(def ^:private session-option-paths
  "Flat session option key -> protocol 4 [group member].  Keys not listed here
  (:sessionId, :model, :workingDirectory, :systemMessage, ...) and unknown keys
  stay at the root."
  {:reasoningEffort                    [:inference :reasoningEffort]
   :reasoningSummary                   [:inference :reasoningSummary]
   :contextTier                        [:inference :contextTier]
   :allowMultipleToolCalls             [:inference :allowMultipleToolCalls]
   :enableCitations                    [:inference :enableCitations]
   :modelCapabilities                  [:inference :capabilities]
   :verbosity                          [:inference :verbosity]
   :providers                          [:inference :providers]
   :models                             [:inference :models]
   :defaultProvider                    [:inference :defaultProvider]
   :tools                              [:tools :definitions]
   :availableTools                     [:tools :availableTools]
   :excludedTools                      [:tools :excludedTools]
   :toolSearch                         [:tools :search]
   :largeOutput                        [:tools :largeOutput]
   :imageGeneration                    [:tools :imageGeneration]
   :manageScheduleEnabled              [:tools :manageScheduleEnabled]
   :webSearch                          [:tools :webSearch]
   :shell                              [:tools :shell]
   :maxInlineBinaryBytes               [:tools :maxInlineBinaryBytes]
   :requestPermission                  [:permissions :request]
   :sandboxConfig                      [:permissions :sandbox]
   :sandbox                            [:permissions :sandbox]
   :permissionMode                     [:permissions :mode]
   :sandboxConfigSource                [:permissions :sandboxSource]
   :commands                           [:interaction :commands]
   :askUserVariant                     [:interaction :askUserVariant]
   :requestUserInput                   [:interaction :requestUserInput]
   :requestElicitation                 [:interaction :requestElicitation]
   :requestShellAuthentication         [:interaction :requestShellAuthentication]
   :requestExitPlanMode                [:interaction :requestExitPlanMode]
   :requestAutoModeSwitch              [:interaction :requestAutoModeSwitch]
   :requestAutoModeRecovery            [:interaction :requestAutoModeRecovery]
   :streaming                          [:events :streaming]
   :includeSubAgentStreamingEvents     [:events :includeSubAgentStreamingEvents]
   :customAgents                       [:agents :definitions]
   :defaultAgent                       [:agents :default]
   :agent                              [:agents :selected]
   :includedBuiltinAgents              [:agents :includedBuiltinAgents]
   :excludedBuiltinAgents              [:agents :excludedBuiltinAgents]
   :customAgentsLocalOnly              [:agents :localDiscoveryOnly]
   :agentDirectories                   [:agents :directories]
   :skillDirectories                   [:skills :directories]
   :enableSkills                       [:skills :enabled]
   :includedBuiltinSkills              [:skills :includedBuiltinSkills]
   :disabledSkills                     [:skills :disabledSkills]
   :hasSkillProvider                   [:skills :hasProvider]
   :ignoredSkillsLocations             [:skills :ignoredLocations]
   :mcpServers                         [:mcp :servers]
   :disabledMcpServers                 [:mcp :disabledServers]
   :requestMcpApps                     [:mcp :enableApps]
   :enableMcpApps                      [:mcp :enableApps]
   :mcpOAuthTokenStorage               [:mcp :oauthTokenStorage]
   :authClientIdMetadataUrl            [:mcp :authClientIdMetadataUrl]
   :envValueMode                       [:mcp :envValueMode]
   :managedMcpServers                  [:mcp :managedServers]
   :allowAllMcpServerInstructions      [:mcp :allowAllServerInstructions]
   :gitHubToken                        [:github :token]
   :gitHubTokenProviderRegistrationId  [:github :tokenProviderRegistrationId]
   :githubMcpToolConfig                [:github :mcp]
   :capi                               [:github :capi]
   :enableSessionTelemetry             [:github :telemetryEnabled]
   :enableManagedSettings              [:github :fetchManagedSettings]
   :interactiveUser                    [:github :interactiveUser]
   :enableGitHubTelemetryForwarding    [:github :telemetryForwarding]
   :authInfo                           [:github :authInfo]
   :copilotUrl                         [:github :copilotUrl]
   :additionalDirectories              [:filesystem :additionalDirectories]
   :enableFileChangeTracking           [:filesystem :trackFileChanges]
   :trustWorkingDirectory              [:filesystem :trustWorkingDirectory]
   :sessionLimits                      [:execution :limits]
   :autopilot                          [:execution :autopilot]
   :autopilotContinuation              [:execution :autopilotContinuation]
   :instructionDirectories             [:instructions :directories]
   :organizationCustomInstructions     [:instructions :organizationText]
   :enableOnDemandInstructionDiscovery [:instructions :onDemandDiscovery]
   :skipCustomInstructions             [:instructions :skipCustomInstructions]
   :refreshCustomInstructions          [:instructions :refreshCustomInstructions]
   :hooks                              [:hooks :hasHandlers]
   :hookDirectories                    [:hooks :directories]
   :enableFileHooks                    [:hooks :enableRepositoryHooks]
   :coauthorEnabled                    [:git :coauthorEnabled]
   :enableHostGitOperations            [:git :enableHostGitOperations]
   :infiniteSessions                   [:state :infiniteSessions]
   :memory                             [:state :memory]
   :hasMemoryStore                     [:state :hasMemoryStore]
   :enableSessionStore                 [:state :enableCrossSessionStore]
   :skipEmbeddingRetrieval             [:state :skipEmbeddingRetrieval]
   :embeddingCacheStorage              [:state :embeddingCacheStorage]
   :pluginDirectories                  [:plugins :directories]
   :installedPlugins                   [:plugins :installed]
   :canvases                           [:canvases :definitions]
   :canvasProvider                     [:canvases :provider]
   :requestCanvasRenderer              [:canvases :requestRenderer]
   :openCanvases                       [:canvases :open]
   :requestExtensions                  [:extensions :enabled]
   :extensionSdkPath                   [:extensions :sdkPath]
   :extensionInfo                      [:extensions :identity]
   :enableExperimentalMode             [:experiments :enableExperimentalMode]
   :isExperimentalMode                 [:experiments :enableExperimentalMode]
   ;; This SDK historically sent the experimental-mode flag as experimentalMode.
   :experimentalMode                   [:experiments :enableExperimentalMode]
   :featureFlags                       [:experiments :flags]
   :expAssignments                     [:experiments :assignments]
   :featureFlagConfiguration           [:experiments :configuration]
   :allowedModels                      [:policy :allowedModels]
   :managedSettings                    [:policy :managedSettings]
   :excludedManagedSettings            [:policy :excludedManagedSettings]
   :enforceManagedModelDefaults        [:policy :enforceManagedModelDefaults]})

(def ^:private legacy-provider-model-fields
  "Legacy :provider fields that describe the model rather than the connection."
  #{:modelId :wireModel :maxPromptTokens :maxContextWindowTokens
    :maxOutputTokens :capabilities})

(def ^:private empty-mode-defaults
  "[group member value] defaults applied under the caller's values in the
  \"empty\" client mode."
  [[:experiments  :enableExperimentalMode  false]
   [:github       :telemetryEnabled        false]
   [:mcp          :oauthTokenStorage       "in-memory"]
   [:state        :skipEmbeddingRetrieval  true]
   [:state        :embeddingCacheStorage   "in-memory"]
   [:state        :enableCrossSessionStore false]
   [:state        :memory                  {:enabled false}]
   [:instructions :onDemandDiscovery       false]
   [:hooks        :enableRepositoryHooks   false]
   [:git          :enableHostGitOperations false]])

(defn- translate-legacy-provider
  "Translate the legacy single BYOK :provider into the named \"default\"
  connection (inference.providers), select it (inference.defaultProvider),
  disable CAPI (github.capi.enabled false, as v1 BYOK did), and move
  model-specific fields into inference.models when the session model is set.
  Skipped when :providers, :defaultProvider, or :capi :enabled are explicit."
  [groups provider model]
  (let [inference (:inference groups)
        capi      (get-in groups [:github :capi])]
    (if (or (not (map? provider))
            (contains? inference :providers)
            (contains? inference :defaultProvider)
            (and (map? capi) (contains? capi :enabled)))
      groups
      (let [entries      (remove (fn [[k v]] (or (nil? v) (= k :name))) provider)
            model-field? (fn [[k _]] (contains? legacy-provider-model-fields k))
            model-fields (into {} (filter model-field?) entries)
            connection   (into {:name "default"} (remove model-field?) entries)]
        (cond-> (-> groups
                    (assoc-in [:inference :providers] [connection])
                    (assoc-in [:inference :defaultProvider] "default")
                    (assoc-in [:github :capi] (assoc (if (map? capi) capi {}) :enabled false)))
          (and (seq model-fields) (string? model))
          (assoc-in [:inference :models]
                    [(merge {:provider "default" :id model} model-fields)]))))))

(defn group-session-params
  "Convert the flat session payload built by this SDK into the SDK protocol 4
  grouped payload sent with session.create (`resume?` false) and
  session.resume (`resume?` true).  Internal; exposed for testing.

  Protocol 4 runtimes reject the old flat keys and unknown group members but
  tolerate unknown root keys.  Known flat keys move to their exact
  group.member (for example :streaming to events.streaming and :hooks true to
  hooks.hasHandlers); root keys and unrecognized keys stay at the top level;
  nil values are skipped (except an explicit :defaultProvider nil on resume,
  which clears the default); members invalid for the method are dropped
  (filesystem.trustWorkingDirectory and instructions.refreshCustomInstructions
  on resume, canvases.open on create); and empty groups are not emitted.
  Already-grouped maps are merged and their members win.  The legacy single
  :provider becomes the named \"default\" connection."
  [flat resume?]
  (let [keep-nil?  (fn [k] (and resume? (= k :defaultProvider)))
        {:keys [root groups explicit provider]}
        (reduce-kv
         (fn [acc k v]
           (cond
             (and (nil? v) (not (keep-nil? k)))
             acc

             (or (and resume? (contains? create-only-session-keys k))
                 (and (not resume?) (contains? resume-only-session-keys k)))
             acc

             (= k :provider)
             (assoc acc :provider v)

             (= k :configDir)
             (update acc :root #(if (contains? % :configDirectory) % (assoc % :configDirectory v)))

             (= k :disableResume)
             (if resume?
               (update acc :root #(if (contains? % :suppressResumeEvent) % (assoc % :suppressResumeEvent v)))
               acc)

             (and (contains? session-option-groups k) (map? v))
             (assoc-in acc [:explicit k] v)

             (contains? session-option-paths k)
             (assoc-in acc (into [:groups] (get session-option-paths k)) v)

             :else
             (assoc-in acc [:root k] v)))
         {:root {} :groups {} :explicit {} :provider nil}
         (or flat {}))
        ;; Explicitly grouped members win over members moved from flat keys.
        groups (reduce-kv
                (fn [gs g members]
                  (reduce-kv
                   (fn [gs m v]
                     (if (or (some? v) (and (= g :inference) (keep-nil? m)))
                       (assoc-in gs [g m] v)
                       gs))
                   gs
                   members))
                groups
                explicit)
        ;; The runtime rejects members that are invalid for this lifecycle method.
        groups (reduce (fn [gs [g m]] (if (contains? gs g) (update gs g dissoc m) gs))
                       groups
                       (if resume?
                         [[:filesystem :trustWorkingDirectory]
                          [:instructions :refreshCustomInstructions]]
                         [[:canvases :open]]))
        groups (if (some? provider)
                 (translate-legacy-provider groups provider (:model root))
                 groups)
        groups (if (some? (get-in groups [:mcp :servers]))
                 (update groups :mcp #(if (contains? % :envValueMode) % (assoc % :envValueMode "direct")))
                 groups)]
    (reduce-kv (fn [r g members] (if (seq members) (assoc r g members) r))
               root
               groups)))

(defn apply-mode-defaults
  "Apply the client mode defaults to a grouped payload.  Internal; exposed for
  testing.  In the :empty mode the empty-mode defaults are filled in where the
  caller left a member unset; :coding-agent leaves the payload unchanged."
  [params mode]
  (if (= mode :empty)
    (reduce (fn [p [g m v]]
              (if (nil? (get-in p [g m]))
                (assoc-in p [g m] v)
                p))
            params
            empty-mode-defaults)
    params))

(defn create-client
  "Create a new CopilotClient.

  Returns a client map (atom-backed) that manages the CLI process, JSON-RPC
  connection, and sessions.

  Options (all optional):
    :cli-path, :cli-args, :cwd, :port, :use-stdio, :cli-url,
    :log-level, :auto-start, :auto-restart, :env,
    :github-token, :use-logged-in-user, :client-info, :mode, :network

  See `copilot.types/client-options` for full documentation."
  [& {:as opts}]
  (let [opts (or opts {})]
    ;; Validate mutually exclusive options
    (when (and (:cli-url opts)
               (or (true? (:use-stdio opts)) (:cli-path opts)))
      (throw (ex-info "cliUrl is mutually exclusive with useStdio and cliPath"
                      {:opts opts})))
    (when (and (:cli-url opts)
               (or (:github-token opts) (contains? opts :use-logged-in-user)))
      (throw (ex-info "githubToken and useLoggedInUser cannot be used with cliUrl"
                      {:opts opts})))

    (let [external? (boolean (:cli-url opts))
          parsed    (when external? (parse-cli-url (:cli-url opts)))
          mode      (types/parse-client-mode (:mode opts))]
      (atom {:options      opts
             :state        :disconnected    ;; :disconnected :connecting :connected :error
             :process      nil              ;; java.lang.Process
             :rpc-client   nil              ;; copilot.json-rpc/JsonRpcClient
             :socket       nil              ;; java.net.Socket (TCP mode only)
             :sessions     {}               ;; session-id -> session atom
             :models-cache nil              ;; cached model list
             :external?    external?
             :host         (or (:host parsed) "localhost")
             :port         (:port parsed)
             :mode         mode             ;; :coding-agent or :empty
             :lifecycle-handlers (atom #{})
             ;; registration-id -> :github-token-provider fn
             :token-providers (atom {})
             ;; [session-id request-id] -> promise of the shell auth reply
             :shell-auth-requests (atom {})}))))

;; ============================================================================
;; Connection management
;; ============================================================================

(defn- attach-connection-handlers!
  "Wire up notification and request handlers on the RPC client."
  [client-atom rpc-client]
  ;; Session event notifications
  (rpc/set-notification-handler!
   rpc-client
   (fn [method params]
     (let [state @client-atom]
       (case method
         "session.event"
         (when-let [session-id (:sessionId params)]
           (when-let [session-atom (get-in state [:sessions session-id])]
             (session/dispatch-event! session-atom (:event params))))

         "session.lifecycle"
         (let [event (types/session-lifecycle-event params)
               handlers @(:lifecycle-handlers state)]
           (doseq [h handlers]
             (try (h event) (catch Exception _))))

         ;; Unknown notification - ignore
         nil))))

  ;; Tool call requests from server
  (rpc/set-request-handler!
   rpc-client "tool.call"
   (fn [params]
     (let [session-id  (:sessionId params)
           tool-name   (:toolName params)
           tool-call-id (:toolCallId params)
           arguments   (:arguments params)
           state       @client-atom
           session-atom (get-in state [:sessions session-id])]
       (if-not session-atom
         {:result {:textResultForLlm (str "Unknown session " session-id)
                   :resultType       "failure"
                   :toolTelemetry    {}}}
         (let [handler (session/get-tool-handler session-atom tool-name)]
           (if-not handler
             {:result {:textResultForLlm (str "Tool '" tool-name "' is not supported by this client instance.")
                       :resultType       "failure"
                       :error            (str "tool '" tool-name "' not supported")
                       :toolTelemetry    {}}}
             (try
               (let [invocation (types/tool-invocation session-id tool-call-id tool-name arguments)
                     result     (handler arguments invocation)]
                 {:result (normalize-tool-result result)})
               (catch Exception e
                 {:result {:textResultForLlm "Invoking this tool produced an error. Detailed information is not available."
                           :resultType       "failure"
                           :error            (.getMessage e)
                           :toolTelemetry    {}}}))))))))

  ;; Permission requests from server
  (rpc/set-request-handler!
   rpc-client "permission.request"
   (fn [params]
     (let [session-id (:sessionId params)
           state      @client-atom
           session-atom (get-in state [:sessions session-id])]
       (if-not session-atom
         {:result {:kind "denied-no-approval-rule-and-could-not-request-from-user"}}
         (try
           (let [result (session/handle-permission-request! session-atom (:permissionRequest params))]
             {:result result})
           (catch Exception _
             {:result {:kind "denied-no-approval-rule-and-could-not-request-from-user"}}))))))

  ;; User input requests from server
  (rpc/set-request-handler!
   rpc-client "userInput.request"
   (fn [params]
     (let [session-id (:sessionId params)
           state      @client-atom
           session-atom (get-in state [:sessions session-id])]
       (if-not session-atom
         (throw (ex-info "Session not found" {:session-id session-id}))
         (session/handle-user-input-request!
          session-atom
          {:question      (:question params)
           :choices       (:choices params)
           :allowFreeform (:allowFreeform params)})))))

  ;; Hooks invoke requests from server
  (rpc/set-request-handler!
   rpc-client "hooks.invoke"
   (fn [params]
     (let [session-id (:sessionId params)
           hook-type  (:hookType params)
           input      (:input params)
           state      @client-atom
           session-atom (get-in state [:sessions session-id])]
       (if-not session-atom
         {:output nil}
         (let [output (session/handle-hooks-invoke! session-atom hook-type input)]
           {:output output})))))

  ;; Exit plan mode requests from server
  (rpc/set-request-handler!
   rpc-client "exitPlanMode.request"
   (fn [params]
     (let [session-id (:sessionId params)
           state      @client-atom
           session-atom (get-in state [:sessions session-id])]
       (if-not session-atom
         {:result {:approved true}}
         (try
           {:result (session/handle-exit-plan-mode! session-atom params)}
           (catch Exception _
             {:result {:approved true}}))))))

  ;; Session filesystem requests from server
  (doseq [[method-name fs-key args-fn]
          [["sessionFs.readFile"         :read-file          (fn [p] [(:sessionId p) (:path p)])]
           ["sessionFs.writeFile"        :write-file         (fn [p] [(:sessionId p) (:path p) (:content p)])]
           ["sessionFs.appendFile"       :append-file        (fn [p] [(:sessionId p) (:path p) (:content p)])]
           ["sessionFs.exists"           :exists?            (fn [p] [(:sessionId p) (:path p)])]
           ["sessionFs.stat"             :stat               (fn [p] [(:sessionId p) (:path p)])]
           ["sessionFs.mkdir"            :mkdir              (fn [p] [(:sessionId p) (:path p) (:recursive p)])]
           ["sessionFs.readdir"          :readdir            (fn [p] [(:sessionId p) (:path p)])]
           ["sessionFs.readdirWithTypes" :readdir-with-types (fn [p] [(:sessionId p) (:path p)])]
           ["sessionFs.rm"               :rm                 (fn [p] [(:sessionId p) (:path p) (:recursive p)])]
           ["sessionFs.rename"           :rename             (fn [p] [(:sessionId p) (:oldPath p) (:newPath p)])]]]
    (rpc/set-request-handler!
     rpc-client method-name
     (fn [params]
       (let [session-id (:sessionId params)
             state      @client-atom
             session-atom (get-in state [:sessions session-id])
             fs-handler (when session-atom
                          (get-in @session-atom [:session-fs-handler fs-key]))]
         (if-not fs-handler
           (throw (ex-info (str "No sessionFs handler registered for " method-name)
                           {:session-id session-id}))
           (apply fs-handler (args-fn params)))))))

  ;; GitHub token provider requests.  The registry is client-level, so a token
  ;; requested while session.create is still in flight is answered.
  (rpc/set-request-handler!
   rpc-client "gitHubToken.getToken"
   (fn [params]
     (let [registration-id (:registrationId params)
           provider        (get @(:token-providers @client-atom) registration-id)]
       (if-not provider
         (throw (ex-info (str "Unknown GitHub token provider registration: " registration-id)
                         {:registration-id registration-id}))
         (types/github-token-response
          (provider {:registration-id registration-id
                     :host            (:host params)
                     :session-id      (:sessionId params)
                     :reason          (:reason params)}))))))

  ;; Shell authentication requests.  The handler runs on a worker so that a
  ;; shellAuthentication.cancel for the same request can answer "cancel".
  (rpc/set-request-handler!
   rpc-client "shellAuthentication.request"
   (fn [params]
     (let [session-atom (get-in @client-atom [:sessions (:sessionId params)])
           pending      (:shell-auth-requests @client-atom)
           request-key  [(:sessionId params) (:requestId params)]
           reply        (promise)]
       (if-not session-atom
         {:action "cancel"}
         (do
           (swap! pending assoc request-key reply)
           (try
             (let [worker (future
                            (deliver reply
                                     (try
                                       (session/handle-shell-authentication-request!
                                        session-atom
                                        (types/shell-authentication-request params))
                                       (catch Throwable _
                                         {:action "cancel"}))))]
               (try
                 @reply
                 (finally
                   (future-cancel worker))))
             (finally
               (swap! pending dissoc request-key))))))))

  (rpc/set-request-handler!
   rpc-client "shellAuthentication.cancel"
   (fn [params]
     (when-let [reply (get @(:shell-auth-requests @client-atom)
                           [(:sessionId params) (:requestId params)])]
       (deliver reply {:action "cancel"}))
     {}))

  ;; Memory store requests (session option :store-memory); replies null.
  (rpc/set-request-handler!
   rpc-client "memoryStore.store"
   (fn [params]
     (let [session-atom (get-in @client-atom [:sessions (:sessionId params)])]
       (if-not session-atom
         (throw (ex-info "Memory store owner is unavailable"
                         {:session-id (:sessionId params)}))
         (do
           (session/handle-memory-store! session-atom (types/stored-memory params))
           rpc/null-result)))))

  ;; Network callbacks (client option :network).  Only registered callbacks
  ;; get a handler; tokens and passwords are never logged.
  (let [network (:network (:options @client-atom))]
    (when-let [resolve-proxy (:resolve-proxy network)]
      (rpc/set-request-handler!
       rpc-client "network.resolveProxy"
       (fn [params]
         (let [proxy (resolve-proxy {:url (:url params)})]
           (if (string? proxy) {:proxy proxy} {})))))
    (when-let [authenticate-negotiate (:authenticate-negotiate network)]
      (rpc/set-request-handler!
       rpc-client "network.authenticateNegotiate"
       (fn [params]
         (let [token (authenticate-negotiate {:spn (:spn params)})]
           (if (string? token) {:token token} {})))))
    (when-let [authenticate-basic (:authenticate-basic network)]
      (rpc/set-request-handler!
       rpc-client "network.authenticateBasic"
       (fn [params]
         (let [credentials (authenticate-basic
                            {:proxy             (:proxy params)
                             :realm             (:realm params)
                             :previous-rejected (true? (:previousRejected params))})]
           (if (map? credentials)
             (cond-> {}
               (:username credentials) (assoc :username (:username credentials))
               (:password credentials) (assoc :password (:password credentials)))
             {})))))))

(defn- connect-stdio!
  "Connect to CLI via stdio pipes."
  [client-atom ^Process process]
  (let [in-stream  (.getInputStream process)
        out-stream (.getOutputStream process)
        rpc-client (rpc/create-client in-stream out-stream)]
    (attach-connection-handlers! client-atom rpc-client)
    (rpc/start! rpc-client)
    (swap! client-atom assoc :rpc-client rpc-client)))

(defn- connect-tcp!
  "Connect to CLI via TCP."
  [client-atom ^String host ^long port]
  (let [{:keys [client socket]} (rpc/create-tcp-client host port)]
    (attach-connection-handlers! client-atom client)
    (rpc/start! client)
    (swap! client-atom assoc :rpc-client client :socket socket)))

(defn- verify-protocol-version!
  "Verify that the server's protocol version matches the SDK's expected version."
  [client-atom]
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "ping" {:message "version-check"})
        server-version (:protocolVersion result)
        expected   (proto/get-sdk-protocol-version)]
    (when (nil? server-version)
      (throw (ex-info (str "SDK protocol version mismatch: SDK supports version " expected
                           ", but server does not report a protocol version. "
                           "Please update your server to ensure compatibility.")
                      {:expected expected})))
    (when (not= server-version expected)
      (throw (ex-info (str "SDK protocol version mismatch: SDK supports version " expected
                           ", but server reports version " server-version ". "
                           "Please update your SDK or server to ensure compatibility.")
                      {:expected expected :actual server-version})))))

(defn- client-info-wire
  "The clientInfo object for the connect handshake, or nil when every field
  is empty.  Accepts a `copilot.types/client-info` map or kebab-case keys."
  [info]
  (when (map? info)
    (let [wire (reduce (fn [m [wire-key kebab-key]]
                         (let [v (or (get info wire-key) (get info kebab-key))]
                           (if (and (string? v) (not= "" v)) (assoc m wire-key v) m)))
                       {}
                       [[:editorName       :editor-name]
                        [:editorVersion    :editor-version]
                        [:extensionName    :extension-name]
                        [:extensionVersion :extension-version]])]
      (when (seq wire) wire))))

(defn- send-client-info!
  "Declare the integrating application (clientInfo) on a `connect` request
  before the protocol check.  Skipped when every clientInfo field is empty;
  older CLIs without `connect` are tolerated."
  [client-atom]
  (when-let [info (client-info-wire (:client-info (:options @client-atom)))]
    (try
      (rpc/request! (:rpc-client @client-atom) "connect" {:clientInfo info})
      (catch clojure.lang.ExceptionInfo e
        (let [{:keys [code message]} (ex-data e)]
          (when-not (or (= -32601 code)
                        (and (string? message)
                             (.contains ^String message "Unhandled method connect")))
            (throw e)))))))

(defn- register-network-provider!
  "Register the client network callbacks (network.setProvider) after the
  protocol check."
  [client-atom]
  (when-let [params (types/network-set-provider-params (:network (:options @client-atom)))]
    (rpc/request! (:rpc-client @client-atom) "network.setProvider" params)))

;; ============================================================================
;; Public API
;; ============================================================================

(defn start!
  "Start the CLI server and establish a connection.

  If the client was created with :cli-url, only establishes the connection.
  Otherwise spawns the CLI server process and connects via stdio or TCP.

  This is called automatically when creating a session if :auto-start is
  true (the default)."
  [client-atom]
  (when-not (= :connected (:state @client-atom))
    (let [state @client-atom]
      (swap! client-atom assoc :state :connecting)
      (try
        (if (:external? state)
          ;; External server - just connect
          (connect-tcp! client-atom (:host state) (:port state))
          ;; Spawn CLI process
          (let [process (spawn-cli-process (:options state))
                use-stdio (if (contains? (:options state) :use-stdio)
                            (:use-stdio (:options state))
                            true)]
            (swap! client-atom assoc :process process)
            (if use-stdio
              (connect-stdio! client-atom process)
              ;; TCP mode: use the configured port
              (let [port (or (:port (:options state)) 0)]
                (when (zero? port)
                  (throw (ex-info "TCP mode requires a port to be specified" {})))
                (Thread/sleep 2000) ;; Wait for server to start
                (connect-tcp! client-atom "localhost" port)))))
        ;; Optional connect handshake (clientInfo), then verify protocol version
        (send-client-info! client-atom)
        (verify-protocol-version! client-atom)
        ;; Register client network callbacks if configured
        (register-network-provider! client-atom)
        ;; Register session filesystem provider if configured
        (when-let [fs-config (:session-fs (:options @client-atom))]
          (let [rpc-client (:rpc-client @client-atom)]
            (rpc/request! rpc-client "sessionFs.setProvider"
                          {:initialCwd       (:initialCwd fs-config)
                           :sessionStatePath (:sessionStatePath fs-config)
                           :conventions      (:conventions fs-config)})))
        (swap! client-atom assoc :state :connected)
        (catch Exception e
          (swap! client-atom assoc :state :error)
          (throw e))))))

(defn- release-client-callbacks!
  "Forget registered token providers and cancel pending shell authentication
  requests."
  [state]
  (some-> (:token-providers state) (reset! {}))
  (doseq [reply (some-> (:shell-auth-requests state) deref vals)]
    (deliver reply {:action "cancel"}))
  (some-> (:shell-auth-requests state) (reset! {})))

(defn stop!
  "Stop the CLI server and close all active sessions.

  Returns a vector of error messages encountered during cleanup."
  [client-atom]
  (let [errors (atom [])
        state  @client-atom]
    ;; Destroy all sessions
    (doseq [[session-id session-atom] (:sessions state)]
      (try
        (session/destroy! session-atom)
        (catch Exception e
          (swap! errors conj (str "Failed to destroy session " session-id ": " (.getMessage e))))))
    (swap! client-atom assoc :sessions {})
    (release-client-callbacks! state)

    ;; Stop RPC client
    (when-let [rpc (:rpc-client state)]
      (try
        (rpc/stop! rpc)
        (catch Exception e
          (swap! errors conj (str "Failed to stop RPC client: " (.getMessage e))))))
    (swap! client-atom assoc :rpc-client nil)

    ;; Clear models cache
    (swap! client-atom assoc :models-cache nil)

    ;; Close TCP socket if present
    (when-let [^Socket socket (:socket state)]
      (try
        (.close socket)
        (catch Exception e
          (swap! errors conj (str "Failed to close socket: " (.getMessage e))))))
    (swap! client-atom assoc :socket nil)

    ;; Kill CLI process (only if we spawned it)
    (when (and (not (:external? state)) (:process state))
      (try
        (.destroyForcibly ^Process (:process state))
        (catch Exception e
          (swap! errors conj (str "Failed to kill CLI process: " (.getMessage e))))))
    (swap! client-atom assoc :process nil :state :disconnected :port nil)

    @errors))

(defn force-stop!
  "Forcefully stop the CLI server without graceful cleanup."
  [client-atom]
  (swap! client-atom assoc :sessions {})
  (release-client-callbacks! @client-atom)
  (when-let [rpc (:rpc-client @client-atom)]
    (try (rpc/stop! rpc) (catch Exception _)))
  (swap! client-atom assoc :rpc-client nil :models-cache nil)
  (when-let [^Socket socket (:socket @client-atom)]
    (try (.close socket) (catch Exception _)))
  (swap! client-atom assoc :socket nil)
  (when-let [^Process process (:process @client-atom)]
    (when-not (:external? @client-atom)
      (try (.destroyForcibly process) (catch Exception _))))
  (swap! client-atom assoc :process nil :state :disconnected :port nil)
  nil)

(defn- ensure-connected!
  "Ensure the client is connected, auto-starting if configured."
  [client-atom]
  (when-not (= :connected (:state @client-atom))
    (let [auto-start (if (contains? (:options @client-atom) :auto-start)
                       (:auto-start (:options @client-atom))
                       true)]
      (if auto-start
        (start! client-atom)
        (throw (ex-info "Client not connected. Call start! first." {}))))))

(defn- validate-session-config!
  "Validate a create/resume config against the client mode and option rules."
  [client-atom config]
  (when (and (:github-token-provider config) (:github-token config))
    (throw (ex-info ":github-token-provider and :github-token are mutually exclusive"
                    {:keys [:github-token-provider :github-token]})))
  (when (and (= :empty (:mode @client-atom)) (nil? (:available-tools config)))
    (throw (ex-info (str "Client mode \"empty\" requires :available-tools on every "
                         "create-session! and resume-session! call")
                    {:mode "empty"}))))

(defn- register-token-provider!
  "Register the session's :github-token-provider and return its registration
  id (sent as github.tokenProviderRegistrationId), or nil."
  [client-atom config]
  (when-let [provider (:github-token-provider config)]
    (let [registration-id (str (UUID/randomUUID))]
      (swap! (:token-providers @client-atom) assoc registration-id provider)
      registration-id)))

(defn- unregister-token-provider!
  [client-atom registration-id]
  (when registration-id
    (swap! (:token-providers @client-atom) dissoc registration-id)))

(defn- request-session!
  "Send session.create / session.resume.  A refusal that carries a
  policyHelperFailure is rethrown with the parsed failure under
  :policy-helper-failure in ex-data (see `copilot.types/policy-helper-failure`)."
  [rpc-client method params]
  (try
    (rpc/request! rpc-client method params)
    (catch clojure.lang.ExceptionInfo e
      (if-let [failure (types/policy-helper-failure e)]
        (throw (ex-info (.getMessage e)
                        (assoc (ex-data e) :policy-helper-failure failure)
                        e))
        (throw e)))))

(defn- open-session!
  "Build the grouped protocol 4 payload, send `method`, and return the
  response.  The token provider is registered first so that token requests
  raised during the call are answered; it is unregistered if the call fails."
  [client-atom method flat-params resume? registration-id]
  (let [params (-> flat-params
                   (cond-> registration-id
                     (assoc :gitHubTokenProviderRegistrationId registration-id))
                   (group-session-params resume?)
                   (apply-mode-defaults (:mode @client-atom)))]
    (try
      (request-session! (:rpc-client @client-atom) method params)
      (catch Exception e
        (unregister-token-provider! client-atom registration-id)
        (throw e)))))

(defn- register-session-callbacks!
  "Install the v3 session callbacks (shell authentication, memory store) and
  the cleanup that releases the token provider and the client's reference."
  [client-atom sess session-id config registration-id]
  (when-let [h (:on-shell-authentication-request config)]
    (session/register-shell-authentication-handler! sess h))
  (when-let [f (:store-memory config)]
    (session/register-memory-store! sess f))
  (session/register-cleanup!
   sess
   (fn []
     (unregister-token-provider! client-atom registration-id)
     (swap! client-atom update :sessions
            (fn [sessions]
              (if (identical? sess (get sessions session-id))
                (dissoc sessions session-id)
                sessions))))))

(defn create-session!
  "Create a new conversation session with the Copilot CLI.

  `config` - session configuration map (see `copilot.types/session-config`)

  Returns a session atom that can be used with `copilot.session` functions."
  [client-atom & {:as config}]
  (let [config (or config {})]
    (ensure-connected! client-atom)
    (validate-session-config! client-atom config)
    (let [rpc-client      (:rpc-client @client-atom)
          registration-id (register-token-provider! client-atom config)
          response   (open-session! client-atom "session.create"
                                    (make-session-create-params config)
                                    false registration-id)
          session-id (or (:sessionId response) (:session-id config))
          workspace  (:workspacePath response)
          sess       (session/create-session session-id rpc-client workspace)]
      ;; Register tools
      (when-let [tools (:tools config)]
        (session/register-tools! sess tools))
      ;; Register permission handler
      (when-let [h (:on-permission-request config)]
        (session/register-permission-handler! sess h))
      ;; Register user input handler
      (when-let [h (:on-user-input-request config)]
        (session/register-user-input-handler! sess h))
      ;; Register hooks
      (when-let [h (:hooks config)]
        (session/register-hooks! sess h))
      ;; Register session filesystem handler if client has sessionFs configured
      (when-let [fs-config (:session-fs (:options @client-atom))]
        (when-let [create-handler (:create-session-fs-handler config)]
          (session/register-session-fs-handler! sess (create-handler))))
      ;; Register trace context provider
      (when-let [p (:on-get-trace-context (:options @client-atom))]
        (session/register-trace-context-provider! sess p))
      ;; Shell authentication, memory store, and close-time cleanup
      (register-session-callbacks! client-atom sess session-id config registration-id)
      ;; Store session in client
      (swap! client-atom assoc-in [:sessions session-id] sess)
      sess)))

(defn resume-session!
  "Resume an existing conversation session.

  `session-id` - the ID of the session to resume
  `config`     - optional resume configuration map

  Returns a session atom."
  [client-atom session-id & {:as config}]
  (let [config (or config {})]
    (ensure-connected! client-atom)
    (validate-session-config! client-atom config)
    (let [rpc-client      (:rpc-client @client-atom)
          registration-id (register-token-provider! client-atom config)
          response   (open-session! client-atom "session.resume"
                                    (-> (make-session-create-params config)
                                        (assoc :sessionId session-id))
                                    true registration-id)
          resumed-id (or (:sessionId response) session-id)
          workspace  (:workspacePath response)
          sess       (session/create-session resumed-id rpc-client workspace)]
      (when-let [tools (:tools config)]
        (session/register-tools! sess tools))
      (when-let [h (:on-permission-request config)]
        (session/register-permission-handler! sess h))
      (when-let [h (:on-user-input-request config)]
        (session/register-user-input-handler! sess h))
      (when-let [h (:hooks config)]
        (session/register-hooks! sess h))
      ;; Register session filesystem handler if client has sessionFs configured
      (when-let [fs-config (:session-fs (:options @client-atom))]
        (when-let [create-handler (:create-session-fs-handler config)]
          (session/register-session-fs-handler! sess (create-handler))))
      ;; Register trace context provider
      (when-let [p (:on-get-trace-context (:options @client-atom))]
        (session/register-trace-context-provider! sess p))
      ;; Shell authentication, memory store, and close-time cleanup
      (register-session-callbacks! client-atom sess resumed-id config registration-id)
      (swap! client-atom assoc-in [:sessions resumed-id] sess)
      sess)))

(defn clear-managed-settings-cache!
  "Clear the runtime's cached managed (enterprise) settings
  (`managedSettings.clearCache`) so they are fetched again."
  [client-atom]
  (ensure-connected! client-atom)
  (rpc/request! (:rpc-client @client-atom) "managedSettings.clearCache" {})
  nil)

(defn get-mode
  "Get the client mode: \"coding-agent\" or \"empty\"."
  [client-atom]
  (name (:mode @client-atom)))

(defn get-state
  "Get the current connection state: :disconnected :connecting :connected :error"
  [client-atom]
  (:state @client-atom))

(defn ping!
  "Send a ping request to the server.

  Returns {:message ... :timestamp ... :protocol-version ...}"
  [client-atom & {:keys [message]}]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "ping" {:message (or message "")})]
    (types/ping-response result)))

(defn get-status!
  "Get CLI status including version and protocol information."
  [client-atom]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "status.get" {})]
    (types/get-status-response result)))

(defn get-auth-status!
  "Get current authentication status."
  [client-atom]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "auth.getStatus" {})]
    (types/get-auth-status-response result)))

(defn list-models!
  "List available models. Results are cached after the first call.

  Returns a vector of model-info maps."
  [client-atom]
  (ensure-connected! client-atom)
  (if-let [cached (:models-cache @client-atom)]
    cached
    (let [rpc-client (:rpc-client @client-atom)
          result     (rpc/request! rpc-client "models.list" {})
          models     (mapv types/model-info (:models result))]
      (swap! client-atom assoc :models-cache models)
      models)))

(defn get-session-metadata!
  "Get metadata for a session by ID.

  Returns the metadata map, or nil if the session is not found."
  [client-atom session-id]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.getMetadata" {:sessionId session-id})]
    result))

(defn get-last-session-id!
  "Get the ID of the most recently updated session."
  [client-atom]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.getLastId" {})]
    (:sessionId result)))

(defn delete-session!
  "Delete a session and its data from disk."
  [client-atom session-id]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.delete" {:sessionId session-id})]
    (when-not (:success result)
      (throw (ex-info (str "Failed to delete session " session-id ": "
                           (or (:error result) "Unknown error"))
                      {:session-id session-id})))
    (swap! client-atom update :sessions dissoc session-id)
    nil))

(defn list-sessions!
  "List all available sessions.

  Returns a vector of session-metadata maps."
  [client-atom]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.list" {})]
    (mapv types/session-metadata (:sessions result))))

(defn get-foreground-session-id!
  "Get the foreground session ID (TUI+server mode only)."
  [client-atom]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.getForeground" {})]
    (:sessionId result)))

(defn set-foreground-session-id!
  "Set the foreground session (TUI+server mode only)."
  [client-atom session-id]
  (ensure-connected! client-atom)
  (let [rpc-client (:rpc-client @client-atom)
        result     (rpc/request! rpc-client "session.setForeground"
                                 {:sessionId session-id})]
    (when-not (:success result)
      (throw (ex-info (or (:error result) "Failed to set foreground session")
                      {:session-id session-id})))
    nil))

(defn on-lifecycle!
  "Subscribe to session lifecycle events.

  `handler` - (fn [event]) where event is a session-lifecycle-event map.
  Returns a zero-arg function that unsubscribes the handler."
  [client-atom handler]
  (let [handlers-atom (:lifecycle-handlers @client-atom)]
    (swap! handlers-atom conj handler)
    (fn [] (swap! handlers-atom disj handler))))

(defn on-lifecycle-type!
  "Subscribe to a specific session lifecycle event type.

  `event-type` - string like \"session.created\", \"session.deleted\", etc.
  `handler`    - (fn [event])
  Returns an unsubscribe function."
  [client-atom event-type handler]
  (on-lifecycle! client-atom
                 (fn [event]
                   (when (= event-type (:type event))
                     (handler event)))))
