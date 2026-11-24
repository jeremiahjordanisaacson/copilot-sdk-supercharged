;;;; ---------------------------------------------------------------------------
;;;;  Copyright (c) Microsoft Corporation. All rights reserved.
;;;; ---------------------------------------------------------------------------
;;;;
;;;; Copilot Session - represents a single conversation session with the
;;;; Copilot CLI.

(ns copilot.session
  "A CopilotSession maintains conversation state, handles events, and manages
  tool execution.  Sessions are created via `copilot.client/create-session!`
  or resumed via `copilot.client/resume-session!`."
  (:require [copilot.json-rpc :as rpc]
            [copilot.types :as types]))

;; ============================================================================
;; Session construction
;; ============================================================================

(defn create-session
  "Create a new session atom.  Internal -- use `copilot.client/create-session!`.

  Returns an atom containing the session state map."
  [session-id rpc-client workspace-path]
  (atom {:session-id         session-id
         :rpc-client         rpc-client
         :workspace-path     workspace-path
         :event-handlers     #{}      ;; set of (fn [event])
         :typed-handlers     {}       ;; {event-type #{(fn [event])}}
         :tool-handlers      {}       ;; {tool-name handler-fn}
         :permission-handler nil      ;; (fn [request invocation])
         :user-input-handler nil      ;; (fn [request invocation])
         :hooks              nil      ;; session-hooks map
         :session-fs-handler nil      ;; session filesystem provider map
         :shell-auth-handler nil      ;; (fn [request]) -> value string or nil
         :memory-store       nil      ;; (fn [memory])
         :cleanup-fns        []}))    ;; run once when the session is closed

;; ============================================================================
;; Event subscription
;; ============================================================================

(defn on!
  "Subscribe to all session events.

  `handler` - (fn [event]) called for every event.
  Returns a zero-arg unsubscribe function."
  [session-atom handler]
  (swap! session-atom update :event-handlers conj handler)
  (fn [] (swap! session-atom update :event-handlers disj handler)))

(defn on-type!
  "Subscribe to a specific event type.

  `event-type` - string, e.g. \"assistant.message\", \"session.idle\"
  `handler`    - (fn [event])
  Returns a zero-arg unsubscribe function."
  [session-atom event-type handler]
  (swap! session-atom update-in [:typed-handlers event-type]
         (fnil conj #{}) handler)
  (fn []
    (swap! session-atom update-in [:typed-handlers event-type] disj handler)))

(defn dispatch-event!
  "Dispatch an event to all registered handlers.  Internal."
  [session-atom event]
  (let [state @session-atom
        event-type (:type event)]
    ;; Typed handlers
    (when-let [handlers (get-in state [:typed-handlers event-type])]
      (doseq [h handlers]
        (try (h event) (catch Exception _))))
    ;; Wildcard handlers
    (doseq [h (:event-handlers state)]
      (try (h event) (catch Exception _)))))

;; ============================================================================
;; Tool registration
;; ============================================================================

(defn register-tools!
  "Register tool handlers for this session.

  `tools` - vector of tool maps (see `copilot.types/tool`)"
  [session-atom tools]
  (swap! session-atom assoc :tool-handlers
         (into {} (map (fn [t] [(:name t) (:handler t)])) tools)))

(defn get-tool-handler
  "Get the handler function for a named tool, or nil."
  [session-atom tool-name]
  (get-in @session-atom [:tool-handlers tool-name]))

;; ============================================================================
;; Permission handling
;; ============================================================================

(defn register-permission-handler!
  "Register a permission handler.

  `handler` - (fn [request invocation] -> permission-result-map)"
  [session-atom handler]
  (swap! session-atom assoc :permission-handler handler))

(defn handle-permission-request!
  "Handle an incoming permission request.  Internal."
  [session-atom request]
  (let [handler (:permission-handler @session-atom)]
    (if-not handler
      {:kind "denied-no-approval-rule-and-could-not-request-from-user"}
      (try
        (handler request {:session-id (:session-id @session-atom)})
        (catch Exception _
          {:kind "denied-no-approval-rule-and-could-not-request-from-user"})))))

;; ============================================================================
;; User input handling
;; ============================================================================

(defn register-user-input-handler!
  "Register a user input handler.

  `handler` - (fn [request invocation] -> {:answer str :wasFreeform bool})
              `request` has :question, :choices, :allowFreeform and, when the
              runtime sends them, :requestId, :_meta, and
              :imageAttachmentConstraints ({:allowedMimeTypes :maxBytesPerImage
              :maxCount}).  The reply may add :attachments (see
              `copilot.types/user-input-image-attachment`)."
  [session-atom handler]
  (swap! session-atom assoc :user-input-handler handler))

(defn handle-user-input-request!
  "Handle an incoming user input request.  Internal."
  [session-atom request]
  (let [handler (:user-input-handler @session-atom)]
    (if-not handler
      (throw (ex-info "User input requested but no handler registered" {}))
      (handler request {:session-id (:session-id @session-atom)}))))

;; ============================================================================
;; Hooks handling
;; ============================================================================

(defn register-hooks!
  "Register session hooks.

  `hooks` - map with optional keys:
    :on-pre-tool-use, :on-post-tool-use, :on-pre-mcp-tool-call,
    :on-user-prompt-submitted, :on-user-prompt-transformed,
    :on-session-start, :on-session-end, :on-error-occurred,
    :on-subagent-start, :on-subagent-stop"
  [session-atom hooks]
  (swap! session-atom assoc :hooks hooks))

(defn register-session-fs-handler!
  "Register a session filesystem handler.

  `handler` - map with function-valued keys:
    :read-file, :write-file, :append-file, :exists?, :stat,
    :mkdir, :readdir, :readdir-with-types, :rm, :rename"
  [session-atom handler]
  (swap! session-atom assoc :session-fs-handler handler))

(defn register-trace-context-provider!
  "Register a trace context provider.  `provider` is a zero-arg fn returning
  {:traceparent s :tracestate s} (see `copilot.types/trace-context`); the
  values are attached to every `send!`."
  [session-atom provider]
  (swap! session-atom assoc :trace-context-provider provider))

(defn register-exit-plan-mode-handler!
  "Register an exit plan mode handler.

  `handler` - (fn [request invocation] -> {:approved bool ...})"
  [session-atom handler]
  (swap! session-atom assoc :exit-plan-mode-handler handler))

(defn handle-exit-plan-mode!
  "Handle an incoming exitPlanMode.request.  Internal.  Approves when no
  handler is registered."
  [session-atom request]
  (if-let [handler (:exit-plan-mode-handler @session-atom)]
    (handler request {:session-id (:session-id @session-atom)})
    {:approved true}))

(defn register-shell-authentication-handler!
  "Register the shell authentication handler (session option
  :on-shell-authentication-request).

  `handler` - (fn [request] -> value string to accept, or nil to cancel);
              `request` is a `copilot.types/shell-authentication-request` map."
  [session-atom handler]
  (swap! session-atom assoc :shell-auth-handler handler))

(defn handle-shell-authentication-request!
  "Invoke the shell authentication handler and return the reply map.
  Internal.  Cancels when no handler is registered or the handler throws."
  [session-atom request]
  (if-let [handler (:shell-auth-handler @session-atom)]
    (try
      (types/shell-authentication-response (handler request))
      (catch Exception _
        {:action "cancel"}))
    {:action "cancel"}))

(defn register-memory-store!
  "Register the memory store callback (session option :store-memory).

  `store-fn` - (fn [memory]) where `memory` is a `copilot.types/stored-memory`
               map; throw to report a failed store."
  [session-atom store-fn]
  (swap! session-atom assoc :memory-store store-fn))

(defn handle-memory-store!
  "Invoke the memory store callback.  Internal."
  [session-atom memory]
  (if-let [store-fn (:memory-store @session-atom)]
    (do (store-fn memory) nil)
    (throw (ex-info "Memory store owner is unavailable"
                    {:session-id (:session-id @session-atom)}))))

(defn register-cleanup!
  "Register a zero-arg fn run once when the session is destroyed or
  detached.  Internal."
  [session-atom f]
  (swap! session-atom update :cleanup-fns conj f))

(defn- run-cleanup!
  "Run and clear the registered cleanup fns (each runs at most once)."
  [session-atom]
  (let [[old _] (swap-vals! session-atom assoc :cleanup-fns [])]
    (doseq [f (:cleanup-fns old)]
      (try (f) (catch Exception _)))))

(defn handle-hooks-invoke!
  "Handle an incoming hooks invoke request.  Internal."
  [session-atom hook-type input]
  (let [hooks (:hooks @session-atom)]
    (when hooks
      (let [handler-map {"preToolUse"            :on-pre-tool-use
                         "postToolUse"           :on-post-tool-use
                         "preMcpToolCall"        :on-pre-mcp-tool-call
                         "userPromptSubmitted"   :on-user-prompt-submitted
                         "userPromptTransformed" :on-user-prompt-transformed
                         "sessionStart"          :on-session-start
                         "sessionEnd"            :on-session-end
                         "errorOccurred"         :on-error-occurred
                         "subagentStart"         :on-subagent-start
                         "subagentStop"          :on-subagent-stop}
            hook-key    (get handler-map hook-type)
            handler     (when hook-key (get hooks hook-key))]
        (when handler
          (try
            (handler input {:session-id (:session-id @session-atom)})
            (catch Exception _ nil)))))))

;; ============================================================================
;; Messaging
;; ============================================================================

(defn- response-schema-format
  "Wrap a ResponseSchema (JSON Schema map) as the json_schema responseFormat
  of session.send."
  [schema]
  {:type       "json_schema"
   :jsonSchema {:name "response" :strict true :schema schema}})

(defn send!
  "Send a message to this session.

  `options` - message options map with :prompt and optional :attachments, :mode,
              :response-format, :image-options, :agent-mode, :display-prompt,
              :request-headers, :source (\"user\", \"system\", or \"agent-<id>\";
              see `copilot.types/message-source`), :response-schema (JSON Schema
              map for structured output; takes precedence over :response-format),
              :client-turn-id (opaque caller-chosen turn identifier)

  Returns the message ID string."
  [session-atom options]
  (let [state      @session-atom
        rpc-client (:rpc-client state)
        trace      (when-let [provider (:trace-context-provider state)]
                     (try (provider) (catch Exception _ nil)))
        params     (cond-> {:sessionId   (:session-id state)
                            :prompt      (:prompt options)
                            :attachments (:attachments options)
                            :mode        (:mode options)}
                     (:response-format options)
                     (assoc :responseFormat (:response-format options))
                     (:response-schema options)
                     (assoc :responseFormat (response-schema-format (:response-schema options)))
                     (:image-options options)
                     (assoc :imageOptions (:image-options options))
                     (:agent-mode options)
                     (assoc :agentMode (:agent-mode options))
                     (:display-prompt options)
                     (assoc :displayPrompt (:display-prompt options))
                     (:request-headers options)
                     (assoc :requestHeaders (:request-headers options))
                     (:source options)
                     (assoc :source (:source options))
                     (:client-turn-id options)
                     (assoc :clientTurnId (:client-turn-id options))
                     (:traceparent trace)
                     (assoc :traceparent (:traceparent trace))
                     (:tracestate trace)
                     (assoc :tracestate (:tracestate trace)))
        response   (rpc/request! rpc-client "session.send" params)]
    (:messageId response)))

(defn send-and-wait!
  "Send a message and wait until the session becomes idle.

  Returns the last assistant.message event, or nil if none received.

  `options`    - message options map with :prompt
  `timeout-ms` - timeout in milliseconds (default 60000)"
  ([session-atom options]
   (send-and-wait! session-atom options 60000))
  ([session-atom options timeout-ms]
   (let [idle-promise     (promise)
         error-promise    (promise)
         last-msg         (atom nil)
         ;; Subscribe BEFORE sending to avoid race
         unsub (on! session-atom
                    (fn [event]
                      (case (:type event)
                        "assistant.message"
                        (reset! last-msg event)

                        "session.idle"
                        (deliver idle-promise true)

                        "session.error"
                        (deliver error-promise
                                 (ex-info (get-in event [:data :message] "Session error")
                                          {:event event}))
                        ;; default
                        nil)))]
     (try
       (send! session-atom options)
       ;; Wait for idle or error or timeout
       (let [result (deref idle-promise timeout-ms ::timeout)]
         (cond
           (realized? error-promise)
           (throw @error-promise)

           (= result ::timeout)
           (throw (ex-info (str "Timeout after " timeout-ms "ms waiting for session.idle")
                           {:timeout-ms timeout-ms}))

           :else
           @last-msg))
       (finally
         (unsub))))))

;; ============================================================================
;; Session management
;; ============================================================================

(defn session-id
  "Get the session ID."
  [session-atom]
  (:session-id @session-atom))

(defn workspace-path
  "Get the workspace path (when infinite sessions are enabled), or nil."
  [session-atom]
  (:workspace-path @session-atom))

(defn get-messages!
  "Retrieve all events/messages from this session's history."
  [session-atom]
  (let [state      @session-atom
        rpc-client (:rpc-client state)
        response   (rpc/request! rpc-client "session.getMessages"
                                 {:sessionId (:session-id state)})]
    (:events response)))

(defn get-metadata!
  "Retrieve metadata for this session."
  [session-atom]
  (let [state      @session-atom
        rpc-client (:rpc-client state)
        response   (rpc/request! rpc-client "session.getMetadata"
                                 {:sessionId (:session-id state)})]
    response))

(defn destroy!
  "Destroy this session and release resources.

  After calling this, the session can no longer be used."
  [session-atom]
  (let [state @session-atom
        rpc-client (:rpc-client state)]
    (try
      (rpc/request! rpc-client "session.destroy"
                    {:sessionId (:session-id state)})
      (finally
        (run-cleanup! session-atom)
        (swap! session-atom assoc
               :event-handlers #{}
               :typed-handlers {}
               :tool-handlers {}
               :permission-handler nil
               :user-input-handler nil
               :hooks nil
               :session-fs-handler nil
               :shell-auth-handler nil
               :memory-store nil)))
    nil))

(defn detach!
  "Detach from this session (`session.detach`) without deleting it.  The
  server keeps the session state, so it can be resumed later; this client
  stops receiving its events and releases local resources."
  [session-atom]
  (let [state @session-atom]
    (rpc/request! (:rpc-client state) "session.detach"
                  {:sessionId (:session-id state)})
    (run-cleanup! session-atom)
    (swap! session-atom assoc
           :event-handlers #{}
           :typed-handlers {}
           :tool-handlers {}
           :permission-handler nil
           :user-input-handler nil
           :hooks nil
           :session-fs-handler nil
           :shell-auth-handler nil
           :memory-store nil)
    nil))

(defn switch-auto-tier!
  "Change the Auto routing preference without changing the selected model
  (`session.model.switchAutoTier`).  `tier` is \"fast\", \"efficiency\",
  \"balance\", or \"intelligence\" (see `copilot.types/auto-tiers`); nil
  returns to the provider's default Auto routing.  Returns the raw result."
  [session-atom tier]
  (let [state @session-atom]
    (rpc/request! (:rpc-client state) "session.model.switchAutoTier"
                  {:sessionId (:session-id state)
                   :autoTier  (some-> tier name)})))

(defn get-managed-settings!
  "Get the managed (enterprise) settings snapshot in effect
  (`session.managedSettings.get`).  Use `copilot.types/policy-helper-failure`
  on the result to read a reported policy helper failure."
  [session-atom]
  (let [state @session-atom]
    (rpc/request! (:rpc-client state) "session.managedSettings.get"
                  {:sessionId (:session-id state)})))

(defn recheck-policy-helper!
  "Re-run the managed-settings policy helper
  (`session.managedSettings.recheckPolicyHelper`).  Returns the raw result."
  [session-atom]
  (let [state @session-atom]
    (rpc/request! (:rpc-client state) "session.managedSettings.recheckPolicyHelper"
                  {:sessionId (:session-id state)})))

(defn set-permission-mode!
  "Change the permission mode (`session.permissions.setMode`), for example
  \"manual\", \"assisted\", or \"allow-all\".  Returns
  {:success bool :mode str :reason str}; a refused change has :success false
  and, when known, :reason \"managed_policy\"
  (`copilot.types/permission-mode-refusals`)."
  [session-atom mode]
  (let [state @session-atom]
    (types/permissions-set-mode-result
     (rpc/request! (:rpc-client state) "session.permissions.setMode"
                   {:sessionId (:session-id state)
                    :mode      (name mode)}))))

(defn abort!
  "Abort the currently processing message in this session."
  [session-atom]
  (let [state      @session-atom
        rpc-client (:rpc-client state)]
    (rpc/request! rpc-client "session.abort"
                  {:sessionId (:session-id state)})
    nil))
