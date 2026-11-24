# Copilot Supercharged SDK for Clojure

Clojure SDK for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

> **Note:** This SDK is in technical preview and may change in breaking ways.

## Installation

### deps.edn

Add the SDK source path to your project or include it as a local dependency:

```clojure
{:deps {copilot-sdk-supercharged/clojure {:local/root "../copilot-sdk-supercharged/clojure"}}}
```

### Dependencies

The SDK requires:

- **Clojure** >= 1.11.0
- **cheshire** (JSON parsing)
- **core.async** (concurrency)
- **GitHub Copilot CLI** installed and available in PATH (or provide a custom `:cli-path`)

## Quick Start

```clojure
(require '[copilot.client :as client]
         '[copilot.session :as session])

;; Create and start client
(def my-client (client/create-client :log-level "info"))
(client/start! my-client)

;; Create a session
(def sess (client/create-session! my-client :model "gpt-5"))

;; Subscribe to events
(session/on-type! sess "assistant.message"
  (fn [event]
    (println "Assistant:" (get-in event [:data :content]))))

;; Send a message and wait for completion
(def result (session/send-and-wait! sess {:prompt "What is 2+2?"}))
(println "Response:" (get-in result [:data :content]))

;; Clean up
(session/destroy! sess)
(client/stop! my-client)
```

## What's New in v3.1.0

### MCP secret store and plugin extension approvals

Six server-scoped client functions. They require **Copilot CLI 1.0.116 or newer**; an older runtime answers with a JSON-RPC method-not-found error, which is thrown as `ex-info` like any other RPC error.

```clojure
;; MCP secret store.  The value is a credential: it is sent to the runtime and never logged.
(client/mcp-secrets-save! my-client "my-server/api-key" (System/getenv "MY_API_KEY")
                          :config-dir "/path/to/config")   ;; => {:used-keychain true}
(client/mcp-secrets-delete! my-client "my-server/api-key") ;; mcp.secrets.delete
(client/mcp-secrets-delete-server! my-client "my-server")  ;; mcp.secrets.deleteServer

;; Plugin extension approvals (user scoped).  Grant persists a pending extension
;; permission request before you answer it.
(client/extension-approvals-grant! my-client session-id request-id)
;; => {:granted true :approval {:extension-name "plugin:my-plugin:my-extension" ...}}
(client/extension-approvals-list! my-client)               ;; optional :session-id
;; => {:approvals [{:extension-name ... :plugin-name ... :capabilities [...] :granted-at ...
;;                  :installation-key ... :installation-label ...}]}
(client/extension-approvals-revoke! my-client "plugin:my-plugin:my-extension" installation-key)
;; => {:revoked true}                                      ;; optional :session-id
```

| Function | Wire method | Returns |
|----------|-------------|---------|
| `mcp-secrets-save!` | `mcp.secrets.save` | `{:used-keychain bool}` (false means the file backend stored it) |
| `mcp-secrets-delete!` | `mcp.secrets.delete` | nil |
| `mcp-secrets-delete-server!` | `mcp.secrets.deleteServer` | nil |
| `extension-approvals-grant!` | `permissions.extensionApprovals.grant` | `{:granted bool :approval map-or-nil}` |
| `extension-approvals-list!` | `permissions.extensionApprovals.list` | `{:approvals [...]}` |
| `extension-approvals-revoke!` | `permissions.extensionApprovals.revoke` | `{:revoked bool}` |

### Ask-user image attachments

When the runtime accepts images, the request map passed to `:on-user-input-request` carries `:imageAttachmentConstraints` (`{:allowedMimeTypes [...] :maxBytesPerImage n :maxCount n}`); it is absent otherwise. The request also carries `:requestId` and `:_meta` when the runtime sends them. Answer with images through the optional `:attachments` of `types/user-input-response`:

```clojure
(client/create-session! my-client
  :on-user-input-request
  (fn [{:keys [question imageAttachmentConstraints]} _invocation]
    (if imageAttachmentConstraints
      ;; Image-only answer: empty answer and was-freeform true
      (types/user-input-response "" true
        :attachments [(types/user-input-image-attachment screenshot-base64 "image/png"
                                                         :display-name "screenshot.png")])
      (types/user-input-response "yes" true))))
```

`:attachments` (each `{:type "image" :data :mimeType}` plus optional `:description` and `:displayName`) is sent only when set, and is only valid when the request advertised `:imageAttachmentConstraints`; the runtime enforces the size and count limits.

## What's New in v3.0.0

This release speaks **SDK protocol 4** and requires **Copilot CLI 1.0.113 or newer**. Older CLIs are rejected by `start!` with a protocol version mismatch error.

Existing options keep working. You still pass the same kebab-case keys to `create-session!` and `resume-session!`; right before `session.create` / `session.resume` the SDK translates them into the grouped protocol 4 payload (`inference`, `tools`, `permissions`, `interaction`, `events`, `mcp`, `github`, `state`, and so on). A legacy single `:provider` is sent as the named `"default"` provider with Copilot API models disabled, which preserves the v1 BYOK behavior.

### Client options and methods

```clojure
(require '[copilot.client :as client]
         '[copilot.session :as session]
         '[copilot.types :as types])

(def my-client
  (client/create-client
    ;; Declared on the `connect` handshake (empty fields are dropped)
    :client-info (types/client-info :editor-name "Emacs" :editor-version "29.4"
                                    :extension-name "copilot.el" :extension-version "0.3.0")
    ;; "coding-agent" (the default) or "empty"; "copilot-cli" is accepted as an alias
    :mode "coding-agent"
    ;; Network callbacks, registered with network.setProvider after connecting
    :network {:scope "connection"
              :resolve-proxy (fn [{:keys [url]}] "PROXY proxy.example:8080; DIRECT")
              :authenticate-negotiate (fn [{:keys [spn]}] nil) ;; nil = no token
              :authenticate-basic (fn [{:keys [proxy realm previous-rejected]}]
                                    {:username "me" :password (System/getenv "PROXY_PASSWORD")})}))

(client/clear-managed-settings-cache! my-client) ;; managedSettings.clearCache
(client/get-mode my-client)                      ;; => "coding-agent"
```

In the `"empty"` mode every `create-session!` and `resume-session!` must pass `:available-tools` (otherwise an `ex-info` is thrown), and the SDK fills in conservative defaults under your own values: experimental mode, telemetry, the cross-session store, memory, on-demand instruction discovery, repository hooks, and host git operations are off, and MCP OAuth tokens and embeddings are kept in memory.

### Session options

```clojure
(require '[copilot.define-tool :refer [define-tool]])

(def sess
  (client/create-session! my-client
    :model "my-ollama/qwen3"
    ;; Named BYOK providers and models; select a model with "<provider>/<id>"
    :providers [(types/named-provider-config :name "my-ollama" :type :openai
                                             :base-url "http://localhost:11434/v1"
                                             :api-key-command "pass show ollama")]
    :models [(types/provider-model-config :provider "my-ollama" :id "qwen3"
                                          :max-output-tokens 8192)]
    :default-provider "my-ollama"
    :capi (types/capi-config :enabled false)  ;; BYOK-only model catalog
    ;; Tools can be marked idempotent (safe to retry)
    :tools [(define-tool "get_weather" "Get the weather" {:type "object"}
              (fn [args _] "22C, sunny")
              {:idempotent true})]
    :ask-user-variant "elicitation"           ;; or "legacy"
    :sandbox-config (types/sandbox-config :enabled true :allow-bypass true)
    :feature-flags {"some_flag" true}
    :allowed-models ["my-ollama/qwen3"]
    :auth-client-id-metadata-url "https://example.com/oauth/client.json"
    :agent-directories ["./agents"]
    :hook-directories ["./hooks"]
    :trust-working-directory "session"        ;; create only: "session" or "user"
    :image-generation (types/image-generation-config :enabled true :save-to-disk true)
    :autopilot (types/autopilot-options :continuation-enabled true :max-continues 5)
    :model-capabilities (types/model-capabilities-override
                          :limits (types/model-limits-override :max-output-tokens 8192))
    ;; GitHub token provider (mutually exclusive with :github-token).
    ;; Return a token string, {:access-token s :expires-in secs}, or :cancelled.
    ;; The runtime needs more than one hour of remaining lifetime (default 8 hours).
    :github-token-provider (fn [{:keys [host session-id reason]}]
                             {:access-token (System/getenv "GH_TOKEN") :expires-in 28800})
    ;; Shell commands that ask for a password: return the value, or nil to cancel
    :on-shell-authentication-request (fn [{:keys [program prompt input-kind]}]
                                       (when (= "git" program) (System/getenv "GIT_PASSWORD")))
    ;; Memories the agent asks to store (after runtime validation and approval)
    :store-memory (fn [{:keys [subject fact citations reason]}]
                    (println "remember:" subject fact))))

;; Resume-only options: transcript recovery, and an explicit nil clears the default provider
(client/resume-session! my-client (session/session-id sess)
  :allow-transcript-recovery true
  :default-provider nil)
```

The legacy single provider also gained `:api-key-command`, `:model-provider`, `:transport`, and `:headers` in `types/provider-config`.

### Session methods and message options

```clojure
;; Message provenance, opaque turn id, and structured output
(session/send! sess {:prompt         "List three colors as JSON"
                     :source         (types/message-source :user) ;; "user", "system", or (types/message-source :agent "id")
                     :client-turn-id "turn-42"
                     :response-schema {:type "object"
                                       :properties {:colors {:type "array" :items {:type "string"}}}}})

(session/switch-auto-tier! sess "efficiency")  ;; "fast" "efficiency" "balance" "intelligence", or nil
(session/get-managed-settings! sess)           ;; session.managedSettings.get
(session/recheck-policy-helper! sess)          ;; session.managedSettings.recheckPolicyHelper

(let [{:keys [success reason]} (session/set-permission-mode! sess "allow-all")]
  (when (and (not success) (= reason (:managed-policy types/permission-mode-refusals)))
    (println "Blocked by managed policy")))

(session/detach! sess) ;; session.detach: keep the session on the server for a later resume
```

### Hooks, events, and errors

```clojure
;; Subagent lifecycle hooks (delivered through hooks.invoke)
(client/create-session! my-client
  :hooks {:on-subagent-start (fn [input _] {:additionalContext (str "You are " (:agentName input))})
          :on-subagent-stop  (fn [input _] nil)})

;; Live shell output (ephemeral tool.shell_output events)
(session/on-type! sess types/tool-shell-output-event
  (fn [event]
    (let [{:keys [tool-call-id text stream]} (types/tool-shell-output event)]
      (print text))))

;; A session refused by a managed-settings policy helper
(try
  (client/create-session! my-client)
  (catch clojure.lang.ExceptionInfo e
    (when-let [failure (:policy-helper-failure (ex-data e))]
      (println "Policy helper failed:" (:category failure) (:message failure)))))
```

`session.abort`, `session.delete`, and `session.setForeground` failures arrive as JSON-RPC errors and are thrown as `ex-info`.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection. This SDK keeps spawning the CLI (stdio or TCP) or connecting to `:cli-url`.

## Custom Tools

Define tools that the Copilot agent can call back into your process:

### Using the `deftool` macro

```clojure
(require '[copilot.define-tool :refer [deftool]])

(deftool lookup-fact
  "Returns a fun fact about a given topic."
  {:type       "object"
   :properties {:topic {:type        "string"
                        :description "Topic to look up"}}
   :required   ["topic"]}
  [args _invocation]
  (get my-facts (:topic args) "No fact found."))

;; Use in a session
(def sess (client/create-session! my-client :tools [lookup-fact]))
```

### Using the `define-tool` function

```clojure
(require '[copilot.define-tool :refer [define-tool]])

(def my-tool
  (define-tool "get_weather"
               "Get weather for a city"
               {:type       "object"
                :properties {:city {:type "string"}}
                :required   ["city"]}
               (fn [args _invocation]
                 (str "Weather in " (:city args) ": 22C, sunny"))))
```

Tool handlers can return:

- A **string** (wrapped as a success result)
- A **ToolResultObject map** with `:textResultForLlm` and `:resultType` (passed through)
- **Any other value** (JSON-serialised as a success result)
- **nil** (empty success)

Exceptions in handlers are caught and returned as failure results without exposing error details to the LLM.

## API Reference

### copilot.client

#### `create-client`

```clojure
(client/create-client & {:as opts})
```

Create a new CopilotClient (returns an atom).

**Options:**

| Key | Default | Description |
|-----|---------|-------------|
| `:cli-path` | `"copilot"` | Path to CLI executable |
| `:cli-args` | `[]` | Extra CLI arguments |
| `:cwd` | current dir | Working directory for CLI process |
| `:port` | `0` | TCP port (0 = random) |
| `:use-stdio` | `true` | Use stdio transport instead of TCP |
| `:cli-url` | nil | URL of existing server (mutually exclusive with `:cli-path`, `:use-stdio`) |
| `:log-level` | `"info"` | Log level |
| `:auto-start` | `true` | Auto-start on first use |
| `:auto-restart` | `true` | Auto-restart on crash |
| `:env` | nil | Environment variables map |
| `:github-token` | nil | GitHub token for authentication |
| `:use-logged-in-user` | `true` | Use logged-in user auth (false when `:github-token` set) |
| `:client-info` | nil | Application identity sent on the `connect` handshake (`types/client-info`) |
| `:mode` | `"coding-agent"` | `"coding-agent"` or `"empty"` (`"copilot-cli"` is an alias of `"coding-agent"`) |
| `:network` | nil | Network callbacks: `:scope`, `:resolve-proxy`, `:authenticate-negotiate`, `:authenticate-basic` |

#### `start!`

```clojure
(client/start! client-atom)
```

Start the CLI server and establish connection. Called automatically if `:auto-start` is true.

#### `stop!`

```clojure
(client/stop! client-atom) ;; => ["error1" "error2"] or []
```

Stop the server and close all sessions. Returns a vector of error messages.

#### `force-stop!`

```clojure
(client/force-stop! client-atom)
```

Force stop without graceful cleanup.

#### `create-session!`

```clojure
(client/create-session! client-atom & {:as config})
```

Create a new conversation session. Returns a session atom.

**Config keys:**

| Key | Description |
|-----|-------------|
| `:session-id` | Custom session ID |
| `:model` | Model to use (e.g., `"gpt-5"`, `"claude-sonnet-4.5"`) |
| `:reasoning-effort` | `:low`, `:medium`, `:high`, or `:xhigh` |
| `:tools` | Vector of tool maps |
| `:system-message` | System message config map |
| `:available-tools` | Vector of allowed tool names |
| `:excluded-tools` | Vector of disabled tool names |
| `:provider` | Custom provider config (BYOK) |
| `:on-permission-request` | Permission handler function |
| `:on-user-input-request` | User input handler function |
| `:hooks` | Session hooks map |
| `:working-directory` | Working directory for tool operations |
| `:streaming` | Enable streaming deltas |
| `:mcp-servers` | MCP server configurations |
| `:custom-agents` | Custom agent configurations |
| `:infinite-sessions` | Infinite session config |
| `:providers` / `:models` / `:default-provider` / `:capi` | Named BYOK providers and models (`types/named-provider-config`, `types/provider-model-config`, `types/capi-config`) |
| `:github-token-provider` | `(fn [request])` returning a token for `gitHubToken.getToken` |
| `:on-shell-authentication-request` | `(fn [request])` returning the answer string, or nil to cancel |
| `:store-memory` | `(fn [memory])` called for `memoryStore.store` |
| `:ask-user-variant` | `"legacy"` or `"elicitation"` |
| `:sandbox-config` | Sandbox settings, including `:allow-bypass` (`types/sandbox-config`) |
| `:feature-flags` / `:allowed-models` | Feature flag overrides and a model allowlist |
| `:auth-client-id-metadata-url` | MCP OAuth client ID metadata document URL |
| `:agent-directories` / `:hook-directories` | Extra agent and hook directories |
| `:trust-working-directory` | Create only: `"session"` or `"user"` |
| `:allow-transcript-recovery` | Resume only: recover from the transcript |
| `:image-generation` / `:autopilot` | `types/image-generation-config`, `types/autopilot-options` |

#### `resume-session!`

```clojure
(client/resume-session! client-atom session-id & {:as config})
```

Resume an existing session by ID.

#### `ping!`

```clojure
(client/ping! client-atom) ;; => {:message "..." :timestamp 123 :protocol-version 4}
```

#### `list-models!`

```clojure
(client/list-models! client-atom) ;; => [{:id "gpt-5" :name "GPT-5" ...} ...]
```

Results are cached after the first call.

#### `list-sessions!`

```clojure
(client/list-sessions! client-atom) ;; => [{:session-id "..." :summary "..." ...} ...]
```

#### `delete-session!`

```clojure
(client/delete-session! client-atom "session-id")
```

#### `get-last-session-id!`

```clojure
(client/get-last-session-id! client-atom) ;; => "session-id" or nil
```

#### `on-lifecycle!`

```clojure
(def unsub (client/on-lifecycle! client-atom
             (fn [event]
               (println (:type event) (:session-id event)))))
(unsub) ;; unsubscribe
```

#### `on-lifecycle-type!`

```clojure
(def unsub (client/on-lifecycle-type! client-atom "session.created"
             (fn [event]
               (println "New session:" (:session-id event)))))
```

---

### copilot.session

#### `send!`

```clojure
(session/send! sess {:prompt "Hello"}) ;; => "message-id"
```

Send a message. Returns immediately with the message ID.

**Options:**

| Key | Description |
|-----|-------------|
| `:prompt` | Message text (required) |
| `:attachments` | Vector of attachment maps |
| `:mode` | `"enqueue"` or `"immediate"` |
| `:source` | `"user"`, `"system"`, or `"agent-<id>"` (`types/message-source`) |
| `:client-turn-id` | Opaque caller-chosen turn identifier |
| `:response-schema` | JSON Schema map for structured output |

#### `detach!`, `switch-auto-tier!`, `set-permission-mode!`

```clojure
(session/detach! sess)                      ;; session.detach
(session/switch-auto-tier! sess "balance")  ;; session.model.switchAutoTier
(session/set-permission-mode! sess "manual") ;; => {:success true :mode "manual" :reason nil}
(session/get-managed-settings! sess)        ;; session.managedSettings.get
(session/recheck-policy-helper! sess)       ;; session.managedSettings.recheckPolicyHelper
```

#### `send-and-wait!`

```clojure
(session/send-and-wait! sess {:prompt "Hello"})
;; => {:type "assistant.message" :data {:content "..."}} or nil

;; With custom timeout (default 60s)
(session/send-and-wait! sess {:prompt "Hello"} 120000)
```

Send a message and block until the session becomes idle.

#### `on!`

```clojure
(def unsub (session/on! sess (fn [event] (println (:type event)))))
(unsub) ;; unsubscribe
```

Subscribe to all session events.

#### `on-type!`

```clojure
(def unsub (session/on-type! sess "assistant.message"
             (fn [event] (println (get-in event [:data :content])))))
```

Subscribe to a specific event type.

#### `abort!`

```clojure
(session/abort! sess)
```

Abort the currently processing message.

#### `get-messages!`

```clojure
(session/get-messages! sess) ;; => [{:type "..." :data {...}} ...]
```

Get all events/messages from the session history.

#### `destroy!`

```clojure
(session/destroy! sess)
```

Destroy the session and release resources.

#### `session-id`

```clojure
(session/session-id sess) ;; => "session-id-string"
```

#### `workspace-path`

```clojure
(session/workspace-path sess) ;; => "/path/to/workspace" or nil
```

---

### copilot.define-tool

#### `define-tool`

```clojure
(define-tool name description parameters handler-fn)
(define-tool name description parameters handler-fn {:idempotent true})
(define-tool name description handler-fn)  ;; no parameters
```

#### `deftool` (macro)

```clojure
(deftool var-name
  "Description for the LLM"
  {:type "object" :properties {...} :required [...]}  ;; optional JSON schema
  [args invocation]  ;; binding vector
  body...)
```

---

## Event Types

Sessions emit various events:

| Event Type | Description |
|------------|-------------|
| `user.message` | User message added |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution completed |
| `tool.shell_output` | Live shell output chunk (`types/tool-shell-output`) |
| `session.idle` | Session finished processing |
| `session.error` | Session error occurred |
| `session.compaction_start` | Background compaction started |
| `session.compaction_complete` | Compaction finished |

## Session Lifecycle Events (Client-level)

| Event Type | Description |
|------------|-------------|
| `session.created` | New session created |
| `session.deleted` | Session deleted |
| `session.updated` | Session updated |
| `session.foreground` | Session became foreground (TUI mode) |
| `session.background` | Session went to background (TUI mode) |

## Streaming

Enable streaming to receive response chunks incrementally:

```clojure
(def sess (client/create-session! my-client :model "gpt-5" :streaming true))

(session/on-type! sess "assistant.message_delta"
  (fn [event]
    (print (get-in event [:data :deltaContent]))
    (flush)))

(session/on-type! sess "assistant.message"
  (fn [event]
    (println "\n--- Final ---")
    (println (get-in event [:data :content]))))

(session/send-and-wait! sess {:prompt "Tell me a short story"})
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```clojure
(def client (copilot/create-client {:session-idle-timeout-seconds 300}))
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```clojure
(def client (copilot/create-client
  {:session-fs {:initial-cwd "/repo"
                :session-state-path "/tmp/state"
                :conventions "posix"}}))
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```clojure
(def meta (copilot/get-session-metadata client "session-123"))
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```clojure
(def session (copilot/create-session client
  {:skill-directories ["./skills"]
   :disabled-skills ["test-skill"]
   :include-sub-agent-streaming-events true}))
```

- `:skill-directories` - directories to scan for skill definitions
- `:disabled-skills` - skills to exclude from the session
- `:include-sub-agent-streaming-events` - receive streaming events from sub-agents

## Custom Providers (BYOK)

```clojure
;; Ollama
(def sess (client/create-session! my-client
            :model "deepseek-coder-v2:16b"
            :provider {:baseUrl "http://localhost:11434/v1"}))

;; Azure OpenAI
(def sess (client/create-session! my-client
            :model "gpt-4"
            :provider {:type    "azure"
                       :baseUrl "https://my-resource.openai.azure.com"
                       :apiKey  (System/getenv "AZURE_OPENAI_KEY")
                       :azure   {:apiVersion "2024-10-21"}}))
```

## Permission Handling

```clojure
(def sess (client/create-session! my-client
            :on-permission-request
            (fn [request invocation]
              (println "Permission:" (:kind request))
              {:kind "approved"})))
```

## Session Hooks

```clojure
(def sess (client/create-session! my-client
            :hooks
            {:on-pre-tool-use
             (fn [input invocation]
               (println "Running:" (:toolName input))
               {:permissionDecision "allow"})

             :on-post-tool-use
             (fn [input invocation]
               (println "Done:" (:toolName input))
               nil)

             :on-session-start
             (fn [input invocation]
               (println "Session started from:" (:source input))
               {:additionalContext "Welcome!"})

             :on-session-end
             (fn [input invocation]
               (println "Session ended:" (:reason input)))}))
```

## Recent Features (v2.4–v2.5)

Options added in the v2.4 and v2.5 upstream syncs. Session keys are passed to
`create-session!`; transport/BYOK options to `create-client` (see
`copilot.types/client-options`). Several are built with `copilot.types` helpers.

**v2.5.0**

- Reasoning effort — `:reasoning-effort` (`:low` `:medium` `:high` `:xhigh`)
- Tool search — `:tool-search`
- Session rewind — `:rewind-enabled`
- Additional directories — `:additional-directories`
- Disabled MCP servers — `:disabled-mcp-servers`
- GitHub MCP tool config — `:github-mcp-tool-config`
- Canvas provider — `:canvas-provider`
- Custom agents local-only — `:custom-agents-local-only`
- Experimental mode — `:experimental-mode`
- Content exclusion — `:content-exclusion`
- User-prompt-transformed hook — `:on-user-prompt-transformed`
- Permission decision context — `types/permission-result` (`:decision-context`)
- Agent-factory args schema — `types/custom-agent-config` (`:args-schema`)
- Built-in plugin directories — `:builtin-plugin-directories`
- In-process FFI transport — `:in-process`

**v2.4.0**

- BYOK bearer-token provider — `types/bearer-token-provider`
- MCP OAuth handler flag — `:on-mcp-auth-request`
- HTTP request handler — `types/copilot-request-handler`
- Session citations — `:enable-citations`
- Excluded built-in agents — `:excluded-builtin-agents`
- Session spending limits — `types/session-limits` (`:max-ai-credits`)
- Session memory — `types/memory-configuration` (`:enabled`)
- OTLP protocol — `:otlp-protocol`
- WebSocket responses — `:enable-web-socket-responses`
- Experiment assignments — `:exp-assignments`
- Tool defer loading — `types/tool-defer-modes` (`:auto` `:never`)
- System-message preamble / preserve — `types/system-prompt-sections` (`:preamble`), `types/section-override-actions` (`:preserve`)
- Post-tool-use / pre-MCP hooks — `:on-post-tool-use`, `:on-pre-mcp-tool-call`
- Message agent mode / display prompt — `:agent-mode`, `:display-prompt`
- GitHub attachment variants — `types/github-attachment-types` (`:commit` `:repository`)

Reasoning effort, content exclusion, extra directories, and rewind:

```clojure
(def sess
  (client/create-session! my-client
    :reasoning-effort       :high          ; :low :medium :high :xhigh
    :content-exclusion      true
    :additional-directories ["../shared" "../docs"]
    :rewind-enabled         true
    :disabled-mcp-servers   ["playwright"]))
```

Tool search, spending limits, memory, and citations:

```clojure
(require '[copilot.types :as types])

(client/create-session! my-client
  :tool-search       {:enabled true}
  :experimental-mode true
  :session-limits    (types/session-limits :max-ai-credits 5.0)
  :memory-config     (types/memory-configuration :enabled true)
  :enable-citations  true)
```

BYOK bearer-token provider and transport (client options):

```clojure
(def my-client
  (client/create-client
    :bearer-token-provider      (types/bearer-token-provider
                                  (fn [_args] "ghs_ephemeral_token"))
    :builtin-plugin-directories ["./plugins"]
    :in-process                 false))
```

Permission decision context:

```clojure
(types/permission-result :approved :decision-context {:source "policy"})
```

## Error Handling

```clojure
(try
  (def sess (client/create-session! my-client))
  (session/send-and-wait! sess {:prompt "Hello"})
  (catch clojure.lang.ExceptionInfo e
    (println "Error:" (.getMessage e))
    (println "Data:" (ex-data e))))
```

## Architecture

The SDK communicates with the Copilot CLI server via JSON-RPC 2.0:

- **Transport**: stdio (default) or TCP
- **Framing**: Content-Length header (`Content-Length: N\r\n\r\n{json}`)
- **Protocol version**: 4 (verified on connect via ping; Copilot CLI 1.0.113 or newer)
- **Server notifications**: `session.event`, `session.lifecycle`
- **Server requests**: `tool.call`, `permission.request`, `userInput.request`, `hooks.invoke`, `gitHubToken.getToken`, `shellAuthentication.request`, `shellAuthentication.cancel`, `memoryStore.store`, `network.resolveProxy`, `network.authenticateNegotiate`, `network.authenticateBasic`

## Requirements

- Clojure >= 1.11.0
- Java >= 11
- GitHub Copilot CLI installed and in PATH

## Image Generation

Request image responses using `:response-format` and `:image-options`:

```clojure
(let [opts (types/message-options "Generate a sunset over mountains"
             :response-format :image
             :image-options (types/image-options :size "1024x1024" :quality "hd" :style "natural"))
      response (session/send-and-wait! session-atom opts)]
  (println (:data @(:last-msg response))))
```

## License

MIT
