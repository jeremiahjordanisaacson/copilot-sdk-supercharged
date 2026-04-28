# Multiple Sessions - Clojure

Patterns for managing multiple independent Copilot conversations simultaneously in Clojure.

## Basic Multi-Session Setup

**Scenario:** Run multiple independent conversations, each with its own system prompt and context.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [code-session (copilot/create-session client
                           {:system-prompt "You are an expert Clojure programmer."})
            docs-session (copilot/create-session client
                           {:system-prompt "You are a technical writer for Clojure libraries."})]

        ;; Each session has its own independent history
        (let [code-resp (copilot/send-and-wait code-session
                          {:message "Write a transducer to filter and transform a collection."})]
          (println "=== Code ===" )
          (println (:message code-resp)))

        (let [docs-resp (copilot/send-and-wait docs-session
                          {:message "Write docstrings for a collection transducer."})]
          (println "=== Docs ===")
          (println (:message docs-resp))))

      (finally
        (copilot/stop! client)))))

(run)
```

## Session Registry with Atom

**Scenario:** Manage a pool of named sessions using a Clojure atom for thread-safe state.

```clojure
(ns myapp.registry
  (:require [copilot.sdk :as copilot]))

(defonce ^:private registry (atom {}))

(defn init-registry!
  "Initialize the registry with a Copilot client."
  [client]
  (reset! registry {:client client :sessions {}}))

(defn get-or-create-session!
  "Gets an existing session by name, or creates a new one."
  [name config]
  (let [current @registry]
    (if-let [session (get-in current [:sessions name])]
      session
      (let [session (copilot/create-session (:client current) config)]
        (swap! registry assoc-in [:sessions name] session)
        session))))

(defn send-to-session!
  "Sends a message to a named session."
  [name message]
  (if-let [session (get-in @registry [:sessions name])]
    (copilot/send-and-wait session {:message message})
    (throw (ex-info "Session not found" {:name name}))))

(defn remove-session!
  "Removes a session from the registry."
  [name]
  (swap! registry update :sessions dissoc name))

(defn list-sessions
  "Returns the names of all active sessions."
  []
  (keys (get @registry :sessions)))

;; Usage
(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)
    (init-registry! client)

    (try
      (get-or-create-session! "frontend"
        {:system-prompt "You are a frontend expert."})
      (get-or-create-session! "backend"
        {:system-prompt "You are a backend expert."})

      (let [resp (send-to-session! "frontend" "How do I use ClojureScript with React?")]
        (println "Frontend:" (:message resp)))

      (println "Active sessions:" (list-sessions))

      (finally
        (copilot/stop! client)))))

(run)
```

## Concurrent Requests with pmap

**Scenario:** Send messages to multiple sessions in parallel using Clojure's parallel map.

```clojure
(ns myapp.concurrent
  (:require [copilot.sdk :as copilot]))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [session-configs [["concise"  "Give answers in under 50 words."]
                             ["detailed" "Give thorough, detailed explanations."]
                             ["creative" "Use analogies and metaphors."]]

            sessions (mapv (fn [[name prompt]]
                            {:name name
                             :session (copilot/create-session client
                                        {:system-prompt prompt})})
                       session-configs)

            question "What is a transducer?"

            ;; Send to all sessions in parallel
            results (pmap (fn [{:keys [name session]}]
                           (try
                             {:name name
                              :response (:message (copilot/send-and-wait session
                                                    {:message question}))}
                             (catch Exception e
                               {:name name
                                :error (.getMessage e)})))
                     sessions)]

        (doseq [{:keys [name response error]} results]
          (println (str "=== " name " ==="))
          (if error
            (println "Error:" error)
            (println response))
          (println)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Per-User Sessions

**Scenario:** Maintain separate sessions for each user in a multi-user application.

```clojure
(ns myapp.users
  (:require [copilot.sdk :as copilot]))

(defonce ^:private user-sessions (atom {}))

(defn user-chat!
  "Chat as a specific user. Creates a session on first use."
  [client user-id message]
  (let [session (or (get @user-sessions user-id)
                    (let [s (copilot/create-session client
                              {:system-prompt
                               (str "You are a helpful assistant for user " user-id ".")})]
                      (swap! user-sessions assoc user-id s)
                      s))]
    (copilot/send-and-wait session {:message message})))

(defn end-user-session!
  "End and remove a user's session."
  [user-id]
  (swap! user-sessions dissoc user-id))

(defn active-users
  "Returns the set of users with active sessions."
  []
  (set (keys @user-sessions)))

;; Usage
(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [resp1 (user-chat! client "alice" "How do I use core.async?")]
        (println "Alice:" (:message resp1)))

      (let [resp2 (user-chat! client "bob" "How do I use spec?")]
        (println "Bob:" (:message resp2)))

      (println "Active users:" (active-users))

      (end-user-session! "alice")
      (println "After cleanup:" (active-users))

      (finally
        (copilot/stop! client)))))

(run)
```

## Pipeline Across Sessions

**Scenario:** Chain the output of one session into the next for a multi-stage workflow.

```clojure
(ns myapp.pipeline
  (:require [copilot.sdk :as copilot]))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [draft-session  (copilot/create-session client
                             {:system-prompt "You write Clojure code. Output only code."})
            review-session (copilot/create-session client
                             {:system-prompt "You review Clojure code for bugs. Be concise."})
            polish-session (copilot/create-session client
                             {:system-prompt "You improve Clojure code based on feedback. Output only code."})]

        ;; Stage 1: Draft
        (println "=== Stage 1: Drafting ===")
        (let [draft (:message (copilot/send-and-wait draft-session
                                {:message "Write a ring middleware for rate limiting."}))]
          (println draft)

          ;; Stage 2: Review
          (println "\n=== Stage 2: Reviewing ===")
          (let [review (:message (copilot/send-and-wait review-session
                                   {:message (str "Review this code:\n" draft)}))]
            (println review)

            ;; Stage 3: Polish
            (println "\n=== Stage 3: Polishing ===")
            (let [final (:message (copilot/send-and-wait polish-session
                                    {:message (str "Improve based on feedback:\n\nCode:\n"
                                                draft "\n\nFeedback:\n" review)}))]
              (println final)))))

      (finally
        (copilot/stop! client)))))

(run)
```

## Best Practices

1. **Use atoms** for thread-safe session registries in concurrent Clojure applications.
2. **Use `pmap`** or `future` for parallel message sending across multiple sessions.
3. **Share a single client** across all sessions to reuse the underlying connection.
4. **Always wrap with `try/finally`** to ensure `copilot/stop!` is called on exit.
5. **Use descriptive session names** in the registry for easy debugging and monitoring.
6. **Clean up sessions** with `dissoc` when they are no longer needed.
7. **Chain sessions in pipelines** for multi-stage workflows like draft, review, and polish.
