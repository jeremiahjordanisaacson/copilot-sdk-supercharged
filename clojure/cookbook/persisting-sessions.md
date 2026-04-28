# Persisting Sessions - Clojure

Patterns for saving and resuming Copilot sessions across application restarts in Clojure.

## Basic Save and Load

**Scenario:** Save session state to an EDN file and restore it on the next run.

```clojure
(ns myapp.persist
  (:require [copilot.sdk :as copilot]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn save-session-state!
  "Saves session state to a file as EDN."
  [session filepath]
  (let [state (copilot/get-session-state session)]
    (spit filepath (pr-str state))
    (println "Session saved to" filepath)))

(defn load-session-state
  "Loads session state from an EDN file. Returns nil if file does not exist."
  [filepath]
  (when (.exists (io/file filepath))
    (edn/read-string (slurp filepath))))

;; Save a session
(defn run-save []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})]
        (copilot/send-and-wait session
          {:message "Remember: the project is called Aurora."})
        (save-session-state! session "session.edn"))

      (finally
        (copilot/stop! client)))))

(run-save)
```

## Resuming a Saved Session

**Scenario:** Load a previously saved session state and continue the conversation.

```clojure
(ns myapp.resume
  (:require [copilot.sdk :as copilot]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn load-session-state [filepath]
  (when (.exists (io/file filepath))
    (edn/read-string (slurp filepath))))

(defn run []
  (let [client (copilot/create-client {})
        state-file "session.edn"]
    (copilot/start! client)

    (try
      (let [saved-state (load-session-state state-file)
            session (if saved-state
                      (do
                        (println "Resuming saved session...")
                        (copilot/create-session client
                          {:system-prompt "You are a helpful assistant."
                           :state saved-state}))
                      (do
                        (println "Starting fresh session...")
                        (copilot/create-session client
                          {:system-prompt "You are a helpful assistant."})))
            response (copilot/send-and-wait session
                       {:message "What is the project name?"})]
        (println "Response:" (:message response)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Named Session Store

**Scenario:** Manage multiple named sessions persisted to a directory.

```clojure
(ns myapp.store
  (:require [copilot.sdk :as copilot]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn create-store
  "Creates a session store backed by a directory."
  [directory]
  (.mkdirs (io/file directory))
  {:directory directory})

(defn store-path
  "Returns the file path for a named session."
  [store name]
  (str (:directory store) "/" name ".edn"))

(defn store-save!
  "Saves a session to the store under the given name."
  [store name session]
  (let [state (copilot/get-session-state session)
        path (store-path store name)]
    (spit path (pr-str state))
    (println "Saved session:" name)))

(defn store-load
  "Loads a session state from the store. Returns nil if not found."
  [store name]
  (let [path (store-path store name)]
    (when (.exists (io/file path))
      (edn/read-string (slurp path)))))

(defn store-delete!
  "Deletes a named session from the store."
  [store name]
  (let [path (store-path store name)]
    (io/delete-file path true)
    (println "Deleted session:" name)))

(defn store-list
  "Lists all session names in the store."
  [store]
  (->> (.listFiles (io/file (:directory store)))
       (filter #(str/ends-with? (.getName %) ".edn"))
       (mapv #(str/replace (.getName %) ".edn" ""))))

;; Usage
(defn run []
  (let [client (copilot/create-client {})
        store (create-store "./sessions")]
    (copilot/start! client)

    (try
      ;; Create and save
      (let [session (copilot/create-session client
                      {:system-prompt "You are a project planner."})]
        (copilot/send-and-wait session
          {:message "We are building a data pipeline in Clojure."})
        (store-save! store "pipeline-project" session))

      ;; Restore
      (when-let [saved (store-load store "pipeline-project")]
        (let [restored (copilot/create-session client
                         {:system-prompt "You are a project planner."
                          :state saved})
              resp (copilot/send-and-wait restored
                     {:message "What are we building?"})]
          (println (:message resp))))

      (println "Saved sessions:" (store-list store))

      (finally
        (copilot/stop! client)))))

(run)
```

## Auto-Save After Every Turn

**Scenario:** Automatically persist session state after each message exchange.

```clojure
(ns myapp.autosave
  (:require [copilot.sdk :as copilot]))

(defn send-and-save!
  "Sends a message and auto-saves the session state afterward."
  [session message save-path]
  (let [response (copilot/send-and-wait session {:message message})
        state (copilot/get-session-state session)]
    (spit save-path (pr-str state))
    response))

;; Usage
(defn run []
  (let [client (copilot/create-client {})
        save-path "./autosave.edn"]
    (copilot/start! client)

    (try
      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})]
        ;; Each call auto-saves
        (send-and-save! session "Hello! Remember I prefer functional style." save-path)
        (let [resp (send-and-save! session "What style do I prefer?" save-path)]
          (println "Response:" (:message resp)))
        (println "Session auto-saved to" save-path))

      (finally
        (copilot/stop! client)))))

(run)
```

## Session Envelope with Metadata

**Scenario:** Save metadata (timestamps, description, turn count) alongside session state.

```clojure
(ns myapp.envelope
  (:require [copilot.sdk :as copilot]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn save-with-metadata!
  "Saves session state wrapped in an envelope with metadata."
  [session filepath metadata]
  (let [envelope {:metadata metadata
                  :state (copilot/get-session-state session)
                  :saved-at (str (java.time.Instant/now))}]
    (spit filepath (pr-str envelope))
    (println "Saved with metadata to" filepath)))

(defn load-with-metadata
  "Loads an envelope with metadata and session state."
  [filepath]
  (when (.exists (io/file filepath))
    (edn/read-string (slurp filepath))))

;; Usage
(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})]
        (copilot/send-and-wait session {:message "Hello!"})

        (save-with-metadata! session "session_envelope.edn"
          {:description "Cookbook example session"
           :created-by "cookbook"
           :turn-count 1}))

      ;; Load and inspect
      (when-let [envelope (load-with-metadata "session_envelope.edn")]
        (println "Saved at:" (:saved-at envelope))
        (println "Description:" (get-in envelope [:metadata :description]))
        (println "Turns:" (get-in envelope [:metadata :turn-count])))

      (finally
        (copilot/stop! client)))))

(run)
```

## Best Practices

1. **Use EDN** (Extensible Data Notation) for serialization, as it is native to Clojure and supports all Clojure data types.
2. **Use `when-let`** for loading sessions that may not exist to handle nil gracefully.
3. **Auto-save after every turn** in production to minimize data loss.
4. **Store metadata** in an envelope map alongside session state for easy inspection.
5. **Use a directory-based store** with one file per session for multi-session applications.
6. **Use `io/delete-file` with `true`** for silent failure when deleting files that may not exist.
7. **Always wrap operations in `try/finally`** to ensure the client is stopped.
