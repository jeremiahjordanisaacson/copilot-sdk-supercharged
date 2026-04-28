# Tools and Skills - Clojure

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Clojure.

## Defining a Simple Tool

**Scenario:** Expose a Clojure function as a tool that the model can invoke during a conversation.

```clojure
(ns myapp.tools
  (:require [copilot.sdk :as copilot])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      ;; Define a tool that returns the current time
      (copilot/define-tool "get_current_time"
        "Returns the current date and time"
        (fn [_params]
          (let [now (LocalDateTime/now)
                fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
            (.format now fmt))))

      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant with access to tools."})
            response (copilot/send-and-wait session
                       {:message "What is the current time?"})]
        (println "Response:" (:message response)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Tool with Parameters

**Scenario:** Define a tool that accepts and processes structured parameters from the model.

```clojure
(ns myapp.calculator
  (:require [copilot.sdk :as copilot]
            [clojure.data.json :as json]))

(defn calculate [params]
  (let [a (get params "a")
        b (get params "b")
        op (get params "operation")]
    (try
      (let [result (case op
                     "add"      (+ a b)
                     "subtract" (- a b)
                     "multiply" (* a b)
                     "divide"   (if (zero? b)
                                  (throw (ex-info "Division by zero" {}))
                                  (/ (double a) (double b)))
                     (throw (ex-info (str "Unknown operation: " op) {})))]
        (json/write-str {:result result}))

      (catch Exception e
        (json/write-str {:error (.getMessage e)})))))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (copilot/define-tool "calculate"
        "Performs arithmetic: add, subtract, multiply, divide"
        calculate)

      (let [session (copilot/create-session client
                      {:system-prompt "You are a calculator. Use the calculate tool for math."})
            response (copilot/send-and-wait session
                       {:message "What is 42 multiplied by 17?"})]
        (println "Response:" (:message response)))

      (finally
        (copilot/stop! client)))))

(run)
```

## File System Tools

**Scenario:** Give the model the ability to explore the file system.

```clojure
(ns myapp.files
  (:require [copilot.sdk :as copilot]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file-tool [params]
  (let [path (get params "path")]
    (if (.exists (io/file path))
      (let [content (slurp path)
            truncated (subs content 0 (min (count content) 10000))]
        (json/write-str {:path path
                         :content truncated
                         :size (count content)}))
      (json/write-str {:error (str "File not found: " path)}))))

(defn list-directory-tool [params]
  (let [dir (get params "directory" ".")
        dir-file (io/file dir)]
    (if (.isDirectory dir-file)
      (let [entries (->> (.listFiles dir-file)
                         (mapv #(.getName %))
                         sort)]
        (json/write-str {:directory dir :files entries}))
      (json/write-str {:error (str "Not a directory: " dir)}))))

(defn search-files-tool [params]
  (let [pattern (get params "pattern")
        dir (get params "directory" ".")
        matches (->> (file-seq (io/file dir))
                     (filter #(.isFile %))
                     (filter #(try
                                (str/includes? (slurp %) pattern)
                                (catch Exception _ false)))
                     (take 20)
                     (mapv #(.getPath %)))]
    (json/write-str {:pattern pattern :matches matches})))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (copilot/define-tool "read_file" "Reads the contents of a file" read-file-tool)
      (copilot/define-tool "list_directory" "Lists files in a directory" list-directory-tool)
      (copilot/define-tool "search_files" "Searches files for a pattern" search-files-tool)

      (let [session (copilot/create-session client
                      {:system-prompt "You are a file explorer. Use your tools."})
            response (copilot/send-and-wait session
                       {:message "List files in the current directory."})]
        (println (:message response)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Stateful Tools with Atoms

**Scenario:** Create tools backed by an atom for mutable shared state.

```clojure
(ns myapp.kvstore
  (:require [copilot.sdk :as copilot]
            [clojure.data.json :as json]))

(defonce store (atom {}))

(defn kv-set [params]
  (let [k (get params "key")
        v (get params "value")]
    (swap! store assoc k v)
    (json/write-str {:status "ok" :key k})))

(defn kv-get [params]
  (let [k (get params "key")]
    (if-let [v (get @store k)]
      (json/write-str {:key k :value v})
      (json/write-str {:error (str "Key not found: " k)}))))

(defn kv-list [_params]
  (json/write-str {:keys (vec (keys @store))}))

(defn kv-delete [params]
  (let [k (get params "key")]
    (swap! store dissoc k)
    (json/write-str {:status "deleted" :key k})))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (copilot/define-tool "kv_set" "Stores a value under a key" kv-set)
      (copilot/define-tool "kv_get" "Retrieves a value by key" kv-get)
      (copilot/define-tool "kv_list" "Lists all keys" kv-list)
      (copilot/define-tool "kv_delete" "Deletes a key" kv-delete)

      (let [session (copilot/create-session client
                      {:system-prompt "You are a key-value store assistant."})
            response (copilot/send-and-wait session
                       {:message "Store name=Alice, role=Developer, then list all keys."})]
        (println (:message response)))

      (finally
        (reset! store {})
        (copilot/stop! client)))))

(run)
```

## Orchestrating Sub-Agents

**Scenario:** Define tools that delegate work to specialized session sub-agents.

```clojure
(ns myapp.orchestrator
  (:require [copilot.sdk :as copilot]))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      ;; Create specialized sub-agent sessions
      (let [code-agent (copilot/create-session client
                         {:system-prompt "You generate Clojure code. Output only code."})
            review-agent (copilot/create-session client
                           {:system-prompt "You review Clojure code for bugs. Be concise."})
            test-agent (copilot/create-session client
                         {:system-prompt "You write clojure.test tests. Output only test code."})]

        ;; Tools that delegate to sub-agents
        (copilot/define-tool "generate_code"
          "Generates Clojure code from a description"
          (fn [params]
            (let [resp (copilot/send-and-wait code-agent
                         {:message (get params "request")})]
              (:message resp))))

        (copilot/define-tool "review_code"
          "Reviews Clojure code for bugs and issues"
          (fn [params]
            (let [resp (copilot/send-and-wait review-agent
                         {:message (str "Review this code:\n" (get params "code"))})]
              (:message resp))))

        (copilot/define-tool "generate_tests"
          "Generates tests for Clojure code"
          (fn [params]
            (let [resp (copilot/send-and-wait test-agent
                         {:message (str "Write tests for:\n" (get params "code"))})]
              (:message resp))))

        ;; Orchestrator session
        (let [orchestrator (copilot/create-session client
                             {:system-prompt (str "You orchestrate development tasks. "
                                              "Use generate_code, review_code, and generate_tests "
                                              "to deliver complete solutions.")})
              response (copilot/send-and-wait orchestrator
                         {:message "Build a function to merge sorted sequences, review it, and write tests."})]
          (println (:message response))))

      (finally
        (copilot/stop! client)))))

(run)
```

## Best Practices

1. **Return JSON from tool handlers** using `clojure.data.json/write-str` for structured output.
2. **Use atoms** for stateful tools that need mutable shared state across invocations.
3. **Handle errors inside tool handlers** with `try/catch` to return error JSON instead of crashing.
4. **Truncate large outputs** with `subs` to stay within token limits.
5. **Define tools as named functions** for reusability and testability, not just inline anonymous functions.
6. **Use sub-agent orchestration** for multi-step workflows with specialized sessions.
7. **Clean up tool state** (e.g., `reset! store {}`) in the `finally` block when appropriate.
