# Error Handling - Clojure

Patterns for handling errors gracefully in the Copilot SDK for Clojure, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with try/catch

**Scenario:** Catch and handle exceptions from SDK calls to prevent application crashes.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defn safe-chat []
  (let [client (copilot/create-client {})]
    (try
      (copilot/start! client)

      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})
            response (copilot/send-and-wait session
                       {:message "Hello!"})]
        (println "Response:" (:message response)))

      (catch Exception e
        (println "Error:" (.getMessage e)))

      (finally
        (copilot/stop! client)))))

(safe-chat)
```

## Returning Data Instead of Throwing

**Scenario:** Wrap SDK calls to return data maps with `:ok` or `:error` status instead of throwing.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defn try-call
  "Wraps a function call, returning {:ok result} or {:error message}."
  [f & args]
  (try
    {:ok (apply f args)}
    (catch Exception e
      {:error (.getMessage e)})))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [session-result (try-call copilot/create-session client
                             {:system-prompt "You are a helpful assistant."})]
        (if (:error session-result)
          (println "Session creation failed:" (:error session-result))

          (let [session (:ok session-result)
                send-result (try-call copilot/send-and-wait session
                              {:message "Explain error handling in Clojure."})]
            (if (:error send-result)
              (println "Send failed:" (:error send-result))
              (println "Response:" (:message (:ok send-result)))))))

      (finally
        (copilot/stop! client)))))

(run)
```

## Retry with Backoff

**Scenario:** Retry transient failures with exponential backoff.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defn retry-with-backoff
  "Retries f up to max-attempts times with exponential backoff."
  [f & {:keys [max-attempts base-delay-ms]
        :or {max-attempts 3 base-delay-ms 1000}}]
  (loop [attempt 1]
    (let [result (try
                   {:ok (f)}
                   (catch Exception e
                     {:error e}))]
      (if (:ok result)
        (:ok result)
        (if (>= attempt max-attempts)
          (throw (:error result))
          (do
            (let [delay (* base-delay-ms (Math/pow 2 (dec attempt)))]
              (println (format "Attempt %d/%d failed: %s. Retrying in %dms..."
                         attempt max-attempts
                         (.getMessage (:error result))
                         (int delay)))
              (Thread/sleep (long delay)))
            (recur (inc attempt))))))))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)

    (try
      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})
            response (retry-with-backoff
                       #(copilot/send-and-wait session {:message "Hello!"})
                       :max-attempts 3
                       :base-delay-ms 1000)]
        (println "Response:" (:message response)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Resource Cleanup with with-open Pattern

**Scenario:** Ensure the client is always stopped using a macro similar to `with-open`.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defmacro with-copilot-client
  "Creates a client, starts it, executes body, then always stops it."
  [[client-sym opts] & body]
  `(let [~client-sym (copilot/create-client ~opts)]
     (copilot/start! ~client-sym)
     (try
       ~@body
       (finally
         (copilot/stop! ~client-sym)
         (println "Client stopped.")))))

;; Usage
(with-copilot-client [client {}]
  (let [session (copilot/create-session client
                  {:system-prompt "You are a helpful assistant."})
        response (copilot/send-and-wait session
                   {:message "Explain with-open in Clojure."})]
    (println "Response:" (:message response))))
```

## Classifying Errors

**Scenario:** Categorize errors by type for targeted recovery actions.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]
            [clojure.string :as str]))

(defn classify-error
  "Classifies an exception into a category keyword."
  [^Exception e]
  (let [msg (str/lower-case (or (.getMessage e) ""))]
    (cond
      (str/includes? msg "timeout")    :timeout
      (str/includes? msg "connection") :connection
      (str/includes? msg "refused")    :connection
      (str/includes? msg "auth")       :auth
      (str/includes? msg "token")      :auth
      :else                            :unknown)))

(defn handle-classified-error
  "Takes action based on the error category."
  [category ^Exception e]
  (case category
    :timeout    (println "Timeout:" (.getMessage e) "- Consider increasing timeout.")
    :connection (println "Connection error:" (.getMessage e) "- Is the CLI running?")
    :auth       (println "Auth error:" (.getMessage e) "- Check your credentials.")
    :unknown    (println "Unexpected error:" (.getMessage e))))

(defn run []
  (let [client (copilot/create-client {})]
    (try
      (copilot/start! client)
      (let [session (copilot/create-session client
                      {:system-prompt "You are a helpful assistant."})
            response (copilot/send-and-wait session {:message "Hello!"})]
        (println (:message response)))

      (catch Exception e
        (let [category (classify-error e)]
          (handle-classified-error category e)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Structured Error Data with ex-info

**Scenario:** Create rich, structured error data using Clojure's `ex-info` for better debugging.

```clojure
(ns myapp.core
  (:require [copilot.sdk :as copilot]))

(defn create-session-safe
  "Creates a session, wrapping failures in ex-info with structured data."
  [client config]
  (try
    (copilot/create-session client config)
    (catch Exception e
      (throw (ex-info "Session creation failed"
               {:type :session-creation
                :config config
                :cause (.getMessage e)}
               e)))))

(defn send-safe
  "Sends a message, wrapping failures in ex-info with structured data."
  [session opts]
  (try
    (copilot/send-and-wait session opts)
    (catch Exception e
      (throw (ex-info "Send failed"
               {:type :send-failure
                :message (:message opts)
                :cause (.getMessage e)}
               e)))))

(defn run []
  (let [client (copilot/create-client {})]
    (copilot/start! client)
    (try
      (let [session (create-session-safe client
                      {:system-prompt "You are a helpful assistant."})
            response (send-safe session {:message "Hello!"})]
        (println (:message response)))

      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (println "Structured error:" (pr-str data))))

      (catch Exception e
        (println "Unexpected error:" (.getMessage e)))

      (finally
        (copilot/stop! client)))))

(run)
```

## Best Practices

1. **Always use `try/finally`** to guarantee `copilot/stop!` is called on every exit path.
2. **Use `ex-info`** for structured error data that includes context about what failed.
3. **Wrap SDK calls** in helper functions that normalize error handling.
4. **Implement retry with backoff** for transient network failures.
5. **Classify errors** by pattern matching on the exception message for targeted recovery.
6. **Use a `with-copilot-client` macro** to encapsulate the start/stop lifecycle.
7. **Log errors with context** (session config, message, timestamps) for post-mortem debugging.
