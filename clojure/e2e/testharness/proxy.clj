;;;; ---------------------------------------------------------------------------
;;;;  Copyright (c) Microsoft Corporation. All rights reserved.
;;;; ---------------------------------------------------------------------------
;;;;
;;;; Replaying CAPI proxy for E2E tests.
;;;;
;;;; Manages a child process that acts as a replaying proxy to AI endpoints.
;;;; Spawns the shared test harness server from test/harness/server.ts.

(ns testharness.proxy
  "Manages the replaying CAPI proxy server for Clojure E2E tests.

  The proxy replays recorded HTTP exchanges so that E2E tests run
  deterministically without hitting real AI endpoints."
  (:require [clojure.string :as str])
  (:import [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [java.io BufferedReader File InputStreamReader]))

;; Proxy state: {:process Process, :url String}
(def ^:private proxy-state (atom nil))

(defn find-cli-path
  "Locate the Copilot CLI for E2E tests (run from the clojure/ directory).

  Lookup order: the COPILOT_CLI_PATH environment variable (when it names an
  existing file), nodejs/node_modules/@github/copilot/npm-loader.js, then the
  legacy index.js entry points.  `copilot.client` runs `.js` paths with node
  and executes any other path (such as node_modules/.bin/copilot) directly."
  []
  (let [github-dir (.getCanonicalFile (File. "../nodejs/node_modules/@github"))
        env-path   (System/getenv "COPILOT_CLI_PATH")
        from-env   (when-not (str/blank? env-path)
                     [(.getAbsoluteFile (File. ^String env-path))])
        legacy     (->> (or (.listFiles github-dir) (make-array File 0))
                        (filter (fn [^File d]
                                  (let [n (.getName d)]
                                    (and (str/starts-with? n "copilot-")
                                         (not (str/includes? n "language-server"))))))
                        (sort-by (fn [^File d] (.getName d)))
                        (map (fn [^File d] (File. d "index.js"))))
        candidates (concat from-env
                           [(File. github-dir "copilot/npm-loader.js")]
                           legacy
                           [(File. github-dir "copilot/index.js")])]
    (if-let [^File found (first (filter (fn [^File f] (.isFile f)) candidates))]
      (.getPath found)
      (throw (ex-info (str "CLI not found for E2E tests. Set COPILOT_CLI_PATH or run "
                           "`npm install` in the nodejs/ directory. Looked at: "
                           (str/join ", " (map str candidates)))
                      {:candidates (mapv str candidates)})))))

(defn start-proxy!
  "Launch the replaying proxy server and return its URL.

  Spawns `npx tsx ../../test/harness/server.ts` (relative to clojure/e2e),
  reads stdout until it sees `Listening: http://...`, and returns that URL.

  Throws on failure to start or parse the listening URL."
  []
  (if-let [url (:url @proxy-state)]
    url
    (let [;; Resolve the server script path relative to this project
          e2e-dir    (-> (java.io.File. ".")
                         (.getAbsoluteFile)
                         (.getParentFile)           ;; clojure/
                         (java.io.File. "e2e"))
          server-dir (-> e2e-dir
                         (java.io.File. "../../test/harness")
                         (.getCanonicalFile))
          server-ts  (java.io.File. server-dir "server.ts")
          _          (when-not (.exists server-ts)
                       (throw (ex-info (str "Harness server not found at " (.getPath server-ts))
                                       {:path (.getPath server-ts)})))

          ;; Build the process — use cmd /c on Windows for npx resolution
          windows?   (-> (System/getProperty "os.name")
                         (.toLowerCase)
                         (.contains "win"))
          command    (if windows?
                      ["cmd" "/c" "npx" "tsx" (.getAbsolutePath server-ts)]
                      ["npx" "tsx" (.getAbsolutePath server-ts)])
          pb         (doto (ProcessBuilder. ^java.util.List command)
                       (.directory server-dir)
                       (.redirectErrorStream false)
                       (.redirectError ProcessBuilder$Redirect/INHERIT))
          process    (.start pb)

          ;; Read stdout for the "Listening: ..." line
          reader     (BufferedReader.
                      (InputStreamReader. (.getInputStream process)))
          line       (try
                       (.readLine reader)
                       (catch Exception e
                         (.destroyForcibly process)
                         (throw (ex-info "Failed to read proxy stdout"
                                         {:cause e}))))]

      (when (nil? line)
        (.destroyForcibly process)
        (throw (ex-info "Proxy process exited before printing URL" {})))

      (let [matcher (re-find #"Listening:\s+(http://[^\s]+)" line)]
        (when-not matcher
          (.destroyForcibly process)
          (throw (ex-info (str "Unexpected proxy output: " line)
                          {:line line})))

        (let [url (second matcher)]
          (reset! proxy-state {:process process :url url :reader reader})
          (println (str "[proxy] Started at " url))
          url)))))

(defn stop-proxy!
  "Gracefully shut down the proxy server.

  Sends a POST to /stop, waits for the process to exit, and cleans up state."
  []
  (when-let [{:keys [^Process process url ^BufferedReader reader]} @proxy-state]
    ;; Best-effort graceful stop via HTTP
    (when url
      (try
        (let [stop-url (java.net.URL. (str url "/stop"))
              conn     (.openConnection stop-url)]
          (doto ^java.net.HttpURLConnection conn
            (.setRequestMethod "POST")
            (.setConnectTimeout 3000)
            (.setReadTimeout 3000)
            (.getResponseCode))
          (.disconnect ^java.net.HttpURLConnection conn))
        (catch Exception _)))

    ;; Wait for exit, then force-kill if still alive
    (try
      (.waitFor process)
      (catch Exception _
        (.destroyForcibly process)))

    (when reader
      (try (.close reader) (catch Exception _)))

    (reset! proxy-state nil)
    (println "[proxy] Stopped")))

(defn proxy-url
  "Return the current proxy URL, or nil if not started."
  []
  (:url @proxy-state))
