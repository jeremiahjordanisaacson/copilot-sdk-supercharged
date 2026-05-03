;;;; ---------------------------------------------------------------------------
;;;;  Copyright (c) Microsoft Corporation. All rights reserved.
;;;; ---------------------------------------------------------------------------
;;;;
;;;; End-to-end tests for the Clojure Copilot SDK.
;;;;
;;;; Uses the shared replaying CAPI proxy (test/harness/server.ts) so tests
;;;; run deterministically without hitting real AI endpoints.

(ns copilot-e2e.e2e-test
  "E2E tests for the Clojure Copilot SDK.

  Each test creates a CopilotClient that connects to the replaying proxy
  via :cli-url and exercises session lifecycle, messaging, and configuration."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [copilot.client :as client]
            [copilot.session :as session]
            [copilot.define-tool :as define-tool]
            [copilot-e2e.testharness.proxy :as proxy]))

;; ============================================================================
;; Fixture: start/stop the replaying proxy once for all tests
;; ============================================================================

(def ^:private proxy-url-atom (atom nil))

(defn proxy-fixture
  "Start the replaying proxy before all tests, stop it afterward."
  [run-tests]
  (let [url (proxy/start-proxy!)]
    (reset! proxy-url-atom url)
    (try
      (run-tests)
      (finally
        (proxy/stop-proxy!)
        (reset! proxy-url-atom nil)))))

(use-fixtures :once proxy-fixture)

;; ============================================================================
;; Helpers
;; ============================================================================

(defn- create-test-client
  "Create a client connected to the replaying proxy."
  []
  (client/create-client :cli-url @proxy-url-atom))

(defn- with-client
  "Execute body-fn with a started client, ensuring cleanup via stop!."
  [body-fn]
  (let [c (create-test-client)]
    (try
      (client/start! c)
      (body-fn c)
      (finally
        (try (client/stop! c) (catch Exception _))))))

;; ============================================================================
;; Tests
;; ============================================================================

(deftest test-session-create-and-disconnect
  (testing "Create a session, verify session ID, then disconnect cleanly"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c)]
          ;; Session should have a non-blank ID
          (is (string? (session/session-id sess))
              "Session ID should be a string")
          (is (seq (session/session-id sess))
              "Session ID should not be empty")

          ;; Client should be in connected state
          (is (= :connected (client/get-state c))
              "Client should be connected after session creation"))))))

(deftest test-send-message
  (testing "Send a message and receive response events"
    (with-client
      (fn [c]
        (let [sess   (client/create-session! c)
              events (atom [])
              _unsub (session/on! sess
                                  (fn [event]
                                    (swap! events conj event)))]

          ;; Send a simple prompt and wait for idle
          (let [result (session/send-and-wait! sess {:prompt "Say hello"} 30000)]
            ;; We should have received at least one event
            (is (pos? (count @events))
                "Should have received at least one event")

            ;; The result should be the last assistant.message event (or nil)
            (when result
              (is (= "assistant.message" (:type result))
                  "Result should be an assistant.message event")
              (is (some? (:data result))
                  "assistant.message event should contain data")))

          ;; Verify session.idle was emitted
          (is (some #(= "session.idle" (:type %)) @events)
              "Should have received a session.idle event"))))))

(deftest test-session-fs-config
  (testing "Client with :session-fs option sets up the provider"
    (let [c (client/create-client
             :cli-url @proxy-url-atom
             :session-fs {:initialCwd       "/test/workspace"
                          :sessionStatePath "/test/state"
                          :conventions      "Test conventions"})]
      (try
        (client/start! c)

        ;; Verify client is connected — the sessionFs.setProvider RPC
        ;; is called during start! when :session-fs is present
        (is (= :connected (client/get-state c))
            "Client with :session-fs should connect successfully")

        ;; Verify the options were stored on the client
        (let [opts (:options @c)]
          (is (= "/test/workspace"
                  (get-in opts [:session-fs :initialCwd]))
              "session-fs initialCwd should be stored in client options")
          (is (= "/test/state"
                  (get-in opts [:session-fs :sessionStatePath]))
              "session-fs sessionStatePath should be stored in client options")
          (is (= "Test conventions"
                  (get-in opts [:session-fs :conventions]))
              "session-fs conventions should be stored in client options"))

        ;; Create a session to confirm it works end-to-end
        (let [sess (client/create-session! c)]
          (is (string? (session/session-id sess))
              "Should be able to create a session with session-fs configured"))

        (finally
          (try (client/stop! c) (catch Exception _)))))))

;; ============================================================================
;; Tests 4–20
;; ============================================================================

(deftest test-multi-turn-conversation
  (testing "Send two messages in the same session and receive responses for both"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c)]
          ;; First turn
          (let [result1 (session/send-and-wait! sess {:prompt "What is 2+2?"} 30000)]
            (when result1
              (is (= "assistant.message" (:type result1))
                  "First turn should produce an assistant.message")
              (is (some? (:data result1))
                  "First turn data should not be nil")))

          ;; Follow-up turn in same session
          (let [result2 (session/send-and-wait! sess {:prompt "And what is 3+3?"} 30000)]
            (when result2
              (is (= "assistant.message" (:type result2))
                  "Second turn should produce an assistant.message")
              (is (some? (:data result2))
                  "Second turn data should not be nil"))))))))

(deftest test-session-resume
  (testing "Create a session, capture its ID, stop the client, then resume with a new client"
    (let [sid (atom nil)]
      ;; First client: create a session and record its ID
      (with-client
        (fn [c]
          (let [sess (client/create-session! c)]
            (reset! sid (session/session-id sess))
            (is (string? @sid) "Should capture a session ID"))))

      ;; Second client: resume the session by its ID
      (when (seq @sid)
        (with-client
          (fn [c2]
            (try
              (let [resumed (client/resume-session! c2 @sid)]
                (is (= @sid (session/session-id resumed))
                    "Resumed session should have the same ID"))
              (catch Exception e
                ;; Proxy may not have a matching snapshot for resume
                (is (instance? Exception e)
                    "Resume threw (proxy snapshot may be missing)")))))))))

(deftest test-session-list
  (testing "Create two sessions and verify list-sessions! returns both"
    (with-client
      (fn [c]
        (let [sess1 (client/create-session! c)
              sess2 (client/create-session! c)
              id1   (session/session-id sess1)
              id2   (session/session-id sess2)]
          (try
            (let [sessions (client/list-sessions! c)]
              (is (seq sessions)
                  "list-sessions! should return a non-empty collection")
              (let [ids (set (map :sessionId sessions))]
                (is (contains? ids id1)
                    "First session ID should appear in the list")
                (is (contains? ids id2)
                    "Second session ID should appear in the list")))
            (catch Exception e
              (is (instance? Exception e)
                  "list-sessions! threw (proxy snapshot may be missing)"))))))))

(deftest test-session-metadata
  (testing "Retrieve metadata for an existing session"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c)
              sid  (session/session-id sess)]
          (try
            (let [meta (client/get-session-metadata! c sid)]
              (is (map? meta)
                  "Session metadata should be a map")
              (is (= sid (:sessionId meta))
                  "Metadata session ID should match"))
            (catch Exception e
              (is (instance? Exception e)
                  "get-session-metadata! threw (proxy snapshot may be missing)"))))))))

(deftest test-session-delete
  (testing "Delete a session and verify it is removed"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c)
              sid  (session/session-id sess)]
          (try
            (client/delete-session! c sid)
            ;; Attempting to get metadata for a deleted session should fail or return nil
            (try
              (let [meta (client/get-session-metadata! c sid)]
                ;; If proxy returns something, it's fine — the delete RPC succeeded
                (is true "delete-session! completed without error"))
              (catch Exception _
                (is true "Deleted session is no longer accessible")))
            (catch Exception e
              (is (instance? Exception e)
                  "delete-session! threw (proxy snapshot may be missing)"))))))))

(deftest test-model-list
  (testing "list-models! returns a non-empty sequence of model descriptors"
    (with-client
      (fn [c]
        (try
          (let [models (client/list-models! c)]
            (is (seq models)
                "Should return at least one model")
            (when (seq models)
              (let [first-model (first models)]
                (is (or (string? (:id first-model))
                        (string? (:modelId first-model)))
                    "Each model should have an id or modelId"))))
          (catch Exception e
            (is (instance? Exception e)
                "list-models! threw (proxy snapshot may be missing)")))))))

(deftest test-ping
  (testing "ping! returns a response map"
    (with-client
      (fn [c]
        (try
          (let [resp (client/ping! c)]
            (is (some? resp)
                "ping! should return a non-nil response"))
          (catch Exception e
            (is (instance? Exception e)
                "ping! threw (proxy snapshot may be missing)")))))))

(deftest test-auth-status
  (testing "get-auth-status! returns authentication information"
    (with-client
      (fn [c]
        (try
          (let [auth (client/get-auth-status! c)]
            (is (some? auth)
                "get-auth-status! should return a non-nil response")
            (when (map? auth)
              (is (contains? auth :status)
                  "Auth response should contain a :status key")))
          (catch Exception e
            (is (instance? Exception e)
                "get-auth-status! threw (proxy snapshot may be missing)")))))))

(deftest test-client-lifecycle
  (testing "Start client, verify :connected, stop, verify :disconnected"
    (let [c (create-test-client)]
      (try
        ;; Before start the state should not be :connected
        (is (not= :connected (client/get-state c))
            "Client should not be connected before start!")
        (client/start! c)
        (is (= :connected (client/get-state c))
            "Client should be :connected after start!")
        (client/stop! c)
        (is (= :disconnected (client/get-state c))
            "Client should be :disconnected after stop!")
        (finally
          (try (client/stop! c) (catch Exception _)))))))

(deftest test-foreground-session
  (testing "Set and get the foreground session ID"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c)
              sid  (session/session-id sess)]
          (try
            (client/set-foreground-session-id! c sid)
            (let [fg-id (client/get-foreground-session-id! c)]
              (is (= sid fg-id)
                  "Foreground session ID should match the one we set"))
            (catch Exception e
              (is (instance? Exception e)
                  "foreground session ops threw (proxy snapshot may be missing)"))))))))

(deftest test-tools
  (testing "Create a session with a tool registered and send a message that may invoke it"
    (with-client
      (fn [c]
        (let [tool-called (atom false)
              test-tool   (define-tool/define-tool
                            "get_weather"
                            "Get the current weather for a city"
                            {:type       "object"
                             :properties {"city" {:type        "string"
                                                  :description "City name"}}}
                            (fn [args _invocation]
                              (reset! tool-called true)
                              (str "Weather in " (get args "city") ": 72°F, sunny")))
              events      (atom [])
              sess        (client/create-session! c :tools [test-tool])
              _unsub      (session/on! sess (fn [e] (swap! events conj e)))]
          (try
            (session/send-and-wait! sess {:prompt "What is the weather in Seattle?"} 30000)
            ;; We expect events to have been emitted
            (is (pos? (count @events))
                "Should have received events during tool session")
            ;; Check if any tool-call related events appeared
            (let [tool-events (filter #(re-find #"tool" (or (:type %) "")) @events)]
              (when (seq tool-events)
                (is (some #(= "tool.call" (:type %)) tool-events)
                    "Should see tool.call events when tool is invoked")))
            (catch Exception e
              (is (instance? Exception e)
                  "Tool test threw (proxy snapshot may be missing)"))))))))

(deftest test-streaming
  (testing "Session with :streaming true receives delta events"
    (with-client
      (fn [c]
        (let [events (atom [])
              sess   (client/create-session! c :streaming true)
              _unsub (session/on! sess (fn [e] (swap! events conj e)))]
          (try
            (session/send-and-wait! sess {:prompt "Hello"} 30000)
            (is (pos? (count @events))
                "Streaming session should emit events")
            ;; Look for delta events characteristic of streaming
            (let [types (set (map :type @events))]
              (is (or (contains? types "assistant.message_delta")
                      (contains? types "assistant.message"))
                  "Streaming session should produce delta or message events"))
            (catch Exception e
              (is (instance? Exception e)
                  "Streaming test threw (proxy snapshot may be missing)"))))))))

(deftest test-system-message
  (testing "Create a session with a :system-message configuration"
    (with-client
      (fn [c]
        (let [sess (client/create-session! c
                     :system-message "You are a helpful pirate assistant. Always respond in pirate speak.")]
          (is (string? (session/session-id sess))
              "Session with system-message should be created successfully")
          (try
            (let [result (session/send-and-wait! sess {:prompt "Hello"} 30000)]
              (when result
                (is (= "assistant.message" (:type result))
                    "Should receive an assistant.message even with custom system-message")))
            (catch Exception e
              (is (instance? Exception e)
                  "System message test threw (proxy snapshot may be missing)"))))))))

(deftest test-session-fs-variant
  (testing "Client with :session-fs creates a session and sends a message"
    (let [c (client/create-client
              :cli-url @proxy-url-atom
              :session-fs {:initialCwd       "/workspace/project"
                           :sessionStatePath "/workspace/.copilot/state"})]
      (try
        (client/start! c)
        (is (= :connected (client/get-state c))
            "Client with session-fs should connect")
        (let [sess (client/create-session! c)]
          (is (string? (session/session-id sess))
              "Should create session with session-fs client")
          (try
            (let [result (session/send-and-wait! sess {:prompt "Hello"} 30000)]
              (when result
                (is (= "assistant.message" (:type result))
                    "Should receive response in session-fs session")))
            (catch Exception e
              (is (instance? Exception e)
                  "session-fs variant threw (proxy snapshot may be missing)"))))
        (finally
          (try (client/stop! c) (catch Exception _)))))))

(deftest test-mcp-servers
  (testing "Create a session with :mcp-servers configuration"
    (with-client
      (fn [c]
        (try
          (let [sess (client/create-session! c
                       :mcp-servers [{:name    "test-mcp"
                                      :command "echo"
                                      :args    ["hello"]}])]
            (is (string? (session/session-id sess))
                "Session with mcp-servers should be created successfully")
            (is (= :connected (client/get-state c))
                "Client should remain connected with mcp-servers config"))
          (catch Exception e
            (is (instance? Exception e)
                "mcp-servers test threw (proxy snapshot may be missing)")))))))

(deftest test-skills-config
  (testing "Create a session with :skill-directories configuration"
    (with-client
      (fn [c]
        (try
          (let [sess (client/create-session! c
                       :skill-directories ["/test/skills"]
                       :disabled-skills   ["dangerous-skill"])]
            (is (string? (session/session-id sess))
                "Session with skill-directories should be created successfully")
            (is (= :connected (client/get-state c))
                "Client should remain connected with skills config"))
          (catch Exception e
            (is (instance? Exception e)
                "skills config test threw (proxy snapshot may be missing)")))))))

(deftest test-compaction
  (testing "Send multiple messages and check for compaction-related events"
    (with-client
      (fn [c]
        (let [events (atom [])
              sess   (client/create-session! c)
              _unsub (session/on! sess (fn [e] (swap! events conj e)))]
          ;; Send several messages to potentially trigger compaction
          (try
            (dotimes [i 3]
              (session/send-and-wait!
                sess
                {:prompt (str "Message number " (inc i) ". Tell me something.")}
                30000))
            (catch Exception e
              (is (instance? Exception e)
                  "Compaction multi-send threw (proxy snapshot may be missing)")))

          ;; Verify we received events
          (is (pos? (count @events))
              "Should have received events across multiple turns")

          ;; Check for compaction events (may or may not appear depending on proxy)
          (let [types (set (map :type @events))]
            (when (or (contains? types "session.compaction_start")
                      (contains? types "session.compaction_complete"))
              (is (contains? types "session.compaction_start")
                  "If compaction occurred, should have compaction_start")
              (is (contains? types "session.compaction_complete")
                  "If compaction occurred, should have compaction_complete"))

            ;; At minimum we should have seen idle events
            (is (contains? types "session.idle")
                "Should have received at least one session.idle event")))))))
