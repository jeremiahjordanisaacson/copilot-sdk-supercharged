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
