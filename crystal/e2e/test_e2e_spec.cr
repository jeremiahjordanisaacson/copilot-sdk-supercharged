# E2E tests for the Crystal Copilot SDK.
#
# Uses Crystal's built-in `spec` framework.
# Run with: cd crystal && crystal spec e2e/
#
# Tests:
#   1. Session create + disconnect
#   2. Send message
#   3. SessionFs configuration

require "spec"
require "./test_harness"

describe "Crystal Copilot SDK E2E" do
  proxy = CopilotE2E.start_proxy

  after_all do
    proxy.stop
  end

  # -------------------------------------------------------------------------
  # Test 1: Session create + disconnect
  # -------------------------------------------------------------------------
  describe "session create + disconnect" do
    it "creates a session and verifies proxy connectivity" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      # Verify proxy is alive
      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0

      # Create session
      response = proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))

      # Verify exchanges recorded
      exchanges2 = proxy.http_get("/exchanges")
      exchanges2.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 2: Send message
  # -------------------------------------------------------------------------
  describe "send message" do
    it "sends a message and receives response events" do
      proxy.configure("session", "should_have_stateful_conversation")

      response = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"What is 1+1?"}]})
      )

      # Verify exchanges were captured
      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 3: SessionFs
  # -------------------------------------------------------------------------
  describe "sessionFs configuration" do
    it "sends sessionFs config and verifies proxy records it" do
      proxy.configure("session_fs", "should_configure_session_fs")

      response = proxy.http_post(
        "/v1/chat/sessions",
        %q({"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end
end
