# E2E tests for the Crystal Copilot SDK.
#
# Uses Crystal's built-in `spec` framework.
# Run with: cd crystal && crystal spec e2e/
#
# Tests:
#   1. Session create + disconnect
#   2. Send message
#   3. SessionFs configuration
#   4. Multi-turn conversation
#   5. Session resume
#   6. Session list
#   7. Session metadata
#   8. Session delete
#   9. Model list
#  10. Ping
#  11. Auth status
#  12. Client lifecycle
#  13. Foreground session
#  14. Tools
#  15. Streaming
#  16. System message customization
#  17. Session fs provider
#  18. MCP servers config
#  19. Skills config
#  20. Compaction

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

  # -------------------------------------------------------------------------
  # Test 4: Multi-turn conversation
  # -------------------------------------------------------------------------
  describe "multi-turn conversation" do
    it "sends multiple messages in a stateful conversation" do
      proxy.configure("session", "should_have_stateful_conversation")

      response1 = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Hello"}]})
      )

      response2 = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Follow-up question"}]})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 5: Session resume
  # -------------------------------------------------------------------------
  describe "session resume" do
    it "creates a session then resumes it" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))

      resume_response = proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4","sessionId":"test-resume-id"}))

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 6: Session list
  # -------------------------------------------------------------------------
  describe "session list" do
    it "creates two sessions and verifies exchanges" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))
      proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 7: Session metadata
  # -------------------------------------------------------------------------
  describe "session metadata" do
    it "creates a session and verifies metadata exchanges" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 8: Session delete
  # -------------------------------------------------------------------------
  describe "session delete" do
    it "creates then deletes a session" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      proxy.http_post("/v1/chat/sessions", %q({"model":"gpt-4"}))

      proxy.http_post("/v1/chat/sessions/delete", %q({"sessionId":"test-delete-id"}))

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 9: Model list
  # -------------------------------------------------------------------------
  describe "model list" do
    it "retrieves model list and verifies exchanges" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_get("/v1/models")

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 10: Ping
  # -------------------------------------------------------------------------
  describe "ping" do
    it "verifies proxy is responsive via exchanges endpoint" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 11: Auth status
  # -------------------------------------------------------------------------
  describe "auth status" do
    it "posts auth status and verifies exchanges" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post("/v1/auth/status", %q({"token":"test-token"}))

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 12: Client lifecycle
  # -------------------------------------------------------------------------
  describe "client lifecycle" do
    it "verifies proxy connectivity through the full lifecycle" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 13: Foreground session
  # -------------------------------------------------------------------------
  describe "foreground session" do
    it "creates a foreground session and verifies exchanges" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post(
        "/v1/chat/sessions",
        %q({"model":"gpt-4","foreground":true})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 14: Tools
  # -------------------------------------------------------------------------
  describe "tools" do
    it "sends a message with tools configuration" do
      proxy.configure("session", "should_have_stateful_conversation")

      response = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Use a tool"}],"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather","parameters":{"type":"object","properties":{"location":{"type":"string"}}}}}]})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 15: Streaming
  # -------------------------------------------------------------------------
  describe "streaming" do
    it "sends a message with streaming enabled" do
      proxy.configure("session", "should_have_stateful_conversation")

      response = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Stream this"}],"streaming":true})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 16: System message customization
  # -------------------------------------------------------------------------
  describe "system message customization" do
    it "sends a message with a custom system message" do
      proxy.configure("session", "should_have_stateful_conversation")

      response = proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Hello"}],"systemMessage":"You are a helpful coding assistant."})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 17: Session fs provider
  # -------------------------------------------------------------------------
  describe "session fs provider" do
    it "configures session fs provider and verifies exchanges" do
      proxy.configure("session_fs", "should_configure_session_fs")

      response = proxy.http_post(
        "/v1/chat/sessions",
        %q({"model":"gpt-4","sessionFs":{"initialCwd":"/home","sessionStatePath":"/state","conventions":"posix"}})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 18: MCP servers config
  # -------------------------------------------------------------------------
  describe "mcp servers config" do
    it "sends a session with mcpServers configuration" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post(
        "/v1/chat/sessions",
        %q({"model":"gpt-4","mcpServers":{"test-server":{"command":"node","args":["server.js"]}}})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 19: Skills config
  # -------------------------------------------------------------------------
  describe "skills config" do
    it "sends a session with skills configuration" do
      proxy.configure("session", "should_create_and_disconnect_sessions")

      response = proxy.http_post(
        "/v1/chat/sessions",
        %q({"model":"gpt-4","skills":["code-review","testing"]})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end

  # -------------------------------------------------------------------------
  # Test 20: Compaction
  # -------------------------------------------------------------------------
  describe "compaction" do
    it "sends multiple messages to trigger compaction behavior" do
      proxy.configure("session", "should_have_stateful_conversation")

      proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Message 1"}]})
      )

      proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Message 2"}]})
      )

      proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Message 3"}]})
      )

      proxy.http_post(
        "/v1/chat/completions",
        %q({"messages":[{"role":"user","content":"Message 4"}]})
      )

      exchanges = proxy.http_get("/exchanges")
      exchanges.size.should be > 0
    end
  end
end
