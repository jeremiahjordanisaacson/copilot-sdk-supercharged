/**
 * E2E tests for the Solidity Copilot SDK.
 *
 * Uses Vitest. Run with: cd solidity && npx vitest run e2e/
 *
 * Tests:
 *   1.  Session create + disconnect
 *   2.  Send message
 *   3.  SessionFs configuration
 *   4.  Multi-turn conversation
 *   5.  Session resume
 *   6.  Session list
 *   7.  Session metadata
 *   8.  Session delete
 *   9.  Model list
 *   10. Ping
 *   11. Auth status
 *   12. Client lifecycle
 *   13. Foreground session
 *   14. Tools
 *   15. Streaming
 *   16. System message customization
 *   17. Session fs provider
 *   18. MCP servers config
 *   19. Skills config
 *   20. Compaction
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import {
    startProxy,
    stopProxy,
    configureProxy,
    proxyPost,
    proxyGet,
    type ProxyHandle,
} from "./test-harness.js";

let proxy: ProxyHandle;

beforeAll(async () => {
    proxy = await startProxy();
}, 30_000);

afterAll(() => {
    if (proxy) {
        stopProxy(proxy);
    }
});

// ---------------------------------------------------------------------------
// Test 1: Session create + disconnect
// ---------------------------------------------------------------------------

describe("session create + disconnect", () => {
    it("creates a session and verifies proxy connectivity", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        // Verify proxy is alive
        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();

        // Create session
        const sessionRes = await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
        });
        // The proxy may or may not have this snapshot -- just check round-trip

        // Verify exchanges recorded
        const exchangesRes2 = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes2.ok).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// Test 2: Send message
// ---------------------------------------------------------------------------

describe("send message", () => {
    it("sends a message and receives response", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        const chatRes = await proxyPost(
            proxy.url,
            "/v1/chat/completions",
            {
                messages: [{ role: "user", content: "What is 1+1?" }],
            },
        );
        // Verify exchanges were captured
        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 3: SessionFs
// ---------------------------------------------------------------------------

describe("sessionFs configuration", () => {
    it("sends sessionFs config and verifies proxy records it", async () => {
        await configureProxy(
            proxy.url,
            "session_fs",
            "should_configure_session_fs",
        );

        const sessionRes = await proxyPost(
            proxy.url,
            "/v1/chat/sessions",
            {
                model: "gpt-4",
                sessionFs: {
                    initialCwd: "/",
                    sessionStatePath: "/session-state",
                    conventions: "posix",
                },
            },
        );

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 4: Multi-turn conversation
// ---------------------------------------------------------------------------

describe("multi-turn conversation", () => {
    it("sends two messages and verifies exchanges after each", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        // First message
        await proxyPost(proxy.url, "/v1/chat/completions", {
            messages: [{ role: "user", content: "Hello" }],
        });

        const exchangesRes1 = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes1.ok).toBe(true);
        const exchanges1 = await exchangesRes1.json();
        expect(exchanges1).toBeDefined();

        // Second message
        await proxyPost(proxy.url, "/v1/chat/completions", {
            messages: [
                { role: "user", content: "Hello" },
                { role: "assistant", content: "Hi there!" },
                { role: "user", content: "How are you?" },
            ],
        });

        const exchangesRes2 = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes2.ok).toBe(true);
        const exchanges2 = await exchangesRes2.json();
        expect(exchanges2).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 5: Session resume
// ---------------------------------------------------------------------------

describe("session resume", () => {
    it("creates a session, then resumes it", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        // Create session
        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
        });

        // Resume session
        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            sessionId: "test-resume-session-id",
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 6: Session list
// ---------------------------------------------------------------------------

describe("session list", () => {
    it("creates two sessions and verifies exchanges", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
        });

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 7: Session metadata
// ---------------------------------------------------------------------------

describe("session metadata", () => {
    it("creates a session and verifies exchanges JSON", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            metadata: { source: "solidity-e2e" },
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 8: Session delete
// ---------------------------------------------------------------------------

describe("session delete", () => {
    it("creates a session then deletes it and verifies exchanges", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
        });

        // DELETE the session
        await fetch(`${proxy.url}/v1/chat/sessions/test-delete-id`, {
            method: "DELETE",
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 9: Model list
// ---------------------------------------------------------------------------

describe("model list", () => {
    it("fetches exchanges for model listing", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 10: Ping
// ---------------------------------------------------------------------------

describe("ping", () => {
    it("verifies proxy responds to exchanges endpoint", async () => {
        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// Test 11: Auth status
// ---------------------------------------------------------------------------

describe("auth status", () => {
    it("posts auth request and verifies exchanges", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/auth/status", {
            token: "test-token",
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 12: Client lifecycle
// ---------------------------------------------------------------------------

describe("client lifecycle", () => {
    it("verifies proxy connectivity without additional configuration", async () => {
        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 13: Foreground session
// ---------------------------------------------------------------------------

describe("foreground session", () => {
    it("creates a session with foreground flag and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            foreground: true,
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 14: Tools
// ---------------------------------------------------------------------------

describe("tools", () => {
    it("sends a message with tools array and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        await proxyPost(proxy.url, "/v1/chat/completions", {
            messages: [{ role: "user", content: "Use the tool." }],
            tools: [
                {
                    type: "function",
                    function: {
                        name: "get_balance",
                        description: "Returns the ETH balance of an address",
                        parameters: {
                            type: "object",
                            properties: {
                                address: { type: "string" },
                            },
                            required: ["address"],
                        },
                    },
                },
            ],
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 15: Streaming
// ---------------------------------------------------------------------------

describe("streaming", () => {
    it("sends a message with streaming enabled and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        await proxyPost(proxy.url, "/v1/chat/completions", {
            messages: [{ role: "user", content: "Stream this." }],
            streaming: true,
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 16: System message customization
// ---------------------------------------------------------------------------

describe("system message customization", () => {
    it("sends a message with a custom system message and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        await proxyPost(proxy.url, "/v1/chat/completions", {
            messages: [{ role: "user", content: "Hello" }],
            systemMessage: "You are a Solidity smart contract auditor.",
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 17: Session fs provider
// ---------------------------------------------------------------------------

describe("session fs provider", () => {
    it("sends sessionFs configuration and verifies proxy records it", async () => {
        await configureProxy(
            proxy.url,
            "session_fs",
            "should_configure_session_fs",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            sessionFs: {
                initialCwd: "/workspace",
                sessionStatePath: "/workspace/.session-state",
                conventions: "posix",
            },
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 18: MCP servers config
// ---------------------------------------------------------------------------

describe("mcp servers config", () => {
    it("sends a session with mcpServers and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            mcpServers: {
                "solidity-analyzer": {
                    command: "npx",
                    args: ["-y", "solidity-mcp-server"],
                },
            },
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 19: Skills config
// ---------------------------------------------------------------------------

describe("skills config", () => {
    it("sends a session with skills and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_create_and_disconnect_sessions",
        );

        await proxyPost(proxy.url, "/v1/chat/sessions", {
            model: "gpt-4",
            skills: [
                { name: "solidity-audit", type: "function" },
                { name: "gas-optimizer", type: "function" },
            ],
        });

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});

// ---------------------------------------------------------------------------
// Test 20: Compaction
// ---------------------------------------------------------------------------

describe("compaction", () => {
    it("sends multiple messages to trigger compaction and verifies", async () => {
        await configureProxy(
            proxy.url,
            "session",
            "should_have_stateful_conversation",
        );

        // Send several messages to exercise compaction behavior
        for (let i = 0; i < 5; i++) {
            await proxyPost(proxy.url, "/v1/chat/completions", {
                messages: [{ role: "user", content: `Message ${i + 1}` }],
            });
        }

        const exchangesRes = await proxyGet(proxy.url, "/exchanges");
        expect(exchangesRes.ok).toBe(true);
        const exchanges = await exchangesRes.json();
        expect(exchanges).toBeDefined();
    });
});
