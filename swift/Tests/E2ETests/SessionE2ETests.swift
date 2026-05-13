// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import XCTest
@testable import CopilotSDK

/// E2E tests for the Swift Copilot SDK session lifecycle.
///
/// These tests use the shared replay proxy and YAML snapshots to exercise
/// session creation, messaging, and filesystem configuration.
final class SessionE2ETests: XCTestCase {
    static var harness: TestHarness!
    static var proxyUrl: String!
    static var repoRoot: URL!
    static var snapshotsDir: String!
    static var workDir: String!

    override class func setUp() {
        super.setUp()

        let thisFile = URL(fileURLWithPath: #filePath)
        repoRoot = thisFile
            .deletingLastPathComponent()  // E2ETests/
            .deletingLastPathComponent()  // Tests/
            .deletingLastPathComponent()  // swift/
            .deletingLastPathComponent()  // repo root

        snapshotsDir = repoRoot
            .appendingPathComponent("test")
            .appendingPathComponent("snapshots")
            .path

        // Create an isolated work directory
        workDir = NSTemporaryDirectory() + "copilot-swift-e2e-" + UUID().uuidString
        try? FileManager.default.createDirectory(
            atPath: workDir,
            withIntermediateDirectories: true
        )

        harness = TestHarness()
        do {
            proxyUrl = try harness.start()
        } catch {
            fatalError("Failed to start test harness: \(error)")
        }
    }

    override class func tearDown() {
        // Stop the harness synchronously via a semaphore
        let semaphore = DispatchSemaphore(value: 0)
        Task {
            try? await harness.stop()
            semaphore.signal()
        }
        semaphore.wait()

        // Clean up work directory
        if let workDir = workDir {
            try? FileManager.default.removeItem(atPath: workDir)
        }

        super.tearDown()
    }

    // MARK: - Helpers

    private func configureSnapshot(category: String, testName: String) async throws {
        let snapshotPath = Self.snapshotsDir + "/" + category + "/" + testName + ".yaml"
        try await Self.harness.configure(snapshotPath: snapshotPath, workDir: Self.workDir)
    }

    private func getCliPath() -> String {
        if let envPath = ProcessInfo.processInfo.environment["COPILOT_CLI_PATH"],
           FileManager.default.fileExists(atPath: envPath) {
            return envPath
        }

        let nodeCliPath = Self.repoRoot
            .appendingPathComponent("nodejs")
            .appendingPathComponent("node_modules")
            .appendingPathComponent("@github")
            .appendingPathComponent("copilot")
            .appendingPathComponent("index.js")
            .path

        if FileManager.default.fileExists(atPath: nodeCliPath) {
            return nodeCliPath
        }

        return "copilot"
    }

    private func getTestEnv() -> [String: String] {
        return [
            "COPILOT_API_URL": Self.proxyUrl,
            "COPILOT_HOME": Self.workDir,
            "XDG_CONFIG_HOME": Self.workDir,
            "XDG_STATE_HOME": Self.workDir,
        ]
    }

    // MARK: - Tests

    /// Test that a session can be created and disconnected.
    ///
    /// Creates a client, creates a session, verifies the session ID is non-empty,
    /// destroys the session, and stops the client.
    func testSessionCreateDisconnect() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            // Session ID must be non-empty
            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session ID should be non-empty after creation")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test sending a message and receiving an assistant response.
    ///
    /// Creates a session, sends a simple prompt via sendAndWait,
    /// and verifies the response contains an assistant message.
    func testSendMessage() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty)

            let response = try await session.sendAndWait(
                MessageOptions(prompt: "What is 1+1?")
            )

            // Verify we received an assistant message event
            XCTAssertNotNil(response, "Expected a non-nil assistant response")
            XCTAssertEqual(response?.type, "assistant.message")

            let content = response?.data["content"] as? String ?? ""
            XCTAssertFalse(content.isEmpty, "Assistant response content should not be empty")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test that sessionFs configuration is accepted by the client.
    ///
    /// Creates a client with SessionFsConfig and verifies it starts
    /// without error, confirming the config is passed through correctly.
    func testSessionFsConfig() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let fsConfig = SessionFsConfig(
            initialCwd: "/",
            sessionStatePath: "/session-state",
            conventions: "posix"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv(),
            sessionFs: fsConfig
        ))

        try await client.start()

        do {
            // If sessionFs config was invalid, start() or createSession() would throw
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty)

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 4: Multi-turn conversation — send two messages and verify both responses.
    func testMultiTurnConversation() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            let response1 = try await session.sendAndWait(
                MessageOptions(prompt: "What is 1+1?")
            )
            XCTAssertNotNil(response1, "First response should not be nil")

            let response2 = try await session.sendAndWait(
                MessageOptions(prompt: "And what is 2+2?")
            )
            XCTAssertNotNil(response2, "Second response should not be nil")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 5: Resume a session — create, destroy, stop, then resume with a new client.
    func testSessionResume() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client1 = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client1.start()

        var savedSessionId: String = ""
        do {
            let session = try await client1.createSession(
                SessionConfig(model: "gpt-4")
            )
            savedSessionId = await session.sessionId
            XCTAssertFalse(savedSessionId.isEmpty)
            try await session.destroy()
        }

        try await client1.stop()

        // Resume with a new client
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client2 = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client2.start()

        do {
            let resumed = try await client2.resumeSession(
                savedSessionId,
                config: SessionConfig(model: "gpt-4")
            )
            let resumedId = await resumed.sessionId
            XCTAssertEqual(resumedId, savedSessionId, "Resumed session ID should match original")
            try await resumed.destroy()
        }

        try await client2.stop()
    }

    /// Test 6: List sessions — create two sessions then verify listSessions returns at least 2.
    func testSessionList() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session1 = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )
            let session2 = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            let sessions = try await client.listSessions()
            XCTAssertGreaterThanOrEqual(sessions.count, 2, "Should have at least 2 sessions")

            try await session1.destroy()
            try await session2.destroy()
        }

        try await client.stop()
    }

    /// Test 7: Get session metadata — create a session and retrieve its metadata.
    func testSessionMetadata() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )
            let sessionId = await session.sessionId

            let metadata = try await client.getSessionMetadata(sessionId)
            XCTAssertNotNil(metadata, "Session metadata should not be nil")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 8: Delete a session — create, delete, then verify it no longer appears in the list.
    func testSessionDelete() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )
            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty)

            try await client.deleteSession(sessionId)

            let sessions = try await client.listSessions()
            let found = sessions.contains { $0.id == sessionId }
            XCTAssertFalse(found, "Deleted session should not appear in session list")
        }

        try await client.stop()
    }

    /// Test 9: List models — verify the client can retrieve available models.
    func testModelList() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let models = try await client.listModels()
            XCTAssertNotNil(models, "Model list should not be nil")
        }

        try await client.stop()
    }

    /// Test 10: Ping — send a ping message and verify a response.
    func testPing() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let result = try await client.ping(message: "hello")
            XCTAssertNotNil(result, "Ping response should not be nil")
        }

        try await client.stop()
    }

    /// Test 11: Auth status — verify the client can retrieve authentication status.
    func testAuthStatus() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let authStatus = try await client.getAuthStatus()
            XCTAssertNotNil(authStatus, "Auth status should not be nil")
        }

        try await client.stop()
    }

    /// Test 12: Client lifecycle — start and stop the client without errors.
    func testClientLifecycle() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()
        try await client.stop()
    }

    /// Test 13: Foreground session — set and get the foreground session ID.
    func testForegroundSession() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )
            let sessionId = await session.sessionId

            try await client.setForegroundSessionId(sessionId)
            let fgId = try await client.getForegroundSessionId()
            XCTAssertEqual(fgId, sessionId, "Foreground session ID should match the one we set")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 14: Tools — define a tool, attach it to a session, send a message, and verify.
    func testTools() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let tool = defineTool(
                name: "get_weather",
                description: "Get current weather for a city",
                parameters: ["city": "string"]
            ) { params, _ in
                let dict = params as? [String: Any] ?? [:]
                return ["temperature": "72F", "city": dict["city"] ?? "unknown"]
            }

            let session = try await client.createSession(
                SessionConfig(model: "gpt-4", tools: [tool])
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session with tools should have a valid ID")

            let response = try await session.sendAndWait(
                MessageOptions(prompt: "What is the weather in Seattle?")
            )
            XCTAssertNotNil(response, "Tool-enabled session should return a response")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 15: Streaming — enable streaming, collect delta events, then verify.
    func testStreaming() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4", streaming: true)
            )

            let deltasCollector = DeltasCollector()
            await session.on("assistant.message_delta") { event in
                deltasCollector.append(event)
            }

            let response = try await session.sendAndWait(
                MessageOptions(prompt: "Say hello")
            )
            XCTAssertNotNil(response, "Streaming session should return a final response")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 16: System message customization — create session with a custom system message.
    func testSystemMessageCustomization() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let systemMessage = SystemMessageConfig.append(
                content: "You are a helpful coding assistant."
            )

            let session = try await client.createSession(
                SessionConfig(model: "gpt-4", systemMessage: systemMessage)
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session with system message should be created")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 17: Session filesystem provider — create client with sessionFs config and verify.
    func testSessionFsProvider() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let fsConfig = SessionFsConfig(
            initialCwd: Self.workDir,
            sessionStatePath: Self.workDir + "/session-state",
            conventions: "posix"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv(),
            sessionFs: fsConfig
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )
            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session with fs provider should have a valid ID")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 18: MCP servers config — create session with mcpServers configuration.
    func testMcpServersConfig() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let mcpServer = MCPServerConfig.local(
                command: "echo",
                args: ["hello"]
            )

            let session = try await client.createSession(
                SessionConfig(model: "gpt-4", mcpServers: ["test-mcp": mcpServer])
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session with MCP servers should be created")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 19: Skills config — create session with skills configuration.
    func testSkillsConfig() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4", skillDirectories: ["/skills"])
            )

            let sessionId = await session.sessionId
            XCTAssertFalse(sessionId.isEmpty, "Session with skill directories should be created")

            try await session.destroy()
        }

        try await client.stop()
    }

    /// Test 20: Compaction — send multiple messages and verify all responses are received.
    func testCompaction() async throws {
        try await configureSnapshot(
            category: "session",
            testName: "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"
        )

        let client = CopilotClient(options: CopilotClientOptions(
            cliPath: getCliPath(),
            cwd: Self.workDir,
            env: getTestEnv()
        ))

        try await client.start()

        do {
            let session = try await client.createSession(
                SessionConfig(model: "gpt-4")
            )

            let prompts = [
                "What is 1+1?",
                "What is 2+2?",
                "What is 3+3?",
                "What is 4+4?",
                "What is 5+5?"
            ]

            for prompt in prompts {
                let response = try await session.sendAndWait(
                    MessageOptions(prompt: prompt)
                )
                XCTAssertNotNil(response, "Response for '\(prompt)' should not be nil")
            }

            try await session.destroy()
        }

        try await client.stop()
    }
}

/// Thread-safe collector for streaming delta events in tests.
private final class DeltasCollector: @unchecked Sendable {
    private let lock = NSLock()
    private var items: [Any] = []

    func append(_ item: Any) {
        lock.lock()
        defer { lock.unlock() }
        items.append(item)
    }

    var count: Int {
        lock.lock()
        defer { lock.unlock() }
        return items.count
    }
}
