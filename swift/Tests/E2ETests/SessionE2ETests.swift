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
}
