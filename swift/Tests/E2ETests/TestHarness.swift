// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

/// Manages a replaying CAPI proxy for E2E tests.
///
/// Spawns the shared test harness server from `test/harness/server.ts` and provides
/// methods to configure snapshot replay and stop the proxy.
final class TestHarness {
    private var process: Process?
    private var stdoutPipe: Pipe?

    /// The URL the proxy is listening on after `start()`.
    private(set) var proxyUrl: String?

    /// Start the replay proxy server.
    ///
    /// - Returns: The proxy URL (e.g. `http://127.0.0.1:PORT`).
    /// - Throws: If the proxy cannot be started or the listening URL cannot be parsed.
    func start() throws -> String {
        if let url = proxyUrl {
            return url
        }

        // Resolve path to test/harness/server.ts relative to this file's package root
        let thisFile = URL(fileURLWithPath: #filePath)
        let repoRoot = thisFile
            .deletingLastPathComponent()  // E2ETests/
            .deletingLastPathComponent()  // Tests/
            .deletingLastPathComponent()  // swift/
            .deletingLastPathComponent()  // repo root

        let serverPath = repoRoot
            .appendingPathComponent("test")
            .appendingPathComponent("harness")
            .appendingPathComponent("server.ts")
            .path

        let serverDir = repoRoot
            .appendingPathComponent("test")
            .appendingPathComponent("harness")
            .path

        let proc = Process()
        let pipe = Pipe()

        #if os(Windows)
        proc.executableURL = URL(fileURLWithPath: "C:\\Windows\\System32\\cmd.exe")
        proc.arguments = ["/c", "npx", "tsx", serverPath]
        #else
        proc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        proc.arguments = ["npx", "tsx", serverPath]
        #endif

        proc.currentDirectoryURL = URL(fileURLWithPath: serverDir)
        proc.standardOutput = pipe
        // Inherit stderr for debugging

        try proc.run()

        self.process = proc
        self.stdoutPipe = pipe

        // Read the first line to get the listening URL
        let handle = pipe.fileHandleForReading
        guard let lineData = readLine(from: handle),
              let line = String(data: lineData, encoding: .utf8)
        else {
            kill()
            throw TestHarnessError.failedToReadUrl
        }

        // Parse "Listening: http://..." from output
        let pattern = "Listening:\\s*(http://\\S+)"
        guard let regex = try? NSRegularExpression(pattern: pattern),
              let match = regex.firstMatch(
                  in: line,
                  range: NSRange(line.startIndex..., in: line)
              ),
              let urlRange = Range(match.range(at: 1), in: line)
        else {
            kill()
            throw TestHarnessError.unexpectedOutput(line)
        }

        let url = String(line[urlRange])
        self.proxyUrl = url
        return url
    }

    /// Configure the proxy for a specific test snapshot.
    ///
    /// - Parameters:
    ///   - snapshotPath: Absolute path to the YAML snapshot file.
    ///   - workDir: Working directory for the test.
    func configure(snapshotPath: String, workDir: String) async throws {
        guard let proxyUrl = proxyUrl else {
            throw TestHarnessError.notStarted
        }

        let url = URL(string: "\(proxyUrl)/config")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: String] = ["filePath": snapshotPath, "workDir": workDir]
        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (_, response) = try await URLSession.shared.data(for: request)
        guard let httpResponse = response as? HTTPURLResponse,
              httpResponse.statusCode == 200
        else {
            throw TestHarnessError.configFailed
        }
    }

    /// Stop the replay proxy server.
    func stop() async throws {
        if let proxyUrl = proxyUrl {
            // Best-effort stop request
            let url = URL(string: "\(proxyUrl)/stop")!
            var request = URLRequest(url: url)
            request.httpMethod = "POST"
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = "{}".data(using: .utf8)

            _ = try? await URLSession.shared.data(for: request)
        }

        kill()
    }

    // MARK: - Private

    private func kill() {
        if let proc = process, proc.isRunning {
            proc.terminate()
            proc.waitUntilExit()
        }
        process = nil
        stdoutPipe = nil
        proxyUrl = nil
    }

    /// Read a single line (up to newline) from a file handle.
    private func readLine(from handle: FileHandle) -> Data? {
        var buffer = Data()
        let newline = UInt8(ascii: "\n")

        while true {
            let chunk = handle.readData(ofLength: 1)
            if chunk.isEmpty {
                return buffer.isEmpty ? nil : buffer
            }
            if chunk[0] == newline {
                return buffer
            }
            buffer.append(chunk)
        }
    }
}

// MARK: - Errors

enum TestHarnessError: Error, CustomStringConvertible {
    case failedToReadUrl
    case unexpectedOutput(String)
    case notStarted
    case configFailed

    var description: String {
        switch self {
        case .failedToReadUrl:
            return "Failed to read proxy URL from stdout"
        case .unexpectedOutput(let line):
            return "Unexpected proxy output: \(line)"
        case .notStarted:
            return "Proxy not started"
        case .configFailed:
            return "Failed to configure replay proxy"
        }
    }
}
