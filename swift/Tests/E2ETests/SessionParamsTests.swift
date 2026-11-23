// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import XCTest
@testable import CopilotSDK

/// Unit tests for the SDK protocol 4 session payload grouping (no CLI or proxy needed).
final class SessionParamsTests: XCTestCase {
    func testGroupsFlatResumePayload() {
        let payload = groupSessionParams(
            [
                "sessionId": "s1",
                "model": "m",
                "name": "create-only",
                "streaming": true,
                "requestPermission": true,
                "tools": [["name": "t"]],
                "hooks": true,
                "disableResume": true,
                "configDir": "/cfg",
                "trustWorkingDirectory": "session",
                "mcpServers": ["srv": ["command": "x"]],
                "unknownKey": 1,
            ],
            isResume: true
        )

        XCTAssertEqual(payload["sessionId"] as? String, "s1")
        XCTAssertEqual(payload["model"] as? String, "m")
        XCTAssertEqual(payload["suppressResumeEvent"] as? Bool, true)
        XCTAssertEqual(payload["configDirectory"] as? String, "/cfg")
        XCTAssertEqual(payload["unknownKey"] as? Int, 1)
        XCTAssertNil(payload["name"])
        XCTAssertNil(payload["streaming"])
        XCTAssertNil(payload["filesystem"])
        XCTAssertEqual((payload["events"] as? [String: Any])?["streaming"] as? Bool, true)
        XCTAssertEqual((payload["permissions"] as? [String: Any])?["request"] as? Bool, true)
        XCTAssertEqual((payload["hooks"] as? [String: Any])?["hasHandlers"] as? Bool, true)
        let definitions = (payload["tools"] as? [String: Any])?["definitions"] as? [[String: Any]]
        XCTAssertEqual(definitions?.first?["name"] as? String, "t")
        let mcp = payload["mcp"] as? [String: Any]
        XCTAssertNotNil(mcp?["servers"])
        XCTAssertEqual(mcp?["envValueMode"] as? String, "direct")
    }

    func testTranslatesLegacyProvider() {
        let payload = groupSessionParams(
            [
                "model": "gpt-x",
                "provider": [
                    "type": "openai", "baseUrl": "https://example.test", "apiKey": "k",
                    "maxOutputTokens": 100,
                ] as [String: Any],
            ],
            isResume: false
        )

        XCTAssertNil(payload["provider"])
        let inference = payload["inference"] as? [String: Any]
        let connection = (inference?["providers"] as? [[String: Any]])?.first
        XCTAssertEqual(connection?["name"] as? String, "default")
        XCTAssertEqual(connection?["baseUrl"] as? String, "https://example.test")
        XCTAssertNil(connection?["maxOutputTokens"])
        XCTAssertEqual(inference?["defaultProvider"] as? String, "default")
        let model = (inference?["models"] as? [[String: Any]])?.first
        XCTAssertEqual(model?["provider"] as? String, "default")
        XCTAssertEqual(model?["id"] as? String, "gpt-x")
        XCTAssertEqual(model?["maxOutputTokens"] as? Int, 100)
        let capi = (payload["github"] as? [String: Any])?["capi"] as? [String: Any]
        XCTAssertEqual(capi?["enabled"] as? Bool, false)
    }
}
