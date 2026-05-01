/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e

import com.github.copilot.CopilotClient
import com.github.copilot.CopilotSession
import com.github.copilot.Types.SessionEvent
import com.github.copilot.Types.SessionFsConfig
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Stepwise

/**
 * End-to-end tests for the Groovy Copilot SDK.
 *
 * <p>These tests run against the shared replaying CAPI proxy so they are
 * deterministic and do not require a live backend.
 */
@Stepwise
class CopilotE2ESpec extends Specification {

    @Shared
    TestHarness harness = new TestHarness()

    @Shared
    String cliUrl

    def setupSpec() {
        harness.startProxy()
        cliUrl = harness.cliUrl
    }

    def cleanupSpec() {
        harness.stopProxy()
    }

    def "should create a session and disconnect"() {
        given: "a client connected to the replay proxy"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)

        when: "the client starts and creates a session"
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        then: "the session has a non-null ID"
        session != null
        session.sessionId != null
        !session.sessionId.isEmpty()

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should send a message and receive response"() {
        given: "a connected client with an active session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        when: "a message is sent and we wait for the response"
        SessionEvent result = session.sendAndWait('Hello!')

        then: "we receive an assistant message with content"
        result != null
        result.type == 'assistant.message'
        result.data != null

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should configure sessionFs provider"() {
        given: "session filesystem configuration"
        SessionFsConfig fsConfig = new SessionFsConfig(
            '/workspace',
            '/state/path',
            'test conventions'
        )

        when: "a client is created with sessionFs config and started"
        CopilotClient client = new CopilotClient(
            cliUrl: cliUrl,
            sessionFs: fsConfig
        )
        client.start()

        then: "the client is connected (sessionFs.setProvider was called during start)"
        client.state == com.github.copilot.Types.ConnectionState.CONNECTED

        cleanup:
        client?.stop()
    }
}
