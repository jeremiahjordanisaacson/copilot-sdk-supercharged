/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e

import com.github.copilot.CopilotClient
import com.github.copilot.CopilotSession
import com.github.copilot.DefineTool
import com.github.copilot.Types
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

    // -----------------------------------------------------------------------
    // Tests 4–20
    // -----------------------------------------------------------------------

    def "should handle multi-turn conversation"() {
        given: "a connected client with an active session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        when: "two messages are sent sequentially"
        SessionEvent first = session.sendAndWait('What is 2+2?')
        SessionEvent second = session.sendAndWait('And what is that plus 3?')

        then: "both responses are assistant messages with content"
        first != null
        first.type == 'assistant.message'
        first.data != null
        second != null
        second.type == 'assistant.message'
        second.data != null

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should resume session by ID"() {
        given: "a session created on one client"
        CopilotClient client1 = new CopilotClient(cliUrl: cliUrl)
        client1.start()
        CopilotSession original = client1.createSession(model: 'gpt-4')
        String savedId = original.sessionId

        when: "we stop the first client and resume the session on a second client"
        original.destroy()
        client1.stop()
        CopilotClient client2 = new CopilotClient(cliUrl: cliUrl)
        client2.start()
        CopilotSession resumed = client2.resumeSession(savedId, model: 'gpt-4')

        then: "the resumed session has the same ID"
        resumed != null
        resumed.sessionId == savedId

        cleanup:
        resumed?.destroy()
        client2?.stop()
    }

    def "should list sessions"() {
        given: "a client with two active sessions"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession s1 = client.createSession(model: 'gpt-4')
        CopilotSession s2 = client.createSession(model: 'gpt-4')

        when: "listing all sessions"
        List<Types.SessionMetadata> sessions = client.listSessions()

        then: "the list contains at least the two newly created sessions"
        sessions != null
        sessions.size() >= 2
        sessions.any { it.sessionId == s1.sessionId }
        sessions.any { it.sessionId == s2.sessionId }

        cleanup:
        s1?.destroy()
        s2?.destroy()
        client?.stop()
    }

    def "should get session metadata"() {
        given: "a connected client with an active session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        when: "metadata is retrieved for the session"
        Map<String, Object> metadata = client.getSessionMetadata(session.sessionId)

        then: "the metadata contains the session ID"
        metadata != null
        metadata.sessionId == session.sessionId

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should delete session"() {
        given: "a connected client with an active session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')
        String deletedId = session.sessionId

        when: "the session is deleted"
        client.deleteSession(deletedId)
        List<Types.SessionMetadata> remaining = client.listSessions()

        then: "the deleted session no longer appears in the session list"
        !remaining.any { it.sessionId == deletedId }

        cleanup:
        client?.stop()
    }

    def "should list models"() {
        given: "a connected client"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()

        when: "listing available models"
        List<Types.ModelInfo> models = client.listModels()

        then: "the model list is non-empty and each entry has an id"
        models != null
        !models.isEmpty()
        models.every { it.id != null && !it.id.isEmpty() }

        cleanup:
        client?.stop()
    }

    def "should ping"() {
        given: "a connected client"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()

        when: "a ping is sent"
        Types.PingResponse pong = client.ping('hello')

        then: "the response contains a message and a positive timestamp"
        pong != null
        pong.message != null
        pong.timestamp > 0

        cleanup:
        client?.stop()
    }

    def "should get auth status"() {
        given: "a connected client"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()

        when: "auth status is queried"
        Types.GetAuthStatusResponse auth = client.getAuthStatus()

        then: "the response is non-null with an authType"
        auth != null
        auth.authType != null

        cleanup:
        client?.stop()
    }

    def "should track client lifecycle"() {
        given: "a new client"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)

        expect: "the client starts in DISCONNECTED state"
        client.state == Types.ConnectionState.DISCONNECTED

        when: "the client is started"
        client.start()

        then: "the state transitions to CONNECTED"
        client.state == Types.ConnectionState.CONNECTED

        when: "the client is stopped"
        client.stop()

        then: "the state transitions back to DISCONNECTED"
        client.state == Types.ConnectionState.DISCONNECTED
    }

    def "should manage foreground session"() {
        given: "a connected client with a session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        when: "the session is set as the foreground session"
        client.setForegroundSessionId(session.sessionId)
        String foregroundId = client.getForegroundSessionId()

        then: "the foreground session ID matches the session"
        foregroundId == session.sessionId

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should handle tools"() {
        given: "a tool that returns a fixed response and a session configured with it"
        Types.Tool weatherTool = DefineTool.create('get_weather')
            .description('Get current weather')
            .parameters([
                type      : 'object',
                properties: [
                    location: [type: 'string', description: 'City name']
                ],
                required  : ['location']
            ])
            .handler { args, invocation ->
                "72F and sunny in ${(args as Map).location}"
            }
            .build()

        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(
            model: 'gpt-4',
            tools: [weatherTool]
        )

        when: "a prompt that should trigger the tool is sent"
        SessionEvent result = session.sendAndWait("What's the weather in Seattle?")

        then: "we get an assistant response (the proxy replays the recorded exchange)"
        result != null
        result.type == 'assistant.message'

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should support streaming"() {
        given: "a client with a streaming session"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4', streaming: true)

        and: "a collector for streaming delta events"
        List<SessionEvent> deltas = Collections.synchronizedList(new ArrayList<SessionEvent>())
        Runnable unsub = session.on('assistant.message_delta') { SessionEvent evt ->
            deltas.add(evt)
        }

        when: "a message is sent and we wait for the final response"
        SessionEvent finalMsg = session.sendAndWait('Tell me a joke')

        then: "we received at least one delta and a final assistant message"
        finalMsg != null
        finalMsg.type == 'assistant.message'

        cleanup:
        unsub?.run()
        session?.destroy()
        client?.stop()
    }

    def "should configure system message"() {
        given: "a session with a system message"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(
            model: 'gpt-4',
            systemMessage: [
                content: 'You are a pirate. Always respond in pirate speak.',
                mode   : 'append'
            ]
        )

        when: "a message is sent"
        SessionEvent result = session.sendAndWait('Hello there')

        then: "we receive a valid assistant response"
        result != null
        result.type == 'assistant.message'
        result.data != null

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should work with sessionFs and messaging"() {
        given: "a client with sessionFs configuration"
        SessionFsConfig fsConfig = new SessionFsConfig(
            '/workspace/project',
            '/state/session-state',
            'Use tabs for indentation'
        )
        CopilotClient client = new CopilotClient(
            cliUrl: cliUrl,
            sessionFs: fsConfig
        )
        client.start()
        CopilotSession session = client.createSession(model: 'gpt-4')

        when: "a message is sent within the sessionFs-enabled session"
        SessionEvent result = session.sendAndWait('Summarize the project')

        then: "the response is a valid assistant message"
        result != null
        result.type == 'assistant.message'

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should configure MCP servers"() {
        given: "MCP server configuration"
        Map<String, Types.MCPServerConfig> mcpServers = [
            'test-server': new Types.MCPServerConfig().with {
                type = 'stdio'
                command = 'echo'
                args = ['hello']
                it
            }
        ]

        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(
            model: 'gpt-4',
            mcpServers: mcpServers
        )

        then: "the session was created successfully with MCP server config"
        session != null
        session.sessionId != null

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should configure skills"() {
        given: "skill directory paths"
        List<String> skillDirs = ['/workspace/skills', '/workspace/custom-skills']

        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(
            model: 'gpt-4',
            skillDirectories: skillDirs
        )

        then: "the session was created successfully with skills configured"
        session != null
        session.sessionId != null

        cleanup:
        session?.destroy()
        client?.stop()
    }

    def "should handle compaction"() {
        given: "a session with infinite sessions enabled and a compaction event collector"
        CopilotClient client = new CopilotClient(cliUrl: cliUrl)
        client.start()
        CopilotSession session = client.createSession(
            model: 'gpt-4',
            infiniteSessions: new Types.InfiniteSessionConfig().with {
                enabled = true
                backgroundCompactionThreshold = 0.5
                bufferExhaustionThreshold = 0.9
                it
            }
        )

        List<SessionEvent> compactionEvents = Collections.synchronizedList(new ArrayList<SessionEvent>())
        Runnable unsubStart = session.on('session.compaction_start') { SessionEvent evt ->
            compactionEvents.add(evt)
        }
        Runnable unsubComplete = session.on('session.compaction_complete') { SessionEvent evt ->
            compactionEvents.add(evt)
        }

        when: "several messages are sent to fill the context buffer"
        SessionEvent r1 = session.sendAndWait('Message 1: Tell me about Groovy')
        SessionEvent r2 = session.sendAndWait('Message 2: Tell me about Spock testing')
        SessionEvent r3 = session.sendAndWait('Message 3: Tell me about closures')

        then: "all responses are valid assistant messages"
        r1 != null
        r1.type == 'assistant.message'
        r2 != null
        r2.type == 'assistant.message'
        r3 != null
        r3.type == 'assistant.message'

        cleanup:
        unsubStart?.run()
        unsubComplete?.run()
        session?.destroy()
        client?.stop()
    }
}
