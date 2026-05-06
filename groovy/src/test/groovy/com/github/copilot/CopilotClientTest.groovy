/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import spock.lang.Specification
import spock.lang.Shared

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap

/**
 * Spock tests for the Groovy Copilot SDK.
 */
class CopilotClientTest extends Specification {

    // --- SdkProtocolVersion ---

    def 'SdkProtocolVersion returns expected version'() {
        expect:
        SdkProtocolVersion.get() == 3
        SdkProtocolVersion.SDK_PROTOCOL_VERSION == 3
    }

    // --- Types ---

    def 'ConnectionState enum has all expected values'() {
        expect:
        ConnectionState.values()*.name() == ['DISCONNECTED', 'CONNECTING', 'CONNECTED', 'ERROR']
    }

    def 'ReasoningEffort enum has all expected values'() {
        expect:
        ReasoningEffort.values()*.name() == ['LOW', 'MEDIUM', 'HIGH', 'XHIGH']
    }

    def 'ToolResultType enum has all expected values'() {
        expect:
        ToolResultType.values()*.name() == ['SUCCESS', 'FAILURE', 'REJECTED', 'DENIED']
    }

    def 'ToolInvocation stores all fields correctly'() {
        when:
        def inv = new ToolInvocation('sid-1', 'tc-1', 'my_tool', [key: 'value'])

        then:
        inv.sessionId == 'sid-1'
        inv.toolCallId == 'tc-1'
        inv.toolName == 'my_tool'
        inv.arguments == [key: 'value']
    }

    def 'Tool stores name description parameters and handler'() {
        given:
        def handler = { args, inv -> 'result' }

        when:
        def tool = new Tool('test', 'A test tool', [type: 'object'], handler)

        then:
        tool.name == 'test'
        tool.description == 'A test tool'
        tool.parameters == [type: 'object']
        tool.handler != null
    }

    def 'PingResponse can be constructed with .with{}'() {
        when:
        def ping = new PingResponse().with {
            message = 'pong'
            timestamp = 12345L
            protocolVersion = 2
            it
        }

        then:
        ping.message == 'pong'
        ping.timestamp == 12345L
        ping.protocolVersion == 2
    }

    def 'SessionEvent has default empty data map'() {
        when:
        def event = new SessionEvent()

        then:
        event.data == [:]
        event.type == null
    }

    def 'PermissionRequestResult can be constructed with kind'() {
        when:
        def result = new PermissionRequestResult('approved')

        then:
        result.kind == 'approved'
    }

    def 'UserInputResponse stores answer and wasFreeform'() {
        when:
        def resp = new UserInputResponse('yes', true)

        then:
        resp.answer == 'yes'
        resp.wasFreeform == true
    }

    def 'StopError stores message'() {
        when:
        def err = new StopError('something went wrong')

        then:
        err.message == 'something went wrong'
    }

    def 'ImageOptions stores all fields'() {
        when:
        def opts = new ImageOptions('1024x1024', 'hd', 'natural')

        then:
        opts.size == '1024x1024'
        opts.quality == 'hd'
        opts.style == 'natural'
    }

    // --- DefineTool ---

    def 'DefineTool builder creates a tool with all fields'() {
        when:
        def tool = DefineTool.create('weather')
            .description('Get weather')
            .parameters([
                type      : 'object',
                properties: [city: [type: 'string']],
                required  : ['city']
            ])
            .handler { args, invocation -> "72F in ${args.city}" }
            .build()

        then:
        tool.name == 'weather'
        tool.description == 'Get weather'
        tool.parameters.type == 'object'
        tool.handler != null
    }

    def 'DefineTool handler closure is invoked correctly'() {
        given:
        def tool = DefineTool.create('echo')
            .description('Echo back')
            .handler { args, inv -> "echo: ${args}" }
            .build()

        when:
        def result = tool.handler.call('hello', null)

        then:
        result == 'echo: hello'
    }

    def 'DefineTool throws when name is missing'() {
        when:
        DefineTool.create(null)
            .handler { args, inv -> 'x' }
            .build()

        then:
        thrown(IllegalStateException)
    }

    def 'DefineTool throws when handler is missing'() {
        when:
        DefineTool.create('test')
            .description('no handler')
            .build()

        then:
        thrown(IllegalStateException)
    }

    // --- CopilotClient construction ---

    def 'CopilotClient default constructor sets disconnected state'() {
        when:
        def client = new CopilotClient()

        then:
        client.state == ConnectionState.DISCONNECTED
    }

    def 'CopilotClient with cliUrl sets external server mode'() {
        when:
        def client = new CopilotClient(cliUrl: 'localhost:3000')

        then:
        client.state == ConnectionState.DISCONNECTED
    }

    def 'CopilotClient rejects cliUrl with useStdio'() {
        when:
        new CopilotClient(cliUrl: 'localhost:3000', useStdio: true)

        then:
        thrown(IllegalArgumentException)
    }

    def 'CopilotClient rejects cliUrl with githubToken'() {
        when:
        new CopilotClient(cliUrl: 'localhost:3000', githubToken: 'tok')

        then:
        thrown(IllegalArgumentException)
    }

    def 'CopilotClient rejects cliUrl with cliPath'() {
        when:
        new CopilotClient(cliUrl: 'localhost:3000', cliPath: '/usr/bin/copilot')

        then:
        thrown(IllegalArgumentException)
    }

    // --- CopilotSession ---

    def 'CopilotSession dispatches events to wildcard handlers'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        def received = []

        session.on { event -> received << event.type }

        when:
        session.dispatchEvent(new SessionEvent().with { type = 'assistant.message'; data = [content: 'hi']; it })

        then:
        received == ['assistant.message']
    }

    def 'CopilotSession dispatches events to typed handlers'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        def received = []
        def ignored = []

        session.on('assistant.message') { event -> received << event.data.content }
        session.on('session.idle') { event -> ignored << 'should not fire' }

        when:
        session.dispatchEvent(new SessionEvent().with { type = 'assistant.message'; data = [content: 'hello']; it })

        then:
        received == ['hello']
        ignored.empty
    }

    def 'CopilotSession unsubscribe removes handler'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        def received = []

        Runnable unsub = session.on { event -> received << event.type }

        when:
        session.dispatchEvent(new SessionEvent().with { type = 'test1'; it })
        unsub.run()
        session.dispatchEvent(new SessionEvent().with { type = 'test2'; it })

        then:
        received == ['test1']
    }

    def 'CopilotSession registers and retrieves tool handlers'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        def handler = { args, inv -> 'result' }
        def tool = new Tool('my_tool', 'desc', [:], handler)

        when:
        session.registerTools([tool])

        then:
        session.getToolHandler('my_tool') != null
        session.getToolHandler('unknown') == null
    }

    def 'CopilotSession handlePermissionRequest returns denied when no handler set'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')

        when:
        def result = session.handlePermissionRequest([kind: 'test', toolCallId: 'tc1'])

        then:
        result.kind == 'denied-no-approval-rule-and-could-not-request-from-user'
    }

    def 'CopilotSession handlePermissionRequest invokes registered handler'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        session.registerPermissionHandler { req, sid ->
            new PermissionRequestResult('approved')
        }

        when:
        def result = session.handlePermissionRequest([kind: 'tool.execute', toolCallId: 'tc1'])

        then:
        result.kind == 'approved'
    }

    // --- JsonRpcClient ---

    def 'JsonRpcClient can be constructed with streams'() {
        given:
        def input = new ByteArrayInputStream(new byte[0])
        def output = new ByteArrayOutputStream()

        when:
        def rpc = new JsonRpcClient(input, output)

        then:
        rpc != null
    }

    def 'JsonRpcClient.JsonRpcException stores code and message'() {
        when:
        def ex = new JsonRpcClient.JsonRpcException(-32601, 'Method not found')

        then:
        ex.code == -32601
        ex.message.contains('-32601')
        ex.message.contains('Method not found')
    }

    // --- CopilotClient stop() ---

    def 'CopilotClient stop on fresh client returns empty errors'() {
        given:
        def client = new CopilotClient()

        when:
        def errors = client.stop()

        then:
        errors.empty
        client.state == ConnectionState.DISCONNECTED
    }

    def 'CopilotClient forceStop on fresh client is safe'() {
        given:
        def client = new CopilotClient()

        when:
        client.forceStop()

        then:
        client.state == ConnectionState.DISCONNECTED
    }

    // --- CopilotSession dispatchEvent from Map ---

    def 'CopilotSession dispatchEvent from map creates SessionEvent correctly'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        def received = []

        session.on { event ->
            received << [type: event.type, content: event.data?.content]
        }

        when:
        session.dispatchEvent([type: 'assistant.message', data: [content: 'hi there']] as Map<String, Object>)

        then:
        received.size() == 1
        received[0].type == 'assistant.message'
        received[0].content == 'hi there'
    }

    // --- DefineTool spread operator and Groovy idioms ---

    def 'Multiple tools can be created and spread into a list'() {
        given:
        def tools = ['tool_a', 'tool_b', 'tool_c'].collect { name ->
            DefineTool.create(name)
                .description("Description for ${name}")
                .handler { args, inv -> "result from ${name}" }
                .build()
        }

        expect:
        tools*.name == ['tool_a', 'tool_b', 'tool_c']
        tools*.description == ['Description for tool_a', 'Description for tool_b', 'Description for tool_c']
    }

    // --- ProviderConfig ---

    def 'ProviderConfig can be configured with Groovy map constructor'() {
        when:
        def provider = new ProviderConfig().with {
            type = 'openai'
            baseUrl = 'http://localhost:11434/v1'
            it
        }

        then:
        provider.type == 'openai'
        provider.baseUrl == 'http://localhost:11434/v1'
    }

    // --- InfiniteSessionConfig ---

    def 'InfiniteSessionConfig stores threshold values'() {
        when:
        def config = new InfiniteSessionConfig().with {
            enabled = true
            backgroundCompactionThreshold = 0.8
            bufferExhaustionThreshold = 0.95
            it
        }

        then:
        config.enabled == true
        config.backgroundCompactionThreshold == 0.8
        config.bufferExhaustionThreshold == 0.95
    }
}
