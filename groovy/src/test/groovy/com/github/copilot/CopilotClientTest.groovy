/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import spock.lang.Specification
import spock.lang.Shared
import spock.util.concurrent.PollingConditions

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap

/**
 * Spock tests for the Groovy Copilot SDK.
 */
class CopilotClientTest extends Specification {

    // --- SdkProtocolVersion ---

    def 'SdkProtocolVersion returns expected version'() {
        expect:
        SdkProtocolVersion.get() == 4
        SdkProtocolVersion.SDK_PROTOCOL_VERSION == 4
    }

    // --- Protocol 4 grouped session payloads ---

    def 'groupSessionParams groups flat options and translates the legacy provider'() {
        given:
        Map<String, Object> flat = [
            model                : 'my-model',
            streaming            : true,
            requestPermission    : true,
            hooks                : true,
            hookTypes            : ['preToolUse'],
            tools                : [[name: 'lookup']],
            mcpServers           : [srv: [command: 'srv']],
            configDir            : '/cfg',
            disableResume        : true,
            trustWorkingDirectory: 'session',
            reasoningEffort      : null,
            provider             : [type: 'openai', baseUrl: 'http://localhost/v1', maxOutputTokens: 512]
        ] as Map<String, Object>

        when:
        Map<String, Object> created = CopilotClient.groupSessionParams(flat, false)
        Map<String, Object> resumed = CopilotClient.groupSessionParams(flat, true)

        then:
        created.model == 'my-model'
        created.configDirectory == '/cfg'
        created.hookTypes == ['preToolUse']
        created.events == [streaming: true]
        created.permissions == [request: true]
        created.hooks == [hasHandlers: true]
        created.tools == [definitions: [[name: 'lookup']]]
        created.mcp == [servers: [srv: [command: 'srv']], envValueMode: 'direct']
        created.filesystem == [trustWorkingDirectory: 'session']
        created.inference == [
            providers      : [[name: 'default', type: 'openai', baseUrl: 'http://localhost/v1']],
            defaultProvider: 'default',
            models         : [[provider: 'default', id: 'my-model', maxOutputTokens: 512]]
        ]
        created.github == [capi: [enabled: false]]
        !created.keySet().any { it in ['streaming', 'provider', 'configDir', 'suppressResumeEvent', 'reasoningEffort'] }

        resumed.suppressResumeEvent == true
        !resumed.containsKey('filesystem')

        when: 'explicit providers are passed and the default provider is cleared on resume'
        Map<String, Object> explicit = CopilotClient.groupSessionParams(
            [model: 'm', provider: [type: 'openai'], providers: [[name: 'gw', baseUrl: 'u']]] as Map<String, Object>, false)
        Map<String, Object> cleared = CopilotClient.groupSessionParams([defaultProvider: null] as Map<String, Object>, true)

        then:
        explicit.inference == [providers: [[name: 'gw', baseUrl: 'u']]]
        !explicit.containsKey('github')
        cleared.inference.containsKey('defaultProvider')
        cleared.inference.defaultProvider == null
        CopilotClient.groupSessionParams([defaultProvider: null] as Map<String, Object>, false).isEmpty()
    }

    def 'buildSendPayload sends v3 message options and drops the legacy string response format'() {
        when:
        Map<String, Object> params = CopilotClient.buildSendPayload('s1', [
            prompt        : 'hi',
            source        : MessageSource.agent('planner'),
            clientTurnId  : 'turn-1',
            responseFormat: 'json_object'
        ] as Map<String, Object>)
        Map<String, Object> structured = CopilotClient.buildSendPayload('s1', [
            prompt        : 'hi',
            responseSchema: [type: 'object']
        ] as Map<String, Object>)

        then:
        params == [sessionId: 's1', prompt: 'hi', source: 'agent-planner', clientTurnId: 'turn-1']
        structured.responseFormat == [type: 'json_schema', jsonSchema: [name: 'response', strict: true, schema: [type: 'object']]]
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
            timestamp = '2025-01-01T00:00:00Z'
            protocolVersion = 2
            it
        }

        then:
        ping.message == 'pong'
        ping.timestamp == '2025-01-01T00:00:00Z'
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

    def 'JsonRpcClient answers server requests with the id exactly as received'() {
        given:
        String body = '{"jsonrpc":"2.0","id":7,"method":"hooks.invoke","params":{}}'
        byte[] frame = "Content-Length: ${body.getBytes('UTF-8').length}\r\n\r\n${body}".toString().getBytes('UTF-8')
        def output = new ByteArrayOutputStream()
        def rpc = new JsonRpcClient(new ByteArrayInputStream(frame), output)
        rpc.setRequestHandler('hooks.invoke') { Map params -> [ok: true] }

        when:
        rpc.start()

        then:
        new PollingConditions(timeout: 5).eventually {
            assert output.toString('UTF-8').contains('"id":7,')
        }

        cleanup:
        rpc.stop()
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

    // --- v3.1.0: MCP secret store, plugin extension approvals, ask-user image attachments ---

    def 'mcpSecretsSave and extensionApprovalsRevoke send their wire names and camelCase params'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def client = new CopilotClient()
        client.@rpcClient = rpc

        when:
        def saved = client.mcpSecretsSave('server-token', 'credential', '/cfg')
        def revoked = client.extensionApprovalsRevoke('plugin:p:ext', 'install-1')

        then:
        1 * rpc.request('mcp.secrets.save', [secretId: 'server-token', value: 'credential', configDir: '/cfg']) >> [usedKeychain: true]
        1 * rpc.request('permissions.extensionApprovals.revoke', [extensionName: 'plugin:p:ext', installationKey: 'install-1']) >> [revoked: true]
        saved.usedKeychain
        revoked.revoked
    }

    def 'user input request passes imageAttachmentConstraints and the response sends attachments only when set'() {
        given:
        def rpc = Mock(JsonRpcClient)
        def session = new CopilotSession('sess-1', rpc, '/workspace')
        UserInputRequest seen = null
        session.registerUserInputHandler { UserInputRequest req, String sid ->
            seen = req
            new UserInputResponse('', true, [new UserInputImageAttachment('aGVsbG8=', 'image/png')])
        }

        when:
        UserInputResponse response = session.handleUserInputRequest([
            sessionId                 : 'sess-1',
            question                  : 'Attach a screenshot?',
            requestId                 : 'req-1',
            imageAttachmentConstraints: [allowedMimeTypes: ['image/png'], maxBytesPerImage: 1048576, maxCount: 2],
            _meta                     : [ext: [k: 1]]
        ] as Map<String, Object>)

        then:
        seen.requestId == 'req-1'
        seen.imageAttachmentConstraints.allowedMimeTypes == ['image/png']
        seen.imageAttachmentConstraints.maxBytesPerImage == 1048576L
        seen.imageAttachmentConstraints.maxCount == 2
        seen.meta == [ext: [k: 1]]
        response.toMap() == [answer: '', wasFreeform: true, attachments: [[type: 'image', data: 'aGVsbG8=', mimeType: 'image/png']]]
        new UserInputResponse('yes', false).toMap() == [answer: 'yes', wasFreeform: false]
    }
}
