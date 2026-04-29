# Advanced Features

Advanced patterns and Groovy-specific idioms for the Copilot SDK.

## Builder-Style Session Configuration

Use Groovy's map literals for concise session config:

```groovy
import com.github.copilot.*
import com.github.copilot.Types.*

def client = new CopilotClient()

def session = client.createSession(
    model: 'gpt-4o',
    streaming: true,
    systemMessage: 'You are a Groovy expert. Always provide idiomatic Groovy code.',
    workingDirectory: System.getProperty('user.dir'),
    onPermissionRequest: { req, sessionId ->
        new PermissionRequestResult('approved')
    }
)
```

## Safe Navigation in Event Handling

Use Groovy's safe navigation operator (`?.`) to handle optional data:

```groovy
session.on { event ->
    def content = event?.data?.content
    def reasoning = event?.data?.reasoning
    def toolName = event?.data?.toolName

    switch (event.type) {
        case 'assistant.message':
            println content ?: '(empty message)'
            break
        case 'assistant.reasoning':
            println "[Thinking] ${reasoning}"
            break
        case 'tool.call':
            println "Calling tool: ${toolName}"
            break
    }
}
```

## Dynamic Tool Registration with Closures

Generate tools dynamically from a configuration:

```groovy
def toolConfigs = [
    [name: 'get_time', desc: 'Get current time', impl: { args, inv -> new Date().toString() }],
    [name: 'get_env', desc: 'Get env variable', impl: { args, inv ->
        def key = (args as Map).key?.toString()
        System.getenv(key) ?: "Not set: ${key}"
    }],
    [name: 'calc', desc: 'Evaluate math expression', impl: { args, inv ->
        def expr = (args as Map).expression?.toString()
        new GroovyShell().evaluate(expr)?.toString() ?: 'null'
    }]
]

def tools = toolConfigs.collect { cfg ->
    DefineTool.create(cfg.name as String)
        .description(cfg.desc as String)
        .handler(cfg.impl as Closure)
        .build()
}

def session = client.createSession(tools: tools)
```

## GString Templating for System Messages

Use GString for dynamic system messages:

```groovy
def username = System.getProperty('user.name')
def os = System.getProperty('os.name')
def javaVersion = System.getProperty('java.version')

def session = client.createSession(
    systemMessage: """You are assisting ${username} on ${os} with Java ${javaVersion}.
The project uses Groovy and Gradle. Prefer idiomatic Groovy solutions."""
)
```

## User Input Handling

Register a handler for when the model needs user input:

```groovy
def session = client.createSession(
    onUserInputRequest: { req, sessionId ->
        println "Agent asks: ${req.question}"
        if (req.choices) {
            req.choices.eachWithIndex { choice, i ->
                println "  ${i + 1}. ${choice}"
            }
        }
        def answer = System.console()?.readLine('Your answer: ') ?: ''
        new UserInputResponse(answer, req.allowFreeform ?: true)
    }
)
```

## Image Generation

Request image responses:

```groovy
def result = session.sendAndWait(
    prompt: 'Generate a sunset over mountains',
    responseFormat: 'image',
    imageOptions: new ImageOptions('1024x1024', 'hd', 'natural')
)
println "Image URL: ${result?.data?.url}"
```

## Foreground/Background Sessions (TUI Mode)

```groovy
def session1 = client.createSession()
def session2 = client.createSession()

// Set the active session for TUI mode
client.foregroundSessionId = session1.sessionId

// Switch to the other session
client.foregroundSessionId = session2.sessionId

// Query which session is active
def activeId = client.foregroundSessionId
println "Active session: ${activeId}"
```

## Ping and Status Checks

```groovy
// Ping the server
def ping = client.ping('hello')
println "Pong: ${ping.message}, protocol v${ping.protocolVersion}"

// Get CLI status
def status = client.status
println "CLI version: ${status.version}"

// Get auth status
def auth = client.authStatus
println "Authenticated: ${auth.isAuthenticated}, user: ${auth.login}"
```
