# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the Groovy Copilot SDK.

## Defining a Simple Tool

Use `DefineTool.create` with a Groovy closure as the handler:

```groovy
import com.github.copilot.*

def client = new CopilotClient()

try {
    def weatherTool = DefineTool.create('get_weather')
        .description('Get the current weather for a city')
        .parameters([
            type      : 'object',
            properties: [
                city: [type: 'string', description: 'City name']
            ],
            required  : ['city']
        ])
        .handler { args, invocation ->
            def city = (args as Map).city
            // In production, call a real weather API
            """{"city": "${city}", "temp": "72F", "condition": "sunny"}"""
        }
        .build()

    def session = client.createSession(tools: [weatherTool])
    def response = session.sendAndWait('What is the weather in Seattle?')
    println response?.data?.content
} finally {
    client.stop()
}
```

## Multiple Tools with the Spread Operator

Register several tools at once using Groovy's collection idioms:

```groovy
import com.github.copilot.*

def toolDefs = [
    [name: 'read_file', desc: 'Read a file from disk'],
    [name: 'list_files', desc: 'List files in a directory'],
    [name: 'search_code', desc: 'Search code by pattern']
]

def tools = toolDefs.collect { td ->
    DefineTool.create(td.name)
        .description(td.desc)
        .handler { args, inv -> "Result from ${td.name}: ${args}" }
        .build()
}

def client = new CopilotClient()
try {
    def session = client.createSession(tools: tools)
    println "Registered tools: ${tools*.name}"
    def resp = session.sendAndWait('List the files in /src')
    println resp?.data?.content
} finally {
    client.stop()
}
```

## Skills Configuration

Pass skill directories in the session config:

```groovy
def session = client.createSession(
    skillDirectories: ['./skills'],
    disabledSkills: ['test-skill']
)
```

## Best Practices

- Give tools clear, descriptive names so the model knows when to use them.
- Keep handler closures fast; the model waits for the result before continuing.
- Return structured JSON strings from tool handlers for easier model interpretation.
- Use the spread operator (`*.name`) to inspect collections of tools quickly.
