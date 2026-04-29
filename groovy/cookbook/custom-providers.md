# Custom Providers (BYOK)

Use your own model provider with the Copilot SDK.

## OpenAI-Compatible Provider

Connect to any OpenAI-compatible API endpoint:

```groovy
import com.github.copilot.*
import com.github.copilot.Types.*

def client = new CopilotClient()

try {
    def provider = new ProviderConfig().with {
        type = 'openai'
        baseUrl = 'http://localhost:11434/v1'  // Ollama, LM Studio, etc.
        it
    }

    def session = client.createSession(
        model: 'llama3',
        provider: provider
    )

    def result = session.sendAndWait('Explain Groovy in one sentence')
    println result?.data?.content

    session.destroy()
} finally {
    client.stop()
}
```

## Azure OpenAI

```groovy
def provider = new ProviderConfig().with {
    type = 'azure-openai'
    baseUrl = 'https://my-resource.openai.azure.com'
    apiKey = System.getenv('AZURE_OPENAI_KEY')
    it
}

def session = client.createSession(
    model: 'gpt-4o',
    provider: provider
)
```

## Provider with Bearer Token

```groovy
def provider = new ProviderConfig().with {
    type = 'openai'
    baseUrl = 'https://api.example.com/v1'
    bearerToken = System.getenv('API_TOKEN')
    it
}
```

## MCP Servers

Configure MCP (Model Context Protocol) servers for extended tool access:

```groovy
def mcpConfig = new MCPServerConfig().with {
    type = 'stdio'
    command = 'npx'
    args = ['-y', '@modelcontextprotocol/server-filesystem', '/home/user']
    it
}

def session = client.createSession(
    mcpServers: [filesystem: mcpConfig]
)

def result = session.sendAndWait('List files in the home directory')
println result?.data?.content
```

## Custom Agents

Define custom agents with specialized prompts and tools:

```groovy
def agent = new CustomAgentConfig().with {
    name = 'code-reviewer'
    displayName = 'Code Reviewer'
    description = 'Reviews code for bugs and improvements'
    prompt = 'You are an expert code reviewer. Focus on security, performance, and readability.'
    it
}

def session = client.createSession(
    customAgents: [agent]
)
```
