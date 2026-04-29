# Streaming

Handle streaming responses and delta events from the Copilot CLI.

## Enabling Streaming

Pass `streaming: true` when creating a session:

```groovy
import com.github.copilot.*

def client = new CopilotClient()

try {
    def session = client.createSession(streaming: true)

    // Listen for streamed content deltas
    session.on('assistant.message_delta') { event ->
        print event.data?.deltaContent ?: ''
    }

    // Listen for the final complete message
    session.on('assistant.message') { event ->
        println "\n\nFull response: ${event.data?.content}"
    }

    session.sendAndWait('Write a haiku about Groovy')
    session.destroy()
} finally {
    client.stop()
}
```

## Streaming with Reasoning

When using models that support reasoning, you can capture reasoning deltas:

```groovy
def session = client.createSession(
    streaming: true,
    model: 'o4-mini',
    reasoningEffort: 'medium'
)

session.on('assistant.reasoning_delta') { event ->
    print "[thinking] ${event.data?.deltaContent ?: ''}"
}

session.on('assistant.message_delta') { event ->
    print event.data?.deltaContent ?: ''
}

session.sendAndWait('What is the derivative of x^3 + 2x?')
```

## Collecting Streamed Chunks

Use a `StringBuilder` to collect chunks for post-processing:

```groovy
def chunks = new StringBuilder()

session.on('assistant.message_delta') { event ->
    def delta = event.data?.deltaContent
    if (delta) chunks.append(delta)
}

session.on('session.idle') { event ->
    def fullText = chunks.toString()
    println "Collected ${fullText.length()} characters"
    chunks.setLength(0) // reset for next message
}

session.sendAndWait('Explain closures in Groovy')
```

## Progress Indicator

Show a spinner while waiting for streamed output:

```groovy
def spinning = true

Thread.start {
    def frames = ['|', '/', '-', '\\']
    int i = 0
    while (spinning) {
        print "\r${frames[i++ % frames.size()]} Thinking..."
        Thread.sleep(100)
    }
    print '\r'
}

session.on('assistant.message_delta') { event ->
    spinning = false
    print event.data?.deltaContent ?: ''
}

session.sendAndWait('Tell me about Groovy metaprogramming')
```
