# Multiple Sessions

Manage multiple independent conversations simultaneously.

## Creating Multiple Sessions

Each session maintains its own conversation history:

```groovy
import com.github.copilot.*

def client = new CopilotClient()

try {
    def codeReview = client.createSession()
    def documentation = client.createSession()

    // Each session tracks its own context
    def reviewResult = codeReview.sendAndWait('Review this function for bugs')
    println "Review: ${reviewResult?.data?.content}"

    def docsResult = documentation.sendAndWait('Write docs for this API endpoint')
    println "Docs: ${docsResult?.data?.content}"

    // Clean up both sessions
    [codeReview, documentation]*.destroy()
} finally {
    client.stop()
}
```

## Listing Active Sessions

```groovy
def sessions = client.listSessions()
sessions.each { meta ->
    println "Session ${meta.sessionId}: ${meta.summary ?: 'no summary'}"
    println "  Started: ${meta.startTime}, Modified: ${meta.modifiedTime}"
}
```

## Session Lifecycle Events

Monitor session creation and deletion across the client:

```groovy
client.on { event ->
    println "Lifecycle [${event.type}]: session ${event.sessionId}"
}

client.on('session.created') { event ->
    println "New session created: ${event.sessionId}"
}
```

## Session with Different Models

```groovy
def fastSession = client.createSession(model: 'gpt-4o-mini')
def powerSession = client.createSession(model: 'claude-sonnet-4-20250514')

def quickAnswer = fastSession.sendAndWait('What is 2+2?')
def deepAnalysis = powerSession.sendAndWait('Analyze this complex algorithm...')

[fastSession, powerSession]*.destroy()
```

## Deleting Sessions

```groovy
def session = client.createSession()
def sid = session.sessionId

// Use the session...
session.sendAndWait('Hello')

// Clean up
session.destroy()
client.deleteSession(sid)
```
