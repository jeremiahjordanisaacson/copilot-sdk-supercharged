# Persisting Sessions

Resume and manage persistent conversations across application restarts.

## Resuming a Session

Use `resumeSession` to continue a previous conversation:

```groovy
import com.github.copilot.*

def client = new CopilotClient()

try {
    // First run: create and save the session ID
    def session = client.createSession()
    def sessionId = session.sessionId
    println "Created session: ${sessionId}"

    session.sendAndWait('Remember: my favorite language is Groovy')
    session.destroy()

    // Later: resume the session
    def resumed = client.resumeSession(sessionId)
    def result = resumed.sendAndWait('What is my favorite language?')
    println "Response: ${result?.data?.content}"

    resumed.destroy()
} finally {
    client.stop()
}
```

## Infinite Sessions

Enable infinite sessions for workspace-scoped persistent state:

```groovy
def infiniteConfig = new Types.InfiniteSessionConfig().with {
    enabled = true
    backgroundCompactionThreshold = 0.8
    bufferExhaustionThreshold = 0.95
    it
}

def session = client.createSession(
    infiniteSessions: infiniteConfig
)

println "Workspace path: ${session.workspacePath}"
```

## Listing Persisted Sessions

```groovy
def sessions = client.listSessions()
def persistedSessions = sessions.findAll { !it.isRemote }

persistedSessions.each { meta ->
    println "${meta.sessionId}: ${meta.summary ?: '(no summary)'}"
    println "  Last modified: ${meta.modifiedTime}"
}
```

## Session Cleanup

Delete old sessions to free resources:

```groovy
def sessions = client.listSessions()

// Delete sessions older than 7 days
def cutoff = System.currentTimeMillis() - (7 * 24 * 60 * 60 * 1000)
sessions.each { meta ->
    // Assume modifiedTime is ISO-8601
    try {
        client.deleteSession(meta.sessionId)
        println "Deleted: ${meta.sessionId}"
    } catch (Exception e) {
        println "Failed to delete ${meta.sessionId}: ${e.message}"
    }
}
```

## Saving Session ID to File

```groovy
def sessionFile = new File('.copilot-session')

// Save
def session = client.createSession()
sessionFile.text = session.sessionId

// Restore
if (sessionFile.exists()) {
    def savedId = sessionFile.text.trim()
    def resumed = client.resumeSession(savedId)
    println "Resumed session: ${resumed.sessionId}"
}
```
