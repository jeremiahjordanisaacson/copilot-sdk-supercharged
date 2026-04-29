# Error Handling

Patterns for handling errors in the Groovy Copilot SDK.

## Basic Error Handling

Wrap SDK calls in try-catch blocks:

```groovy
import com.github.copilot.*

def client = new CopilotClient()

try {
    def session = client.createSession()
    def response = session.sendAndWait('Explain Groovy closures')
    println response?.data?.content
} catch (Exception e) {
    System.err.println "SDK error: ${e.message}"
} finally {
    client.stop()
}
```

## Handling JSON-RPC Errors

The SDK throws `JsonRpcClient.JsonRpcException` for protocol-level errors:

```groovy
import com.github.copilot.*
import com.github.copilot.JsonRpcClient.JsonRpcException

def client = new CopilotClient()

try {
    client.start()
    def session = client.createSession()
    session.sendAndWait('Hello')
} catch (JsonRpcException e) {
    System.err.println "JSON-RPC error code ${e.code}: ${e.message}"
} catch (RuntimeException e) {
    System.err.println "Runtime error: ${e.message}"
} finally {
    client.stop()
}
```

## Session Error Events

Listen for session-level errors using event handlers:

```groovy
def session = client.createSession()

session.on('session.error') { event ->
    System.err.println "Session error: ${event.data?.message}"
}

session.on('session.idle') { event ->
    println 'Session became idle'
}
```

## Tool Error Handling

Tool handlers should catch their own errors and return descriptive messages:

```groovy
def safeTool = DefineTool.create('safe_operation')
    .description('Performs a safe operation')
    .handler { args, invocation ->
        try {
            // risky operation
            def result = performOperation(args)
            return result
        } catch (Exception e) {
            return [
                textResultForLlm: "Operation failed: ${e.message}",
                resultType      : 'failure',
                error           : e.message,
                toolTelemetry   : [:]
            ]
        }
    }
    .build()
```

## Timeout Handling

Use `sendAndWait` with a custom timeout:

```groovy
try {
    // 2-minute timeout
    def result = session.sendAndWait(prompt: 'Complex analysis task', timeoutMs: 120_000L)
    println result?.data?.content
} catch (RuntimeException e) {
    if (e.message?.contains('Timeout')) {
        println 'Request timed out, aborting...'
        session.abort()
    } else {
        throw e
    }
}
```
