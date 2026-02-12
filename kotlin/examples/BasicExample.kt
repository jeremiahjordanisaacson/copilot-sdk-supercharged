/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import com.github.copilot.*
import kotlinx.coroutines.runBlocking

/**
 * Basic example of using the GitHub Copilot Kotlin SDK.
 *
 * This example demonstrates:
 * - Creating a client and session
 * - Registering custom tools
 * - Subscribing to events
 * - Sending messages and waiting for responses
 * - Graceful cleanup
 *
 * Prerequisites:
 * - The Copilot CLI must be installed and available on PATH (or set COPILOT_CLI_PATH)
 * - You must be authenticated with GitHub Copilot
 */

val FACTS = mapOf(
    "javascript" to "JavaScript was created in 10 days by Brendan Eich in 1995.",
    "java" to "Java was created by James Gosling at Sun Microsystems in 1995.",
    "kotlin" to "Kotlin was created by JetBrains and first released in 2011.",
    "node" to "Node.js lets you run JavaScript outside the browser using the V8 engine."
)

@Suppress("UNCHECKED_CAST")
fun main() = runBlocking {
    println("Starting Copilot SDK Kotlin Example\n")

    // Define a custom tool using the DSL builder
    val lookupFactTool = defineTool("lookup_fact") {
        description = "Returns a fun fact about a given programming topic."
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf(
                "topic" to mapOf("type" to "string", "description" to "Topic to look up")
            ),
            "required" to listOf("topic")
        )
        handler = { args, _ ->
            // args comes as a JsonElement from the JSON-RPC layer
            val topic = when (args) {
                is kotlinx.serialization.json.JsonObject -> {
                    args["topic"]?.let {
                        (it as? kotlinx.serialization.json.JsonPrimitive)?.content
                    }?.lowercase() ?: ""
                }
                is Map<*, *> -> (args["topic"] as? String)?.lowercase() ?: ""
                else -> ""
            }
            FACTS[topic] ?: "No fact stored for $topic."
        }
    }

    // Create client - will auto-start CLI server via stdio
    val client = CopilotClient(
        CopilotClientOptions(logLevel = "info")
    )

    try {
        // Create a session with our custom tool
        val session = client.createSession(
            SessionConfig(tools = listOf(lookupFactTool))
        )
        println("Session created: ${session.sessionId}\n")

        // Subscribe to all events
        val unsubscribe = session.on { event ->
            println("Event [${event.type}]: ${event.data}")
        }

        // Send a simple math question
        println("Sending message: 'What is 2+2?'")
        val result1 = session.sendAndWait(MessageOptions(prompt = "What is 2+2?"))
        if (result1 != null) {
            val content = result1.data?.get("content")
                ?.let { (it as? kotlinx.serialization.json.JsonPrimitive)?.content }
            println("Response: $content\n")
        }

        // Send a message that should trigger the tool
        println("Sending message: 'Use lookup_fact to tell me about kotlin'")
        val result2 = session.sendAndWait(
            MessageOptions(prompt = "Use the lookup_fact tool to tell me about 'kotlin'")
        )
        if (result2 != null) {
            val content = result2.data?.get("content")
                ?.let { (it as? kotlinx.serialization.json.JsonPrimitive)?.content }
            println("Response: $content\n")
        }

        // Unsubscribe from events and clean up
        unsubscribe()
        session.destroy()
        println("Session destroyed.")
    } finally {
        // Always stop the client to clean up the CLI process
        val errors = client.stop()
        if (errors.isNotEmpty()) {
            println("Cleanup errors: $errors")
        }
    }

    println("Done!")
}
