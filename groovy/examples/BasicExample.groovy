#!/usr/bin/env groovy
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

@Grab('io.github.jeremiahjordanisaacson:copilot-sdk-groovy:2.0.0')
import com.github.copilot.*
import com.github.copilot.Types.*

/**
 * Basic example of using the GitHub Copilot Groovy SDK.
 *
 * Run with: groovy BasicExample.groovy
 * Or with Gradle: gradle run
 */

// Fact lookup database
def facts = [
    javascript: 'JavaScript was created in 10 days by Brendan Eich in 1995.',
    java      : 'Java was created by James Gosling at Sun Microsystems in 1995.',
    groovy    : 'Groovy was created by James Strachan in 2003 and runs on the JVM.',
    node      : 'Node.js lets you run JavaScript outside the browser using V8.'
]

// Define a custom tool using a Groovy closure
def lookupTool = DefineTool.create('lookup_fact')
    .description('Returns a fun fact about a given topic.')
    .parameters([
        type      : 'object',
        properties: [
            topic: [type: 'string', description: 'Topic to look up']
        ],
        required  : ['topic']
    ])
    .handler { args, invocation ->
        def topic = (args as Map).topic?.toString()?.toLowerCase()
        facts[topic] ?: "No fact stored for ${topic}."
    }
    .build()

println 'Starting Copilot SDK Groovy Example\n'

// Create client -- will auto-start CLI server
def client = new CopilotClient(logLevel: 'info')

try {
    def session = client.createSession(tools: [lookupTool])
    println "Session created: ${session.sessionId}\n"

    // Listen to events using a closure
    session.on { event ->
        println "Event [${event.type}]: ${event.data}"
    }

    // Send a simple message
    println 'Sending message...'
    def result1 = session.sendAndWait('Tell me 2+2')
    if (result1) {
        println "Response: ${result1.data?.content}\n"
    }

    // Send a message that uses the tool
    println 'Sending follow-up message...'
    def result2 = session.sendAndWait("Use lookup_fact to tell me about 'groovy'")
    if (result2) {
        println "Response: ${result2.data?.content}\n"
    }

    // List available models
    def models = client.listModels()
    println "Available models: ${models*.name}"

    // Clean up
    session.destroy()
} finally {
    client.stop()
}

println 'Done!'
