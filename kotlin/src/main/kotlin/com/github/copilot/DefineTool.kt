/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

/**
 * DSL-style helper for defining tools with a builder pattern.
 *
 * Example using the builder:
 * ```kotlin
 * val weatherTool = defineTool("get_weather") {
 *     description = "Get weather for a location"
 *     parameters = mapOf(
 *         "type" to "object",
 *         "properties" to mapOf(
 *             "location" to mapOf("type" to "string", "description" to "City name")
 *         ),
 *         "required" to listOf("location")
 *     )
 *     handler = { args, invocation ->
 *         val arguments = args as? Map<*, *>
 *         val location = arguments?.get("location") ?: "unknown"
 *         "72F in $location"
 *     }
 * }
 * ```
 *
 * Example using the class directly:
 * ```kotlin
 * val tool = ToolBuilder("get_weather")
 *     .description("Get weather for a location")
 *     .parameters(mapOf("type" to "object", ...))
 *     .handler { args, invocation -> "result" }
 *     .build()
 * ```
 */
class ToolBuilder(private val name: String) {
    /** Description of what the tool does. */
    var description: String? = null

    /** JSON Schema parameters for the tool. */
    var parameters: Map<String, Any?>? = null

    /** Handler function that executes the tool. */
    var handler: ToolHandler? = null

    /** Sets the description and returns this builder for chaining. */
    fun description(description: String): ToolBuilder {
        this.description = description
        return this
    }

    /** Sets the parameters and returns this builder for chaining. */
    fun parameters(parameters: Map<String, Any?>): ToolBuilder {
        this.parameters = parameters
        return this
    }

    /** Sets the handler and returns this builder for chaining. */
    fun handler(handler: ToolHandler): ToolBuilder {
        this.handler = handler
        return this
    }

    /**
     * Builds the [Tool] instance.
     *
     * @throws IllegalStateException if handler is not set
     */
    fun build(): Tool {
        val h = handler ?: throw IllegalStateException("Tool handler is required for tool '$name'")
        return Tool(
            name = name,
            description = description,
            parameters = parameters,
            handler = h
        )
    }
}

/**
 * DSL function for defining a tool using a lambda-based builder.
 *
 * ```kotlin
 * val myTool = defineTool("my_tool") {
 *     description = "Does something useful"
 *     parameters = mapOf(
 *         "type" to "object",
 *         "properties" to mapOf(
 *             "input" to mapOf("type" to "string")
 *         )
 *     )
 *     handler = { args, invocation ->
 *         "processed: $args"
 *     }
 * }
 * ```
 *
 * @param name The unique name of the tool
 * @param block Configuration block for the tool builder
 * @return A fully configured [Tool] instance
 */
fun defineTool(name: String, block: ToolBuilder.() -> Unit): Tool {
    return ToolBuilder(name).apply(block).build()
}

/**
 * Creates a tool directly without using the builder DSL.
 *
 * This is a convenience function for simple tool definitions:
 * ```kotlin
 * val tool = createTool(
 *     name = "add",
 *     description = "Add two numbers",
 *     parameters = mapOf(
 *         "type" to "object",
 *         "properties" to mapOf(
 *             "a" to mapOf("type" to "number"),
 *             "b" to mapOf("type" to "number")
 *         ),
 *         "required" to listOf("a", "b")
 *     )
 * ) { args, _ ->
 *     val map = args as? Map<*, *>
 *     val a = (map?.get("a") as? Number)?.toDouble() ?: 0.0
 *     val b = (map?.get("b") as? Number)?.toDouble() ?: 0.0
 *     a + b
 * }
 * ```
 */
fun createTool(
    name: String,
    description: String? = null,
    parameters: Map<String, Any?>? = null,
    handler: ToolHandler
): Tool = Tool(
    name = name,
    description = description,
    parameters = parameters,
    handler = handler
)
