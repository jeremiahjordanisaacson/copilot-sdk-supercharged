/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import groovy.transform.CompileStatic

/**
 * Builder for defining custom tools with Groovy closures.
 *
 * <p>Uses a fluent builder pattern that feels natural in Groovy:
 * <pre>
 * def tool = DefineTool.create('get_weather')
 *     .description('Get weather for a location')
 *     .parameters([
 *         type: 'object',
 *         properties: [
 *             location: [type: 'string', description: 'City name']
 *         ],
 *         required: ['location']
 *     ])
 *     .handler { args, invocation ->
 *         "72F in ${args.location}"
 *     }
 *     .build()
 * </pre>
 */
@CompileStatic
class DefineTool {
    private String name
    private String description
    private Map<String, Object> parameters
    private Closure handler
    private Boolean idempotent

    private DefineTool(String name) {
        this.name = name
    }

    /** Creates a new tool builder with the given name. */
    static DefineTool create(String name) {
        new DefineTool(name)
    }

    /** Sets the tool description. */
    DefineTool description(String description) {
        this.description = description
        this
    }

    /** Sets the JSON Schema parameters for the tool. */
    DefineTool parameters(Map<String, Object> parameters) {
        this.parameters = parameters
        this
    }

    /**
     * Declares that repeating a call with the same arguments has no additional effect, so the
     * runtime may safely retry it (sent as {@code idempotent} in the tool definition).
     */
    DefineTool idempotent(boolean idempotent) {
        this.idempotent = idempotent
        this
    }

    /**
     * Sets the handler closure.
     * The closure receives (Object args, ToolInvocation invocation) and returns a result.
     */
    DefineTool handler(Closure handler) {
        this.handler = handler
        this
    }

    /** Builds the Tool instance. */
    Types.Tool build() {
        if (!name) {
            throw new IllegalStateException('Tool name is required')
        }
        if (handler == null) {
            throw new IllegalStateException('Tool handler is required')
        }
        Types.Tool tool = new Types.Tool(name, description, parameters, handler)
        tool.idempotent = idempotent
        tool
    }
}
