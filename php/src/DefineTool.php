<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Helper for defining tools with a builder pattern.
 *
 * Provides a convenient API for creating Tool instances with automatic
 * result normalization and error handling.
 *
 * Usage:
 *
 *     // Builder pattern
 *     $tool = DefineTool::create('get_weather')
 *         ->description('Get current weather for a location')
 *         ->parameters([
 *             'type' => 'object',
 *             'properties' => [
 *                 'location' => [
 *                     'type' => 'string',
 *                     'description' => 'City name',
 *                 ],
 *             ],
 *             'required' => ['location'],
 *         ])
 *         ->handler(function (mixed $args, ToolInvocation $invocation): string {
 *             $location = $args['location'] ?? 'unknown';
 *             return "The weather in {$location} is sunny, 72F.";
 *         })
 *         ->build();
 *
 *     // Quick function
 *     $tool = DefineTool::simple(
 *         name: 'get_weather',
 *         description: 'Get current weather',
 *         parameters: [...],
 *         handler: fn($args) => "Sunny, 72F",
 *     );
 */
class DefineTool
{
    private string $name;
    private ?string $toolDescription = null;
    private ?array $toolParameters = null;
    private $toolHandler = null;

    private function __construct(string $name)
    {
        $this->name = $name;
    }

    /**
     * Start building a tool definition.
     *
     * @param string $name The tool name
     * @return static
     */
    public static function create(string $name): static
    {
        return new static($name);
    }

    /**
     * Set the tool description.
     *
     * @param string $description Description shown to the LLM
     * @return $this
     */
    public function description(string $description): static
    {
        $this->toolDescription = $description;
        return $this;
    }

    /**
     * Set the tool parameters as a JSON Schema object.
     *
     * @param array $parameters JSON Schema for the tool parameters
     * @return $this
     */
    public function parameters(array $parameters): static
    {
        $this->toolParameters = $parameters;
        return $this;
    }

    /**
     * Set the tool handler function.
     *
     * The handler receives (mixed $args, ToolInvocation $invocation) and can return:
     * - string: wrapped as a successful ToolResultObject
     * - ToolResultObject: passed through directly
     * - array with textResultForLlm + resultType: passed through
     * - any other value: JSON-encoded and wrapped as success
     * - null: treated as failure
     *
     * @param callable $handler The handler function
     * @return $this
     */
    public function handler(callable $handler): static
    {
        $this->toolHandler = $handler;
        return $this;
    }

    /**
     * Build the Tool instance.
     *
     * @return Tool
     * @throws \LogicException If no handler is set
     */
    public function build(): Tool
    {
        if ($this->toolHandler === null) {
            throw new \LogicException("Tool '{$this->name}' requires a handler");
        }

        $rawHandler = $this->toolHandler;

        // Wrap the handler to normalize results and catch errors
        $wrappedHandler = function (mixed $args, ToolInvocation $invocation) use ($rawHandler): ToolResultObject {
            try {
                $result = $rawHandler($args, $invocation);
                return self::normalizeResult($result);
            } catch (\Throwable $e) {
                // Don't expose detailed error information to the LLM for security reasons
                return new ToolResultObject(
                    textResultForLlm: 'Invoking this tool produced an error. Detailed information is not available.',
                    resultType: 'failure',
                    error: $e->getMessage(),
                    toolTelemetry: [],
                );
            }
        };

        return new Tool(
            name: $this->name,
            description: $this->toolDescription,
            parameters: $this->toolParameters,
            handler: $wrappedHandler,
        );
    }

    /**
     * Quick tool definition without the builder pattern.
     *
     * @param string $name Tool name
     * @param callable $handler Handler function
     * @param string|null $description Tool description
     * @param array|null $parameters JSON Schema parameters
     * @return Tool
     */
    public static function simple(
        string $name,
        callable $handler,
        ?string $description = null,
        ?array $parameters = null,
    ): Tool {
        $builder = self::create($name)->handler($handler);
        if ($description !== null) {
            $builder->description($description);
        }
        if ($parameters !== null) {
            $builder->parameters($parameters);
        }
        return $builder->build();
    }

    /**
     * Normalize any return value to a ToolResultObject.
     *
     * @param mixed $result The raw result from the handler
     * @return ToolResultObject
     */
    public static function normalizeResult(mixed $result): ToolResultObject
    {
        if ($result === null) {
            return new ToolResultObject(
                textResultForLlm: 'Tool returned no result',
                resultType: 'failure',
                error: 'tool returned no result',
                toolTelemetry: [],
            );
        }

        // ToolResultObject passes through directly
        if ($result instanceof ToolResultObject) {
            return $result;
        }

        // Array with duck-typed fields
        if (is_array($result) && isset($result['textResultForLlm'], $result['resultType'])) {
            return ToolResultObject::fromArray($result);
        }

        // String result
        if (is_string($result)) {
            return new ToolResultObject(
                textResultForLlm: $result,
                resultType: 'success',
                toolTelemetry: [],
            );
        }

        // Everything else gets JSON-serialized
        $jsonStr = json_encode($result, JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);
        if ($jsonStr === false) {
            return new ToolResultObject(
                textResultForLlm: 'Failed to serialize tool result',
                resultType: 'failure',
                error: 'JSON serialization failed: ' . json_last_error_msg(),
                toolTelemetry: [],
            );
        }

        return new ToolResultObject(
            textResultForLlm: $jsonStr,
            resultType: 'success',
            toolTelemetry: [],
        );
    }
}
