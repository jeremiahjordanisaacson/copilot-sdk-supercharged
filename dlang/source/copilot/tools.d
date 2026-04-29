/// Helper utilities for defining tools with D idioms.
///
/// Provides a `defineTool` factory that builds a `Tool` struct from a
/// name, description, JSON Schema string, and a handler delegate.
module copilot.tools;

import std.json;
import copilot.types : Tool, ToolResult;

/// Signature for a tool handler: receives the call ID and parsed arguments,
/// returns a ToolResult.
alias ToolHandler = ToolResult delegate(string callId, JSONValue arguments) @safe;

/// A tool paired with its runtime handler.
struct ToolBinding
{
    Tool tool;
    ToolHandler handler;
}

/// Convenience factory for building a ToolBinding in one call.
///
/// Params:
///   name        = Machine-readable tool name.
///   description = Human-readable description for the model.
///   schemaJson  = JSON Schema string for the parameters object.
///   handler     = Delegate invoked when the assistant calls this tool.
///
/// Returns:
///   A ToolBinding ready to be registered on a session.
ToolBinding defineTool(
    string name,
    string description,
    string schemaJson,
    ToolHandler handler,
) @safe
{
    return ToolBinding(
        Tool(
            name,
            description,
            parseJSON(schemaJson),
        ),
        handler,
    );
}

/// Overload that accepts a pre-built JSONValue schema.
ToolBinding defineTool(
    string name,
    string description,
    JSONValue schema,
    ToolHandler handler,
) @safe pure nothrow
{
    return ToolBinding(
        Tool(name, description, schema),
        handler,
    );
}

/// Build a simple string-result ToolResult for a successful invocation.
ToolResult successResult(string callId, string value) @safe pure nothrow
{
    return ToolResult(callId, value, false);
}

/// Build an error ToolResult.
ToolResult errorResult(string callId, string message) @safe pure nothrow
{
    return ToolResult(callId, message, true);
}
