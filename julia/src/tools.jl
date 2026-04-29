# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# Tool definition utilities for the Copilot SDK.

"""
    Tool

Represents a tool that can be called by the Copilot assistant.

# Fields
- `name::String` - Unique tool name.
- `description::String` - Human-readable description shown to the model.
- `handler::Function` - Callback `(ToolInvocation) -> ToolResult`.
- `parameters::Union{Dict{String,Any},Nothing}` - JSON Schema for the tool input.
- `overrides_built_in_tool::Bool` - Whether this replaces a built-in CLI tool.
- `skip_permission::Bool` - Whether to skip permission prompts.
"""
Base.@kwdef struct Tool
    name::String
    description::String
    handler::Function
    parameters::Union{Dict{String, Any}, Nothing} = nothing
    overrides_built_in_tool::Bool = false
    skip_permission::Bool = false
end

"""
    define_tool(name, description; parameters=nothing, kwargs...) do invocation
        # ...
        ToolResult(text_result_for_llm="done")
    end

Define a tool using a do-block for the handler. Returns a `Tool`.

# Example

```julia
my_tool = define_tool("greet", "Greets a user") do inv
    name = get(inv.arguments, "name", "World")
    ToolResult(text_result_for_llm="Hello, \$name!")
end
```
"""
function define_tool(
    handler::Function,
    name::String,
    description::String;
    parameters::Union{Dict{String, Any}, Nothing} = nothing,
    overrides_built_in_tool::Bool = false,
    skip_permission::Bool = false,
)
    Tool(;
        name,
        description,
        handler,
        parameters,
        overrides_built_in_tool,
        skip_permission,
    )
end

"""
    define_tool(name, description, handler; kwargs...)

Positional-argument form (handler is the third argument).
"""
function define_tool(
    name::String,
    description::String,
    handler::Function;
    parameters::Union{Dict{String, Any}, Nothing} = nothing,
    overrides_built_in_tool::Bool = false,
    skip_permission::Bool = false,
)
    define_tool(handler, name, description; parameters, overrides_built_in_tool, skip_permission)
end

"""Convert a `Tool` to the JSON-RPC wire format expected by the CLI server."""
function tool_to_wire(t::Tool)
    wire = Dict{String, Any}(
        "name"        => t.name,
        "description" => t.description,
    )
    if t.parameters !== nothing
        wire["parameters"] = t.parameters
    end
    if t.overrides_built_in_tool
        wire["overridesBuiltInTool"] = true
    end
    if t.skip_permission
        wire["skipPermission"] = true
    end
    return wire
end
