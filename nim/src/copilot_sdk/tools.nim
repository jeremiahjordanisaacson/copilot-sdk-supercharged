## Tool definition helpers for the GitHub Copilot SDK for Nim.
##
## Provides the `defineTool` template and JSON schema generation
## for registering tools that the Copilot model can invoke.

import std/[json, tables]
import ./types

# ---------------------------------------------------------------------------
# JSON Schema generation from ToolParameter sequences
# ---------------------------------------------------------------------------

proc toJsonSchema*(params: seq[ToolParameter]): JsonNode =
  ## Convert a sequence of ToolParameter into a JSON Schema object.
  let properties = newJObject()
  var requiredFields = newJArray()

  for p in params:
    properties[p.name] = %*{
      "type": p.`type`,
      "description": p.description,
    }
    if p.required:
      requiredFields.add(%p.name)

  result = %*{
    "type": "object",
    "properties": properties,
  }
  if requiredFields.len > 0:
    result["required"] = requiredFields

# ---------------------------------------------------------------------------
# Tool builder
# ---------------------------------------------------------------------------

proc newTool*(name, description: string; params: seq[ToolParameter];
              handler: ToolHandler): Tool =
  ## Create a Tool with the given name, description, parameters, and handler.
  Tool(
    name: name,
    description: description,
    parameters: params,
    handler: handler,
  )

proc newToolParameter*(name, `type`, description: string;
                       required = true): ToolParameter =
  ## Create a single ToolParameter descriptor.
  ToolParameter(
    name: name,
    `type`: `type`,
    description: description,
    required: required,
  )

# ---------------------------------------------------------------------------
# Convenience template for inline tool definition
# ---------------------------------------------------------------------------

template defineTool*(toolName, toolDesc: string;
                     toolParams: seq[ToolParameter];
                     body: untyped): Tool =
  ## Define a tool inline. Inside `body`, `params` is a `JsonNode`
  ## containing the invocation parameters from the model.
  newTool(toolName, toolDesc, toolParams,
    proc(params {.inject.}: JsonNode): string =
      body
  )

# ---------------------------------------------------------------------------
# Serialization helpers
# ---------------------------------------------------------------------------

proc toRegistrationJson*(tool: Tool): JsonNode =
  ## Serialize a Tool to the JSON format expected by the CLI
  ## `tools.register` RPC method.
  result = %*{
    "name": tool.name,
    "description": tool.description,
    "parameters": toJsonSchema(tool.parameters),
  }

proc toolsToJson*(tools: seq[Tool]): JsonNode =
  ## Serialize a list of tools to a JSON array for registration.
  result = newJArray()
  for t in tools:
    result.add(t.toRegistrationJson())
