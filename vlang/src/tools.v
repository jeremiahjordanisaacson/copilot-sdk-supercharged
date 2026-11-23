// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Tool definition helpers

module copilot

import x.json2

// ToolHandler runs a tool with its decoded arguments and returns the result text.
pub type ToolHandler = fn (args map[string]string) !string

// Tool describes a function the model can invoke.
pub struct Tool {
pub mut:
	name        string                  @[json: 'name']
	description string                  @[json: 'description']
	parameters  map[string]ToolParam    @[json: 'parameters']
	handler     ToolHandler = unsafe { nil }
	idempotent  bool                    @[json: 'idempotent'] // safe to retry with the same arguments
}

// ToolParam describes a single parameter for a tool.
pub struct ToolParam {
pub:
	param_type  string @[json: 'type']        // "string", "number", "boolean"
	description string @[json: 'description']
	required    bool   @[json: 'required']
}

// define_tool creates a Tool with the given metadata and handler.
pub fn define_tool(name string, description string, params map[string]ToolParam, handler ToolHandler) Tool {
	return Tool{
		name: name
		description: description
		parameters: params
		handler: handler
	}
}

// tool_to_json_schema converts tool parameters to a JSON-schema-style map
// suitable for the wire protocol.
pub fn tool_to_json_schema(t &Tool) map[string]json2.Any {
	mut properties := map[string]json2.Any{}
	mut required_list := []json2.Any{}

	for key, param in t.parameters {
		mut prop := map[string]json2.Any{}
		prop['type'] = json2.Any(param.param_type)
		prop['description'] = json2.Any(param.description)
		properties[key] = json2.Any(prop)
		if param.required {
			required_list << json2.Any(key)
		}
	}

	mut schema := map[string]json2.Any{}
	schema['type'] = json2.Any('object')
	schema['properties'] = json2.Any(properties)
	if required_list.len > 0 {
		schema['required'] = json2.Any(required_list)
	}
	return schema
}

// tool_definition_json renders the wire tool definition (`tools.definitions[]`).
pub fn tool_definition_json(t &Tool) string {
	mut parts := map[string]string{}
	parts['name'] = json_str(t.name)
	parts['description'] = json_str(t.description)
	parts['parameters'] = json2.Any(tool_to_json_schema(t)).json_str()
	if t.idempotent {
		parts['idempotent'] = 'true'
	}
	return build_params(parts)
}

// execute_tool runs the handler for a tool and returns the result.
pub fn execute_tool(t &Tool, args map[string]string) !string {
	if isnil(t.handler) {
		return error('tool "${t.name}" has no handler')
	}
	return t.handler(args)!
}
