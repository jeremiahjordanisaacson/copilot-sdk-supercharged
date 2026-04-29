// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Tool definition helpers

module copilot

import json

// Tool describes a function the model can invoke.
pub struct Tool {
pub mut:
	name        string                  [json: 'name']
	description string                  [json: 'description']
	parameters  map[string]ToolParam    [json: 'parameters']
	handler     fn (map[string]string) !string = unsafe { nil }
}

// ToolParam describes a single parameter for a tool.
pub struct ToolParam {
pub:
	param_type  string [json: 'type']        // "string", "number", "boolean"
	description string [json: 'description']
	required    bool   [json: 'required']
}

// define_tool creates a Tool with the given metadata and handler.
pub fn define_tool(name string, description string, params map[string]ToolParam, handler fn (map[string]string) !string) Tool {
	return Tool{
		name: name
		description: description
		parameters: params
		handler: handler
	}
}

// tool_to_json_schema converts tool parameters to a JSON-schema-style map
// suitable for the wire protocol.
pub fn tool_to_json_schema(t &Tool) map[string]json.Any {
	mut properties := map[string]json.Any{}
	mut required_list := []json.Any{}

	for key, param in t.parameters {
		mut prop := map[string]json.Any{}
		prop['type'] = json.Any(param.param_type)
		prop['description'] = json.Any(param.description)
		properties[key] = json.Any(prop)
		if param.required {
			required_list << json.Any(key)
		}
	}

	mut schema := map[string]json.Any{}
	schema['type'] = json.Any('object')
	schema['properties'] = json.Any(properties)
	if required_list.len > 0 {
		schema['required'] = json.Any(required_list)
	}
	return schema
}

// execute_tool runs the handler for a tool and returns the result.
pub fn execute_tool(t &Tool, args map[string]string) !string {
	if isnil(t.handler) {
		return error('tool "${t.name}" has no handler')
	}
	return t.handler(args)!
}
