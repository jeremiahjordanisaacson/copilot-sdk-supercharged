/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package copilot

import (
	"bytes"
	"encoding/json"
	"fmt"
	"maps"
	"reflect"
	"slices"
	"strconv"
	"strings"

	"github.com/google/jsonschema-go/jsonschema"
)

// DefineTool creates a Tool with automatic JSON schema generation from a typed handler function.
// The handler receives typed arguments (automatically unmarshaled from JSON) and the raw ToolInvocation.
// The handler can return any value - strings pass through directly, other types are JSON-serialized.
//
// Example:
//
//	type GetWeatherParams struct {
//	    City string `json:"city" jsonschema:"city name"`
//	    Unit string `json:"unit" jsonschema:"temperature unit (celsius or fahrenheit)"`
//	}
//
//	tool := copilot.DefineTool("get_weather", "Get weather for a city",
//	    func(params GetWeatherParams, inv copilot.ToolInvocation) (any, error) {
//	        return fmt.Sprintf("Weather in %s: 22°%s", params.City, params.Unit), nil
//	    })
func DefineTool[T any, U any](name, description string, handler func(T, ToolInvocation) (U, error)) Tool {
	var zero T
	schema := generateSchemaForType(reflect.TypeOf(zero))

	return Tool{
		Name:        name,
		Description: description,
		Parameters:  schema,
		Handler:     createTypedHandler(handler),
	}
}

// createTypedHandler wraps a typed handler function into the standard ToolHandler signature.
func createTypedHandler[T any, U any](handler func(T, ToolInvocation) (U, error)) ToolHandler {
	return func(inv ToolInvocation) (ToolResult, error) {
		var params T

		// Convert arguments to typed struct via JSON round-trip
		// Arguments is already map[string]any from JSON-RPC parsing
		jsonBytes, err := json.Marshal(argumentsForTypedHandler(inv))
		if err != nil {
			return ToolResult{}, fmt.Errorf("failed to marshal arguments: %w", err)
		}

		if err := json.Unmarshal(jsonBytes, &params); err != nil {
			return ToolResult{}, fmt.Errorf("failed to unmarshal arguments into %T: %w", params, err)
		}

		result, err := handler(params, inv)
		if err != nil {
			return ToolResult{}, err
		}

		return normalizeResult(result)
	}
}

// argumentsForTypedHandler returns the value a typed handler's parameters are
// decoded from. Arguments holds every JSON number as a float64, which rounds
// integers beyond 2^53, so an integer the runtime sent that fits an int64 or a
// uint64 is taken from the original JSON instead. Every other value, including
// an integer-valued float such as 5.0, is the one Arguments holds. Arguments
// is used unchanged when the original JSON is absent, as when the invocation
// was built by the caller, or when the caller replaced or edited Arguments or
// any object or array in it.
func argumentsForTypedHandler(inv ToolInvocation) any {
	if len(inv.rawArguments) == 0 || !sameContainers(inv.deliveredContainers, inv.Arguments) {
		return inv.Arguments
	}
	var decoded any
	if err := json.Unmarshal(inv.rawArguments, &decoded); err != nil || !reflect.DeepEqual(decoded, inv.Arguments) {
		return inv.Arguments
	}
	decoder := json.NewDecoder(bytes.NewReader(inv.rawArguments))
	decoder.UseNumber()
	var numbers any
	if err := decoder.Decode(&numbers); err != nil {
		return inv.Arguments
	}
	return keepExactIntegers(numbers)
}

// argumentContainers returns every object and array in arguments in a fixed
// order, the values a caller can replace or reorder without changing what a
// float64 comparison sees.
func argumentContainers(arguments any) []any {
	var containers []any
	var walk func(any)
	walk = func(value any) {
		switch value := value.(type) {
		case map[string]any:
			containers = append(containers, value)
			for _, key := range slices.Sorted(maps.Keys(value)) {
				walk(value[key])
			}
		case []any:
			containers = append(containers, value)
			for _, item := range value {
				walk(item)
			}
		}
	}
	walk(arguments)
	return containers
}

// sameContainers reports whether arguments holds the objects and arrays in
// delivered, each at the same position, compared by identity. Holding
// delivered keeps those values alive, so a replacement cannot reuse one of
// their addresses.
func sameContainers(delivered []any, arguments any) bool {
	current := argumentContainers(arguments)
	if len(current) != len(delivered) {
		return false
	}
	for i, container := range current {
		if reflect.ValueOf(container).Pointer() != reflect.ValueOf(delivered[i]).Pointer() {
			return false
		}
	}
	return true
}

// keepExactIntegers replaces each json.Number in value with the float64 the
// standard decoder would have produced, except integers that fit an int64 or
// a uint64, which keep their original digits.
func keepExactIntegers(value any) any {
	switch value := value.(type) {
	case map[string]any:
		for key, item := range value {
			value[key] = keepExactIntegers(item)
		}
	case []any:
		for i, item := range value {
			value[i] = keepExactIntegers(item)
		}
	case json.Number:
		if _, err := strconv.ParseInt(value.String(), 10, 64); err == nil {
			return value
		}
		if _, err := strconv.ParseUint(value.String(), 10, 64); err == nil {
			return value
		}
		// The standard decoder already accepted this number, so it parses.
		number, _ := value.Float64()
		return number
	}
	return value
}

// normalizeResult converts any value to a ToolResult.
// Strings pass through directly, ToolResult passes through, and other types
// are JSON-serialized.
func normalizeResult(result any) (ToolResult, error) {
	if result == nil {
		return ToolResult{
			TextResultForLLM: "",
			ResultType:       "success",
		}, nil
	}

	// ToolResult passes through directly
	if tr, ok := result.(ToolResult); ok {
		return tr, nil
	}

	// Strings pass through directly
	if str, ok := result.(string); ok {
		return ToolResult{
			TextResultForLLM: str,
			ResultType:       "success",
		}, nil
	}

	// Everything else gets JSON-serialized
	jsonBytes, err := json.Marshal(result)
	if err != nil {
		return ToolResult{}, fmt.Errorf("failed to serialize result: %w", err)
	}

	return ToolResult{
		TextResultForLLM: string(jsonBytes),
		ResultType:       "success",
	}, nil
}

// ConvertMCPCallToolResult converts an MCP CallToolResult value (a map or struct
// with a "content" array and optional "isError" bool) into a ToolResult.
// Returns the converted ToolResult and true if the value matched the expected
// shape, or a zero ToolResult and false otherwise.
func ConvertMCPCallToolResult(value any) (ToolResult, bool) {
	m, ok := value.(map[string]any)
	if !ok {
		jsonBytes, err := json.Marshal(value)
		if err != nil {
			return ToolResult{}, false
		}

		if err := json.Unmarshal(jsonBytes, &m); err != nil {
			return ToolResult{}, false
		}
	}

	contentRaw, exists := m["content"]
	if !exists {
		return ToolResult{}, false
	}

	contentSlice, ok := contentRaw.([]any)
	if !ok {
		return ToolResult{}, false
	}

	// Verify every element has a string "type" field
	for _, item := range contentSlice {
		block, ok := item.(map[string]any)
		if !ok {
			return ToolResult{}, false
		}
		if _, ok := block["type"].(string); !ok {
			return ToolResult{}, false
		}
	}

	var textParts []string
	var binaryResults []ToolBinaryResult

	for _, item := range contentSlice {
		block := item.(map[string]any)
		blockType := block["type"].(string)

		switch blockType {
		case "text":
			if text, ok := block["text"].(string); ok {
				textParts = append(textParts, text)
			}
		case "image":
			data, _ := block["data"].(string)
			mimeType, _ := block["mimeType"].(string)
			if data == "" {
				continue
			}
			binaryResults = append(binaryResults, ToolBinaryResult{
				Data:     data,
				MIMEType: mimeType,
				Type:     "image",
			})
		case "resource":
			if resRaw, ok := block["resource"].(map[string]any); ok {
				if text, ok := resRaw["text"].(string); ok && text != "" {
					textParts = append(textParts, text)
				}
				if blob, ok := resRaw["blob"].(string); ok && blob != "" {
					mimeType, _ := resRaw["mimeType"].(string)
					if mimeType == "" {
						mimeType = "application/octet-stream"
					}
					uri, _ := resRaw["uri"].(string)
					binaryResults = append(binaryResults, ToolBinaryResult{
						Data:        blob,
						MIMEType:    mimeType,
						Type:        "resource",
						Description: uri,
					})
				}
			}
		}
	}

	resultType := "success"
	if isErr, ok := m["isError"].(bool); ok && isErr {
		resultType = "failure"
	}

	tr := ToolResult{
		TextResultForLLM: strings.Join(textParts, "\n"),
		ResultType:       resultType,
	}
	if len(binaryResults) > 0 {
		tr.BinaryResultsForLLM = binaryResults
	}
	return tr, true
}

// generateSchemaForType generates a JSON schema map from a Go type using reflection.
// Panics if schema generation fails, as this indicates a programming error.
func generateSchemaForType(t reflect.Type) map[string]any {
	if t == nil {
		return nil
	}

	// Handle pointer types
	if t.Kind() == reflect.Pointer {
		t = t.Elem()
	}

	// Use google/jsonschema-go to generate the schema
	schema, err := jsonschema.ForType(t, nil)
	if err != nil {
		panic(fmt.Sprintf("failed to generate schema for type %v: %v", t, err))
	}

	// Convert schema to map[string]any
	schemaBytes, err := json.Marshal(schema)
	if err != nil {
		panic(fmt.Sprintf("failed to marshal schema for type %v: %v", t, err))
	}

	var schemaMap map[string]any
	if err := json.Unmarshal(schemaBytes, &schemaMap); err != nil {
		panic(fmt.Sprintf("failed to unmarshal schema for type %v: %v", t, err))
	}

	return schemaMap
}
