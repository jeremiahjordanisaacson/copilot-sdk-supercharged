package copilot

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"testing"
)

func TestDefineTool(t *testing.T) {
	t.Run("creates tool with correct name and description", func(t *testing.T) {
		type Params struct {
			Query string `json:"query"`
		}

		tool := DefineTool("search", "Search for something",
			func(params Params, inv ToolInvocation) (any, error) {
				return "result", nil
			})

		if tool.Name != "search" {
			t.Errorf("Expected name 'search', got %q", tool.Name)
		}
		if tool.Description != "Search for something" {
			t.Errorf("Expected description 'Search for something', got %q", tool.Description)
		}
		if tool.Handler == nil {
			t.Error("Expected handler to be set")
		}
		if tool.Parameters == nil {
			t.Error("Expected parameters schema to be generated")
		}
	})

	t.Run("generates schema from struct tags", func(t *testing.T) {
		type Params struct {
			City string `json:"city"`
			Unit string `json:"unit"`
		}

		tool := DefineTool("get_weather", "Get weather",
			func(params Params, inv ToolInvocation) (any, error) {
				return "sunny", nil
			})

		schema := tool.Parameters
		if schema["type"] != "object" {
			t.Errorf("Expected schema type 'object', got %v", schema["type"])
		}

		props, ok := schema["properties"].(map[string]any)
		if !ok {
			t.Fatalf("Expected properties to be map, got %T", schema["properties"])
		}

		if _, ok := props["city"]; !ok {
			t.Error("Expected 'city' property in schema")
		}
		if _, ok := props["unit"]; !ok {
			t.Error("Expected 'unit' property in schema")
		}
	})

	t.Run("handler receives typed arguments", func(t *testing.T) {
		type Params struct {
			Name  string `json:"name"`
			Count int    `json:"count"`
		}

		var receivedParams Params
		tool := DefineTool("test", "Test tool",
			func(params Params, inv ToolInvocation) (any, error) {
				receivedParams = params
				return "ok", nil
			})

		inv := ToolInvocation{
			SessionID:  "session-1",
			ToolCallID: "call-1",
			ToolName:   "test",
			Arguments: map[string]any{
				"name":  "Alice",
				"count": float64(42), // JSON numbers are float64
			},
		}

		_, err := tool.Handler(inv)
		if err != nil {
			t.Fatalf("Handler returned error: %v", err)
		}

		if receivedParams.Name != "Alice" {
			t.Errorf("Expected name 'Alice', got %q", receivedParams.Name)
		}
		if receivedParams.Count != 42 {
			t.Errorf("Expected count 42, got %d", receivedParams.Count)
		}
	})

	t.Run("handler receives ToolInvocation", func(t *testing.T) {
		type Params struct{}

		var receivedInv ToolInvocation
		tool := DefineTool("test", "Test tool",
			func(params Params, inv ToolInvocation) (any, error) {
				receivedInv = inv
				return "ok", nil
			})

		inv := ToolInvocation{
			SessionID:  "session-123",
			ToolCallID: "call-456",
			ToolName:   "test",
			Arguments:  map[string]any{},
		}

		tool.Handler(inv)

		if receivedInv.SessionID != "session-123" {
			t.Errorf("Expected SessionID 'session-123', got %q", receivedInv.SessionID)
		}
		if receivedInv.ToolCallID != "call-456" {
			t.Errorf("Expected ToolCallID 'call-456', got %q", receivedInv.ToolCallID)
		}
	})

	t.Run("handler error is propagated", func(t *testing.T) {
		type Params struct{}

		tool := DefineTool("failing", "A failing tool",
			func(params Params, inv ToolInvocation) (any, error) {
				return nil, errors.New("something went wrong")
			})

		inv := ToolInvocation{
			Arguments: map[string]any{},
		}

		_, err := tool.Handler(inv)
		if err == nil {
			t.Fatal("Expected error, got nil")
		}
		if err.Error() != "something went wrong" {
			t.Errorf("Expected error 'something went wrong', got %q", err.Error())
		}
	})
}

func TestNormalizeResult(t *testing.T) {
	t.Run("nil returns empty success result", func(t *testing.T) {
		result, err := normalizeResult(nil)
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		if result.TextResultForLLM != "" {
			t.Errorf("Expected empty TextResultForLLM, got %q", result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected ResultType 'success', got %q", result.ResultType)
		}
	})

	t.Run("string passes through directly", func(t *testing.T) {
		result, err := normalizeResult("hello world")
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		if result.TextResultForLLM != "hello world" {
			t.Errorf("Expected 'hello world', got %q", result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected ResultType 'success', got %q", result.ResultType)
		}
	})

	t.Run("ToolResult passes through directly", func(t *testing.T) {
		input := ToolResult{
			TextResultForLLM: "custom result",
			ResultType:       "failure",
			Error:            "some error",
		}

		result, err := normalizeResult(input)
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		if result.TextResultForLLM != "custom result" {
			t.Errorf("Expected 'custom result', got %q", result.TextResultForLLM)
		}
		if result.ResultType != "failure" {
			t.Errorf("Expected ResultType 'failure', got %q", result.ResultType)
		}
		if result.Error != "some error" {
			t.Errorf("Expected Error 'some error', got %q", result.Error)
		}
	})

	t.Run("struct is JSON serialized", func(t *testing.T) {
		type Response struct {
			Status string `json:"status"`
			Count  int    `json:"count"`
		}

		result, err := normalizeResult(Response{Status: "ok", Count: 5})
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		expected := `{"status":"ok","count":5}`
		if result.TextResultForLLM != expected {
			t.Errorf("Expected %q, got %q", expected, result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected ResultType 'success', got %q", result.ResultType)
		}
	})

	t.Run("map is JSON serialized", func(t *testing.T) {
		result, err := normalizeResult(map[string]any{
			"key": "value",
		})
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		expected := `{"key":"value"}`
		if result.TextResultForLLM != expected {
			t.Errorf("Expected %q, got %q", expected, result.TextResultForLLM)
		}
	})

	t.Run("slice is JSON serialized", func(t *testing.T) {
		result, err := normalizeResult([]string{"a", "b", "c"})
		if err != nil {
			t.Fatalf("Unexpected error: %v", err)
		}

		expected := `["a","b","c"]`
		if result.TextResultForLLM != expected {
			t.Errorf("Expected %q, got %q", expected, result.TextResultForLLM)
		}
	})

	t.Run("returns error for unserializable value", func(t *testing.T) {
		// Channels cannot be JSON serialized
		ch := make(chan int)
		_, err := normalizeResult(ch)
		if err == nil {
			t.Fatal("Expected error for unserializable value, got nil")
		}
	})
}

func TestConvertMCPCallToolResult(t *testing.T) {
	t.Run("typed CallToolResult struct is converted", func(t *testing.T) {
		type Resource struct {
			URI  string `json:"uri"`
			Text string `json:"text"`
		}
		type ContentBlock struct {
			Type     string    `json:"type"`
			Resource *Resource `json:"resource,omitempty"`
		}
		type CallToolResult struct {
			Content []ContentBlock `json:"content"`
		}

		input := CallToolResult{
			Content: []ContentBlock{
				{
					Type:     "resource",
					Resource: &Resource{URI: "file:///report.txt", Text: "details"},
				},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.TextResultForLLM != "details" {
			t.Errorf("Expected 'details', got %q", result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected 'success', got %q", result.ResultType)
		}
	})

	t.Run("text-only CallToolResult is converted", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{"type": "text", "text": "hello"},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.TextResultForLLM != "hello" {
			t.Errorf("Expected 'hello', got %q", result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected 'success', got %q", result.ResultType)
		}
	})

	t.Run("multiple text blocks are joined with newline", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{"type": "text", "text": "line 1"},
				map[string]any{"type": "text", "text": "line 2"},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.TextResultForLLM != "line 1\nline 2" {
			t.Errorf("Expected 'line 1\\nline 2', got %q", result.TextResultForLLM)
		}
	})

	t.Run("isError maps to failure resultType", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{"type": "text", "text": "oops"},
			},
			"isError": true,
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.ResultType != "failure" {
			t.Errorf("Expected 'failure', got %q", result.ResultType)
		}
	})

	t.Run("image content becomes binaryResultsForLLM", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{"type": "image", "data": "base64data", "mimeType": "image/png"},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if len(result.BinaryResultsForLLM) != 1 {
			t.Fatalf("Expected 1 binary result, got %d", len(result.BinaryResultsForLLM))
		}
		if result.BinaryResultsForLLM[0].Data != "base64data" {
			t.Errorf("Expected data 'base64data', got %q", result.BinaryResultsForLLM[0].Data)
		}
		if result.BinaryResultsForLLM[0].MIMEType != "image/png" {
			t.Errorf("Expected mimeType 'image/png', got %q", result.BinaryResultsForLLM[0].MIMEType)
		}
	})

	t.Run("resource text goes to textResultForLLM", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{
					"type":     "resource",
					"resource": map[string]any{"uri": "file:///tmp/data.txt", "text": "file contents"},
				},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.TextResultForLLM != "file contents" {
			t.Errorf("Expected 'file contents', got %q", result.TextResultForLLM)
		}
	})

	t.Run("resource blob goes to binaryResultsForLLM", func(t *testing.T) {
		input := map[string]any{
			"content": []any{
				map[string]any{
					"type":     "resource",
					"resource": map[string]any{"uri": "file:///img.png", "blob": "blobdata", "mimeType": "image/png"},
				},
			},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if len(result.BinaryResultsForLLM) != 1 {
			t.Fatalf("Expected 1 binary result, got %d", len(result.BinaryResultsForLLM))
		}
		if result.BinaryResultsForLLM[0].Description != "file:///img.png" {
			t.Errorf("Expected description 'file:///img.png', got %q", result.BinaryResultsForLLM[0].Description)
		}
	})

	t.Run("non-CallToolResult map returns false", func(t *testing.T) {
		input := map[string]any{
			"key": "value",
		}

		_, ok := ConvertMCPCallToolResult(input)
		if ok {
			t.Error("Expected ConvertMCPCallToolResult to return false for non-CallToolResult map")
		}
	})

	t.Run("empty content array is converted", func(t *testing.T) {
		input := map[string]any{
			"content": []any{},
		}

		result, ok := ConvertMCPCallToolResult(input)
		if !ok {
			t.Fatal("Expected ConvertMCPCallToolResult to succeed")
		}
		if result.TextResultForLLM != "" {
			t.Errorf("Expected empty text, got %q", result.TextResultForLLM)
		}
		if result.ResultType != "success" {
			t.Errorf("Expected 'success', got %q", result.ResultType)
		}
	})
}

func TestGenerateSchemaForType(t *testing.T) {
	t.Run("generates schema for simple struct", func(t *testing.T) {
		type Simple struct {
			Name string `json:"name"`
			Age  int    `json:"age"`
		}

		schema := generateSchemaForType(reflect.TypeOf(Simple{}))

		if schema["type"] != "object" {
			t.Errorf("Expected type 'object', got %v", schema["type"])
		}

		props, ok := schema["properties"].(map[string]any)
		if !ok {
			t.Fatalf("Expected properties map, got %T", schema["properties"])
		}

		nameProp, ok := props["name"].(map[string]any)
		if !ok {
			t.Fatal("Expected 'name' property")
		}
		if nameProp["type"] != "string" {
			t.Errorf("Expected name type 'string', got %v", nameProp["type"])
		}

		ageProp, ok := props["age"].(map[string]any)
		if !ok {
			t.Fatal("Expected 'age' property")
		}
		if ageProp["type"] != "integer" {
			t.Errorf("Expected age type 'integer', got %v", ageProp["type"])
		}
	})

	t.Run("handles nested structs", func(t *testing.T) {
		type Address struct {
			City    string `json:"city"`
			Country string `json:"country"`
		}
		type Person struct {
			Name    string  `json:"name"`
			Address Address `json:"address"`
		}

		schema := generateSchemaForType(reflect.TypeOf(Person{}))

		props := schema["properties"].(map[string]any)
		addrProp, ok := props["address"].(map[string]any)
		if !ok {
			t.Fatal("Expected 'address' property")
		}

		// Nested struct should have properties
		addrProps, ok := addrProp["properties"].(map[string]any)
		if !ok {
			t.Fatal("Expected address to have properties")
		}
		if _, ok := addrProps["city"]; !ok {
			t.Error("Expected 'city' in address properties")
		}
	})

	t.Run("handles pointer types", func(t *testing.T) {
		type Params struct {
			Value string `json:"value"`
		}

		schema := generateSchemaForType(reflect.TypeOf(&Params{}))

		if schema["type"] != "object" {
			t.Errorf("Expected type 'object', got %v", schema["type"])
		}

		props := schema["properties"].(map[string]any)
		if _, ok := props["value"]; !ok {
			t.Error("Expected 'value' property")
		}
	})

	t.Run("handles nil type", func(t *testing.T) {
		schema := generateSchemaForType(nil)

		if schema != nil {
			t.Errorf("Expected nil schema for nil type, got %v", schema)
		}
	})

	t.Run("handles slices", func(t *testing.T) {
		type Params struct {
			Tags []string `json:"tags"`
		}

		schema := generateSchemaForType(reflect.TypeOf(Params{}))

		props := schema["properties"].(map[string]any)
		tagsProp, ok := props["tags"].(map[string]any)
		if !ok {
			t.Fatal("Expected 'tags' property")
		}

		// Schema library may return "array" or ["null", "array"] for slices
		tagType := tagsProp["type"]
		switch v := tagType.(type) {
		case string:
			if v != "array" {
				t.Errorf("Expected tags type 'array', got %v", v)
			}
		case []any:
			hasArray := false
			for _, item := range v {
				if item == "array" {
					hasArray = true
					break
				}
			}
			if !hasArray {
				t.Errorf("Expected tags type to include 'array', got %v", v)
			}
		default:
			t.Errorf("Expected tags type to be string or array, got %T: %v", tagType, tagType)
		}
	})
}

type typedToolArgsNested struct {
	N int64 `json:"n"`
}

type typedToolArgs struct {
	ID     int64               `json:"id"`
	Neg    int64               `json:"neg"`
	U      uint64              `json:"u"`
	Small  int                 `json:"small"`
	F      float64             `json:"f"`
	S      string              `json:"s"`
	Nested typedToolArgsNested `json:"nested"`
	Opt    *int                `json:"opt"`
	Num    json.Number         `json:"num"`
	Raw    json.RawMessage     `json:"raw"`
	Any    any                 `json:"any"`
	List   []int64             `json:"list"`
	A      typedToolArgsNested `json:"a"`
	B      typedToolArgsNested `json:"b"`
}

func renderTypedToolArgs(args typedToolArgs) string {
	opt := "nil"
	if args.Opt != nil {
		opt = strconv.Itoa(*args.Opt)
	}
	return fmt.Sprintf("id=%d neg=%d u=%d small=%d f=%v s=%q n=%d opt=%s num=%s raw=%s any=%#v list=%v a=%d b=%d",
		args.ID, args.Neg, args.U, args.Small, args.F, args.S, args.Nested.N, opt, args.Num, args.Raw, args.Any, args.List, args.A.N, args.B.N)
}

// Each row sends one external_tool.requested event through the client's
// session.event decoding with the arguments exactly as the runtime wrote
// them, and reads back what the tool answered.
func TestTypedToolArgumentsFromTheRuntime(t *testing.T) {
	typed := DefineTool("typed", "", func(args typedToolArgs, _ ToolInvocation) (string, error) {
		return renderTypedToolArgs(args), nil
	})
	tools := []Tool{
		typed,
		{Name: "untyped", Handler: func(inv ToolInvocation) (ToolResult, error) {
			return ToolResult{TextResultForLLM: fmt.Sprintf("%#v", inv.Arguments)}, nil
		}},
		{Name: "edited", Handler: func(inv ToolInvocation) (ToolResult, error) {
			inv.Arguments = map[string]any{"id": float64(7)}
			return typed.Handler(inv)
		}},
		{Name: "replaced-rounded", Handler: func(inv ToolInvocation) (ToolResult, error) {
			inv.Arguments = map[string]any{"id": float64(9007199254740992)}
			return typed.Handler(inv)
		}},
		{Name: "nested-replaced-rounded", Handler: func(inv ToolInvocation) (ToolResult, error) {
			inv.Arguments.(map[string]any)["nested"] = map[string]any{"n": float64(12345678901234568)}
			return typed.Handler(inv)
		}},
		{Name: "swapped", Handler: func(inv ToolInvocation) (ToolResult, error) {
			arguments := inv.Arguments.(map[string]any)
			arguments["a"], arguments["b"] = arguments["b"], arguments["a"]
			return typed.Handler(inv)
		}},
		{Name: "passed-on", Handler: func(inv ToolInvocation) (ToolResult, error) {
			forwarded := inv
			forwarded.ToolName = "renamed"
			return typed.Handler(forwarded)
		}},
	}
	const argsPrefix = "failed to unmarshal arguments into copilot.typedToolArgs: json: cannot unmarshal "
	tests := []struct {
		name      string
		tool      string
		arguments string // omitted from the event when empty
		want      typedToolArgs
		wantText  string
		wantError string
	}{
		// Integers a float64 cannot hold exactly.
		{name: "int64 beyond 2^53", arguments: `{"id":9007199254740993}`, want: typedToolArgs{ID: 9007199254740993}},
		{name: "negative int64 beyond 2^53", arguments: `{"neg":-9007199254740993}`, want: typedToolArgs{Neg: -9007199254740993}},
		{name: "nested int64 beyond 2^53", arguments: `{"nested":{"n":12345678901234567}}`, want: typedToolArgs{Nested: typedToolArgsNested{N: 12345678901234567}}},
		{name: "uint64 maximum", arguments: `{"u":18446744073709551615}`, want: typedToolArgs{U: 18446744073709551615}},
		{name: "int64 beyond 2^53 in an array", arguments: `{"list":[9007199254740993,1]}`, want: typedToolArgs{List: []int64{9007199254740993, 1}}},
		{name: "int64 beyond 2^53 beside an integer-valued float", arguments: `{"id":9007199254740993,"small":5.0}`, want: typedToolArgs{ID: 9007199254740993, Small: 5}},
		{name: "json.Number holding an integer beyond 2^53", arguments: `{"num":9007199254740993}`, want: typedToolArgs{Num: "9007199254740993"}},
		{name: "json.RawMessage holding an integer beyond 2^53", arguments: `{"raw":{"b":9007199254740993}}`, want: typedToolArgs{Raw: json.RawMessage(`{"b":9007199254740993}`)}},

		// Values a float64 holds exactly, non-integers, and integers outside the 64-bit ranges.
		{name: "small values", arguments: `{"id":7,"small":3,"f":0.1,"s":"x","nested":{"n":9},"opt":5}`, want: typedToolArgs{ID: 7, Small: 3, F: 0.1, S: "x", Nested: typedToolArgsNested{N: 9}, Opt: ptr(5)}},
		{name: "integer-valued floats for integer fields", arguments: `{"small":5.0,"id":1e3}`, want: typedToolArgs{Small: 5, ID: 1000}},
		{name: "json.Number holding an integer-valued float", arguments: `{"num":1.0}`, want: typedToolArgs{Num: "1"}},
		{name: "json.RawMessage is re-encoded", arguments: `{"raw":{ "b" : 1.0, "a" : [ 2 ] }}`, want: typedToolArgs{Raw: json.RawMessage(`{"a":[2],"b":1}`)}},
		{name: "float64 field given an integer beyond 2^53", arguments: `{"f":9007199254740993}`, want: typedToolArgs{F: 9007199254740992}},
		{name: "interface field given an integer beyond 2^53", arguments: `{"any":9007199254740993}`, want: typedToolArgs{Any: float64(9007199254740992)}},
		{name: "integer beyond 2^53 written as a float", arguments: `{"id":9007199254740993.0}`, want: typedToolArgs{ID: 9007199254740992}},
		{name: "integer below the int64 minimum", arguments: `{"neg":-9223372036854775809}`, wantError: argsPrefix + "number -9223372036854776000 into Go struct field typedToolArgs.neg of type int64"},
		{name: "integer above the uint64 maximum", arguments: `{"u":18446744073709551616}`, wantError: argsPrefix + "number 18446744073709552000 into Go struct field typedToolArgs.u of type uint64"},
		{name: "string for an integer field", arguments: `{"id":"x"}`, wantError: argsPrefix + "string into Go struct field typedToolArgs.id of type int64"},
		{name: "no arguments", want: typedToolArgs{}},
		{name: "null arguments", arguments: `null`, want: typedToolArgs{}},

		// Handlers that read or replace Arguments.
		{name: "untyped handler reads float64 numbers", tool: "untyped", arguments: `{"id":9007199254740993,"num":1.0}`, wantText: `map[string]interface {}{"id":9.007199254740992e+15, "num":1}`},
		{name: "arguments replaced before the typed handler runs", tool: "edited", arguments: `{"id":9007199254740993}`, want: typedToolArgs{ID: 7}},
		{name: "arguments replaced with the rounded value before the typed handler runs", tool: "replaced-rounded", arguments: `{"id":9007199254740993}`, want: typedToolArgs{ID: 9007199254740992}},
		{name: "nested object replaced with the rounded value before the typed handler runs", tool: "nested-replaced-rounded", arguments: `{"nested":{"n":12345678901234567}}`, want: typedToolArgs{Nested: typedToolArgsNested{N: 12345678901234568}}},
		{name: "objects swapped before the typed handler runs", tool: "swapped", arguments: `{"a":{"n":9007199254740992},"b":{"n":9007199254740993}}`, want: typedToolArgs{A: typedToolArgsNested{N: 9007199254740992}, B: typedToolArgsNested{N: 9007199254740992}}},
		{name: "invocation passed on unchanged by a wrapper", tool: "passed-on", arguments: `{"id":9007199254740993}`, want: typedToolArgs{ID: 9007199254740993}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			session, server := newSetToolsTestSession(t)
			session.eventCh = make(chan SessionEvent, 1)
			session.registerTools(tools)
			tool := tt.tool
			if tool == "" {
				tool = "typed"
			}
			arguments := ""
			if tt.arguments != "" {
				arguments = `,"arguments":` + tt.arguments
			}
			payload := `{"sessionId":"session-1","event":{"id":"00000000-0000-4000-8000-000000000001","timestamp":"2026-01-01T00:00:00Z",` +
				`"type":"external_tool.requested","data":{"requestId":"request-1","sessionId":"session-1","toolCallId":"call-1","toolName":"` + tool + `"` + arguments + `}}}`
			var request sessionEventRequest
			if err := json.Unmarshal([]byte(payload), &request); err != nil {
				t.Fatal(err)
			}
			client := &Client{sessions: map[string]*Session{session.SessionID: session}}
			client.handleSessionEvent(request)

			answer := server.expectRequest(t, "session.tools.handlePendingToolCall")
			defer server.respond(t, answer.ID, map[string]any{})
			if tt.wantError != "" {
				if answer.Params["error"] != tt.wantError {
					t.Errorf("error = %#v, want %q", answer.Params["error"], tt.wantError)
				}
				return
			}
			result, ok := answer.Params["result"].(map[string]any)
			if !ok {
				t.Fatalf("answer = %#v, want a result", answer.Params)
			}
			want := tt.wantText
			if want == "" {
				want = renderTypedToolArgs(tt.want)
			}
			if result["textResultForLlm"] != want {
				t.Errorf("tool received\n  %v\nwant\n  %s", result["textResultForLlm"], want)
			}
		})
	}
}

// A subscriber may edit the event's Arguments, which the tool goroutine also
// holds, while a handler that never reads them runs. Run under -race.
func TestExternalToolArgumentsEditedBySubscriber(t *testing.T) {
	session, server := newSetToolsTestSession(t)
	session.eventCh = make(chan SessionEvent, 1)
	session.registerTools([]Tool{{Name: "ignores", Handler: func(ToolInvocation) (ToolResult, error) {
		return ToolResult{TextResultForLLM: "done"}, nil
	}}})
	payload := `{"sessionId":"session-1","event":{"id":"00000000-0000-4000-8000-000000000001","timestamp":"2026-01-01T00:00:00Z",` +
		`"type":"external_tool.requested","data":{"requestId":"request-1","sessionId":"session-1","toolCallId":"call-1","toolName":"ignores",` +
		`"arguments":{"secret":"x","nested":{"id":9007199254740993},"list":[{"id":1}]}}}}`
	var request sessionEventRequest
	if err := json.Unmarshal([]byte(payload), &request); err != nil {
		t.Fatal(err)
	}
	client := &Client{sessions: map[string]*Session{session.SessionID: session}}
	client.handleSessionEvent(request)

	event := <-session.eventCh
	arguments := event.Data.(*ExternalToolRequestedData).Arguments.(map[string]any)
	delete(arguments, "secret")
	arguments["nested"] = map[string]any{}
	arguments["list"] = []any{}

	answer := server.expectRequest(t, "session.tools.handlePendingToolCall")
	defer server.respond(t, answer.ID, map[string]any{})
	if result, ok := answer.Params["result"].(map[string]any); !ok || result["textResultForLlm"] != "done" {
		t.Fatalf("answer = %#v, want the tool's result", answer.Params)
	}
}
