package rpc

import (
	"bytes"
	"encoding/json"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"path/filepath"
	"reflect"
	"runtime"
	"testing"
)

var (
	_ ExternalToolResult      = ExternalToolStringResult("")
	_ ExternalToolResult      = (*ExternalToolTextResultForLlm)(nil)
	_ FilterMapping           = FilterMappingEnumMap{}
	_ FilterMapping           = ContentFilterModeMarkdown
	_ MCPServerConfig         = (*MCPServerConfigHTTP)(nil)
	_ MCPServerConfig         = (*MCPServerConfigStdio)(nil)
	_ UIElicitationFieldValue = UIElicitationStringValue("")
	_ UIElicitationFieldValue = UIElicitationStringArrayValue(nil)
	_ UIElicitationFieldValue = UIElicitationBooleanValue(false)
	_ UIElicitationFieldValue = UIElicitationNumberValue(0)
)

func TestGeneratedRPCAPIShape(t *testing.T) {
	file, fileSet := parseGeneratedRPC(t)

	assertInterfaceType(t, file, "ExternalToolResult")
	assertTypeExpr(t, fileSet, findTypeSpec(t, file, "ExternalToolStringResult").Type, "string")
	assertStructFieldType(t, file, fileSet, "HandlePendingToolCallRequest", "Result", "ExternalToolResult")

	assertInterfaceType(t, file, "FilterMapping")
	assertTypeExpr(t, fileSet, findTypeSpec(t, file, "FilterMappingEnumMap").Type, "map[string]ContentFilterMode")

	assertInterfaceType(t, file, "MCPServerConfig")
	assertStructFieldType(t, file, fileSet, "MCPConfigAddRequest", "Config", "MCPSerializableServerConfig")
	assertStructFieldType(t, file, fileSet, "MCPConfigList", "Servers", "map[string]MCPSerializableServerConfig")
	assertStructFieldType(t, file, fileSet, "MCPConfigUpdateRequest", "Config", "MCPSerializableServerConfig")
	assertStructFieldType(t, file, fileSet, "MCPServerConfigHTTP", "FilterMapping", "FilterMapping")
	assertStructFieldType(t, file, fileSet, "MCPServerConfigStdio", "FilterMapping", "FilterMapping")
	assertStructFieldType(t, file, fileSet, "ModelSwitchToRequest", "AutoTier", "**AutoTier")
	assertStructFieldType(t, file, fileSet, "TaskClientUpdateProgress", "Percentage", "**float64")
	assertStructFieldType(t, file, fileSet, "TaskClientUpdateProgress", "Phase", "**string")
	assertTypeExpr(t, fileSet, findTypeSpec(t, file, "SubagentSettings").Type, "*SubagentSettingsConfig")
	assertStructFieldType(t, file, fileSet, "ToolsGetSubagentSettingsResult", "Subagents", "SubagentSettings")
	assertStructFieldType(t, file, fileSet, "UpdateSubagentSettingsRequest", "Subagents", "SubagentSettings")

	assertInterfaceType(t, file, "UIElicitationFieldValue")
	assertTypeExpr(t, fileSet, findTypeSpec(t, file, "UIElicitationStringArrayValue").Type, "[]string")
	assertStructFieldType(t, file, fileSet, "UIElicitationResponse", "Content", "map[string]UIElicitationFieldValue")
}

func TestSubagentSettingsReadModifyWrite(t *testing.T) {
	for _, tc := range []struct {
		name     string
		settings string
	}{
		{"absent", ""},
		{"enabled", `,"contextManagementTools":true`},
		{"disabled", `,"contextManagementTools":false`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			input := []byte(`{"subagents":{"agents":{"rubber-duck":{"model":"model-id","providerId":"provider-id","autoInvoke":false}},"disabledSubagents":["explore"],"maxConcurrency":7,"maxDepth":3` + tc.settings + `}}`)
			var result ToolsGetSubagentSettingsResult
			if err := json.Unmarshal(input, &result); err != nil {
				t.Fatalf("decode getter result: %v", err)
			}
			if result.Subagents == nil || result.Subagents.Agents["rubber-duck"].AutoInvoke == nil {
				t.Fatalf("missing configured agent: %#v", result.Subagents)
			}
			*result.Subagents.Agents["rubber-duck"].AutoInvoke = true
			request := UpdateSubagentSettingsRequest{Subagents: result.Subagents}
			encoded, err := json.Marshal(request)
			if err != nil {
				t.Fatalf("encode setter request: %v", err)
			}
			expected := bytes.Replace(input, []byte(`"autoInvoke":false`), []byte(`"autoInvoke":true`), 1)
			var got, want any
			if err := json.Unmarshal(encoded, &got); err != nil {
				t.Fatalf("decode encoded request: %v", err)
			}
			if err := json.Unmarshal(expected, &want); err != nil {
				t.Fatalf("decode expected request: %v", err)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("settings round trip:\ngot: %s\nwant: %s", encoded, expected)
			}
		})
	}
	var result ToolsGetSubagentSettingsResult
	if err := json.Unmarshal([]byte(`{"subagents":null}`), &result); err != nil {
		t.Fatalf("decode absent settings: %v", err)
	}
	if result.Subagents != nil {
		t.Fatalf("null decoded as settings: %#v", result.Subagents)
	}
}

func parseGeneratedRPC(t *testing.T) (*ast.File, *token.FileSet) {
	t.Helper()
	_, currentFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("locate test file")
	}
	fileSet := token.NewFileSet()
	file, err := parser.ParseFile(fileSet, filepath.Join(filepath.Dir(currentFile), "zrpc.go"), nil, 0)
	if err != nil {
		t.Fatalf("parse zrpc.go: %v", err)
	}
	return file, fileSet
}

func findTypeSpec(t *testing.T, file *ast.File, typeName string) *ast.TypeSpec {
	t.Helper()
	for _, decl := range file.Decls {
		genDecl, ok := decl.(*ast.GenDecl)
		if !ok || genDecl.Tok != token.TYPE {
			continue
		}
		for _, spec := range genDecl.Specs {
			typeSpec, ok := spec.(*ast.TypeSpec)
			if ok && typeSpec.Name.Name == typeName {
				return typeSpec
			}
		}
	}
	t.Fatalf("type %s not found", typeName)
	return nil
}

func assertInterfaceType(t *testing.T, file *ast.File, typeName string) {
	t.Helper()
	if _, ok := findTypeSpec(t, file, typeName).Type.(*ast.InterfaceType); !ok {
		t.Fatalf("type %s has unexpected AST node %T", typeName, findTypeSpec(t, file, typeName).Type)
	}
}

func assertStructFieldType(t *testing.T, file *ast.File, fileSet *token.FileSet, structName, fieldName, want string) {
	t.Helper()
	structType, ok := findTypeSpec(t, file, structName).Type.(*ast.StructType)
	if !ok {
		t.Fatalf("type %s is %T, want struct", structName, findTypeSpec(t, file, structName).Type)
	}
	for _, field := range structType.Fields.List {
		for _, name := range field.Names {
			if name.Name == fieldName {
				assertTypeExpr(t, fileSet, field.Type, want)
				return
			}
		}
	}
	t.Fatalf("field %s.%s not found", structName, fieldName)
}

func assertTypeExpr(t *testing.T, fileSet *token.FileSet, expr ast.Expr, want string) {
	t.Helper()
	var buffer bytes.Buffer
	if err := format.Node(&buffer, fileSet, expr); err != nil {
		t.Fatalf("format type expression: %v", err)
	}
	if got := buffer.String(); got != want {
		t.Fatalf("type expression = %s, want %s", got, want)
	}
}
