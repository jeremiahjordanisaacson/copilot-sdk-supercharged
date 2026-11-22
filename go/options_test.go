package copilot

import (
	"encoding/json"
	"reflect"
	"strings"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestGroupedSessionOptionsWireOwnership(t *testing.T) {
	client, requests, cleanup := newInMemoryClient(t)
	defer cleanup()
	options := &CreateSessionOptions{
		Tools:       SessionToolOptions{Definitions: []Tool{}, AvailableTools: []string{}},
		Events:      SessionEventOptions{Streaming: Bool(false)},
		Filesystem:  CreateSessionFilesystemOptions{AdditionalDirectories: []string{}, TrackFileChanges: Bool(false)},
		MCP:         SessionMCPOptions{Servers: map[string]MCPServerConfig{}, DisabledServers: []string{}},
		Inference:   SessionInferenceOptions{AllowMultipleToolCalls: Bool(false)},
		Experiments: SessionExperimentOptions{Flags: map[string]bool{}, EnableExperimentalMode: Bool(false)},
		Permissions: SessionPermissionOptions{OnRequest: PermissionHandler.ApproveAll},
	}
	session, err := client.CreateSession(t.Context(), options)
	if err != nil {
		t.Fatal(err)
	}
	defer session.Disconnect()
	var payload map[string]any
	recorded := requests.snapshot()
	assertRequestMethod(t, recorded, "session.create")
	for _, request := range recorded {
		if request.Method == "session.create" {
			payload = request.Params
		}
	}
	for group, want := range map[string]map[string]any{
		"tools":       {"definitions": []any{}, "availableTools": []any{}},
		"filesystem":  {"additionalDirectories": []any{}, "trackFileChanges": false},
		"mcp":         {"servers": map[string]any{}, "disabledServers": []any{}, "envValueMode": "direct"},
		"inference":   {"allowMultipleToolCalls": false},
		"experiments": {"flags": map[string]any{}, "enableExperimentalMode": false},
		"permissions": {"request": true},
	} {
		if got := sessionWireGroup(payload, group); !reflect.DeepEqual(got, want) {
			t.Errorf("%s = %#v, want %#v", group, got, want)
		}
	}
	if sessionWireGroup(payload, "events")["streaming"] != false {
		t.Fatalf("explicit false streaming was lost: %#v", payload)
	}
	for _, old := range []string{"requestPermission", "mcpServers", "streaming", "allowMultipleToolCalls", "featureFlags"} {
		if _, present := payload[old]; present {
			t.Errorf("legacy top-level field %s was emitted", old)
		}
	}
	if options.Tools.Definitions == nil || options.MCP.Servers == nil || options.Events.Streaming == nil || *options.Events.Streaming {
		t.Fatal("caller-owned options were changed")
	}
}

func TestForDefaultResolvesAtInvocation(t *testing.T) {
	t.Setenv(defaultConnectionEnvVar, "StDiO")
	first := ForDefault(&OwnedRuntimeOptions{BaseDirectory: "first"})
	t.Setenv(defaultConnectionEnvVar, "INPROCESS")
	second := ForDefault(&OwnedRuntimeOptions{BaseDirectory: "second", SessionIdleTimeoutSeconds: 12})
	if conn, ok := first.(SpawnedStdioConnection); !ok || conn.BaseDirectory != "first" || conn.SessionIdleTimeoutSeconds != 0 {
		t.Fatalf("stdio default = %#v", first)
	}
	if conn, ok := second.(InProcessConnection); !ok || conn.BaseDirectory != "second" || conn.SessionIdleTimeoutSeconds != 12 {
		t.Fatalf("in-process default = %#v", second)
	}
	t.Setenv(defaultConnectionEnvVar, "uri")
	defer func() {
		if value := recover(); value == nil || !strings.Contains(value.(string), defaultConnectionEnvVar) {
			t.Fatalf("invalid deployment default did not fail clearly: %v", value)
		}
	}()
	ForDefault(nil)
}

func TestDecodedProviderRoutingPreservesRequestPresence(t *testing.T) {
	for _, inferenceJSON := range []string{
		`{}`,
		`{"defaultProvider":null,"providers":[],"models":[]}`,
		`{"defaultProvider":"named"}`,
	} {
		t.Run(inferenceJSON, func(t *testing.T) {
			client, requests, cleanup := newInMemoryClient(t)
			defer cleanup()
			data := []byte(`{"inference":` + inferenceJSON + `}`)
			var createOptions CreateSessionOptions
			if err := json.Unmarshal(data, &createOptions); err != nil {
				t.Fatal(err)
			}
			createOptions.Permissions.OnRequest = PermissionHandler.ApproveAll
			session, err := client.CreateSession(t.Context(), &createOptions)
			if err != nil {
				t.Fatal(err)
			}
			defer session.Disconnect()
			var resumeOptions ResumeSessionOptions
			if err := json.Unmarshal(data, &resumeOptions); err != nil {
				t.Fatal(err)
			}
			resumeOptions.Permissions.OnRequest = PermissionHandler.ApproveAll
			resumed, err := client.ResumeSession(t.Context(), session.SessionID, &resumeOptions)
			if err != nil {
				t.Fatal(err)
			}
			defer resumed.Disconnect()
			var update rpc.SessionUpdateOptionsParams
			if err := json.Unmarshal(data, &update); err != nil {
				t.Fatal(err)
			}
			if _, err := resumed.RPC.Options.Update(t.Context(), &update); err != nil {
				t.Fatal(err)
			}
			var expected map[string]any
			if err := json.Unmarshal([]byte(inferenceJSON), &expected); err != nil {
				t.Fatal(err)
			}
			recorded := requests.snapshot()
			for _, method := range []string{"session.create", "session.resume", "session.options.update"} {
				assertRequestMethod(t, recorded, method)
			}
			for _, request := range recorded {
				if request.Method != "session.create" && request.Method != "session.resume" && request.Method != "session.options.update" {
					continue
				}
				actual := sessionWireGroup(request.Params, "inference")
				for _, key := range []string{"defaultProvider", "providers", "models"} {
					if request.Method == "session.options.update" && key != "defaultProvider" {
						continue
					}
					want, wantPresent := expected[key]
					got, gotPresent := actual[key]
					if gotPresent != wantPresent || !reflect.DeepEqual(got, want) {
						t.Errorf("%s %s = %#v (present=%t), want %#v (present=%t)", request.Method, key, got, gotPresent, want, wantPresent)
					}
				}
			}
		})
	}
}

func TestGroupedSDKRejectsProtocolThree(t *testing.T) {
	transport, server, _ := newRuntimeShutdownRpcPair(t)
	defer server.Stop()
	server.SetRequestHandler("connect", func(json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
		return json.RawMessage(`{"ok":true,"protocolVersion":3,"version":"old"}`), nil
	})
	client := &Client{
		client: transport, RPC: rpc.NewServerRPC(transport),
		internalRPC: rpc.NewInternalServerRPC(transport),
	}
	err := client.verifyProtocolVersion(t.Context())
	if err == nil || !strings.Contains(err.Error(), "SDK supports versions 4-4") || !strings.Contains(err.Error(), "server reports version 3") {
		t.Fatalf("old flat-wire runtime was not rejected clearly: %v", err)
	}
}
