// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"reflect"
	"strings"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

var _ interface {
	List(context.Context, *PluginsListRequest) (*PluginListResult, error)
} = (*ServerPluginsAPI)(nil)

var _ interface {
	List(context.Context) (*PluginList, error)
} = (*PluginsAPI)(nil)

func TestDiscoveryFormattingRedactsInlineConfiguration(t *testing.T) {
	const secret = "discovery-authorization-secret"
	const inline = `{"mcpServers":{"private":{"type":"http","url":"https://example.test","headers":{"Authorization":"Bearer ` + secret + `"}}}}`
	options := MCPDiscoveryOptions{
		AdditionalConfig: []string{inline},
		AvailableTools:   []string{"visible-tool"},
	}
	request := MCPDiscoverRequest{Options: &options, PluginDirectories: []string{"visible-plugin"}}
	const resultJSON = `{"diagnostics":[{"message":"visible-warning","reason":"invalid-server","source":"workspace"}],"servers":[{"configuration":{"headers":{"Authorization":"Bearer ` + secret + `"},"type":"http","url":"https://example.test"},"enabled":true,"name":"visible-server","source":"workspace"}]}`
	var result MCPDiscoverResult
	if err := json.Unmarshal([]byte(resultJSON), &result); err != nil {
		t.Fatal(err)
	}
	for _, tt := range []struct {
		name    string
		value   any
		visible []string
	}{
		{"options", options, []string{"AvailableTools", "visible-tool"}},
		{"options pointer", &options, []string{"AvailableTools", "visible-tool"}},
		{"request", request, []string{"visible-plugin"}},
		{"request pointer", &request, []string{"visible-plugin"}},
		{"result", result, []string{"visible-warning", "visible-server", "workspace"}},
		{"result pointer", &result, []string{"visible-warning", "visible-server", "workspace"}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			before, err := json.Marshal(tt.value)
			if err != nil {
				t.Fatal(err)
			}
			for _, format := range []string{"%v", "%+v", "%#v", "%s", "%q"} {
				t.Run(format, func(t *testing.T) {
					output := fmt.Sprintf(format, tt.value)
					if strings.Contains(output, secret) {
						t.Fatalf("configuration secret leaked through %s: %s", format, output)
					}
					if tt.name != "request" && tt.name != "request pointer" {
						if !strings.Contains(output, "<redacted>") {
							t.Fatalf("missing redaction marker: %s", output)
						}
					}
					for _, visible := range tt.visible {
						if !strings.Contains(output, visible) {
							t.Errorf("missing diagnostic %q: %s", visible, output)
						}
					}
				})
			}
			after, err := json.Marshal(tt.value)
			if err != nil {
				t.Fatal(err)
			}
			if string(after) != string(before) || !strings.Contains(string(after), secret) {
				t.Fatalf("formatting changed wire configuration:\nbefore: %s\nafter: %s", before, after)
			}
		})
	}
	if actual, err := json.Marshal(options); err != nil {
		t.Fatal(err)
	} else if want := `{"additionalConfig":[` + fmt.Sprintf("%q", inline) + `],"availableTools":["visible-tool"]}`; string(actual) != want {
		t.Fatalf("options JSON = %s, want %s", actual, want)
	}
	if actual, err := json.Marshal(result); err != nil {
		t.Fatal(err)
	} else if string(actual) != resultJSON {
		t.Fatalf("result JSON = %s, want %s", actual, resultJSON)
	}
}

func TestDiscoveryCompatibilityOnWire(t *testing.T) {
	clientConn, serverConn := net.Pipe()
	client := jsonrpc2.NewClient(clientConn, clientConn)
	server := jsonrpc2.NewClient(serverConn, serverConn)
	t.Cleanup(func() {
		client.Stop()
		server.Stop()
		clientConn.Close()
		serverConn.Close()
	})
	requests := make(chan json.RawMessage, 1)
	for method, response := range map[string]string{
		"plugins.list":         `{"plugins":[]}`,
		"session.plugins.list": `{"plugins":[]}`,
		"mcp.discover":         `{"servers":[]}`,
	} {
		server.SetRequestHandler(method, func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
			requests <- params
			return json.RawMessage(response), nil
		})
	}
	client.Start()
	server.Start()
	api := NewServerRPC(client)
	session := NewSessionRPC(client, "compatibility-session")
	serverList := api.Plugins.List
	sessionList := session.Plugins.List
	directory := "/workspace"
	cases := []struct {
		name string
		call func() error
		want map[string]any
	}{
		{"server method value", func() error { _, err := serverList(t.Context(), nil); return err }, map[string]any{}},
		{"nil plugin options", func() error { _, err := api.Plugins.List(t.Context(), nil); return err }, map[string]any{}},
		{"plugin options", func() error {
			_, err := api.Plugins.List(t.Context(), &PluginsListRequest{WorkingDirectory: &directory})
			return err
		}, map[string]any{"workingDirectory": directory}},
		{"nil MCP request", func() error { _, err := api.MCP.Discover(t.Context(), nil); return err }, map[string]any{}},
		{"session method value", func() error { _, err := sessionList(t.Context()); return err }, map[string]any{"sessionId": "compatibility-session"}},
	}
	for _, tt := range cases {
		t.Run(tt.name, func(t *testing.T) {
			if err := tt.call(); err != nil {
				t.Fatal(err)
			}
			var actual map[string]any
			if err := json.Unmarshal(<-requests, &actual); err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(actual, tt.want) {
				t.Fatalf("wire params = %#v, want %#v", actual, tt.want)
			}
		})
	}
}
