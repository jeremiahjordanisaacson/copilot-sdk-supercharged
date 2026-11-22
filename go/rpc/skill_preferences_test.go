// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"reflect"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

func TestSkillPreferencesLegacyRequests(t *testing.T) {
	request := SkillsConfigSetSkillDisabledRequest{true, "skill"}
	replacement := SkillsConfigSetDisabledSkillsRequest{[]string{"skill"}}
	for _, test := range []struct {
		request any
		want    map[string]any
	}{
		{request, map[string]any{"disabled": true, "name": "skill"}},
		{replacement, map[string]any{"disabledSkills": []any{"skill"}}},
	} {
		if got := encodeRequest(t, test.request); !reflect.DeepEqual(got, test.want) {
			t.Fatalf("legacy request %T = %v, want %v", test.request, got, test.want)
		}
	}
}

func TestSkillPreferencesLegacyAndProfileCalls(t *testing.T) {
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()
	client := jsonrpc2.NewClient(clientConn, clientConn)
	server := jsonrpc2.NewClient(serverConn, serverConn)
	requests := make(chan map[string]any, 3)
	for _, method := range []string{"skills.config.setSkillDisabled", "skills.config.setDisabledSkills"} {
		server.SetRequestHandler(method, func(raw json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
			var params map[string]any
			if err := json.Unmarshal(raw, &params); err != nil {
				return nil, &jsonrpc2.Error{Code: -32602, Message: err.Error()}
			}
			params["method"] = method
			requests <- params
			return json.RawMessage(`{}`), nil
		})
	}
	client.Start()
	server.Start()
	defer client.Stop()
	defer server.Stop()
	ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
	defer cancel()
	api := NewServerRPC(client).Skills.Config()
	if _, err := api.SetSkillDisabled(ctx, &SkillsConfigSetSkillDisabledRequest{true, "legacy"}); err != nil {
		t.Fatal(err)
	}
	if _, err := api.SetDisabledSkills(ctx, &SkillsConfigSetDisabledSkillsRequest{[]string{"replacement"}}); err != nil {
		t.Fatal(err)
	}
	configDir := "/caller/link/../profile"
	options := SkillsConfigSetSkillDisabledOptions{ConfigDir: &configDir, Disabled: false, Name: "profile"}
	if _, err := api.SetSkillDisabledWithOptions(ctx, &options); err != nil {
		t.Fatal(err)
	}
	for _, want := range []map[string]any{
		{"method": "skills.config.setSkillDisabled", "disabled": true, "name": "legacy"},
		{"method": "skills.config.setDisabledSkills", "disabledSkills": []any{"replacement"}},
		{"method": "skills.config.setSkillDisabled", "configDir": configDir, "disabled": false, "name": "profile"},
	} {
		select {
		case got := <-requests:
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("skill preference request = %v, want %v", got, want)
			}
		case <-ctx.Done():
			t.Fatal(ctx.Err())
		}
	}
	if options.ConfigDir != &configDir || configDir != "/caller/link/../profile" || options.Disabled || options.Name != "profile" {
		t.Fatalf("skill preference options changed: %+v", options)
	}
}

func ExampleSkillsConfigSetSkillDisabledOptions() {
	configDir := "/caller-profile"
	options := SkillsConfigSetSkillDisabledOptions{
		ConfigDir: &configDir,
		Disabled:  true,
		Name:      "weather",
	}
	wire, err := json.Marshal(options)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(wire))
	// Output: {"configDir":"/caller-profile","disabled":true,"name":"weather"}
}
