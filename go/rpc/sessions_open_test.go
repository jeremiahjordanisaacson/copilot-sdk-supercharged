// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"encoding/json"
	"io"
	"reflect"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

func TestSessionsOpenPreservesUnionValuesAndWire(t *testing.T) {
	emit := false
	suppress := true
	sessionID := "legacy"
	var inference SessionOpenInferenceOptions
	if err := json.Unmarshal([]byte(`{"defaultProvider":"gateway","providers":[{"name":"gateway","baseUrl":"https://provider.invalid"}],"models":[{"provider":"gateway","id":"friendly","wireModel":"deployment"}]}`), &inference); err != nil {
		t.Fatal(err)
	}
	if err := json.Unmarshal([]byte(`{"reasoningEffort":"high"}`), &inference); err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		params SessionOpenParams
		want   map[string]any
	}{
		{
			SessionsOpenCreate{EmitStart: &emit},
			map[string]any{"kind": "create", "emitStart": false},
		},
		{
			SessionsOpenResume{Resume: &emit, SessionID: sessionID, SuppressResumeWorkspaceMetadataWriteback: &suppress},
			map[string]any{
				"kind": "resume", "resume": false, "sessionId": sessionID,
				"suppressResumeWorkspaceMetadataWriteback": true,
			},
		},
		{nil, nil},
		{
			SessionsOpenCreate{Options: &SessionOpenOptions{Inference: &inference}},
			map[string]any{
				"kind": "create",
				"options": map[string]any{"inference": map[string]any{
					"defaultProvider": "gateway",
					"providers":       []any{map[string]any{"name": "gateway", "baseUrl": "https://provider.invalid"}},
					"models":          []any{map[string]any{"provider": "gateway", "id": "friendly", "wireModel": "deployment"}},
					"reasoningEffort": "high",
				}},
			},
		},
	}
	expected := SessionOpenResult{
		SessionAPI:     map[string]any{"legacy": true},
		SessionID:      &sessionID,
		StartupPrompts: []string{"startup"},
		Status:         SessionsOpenStatusCreated,
	}

	clientReader, clientWriter := io.Pipe()
	serverReader, serverWriter := io.Pipe()
	client := jsonrpc2.NewClient(clientWriter, serverReader)
	server := jsonrpc2.NewClient(serverWriter, clientReader)
	requests := make(chan map[string]any, len(cases))
	server.SetRequestHandler("sessions.open", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
		var request map[string]any
		if err := json.Unmarshal(params, &request); err != nil {
			return nil, &jsonrpc2.Error{Code: -32602, Message: err.Error()}
		}
		requests <- request
		return json.RawMessage(`{"sessionId":"legacy","startupPrompts":["startup"],"status":"created","sessionApi":{"legacy":true},"nativeConstructionPlan":{"host":"private"}}`), nil
	})
	client.Start()
	server.Start()
	t.Cleanup(func() {
		client.Stop()
		server.Stop()
		_ = clientWriter.Close()
		_ = clientReader.Close()
		_ = serverWriter.Close()
		_ = serverReader.Close()
	})

	sessions := NewServerRPC(client).Sessions
	for _, testCase := range cases {
		result, err := sessions.Open(t.Context(), testCase.params)
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(result, &expected) {
			t.Fatalf("open result = %#v, want %#v", result, &expected)
		}
		if actual := <-requests; !reflect.DeepEqual(actual, testCase.want) {
			t.Fatalf("open request = %#v, want %#v", actual, testCase.want)
		}
		encoded, err := json.Marshal(result)
		if err != nil {
			t.Fatal(err)
		}
		var publicResult map[string]any
		if err := json.Unmarshal(encoded, &publicResult); err != nil {
			t.Fatal(err)
		}
		if plan, exists := publicResult["nativeConstructionPlan"]; exists {
			t.Fatalf("private host plan reached the public result: %#v", plan)
		}
	}
	if err := json.Unmarshal([]byte(`{"defaultProvider":null,"models":[]}`), &inference); err != nil {
		t.Fatal(err)
	}
	if inference.DefaultProvider == nil || *inference.DefaultProvider != nil {
		t.Fatalf("explicit null did not clear the default provider: %#v", inference.DefaultProvider)
	}
	if inference.Models == nil || len(inference.Models) != 0 || len(inference.Providers) != 1 || inference.ReasoningEffort == nil || *inference.ReasoningEffort != "high" {
		t.Fatalf("partial clear changed omitted inference fields: %#v", inference)
	}
}
