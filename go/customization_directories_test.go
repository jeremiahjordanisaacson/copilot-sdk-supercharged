// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot

import (
	"encoding/json"
	"reflect"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestClient_CustomizationDirectoryAdapters(t *testing.T) {
	for _, tt := range []struct {
		name  string
		roots []string
	}{
		{"omitted", nil},
		{"empty", []string{}},
		{"relative", []string{"relative-root", "other-root"}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			directories := make(map[string][]string)
			for key, suffix := range map[string]string{
				"skillDirectories":       "skills",
				"agentDirectories":       "agents",
				"instructionDirectories": "instructions",
				"hookDirectories":        "hooks",
			} {
				if tt.roots != nil {
					directories[key] = make([]string, 0, len(tt.roots))
					for _, root := range tt.roots {
						directories[key] = append(directories[key], root+"-"+suffix)
					}
				}
			}
			rpcClient, server, _ := newRuntimeShutdownRpcPair(t)
			t.Cleanup(server.Stop)
			t.Cleanup(rpcClient.Stop)
			client := &Client{
				client:   rpcClient,
				RPC:      rpc.NewServerRPC(rpcClient),
				sessions: make(map[string]*Session),
			}
			requests := make(chan json.RawMessage, 2)
			for _, method := range []string{"session.create", "session.resume"} {
				server.SetRequestHandler(method, func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
					requests <- append(json.RawMessage(nil), params...)
					id := sessionIDFromParams(t, params)
					response, err := json.Marshal(map[string]string{"sessionId": id})
					if err != nil {
						return nil, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
					}
					return response, nil
				})
			}
			_, err := client.CreateSession(t.Context(), &CreateSessionOptions{WorkingDirectory: "request-cwd", EnableConfigDiscovery: Bool(false), Hooks: SessionHookOptions{EnableRepositoryHooks: Bool(false), Directories: directories["hookDirectories"]}, Skills: SessionSkillOptions{Directories: directories["skillDirectories"]}, Agents: SessionAgentOptions{Directories: directories["agentDirectories"]}, Instructions: CreateSessionInstructionOptions{Directories: directories["instructionDirectories"]}})
			if err != nil {
				t.Fatal(err)
			}
			_, err = client.ResumeSession(t.Context(), "resumed", &ResumeSessionOptions{WorkingDirectory: "resume-cwd", EnableConfigDiscovery: Bool(false), Hooks: SessionHookOptions{EnableRepositoryHooks: Bool(false), Directories: directories["hookDirectories"]}, Skills: SessionSkillOptions{Directories: directories["skillDirectories"]}, Agents: SessionAgentOptions{Directories: directories["agentDirectories"]}, Instructions: SessionInstructionOptions{Directories: directories["instructionDirectories"]}})
			if err != nil {
				t.Fatal(err)
			}
			for range 2 {
				var payload map[string]json.RawMessage
				if err := json.Unmarshal(<-requests, &payload); err != nil {
					t.Fatal(err)
				}
				for key, group := range map[string]string{"skillDirectories": "skills", "agentDirectories": "agents", "instructionDirectories": "instructions", "hookDirectories": "hooks"} {
					value, present := sessionWireGroup(payload, group)["directories"]
					if tt.roots == nil {
						if present {
							t.Errorf("unexpected %s: %s", key, value)
						}
					} else {
						var actual []string
						if err := json.Unmarshal(value, &actual); err != nil {
							t.Fatal(err)
						}
						if !reflect.DeepEqual(actual, directories[key]) {
							t.Errorf("%s = %#v, want %#v", key, actual, directories[key])
						}
					}
				}
				if string(payload["enableConfigDiscovery"]) != "false" {
					t.Errorf("enableConfigDiscovery = %s, want false", payload["enableConfigDiscovery"])
				}
				if value := sessionWireGroup(payload, "hooks")["enableRepositoryHooks"]; string(value) != "false" {
					t.Errorf("hooks.enableRepositoryHooks = %s, want false", value)
				}
				for group, key := range map[string]string{"filesystem": "additionalDirectories", "agents": "definitions", "hooks": "hasHandlers"} {
					if _, present := sessionWireGroup(payload, group)[key]; present {
						t.Errorf("unexpected %s.%s in %s", group, key, payload[group])
					}
				}
			}
		})
	}
}
