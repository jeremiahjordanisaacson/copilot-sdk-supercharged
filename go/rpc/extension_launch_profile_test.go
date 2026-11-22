// Copyright (c) GitHub. All rights reserved.

package rpc

import (
	"context"
	"maps"
	"net"
	"reflect"
	"slices"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

type extensionLaunchProviderFunc func(*ExtensionLaunchProviderResolveRequest) (*ExtensionLaunchProviderResolveResult, error)

func (f extensionLaunchProviderFunc) Resolve(request *ExtensionLaunchProviderResolveRequest) (*ExtensionLaunchProviderResolveResult, error) {
	return f(request)
}

func TestExtensionLaunchProviderResolveCollectionsOnWire(t *testing.T) {
	for _, test := range []struct {
		name   string
		result *ExtensionLaunchProviderResolveResult
		want   string
	}{
		{
			name:   "nil args and env",
			result: &ExtensionLaunchProviderResolveResult{Launch: &ExtensionLaunchProfile{Executable: "/bin/launcher"}},
			want:   `{"launch":{"args":[],"env":{},"executable":"/bin/launcher"}}`,
		},
		{
			name:   "nil args",
			result: &ExtensionLaunchProviderResolveResult{Launch: &ExtensionLaunchProfile{Env: map[string]string{"A": "1"}, Executable: "/bin/launcher"}},
			want:   `{"launch":{"args":[],"env":{"A":"1"},"executable":"/bin/launcher"}}`,
		},
		{
			name:   "nil env",
			result: &ExtensionLaunchProviderResolveResult{Launch: &ExtensionLaunchProfile{Args: []string{"--flag"}, Executable: "/bin/launcher"}},
			want:   `{"launch":{"args":["--flag"],"env":{},"executable":"/bin/launcher"}}`,
		},
		{
			name:   "empty args and env",
			result: &ExtensionLaunchProviderResolveResult{Launch: &ExtensionLaunchProfile{Args: []string{}, Env: map[string]string{}, Executable: "/bin/launcher"}},
			want:   `{"launch":{"args":[],"env":{},"executable":"/bin/launcher"}}`,
		},
		{
			name: "populated args and env",
			result: &ExtensionLaunchProviderResolveResult{Launch: &ExtensionLaunchProfile{
				Args:       []string{"--flag", ""},
				Env:        map[string]string{"B": "2", "EMPTY": ""},
				Executable: "/bin/launcher",
			}},
			want: `{"launch":{"args":["--flag",""],"env":{"B":"2","EMPTY":""},"executable":"/bin/launcher"}}`,
		},
		{
			name:   "unsupported entrypoint",
			result: &ExtensionLaunchProviderResolveResult{},
			want:   `{}`,
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			var originalArgs []string
			var originalEnv map[string]string
			if test.result.Launch != nil {
				originalArgs = slices.Clone(test.result.Launch.Args)
				originalEnv = maps.Clone(test.result.Launch.Env)
			}

			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			runtime := jsonrpc2.NewClient(serverConn, serverConn)
			RegisterClientGlobalAPIHandlers(client, &ClientGlobalAPIHandlers{
				ExtensionLaunchProvider: extensionLaunchProviderFunc(func(*ExtensionLaunchProviderResolveRequest) (*ExtensionLaunchProviderResolveResult, error) {
					return test.result, nil
				}),
			})
			client.Start()
			runtime.Start()
			defer client.Stop()
			defer runtime.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			raw, err := runtime.Request(ctx, "extensionLaunchProvider.resolve", map[string]any{
				"id": "project:probe", "modulePath": "/work/.github/extensions/probe/extension.mjs", "name": "probe", "source": "project",
			})
			if err != nil {
				t.Fatal(err)
			}
			if string(raw) != test.want {
				t.Errorf("wire result = %s, want %s", raw, test.want)
			}
			if test.result.Launch != nil {
				if !reflect.DeepEqual(test.result.Launch.Args, originalArgs) {
					t.Errorf("provider args changed: got %#v, want %#v", test.result.Launch.Args, originalArgs)
				}
				if !reflect.DeepEqual(test.result.Launch.Env, originalEnv) {
					t.Errorf("provider env changed: got %#v, want %#v", test.result.Launch.Env, originalEnv)
				}
			}
		})
	}
}
