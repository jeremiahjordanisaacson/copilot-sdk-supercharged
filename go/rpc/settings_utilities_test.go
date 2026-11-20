// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc_test

import (
	"bytes"
	"context"
	"encoding/json"
	"net"
	"reflect"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestSettingsLoadForEditRequestEncoding(t *testing.T) {
	configDir := "/scoped-config"
	for _, test := range []struct {
		name   string
		params *rpc.UserSettingsLoadForConfigDirRequest
		want   string
	}{
		{"default", &rpc.UserSettingsLoadForConfigDirRequest{}, `{}`},
		{"nil", nil, `{}`},
		{"scoped", &rpc.UserSettingsLoadForConfigDirRequest{ConfigDir: &configDir}, `{"configDir":"/scoped-config"}`},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			server := jsonrpc2.NewClient(serverConn, serverConn)
			requests := make(chan json.RawMessage, 1)
			server.SetRequestHandler("user.settings.loadForEdit", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
				requests <- params
				return json.RawMessage(`{"status":"missing"}`), nil
			})
			client.Start()
			server.Start()
			defer client.Stop()
			defer server.Stop()
			ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
			defer cancel()
			if _, err := rpc.NewInternalServerRPC(client).User.Settings().LoadForEdit(ctx, test.params); err != nil {
				t.Fatal(err)
			}
			select {
			case got := <-requests:
				if string(got) != test.want {
					t.Fatalf("params = %s, want %s", got, test.want)
				}
			case <-ctx.Done():
				t.Fatal(ctx.Err())
			}
		})
	}
}

func TestSettingsRedactionPreservesJSONValues(t *testing.T) {
	decode := func(t *testing.T, data []byte) any {
		t.Helper()
		decoder := json.NewDecoder(bytes.NewReader(data))
		decoder.UseNumber()
		var value any
		if err := decoder.Decode(&value); err != nil {
			t.Fatal(err)
		}
		return value
	}
	for _, input := range []string{
		`{"value":{"token":"******","extra":{"nullable":null,"items":[true,12.5,"text"]}}}`,
		`{"value":[null,{"arbitrary":"preserved"}]}`,
		`{"value":null}`,
		`{"value":false}`,
		`{"value":12.5}`,
		`{"value":"******"}`,
		`{"value":9007199254740993}`,
		`{"value":-9007199254740993}`,
		`{"value":18446744073709551615}`,
		`{"value":{"token":"******","padding":9007199254740993,"extra":{"items":[-9007199254740993,18446744073709551615,null,1.25]}}}`,
	} {
		t.Run(input, func(t *testing.T) {
			var decoded rpc.SettingsRedactValueForDisplayResult
			if err := json.Unmarshal([]byte(input), &decoded); err != nil {
				t.Fatal(err)
			}
			output, err := json.Marshal(decoded)
			if err != nil {
				t.Fatal(err)
			}
			expected, actual := decode(t, []byte(input)), decode(t, output)
			if !reflect.DeepEqual(actual, expected) {
				t.Fatalf("redacted JSON changed:\nactual: %s\nexpected: %s", output, input)
			}
		})
	}
}
