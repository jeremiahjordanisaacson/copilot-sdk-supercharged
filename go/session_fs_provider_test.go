// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot

import (
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

type failingWriteProvider struct {
	SessionFSProvider
	err error
}

func (p failingWriteProvider) WriteFile(string, string, *int) error  { return p.err }
func (p failingWriteProvider) AppendFile(string, string, *int) error { return p.err }

func TestSessionFSWriteFailure(t *testing.T) {
	for _, tc := range []struct {
		name        string
		err         error
		wantChanged bool
		wantCode    rpc.SessionFSErrorCode
		wantMessage string
	}{
		{"rejected", errors.New("rejected"), false, rpc.SessionFSErrorCodeUNKNOWN, "rejected"},
		{"partial", &SessionFSWriteFailure{Err: errors.New("disk full")}, true, rpc.SessionFSErrorCodeUNKNOWN, "disk full"},
		{"wrapped", fmt.Errorf("write failed: %w", &SessionFSWriteFailure{Err: os.ErrNotExist}), true, rpc.SessionFSErrorCodeENOENT, "write failed: file does not exist"},
		{"nil cause", &SessionFSWriteFailure{}, true, rpc.SessionFSErrorCodeUNKNOWN, "session filesystem write failed after changing the target"},
		{"nil marker", (*SessionFSWriteFailure)(nil), false, rpc.SessionFSErrorCodeUNKNOWN, "session filesystem write failed after changing the target"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			adapter := newSessionFSAdapter(failingWriteProvider{err: tc.err})
			wire, err := adapter.WriteFile(&rpc.SessionFSWriteFileRequest{Path: "/file", Content: "data"})
			if err != nil || wire == nil {
				t.Fatalf("WriteFile = %v, %v; want wire error", wire, err)
			}
			if wire.Code != tc.wantCode {
				t.Errorf("code = %v; want %v", wire.Code, tc.wantCode)
			}
			if wire.Message == nil || *wire.Message != tc.wantMessage {
				t.Errorf("message = %v; want %q", wire.Message, tc.wantMessage)
			}
			gotChanged := wire.WriteChanged != nil && *wire.WriteChanged
			if gotChanged != tc.wantChanged || !tc.wantChanged && wire.WriteChanged != nil {
				t.Errorf("writeChanged = %v; want %v", wire.WriteChanged, tc.wantChanged)
			}
		})
	}
}

func TestSessionFSWriteFailureDoesNotMarkAppend(t *testing.T) {
	adapter := newSessionFSAdapter(failingWriteProvider{
		err: &SessionFSWriteFailure{Err: errors.New("append failed")},
	})
	wire, err := adapter.AppendFile(&rpc.SessionFSAppendFileRequest{Path: "/file", Content: "data"})
	if err != nil || wire == nil || wire.WriteChanged != nil {
		t.Fatalf("AppendFile = %v, %v; want unmarked wire error", wire, err)
	}
}

type readdirProvider struct {
	SessionFSProvider
	names   []string
	entries []rpc.SessionFSReaddirWithTypesEntry
	err     error
}

func (p readdirProvider) ReadDirectory(string) ([]string, error) { return p.names, p.err }
func (p readdirProvider) ReadDirectoryWithTypes(string) ([]rpc.SessionFSReaddirWithTypesEntry, error) {
	return p.entries, p.err
}

func TestSessionFSReaddirCollectionsOnWire(t *testing.T) {
	notFound := &fs.PathError{Op: "open", Path: "/sfs/files", Err: fs.ErrNotExist}
	for _, tc := range []struct {
		name          string
		provider      readdirProvider
		wantNames     string
		wantWithTypes string
	}{
		{
			name:          "nil slice",
			provider:      readdirProvider{},
			wantNames:     `{"entries":[]}`,
			wantWithTypes: `{"entries":[]}`,
		},
		{
			name:          "empty slice",
			provider:      readdirProvider{names: []string{}, entries: []rpc.SessionFSReaddirWithTypesEntry{}},
			wantNames:     `{"entries":[]}`,
			wantWithTypes: `{"entries":[]}`,
		},
		{
			name: "populated",
			provider: readdirProvider{
				names: []string{"a.txt", "d"},
				entries: []rpc.SessionFSReaddirWithTypesEntry{
					{Name: "a.txt", Type: rpc.SessionFSReaddirWithTypesEntryTypeFile},
					{Name: "d", Type: rpc.SessionFSReaddirWithTypesEntryTypeDirectory},
				},
			},
			wantNames:     `{"entries":["a.txt","d"]}`,
			wantWithTypes: `{"entries":[{"name":"a.txt","type":"file"},{"name":"d","type":"directory"}]}`,
		},
		{
			name:          "not found",
			provider:      readdirProvider{err: notFound},
			wantNames:     `{"entries":[],"error":{"code":"ENOENT","message":"open /sfs/files: file does not exist"}}`,
			wantWithTypes: `{"entries":[],"error":{"code":"ENOENT","message":"open /sfs/files: file does not exist"}}`,
		},
		{
			name:          "other error",
			provider:      readdirProvider{err: errors.New("disk offline")},
			wantNames:     `{"entries":[],"error":{"code":"UNKNOWN","message":"disk offline"}}`,
			wantWithTypes: `{"entries":[],"error":{"code":"UNKNOWN","message":"disk offline"}}`,
		},
		{
			name: "entries with error",
			provider: readdirProvider{
				names:   []string{"partial"},
				entries: []rpc.SessionFSReaddirWithTypesEntry{{Name: "partial", Type: rpc.SessionFSReaddirWithTypesEntryTypeFile}},
				err:     errors.New("listing interrupted"),
			},
			wantNames:     `{"entries":[],"error":{"code":"UNKNOWN","message":"listing interrupted"}}`,
			wantWithTypes: `{"entries":[],"error":{"code":"UNKNOWN","message":"listing interrupted"}}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			runtime := newSessionFSWireRuntime(t, tc.provider)
			params := map[string]any{"sessionId": "s1", "path": "/sfs/files"}
			for _, call := range []struct{ method, want string }{
				{"sessionFs.readdir", tc.wantNames},
				{"sessionFs.readdirWithTypes", tc.wantWithTypes},
			} {
				raw, err := runtime.Request(t.Context(), call.method, params)
				if err != nil {
					t.Fatalf("%s: %v", call.method, err)
				}
				if string(raw) != call.want {
					t.Errorf("%s wire result = %s, want %s", call.method, raw, call.want)
				}
			}
		})
	}
}

// newSessionFSWireRuntime serves provider for session s1 the way the client registers a
// session filesystem, and returns the runtime end of the connection.
func newSessionFSWireRuntime(t *testing.T, provider SessionFSProvider) *jsonrpc2.Client {
	t.Helper()
	runtimeToClientReader, runtimeToClientWriter := io.Pipe()
	clientToRuntimeReader, clientToRuntimeWriter := io.Pipe()
	runtime := jsonrpc2.NewClient(runtimeToClientWriter, clientToRuntimeReader)
	client := jsonrpc2.NewClient(clientToRuntimeWriter, runtimeToClientReader)
	handlers := &rpc.ClientSessionAPIHandlers{SessionFS: newSessionFSAdapter(provider)}
	rpc.RegisterClientSessionAPIHandlers(client, func(sessionID string) *rpc.ClientSessionAPIHandlers {
		if sessionID == "s1" {
			return handlers
		}
		return nil
	})
	runtime.Start()
	client.Start()
	t.Cleanup(func() {
		runtime.Stop()
		client.Stop()
		_ = runtimeToClientWriter.Close()
		_ = runtimeToClientReader.Close()
		_ = clientToRuntimeWriter.Close()
		_ = clientToRuntimeReader.Close()
	})
	return runtime
}
