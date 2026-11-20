// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"math"
	"math/big"
	"net"
	"os"
	"os/exec"
	"reflect"
	"runtime/debug"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

func TestForgeManifestRetainsNullableMetadata(t *testing.T) {
	for _, metadata := range []string{"", `,"after_hash":null,"before_content":null,"before_hash":null`, `,"after_hash":"after","before_content":"before","before_hash":"hash"`} {
		wire := []byte(`{"path":".github/skills/demo/SKILL.md","change_type":"added"` + metadata + `}`)
		var entry ForgeProposalManifestEntry
		if err := json.Unmarshal(wire, &entry); err != nil {
			t.Fatal(err)
		}
		clientConn, serverConn := net.Pipe()
		defer clientConn.Close()
		defer serverConn.Close()
		client := jsonrpc2.NewClient(clientConn, clientConn)
		server := jsonrpc2.NewClient(serverConn, serverConn)
		requests := make(chan json.RawMessage, 1)
		server.SetRequestHandler("sessions.forgeProposals.reconcile", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
			requests <- params
			return json.RawMessage(`{"proposal":null}`), nil
		})
		client.Start()
		server.Start()
		defer client.Stop()
		defer server.Stop()
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if _, err := NewInternalServerRPC(client).Sessions.ForgeProposals().Reconcile(ctx, &ForgeProposalReconcileRequest{
			Scope:       ForgeProposalScope{GitRootPath: "/repo", BranchName: "feature"},
			TriggerMode: "on_demand",
			Fingerprint: "hash",
			Manifest:    []ForgeProposalManifestEntry{entry},
			Summary:     ForgeProposalSummary{FileCount: 1, AddedCount: 1},
		}); err != nil {
			t.Fatal(err)
		}
		var encoded json.RawMessage
		select {
		case raw := <-requests:
			var request struct {
				Manifest []json.RawMessage `json:"manifest"`
			}
			if err := json.Unmarshal(raw, &request); err != nil {
				t.Fatal(err)
			}
			if len(request.Manifest) != 1 {
				t.Fatalf("wrong manifest count: %s", raw)
			}
			encoded = request.Manifest[0]
		case <-ctx.Done():
			t.Fatal(ctx.Err())
		}
		var want, got map[string]any
		if err := json.Unmarshal(wire, &want); err != nil {
			t.Fatal(err)
		}
		if err := json.Unmarshal(encoded, &got); err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("manifest metadata changed: got %s, want %s", encoded, wire)
		}
	}
}

type settingsIntegerMarshaler struct {
	calls int
}

func (value *settingsIntegerMarshaler) MarshalJSON() ([]byte, error) {
	value.calls++
	return []byte(`{"nested":[-9223372036854775809]}`), nil
}

func TestSettingsRejectOversizedIntegerTokens(t *testing.T) {
	integer, _ := new(big.Int).SetString("-9223372036854775809", 10)
	custom := &settingsIntegerMarshaler{}
	for _, test := range []struct {
		name  string
		value any
	}{
		{"lower json.Number", json.Number("-9223372036854775809")},
		{"upper json.Number", json.Number("18446744073709551616")},
		{"big.Int", integer},
		{"raw JSON", json.RawMessage(`{"nested":[18446744073709551616]}`)},
		{"custom marshaler", custom},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			server := jsonrpc2.NewClient(serverConn, serverConn)
			requests := make(chan json.RawMessage, 2)
			server.SetRequestHandler("user.settings.update", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
				requests <- params
				return json.RawMessage(`{"value":{},"applied":true}`), nil
			})
			client.Start()
			server.Start()
			defer client.Stop()
			defer server.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			settings := NewInternalServerRPC(client).User.Settings()
			conditions := map[string]any{"nested": []any{test.value}}
			_, err := settings.Update(ctx, &UserSettingsUpdateRequest{
				Key: "statusLine", Value: map[string]any{"command": "echo after"},
				IfFieldsMatch: conditions,
			})
			var unsupported *json.UnsupportedValueError
			if !errors.As(err, &unsupported) || !strings.Contains(err.Error(), "integer") || !strings.Contains(err.Error(), "range") {
				t.Fatalf("oversized integer error = %v, want integer range error", err)
			}
			select {
			case request := <-requests:
				t.Fatalf("invalid request reached the wire: %s", request)
			default:
			}
			result, err := settings.Update(ctx, &UserSettingsUpdateRequest{
				Key: "statusLine", Value: map[string]any{"command": "echo valid"},
				IfFieldsMatch: map[string]any{"padding": json.Number("-9223372036854775808")},
			})
			if err != nil || result.Applied == nil || !*result.Applied {
				t.Fatalf("connection unusable after rejection: result=%+v error=%v", result, err)
			}
			select {
			case request := <-requests:
				if !bytes.Contains(request, []byte(`"padding":-9223372036854775808`)) {
					t.Fatalf("unexpected request after rejection: %s", request)
				}
			default:
				t.Fatal("valid request did not reach the wire")
			}
		})
	}
	if custom.calls != 1 {
		t.Fatalf("custom marshaler calls = %d, want 1", custom.calls)
	}
}

type settingsTextFloat float64

func (settingsTextFloat) MarshalText() ([]byte, error) {
	return []byte("caller-text"), nil
}

type settingsTextFloat32 float32

func (settingsTextFloat32) MarshalText() ([]byte, error) {
	return []byte("caller-float32"), nil
}

func TestUserSettingsUpdateConditionPresenceOnWire(t *testing.T) {
	score := float64(9.223372036854776e18)
	shared := map[int]float64{7: score}
	type embedded struct {
		Promoted float64 `json:"promoted"`
	}
	structure := struct {
		embedded
		Score       float64     `json:"score"`
		Exact       json.Number `json:"exact"`
		Omitted     float64     `json:"omitted,omitempty"`
		Empty       []float64   `json:"empty,omitzero"`
		Quoted      float64     `json:"quoted,string"`
		QuotedExact json.Number `json:"quotedExact,string"`
		Hidden      float64     `json:"-"`
	}{
		embedded:    embedded{Promoted: score},
		Score:       score,
		Exact:       json.Number("9223372036854776000"),
		Empty:       []float64{},
		Quoted:      score,
		QuotedExact: json.Number("9223372036854776000"),
		Hidden:      score,
	}
	for _, test := range []struct {
		name     string
		expected map[string]any
		wire     string
	}{
		{name: "unconditional"},
		{name: "empty conditions", expected: map[string]any{}, wire: `{}`},
		{name: "selection condition", expected: map[string]any{"selectedModel": nil}, wire: `{"selectedModel":null}`},
		{
			name: "integer endpoints and floating-point controls",
			expected: map[string]any{
				"values": []any{json.Number("-9223372036854775808"), json.Number("18446744073709551615"), float64(1e20), float32(1e20), json.Number("1e20"), "-9223372036854775809"},
			},
			wire: `{"values":[-9223372036854775808,18446744073709551615,100000000000000000000e0,100000000000000000000e0,1e20,"-9223372036854775809"]}`,
		},
		{
			name: "floating-point conditions with exact integers and null containers",
			expected: map[string]any{
				"samples": []any{float64(9.223372036854776e18), json.Number("9007199254740993"), map[string]any(nil), []any(nil), []any{}},
				"score":   float64(-9.223372036854775e18),
			},
			wire: `{"samples":[9223372036854776000e0,9007199254740993,null,null,[]],"score":-9223372036854775000e0}`,
		},
		{
			name:     "struct tags and embedded fields",
			expected: map[string]any{"struct": &structure},
			wire:     `{"struct":{"promoted":9223372036854776000e0,"score":9223372036854776000e0,"exact":9223372036854776000,"empty":[],"quoted":"9223372036854776000","quotedExact":"9223372036854776000"}}`,
		},
		{
			name: "float32 integer equality with standard fractions and custom encoders",
			expected: map[string]any{
				"fraction": float32(0.1),
				"integer":  []float32{9.223372e18, -9.223372e18},
				"custom":   settingsTextFloat32(0.1),
				"quoted": struct {
					Score float32 `json:"score,string"`
				}{0.1},
			},
			wire: `{"custom":"caller-float32","fraction":0.1,"integer":[9223372036854776000e0,-9223372036854776000e0],"quoted":{"score":"0.1"}}`,
		},
		{
			name: "typed JSON containers",
			expected: map[string]any{
				"array":      [1]float64{score},
				"bytes":      []byte{0, 255},
				"empty":      []float64{},
				"matrix":     []map[int]float64{shared, shared},
				"nil":        []float64(nil),
				"number-key": map[json.Number]float64{"": score, "7": score, "not-a-number": score},
				"pointer":    &score,
				"text":       settingsTextFloat(score),
				"text-key": map[settingsTextFloat]float64{
					settingsTextFloat(score): score,
				},
			},
			wire: `{"array":[9223372036854776000e0],"bytes":"AP8=","empty":[],"matrix":[{"7":9223372036854776000e0},{"7":9223372036854776000e0}],"nil":null,"number-key":{"":9223372036854776000e0,"7":9223372036854776000e0,"not-a-number":9223372036854776000e0},"pointer":9223372036854776000e0,"text":"caller-text","text-key":{"caller-text":9223372036854776000e0}}`,
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			server := jsonrpc2.NewClient(serverConn, serverConn)
			requests := make(chan json.RawMessage, 1)
			server.SetRequestHandler("user.settings.update", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
				requests <- params
				return json.RawMessage(`{"value":{}}`), nil
			})
			client.Start()
			server.Start()
			defer client.Stop()
			defer server.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			original, err := json.Marshal(test.expected)
			if err != nil {
				t.Fatal(err)
			}
			_, err = NewInternalServerRPC(client).User.Settings().Update(ctx, &UserSettingsUpdateRequest{
				Key:           "voice",
				Value:         map[string]any{"enabled": false},
				IfFieldsMatch: test.expected,
			})
			if err != nil {
				t.Fatal(err)
			}
			after, err := json.Marshal(test.expected)
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(after, original) {
				t.Fatalf("caller conditions changed: got %s, want %s", after, original)
			}
			select {
			case raw := <-requests:
				var fields map[string]json.RawMessage
				if err := json.Unmarshal(raw, &fields); err != nil {
					t.Fatal(err)
				}
				got, present := fields["ifFieldsMatch"]
				if present != (test.wire != "") || string(got) != test.wire {
					t.Fatalf("ifFieldsMatch = %s (present %v), want %s; wire params: %s", got, present, test.wire, raw)
				}
			case <-ctx.Done():
				t.Fatal(ctx.Err())
			}
		})
	}
}

func TestUserSettingsUpdateRejectsCyclicValues(t *testing.T) {
	object := map[string]any{}
	object["self"] = object
	array := []any{nil}
	array[0] = array
	var pointer any
	pointer = &pointer
	for name, value := range map[string]any{"map": object, "slice": array, "pointer": pointer} {
		t.Run(name, func(t *testing.T) {
			_, err := json.Marshal(UserSettingsUpdateRequest{Key: "extensions", Value: value})
			var unsupported *json.UnsupportedValueError
			if !errors.As(err, &unsupported) {
				t.Fatalf("cyclic value error = %v, want json.UnsupportedValueError", err)
			}
		})
	}
}

func TestSettingsGeneratedRequestsRejectCycles(t *testing.T) {
	const childCase = "COPILOT_GO_SETTINGS_CYCLE_TEST"
	if name := os.Getenv(childCase); name != "" {
		debug.SetMaxStack(4 << 20)
		request := &UserSettingsUpdateRequest{Key: "extensions"}
		switch name {
		case "self pointer":
			request.Value = request
		case "nested generated types":
			request.Value = &UserSettingsNormalizeRequest{Settings: request}
		default:
			t.Fatalf("unknown child case %q", name)
		}
		_, err := json.Marshal(request)
		var unsupported *json.UnsupportedValueError
		if !errors.As(err, &unsupported) {
			t.Fatalf("generated settings cycle error = %v, want json.UnsupportedValueError", err)
		}
		return
	}
	for _, name := range []string{"self pointer", "nested generated types"} {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 20*time.Second)
			defer cancel()
			command := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestSettingsGeneratedRequestsRejectCycles$")
			command.Env = append(os.Environ(), childCase+"="+name)
			output, err := command.CombinedOutput()
			if err != nil {
				if len(output) > 8192 {
					output = output[:8192]
				}
				t.Fatalf("isolated settings encoding failed: %v; context: %v\n%s", err, ctx.Err(), output)
			}
		})
	}
}

type settingsCustomGeneratedRequest struct {
	UserSettingsNormalizeRequest
	calls int
}

func (request *settingsCustomGeneratedRequest) MarshalJSON() ([]byte, error) {
	request.calls++
	return []byte(`true`), nil
}

func TestSettingsNestedGeneratedRequestsPreserveCallerEncoders(t *testing.T) {
	shared := &UserSettingsNormalizeRequest{Settings: float32(0.1)}
	custom := &settingsCustomGeneratedRequest{}
	custom.Settings = custom
	encoded, err := json.Marshal(UserSettingsUpdateRequest{
		Key: "extensions", Value: []any{shared, shared, custom},
	})
	if err != nil {
		t.Fatal(err)
	}
	const expected = `{"key":"extensions","value":[{"settings":0.1},{"settings":0.1},true]}`
	if string(encoded) != expected {
		t.Fatalf("nested settings encoding = %s, want %s", encoded, expected)
	}
	if custom.calls != 1 {
		t.Fatalf("caller marshaler calls = %d, want 1", custom.calls)
	}
	if shared.Settings != float32(0.1) || custom.Settings != custom {
		t.Fatal("the encoder changed the caller's settings")
	}
}

func TestUserSettingsUpdatePreservesStandardEncodingErrors(t *testing.T) {
	for _, test := range []struct {
		name   string
		value  any
		target any
	}{
		{"non-finite float", math.Inf(1), new(*json.UnsupportedValueError)},
		{"non-finite float32", float32(math.Inf(1)), new(*json.UnsupportedValueError)},
		{"unsupported type", make(chan int), new(*json.UnsupportedTypeError)},
	} {
		t.Run(test.name, func(t *testing.T) {
			_, err := json.Marshal(UserSettingsUpdateRequest{Key: "extensions", Value: test.value})
			if !errors.As(err, test.target) {
				t.Fatalf("encoding error = %v, want %T", err, test.target)
			}
			var actual *json.UnsupportedValueError
			if errors.As(err, &actual) {
				_, standardErr := json.Marshal(test.value)
				var expected *json.UnsupportedValueError
				if !errors.As(standardErr, &expected) || actual.Value.Type() != expected.Value.Type() || actual.Str != expected.Str {
					t.Fatalf("unsupported value = %#v, want the standard error %v", actual, standardErr)
				}
			}
		})
	}
}

func TestFeatureFlagConfigPatchValuePresenceOnWire(t *testing.T) {
	for _, test := range []struct {
		name        string
		params      *SessionFeatureFlagsUpdateRequest
		patch       string
		assignments string
	}{
		{
			name:   "values only",
			params: &SessionFeatureFlagsUpdateRequest{ConfigPatch: &FeatureFlagConfigPatch{Values: map[string]any{"debug": true}}},
			patch:  `{"undefinedKeys":[],"values":{"debug":true}}`,
		},
		{
			name:   "empty removal list",
			params: &SessionFeatureFlagsUpdateRequest{ConfigPatch: &FeatureFlagConfigPatch{UndefinedKeys: []string{}, Values: map[string]any{}}},
			patch:  `{"undefinedKeys":[],"values":{}}`,
		},
		{
			name:   "populated removal list",
			params: &SessionFeatureFlagsUpdateRequest{ConfigPatch: &FeatureFlagConfigPatch{UndefinedKeys: []string{"obsolete"}, Values: map[string]any{"debug": false}}},
			patch:  `{"undefinedKeys":["obsolete"],"values":{"debug":false}}`,
		},
		{
			name:   "null values",
			params: &SessionFeatureFlagsUpdateRequest{ConfigPatch: &FeatureFlagConfigPatch{}},
			patch:  `{"undefinedKeys":[],"values":null}`,
		},
		{name: "omitted patch", params: &SessionFeatureFlagsUpdateRequest{}},
		{
			name:        "clear assignments",
			params:      &SessionFeatureFlagsUpdateRequest{Assignments: &FeatureFlagAssignmentsUpdate{}},
			assignments: `{"response":null}`,
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			server := jsonrpc2.NewClient(serverConn, serverConn)
			requests := make(chan json.RawMessage, 1)
			server.SetRequestHandler("session.featureFlags.update", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
				requests <- params
				return json.RawMessage(`{}`), nil
			})
			client.Start()
			server.Start()
			defer client.Stop()
			defer server.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			var originalKeys []string
			var originalValues []byte
			if patch := test.params.ConfigPatch; patch != nil {
				originalKeys = slices.Clone(patch.UndefinedKeys)
				var err error
				originalValues, err = json.Marshal(patch.Values)
				if err != nil {
					t.Fatal(err)
				}
			}
			if _, err := NewSessionRPC(client, "feature-session").FeatureFlags.Update(ctx, test.params); err != nil {
				t.Fatal(err)
			}
			select {
			case raw := <-requests:
				var fields map[string]json.RawMessage
				if err := json.Unmarshal(raw, &fields); err != nil {
					t.Fatal(err)
				}
				if string(fields["sessionId"]) != `"feature-session"` {
					t.Fatalf("wrong session: %s", raw)
				}
				for key, expected := range map[string]string{"configPatch": test.patch, "assignments": test.assignments} {
					got, present := fields[key]
					if present != (expected != "") || string(got) != expected {
						t.Errorf("%s = %s (present %v), want %s; wire params: %s", key, got, present, expected, raw)
					}
				}
			case <-ctx.Done():
				t.Fatal(ctx.Err())
			}
			if patch := test.params.ConfigPatch; patch != nil {
				if (patch.UndefinedKeys == nil) != (originalKeys == nil) || len(patch.UndefinedKeys) != len(originalKeys) {
					t.Errorf("caller removal list changed: got %#v, want %#v", patch.UndefinedKeys, originalKeys)
				}
				for i, key := range originalKeys {
					if i < len(patch.UndefinedKeys) && patch.UndefinedKeys[i] != key {
						t.Errorf("caller removal key %d changed: got %q, want %q", i, patch.UndefinedKeys[i], key)
					}
				}
				values, err := json.Marshal(patch.Values)
				if err != nil {
					t.Fatal(err)
				}
				if !bytes.Equal(values, originalValues) {
					t.Errorf("caller values changed: got %s, want %s", values, originalValues)
				}
			}
		})
	}
}

func TestSubagentSettingsValuePresenceOnWire(t *testing.T) {
	for _, test := range []struct {
		name     string
		settings SubagentSettings
	}{
		{name: "clear"},
		{name: "empty override", settings: &SubagentSettingsConfig{}},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			server := jsonrpc2.NewClient(serverConn, serverConn)
			requests := make(chan json.RawMessage, 1)
			server.SetRequestHandler("session.tools.updateSubagentSettings", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
				requests <- params
				return json.RawMessage(`{}`), nil
			})
			client.Start()
			server.Start()
			defer client.Stop()
			defer server.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			_, err := NewSessionRPC(client, "settings-session").Tools.UpdateSubagentSettings(ctx, &UpdateSubagentSettingsRequest{
				Subagents: test.settings,
			})
			if err != nil {
				t.Fatal(err)
			}
			select {
			case raw := <-requests:
				var fields map[string]json.RawMessage
				if err := json.Unmarshal(raw, &fields); err != nil {
					t.Fatal(err)
				}
				if string(fields["sessionId"]) != `"settings-session"` {
					t.Fatalf("wrong session: %s", raw)
				}
				value, present := fields["subagents"]
				expected, err := json.Marshal(test.settings)
				if err != nil {
					t.Fatal(err)
				}
				if !present || string(value) != string(expected) {
					t.Fatalf("subagents = %s (present %v), want %s; wire params: %s", value, present, expected, raw)
				}
			case <-ctx.Done():
				t.Fatal(ctx.Err())
			}
		})
	}
}

func TestSettingsValuePresenceOnWire(t *testing.T) {
	for _, method := range []string{"user.settings.applyTopKey", "repo.settings.writeKey", "user.settings.edit"} {
		for _, test := range []struct {
			name         string
			value        any
			present      bool
			delete       bool
			settingsWire string
		}{
			{name: "omitted"},
			{name: "explicit null", value: json.RawMessage("null"), present: true},
			{name: "value", value: "chosen", present: true},
			{name: "comparison float64", value: float64(9.223372036854776e18), present: true, settingsWire: "9223372036854776000e0"},
			{name: "standard float32 fraction", value: float32(0.1), present: true},
			{name: "comparison float32 integer", value: float32(9.223372e18), present: true, settingsWire: "9223372036854776000e0"},
			{name: "exact integer", value: json.Number("9007199254740993"), present: true},
			{name: "custom float32 encoder", value: settingsTextFloat32(0.1), present: true},
			{name: "delete", delete: true},
		} {
			t.Run(method+"/"+test.name, func(t *testing.T) {
				clientConn, serverConn := net.Pipe()
				defer clientConn.Close()
				defer serverConn.Close()
				client := jsonrpc2.NewClient(clientConn, clientConn)
				server := jsonrpc2.NewClient(serverConn, serverConn)
				requests := make(chan json.RawMessage, 1)
				server.SetRequestHandler(method, func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
					requests <- params
					return json.RawMessage(`{}`), nil
				})
				client.Start()
				server.Start()
				defer client.Stop()
				defer server.Stop()
				ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
				defer cancel()
				api := NewInternalServerRPC(client)
				original, err := json.Marshal(test.value)
				if err != nil {
					t.Fatal(err)
				}
				switch method {
				case "user.settings.applyTopKey":
					var value *any
					if test.present {
						value = &test.value
					}
					_, err = api.User.Settings().ApplyTopKey(ctx, &UserSettingsApplyTopKeyRequest{
						CanonicalPath: []string{"model"},
						TopKey:        "model",
						Value:         value,
						Delete:        &test.delete,
					})
				case "repo.settings.writeKey":
					_, err = api.Repo.Settings().WriteKey(ctx, &RepoSettingsWriteKeyRequest{
						RepoRoot: t.TempDir(),
						Scope:    "repo",
						Key:      "model",
						Value:    test.value,
						Delete:   &test.delete,
					})
				case "user.settings.edit":
					operation := SettingsEditOperationSet
					if test.delete {
						operation = SettingsEditOperationRemove
					}
					_, err = api.User.Settings().Edit(ctx, &UserSettingsEditRequest{
						CanonicalPath: []string{"model"},
						Operation:     operation,
						Value:         test.value,
					})
				}
				if err != nil {
					t.Fatal(err)
				}
				after, err := json.Marshal(test.value)
				if err != nil {
					t.Fatal(err)
				}
				if !bytes.Equal(after, original) {
					t.Fatalf("caller value changed: got %s, want %s", after, original)
				}
				select {
				case raw := <-requests:
					var fields map[string]json.RawMessage
					if err := json.Unmarshal(raw, &fields); err != nil {
						t.Fatal(err)
					}
					value, present := fields["value"]
					wantPresent := test.present || (method == "user.settings.edit" && !test.delete)
					if present != wantPresent {
						t.Fatalf("value presence = %v, want %v; wire params: %s", present, wantPresent, raw)
					}
					if present {
						expected, err := json.Marshal(test.value)
						if err != nil {
							t.Fatal(err)
						}
						if method != "repo.settings.writeKey" && test.settingsWire != "" {
							expected = []byte(test.settingsWire)
						}
						if string(value) != string(expected) {
							t.Fatalf("value = %s, want %s; wire params: %s", value, expected, raw)
						}
					}
				case <-ctx.Done():
					t.Fatal(ctx.Err())
				}
			})
		}
	}
}

func settingsRequestCall[P, R any](method func(context.Context, *P) (*R, error), params *P) func(context.Context) error {
	return func(ctx context.Context) error {
		_, err := method(ctx, params)
		return err
	}
}

func TestSettingsInputRoutesPreserveNumericComparisonsOnWire(t *testing.T) {
	t.Parallel()
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()
	client := jsonrpc2.NewClient(clientConn, clientConn)
	server := jsonrpc2.NewClient(serverConn, serverConn)
	api := NewInternalServerRPC(client)
	settings := api.User.Settings()
	value := map[string]any{
		"exact":  json.Number("9007199254740993"),
		"nested": []any{float32(0.1), float32(9.223372e18), float64(9.223372036854776e18)},
	}
	var pointedValue any = value
	const expected = `{"exact":9007199254740993,"nested":[0.1,9223372036854776000e0,9223372036854776000e0]}`
	tests := []struct {
		method string
		call   func(context.Context) error
		fields []string
		want   string
	}{
		{
			method: "settings.redactValueForDisplay",
			call: settingsRequestCall(api.Settings.RedactValueForDisplay, &SettingsRedactValueForDisplayRequest{
				BasePath: []string{"extensions"}, Value: value,
			}),
			fields: []string{"value"},
		},
		{
			method: "user.settings.applyTopKey",
			call: settingsRequestCall(settings.ApplyTopKey, &UserSettingsApplyTopKeyRequest{
				CanonicalPath: []string{"extensions"}, TopKey: "extensions", Value: &pointedValue,
			}),
			fields: []string{"value"},
		},
		{
			method: "user.settings.buildRuntime",
			call: settingsRequestCall(settings.BuildRuntime, &UserSettingsBuildRuntimeRequest{
				Environment: map[string]string{}, Overrides: value, Platform: "linux", WorkingDirectory: ".",
			}),
			fields: []string{"overrides"},
		},
		{
			method: "user.settings.diagnoseProblems",
			call: settingsRequestCall(settings.DiagnoseProblems, &UserSettingsProblemsRequest{
				Sources: []UserSettingsProblemSource{{Config: value, FilePath: "settings.json", Kind: "user", Label: "User"}},
			}),
			fields: []string{"sources"},
			want:   `[{"config":` + expected + `,"filePath":"settings.json","kind":"user","label":"User"}]`,
		},
		{
			method: "user.settings.loadAndMergeRepo",
			call: settingsRequestCall(settings.LoadAndMergeRepo, &UserSettingsLoadAndMergeRepoRequest{
				RepoRoot: ".", UserSettings: value,
			}),
			fields: []string{"userSettings"},
		},
		{
			method: "user.settings.mergeManagedAndUser",
			call: settingsRequestCall(settings.MergeManagedAndUser, &UserSettingsMergeManagedAndUserParams{
				Managed: value, User: value,
			}),
			fields: []string{"managed", "user"},
		},
		{
			method: "user.settings.mergeRepoValue",
			call: settingsRequestCall(settings.MergeRepoValue, &UserSettingsMergeRepoValueParams{
				RepoSettings: value, UserSettings: value,
			}),
			fields: []string{"repoSettings", "userSettings"},
		},
		{
			method: "user.settings.normalize",
			call: settingsRequestCall(settings.Normalize, &UserSettingsNormalizeRequest{
				Settings: value,
			}),
			fields: []string{"settings"},
		},
		{
			method: "user.settings.set",
			call: settingsRequestCall(NewServerRPC(client).User.Settings().Set, &UserSettingsSetRequest{
				Settings: value,
			}),
			fields: []string{"settings"},
		},
		{
			method: "user.settings.update",
			call: settingsRequestCall(settings.Update, &UserSettingsUpdateRequest{
				Key: "extensions", Value: value, IfFieldsMatch: value,
			}),
			fields: []string{"value", "ifFieldsMatch"},
		},
		{
			method: "user.settings.validate",
			call: settingsRequestCall(settings.Validate, &UserSettingsValidateRequest{
				TopKey: "extensions", Value: value,
			}),
			fields: []string{"value"},
		},
		{
			method: "user.settings.writeAll",
			call: settingsRequestCall(settings.WriteAll, &UserSettingsWriteAllRequest{
				Settings: value,
			}),
			fields: []string{"settings"},
		},
		{
			method: "user.settings.writeKey",
			call: settingsRequestCall(settings.WriteKey, &UserSettingsWriteKeyRequest{
				Key: "extensions", Value: &pointedValue,
			}),
			fields: []string{"value"},
		},
	}
	type wireRequest struct {
		method string
		params json.RawMessage
	}
	requests := make(chan wireRequest, 1)
	for _, test := range tests {
		server.SetRequestHandler(test.method, func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
			requests <- wireRequest{method: test.method, params: params}
			return json.RawMessage(`{}`), nil
		})
	}
	client.Start()
	server.Start()
	defer client.Stop()
	defer server.Stop()
	original, err := json.Marshal(value)
	if err != nil {
		t.Fatal(err)
	}
	for _, test := range tests {
		t.Run(test.method, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
			defer cancel()
			if err := test.call(ctx); err != nil {
				t.Fatal(err)
			}
			select {
			case request := <-requests:
				if request.method != test.method {
					t.Fatalf("method = %q, want %q", request.method, test.method)
				}
				raw := request.params
				var fields map[string]json.RawMessage
				if err := json.Unmarshal(raw, &fields); err != nil {
					t.Fatal(err)
				}
				want := expected
				if test.want != "" {
					want = test.want
				}
				for _, field := range test.fields {
					if string(fields[field]) != want {
						t.Errorf("%s = %s, want %s; wire params: %s", field, fields[field], want, raw)
					}
				}
			case <-ctx.Done():
				t.Fatal(ctx.Err())
			}
			after, err := json.Marshal(value)
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(after, original) {
				t.Fatalf("caller settings changed: got %s, want %s", after, original)
			}
		})
	}
}
