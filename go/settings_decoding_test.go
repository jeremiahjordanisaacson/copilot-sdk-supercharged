// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestSettingsLoadForEditUsesDefaultAndScopedConfigDirectories(t *testing.T) {
	t.Parallel()
	configDir, scopedDir := t.TempDir(), t.TempDir()
	for directory, contents := range map[string]string{
		configDir: `{"theme":"dim"}`,
		scopedDir: `{"theme":"light"}`,
	} {
		if err := os.WriteFile(filepath.Join(directory, "settings.json"), []byte(contents), 0600); err != nil {
			t.Fatal(err)
		}
	}
	client := NewClient(&ClientOptions{
		Mode: ModeCodingAgent, Runtime: SpawnedStdioConnection{Path: findCLIPathForTest(t), BaseDirectory: configDir},
	})
	t.Cleanup(func() { client.ForceStop() })
	if err := client.Start(t.Context()); err != nil {
		t.Fatal(err)
	}
	for _, test := range []struct {
		name   string
		params *rpc.UserSettingsLoadForConfigDirRequest
		theme  string
	}{
		{"default", &rpc.UserSettingsLoadForConfigDirRequest{}, "dim"},
		{"nil", nil, "dim"},
		{"scoped", &rpc.UserSettingsLoadForConfigDirRequest{ConfigDir: &scopedDir}, "light"},
	} {
		t.Run(test.name, func(t *testing.T) {
			result, err := client.internalRPC.User.Settings().LoadForEdit(t.Context(), test.params)
			if err != nil {
				t.Fatal(err)
			}
			settings, ok := result.Settings.(map[string]any)
			if result.Status != "ok" || !ok || settings["theme"] != test.theme {
				t.Fatalf("loadForEdit = %#v, want status ok and theme %q", result, test.theme)
			}
		})
	}
}

func TestSettingsReadValuesSupportExactConditionalUpdate(t *testing.T) {
	t.Parallel()
	client := NewClient(&ClientOptions{
		Mode: ModeCodingAgent, Runtime: SpawnedStdioConnection{Path: findCLIPathForTest(t), BaseDirectory: t.TempDir()},
	})
	t.Cleanup(func() { client.ForceStop() })
	if err := client.Start(t.Context()); err != nil {
		t.Fatal(err)
	}
	settings := client.internalRPC.User.Settings()
	for _, source := range []string{"loadForEdit", "update conflict result", "caller supplied values", "caller typed maps", "caller typed slices", "caller structs", "normalized caller values", "merged repository values", "merged managed values", "runtime overrides", "loaded repository values", "redacted caller values"} {
		for _, test := range []struct {
			name     string
			original string
			current  string
		}{
			{"unchanged", "9007199254740993", "9007199254740993"},
			{"adjacent concurrent value", "9007199254740993", "9007199254740992"},
			{"unchanged floating point", "9.223372036854776e18", "9.223372036854776e18"},
			{"floating point changed to integer", "9.223372036854776e18", "9223372036854776000"},
		} {
			t.Run(source+"/"+test.name, func(t *testing.T) {
				configDir := t.TempDir()
				path := filepath.Join(configDir, "settings.json")
				shape := func(number string) string {
					switch source {
					case "caller typed maps", "caller structs":
						return fmt.Sprintf(`{"score":%s}`, number)
					case "caller typed slices":
						return fmt.Sprintf(`[%s]`, number)
					default:
						return number
					}
				}
				original := fmt.Sprintf(`{"extensions":{"revision":%s,"enabled":false}}`, shape(test.original))
				if err := os.WriteFile(path, []byte(original), 0600); err != nil {
					t.Fatal(err)
				}
				var before map[string]any
				switch source {
				case "loadForEdit":
					result, err := settings.LoadForEdit(t.Context(), &rpc.UserSettingsLoadForConfigDirRequest{ConfigDir: &configDir})
					if err != nil {
						t.Fatal(err)
					}
					if result.Status != "ok" {
						t.Fatalf("loadForEdit status = %q, want ok", result.Status)
					}
					before = result.Settings.(map[string]any)["extensions"].(map[string]any)
				case "update conflict result":
					result, err := settings.Update(t.Context(), &rpc.UserSettingsUpdateRequest{
						ConfigDir: &configDir, Key: "extensions", Value: map[string]any{"enabled": false},
						IfFieldsMatch: map[string]any{"revision": json.Number("9007199254740991")},
					})
					if err != nil {
						t.Fatal(err)
					}
					if result.Applied == nil || *result.Applied {
						t.Fatalf("the stale condition must fail: %#v", result)
					}
					before = result.Value.(map[string]any)
				case "caller supplied values", "caller typed maps", "caller typed slices", "caller structs", "normalized caller values", "merged repository values", "merged managed values", "runtime overrides", "loaded repository values", "redacted caller values":
					var revision any = json.Number(test.original)
					if strings.ContainsAny(test.original, ".eE") {
						number, err := strconv.ParseFloat(test.original, 64)
						if err != nil {
							t.Fatal(err)
						}
						revision = number
					}
					switch source {
					case "caller typed maps":
						if number, ok := revision.(float64); ok {
							revision = map[string]float64{"score": number}
						} else {
							revision = map[string]json.Number{"score": json.Number(test.original)}
						}
					case "caller typed slices":
						if number, ok := revision.(float64); ok {
							revision = []float64{number}
						} else {
							revision = []json.Number{json.Number(test.original)}
						}
					case "caller structs":
						if number, ok := revision.(float64); ok {
							revision = struct {
								Score float64 `json:"score"`
							}{number}
						} else {
							revision = struct {
								Score json.Number `json:"score"`
							}{json.Number(test.original)}
						}
					}
					before = map[string]any{"revision": revision}
				}
				if source == "normalized caller values" {
					normalized, err := settings.Normalize(t.Context(), &rpc.UserSettingsNormalizeRequest{
						Settings: map[string]any{"extensions": before},
					})
					if err != nil {
						t.Fatal(err)
					}
					before = normalized.Settings.(map[string]any)["extensions"].(map[string]any)
				}
				document := map[string]any{"extensions": before}
				callerDocument, err := json.Marshal(document)
				if err != nil {
					t.Fatal(err)
				}
				switch source {
				case "merged repository values":
					result, err := settings.MergeRepoValue(t.Context(), &rpc.UserSettingsMergeRepoValueParams{
						UserSettings: document, RepoSettings: map[string]any{},
					})
					if err != nil {
						t.Fatal(err)
					}
					before = result.SettingsJSON.(map[string]any)["extensions"].(map[string]any)
				case "merged managed values":
					result, err := settings.MergeManagedAndUser(t.Context(), &rpc.UserSettingsMergeManagedAndUserParams{
						User: document,
					})
					if err != nil {
						t.Fatal(err)
					}
					before = result.SettingsJSON.(map[string]any)["extensions"].(map[string]any)
				case "runtime overrides":
					result, err := settings.BuildRuntime(t.Context(), &rpc.UserSettingsBuildRuntimeRequest{
						Environment: map[string]string{}, Platform: "linux", WorkingDirectory: t.TempDir(), Overrides: document,
					})
					if err != nil {
						t.Fatal(err)
					}
					before = (*result)["extensions"].(map[string]any)
				case "loaded repository values":
					result, err := settings.LoadAndMergeRepo(t.Context(), &rpc.UserSettingsLoadAndMergeRepoRequest{
						RepoRoot: t.TempDir(), UserSettings: document,
					})
					if err != nil {
						t.Fatal(err)
					}
					before = result.Settings["extensions"].(map[string]any)
				case "redacted caller values":
					result, err := client.internalRPC.Settings.RedactValueForDisplay(t.Context(), &rpc.SettingsRedactValueForDisplayRequest{
						BasePath: []string{"extensions"}, Value: before,
					})
					if err != nil {
						t.Fatal(err)
					}
					before = result.Value.(map[string]any)
				}
				afterTransformation, err := json.Marshal(document)
				if err != nil {
					t.Fatal(err)
				}
				if string(afterTransformation) != string(callerDocument) {
					t.Fatalf("caller settings changed: got %s, want %s", afterTransformation, callerDocument)
				}
				originalConditions, err := json.Marshal(before)
				if err != nil {
					t.Fatal(err)
				}
				wantUnchanged := original
				concurrentChange := test.original != test.current
				if concurrentChange {
					wantUnchanged = fmt.Sprintf(`{"extensions":{"revision":%s,"enabled":false}}`, shape(test.current))
					if err := os.WriteFile(path, []byte(wantUnchanged), 0600); err != nil {
						t.Fatal(err)
					}
				}
				result, err := settings.Update(t.Context(), &rpc.UserSettingsUpdateRequest{
					ConfigDir: &configDir, Key: "extensions", IfFieldsMatch: before,
					Value: map[string]any{"enabled": true},
				})
				if err != nil {
					t.Fatal(err)
				}
				conditionsAfter, err := json.Marshal(before)
				if err != nil {
					t.Fatal(err)
				}
				if string(conditionsAfter) != string(originalConditions) {
					t.Fatalf("caller conditions changed: got %s, want %s", conditionsAfter, originalConditions)
				}
				if result.Applied == nil {
					t.Fatal("the conditional update omitted applied")
				}
				if *result.Applied != !concurrentChange {
					t.Errorf("applied = %v, want %v; read values: %#v", *result.Applied, !concurrentChange, before)
				}
				stored, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				var storedDocument struct {
					Extensions struct {
						Enabled bool `json:"enabled"`
					} `json:"extensions"`
				}
				if err := json.Unmarshal(stored, &storedDocument); err != nil {
					t.Fatal(err)
				}
				if storedDocument.Extensions.Enabled != !concurrentChange {
					t.Fatalf("stored document = %s; want enabled %v", stored, !concurrentChange)
				}
				if concurrentChange && string(stored) != wantUnchanged {
					t.Fatalf("the rejected update changed the file:\ngot: %s\nwant: %s", stored, wantUnchanged)
				}
			})
		}
	}
}

func TestSettingsFloat32ConditionsPreserveJSONPrecision(t *testing.T) {
	t.Parallel()
	client := NewClient(&ClientOptions{
		Mode: ModeCodingAgent, Runtime: SpawnedStdioConnection{Path: findCLIPathForTest(t), BaseDirectory: t.TempDir()},
	})
	t.Cleanup(func() { client.ForceStop() })
	if err := client.Start(t.Context()); err != nil {
		t.Fatal(err)
	}
	for _, test := range []struct {
		number float32
		source string
		stored string
	}{
		{0.1, "0.1", "0.1"},
		{9.223372e18, "9.223372036854776e18", "9223372036854776000e0"},
	} {
		number := test.number
		for _, shape := range []string{"scalar", "typed map", "typed slice", "struct", "nested pointer"} {
			t.Run(fmt.Sprintf("%g/%s", number, shape), func(t *testing.T) {
				var revision any = number
				switch shape {
				case "typed map":
					revision = map[string]float32{"score": number}
				case "typed slice":
					revision = []float32{number}
				case "struct":
					revision = struct {
						Score float32 `json:"score"`
					}{number}
				case "nested pointer":
					revision = []any{map[string]any{"score": &number}}
				}
				encoded, err := json.Marshal(revision)
				if err != nil {
					t.Fatal(err)
				}
				before := string(encoded)
				standard, err := json.Marshal(number)
				if err != nil {
					t.Fatal(err)
				}
				source := strings.ReplaceAll(before, string(standard), test.source)
				expected := strings.ReplaceAll(before, string(standard), test.stored)
				t.Logf("standard JSON = %s; runtime JSON = %s", before, source)
				configDir := t.TempDir()
				path := filepath.Join(configDir, "settings.json")
				if err := os.WriteFile(path, []byte(`{"extensions":{"revision":`+source+`,"enabled":false}}`), 0600); err != nil {
					t.Fatal(err)
				}
				result, err := client.internalRPC.User.Settings().Update(t.Context(), &rpc.UserSettingsUpdateRequest{
					ConfigDir: &configDir, Key: "extensions",
					IfFieldsMatch: map[string]any{"revision": revision},
					Value:         map[string]any{"enabled": true, "writtenRevision": revision},
				})
				if err != nil {
					t.Fatal(err)
				}
				if result.Applied == nil || !*result.Applied {
					t.Fatalf("the unchanged float32 condition must match: %#v; runtime value %s", result, source)
				}
				after, err := json.Marshal(revision)
				if err != nil {
					t.Fatal(err)
				}
				if string(after) != before {
					t.Fatalf("caller condition changed: got %s, want %s", after, before)
				}
				stored, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				var document struct {
					Extensions struct {
						Enabled         bool            `json:"enabled"`
						WrittenRevision json.RawMessage `json:"writtenRevision"`
					} `json:"extensions"`
				}
				if err := json.Unmarshal(stored, &document); err != nil {
					t.Fatal(err)
				}
				if !document.Extensions.Enabled {
					t.Fatalf("the matched update did not persist: %s", stored)
				}
				var written bytes.Buffer
				if err := json.Compact(&written, document.Extensions.WrittenRevision); err != nil {
					t.Fatal(err)
				}
				if written.String() != expected {
					t.Fatalf("the float32 write changed its JSON value: got %s, want %s", written.String(), expected)
				}
			})
		}
	}
}

func TestSettingsFloat32ConditionsCompareRepresentedIntegers(t *testing.T) {
	t.Parallel()
	client := NewClient(&ClientOptions{
		Mode: ModeCodingAgent, Runtime: SpawnedStdioConnection{Path: findCLIPathForTest(t), BaseDirectory: t.TempDir()},
	})
	t.Cleanup(func() { client.ForceStop() })
	if err := client.Start(t.Context()); err != nil {
		t.Fatal(err)
	}
	for _, number := range []float32{0.1, 9.223372e18} {
		t.Run(fmt.Sprint(number), func(t *testing.T) {
			configDir := t.TempDir()
			path := filepath.Join(configDir, "settings.json")
			promoted := strconv.FormatFloat(float64(number), 'e', -1, 64)
			original := []byte(`{"extensions":{"revision":` + promoted + `,"enabled":false}}`)
			if err := os.WriteFile(path, original, 0600); err != nil {
				t.Fatal(err)
			}
			settings := client.internalRPC.User.Settings()
			conditions := map[string]any{"revision": number}
			before, err := json.Marshal(conditions)
			if err != nil {
				t.Fatal(err)
			}
			result, err := settings.Update(t.Context(), &rpc.UserSettingsUpdateRequest{
				ConfigDir: &configDir, Key: "extensions", IfFieldsMatch: conditions,
				Value: map[string]any{"enabled": true},
			})
			if err != nil {
				t.Fatal(err)
			}
			wantMatch := number == float32(9.223372e18)
			if result.Applied == nil {
				t.Fatal("the conditional update omitted applied")
			}
			if *result.Applied != wantMatch {
				t.Fatalf("applied = %v, want %v; float32 %v represents %.0f; runtime value %s", *result.Applied, wantMatch, number, float64(number), promoted)
			}
			t.Logf("applied = %v; float32 %v represents %.20g; runtime value %s", *result.Applied, number, float64(number), promoted)
			after, err := json.Marshal(conditions)
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(after, before) || conditions["revision"] != number {
				t.Fatalf("caller conditions changed: got %s, want %s", after, before)
			}
			unchanged, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}
			if !wantMatch && !bytes.Equal(unchanged, original) {
				t.Fatalf("the rejected float32 condition changed the file: got %s, want %s", unchanged, original)
			}
			if err := os.WriteFile(path, original, 0600); err != nil {
				t.Fatal(err)
			}
			result, err = settings.Update(t.Context(), &rpc.UserSettingsUpdateRequest{
				ConfigDir: &configDir, Key: "extensions", IfFieldsMatch: map[string]any{"revision": float64(number)},
				Value: map[string]any{"enabled": true},
			})
			if err != nil {
				t.Fatal(err)
			}
			if result.Applied == nil || !*result.Applied {
				t.Fatalf("the explicit float64 condition must match: %#v; promoted value %s", result, promoted)
			}
			stored, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}
			var document struct {
				Extensions struct {
					Enabled bool `json:"enabled"`
				} `json:"extensions"`
			}
			if err := json.Unmarshal(stored, &document); err != nil {
				t.Fatal(err)
			}
			if !document.Extensions.Enabled {
				t.Fatalf("the explicit float64 update did not persist: %s", stored)
			}
			if wantMatch {
				distinct := []byte(`{"extensions":` + string(before) + `}`)
				if err := os.WriteFile(path, distinct, 0600); err != nil {
					t.Fatal(err)
				}
				result, err = settings.Update(t.Context(), &rpc.UserSettingsUpdateRequest{
					ConfigDir: &configDir, Key: "extensions", IfFieldsMatch: conditions,
					Value: map[string]any{"enabled": true},
				})
				if err != nil {
					t.Fatal(err)
				}
				if result.Applied == nil || *result.Applied {
					t.Fatalf("the float32 condition must reject the distinct rounded integer: %#v; runtime value %s", result, distinct)
				}
				unchanged, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				if !bytes.Equal(unchanged, distinct) {
					t.Fatalf("the rejected condition changed the file: got %s, want %s", unchanged, distinct)
				}
			}
		})
	}
}

type oneShotSettingsJSON struct {
	calls int
	value string
}

func (value *oneShotSettingsJSON) MarshalJSON() ([]byte, error) {
	value.calls++
	if value.calls != 1 {
		return nil, fmt.Errorf("settings marshaler called %d times", value.calls)
	}
	return []byte(value.value), nil
}

func (value *oneShotSettingsJSON) MarshalText() ([]byte, error) {
	return value.MarshalJSON()
}

func TestSettingsUpdateCallsCustomMarshalersOnce(t *testing.T) {
	t.Parallel()
	client := NewClient(&ClientOptions{
		Mode: ModeCodingAgent, Runtime: SpawnedStdioConnection{Path: findCLIPathForTest(t), BaseDirectory: t.TempDir()},
	})
	t.Cleanup(func() { client.ForceStop() })
	if err := client.Start(t.Context()); err != nil {
		t.Fatal(err)
	}
	configDir := t.TempDir()
	path := filepath.Join(configDir, "settings.json")
	if err := os.WriteFile(path, []byte(`{"extensions":{"revision":9.223372036854776e18,"enabled":false}}`), 0600); err != nil {
		t.Fatal(err)
	}
	condition := &oneShotSettingsJSON{value: "9.223372036854776e18"}
	value := &oneShotSettingsJSON{value: "true"}
	key := &oneShotSettingsJSON{value: "score"}
	nested := []struct {
		Value oneShotSettingsJSON `json:"value"`
	}{{Value: oneShotSettingsJSON{value: "true"}}}
	result, err := client.internalRPC.User.Settings().Update(t.Context(), &rpc.UserSettingsUpdateRequest{
		ConfigDir: &configDir, Key: "extensions",
		Value: map[string]any{
			"enabled": true, "custom": value, "nested": nested,
			"keyed": map[*oneShotSettingsJSON]float64{key: 9.223372036854776e18},
		},
		IfFieldsMatch: map[string]any{"revision": condition},
	})
	if err != nil {
		t.Fatal(err)
	}
	if result.Applied == nil || !*result.Applied {
		t.Fatalf("the unchanged custom condition must match: %#v", result)
	}
	if condition.calls != 1 || value.calls != 1 || nested[0].Value.calls != 1 || key.calls != 1 {
		t.Fatalf("custom marshaler calls = (condition %d, value %d, nested %d, key %d), want (1, 1, 1, 1)", condition.calls, value.calls, nested[0].Value.calls, key.calls)
	}
	stored, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var document struct {
		Extensions struct {
			Enabled bool               `json:"enabled"`
			Custom  bool               `json:"custom"`
			Keyed   map[string]float64 `json:"keyed"`
			Nested  []struct {
				Value bool `json:"value"`
			} `json:"nested"`
		} `json:"extensions"`
	}
	if err := json.Unmarshal(stored, &document); err != nil {
		t.Fatal(err)
	}
	if document.Extensions.Keyed["score"] != 9.223372036854776e18 {
		t.Fatalf("stored custom key = %#v, want the caller's floating-point score", document.Extensions.Keyed)
	}
	if !document.Extensions.Enabled || !document.Extensions.Custom || len(document.Extensions.Nested) != 1 || !document.Extensions.Nested[0].Value {
		t.Fatalf("custom update did not persist: %s", stored)
	}
}
