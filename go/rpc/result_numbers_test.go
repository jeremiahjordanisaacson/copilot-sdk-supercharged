// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"reflect"
	"testing"

	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestScopedSettingsReadPreservesConditionalValues(t *testing.T) {
	var result rpc.UserSettingsLoadForConfigDirResult
	if err := json.Unmarshal([]byte(`{"extensions":{"revision":9007199254740993,"nested":[null,9007199254740995]}}`), &result); err != nil {
		t.Fatal(err)
	}
	request, err := json.Marshal(rpc.UserSettingsUpdateRequest{
		Key: "extensions", Value: map[string]any{"enabled": true},
		IfFieldsMatch: result["extensions"].(map[string]any),
	})
	if err != nil {
		t.Fatal(err)
	}
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(request, &fields); err != nil {
		t.Fatal(err)
	}
	const expected = `{"nested":[null,9007199254740995],"revision":9007199254740993}`
	if string(fields["ifFieldsMatch"]) != expected {
		t.Fatalf("the read changed the conditional values:\ngot: %s\nwant: %s", fields["ifFieldsMatch"], expected)
	}
}

func TestNormalizedSettingsPreserveWriteValues(t *testing.T) {
	const document = `{"voice":{"negative":-9007199254740993,"maximum":18446744073709551615,"nested":[9007199254740993,null,1.25]}}`
	var result rpc.UserSettingsNormalizeResult
	if err := json.Unmarshal([]byte(`{"settings":`+document+`}`), &result); err != nil {
		t.Fatal(err)
	}
	request, err := json.Marshal(rpc.UserSettingsWriteAllRequest(result))
	if err != nil {
		t.Fatal(err)
	}
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(request, &fields); err != nil {
		t.Fatal(err)
	}
	decoder := json.NewDecoder(bytes.NewReader([]byte(document)))
	decoder.UseNumber()
	var expected any
	if err := decoder.Decode(&expected); err != nil {
		t.Fatal(err)
	}
	want, err := json.Marshal(expected)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(fields["settings"], want) {
		t.Fatalf("normalization changed settings before the write:\ngot: %s\nwant: %s", fields["settings"], want)
	}
}

func TestPublicSettingsMetadataPreservesWriteValues(t *testing.T) {
	const source = `{"settings":{"statusLine":{"value":{"command":"echo before","padding":-9007199254740993,"small":7,"fraction":1.25},"default":{"padding":18446744073709551615},"isDefault":false}}}`
	var result rpc.UserSettingsGetResult
	if err := json.Unmarshal([]byte(source), &result); err != nil {
		t.Fatal(err)
	}
	metadata := result.Settings["statusLine"]
	request, err := json.Marshal(rpc.UserSettingsSetRequest{
		Settings: map[string]any{"statusLine": metadata.Value},
	})
	if err != nil {
		t.Fatal(err)
	}
	const expected = `{"settings":{"statusLine":{"command":"echo before","fraction":1.25,"padding":-9007199254740993,"small":7}}}`
	if string(request) != expected {
		t.Fatalf("public settings read changed the write:\ngot: %s\nwant: %s", request, expected)
	}
	defaults, err := json.Marshal(metadata.Default)
	if err != nil {
		t.Fatal(err)
	}
	if string(defaults) != `{"padding":18446744073709551615}` {
		t.Fatalf("public settings read changed its default: %s", defaults)
	}
	value := metadata.Value.(map[string]any)
	if value["small"] != float64(7) || value["fraction"] != float64(1.25) {
		t.Fatalf("public metadata changed compatible number types: %#v", value)
	}
}

func TestSettingsDocumentReadsPreserveWriteValues(t *testing.T) {
	var configDir rpc.UserSettingsLoadForConfigDirResult
	var edit rpc.UserSettingsLoadForEditResult
	var normalized rpc.UserSettingsNormalizeResult
	var updated rpc.UserSettingsUpdateResult
	var warning rpc.UserSettingsLoadWithWarningResult
	var repo rpc.UserSettingsLoadRepoResult
	var mergedRepo rpc.UserSettingsLoadAndMergeRepoResult
	var managed rpc.UserSettingsMergeManagedAndUserResult
	var mergedValue rpc.UserSettingsMergeRepoValueResult
	var runtime rpc.UserSettingsBuildRuntimeResult
	for _, test := range []struct {
		name     string
		field    string
		result   any
		settings func() map[string]any
	}{
		{"load for config dir", "", &configDir, func() map[string]any { return map[string]any(configDir) }},
		{"load for edit", "settings", &edit, func() map[string]any { return edit.Settings.(map[string]any) }},
		{"normalize", "settings", &normalized, func() map[string]any { return normalized.Settings.(map[string]any) }},
		{"update", "value", &updated, func() map[string]any { return updated.Value.(map[string]any) }},
		{"load with warning", "settings", &warning, func() map[string]any { return warning.Settings }},
		{"load repo", "settings", &repo, func() map[string]any { return repo.Settings }},
		{"load and merge repo", "settings", &mergedRepo, func() map[string]any { return mergedRepo.Settings }},
		{"merge managed and user", "settingsJson", &managed, func() map[string]any { return managed.SettingsJSON.(map[string]any) }},
		{"merge repo value", "settingsJson", &mergedValue, func() map[string]any { return mergedValue.SettingsJSON.(map[string]any) }},
		{"build runtime", "", &runtime, func() map[string]any { return map[string]any(runtime) }},
	} {
		t.Run(test.name, func(t *testing.T) {
			const document = `{"statusLine":{"padding":-9007199254740993},"custom":{"maximum":18446744073709551615,"precise":1.234567890123456789,"largeFloat":9.223372036854776e18,"compatibleLargeFloat":1.2345678901234567e20,"small":1,"fraction":0.1,"nested":[1e2,{"negative":-7,"unsafe":9007199254740993}]}}`
			input := document
			if test.field != "" {
				input = `{"` + test.field + `":` + document + `}`
			}
			if err := json.Unmarshal([]byte(input), test.result); err != nil {
				t.Fatal(err)
			}
			settings := test.settings()
			request, err := json.Marshal(rpc.UserSettingsSetRequest{Settings: settings})
			if err != nil {
				t.Fatal(err)
			}
			for _, number := range []string{"-9007199254740993", "18446744073709551615", "9007199254740993", "1.234567890123456789", "9.223372036854776e18"} {
				if !bytes.Contains(request, []byte(number)) {
					t.Errorf("settings read changed %s before the write: %s", number, request)
				}
			}
			custom := settings["custom"].(map[string]any)
			if custom["small"] != float64(1) || custom["fraction"] != float64(0.1) || custom["compatibleLargeFloat"] != float64(1.2345678901234567e20) {
				t.Fatalf("settings read changed compatible number types: %#v", custom)
			}
			nested := custom["nested"].([]any)
			if nested[0] != float64(100) || nested[1].(map[string]any)["negative"] != float64(-7) {
				t.Fatalf("settings read changed nested compatible number types: %#v", nested)
			}
		})
	}
}

func ExampleUserSettingsLoadForEditResult() {
	var snapshot rpc.UserSettingsLoadForEditResult
	if err := json.Unmarshal([]byte(`{"status":"ok","settings":{"extensions":{"revision":9007199254740993,"score":9.223372036854776e18}}}`), &snapshot); err != nil {
		panic(err)
	}
	before := snapshot.Settings.(map[string]any)["extensions"].(map[string]any)
	request, err := json.Marshal(rpc.UserSettingsUpdateRequest{
		Key: "extensions", Value: map[string]any{"enabled": true}, IfFieldsMatch: before,
	})
	if err != nil {
		panic(err)
	}
	fmt.Println(string(request))
	// Output: {"ifFieldsMatch":{"revision":9007199254740993,"score":9.223372036854776e18},"key":"extensions","value":{"enabled":true}}
}

func ExampleUserSettingsUpdateRequest() {
	conditions := map[string]any{
		"revision": json.Number("9007199254740993"),
		"nested":   []map[string]float64{{"score": 9.223372036854776e18}},
		"float32":  []float32{0.1, 9.223372e18},
		"promoted": float64(float32(9.223372e18)),
		"struct": struct {
			Score float64 `json:"score"`
		}{9.223372036854776e18},
	}
	request, err := json.Marshal(rpc.UserSettingsUpdateRequest{
		Key: "extensions", IfFieldsMatch: conditions,
		Value: struct {
			Score float64 `json:"score"`
		}{-9.223372036854775e18},
	})
	if err != nil {
		panic(err)
	}
	fmt.Println(string(request))
	fmt.Printf("Caller score type: %T\n", conditions["nested"].([]map[string]float64)[0]["score"])
	fmt.Printf("Caller float32: %g (represented %.0f)\n", conditions["float32"].([]float32)[1], float64(conditions["float32"].([]float32)[1]))
	// Output:
	// {"ifFieldsMatch":{"float32":[0.1,9223372036854776000e0],"nested":[{"score":9223372036854776000e0}],"promoted":9223372036854776000e0,"revision":9007199254740993,"struct":{"score":9223372036854776000e0}},"key":"extensions","value":{"score":-9223372036854775000e0}}
	// Caller score type: float64
	// Caller float32: 9.223372e+18 (represented 9223372036854775808)
}

func ExampleUserSettingsNormalizeRequest() {
	request, err := json.Marshal(rpc.UserSettingsNormalizeRequest{
		Settings: map[string]any{
			"extensions": map[string]float64{"score": 9.223372036854776e18},
		},
	})
	if err != nil {
		panic(err)
	}
	fmt.Println(string(request))
	// Output: {"settings":{"extensions":{"score":9223372036854776000e0}}}
}

func ExampleUserSettingsMergeRepoValueParams() {
	request, err := json.Marshal(rpc.UserSettingsMergeRepoValueParams{
		UserSettings: map[string]any{
			"extensions": map[string]any{
				"revision": json.Number("9007199254740993"),
				"score":    float64(9.223372036854776e18),
			},
		},
		RepoSettings: map[string]any{},
	})
	if err != nil {
		panic(err)
	}
	fmt.Println(string(request))
	// Output: {"repoSettings":{},"userSettings":{"extensions":{"revision":9007199254740993,"score":9223372036854776000e0}}}
}

func TestAzureMLCompensationPreservesOpaqueNumbers(t *testing.T) {
	input := []byte(`{"before":{"providerName":"provider","modelId":"model","provider":{"revision":9007199254740993},"model":{"metadata":{"revision":9007199254740993,"nested":[null,true,9007199254740995]}}}}`)
	var result rpc.AzureMlRegistrationResult
	if err := json.Unmarshal(input, &result); err != nil {
		t.Fatal(err)
	}
	encoded, err := json.Marshal(rpc.AzureMlRegistrationRequest{
		Before: result.Before, LeaseID: "lease", Name: "family",
		Operation: "restore", Registry: "azureml",
	})
	if err != nil {
		t.Fatal(err)
	}
	decode := func(data []byte) map[string]any {
		t.Helper()
		decoder := json.NewDecoder(bytes.NewReader(data))
		decoder.UseNumber()
		var value map[string]any
		if err := decoder.Decode(&value); err != nil {
			t.Fatal(err)
		}
		return value
	}
	want, got := decode(input)["before"], decode(encoded)["before"]
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Compensation rows changed:\ngot: %#v\nwant: %#v\nwire: %s", got, want, encoded)
	}
}

func TestLocalModelDescriptorsPreserveOpaqueNumbers(t *testing.T) {
	input := []byte(`{"metadata":{"revision":9007199254740993,"nested":[null,true,9007199254740995]},"serveOptions":{"seed":9007199254740993,"nested":{"revision":9007199254740995}}}`)
	var descriptor rpc.LocalModelDescriptor
	var prepared rpc.LocalModelsPrepareResult
	decode := func(data []byte) map[string]any {
		t.Helper()
		decoder := json.NewDecoder(bytes.NewReader(data))
		decoder.UseNumber()
		var value map[string]any
		if err := decoder.Decode(&value); err != nil {
			t.Fatal(err)
		}
		return value
	}
	want := decode(input)
	for _, test := range []struct {
		name   string
		result any
	}{
		{"snapshot descriptor", &descriptor},
		{"prepare result", &prepared},
	} {
		t.Run(test.name, func(t *testing.T) {
			if err := json.Unmarshal(input, test.result); err != nil {
				t.Fatal(err)
			}
			encoded, err := json.Marshal(test.result)
			if err != nil {
				t.Fatal(err)
			}
			got := decode(encoded)
			for _, field := range []string{"metadata", "serveOptions"} {
				if !reflect.DeepEqual(got[field], want[field]) {
					t.Errorf("%s changed:\ngot: %#v\nwant: %#v\nwire: %s", field, got[field], want[field], encoded)
				}
			}
		})
	}
}

func TestSavedProviderConfigurationPreservesLocalModelNumbers(t *testing.T) {
	var request rpc.SavedProviderConfigurationAddModelRequest
	if err := json.Unmarshal([]byte(`{"provider":{"name":"local","baseUrl":"model.gguf","localModel":{"seed":9007199254740993,"nested":[9007199254740995]}},"model":{"id":"model","provider":"local"}}`), &request); err != nil {
		t.Fatal(err)
	}
	encoded, err := json.Marshal(request)
	if err != nil {
		t.Fatal(err)
	}
	for _, exact := range []string{"9007199254740993", "9007199254740995"} {
		if !bytes.Contains(encoded, []byte(exact)) {
			t.Errorf("local-model number %s changed in save request: %s", exact, encoded)
		}
	}
}

func TestFeatureFlagAssignmentSnapshotPreservesNumbers(t *testing.T) {
	const response = `{"ParameterGroups":[{"Parameters":{"nullable":null,"opaque":9007199254740993}}]}`
	const snapshotJSON = `{"response":` + response + `,"expFlags":{}}`
	for _, name := range []string{"configure result", "update result", "snapshot", "read snapshot"} {
		t.Run(name, func(t *testing.T) {
			var assignmentResponse any
			switch name {
			case "configure result":
				var result rpc.SessionFeatureFlagsConfigureResult
				if err := json.Unmarshal([]byte(snapshotJSON), &result); err != nil {
					t.Fatal(err)
				}
				assignmentResponse = result.Response
			case "update result":
				var result rpc.SessionFeatureFlagsUpdateResult
				if err := json.Unmarshal([]byte(snapshotJSON), &result); err != nil {
					t.Fatal(err)
				}
				assignmentResponse = result.Response
			case "read snapshot":
				var result rpc.FeatureFlagServiceReadResult
				if err := json.Unmarshal([]byte(`{"snapshot":`+snapshotJSON+`}`), &result); err != nil {
					t.Fatal(err)
				}
				assignmentResponse = result.Snapshot.Response
			case "snapshot":
				var snapshot rpc.FeatureFlagServiceSnapshot
				if err := json.Unmarshal([]byte(snapshotJSON), &snapshot); err != nil {
					t.Fatal(err)
				}
				assignmentResponse = snapshot.Response
			}
			update := rpc.FeatureFlagServiceUpdateParams{
				Assignments: &rpc.FeatureFlagAssignmentsUpdate{Response: &assignmentResponse},
			}
			forwarded, err := json.Marshal(update)
			if err != nil {
				t.Fatal(err)
			}
			var decoded struct {
				Assignments struct {
					Response json.RawMessage `json:"response"`
				} `json:"assignments"`
			}
			if err := json.Unmarshal(forwarded, &decoded); err != nil {
				t.Fatal(err)
			}
			var compact bytes.Buffer
			if err := json.Compact(&compact, decoded.Assignments.Response); err != nil {
				t.Fatal(err)
			}
			if compact.String() != response {
				t.Fatalf("assignment document changed:\ngot:  %s\nwant: %s", compact.String(), response)
			}
		})
	}
}

func TestExistingRPCResultNumbersRetainFloat64(t *testing.T) {
	var settings rpc.UserSettingsGetResult
	var redacted rpc.SettingsRedactValueForDisplayResult
	var models rpc.ModelList
	var prepared rpc.ModelProviderConfigurationPlan
	var prompt rpc.MCPPrompt
	var workflow rpc.WorkflowRunResult
	var canvas rpc.CanvasActionInvokeResult
	for _, test := range []struct {
		name   string
		input  string
		result any
		value  func() any
	}{
		{
			name:   "redacted setting",
			input:  `{"value":42}`,
			result: &redacted,
			value:  func() any { return redacted.Value },
		},
		{
			name:   "user setting value",
			input:  `{"settings":{"maxSessionCount":{"value":42,"default":10,"isDefault":false}}}`,
			result: &settings,
			value:  func() any { return settings.Settings["maxSessionCount"].Value },
		},
		{
			name:   "user setting default",
			input:  `{"settings":{"maxSessionCount":{"value":10,"default":42,"isDefault":false}}}`,
			result: &settings,
			value:  func() any { return settings.Settings["maxSessionCount"].Default },
		},
		{
			name:   "model metadata",
			input:  `{"models":[{"id":"model","name":"Model","capabilities":{},"metadata":{"revision":42}}]}`,
			result: &models,
			value:  func() any { return models.Models[0].Metadata["revision"] },
		},
		{
			name:   "prepared provider model metadata",
			input:  `{"model":{"id":"model","provider":"local","metadata":{"revision":42}}}`,
			result: &prepared,
			value:  func() any { return prepared.Model.Metadata["revision"] },
		},
		{
			name:   "MCP prompt metadata",
			input:  `{"name":"prompt","_meta":{"revision":42}}`,
			result: &prompt,
			value:  func() any { return prompt.Meta["revision"] },
		},
		{
			name:   "workflow result",
			input:  `{"runId":"run","status":"completed","result":42}`,
			result: &workflow,
			value:  func() any { return workflow.Result },
		},
		{
			name:   "canvas action result",
			input:  `{"result":42}`,
			result: &canvas,
			value:  func() any { return canvas.Result },
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			if err := json.Unmarshal([]byte(test.input), test.result); err != nil {
				t.Fatal(err)
			}
			value := test.value()
			number, ok := value.(float64)
			if !ok || number != 42 {
				t.Fatalf("expected float64(42), got %T (%v)", value, value)
			}
		})
	}
}

func TestProviderModelMetadataPreservesCompatibleNumbers(t *testing.T) {
	input := []byte(`{"id":"model","provider":"local","metadata":{"integer":42,"decimal":0.1,"exponent":1e2,"wideInteger":9223372036854776000,"wideExponent":9.223372036854776e18,"nested":[42.0,{"negative":-7,"wide":9223372036854776000}],"large":9007199254740993,"precise":1.234567890123456789}}`)
	var model rpc.ProviderModelConfig
	if err := json.Unmarshal(input, &model); err != nil {
		t.Fatal(err)
	}
	for key, want := range map[string]float64{
		"integer": 42, "decimal": 0.1, "exponent": 100,
		"wideInteger": 9.223372036854776e18, "wideExponent": 9.223372036854776e18,
	} {
		if got, ok := model.Metadata[key].(float64); !ok || got != want {
			t.Errorf("%s: expected float64(%v), got %T (%v)", key, want, model.Metadata[key], model.Metadata[key])
		}
	}
	nested := model.Metadata["nested"].([]any)
	if got, ok := nested[0].(float64); !ok || got != 42 {
		t.Errorf("nested integer: expected float64(42), got %T (%v)", nested[0], nested[0])
	}
	negative := nested[1].(map[string]any)["negative"]
	if got, ok := negative.(float64); !ok || got != -7 {
		t.Errorf("nested negative: expected float64(-7), got %T (%v)", negative, negative)
	}
	wide := nested[1].(map[string]any)["wide"]
	if got, ok := wide.(float64); !ok || got != 9.223372036854776e18 {
		t.Errorf("nested wide integer: expected float64(9.223372036854776e18), got %T (%v)", wide, wide)
	}
	encoded, err := json.Marshal(model)
	if err != nil {
		t.Fatal(err)
	}
	for _, exact := range []string{"9007199254740993", "1.234567890123456789"} {
		if !bytes.Contains(encoded, []byte(exact)) {
			t.Errorf("metadata number %s changed: %s", exact, encoded)
		}
	}
}
