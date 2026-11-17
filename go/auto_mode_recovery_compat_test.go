// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot_test

import (
	"encoding/json"
	"testing"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
)

func TestAutoModeRecoveryCanonicalEventFields(t *testing.T) {
	wire := []byte(`{"id":"capabilities","timestamp":"2026-11-05T00:00:00Z","type":"capabilities.changed","data":{"ui":{"autoModeRecovery":true,"canvases":true,"elicitation":false,"mcpApps":true}}}`)
	var event copilot.SessionEvent
	if err := json.Unmarshal(wire, &event); err != nil {
		t.Fatal(err)
	}
	data, ok := event.Data.(*copilot.CapabilitiesChangedData)
	if !ok {
		t.Fatalf("capability payload type = %T", event.Data)
	}
	if data.UI == nil || data.UI.Canvases == nil || !*data.UI.Canvases ||
		data.UI.Elicitation == nil || *data.UI.Elicitation ||
		data.UI.MCPApps == nil || !*data.UI.MCPApps ||
		data.UI.AutoModeRecovery == nil || !*data.UI.AutoModeRecovery {
		t.Fatalf("capability values = %#v", data.UI)
	}
	roundTrip, err := json.Marshal(event)
	if err != nil {
		t.Fatal(err)
	}
	var decoded map[string]any
	if err := json.Unmarshal(roundTrip, &decoded); err != nil {
		t.Fatal(err)
	}
	payload, ok := decoded["data"].(map[string]any)
	if !ok {
		t.Fatalf("missing data in %s", roundTrip)
	}
	ui, ok := payload["ui"].(map[string]any)
	if !ok || ui["autoModeRecovery"] != true {
		t.Fatalf("recovery acknowledgment lost in %s", roundTrip)
	}
}
