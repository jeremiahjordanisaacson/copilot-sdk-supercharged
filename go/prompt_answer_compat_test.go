// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot_test

import (
	"testing"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
	legacy "github.com/microsoft/copilot-sdk/go/v2/testdata/prompt_answer_legacy"
)

// The legacy package compiles only while the prompt answer structs keep their
// positional fields and the plain handler config fields keep their types. The
// comparisons also require the structs to stay comparable.
func TestPromptAnswerLegacyPositionalLiterals(t *testing.T) {
	if want := (copilot.UserInputResponse{Answer: "Only src/app.ts", WasFreeform: true}); legacy.UserInput != want {
		t.Errorf("positional UserInputResponse = %+v, want %+v", legacy.UserInput, want)
	}
	if want := (copilot.ExitPlanModeResult{Approved: true, SelectedAction: "interactive", Feedback: "Looks good"}); legacy.PlanReview != want {
		t.Errorf("positional ExitPlanModeResult = %+v, want %+v", legacy.PlanReview, want)
	}
	if legacy.Elicitation.Action != copilot.ElicitationActionAccept || len(legacy.Elicitation.Content) != 1 || legacy.Elicitation.Content["region"] != "us-east" {
		t.Errorf("positional ElicitationResult = %+v, want accept with region us-east", legacy.Elicitation)
	}
	if legacy.Create.Interaction.OnUserInputRequest == nil || legacy.Create.Interaction.OnExitPlanModeRequest == nil || legacy.Create.Interaction.OnElicitationRequest == nil ||
		legacy.Resume.Interaction.OnUserInputRequest == nil || legacy.Resume.Interaction.OnExitPlanModeRequest == nil || legacy.Resume.Interaction.OnElicitationRequest == nil {
		t.Error("plain prompt handlers were not retained in the session configs")
	}
}
