// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
)

var _ interface {
	HandlePendingElicitation(context.Context, *UIHandlePendingElicitationRequest) (*UIElicitationResult, error)
	HandlePendingUserInput(context.Context, *UIHandlePendingUserInputRequest) (*UIHandlePendingResult, error)
	HandlePendingExitPlanMode(context.Context, *UIHandlePendingExitPlanModeRequest) (*UIHandlePendingResult, error)
} = (*UIAPI)(nil)

// Unspecified decision attribution must remain absent from prompt-answer requests.
func TestUIHandlePendingRequestsOmitUnspecifiedDecisionContext(t *testing.T) {
	for _, request := range []any{
		UIHandlePendingElicitationRequest{RequestID: "elicitation", Result: UIElicitationResponse{Action: UIElicitationResponseActionAccept}},
		UIHandlePendingUserInputRequest{RequestID: "input", Response: UIUserInputResponse{Answer: "yes"}},
		UIHandlePendingExitPlanModeRequest{RequestID: "plan", Response: UIExitPlanModeResponse{Approved: true}},
	} {
		data, err := json.Marshal(request)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(data), "decisionContext") {
			t.Fatalf("legacy request %T carried a label: %s", request, data)
		}
	}
}
