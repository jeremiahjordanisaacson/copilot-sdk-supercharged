// Copyright (c) Microsoft Corporation. All rights reserved.

// Package legacy builds prompt answers and handlers the way code written
// before attributed prompt answers does, so the SDK must keep compiling it.
package legacy

import copilot "github.com/microsoft/copilot-sdk/go/v2"

var (
	UserInput   = copilot.UserInputResponse{"Only src/app.ts", true}
	PlanReview  = copilot.ExitPlanModeResult{true, "interactive", "Looks good"}
	Elicitation = copilot.ElicitationResult{copilot.ElicitationActionAccept, map[string]copilot.ElicitationFieldValue{"region": "us-east"}}

	Create = copilot.CreateSessionOptions{
		Interaction: copilot.SessionInteractionOptions{
			OnUserInputRequest: func(copilot.UserInputRequest, copilot.UserInputInvocation) (copilot.UserInputResponse, error) {
				return UserInput, nil
			},
			OnExitPlanModeRequest: func(copilot.ExitPlanModeRequest, copilot.ExitPlanModeInvocation) (copilot.ExitPlanModeResult, error) {
				return PlanReview, nil
			},
			OnElicitationRequest: func(copilot.ElicitationContext) (copilot.ElicitationResult, error) {
				return Elicitation, nil
			},
		},
	}
	Resume = copilot.ResumeSessionOptions{
		Interaction: copilot.SessionInteractionOptions{
			OnUserInputRequest:    Create.Interaction.OnUserInputRequest,
			OnExitPlanModeRequest: Create.Interaction.OnExitPlanModeRequest,
			OnElicitationRequest:  Create.Interaction.OnElicitationRequest,
		},
	}
)
