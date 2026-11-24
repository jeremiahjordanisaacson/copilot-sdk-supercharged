// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"context"
	"encoding/json"
	"net"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
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
		UIHandlePendingUserInputRequest{RequestID: "input", Response: UIUserInputResponse{"yes", true}},
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

func TestUIHandlePendingUserInputWithImagesPreservesWireEnvelope(t *testing.T) {
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()
	client := jsonrpc2.NewClient(clientConn, clientConn)
	server := jsonrpc2.NewClient(serverConn, serverConn)
	requests := make(chan json.RawMessage, 1)
	server.SetRequestHandler("session.ui.handlePendingUserInput", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
		requests <- params
		return json.RawMessage(`{"success":true}`), nil
	})
	client.Start()
	server.Start()
	defer client.Stop()
	defer server.Stop()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	var response UIUserInputResponseWithImages
	const imageResponse = `{"answer":"","wasFreeform":true,"attachments":[{"type":"image","data":"aW1hZ2U=","mimeType":"image/png"}]}`
	if err := json.Unmarshal([]byte(imageResponse), &response); err != nil {
		t.Fatal(err)
	}
	const decisionJSON = `{"outcome":"prompted_user","source":"human_response","surface":"copilot_app","responseCapability":"interactive"}`
	var decision PermissionDecisionContext
	if err := json.Unmarshal([]byte(decisionJSON), &decision); err != nil {
		t.Fatal(err)
	}
	result, err := NewSessionRPC(client, "session").UI.HandlePendingUserInputWithImages(ctx, &UIHandlePendingUserInputWithImagesRequest{
		RequestID:       "input",
		Response:        response,
		DecisionContext: &decision,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !result.Success {
		t.Fatalf("pending image answer not accepted: %+v", result)
	}
	select {
	case raw := <-requests:
		var actual, expected any
		if err := json.Unmarshal(raw, &actual); err != nil {
			t.Fatal(err)
		}
		if err := json.Unmarshal([]byte(`{"sessionId":"session","requestId":"input","response":`+imageResponse+`,"decisionContext":`+decisionJSON+`}`), &expected); err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(actual, expected) {
			t.Fatalf("wire request = %s; want %+v", raw, expected)
		}
	case <-ctx.Done():
		t.Fatal(ctx.Err())
	}
}
