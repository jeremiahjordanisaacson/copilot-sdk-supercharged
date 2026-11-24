package copilot

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

// personAnsweredContext is the label a host attaches when a person answered a
// prompt interactively.
func personAnsweredContext() *PermissionDecisionContext {
	interactive := PermissionResponseCapabilityInteractive
	return &PermissionDecisionContext{
		Source:             PermissionDecisionSourceHumanResponse,
		ResponseCapability: &interactive,
		Surface:            PermissionDecisionSurfaceSDK,
		Outcome:            PermissionDecisionOutcomePromptedUser,
	}
}

const personAnsweredContextJSON = `{"source":"human_response","responseCapability":"interactive","surface":"sdk","outcome":"prompted_user"}`

// assertWireJSON compares JSON documents structurally, so key order does not
// matter but any extra, missing or misplaced key does.
func assertWireJSON(t *testing.T, label string, got []byte, want string) {
	t.Helper()
	var gotValue, wantValue any
	if err := json.Unmarshal(got, &gotValue); err != nil {
		t.Fatalf("%s: decode actual JSON %s: %v", label, got, err)
	}
	if err := json.Unmarshal([]byte(want), &wantValue); err != nil {
		t.Fatalf("%s: decode expected JSON %s: %v", label, want, err)
	}
	if !reflect.DeepEqual(gotValue, wantValue) {
		t.Errorf("%s:\n got %s\nwant %s", label, got, want)
	}
}

func TestUserInputReplyCarriesDecisionContextOnlyWhenLabeled(t *testing.T) {
	answer := UserInputResponse{Answer: "Only src/app.ts", WasFreeform: true}
	for _, tc := range []struct {
		name     string
		register func(*Session)
		want     string
	}{
		{
			name: "person answered",
			register: func(s *Session) {
				s.registerAttributedUserInputHandler(func(UserInputRequest, UserInputInvocation) (AttributedUserInputResponse, error) {
					return AttributedUserInputResponse{UserInputResponse: answer, DecisionContext: personAnsweredContext()}, nil
				})
			},
			want: `{"answer":"Only src/app.ts","wasFreeform":true,"decisionContext":` + personAnsweredContextJSON + `}`,
		},
		{
			name: "attributed handler without a label",
			register: func(s *Session) {
				s.registerAttributedUserInputHandler(func(UserInputRequest, UserInputInvocation) (AttributedUserInputResponse, error) {
					return AttributedUserInputResponse{UserInputResponse: answer}, nil
				})
			},
			want: `{"answer":"Only src/app.ts","wasFreeform":true}`,
		},
		{
			name: "plain handler",
			register: func(s *Session) {
				s.registerUserInputHandler(func(UserInputRequest, UserInputInvocation) (UserInputResponse, error) {
					return answer, nil
				})
			},
			want: `{"answer":"Only src/app.ts","wasFreeform":true}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			session := &Session{SessionID: "s1"}
			client := &Client{sessions: map[string]*Session{"s1": session}}
			tc.register(session)

			handler := jsonrpc2.RequestHandlerFor(client.handleUserInputRequest)
			reply, rpcErr := handler(json.RawMessage(`{"sessionId":"s1","question":"Which files?","requestId":"req-user-1"}`))
			if rpcErr != nil {
				t.Fatalf("unexpected RPC error: %v", rpcErr)
			}
			assertWireJSON(t, "userInput.request reply", reply, tc.want)
		})
	}
}

func TestUserInputImageAnswerWithoutDecisionLabel(t *testing.T) {
	session := &Session{SessionID: "s1"}
	client := &Client{sessions: map[string]*Session{"s1": session}}
	session.registerAttributedUserInputHandler(func(request UserInputRequest, _ UserInputInvocation) (AttributedUserInputResponse, error) {
		if request.ImageAttachmentConstraints == nil || request.ImageAttachmentConstraints.MaxCount != 1 {
			t.Fatalf("image constraints were dropped: %+v", request.ImageAttachmentConstraints)
		}
		return AttributedUserInputResponse{
			UserInputResponse: UserInputResponse{Answer: "", WasFreeform: true},
			Attachments: []rpc.UserInputImageAttachment{{
				Type: "image", Data: "aW1hZ2U=", MIMEType: "image/png",
			}},
		}, nil
	})
	handler := jsonrpc2.RequestHandlerFor(client.handleUserInputRequest)
	reply, rpcErr := handler(json.RawMessage(`{"sessionId":"s1","question":"Share a screenshot","imageAttachmentConstraints":{"allowedMimeTypes":["image/png"],"maxCount":1,"maxBytesPerImage":1024}}`))
	if rpcErr != nil {
		t.Fatalf("unexpected RPC error: %v", rpcErr)
	}
	assertWireJSON(t, "image answer", reply, `{"answer":"","wasFreeform":true,"attachments":[{"type":"image","data":"aW1hZ2U=","mimeType":"image/png"}]}`)
}

func TestExitPlanModeReplyCarriesDecisionContextOnlyWhenLabeled(t *testing.T) {
	review := ExitPlanModeResult{Approved: true, SelectedAction: "interactive"}
	for _, tc := range []struct {
		name     string
		register func(*Session)
		want     string
	}{
		{
			name: "person answered",
			register: func(s *Session) {
				s.registerAttributedExitPlanModeHandler(func(ExitPlanModeRequest, ExitPlanModeInvocation) (AttributedExitPlanModeResult, error) {
					return AttributedExitPlanModeResult{ExitPlanModeResult: review, DecisionContext: personAnsweredContext()}, nil
				})
			},
			want: `{"approved":true,"selectedAction":"interactive","decisionContext":` + personAnsweredContextJSON + `}`,
		},
		{
			name: "attributed handler without a label",
			register: func(s *Session) {
				s.registerAttributedExitPlanModeHandler(func(ExitPlanModeRequest, ExitPlanModeInvocation) (AttributedExitPlanModeResult, error) {
					return AttributedExitPlanModeResult{ExitPlanModeResult: review}, nil
				})
			},
			want: `{"approved":true,"selectedAction":"interactive"}`,
		},
		{
			name: "plain handler",
			register: func(s *Session) {
				s.registerExitPlanModeHandler(func(ExitPlanModeRequest, ExitPlanModeInvocation) (ExitPlanModeResult, error) {
					return review, nil
				})
			},
			want: `{"approved":true,"selectedAction":"interactive"}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			session := &Session{SessionID: "s1"}
			client := &Client{sessions: map[string]*Session{"s1": session}}
			tc.register(session)

			handler := jsonrpc2.RequestHandlerFor(client.handleExitPlanModeRequest)
			reply, rpcErr := handler(json.RawMessage(`{"sessionId":"s1","summary":"Plan","actions":["interactive","autopilot"],"recommendedAction":"autopilot","requestId":"req-plan-1"}`))
			if rpcErr != nil {
				t.Fatalf("unexpected RPC error: %v", rpcErr)
			}
			assertWireJSON(t, "exitPlanMode.request reply", reply, tc.want)
		})
	}
}

func TestElicitationAnswerSendsDecisionContextBesideResult(t *testing.T) {
	accepted := ElicitationResult{
		Action:  ElicitationActionAccept,
		Content: map[string]ElicitationFieldValue{"region": "us-east"},
	}
	const unlabeled = `{"sessionId":"session-1","requestId":"elicitation-1","result":{"action":"accept","content":{"region":"us-east"}}}`
	for _, tc := range []struct {
		name     string
		register func(*Session)
		want     string
	}{
		{
			name: "person answered",
			register: func(s *Session) {
				s.registerAttributedElicitationHandler(func(ElicitationContext) (AttributedElicitationResult, error) {
					return AttributedElicitationResult{ElicitationResult: accepted, DecisionContext: personAnsweredContext()}, nil
				})
			},
			want: `{"sessionId":"session-1","requestId":"elicitation-1","result":{"action":"accept","content":{"region":"us-east"}},"decisionContext":` + personAnsweredContextJSON + `}`,
		},
		{
			name: "attributed handler without a label",
			register: func(s *Session) {
				s.registerAttributedElicitationHandler(func(ElicitationContext) (AttributedElicitationResult, error) {
					return AttributedElicitationResult{ElicitationResult: accepted}, nil
				})
			},
			want: unlabeled,
		},
		{
			name: "plain handler",
			register: func(s *Session) {
				s.registerElicitationHandler(func(ElicitationContext) (ElicitationResult, error) {
					return accepted, nil
				})
			},
			want: unlabeled,
		},
		{
			// The SDK, not the person, cancels a result it cannot send.
			name: "SDK cancels an unsendable labeled answer",
			register: func(s *Session) {
				s.registerAttributedElicitationHandler(func(ElicitationContext) (AttributedElicitationResult, error) {
					return AttributedElicitationResult{
						ElicitationResult: ElicitationResult{
							Action:  ElicitationActionAccept,
							Content: map[string]ElicitationFieldValue{"region": struct{}{}},
						},
						DecisionContext: personAnsweredContext(),
					}, nil
				})
			},
			want: `{"sessionId":"session-1","requestId":"elicitation-1","result":{"action":"cancel"}}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			method, params := dispatchAndCaptureReply(t, tc.register, SessionEvent{Data: &ElicitationRequestedData{RequestID: "elicitation-1", Message: "Pick a region"}})

			if method != "session.ui.handlePendingElicitation" {
				t.Fatalf("method = %q, want session.ui.handlePendingElicitation", method)
			}
			got, err := json.Marshal(params)
			if err != nil {
				t.Fatalf("encode params: %v", err)
			}
			assertWireJSON(t, "session.ui.handlePendingElicitation params", got, tc.want)
		})
	}
}

func TestElicitationImageMetadataDoesNotImplyHumanAttribution(t *testing.T) {
	const key = "github.com/copilot/ask-user-images"
	metadata := map[string]any{key: map[string]any{"version": 1, "fieldNames": []any{"screenshot"}}}
	method, params := dispatchAndCaptureReply(t, func(s *Session) {
		s.registerAttributedElicitationHandler(func(context ElicitationContext) (AttributedElicitationResult, error) {
			if context.RequestID != "image-1" {
				t.Errorf("request correlation was lost: %q", context.RequestID)
			}
			if context.Meta == nil || !reflect.DeepEqual(*context.Meta, metadata) {
				t.Errorf("negotiated image constraints were lost: %+v", context.Meta)
			}
			return AttributedElicitationResult{
				ElicitationResult: ElicitationResult{Action: ElicitationActionAccept, Content: map[string]ElicitationFieldValue{"screenshot": ""}},
				Meta: map[string]any{key: map[string]any{"version": 1, "attachments": map[string]any{
					"screenshot": []any{map[string]any{"type": "image", "data": "aW1hZ2U=", "mimeType": "image/png"}},
				}}},
			}, nil
		})
	}, SessionEvent{Data: &ElicitationRequestedData{RequestID: "image-1", Message: "Show screenshot", Meta: metadata}})
	if method != "session.ui.handlePendingElicitation" {
		t.Fatalf("unexpected RPC %q", method)
	}
	got, err := json.Marshal(params)
	if err != nil {
		t.Fatal(err)
	}
	assertWireJSON(t, "image answer remains unlabeled", got, `{"sessionId":"session-1","requestId":"image-1","result":{"action":"accept","content":{"screenshot":""},"_meta":{"github.com/copilot/ask-user-images":{"version":1,"attachments":{"screenshot":[{"type":"image","data":"aW1hZ2U=","mimeType":"image/png"}]}}}}}`)
}

func TestElicitationMetadataIsIsolatedFromEventSubscribers(t *testing.T) {
	const key = "github.com/copilot/ask-user-images"
	callbackEntered := make(chan struct{})
	observerEdited := make(chan struct{})
	callbackEdited := make(chan struct{})
	observerValue := make(chan any, 1)
	metadata := map[string]any{key: map[string]any{"fields": []any{map[string]any{"name": "screenshot"}}}}
	field := func(meta map[string]any) map[string]any {
		return meta[key].(map[string]any)["fields"].([]any)[0].(map[string]any)
	}
	method, params := dispatchAndCaptureReply(t, func(s *Session) {
		s.On(func(event SessionEvent) {
			data, ok := event.Data.(*ElicitationRequestedData)
			if !ok {
				return
			}
			<-callbackEntered
			field(data.Meta)["name"] = "redacted"
			close(observerEdited)
			<-callbackEdited
			observerValue <- field(data.Meta)["name"]
		})
		s.registerElicitationHandler(func(context ElicitationContext) (ElicitationResult, error) {
			defer close(callbackEdited)
			// Existing callers can still use contexts as map keys.
			_ = map[ElicitationContext]bool{context: true}
			close(callbackEntered)
			<-observerEdited
			if context.Meta == nil {
				return ElicitationResult{}, fmt.Errorf("callback metadata was lost")
			}
			if got := field(*context.Meta)["name"]; got != "screenshot" {
				t.Errorf("observer changed callback metadata: %v", got)
			}
			field(*context.Meta)["name"] = "callback"
			return ElicitationResult{Action: ElicitationActionAccept}, nil
		})
	}, SessionEvent{Data: &ElicitationRequestedData{RequestID: "isolated-meta", Meta: metadata}})
	if method != "session.ui.handlePendingElicitation" {
		t.Errorf("method = %q, want session.ui.handlePendingElicitation", method)
	}
	assertWireJSON(t, "elicitation callback result", params["result"], `{"action":"accept"}`)
	select {
	case got := <-observerValue:
		if got != "redacted" {
			t.Errorf("callback changed observer metadata: %v", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for the observer metadata")
	}
}

func TestElicitationRejectedImageAnswerCancelsWithoutHumanAttribution(t *testing.T) {
	client, server, _ := newRuntimeShutdownRpcPair(t)
	t.Cleanup(server.Stop)
	calls := make(chan json.RawMessage, 2)
	server.SetRequestHandler("session.ui.handlePendingElicitation", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
		calls <- append(json.RawMessage(nil), params...)
		var request struct {
			Result struct {
				Action string `json:"action"`
			} `json:"result"`
		}
		if err := json.Unmarshal(params, &request); err != nil {
			t.Errorf("decode answer: %v", err)
		}
		if request.Result.Action == "accept" {
			return nil, &jsonrpc2.Error{Code: -32602, Message: "Invalid user-input image response"}
		}
		return []byte(`{"success":true}`), nil
	})
	session := &Session{SessionID: "session-1", client: client, RPC: rpc.NewSessionRPC(client, "session-1")}
	session.registerAttributedElicitationHandler(func(ElicitationContext) (AttributedElicitationResult, error) {
		return AttributedElicitationResult{
			ElicitationResult: ElicitationResult{Action: ElicitationActionAccept, Content: map[string]ElicitationFieldValue{"screenshot": ""}},
			Meta: map[string]any{"github.com/copilot/ask-user-images": map[string]any{
				"version": 1, "attachments": map[string]any{"screenshot": []any{
					map[string]any{"type": "image", "data": "invalid", "mimeType": "image/png"},
				}},
			}},
			DecisionContext: personAnsweredContext(),
		}, nil
	})
	session.handleElicitationRequest(ElicitationContext{RequestID: "invalid-image"}, "invalid-image")
	if len(calls) != 2 {
		t.Fatalf("rejected callback returned after %d requests, want answer and cancellation", len(calls))
	}
	<-calls
	select {
	case cancel := <-calls:
		assertWireJSON(t, "SDK cancellation is unattributed", cancel, `{"sessionId":"session-1","requestId":"invalid-image","result":{"action":"cancel"}}`)
	default:
		t.Fatal("rejected callback returned without cancelling the pending question")
	}
}

// TestPromptHandlersFromSessionConfig drives create and resume with plain
// handlers, attributed handlers, or both. Either kind enables the prompt's
// callbacks; when both are set, the attributed handler answers.
func TestPromptHandlersFromSessionConfig(t *testing.T) {
	plainUserInput := func(UserInputRequest, UserInputInvocation) (UserInputResponse, error) {
		return UserInputResponse{Answer: "plain", WasFreeform: true}, nil
	}
	attributedUserInput := func(UserInputRequest, UserInputInvocation) (AttributedUserInputResponse, error) {
		return AttributedUserInputResponse{
			UserInputResponse: UserInputResponse{Answer: "attributed", WasFreeform: true},
			DecisionContext:   personAnsweredContext(),
		}, nil
	}
	plainPlan := func(ExitPlanModeRequest, ExitPlanModeInvocation) (ExitPlanModeResult, error) {
		return ExitPlanModeResult{Approved: true, Feedback: "plain"}, nil
	}
	attributedPlan := func(ExitPlanModeRequest, ExitPlanModeInvocation) (AttributedExitPlanModeResult, error) {
		return AttributedExitPlanModeResult{
			ExitPlanModeResult: ExitPlanModeResult{Approved: true, Feedback: "attributed"},
			DecisionContext:    personAnsweredContext(),
		}, nil
	}
	plainElicitation := func(ElicitationContext) (ElicitationResult, error) {
		return ElicitationResult{Action: ElicitationActionAccept, Content: map[string]ElicitationFieldValue{"by": "plain"}}, nil
	}
	attributedElicitation := func(ElicitationContext) (AttributedElicitationResult, error) {
		return AttributedElicitationResult{
			ElicitationResult: ElicitationResult{Action: ElicitationActionAccept, Content: map[string]ElicitationFieldValue{"by": "attributed"}},
			DecisionContext:   personAnsweredContext(),
		}, nil
	}

	for _, resume := range []bool{false, true} {
		for _, tc := range []struct {
			name              string
			plain, attributed bool
		}{
			{name: "plain", plain: true},
			{name: "attributed", attributed: true},
			{name: "both", plain: true, attributed: true},
		} {
			t.Run(fmt.Sprintf("%s/resume=%t", tc.name, resume), func(t *testing.T) {
				rpcClient, server, _ := newRuntimeShutdownRpcPair(t)
				t.Cleanup(server.Stop)
				client := &Client{
					client:   rpcClient,
					RPC:      rpc.NewServerRPC(rpcClient),
					sessions: make(map[string]*Session),
				}
				client.setupNotificationHandler()

				openParams := make(chan json.RawMessage, 1)
				open := func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
					openParams <- append(json.RawMessage(nil), params...)
					return []byte(`{"sessionId":"` + sessionIDFromParams(t, params) + `","workspacePath":"/workspace"}`), nil
				}
				server.SetRequestHandler("session.create", open)
				server.SetRequestHandler("session.resume", open)
				elicitationAnswers := make(chan json.RawMessage, 1)
				server.SetRequestHandler("session.ui.handlePendingElicitation", func(params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
					elicitationAnswers <- append(json.RawMessage(nil), params...)
					return []byte(`{"success":true}`), nil
				})

				var session *Session
				var err error
				if resume {
					config := &ResumeSessionOptions{}
					if tc.plain {
						config.Interaction.OnUserInputRequest, config.Interaction.OnExitPlanModeRequest, config.Interaction.OnElicitationRequest = plainUserInput, plainPlan, plainElicitation
					}
					if tc.attributed {
						config.Interaction.OnAttributedUserInputRequest, config.Interaction.OnAttributedExitPlanModeRequest, config.Interaction.OnAttributedElicitationRequest = attributedUserInput, attributedPlan, attributedElicitation
					}
					session, err = client.ResumeSession(t.Context(), "prompt-handlers", config)
				} else {
					config := &CreateSessionOptions{SessionID: "prompt-handlers"}
					if tc.plain {
						config.Interaction.OnUserInputRequest, config.Interaction.OnExitPlanModeRequest, config.Interaction.OnElicitationRequest = plainUserInput, plainPlan, plainElicitation
					}
					if tc.attributed {
						config.Interaction.OnAttributedUserInputRequest, config.Interaction.OnAttributedExitPlanModeRequest, config.Interaction.OnAttributedElicitationRequest = attributedUserInput, attributedPlan, attributedElicitation
					}
					session, err = client.CreateSession(t.Context(), config)
				}
				if err != nil {
					t.Fatalf("open session: %v", err)
				}
				t.Cleanup(session.stopEventProcessing)

				var flags struct {
					Interaction struct {
						RequestUserInput    *bool `json:"requestUserInput"`
						RequestExitPlanMode *bool `json:"requestExitPlanMode"`
						RequestElicitation  *bool `json:"requestElicitation"`
					} `json:"interaction"`
				}
				params := <-openParams
				if err := json.Unmarshal(params, &flags); err != nil {
					t.Fatalf("decode open params: %v", err)
				}
				for name, flag := range map[string]*bool{
					"requestUserInput":    flags.Interaction.RequestUserInput,
					"requestExitPlanMode": flags.Interaction.RequestExitPlanMode,
					"requestElicitation":  flags.Interaction.RequestElicitation,
				} {
					if flag == nil || !*flag {
						t.Errorf("%s = %v, want true in %s", name, flag, params)
					}
				}

				wantUserInput := `{"answer":"plain","wasFreeform":true}`
				wantPlan := `{"approved":true,"feedback":"plain"}`
				wantElicitation := `{"sessionId":"prompt-handlers","requestId":"elicitation-1","result":{"action":"accept","content":{"by":"plain"}}}`
				if tc.attributed {
					wantUserInput = `{"answer":"attributed","wasFreeform":true,"decisionContext":` + personAnsweredContextJSON + `}`
					wantPlan = `{"approved":true,"feedback":"attributed","decisionContext":` + personAnsweredContextJSON + `}`
					wantElicitation = `{"sessionId":"prompt-handlers","requestId":"elicitation-1","result":{"action":"accept","content":{"by":"attributed"}},"decisionContext":` + personAnsweredContextJSON + `}`
				}

				reply, err := server.Request(t.Context(), "userInput.request", map[string]any{
					"sessionId": session.SessionID, "question": "Which files?", "requestId": "req-user-1",
				})
				if err != nil {
					t.Fatalf("userInput.request: %v", err)
				}
				assertWireJSON(t, "userInput.request reply", reply, wantUserInput)

				reply, err = server.Request(t.Context(), "exitPlanMode.request", map[string]any{
					"sessionId": session.SessionID, "summary": "Plan", "actions": []string{"interactive"}, "recommendedAction": "interactive",
				})
				if err != nil {
					t.Fatalf("exitPlanMode.request: %v", err)
				}
				assertWireJSON(t, "exitPlanMode.request reply", reply, wantPlan)

				event := json.RawMessage(`{"sessionId":"prompt-handlers","event":{"id":"event-1","timestamp":"2026-11-11T00:00:00Z","type":"elicitation.requested","data":{"requestId":"elicitation-1","message":"Pick a region"}}}`)
				if _, rpcErr := jsonrpc2.NotificationHandlerFor(client.handleSessionEvent)(event); rpcErr != nil {
					t.Fatalf("session.event: %v", rpcErr)
				}
				select {
				case answer := <-elicitationAnswers:
					assertWireJSON(t, "session.ui.handlePendingElicitation params", answer, wantElicitation)
				case <-time.After(5 * time.Second):
					t.Fatal("timed out waiting for the elicitation answer")
				}
			})
		}
	}
}
