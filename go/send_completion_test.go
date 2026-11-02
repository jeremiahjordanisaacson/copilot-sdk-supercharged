// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestSession_SendAndWaitCompletionRPC(t *testing.T) {
	for _, kind := range []string{"response", "no-output", "execution-error", "child-response", "autopilot", "aborted"} {
		t.Run(kind, func(t *testing.T) {
			session, server := newSetToolsTestSession(t)
			session.eventCh = make(chan SessionEvent, 8)
			session.eventDone = make(chan struct{})
			go session.processEvents()
			defer session.stopEventProcessing()
			ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
			defer cancel()
			type outcome struct {
				event *SessionEvent
				err   error
			}

			result := make(chan outcome, 1)
			go func() {
				event, err := session.SendAndWait(ctx, MessageOptions{Prompt: "question"})
				result <- outcome{event, err}
			}()
			request := server.expectRequest(t, "session.sendAndWait")
			if request.Params["prompt"] != "question" {
				t.Fatalf("unexpected completion params: %#v", request.Params)
			}
			session.dispatchEvent(SessionEvent{ID: "old-error", Data: &SessionErrorData{Message: "unrelated failure"}})
			session.dispatchEvent(SessionEvent{ID: "old-idle", Data: &SessionIdleData{}})
			if kind == "execution-error" {
				server.respondError(t, request.ID, -32603, "owned failure")
			} else {
				response := &SessionEvent{
					ID:   "owned-response",
					Data: &AssistantMessageData{MessageID: "owned-response", Content: "owned response"},
				}
				switch kind {
				case "no-output":
					response = nil
				case "child-response":
					response.AgentID = ptr("child")
				}
				server.respond(t, request.ID, map[string]any{
					"messageId": "owned-request", "response": response,
					"aborted": kind == "aborted", "completionEventId": "owned-idle",
				})
				var terminal SessionEventData = &SessionIdleData{}
				if kind == "autopilot" || kind == "aborted" {
					terminal = &SessionAutopilotStoppedData{Reason: "task_complete", RequestID: ptr("owned-request")}
				}
				session.dispatchEvent(SessionEvent{ID: "owned-idle", Data: terminal})
			}
			select {
			case got := <-result:
				switch kind {
				case "execution-error":
					if got.err == nil || !strings.Contains(got.err.Error(), "owned failure") {
						t.Fatalf("execution failure = %v", got.err)
					}
				case "child-response":
					if got.err == nil || !strings.Contains(got.err.Error(), "non-assistant") {
						t.Fatalf("invalid response failure = %v", got.err)
					}
				case "aborted":
					if got.err == nil || !strings.Contains(got.err.Error(), "aborted") {
						t.Fatalf("aborted completion = %+v, %v", got.event, got.err)
					}
				case "no-output":
					if got.err != nil || got.event != nil {
						t.Fatalf("no-output completion = %+v, %v", got.event, got.err)
					}
				default:
					if got.err != nil || got.event == nil || got.event.ID != "owned-response" {
						t.Fatalf("owned completion = %+v, %v", got.event, got.err)
					}
				}
			case <-ctx.Done():
				t.Fatal("completion did not finish:", ctx.Err())
			}
		})
	}
}

func TestSession_DisconnectRetiresPendingCompletionRPC(t *testing.T) {
	session, server := newSetToolsTestSession(t)
	session.eventCh = make(chan SessionEvent, 8)
	session.eventDone = make(chan struct{})
	go session.processEvents()
	defer session.stopEventProcessing()
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	result := make(chan error, 1)
	go func() {
		_, err := session.SendAndWait(ctx, MessageOptions{Prompt: "unresolved work"})
		result <- err
	}()
	request := server.expectRequest(t, "session.sendAndWait")
	disconnected := make(chan error, 1)
	go func() { disconnected <- session.Disconnect() }()
	detach := server.expectRequest(t, "session.detach")
	server.respond(t, detach.ID, map[string]any{"success": true})
	if err := <-disconnected; err != nil {
		t.Fatal("disconnect:", err)
	}
	select {
	case err := <-result:
		if err == nil || !strings.Contains(err.Error(), "session closed before response completed") {
			t.Fatalf("expected response retirement error, got %v", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("completion wait outlived local session retirement")
	}
	cancellation := server.expectRequest(t, "$/cancelRequest")
	if id, err := json.Marshal(cancellation.Params["id"]); err != nil || string(id) != string(request.ID) {
		t.Fatalf("cancelled %v instead of completion request %s: %v", cancellation.Params["id"], request.ID, err)
	}
}

func TestSession_CompletionSurvivesDisconnectInItsTerminalCallback(t *testing.T) {
	session, server := newSetToolsTestSession(t)
	session.eventCh = make(chan SessionEvent, 8)
	session.eventDone = make(chan struct{})
	go session.processEvents()
	defer session.stopEventProcessing()
	ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
	defer cancel()
	disconnected := make(chan error, 1)
	session.On(func(event SessionEvent) {
		if event.ID == "owned-idle" {
			disconnected <- session.Disconnect()
		}
	})
	type outcome struct {
		event *SessionEvent
		err   error
	}
	result := make(chan outcome, 1)
	go func() {
		event, err := session.SendAndWait(ctx, MessageOptions{Prompt: "question"})
		result <- outcome{event, err}
	}()
	request := server.expectRequest(t, "session.sendAndWait")
	server.respond(t, request.ID, map[string]any{
		"messageId": "owned-request", "aborted": false, "completionEventId": "owned-idle",
		"response": &SessionEvent{ID: "owned-response", Data: &AssistantMessageData{Content: "owned response"}},
	})
	ping := make(chan error, 1)
	go func() {
		_, err := session.client.Request(ctx, "ping", nil)
		ping <- err
	}()
	barrier := server.expectRequest(t, "ping")
	server.respond(t, barrier.ID, map[string]any{})
	if err := <-ping; err != nil {
		t.Fatal("completion response transport barrier:", err)
	}
	session.dispatchEvent(SessionEvent{ID: "owned-idle", Data: &SessionIdleData{}})
	detach := server.expectRequest(t, "session.detach")
	server.respond(t, detach.ID, map[string]any{"success": true})
	if err := <-disconnected; err != nil {
		t.Fatal("disconnect from terminal callback:", err)
	}
	select {
	case got := <-result:
		if got.err != nil || got.event == nil || got.event.ID != "owned-response" {
			t.Fatalf("lost an attributable completion at retirement: %+v, %v", got.event, got.err)
		}
	case <-ctx.Done():
		t.Fatal("completion did not finish:", ctx.Err())
	}
}
