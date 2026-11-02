package copilot

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/github/copilot-sdk/go/internal/jsonrpc2"
	"github.com/github/copilot-sdk/go/rpc"
)

func TestAhpAuthenticationPolicyValidatedBeforeConnection(t *testing.T) {
	valid := func(context.Context, string) (*AhpAuthenticationResult, error) { return nil, nil }
	for _, mode := range []ClientMode{ModeCopilotCli, ModeEmpty} {
		for _, policy := range []AhpHostAuthentication{
			{}, {Mode: "unknown"}, {Mode: "custom", Authenticate: valid},
			{Mode: "custom", RequireIdentity: "  ", Authenticate: valid},
			{Mode: "custom", RequireIdentity: "owner"},
		} {
			t.Run(string(mode)+"/"+string(policy.Mode)+"/"+policy.RequireIdentity, func(t *testing.T) {
				client := NewClient(&ClientOptions{Mode: mode, BaseDirectory: ".", Connection: StdioConnection{Path: "must-not-start"}})
				_, err := client.StartAhpHost(t.Context(), &AhpHostOptions{
					LocalServer: &rpc.HostLocalServerOptions{}, Authentication: policy,
				})
				if err == nil || !strings.Contains(err.Error(), "authentication") || client.state != stateDisconnected {
					t.Fatalf("invalid policy should fail before connecting: state=%v error=%v", client.state, err)
				}
			})
		}
	}
	for _, policy := range []AhpHostAuthentication{
		{Mode: "none"}, {Mode: "custom", RequireIdentity: "owner", Authenticate: valid},
	} {
		client := NewClient(&ClientOptions{Connection: StdioConnection{Path: "must-not-start"}})
		_, err := client.StartAhpHost(t.Context(), &AhpHostOptions{
			GitHubEnvironment: &rpc.HostGitHubEnvironmentOptions{}, Authentication: policy,
		})
		if err == nil || !strings.Contains(err.Error(), "githubEnvironment requires github") || client.state != stateDisconnected {
			t.Fatalf("Mission Control incompatibility should fail before connecting: %v", err)
		}
	}
}

func TestAhpCustomAuthenticationBoundary(t *testing.T) {
	for _, scenario := range []string{"accept", "accept-lower-t", "accept-lower-z", "accept-lower-tz", "reject", "error", "panic", "identity", "missing-expiry", "invalid-expiry", "past-expiry"} {
		t.Run(scenario, func(t *testing.T) {
			client, server, _ := ahpFixture(t)
			var callbackContext context.Context
			var acceptedExpiry string
			policy := AhpHostAuthentication{Mode: "custom", RequireIdentity: "owner"}
			policy.Authenticate = func(ctx context.Context, token string) (*AhpAuthenticationResult, error) {
				callbackContext = ctx
				if token != "admission-only" || ctx.Err() != nil {
					return nil, errors.New("wrong token or cancelled context")
				}
				result := &AhpAuthenticationResult{Identity: "owner", ExpiresAt: time.Now().Add(time.Hour).UTC().Format(time.RFC3339)}
				switch scenario {
				case "accept-lower-t":
					result.ExpiresAt = strings.ReplaceAll(result.ExpiresAt, "T", "t")
				case "accept-lower-z":
					result.ExpiresAt = strings.ReplaceAll(result.ExpiresAt, "Z", "z")
				case "accept-lower-tz":
					result.ExpiresAt = strings.ToLower(result.ExpiresAt)
				case "reject":
					return nil, nil
				case "error":
					return nil, errors.New("validator unavailable")
				case "panic":
					panic("validator failure")
				case "identity":
					result.Identity = "another-owner"
				case "missing-expiry":
					result.ExpiresAt = ""
				case "invalid-expiry":
					result.ExpiresAt = "not-a-date"
				case "past-expiry":
					result.ExpiresAt = "2000-01-01T00:00:00Z"
				}
				acceptedExpiry = result.ExpiresAt
				return result, nil
			}
			server.SetRequestHandler("host.start", jsonrpc2.RequestHandlerFor(func(params *rpc.HostStartRequest) (*rpc.HostStartResult, *jsonrpc2.Error) {
				got, err := json.Marshal(params.Authentication)
				if err != nil || string(got) != `{"mode":"custom","requireIdentity":"owner"}` {
					t.Errorf("wire authentication = %s (%v)", got, err)
				}
				return &rpc.HostStartResult{HostID: params.HostID}, nil
			}))
			host, err := client.StartAhpHost(t.Context(), &AhpHostOptions{Authentication: policy, LocalServer: &rpc.HostLocalServerOptions{}})
			if err != nil {
				t.Fatal(err)
			}
			// Mutating the caller's policy must not reconfigure the captured validator.
			policy.RequireIdentity = "changed"
			response, err := server.Request(t.Context(), "host.validateAuthentication", &rpc.HostValidateAuthenticationRequest{
				HostID: host.HostID, RequestID: "validation", Token: "admission-only",
			})
			if strings.HasPrefix(scenario, "accept") {
				var result AhpAuthenticationResult
				if err != nil || json.Unmarshal(response, &result) != nil || result.Identity != "owner" || result.ExpiresAt != acceptedExpiry {
					t.Fatalf("accepted result = %s, error = %v", response, err)
				}
			} else if scenario == "reject" {
				if err != nil || string(response) != "null" {
					t.Fatalf("rejection = %s, error = %v", response, err)
				}
			} else if err == nil {
				t.Fatalf("invalid validator response accepted: %s", response)
			}
			if callbackContext == nil || callbackContext.Err() != context.Canceled {
				t.Fatalf("validation context not cancelled on completion: %v", callbackContext)
			}
			client.ahp.mu.Lock()
			pending := len(client.ahp.authentications)
			client.ahp.mu.Unlock()
			if pending != 0 {
				t.Fatalf("pending validations = %d, want 0", pending)
			}
		})
	}
}

func TestAhpAuthenticationCancelledBeforeValidationStarts(t *testing.T) {
	client, _, _ := ahpFixture(t)
	const hostID = "host"
	client.ahp.hosts = map[string]AhpHostOptions{
		hostID: {Authentication: AhpHostAuthentication{
			Mode: "custom", RequireIdentity: "owner",
			Authenticate: func(context.Context, string) (*AhpAuthenticationResult, error) {
				t.Error("cancelled attempt must not invoke the application")
				return nil, nil
			},
		}},
	}
	client.ahp.authentications = make(map[string]*ahpAuthentication)
	request := &rpc.HostValidateAuthenticationRequest{HostID: hostID, RequestID: "pending", Token: "token"}
	if err := client.prepareAhpAuthentication(request); err != nil {
		t.Fatal(err)
	}
	client.cancelAhpAuthentication(&rpc.HostAuthenticationCancelledNotification{HostID: hostID, RequestID: request.RequestID})
	if _, err := client.validateAhpAuthentication(request); err == nil || !strings.Contains(err.Message, "authentication ended") {
		t.Fatalf("cancelled attempt result = %v", err)
	}
	if len(client.ahp.authentications) != 0 {
		t.Fatal("cancelled attempt was not removed")
	}
}

func TestAhpAuthenticationCancellationFencesLateResults(t *testing.T) {
	for _, end := range []string{"notification", "exit", "disconnect"} {
		t.Run(end, func(t *testing.T) {
			client, server, _ := ahpFixture(t)
			entered := make(chan context.Context, 1)
			finish := make(chan struct{})
			exited := make(chan struct{})
			host, err := client.StartAhpHost(t.Context(), &AhpHostOptions{
				LocalServer: &rpc.HostLocalServerOptions{},
				Authentication: AhpHostAuthentication{Mode: "custom", RequireIdentity: "owner",
					Authenticate: func(ctx context.Context, _ string) (*AhpAuthenticationResult, error) {
						defer close(exited)
						entered <- ctx
						<-finish
						return &AhpAuthenticationResult{Identity: "owner", ExpiresAt: time.Now().Add(time.Hour).Format(time.RFC3339)}, nil
					}},
			})
			if err != nil {
				t.Fatal(err)
			}
			done := make(chan error, 1)
			go func() {
				_, err := server.Request(t.Context(), "host.validateAuthentication", &rpc.HostValidateAuthenticationRequest{HostID: host.HostID, RequestID: "pending", Token: "token"})
				done <- err
			}()
			ctx := awaitAhpTest(t, entered)
			// A mismatched host must not cancel another host's request.
			client.cancelAhpAuthentication(&rpc.HostAuthenticationCancelledNotification{HostID: "another-host", RequestID: "pending"})
			if ctx.Err() != nil {
				t.Fatal("mismatched cancellation ended validation")
			}
			_, duplicateErr := server.Request(t.Context(), "host.validateAuthentication", &rpc.HostValidateAuthenticationRequest{HostID: host.HostID, RequestID: "pending"})
			if duplicateErr == nil {
				t.Fatal("duplicate validation request accepted")
			}
			switch end {
			case "notification":
				data, err := json.Marshal(&rpc.HostAuthenticationCancelledNotification{HostID: host.HostID, RequestID: "pending"})
				if err != nil {
					t.Fatal(err)
				}
				jsonrpc2.NotificationHandlerFor(client.cancelAhpAuthentication)(data)
			case "exit":
				client.handleAhpExit(&rpc.HostExitedNotification{HostID: host.HostID})
			case "disconnect":
				client.disconnectAhpHosts()
			}
			if err := awaitAhpTest(t, done); err == nil || !strings.Contains(err.Error(), "authentication ended") {
				t.Fatalf("cancelled request error = %v", err)
			}
			if ctx.Err() != context.Canceled {
				t.Fatalf("callback context error = %v", ctx.Err())
			}
			close(finish)
			awaitAhpTest(t, exited)
		})
	}
}

func TestAhpAuthenticationTrustedSiblingInFactories(t *testing.T) {
	for _, resume := range []bool{false, true} {
		t.Run(map[bool]string{false: "create", true: "resume"}[resume], func(t *testing.T) {
			client, server, _ := ahpFixture(t)
			check := func(authentication AhpSessionAuthentication, config any) {
				t.Helper()
				verified, ok := authentication.(*AhpSessionAuthenticationCustom)
				if !ok || verified.Identity != "trusted-owner" {
					t.Errorf("authentication = %#v", authentication)
				}
				switch config := config.(type) {
				case *SessionConfig:
					if config.GitHubToken != "" {
						t.Errorf("admission identity became a GitHub token: %q", config.GitHubToken)
					}
				case *ResumeSessionConfig:
					if config.GitHubToken != "" {
						t.Errorf("admission identity became a GitHub token: %q", config.GitHubToken)
					}
				}
			}
			host, err := client.StartAhpHost(t.Context(), &AhpHostOptions{
				LocalServer: &rpc.HostLocalServerOptions{}, Authentication: AhpHostAuthentication{Mode: "custom", RequireIdentity: "trusted-owner", Authenticate: func(context.Context, string) (*AhpAuthenticationResult, error) {
					return &AhpAuthenticationResult{Identity: "trusted-owner"}, nil
				}},
				CreateSession: func(ctx context.Context, request AhpSessionCreateRequest) (*Session, error) {
					check(request.Authentication, request.Config)
					request.Config.OnPermissionRequest = PermissionHandler.ApproveAll
					return client.CreateSession(ctx, request.Config)
				},
				ResumeSession: func(ctx context.Context, request AhpSessionResumeRequest) (*Session, error) {
					check(request.Authentication, request.Config)
					request.Config.OnPermissionRequest = PermissionHandler.ApproveAll
					return client.ResumeSessionWithOptions(ctx, request.SessionID, request.Config)
				},
			})
			if err != nil {
				t.Fatal(err)
			}
			_, err = server.Request(t.Context(), "host.materializeSession", &rpc.HostSessionCreateCallback{
				HostID: host.HostID, HandoffID: "handoff", Resume: &resume,
				Authentication: rpc.HostSessionAuthenticationCustom{Identity: "trusted-owner"},
				Config:         map[string]any{"sessionId": "requested", "workingDirectory": "."},
			})
			if err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestAhpTrustedAuthenticationMustMatchHostPolicy(t *testing.T) {
	custom := AhpHostAuthentication{Mode: "custom", RequireIdentity: "owner"}
	cases := []struct {
		name    string
		policy  AhpHostAuthentication
		trusted rpc.HostSessionAuthentication
		ok      bool
	}{
		{"github", AhpHostAuthentication{Mode: "github"}, rpc.HostSessionAuthenticationGitHub{Identity: "octocat"}, true},
		{"github blank", AhpHostAuthentication{Mode: "github"}, rpc.HostSessionAuthenticationGitHub{Identity: " "}, false},
		{"github vs none", AhpHostAuthentication{Mode: "github"}, rpc.HostSessionAuthenticationNone{}, false},
		{"none", AhpHostAuthentication{Mode: "none"}, rpc.HostSessionAuthenticationNone{}, true},
		{"none with identity", AhpHostAuthentication{Mode: "none"}, rpc.HostSessionAuthenticationCustom{Identity: "owner"}, false},
		{"custom", custom, rpc.HostSessionAuthenticationCustom{Identity: "owner"}, true},
		{"custom wrong identity", custom, rpc.HostSessionAuthenticationCustom{Identity: "other"}, false},
		{"custom vs github", custom, rpc.HostSessionAuthenticationGitHub{Identity: "owner"}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if err := validateAhpTrustedAuthentication(tc.policy, tc.trusted); (err == nil) != tc.ok {
				t.Fatalf("ok=%v, err=%v", tc.ok, err)
			}
		})
	}
}
