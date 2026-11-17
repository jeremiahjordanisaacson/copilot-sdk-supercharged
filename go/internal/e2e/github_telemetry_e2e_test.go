package e2e

import (
	"testing"
	"time"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
	"github.com/microsoft/copilot-sdk/go/v2/internal/e2e/testharness"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestGitHubTelemetryE2E(t *testing.T) {
	t.Parallel()
	t.Run("should forward github telemetry for a live session", func(t *testing.T) {
		// This notification-forwarding test currently runs in the explicit stdio matrix cell.
		testharness.SkipIfInProcess(t, "GitHub telemetry forwarding is not honored in-process")

		ctx := testharness.NewTestContext(t)
		ctx.ConfigureForTest(t)

		notifications := make(chan *rpc.GitHubTelemetryNotification, 1)
		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.GitHub.OnTelemetry = func(notification *rpc.GitHubTelemetryNotification) {
				select {
				case notifications <- notification:
				default:
				}
			}
		})
		t.Cleanup(func() { client.ForceStop() })

		session, err := client.CreateSession(t.Context(), &copilot.CreateSessionOptions{Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}})
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}
		t.Cleanup(func() { session.Disconnect() })

		var notification *rpc.GitHubTelemetryNotification
		select {
		case notification = <-notifications:
			if notification == nil {
				t.Fatal("Received nil GitHub telemetry notification")
			}
		case <-time.After(30 * time.Second):
			t.Fatal("Timed out waiting for GitHub telemetry notification after 30s")
		}
		if notification.SessionID == nil || *notification.SessionID == "" {
			t.Fatal("Expected a non-empty SessionID")
		}
		if notification.Event.Kind == "" {
			t.Fatal("Expected a non-empty Event.Kind")
		}
	})
}
