package e2e

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
	"github.com/microsoft/copilot-sdk/go/v2/internal/e2e/testharness"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

func TestExtensionPathsPublicCreateAndResume(t *testing.T) {
	t.Parallel()
	ctx := testharness.NewTestContext(t)
	assets := extensionPathAssets(t, ctx.CLIPath)
	node, err := exec.LookPath("node")
	if err != nil {
		t.Fatalf("Find extension host: %v", err)
	}
	client := ctx.NewClient(func(options *copilot.ClientOptions) {
		options.Extensions.LaunchProvider = extensionLaunchProviderFunc(func(request *copilot.ExtensionLaunchProviderResolveRequest) (*copilot.ExtensionLaunchProviderResolveResult, error) {
			return &copilot.ExtensionLaunchProviderResolveResult{
				Launch: &copilot.ExtensionLaunchProfile{
					Executable: node,
					Args:       []string{filepath.Join(assets, "preloads", "extension_bootstrap.mjs")},
					Env: map[string]string{
						"COPILOT_CLI_DIST_DIR": assets,
						"EXTENSION_PATH":       request.ModulePath,
					},
				},
			}, nil
		})
	})
	t.Cleanup(func() {
		if err := client.Stop(); err != nil {
			client.ForceStop()
			t.Errorf("Stop extension runtime: %v", err)
		}
	})

	first := writeExtensionPathProbe(t, ctx.WorkDir, "first")
	second := writeExtensionPathProbe(t, ctx.WorkDir, "second")
	extensions := copilot.SessionExtensionOptions{
		Enabled: copilot.Bool(true), SDKPath: copilot.String(filepath.Join(assets, "copilot-sdk")),
	}
	permissions := copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}
	session, err := client.Client.CreateSession(t.Context(), &copilot.CreateSessionOptions{
		WorkingDirectory:      ctx.WorkDir,
		ConfigDirectory:       ctx.HomeDir,
		EnableConfigDiscovery: copilot.Bool(false),
		ExtensionPaths:        []string{first},
		Extensions:            extensions,
		Permissions:           permissions,
		Filesystem: copilot.CreateSessionFilesystemOptions{
			TrustWorkingDirectory: "session",
		},
	})
	if err != nil {
		t.Fatalf("Create with exact paths: %v", err)
	}
	assertExtensionPathOperation(t, session, first, "first")

	resumed, err := client.ResumeSession(t.Context(), session.SessionID, &copilot.ResumeSessionOptions{
		WorkingDirectory:      ctx.WorkDir,
		ConfigDirectory:       ctx.HomeDir,
		EnableConfigDiscovery: copilot.Bool(false),
		ExtensionPaths:        []string{second},
		Extensions:            extensions,
		Permissions:           permissions,
	})
	if err != nil {
		t.Fatalf("Resume with replacement exact paths: %v", err)
	}
	assertExtensionPathOperation(t, resumed, second, "second")
}

func assertExtensionPathOperation(t *testing.T, session *copilot.Session, entry, name string) {
	t.Helper()
	if _, err := session.RPC.Tools.InitializeAndValidate(t.Context()); err != nil {
		t.Fatalf("Initialize selected extension: %v", err)
	}
	inventory, err := session.RPC.Extensions.Reconcile(t.Context())
	if err != nil {
		t.Fatalf("Reconcile selected extension: %v", err)
	}
	if len(inventory.Extensions) != 1 {
		t.Fatalf("Expected only %q, got %+v", entry, inventory.Extensions)
	}
	extension := inventory.Extensions[0]
	if extension.ModulePath == nil || *extension.ModulePath != entry || extension.Source != "explicit" || extension.Status != "running" {
		t.Fatalf("Expected running explicit entry %q, got %+v", entry, extension)
	}
	instance := "go-path-" + name
	if _, err := session.RPC.Canvas.Open(t.Context(), &rpc.CanvasOpenRequest{
		CanvasID: "path-" + name, InstanceID: instance, Input: map[string]any{},
	}); err != nil {
		t.Fatalf("Open selected extension canvas: %v", err)
	}
	result, err := session.RPC.Canvas.Action().Invoke(t.Context(), &rpc.CanvasActionInvokeRequest{
		InstanceID: instance, ActionName: "triple", Input: map[string]any{"value": 17},
	})
	if err != nil {
		t.Fatalf("Invoke selected extension: %v", err)
	}
	value, ok := result.Result.(map[string]any)
	if !ok || value["tripled"] != float64(51) {
		t.Fatalf("Expected selected extension to return tripled=51, got %#v", result.Result)
	}
	if _, err := session.RPC.Canvas.Close(t.Context(), &rpc.CanvasCloseRequest{InstanceID: instance}); err != nil {
		t.Fatalf("Close selected extension canvas: %v", err)
	}
}

func writeExtensionPathProbe(t *testing.T, root, name string) string {
	t.Helper()
	entry := filepath.Join(root, name, "extension.mjs")
	if err := os.MkdirAll(filepath.Dir(entry), 0755); err != nil {
		t.Fatal(err)
	}
	script := fmt.Sprintf(`import { joinSession, createCanvas } from "@microsoft/copilot-sdk/extension";
await joinSession({ canvases: { definitions: [createCanvas({
  id: %q, displayName: "Go path probe", description: "Exercises a selected extension.",
  open: () => ({ title: "Selected extension" }),
  actions: [{
    name: "triple", description: "Triples the supplied integer.",
    inputSchema: { type: "object", properties: { value: { type: "integer" } }, required: ["value"] },
    handler: ({ input }) => ({ tripled: input.value * 3 }),
  }],
})] } });
`, "path-"+name)
	if err := os.WriteFile(entry, []byte(script), 0644); err != nil {
		t.Fatal(err)
	}
	return entry
}

func extensionPathAssets(t *testing.T, runtime string) string {
	t.Helper()
	for directory := filepath.Dir(runtime); ; directory = filepath.Dir(directory) {
		if _, err := os.Stat(filepath.Join(directory, "preloads", "extension_bootstrap.mjs")); err == nil {
			if _, err := os.Stat(filepath.Join(directory, "copilot-sdk", "extension.js")); err == nil {
				return directory
			}
		}
		if filepath.Dir(directory) == directory {
			t.Fatalf("Missing extension assets beside runtime %q", runtime)
		}
	}
}
