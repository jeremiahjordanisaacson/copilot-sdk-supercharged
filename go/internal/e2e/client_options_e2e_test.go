package e2e

import (
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
	"github.com/microsoft/copilot-sdk/go/v2/internal/e2e/testharness"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

// Mirrors the E2E portions of dotnet/test/ClientOptionsTests.cs (snapshot category "client_options").
// .NET-only tests that exercise validation on the options struct alone are skipped here because
// Go's ClientOptions is a plain struct with no setter validation; equivalent behavior is covered
// in package-level unit tests.
func TestClientOptionsE2E(t *testing.T) {
	t.Parallel()
	t.Run("should listen on configured TCP port", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		port := getAvailableTCPPort(t)

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			useTCP(opts, port, "")
		})
		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}
		if got := client.RuntimePort(); got != port {
			t.Errorf("Expected RuntimePort=%d, got %d", port, got)
		}

		// Ping over the connection to confirm it is usable.
		pingResp, err := client.Ping(t.Context(), "fixed-port")
		if err != nil {
			t.Fatalf("Ping failed: %v", err)
		}
		if !strings.Contains(pingResp.Message, "fixed-port") {
			t.Errorf("Expected ping response to echo 'fixed-port', got %q", pingResp.Message)
		}
	})

	t.Run("should use client cwd for default workingdirectory", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		ctx.ConfigureForTest(t)

		clientCwd := filepath.Join(ctx.WorkDir, "client-cwd")
		if err := os.MkdirAll(clientCwd, 0755); err != nil {
			t.Fatalf("Failed to create clientCwd: %v", err)
		}
		if err := os.WriteFile(filepath.Join(clientCwd, "marker.txt"), []byte("I am in the client cwd"), 0644); err != nil {
			t.Fatalf("Failed to write marker file: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			if !testharness.IsInProcessTransport() {
				connection := opts.Runtime.(copilot.SpawnedStdioConnection)
				connection.WorkingDirectory = clientCwd
				opts.Runtime = connection
			}
		})
		t.Cleanup(func() { client.ForceStop() })

		config := &copilot.CreateSessionOptions{Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}}
		if testharness.IsInProcessTransport() {
			config.WorkingDirectory = clientCwd
		}
		// Exercise the raw SDK default on stdio, not the fixture's session default.
		session, err := client.Client.CreateSession(t.Context(), config)
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}
		t.Cleanup(func() { session.Disconnect() })

		evt, err := session.SendAndWait(t.Context(), copilot.MessageOptions{
			Prompt: "Read the file marker.txt and tell me what it says",
		})
		if err != nil {
			t.Fatalf("SendAndWait failed: %v", err)
		}
		assistant, ok := evt.Data.(*copilot.AssistantMessageData)
		if !ok {
			t.Fatalf("Expected AssistantMessageData, got %T", evt.Data)
		}
		if !strings.Contains(assistant.Content, "client cwd") {
			t.Errorf("Expected assistant message to contain 'client cwd', got %q", assistant.Content)
		}
	})

	t.Run("should propagate process options to spawned cli", func(t *testing.T) {
		// Mirrors: Should_Propagate_Process_Options_To_Spawned_Cli
		// Spawns a fake stdio CLI (a Node.js script) so we can assert that the
		// SDK passes the right argv / env / cwd / RPC params through to the
		// subprocess.
		ctx := testharness.NewTestContext(t)

		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-capture-"+randomHex(t)+".json")
		telemetryPath := filepath.Join(ctx.WorkDir, "telemetry.jsonl")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{
				Path:                      cliPath,
				Args:                      []string{"--capture-file", capturePath},
				BaseDirectory:             filepath.Join(ctx.WorkDir, "copilot-home-from-option"),
				SessionIdleTimeoutSeconds: 17,
				Env:                       append(ctx.Env(), "COPILOT_HOME="+filepath.Join(ctx.WorkDir, "copilot-home-from-env")),
			}
			opts.GitHub.Token = "process-option-token"
			opts.Observability.LogLevel = "debug"
			opts.Observability.Telemetry = &copilot.TelemetryConfig{
				OTLPEndpoint:   "http://127.0.0.1:4318",
				OTLPProtocol:   "http/protobuf",
				FilePath:       telemetryPath,
				ExporterType:   "file",
				SourceName:     "go-sdk-e2e",
				CaptureContent: copilot.Bool(true),
			}
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
		})

		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}

		capture := readCapture(t, capturePath)
		args := capture.Args

		assertArgValue(t, args, "--log-level", "debug")
		if !containsStringE(args, "--stdio") {
			t.Errorf("Expected --stdio in args, got %v", args)
		}
		assertArgValue(t, args, "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN")
		if !containsStringE(args, "--no-auto-login") {
			t.Errorf("Expected --no-auto-login in args, got %v", args)
		}
		assertArgValue(t, args, "--session-idle-timeout", "17")

		expectedCwd, _ := filepath.Abs(ctx.WorkDir)
		actualCwd, _ := filepath.Abs(capture.WorkingDirectory)
		if expectedCwd != actualCwd {
			t.Errorf("Expected cwd=%q, got %q", expectedCwd, actualCwd)
		}

		expectEnv := map[string]string{
			"COPILOT_HOME":                                       filepath.Join(ctx.WorkDir, "copilot-home-from-option"),
			"COPILOT_SDK_AUTH_TOKEN":                             "process-option-token",
			"COPILOT_OTEL_ENABLED":                               "true",
			"OTEL_EXPORTER_OTLP_ENDPOINT":                        "http://127.0.0.1:4318",
			"OTEL_EXPORTER_OTLP_PROTOCOL":                        "http/protobuf",
			"COPILOT_OTEL_FILE_EXPORTER_PATH":                    telemetryPath,
			"COPILOT_OTEL_EXPORTER_TYPE":                         "file",
			"COPILOT_OTEL_SOURCE_NAME":                           "go-sdk-e2e",
			"OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT": "true",
		}
		for k, v := range expectEnv {
			if got := capture.Env[k]; got != v {
				t.Errorf("Expected env[%s]=%q, got %q", k, v, got)
			}
		}

		session, err := client.CreateSession(t.Context(), &copilot.CreateSessionOptions{EnableConfigDiscovery: copilot.Bool(true), Instructions: copilot.CreateSessionInstructionOptions{OnDemandDiscovery: copilot.Bool(true)}, Events: copilot.SessionEventOptions{IncludeSubAgentStreamingEvents: copilot.Bool(false)}, Agents: copilot.SessionAgentOptions{LocalDiscoveryOnly: copilot.Bool(false)}, Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}})
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}

		updated := readCapture(t, capturePath)
		var createReq *capturedRequest
		for i := range updated.Requests {
			if updated.Requests[i].Method == "session.create" {
				createReq = &updated.Requests[i]
				break
			}
		}
		if createReq == nil {
			t.Fatalf("session.create request was not captured. Captured requests: %+v", updated.Requests)
			return
		}
		params, ok := createReq.Params.(map[string]any)
		if !ok {
			t.Fatalf("Expected session.create params to be an object, got %T", createReq.Params)
		}
		if v, ok := capturedParam(t, params, "enableConfigDiscovery").(bool); !ok || !v {
			t.Errorf("Expected enableConfigDiscovery=true, got %v", v)
		}
		if v, ok := capturedParam(t, params, "instructions.onDemandDiscovery").(bool); !ok || !v {
			t.Errorf("Expected instructions.onDemandDiscovery=true, got %v", v)
		}
		if v, ok := capturedParam(t, params, "events.includeSubAgentStreamingEvents").(bool); !ok || v {
			t.Errorf("Expected events.includeSubAgentStreamingEvents=false, got %v", v)
		}
		if v, ok := capturedParam(t, params, "agents.localDiscoveryOnly").(bool); !ok || v {
			t.Errorf("Expected agents.localDiscoveryOnly=false, got %v", v)
		}

		sessionID := session.SessionID
		if err := session.Disconnect(); err != nil {
			t.Fatalf("Disconnect failed: %v", err)
		}
		resumed, err := client.ResumeSession(t.Context(), sessionID, &copilot.ResumeSessionOptions{Agents: copilot.SessionAgentOptions{LocalDiscoveryOnly: copilot.Bool(false)}, Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}})
		if err != nil {
			t.Fatalf("ResumeSession failed: %v", err)
		}
		t.Cleanup(func() { _ = resumed.Disconnect() })

		resumedCapture := readCapture(t, capturePath)
		for _, req := range resumedCapture.Requests {
			if req.Method != "session.resume" {
				continue
			}
			resumeParams, ok := req.Params.(map[string]any)
			if !ok {
				t.Fatalf("Expected session.resume params to be an object, got %T", req.Params)
			}
			if v, ok := capturedParam(t, resumeParams, "agents.localDiscoveryOnly").(bool); !ok || v {
				t.Errorf("Expected resume agents.localDiscoveryOnly=false, got %v", v)
			}
			return
		}
		t.Fatalf("session.resume request was not captured. Captured requests: %+v", resumedCapture.Requests)
	})

	t.Run("should register and invoke extension launch provider during startup", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-extension-provider-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-extension-provider-"+randomHex(t)+".json")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		requests := make(chan *copilot.ExtensionLaunchProviderResolveRequest, 1)
		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{
				Path: cliPath,
				Args: []string{"--capture-file", capturePath},
			}
			opts.GitHub.Token = ""
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
			opts.Extensions.LaunchProvider = extensionLaunchProviderFunc(func(
				request *copilot.ExtensionLaunchProviderResolveRequest,
			) (*copilot.ExtensionLaunchProviderResolveResult, error) {
				requests <- request
				return &copilot.ExtensionLaunchProviderResolveResult{
					Launch: &copilot.ExtensionLaunchProfile{
						Executable: "go",
						Args:       []string{"extension-host"},
						Env:        map[string]string{"EXTENSION_SOURCE": "go"},
					},
				}, nil
			})
		})
		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}

		request := <-requests
		if request.ID != "project:go-e2e" || request.Name != "go-e2e" ||
			request.ModulePath != "/extensions/go-e2e.go" || request.Source != rpc.ExtensionSourceProject {
			t.Fatalf("Unexpected extension launch request: %+v", request)
		}

		capture := readCapture(t, capturePath)
		foundRegistration := false
		for _, captured := range capture.Requests {
			if captured.Method == "registerExtensionLaunchProvider" {
				foundRegistration = true
				break
			}
		}
		if !foundRegistration {
			t.Fatalf("registerExtensionLaunchProvider request was not captured: %+v", capture.Requests)
		}
		if len(capture.ClientResponses) != 1 {
			t.Fatalf("Expected one extension launch response, got %+v", capture.ClientResponses)
		}
		response := capture.ClientResponses[0]
		if response.ID != 9001 {
			t.Fatalf("Expected response id 9001, got %d", response.ID)
		}
		launch := response.Result["launch"].(map[string]any)
		if launch["executable"] != "go" {
			t.Fatalf("Expected Go launch profile, got %+v", launch)
		}
	})

	t.Run("should send empty-mode custom agent locality defaults in initial requests", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-empty-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-empty-capture-"+randomHex(t)+".json")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{
				Path: cliPath,
				Args: []string{"--capture-file", capturePath},
			}
			opts.Mode = copilot.ModeEmpty
			connection := opts.Runtime.(copilot.SpawnedStdioConnection)
			connection.BaseDirectory = ctx.WorkDir
			opts.Runtime = connection
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
		})
		t.Cleanup(func() { client.ForceStop() })

		session, err := client.CreateSession(t.Context(), &copilot.CreateSessionOptions{Tools: copilot.SessionToolOptions{AvailableTools: []string{"builtin:ask_user"}}, Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}})
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}
		sessionID := session.SessionID
		if err := session.Disconnect(); err != nil {
			t.Fatalf("Disconnect failed: %v", err)
		}

		resumed, err := client.ResumeSession(t.Context(), sessionID, &copilot.ResumeSessionOptions{Tools: copilot.SessionToolOptions{AvailableTools: []string{"builtin:ask_user"}}, Permissions: copilot.SessionPermissionOptions{OnRequest: copilot.PermissionHandler.ApproveAll}})
		if err != nil {
			t.Fatalf("ResumeSession failed: %v", err)
		}
		t.Cleanup(func() { _ = resumed.Disconnect() })

		capture := readCapture(t, capturePath)
		foundCreate := false
		foundResume := false
		for _, req := range capture.Requests {
			params, ok := req.Params.(map[string]any)
			if !ok {
				continue
			}
			switch req.Method {
			case "session.create":
				foundCreate = true
				if v, ok := capturedParam(t, params, "agents.localDiscoveryOnly").(bool); !ok || !v {
					t.Errorf("Expected create agents.localDiscoveryOnly=true, got %v", v)
				}
			case "session.resume":
				foundResume = true
				if v, ok := capturedParam(t, params, "agents.localDiscoveryOnly").(bool); !ok || !v {
					t.Errorf("Expected resume agents.localDiscoveryOnly=true, got %v", v)
				}
			}
		}
		if !foundCreate || !foundResume {
			t.Fatalf("Expected create and resume requests, got %+v", capture.Requests)
		}
	})

	t.Run("should forward advanced session creation options to the CLI", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-capture-"+randomHex(t)+".json")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{Path: cliPath, Args: []string{"--capture-file", capturePath}}
			opts.GitHub.Token = "advanced-create-client-token"
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
		})
		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}

		sessionID := "advanced-session-id"
		workingDirectory := t.TempDir()
		configDirectory := t.TempDir()
		embeddingCacheStorage := "in-memory"
		organizationCustomInstructions := "organization guidance"
		maxAiCredits := float64(42)
		extensionSDKPath := filepath.Join(ctx.WorkDir, "extension-sdk")
		session, err := client.CreateSession(t.Context(), &copilot.CreateSessionOptions{SessionID: sessionID, Name: "go named at create", ClientName: "go-sdk-e2e-client", Model: "claude-sonnet-5", Inference: copilot.SessionInferenceOptions{ReasoningEffort: "low", ReasoningSummary: copilot.ReasoningSummaryNone, ContextTier: copilot.ContextTierLongContext, EnableCitations: copilot.Bool(true)}, ConfigDirectory: configDirectory, EnableConfigDiscovery: copilot.Bool(true), State: copilot.SessionStateOptions{SkipEmbeddingRetrieval: copilot.Bool(true), EmbeddingCacheStorage: &embeddingCacheStorage, EnableCrossSessionStore: copilot.Bool(false)}, Instructions: copilot.CreateSessionInstructionOptions{OrganizationText: &organizationCustomInstructions, OnDemandDiscovery: copilot.Bool(true), SkipCustomInstructions: copilot.Bool(true), Directories: []string{"instructions"}}, Hooks: copilot.SessionHookOptions{EnableRepositoryHooks: copilot.Bool(true)}, Git: copilot.SessionGitOptions{EnableHostGitOperations: copilot.Bool(false), CoauthorEnabled: copilot.Bool(false)}, Skills: copilot.SessionSkillOptions{Enabled: copilot.Bool(false), Directories: []string{"skills"}, DisabledSkills: []string{"disabled-skill"}}, WorkingDirectory: workingDirectory, Events: copilot.SessionEventOptions{Streaming: copilot.Bool(true), IncludeSubAgentStreamingEvents: copilot.Bool(false)}, Tools: copilot.SessionToolOptions{AvailableTools: []string{"read_file"}, ExcludedTools: []string{"bash"}, ManageScheduleEnabled: copilot.Bool(false)}, Agents: copilot.SessionAgentOptions{ExcludedBuiltinAgents: []string{"legacy-agent"}, LocalDiscoveryOnly: copilot.Bool(true)}, GitHub: copilot.SessionGitHubOptions{TelemetryEnabled: copilot.Bool(false), Token: "advanced-create-session-token"}, Execution: copilot.SessionExecutionOptions{Limits: &rpc.SessionLimitsConfig{MaxAiCredits: &maxAiCredits}}, Plugins: copilot.SessionPluginOptions{Directories: []string{"plugins"}}, MCP: copilot.SessionMCPOptions{EnableApps: true}, Canvases: copilot.SessionCanvasOptions{Definitions: []copilot.CanvasDeclaration{{
			ID:          "canvas",
			DisplayName: "Canvas",
			Description: "Canvas description",
			InputSchema: map[string]any{"type": "object"},
		}}, RequestRenderer: copilot.Bool(true)}, Extensions: copilot.SessionExtensionOptions{Enabled: copilot.Bool(true), SDKPath: &extensionSDKPath, Identity: &copilot.ExtensionInfo{Source: "github-app", Name: "go-e2e-extension"}}, Experiments: copilot.SessionExperimentOptions{Assignments: &copilot.CopilotExpAssignmentResponse{Flights: map[string]string{"feature": "enabled"}, AssignmentContext: "ctx"}},
		})
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}
		session.Disconnect()

		createReq := getCapturedRequest(t, capturePath, "session.create")
		params, ok := createReq.Params.(map[string]any)
		if !ok {
			t.Fatalf("Expected session.create params object, got %T", createReq.Params)
		}
		expectedValues := map[string]any{
			"sessionId":                             sessionID,
			"name":                                  "go named at create",
			"clientName":                            "go-sdk-e2e-client",
			"model":                                 "claude-sonnet-5",
			"inference.reasoningEffort":             "low",
			"inference.reasoningSummary":            "none",
			"inference.contextTier":                 "long_context",
			"configDirectory":                       configDirectory,
			"enableConfigDiscovery":                 true,
			"state.skipEmbeddingRetrieval":          true,
			"state.embeddingCacheStorage":           embeddingCacheStorage,
			"instructions.organizationText":         organizationCustomInstructions,
			"instructions.onDemandDiscovery":        true,
			"hooks.enableRepositoryHooks":           true,
			"git.enableHostGitOperations":           false,
			"state.enableCrossSessionStore":         false,
			"skills.enabled":                        false,
			"workingDirectory":                      workingDirectory,
			"events.streaming":                      true,
			"events.includeSubAgentStreamingEvents": false,
			"github.telemetryEnabled":               false,
			"inference.enableCitations":             true,
			"instructions.skipCustomInstructions":   true,
			"agents.localDiscoveryOnly":             true,
			"git.coauthorEnabled":                   false,
			"tools.manageScheduleEnabled":           false,
			"github.token":                          "advanced-create-session-token",
			"mcp.enableApps":                        true,
			"canvases.requestRenderer":              true,
			"extensions.enabled":                    true,
			"extensions.sdkPath":                    extensionSDKPath,
			"mcp.envValueMode":                      "direct",
		}
		for key, expected := range expectedValues {
			if actual := capturedParam(t, params, key); actual != expected {
				t.Fatalf("Expected %s=%#v, got %#v in %#v", key, expected, actual, params)
			}
			if _, exists := params["remoteSession"]; exists {
				t.Fatalf("High-level session creation must omit legacy remoteSession, got %#v", params)
			}
		}
		assertStringArray(t, capturedParam(t, params, "tools.availableTools"), []string{"read_file"})
		assertStringArray(t, capturedParam(t, params, "tools.excludedTools"), []string{"bash"})
		assertStringArray(t, capturedParam(t, params, "agents.excludedBuiltinAgents"), []string{"legacy-agent"})
		assertStringArray(t, capturedParam(t, params, "skills.directories"), []string{"skills"})
		assertStringArray(t, capturedParam(t, params, "plugins.directories"), []string{"plugins"})
		assertStringArray(t, capturedParam(t, params, "instructions.directories"), []string{"instructions"})
		assertStringArray(t, capturedParam(t, params, "skills.disabledSkills"), []string{"disabled-skill"})
		if actual := capturedParam(t, params, "execution.limits.maxAiCredits"); actual != maxAiCredits {
			t.Fatalf("Expected execution.limits.maxAiCredits=%v, got %#v", maxAiCredits, actual)
		}
		extensionInfo := capturedParam(t, params, "extensions.identity").(map[string]any)
		if extensionInfo["source"] != "github-app" || extensionInfo["name"] != "go-e2e-extension" {
			t.Fatalf("Expected extensionInfo to be forwarded, got %#v", extensionInfo)
		}
		canvases := capturedParam(t, params, "canvases.definitions").([]any)
		canvas := canvases[0].(map[string]any)
		if canvas["id"] != "canvas" || canvas["displayName"] != "Canvas" || canvas["description"] != "Canvas description" {
			t.Fatalf("Expected canvas declaration to be forwarded, got %#v", canvas)
		}
		if actual := capturedParam(t, params, "experiments.assignments.Flights.feature"); actual != "enabled" {
			t.Fatalf("Expected experiment flight=enabled, got %#v", actual)
		}
	})

	t.Run("should forward singular provider configuration on session creation", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-capture-"+randomHex(t)+".json")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{Path: cliPath, Args: []string{"--capture-file", capturePath}}
			opts.GitHub.Token = "provider-client-token"
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
		})
		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}

		session, err := client.CreateSession(t.Context(), &copilot.CreateSessionOptions{Model: "base-model", GitHub: copilot.SessionGitHubOptions{Capi: &copilot.CapiSessionOptions{Enabled: copilot.Bool(false)}}, Inference: byokInference(&copilot.ProviderConfig{
			Type:            "openai",
			WireAPI:         "responses",
			Transport:       "websockets",
			BaseURL:         "https://models.example.test/v1",
			APIKey:          "provider-key",
			ModelID:         "base-model",
			WireModel:       "wire-model",
			MaxPromptTokens: 1000,
			MaxOutputTokens: 2000,
			Headers:         map[string]string{"x-provider": "go"},
		}, copilot.SessionInferenceOptions{}),
		})
		if err != nil {
			t.Fatalf("CreateSession failed: %v", err)
		}
		session.Disconnect()

		createReq := getCapturedRequest(t, capturePath, "session.create")
		params := createReq.Params.(map[string]any)
		provider := capturedParam(t, params, "inference.providers").([]any)[0].(map[string]any)
		model := capturedParam(t, params, "inference.models").([]any)[0].(map[string]any)
		for key, expected := range map[string]any{
			"type":      "openai",
			"wireApi":   "responses",
			"transport": "websockets",
			"baseUrl":   "https://models.example.test/v1",
			"apiKey":    "provider-key",
		} {
			if provider[key] != expected {
				t.Fatalf("Expected provider.%s=%#v, got %#v in %#v", key, expected, provider[key], provider)
			}
			for key, expected := range map[string]any{
				"modelId": "base-model", "wireModel": "wire-model",
				"maxPromptTokens": float64(1000), "maxOutputTokens": float64(2000),
			} {
				if model[key] != expected {
					t.Fatalf("Expected model.%s=%#v, got %#v in %#v", key, expected, model[key], model)
				}
			}
		}
		if provider["headers"].(map[string]any)["x-provider"] != "go" {
			t.Fatalf("Expected provider headers to be forwarded, got %#v", provider["headers"])
		}
	})

	t.Run("should forward advanced session resume options to the CLI", func(t *testing.T) {
		ctx := testharness.NewTestContext(t)
		cliPath := filepath.Join(ctx.WorkDir, "fake-cli-"+randomHex(t)+".js")
		capturePath := filepath.Join(ctx.WorkDir, "fake-cli-capture-"+randomHex(t)+".json")
		if err := os.WriteFile(cliPath, []byte(fakeStdioCliScript), 0644); err != nil {
			t.Fatalf("Failed to write fake CLI script: %v", err)
		}

		client := ctx.NewClient(func(opts *copilot.ClientOptions) {
			opts.Runtime = copilot.SpawnedStdioConnection{Path: cliPath, Args: []string{"--capture-file", capturePath}}
			opts.GitHub.Token = "advanced-resume-client-token"
			opts.GitHub.UseLoggedInUser = copilot.Bool(false)
		})
		t.Cleanup(func() { client.ForceStop() })

		if err := client.Start(t.Context()); err != nil {
			t.Fatalf("Start failed: %v", err)
		}

		workingDirectory := t.TempDir()
		configDirectory := t.TempDir()
		continuePendingWork := false
		extensionSDKPath := filepath.Join(ctx.WorkDir, "resume-extension-sdk")
		session, err := client.ResumeSession(t.Context(), "resume-session-id", &copilot.ResumeSessionOptions{Model: "gpt-5-mini", Inference: copilot.SessionInferenceOptions{ReasoningEffort: "low", ReasoningSummary: copilot.ReasoningSummaryNone, ContextTier: copilot.ContextTierLongContext}, WorkingDirectory: workingDirectory, ConfigDirectory: configDirectory, EnableConfigDiscovery: copilot.Bool(false), Hooks: copilot.SessionHookOptions{EnableRepositoryHooks: copilot.Bool(true)}, SuppressResumeEvent: true, ContinuePendingWork: &continuePendingWork, Events: copilot.SessionEventOptions{Streaming: copilot.Bool(true), IncludeSubAgentStreamingEvents: copilot.Bool(false)}, GitHub: copilot.SessionGitHubOptions{Token: "advanced-resume-session-token"}, Canvases: copilot.ResumeSessionCanvasOptions{Definitions: []copilot.CanvasDeclaration{{
			ID:          "resume-canvas",
			DisplayName: "Resume Canvas",
			Description: "Resume canvas description",
			InputSchema: map[string]any{"type": "object"},
		}}, Open: []rpc.OpenCanvasInstance{{
			CanvasID:    "resume-canvas",
			ExtensionID: "github-app/go-e2e-extension",
			InstanceID:  "resume-instance",
			Input:       map[string]any{"value": "from-resume"},
		}}, RequestRenderer: copilot.Bool(true)}, Extensions: copilot.SessionExtensionOptions{Enabled: copilot.Bool(true), SDKPath: &extensionSDKPath, Identity: &copilot.ExtensionInfo{Source: "github-app", Name: "go-e2e-extension"}}, Experiments: copilot.SessionExperimentOptions{Assignments: &copilot.CopilotExpAssignmentResponse{Flights: map[string]string{"resumeFeature": "enabled"}, AssignmentContext: "ctx"}},
		})
		if err != nil {
			t.Fatalf("ResumeSession failed: %v", err)
		}
		session.Disconnect()

		resumeReq := getCapturedRequest(t, capturePath, "session.resume")
		params := resumeReq.Params.(map[string]any)
		expectedValues := map[string]any{
			"sessionId":                             "resume-session-id",
			"model":                                 "gpt-5-mini",
			"inference.reasoningEffort":             "low",
			"inference.reasoningSummary":            "none",
			"inference.contextTier":                 "long_context",
			"workingDirectory":                      workingDirectory,
			"configDirectory":                       configDirectory,
			"enableConfigDiscovery":                 false,
			"hooks.enableRepositoryHooks":           true,
			"suppressResumeEvent":                   true,
			"continuePendingWork":                   false,
			"events.streaming":                      true,
			"events.includeSubAgentStreamingEvents": false,
			"github.token":                          "advanced-resume-session-token",
			"canvases.requestRenderer":              true,
			"extensions.enabled":                    true,
			"extensions.sdkPath":                    extensionSDKPath,
			"mcp.envValueMode":                      "direct",
		}
		for key, expected := range expectedValues {
			if actual := capturedParam(t, params, key); actual != expected {
				t.Fatalf("Expected resume %s=%#v, got %#v in %#v", key, expected, actual, params)
			}
		}
		openCanvases := capturedParam(t, params, "canvases.open").([]any)
		openCanvas := openCanvases[0].(map[string]any)
		if openCanvas["canvasId"] != "resume-canvas" || openCanvas["extensionId"] != "github-app/go-e2e-extension" ||
			openCanvas["instanceId"] != "resume-instance" {
			t.Fatalf("Expected open canvas state to be forwarded, got %#v", openCanvas)
		}
		extensionInfo := capturedParam(t, params, "extensions.identity").(map[string]any)
		if extensionInfo["source"] != "github-app" || extensionInfo["name"] != "go-e2e-extension" {
			t.Fatalf("Expected extensionInfo on resume, got %#v", extensionInfo)
		}
		if actual := capturedParam(t, params, "experiments.assignments.Flights.resumeFeature"); actual != "enabled" {
			t.Fatalf("Expected resume experiment flight=enabled, got %#v", actual)
		}
	})

}

func capturedParam(t *testing.T, params map[string]any, path string) any {
	t.Helper()
	var current any = params
	for _, key := range strings.Split(path, ".") {
		object, ok := current.(map[string]any)
		if !ok {
			t.Fatalf("Expected object while reading %s, got %#v in %#v", path, current, params)
		}
		current, ok = object[key]
		if !ok {
			t.Fatalf("Missing %s in captured params: %#v", path, params)
		}
	}
	return current
}

// ---------------------------------------------------------------------------
// Unit-style tests mirroring the property-only tests in
// dotnet/test/ClientOptionsTests.cs.
// ---------------------------------------------------------------------------

func TestClientOptionsUnit(t *testing.T) {
	t.Parallel()
	t.Run("should accept GitHubToken option", func(t *testing.T) {
		// Mirrors: Should_Accept_GitHubToken_Option
		opts := copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{Token: "gho_test_token"}}
		if opts.GitHub.Token != "gho_test_token" {
			t.Errorf("Expected GitHubToken=%q, got %q", "gho_test_token", opts.GitHub.Token)
		}
	})

	t.Run("should default UseLoggedInUser to nil", func(t *testing.T) {
		// Mirrors: Should_Default_UseLoggedInUser_To_Null
		opts := copilot.ClientOptions{Mode: copilot.ModeCodingAgent}
		if opts.GitHub.UseLoggedInUser != nil {
			t.Errorf("Expected UseLoggedInUser to be nil by default, got %v", opts.GitHub.UseLoggedInUser)
		}
	})

	t.Run("should allow explicit UseLoggedInUser false", func(t *testing.T) {
		// Mirrors: Should_Allow_Explicit_UseLoggedInUser_False
		opts := copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{UseLoggedInUser: copilot.Bool(false)}}
		if opts.GitHub.UseLoggedInUser == nil || *opts.GitHub.UseLoggedInUser != false {
			t.Errorf("Expected UseLoggedInUser=false, got %v", opts.GitHub.UseLoggedInUser)
		}
	})

	t.Run("should allow explicit UseLoggedInUser true with GitHubToken", func(t *testing.T) {
		// Mirrors: Should_Allow_Explicit_UseLoggedInUser_True_With_GitHubToken
		opts := copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{Token: "gho_test_token", UseLoggedInUser: copilot.Bool(true)}}
		if opts.GitHub.UseLoggedInUser == nil || *opts.GitHub.UseLoggedInUser != true {
			t.Errorf("Expected UseLoggedInUser=true, got %v", opts.GitHub.UseLoggedInUser)
		}
		if opts.GitHub.Token != "gho_test_token" {
			t.Errorf("Expected GitHubToken=%q, got %q", "gho_test_token", opts.GitHub.Token)
		}
	})

	t.Run("should panic when GitHubToken used with TCPConnection", func(t *testing.T) {
		assertPanics(t, func() {
			_ = copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{Token: "gho_test_token"}, Runtime: copilot.TCPConnection{Host: "localhost", Port: 8080}})
		})
	})

	t.Run("should panic when UseLoggedInUser used with TCPConnection", func(t *testing.T) {
		assertPanics(t, func() {
			_ = copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{UseLoggedInUser: copilot.Bool(false)}, Runtime: copilot.TCPConnection{Host: "localhost", Port: 8080}})
		})
	})

	t.Run("should default SessionIdleTimeoutSeconds to zero", func(t *testing.T) {
		// Mirrors: Should_Default_SessionIdleTimeoutSeconds_To_Null
		// Go uses int (no nullable wrapper); the zero value is 0 and is
		// treated as "unset" by the SDK (no --session-idle-timeout flag).
		connection := copilot.SpawnedStdioConnection{}
		if connection.SessionIdleTimeoutSeconds != 0 {
			t.Errorf("Expected SessionIdleTimeoutSeconds=0 by default, got %d", connection.SessionIdleTimeoutSeconds)
		}
	})

	t.Run("should accept SessionIdleTimeoutSeconds option", func(t *testing.T) {
		// Mirrors: Should_Accept_SessionIdleTimeoutSeconds_Option
		opts := copilot.ClientOptions{Mode: copilot.ModeCodingAgent, Runtime: copilot.SpawnedStdioConnection{SessionIdleTimeoutSeconds: 600}}
		if opts.Runtime.(copilot.SpawnedStdioConnection).SessionIdleTimeoutSeconds != 600 {
			t.Errorf("Expected SessionIdleTimeoutSeconds=600, got %d", opts.Runtime.(copilot.SpawnedStdioConnection).SessionIdleTimeoutSeconds)
		}
	})
}

func getAvailableTCPPort(t *testing.T) int {
	t.Helper()
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Failed to listen on a free TCP port: %v", err)
	}
	defer listener.Close()
	return listener.Addr().(*net.TCPAddr).Port
}

func assertPanics(t *testing.T, fn func()) {
	t.Helper()
	defer func() {
		if r := recover(); r == nil {
			t.Error("Expected the function to panic, but it did not")
		}
	}()
	fn()
}

func containsStringE(slice []string, s string) bool {
	for _, v := range slice {
		if v == s {
			return true
		}
	}
	return false
}

func assertArgValue(t *testing.T, args []string, name, expected string) {
	t.Helper()
	for i, v := range args {
		if v == name {
			if i+1 >= len(args) {
				t.Errorf("Argument %q is missing a value. Args: %v", name, args)
				return
			}
			if args[i+1] != expected {
				t.Errorf("Expected argument %q to have value %q, got %q. Args: %v", name, expected, args[i+1], args)
			}
			return
		}
	}
	t.Errorf("Argument %q was not present. Args: %v", name, args)
}

// capturedCli mirrors the JSON file written by the fake stdio CLI script.
type capturedCli struct {
	Args             []string           `json:"args"`
	WorkingDirectory string             `json:"cwd"`
	Requests         []capturedRequest  `json:"requests"`
	ClientResponses  []capturedResponse `json:"clientResponses"`
	Env              map[string]string  `json:"env"`
}

type capturedRequest struct {
	Method string `json:"method"`
	Params any    `json:"params"`
}

type capturedResponse struct {
	ID     int            `json:"id"`
	Result map[string]any `json:"result"`
}

type extensionLaunchProviderFunc func(
	request *copilot.ExtensionLaunchProviderResolveRequest,
) (*copilot.ExtensionLaunchProviderResolveResult, error)

func (f extensionLaunchProviderFunc) Resolve(
	request *copilot.ExtensionLaunchProviderResolveRequest,
) (*copilot.ExtensionLaunchProviderResolveResult, error) {
	return f(request)
}

func readCapture(t *testing.T, path string) capturedCli {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("Failed to read capture file %q: %v", path, err)
	}
	var c capturedCli
	if err := json.Unmarshal(data, &c); err != nil {
		t.Fatalf("Failed to parse capture file %q: %v\nContent: %s", path, err, string(data))
	}
	return c
}

func getCapturedRequest(t *testing.T, path, method string) capturedRequest {
	t.Helper()
	capture := readCapture(t, path)
	for _, request := range capture.Requests {
		if request.Method == method {
			return request
		}
	}
	t.Fatalf("Expected %s request in capture, got %+v", method, capture.Requests)
	return capturedRequest{}
}

func assertStringArray(t *testing.T, value any, expected []string) {
	t.Helper()
	items, ok := value.([]any)
	if !ok {
		t.Fatalf("Expected string array %v, got %#v", expected, value)
	}
	if len(items) != len(expected) {
		t.Fatalf("Expected string array %v, got %#v", expected, items)
	}
	for i, expectedValue := range expected {
		if items[i] != expectedValue {
			t.Fatalf("Expected string array %v, got %#v", expected, items)
		}
	}
}

// fakeStdioCliScript is intentionally kept close to the fake CLIs used by the
// other SDK client-options E2E tests, while still matching Go's request capture shape.
const fakeStdioCliScript = `
const fs = require("fs");

const captureIndex = process.argv.indexOf("--capture-file");
const captureFile = captureIndex >= 0 ? process.argv[captureIndex + 1] : undefined;
const requests = [];
const clientResponses = [];
let extensionRegistrationId;

function saveCapture() {
  if (!captureFile) {
    return;
  }
  fs.writeFileSync(captureFile, JSON.stringify({
    args: process.argv.slice(2),
    cwd: process.cwd(),
    requests,
    clientResponses,
    env: {
      COPILOT_HOME: process.env.COPILOT_HOME,
      COPILOT_SDK_AUTH_TOKEN: process.env.COPILOT_SDK_AUTH_TOKEN,
      COPILOT_OTEL_ENABLED: process.env.COPILOT_OTEL_ENABLED,
      OTEL_EXPORTER_OTLP_ENDPOINT: process.env.OTEL_EXPORTER_OTLP_ENDPOINT,
      OTEL_EXPORTER_OTLP_PROTOCOL: process.env.OTEL_EXPORTER_OTLP_PROTOCOL,
      COPILOT_OTEL_FILE_EXPORTER_PATH: process.env.COPILOT_OTEL_FILE_EXPORTER_PATH,
      COPILOT_OTEL_EXPORTER_TYPE: process.env.COPILOT_OTEL_EXPORTER_TYPE,
      COPILOT_OTEL_SOURCE_NAME: process.env.COPILOT_OTEL_SOURCE_NAME,
      OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT:
        process.env.OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT,
    },
  }));
}

saveCapture();

let buffer = Buffer.alloc(0);
process.stdin.on("data", chunk => {
  buffer = Buffer.concat([buffer, chunk]);
  processBuffer();
});
process.stdin.resume();

function processBuffer() {
  while (true) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd < 0) return;
    const header = buffer.subarray(0, headerEnd).toString("utf8");
    const match = /Content-Length:\s*(\d+)/i.exec(header);
    if (!match) throw new Error("Missing Content-Length header");
    const length = Number(match[1]);
    const bodyStart = headerEnd + 4;
    const bodyEnd = bodyStart + length;
    if (buffer.length < bodyEnd) return;
    const body = buffer.subarray(bodyStart, bodyEnd).toString("utf8");
    buffer = buffer.subarray(bodyEnd);
    handleMessage(JSON.parse(body));
  }
}

function handleMessage(message) {
  if (!Object.prototype.hasOwnProperty.call(message, "id")) {
    return;
  }
  if (!message.method) {
    clientResponses.push(message);
    saveCapture();
    if (message.id === 9001 && extensionRegistrationId !== undefined) {
      writeResponse(extensionRegistrationId, {});
      extensionRegistrationId = undefined;
    }
    return;
  }
  requests.push({ method: message.method, params: message.params });
  saveCapture();
  if (message.method === "connect") {
    writeResponse(message.id, { ok: true, protocolVersion: 4, version: "fake" });
    return;
  }
  if (message.method === "ping") {
    writeResponse(message.id, { message: "pong", protocolVersion: 4, timestamp: Date.now() });
    return;
  }
  if (message.method === "registerExtensionLaunchProvider") {
    extensionRegistrationId = message.id;
    writeRequest(9001, "extensionLaunchProvider.resolve", {
      id: "project:go-e2e",
      name: "go-e2e",
      modulePath: "/extensions/go-e2e.go",
      source: "project",
    });
    return;
  }
  if (message.method === "session.create" || message.method === "session.resume") {
    const sessionId = (message.params && message.params.sessionId) || "fake-session";
    writeResponse(message.id, { sessionId, workspacePath: null, capabilities: null });
    return;
  }
  if (message.method === "session.detach") {
    writeResponse(message.id, { success: true });
    return;
  }
  if (message.method === "session.resume") {
    const sessionId = (message.params && message.params.sessionId) || "fake-session";
    writeResponse(message.id, { sessionId, workspacePath: null, capabilities: null });
    return;
  }
  writeResponse(message.id, {});
}

function writeResponse(id, result) {
  const body = JSON.stringify({ jsonrpc: "2.0", id, result });
  process.stdout.write("Content-Length: " + Buffer.byteLength(body, "utf8") + "\r\n\r\n" + body);
}

function writeRequest(id, method, params) {
  const body = JSON.stringify({ jsonrpc: "2.0", id, method, params });
  process.stdout.write("Content-Length: " + Buffer.byteLength(body, "utf8") + "\r\n\r\n" + body);
}
`
