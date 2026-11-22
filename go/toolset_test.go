/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package copilot

import (
	"fmt"
	"reflect"
	"slices"
	"strings"
	"testing"
)

func TestToolSet_emitsSourceQualifiedStrings(t *testing.T) {
	items := NewToolSet().
		AddBuiltIn("bash").
		AddBuiltIn("*").
		AddCustom("my_tool").
		AddCustom("*").
		AddMCP("github-list_issues").
		AddMCP("*").
		ToSlice()
	want := []string{
		"builtin:bash",
		"builtin:*",
		"custom:my_tool",
		"custom:*",
		"mcp:github-list_issues",
		"mcp:*",
	}
	if !reflect.DeepEqual(items, want) {
		t.Errorf("got %v, want %v", items, want)
	}
}

func TestToolSet_addBuiltInVariadic(t *testing.T) {
	items := NewToolSet().AddBuiltIn("bash", "view").ToSlice()
	want := []string{"builtin:bash", "builtin:view"}
	if !reflect.DeepEqual(items, want) {
		t.Errorf("got %v, want %v", items, want)
	}
}

func TestToolSet_toSliceReturnsDefensiveCopy(t *testing.T) {
	set := NewToolSet().AddBuiltIn("bash")
	a := set.ToSlice()
	a[0] = "builtin:tampered"
	if got := set.ToSlice(); !reflect.DeepEqual(got, []string{"builtin:bash"}) {
		t.Errorf("internal state mutated: %v", got)
	}
}

func TestToolSet_rejectsInvalidNames(t *testing.T) {
	cases := []struct {
		name string
		fn   func()
	}{
		{"colon in builtin", func() { NewToolSet().AddBuiltIn("has:colon") }},
		{"space in mcp", func() { NewToolSet().AddMCP("has space") }},
		{"empty custom", func() { NewToolSet().AddCustom("") }},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			defer func() {
				if r := recover(); r == nil {
					t.Fatal("expected panic, got none")
				}
			}()
			c.fn()
		})
	}
}

func TestBuiltInToolsIsolated_membership(t *testing.T) {
	for _, banned := range []string{"bash", "edit", "grep", "web_fetch"} {
		if slices.Contains(BuiltInToolsIsolated, banned) {
			t.Errorf("isolated set must not contain %q", banned)
		}
	}
	for _, expected := range []string{"ask_user", "task_complete"} {
		if !slices.Contains(BuiltInToolsIsolated, expected) {
			t.Errorf("isolated set must contain %q", expected)
		}
	}
}

func TestNewClient_modeEmptyRejectsWithoutStorage(t *testing.T) {
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic, got none")
		}
		msg, ok := r.(string)
		if !ok {
			t.Fatalf("expected string panic, got %T", r)
		}
		if !strings.Contains(strings.ToLower(msg), "empty") {
			t.Errorf("panic message should mention empty mode, got %q", msg)
		}
	}()
	NewClient(&ClientOptions{Mode: ModeEmpty})
}

func TestNewClient_modeEmptyAcceptsBaseDirectory(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	if c.options.Mode != ModeEmpty {
		t.Errorf("expected ModeEmpty, got %q", c.options.Mode)
	}
}

func TestNewClient_modeEmptyAcceptsTCPConnection(t *testing.T) {
	c := NewClient(&ClientOptions{
		Mode:    ModeEmpty,
		Runtime: TCPConnection{Host: "localhost", Port: 8080},
	})
	if c.options.Mode != ModeEmpty {
		t.Errorf("expected ModeEmpty, got %q", c.options.Mode)
	}
}

func TestNewClient_requiresExplicitMode(t *testing.T) {
	for _, opts := range []*ClientOptions{nil, {}, {Mode: "copilot-cli"}, {Mode: "invalid"}} {
		t.Run(fmt.Sprintf("%v", opts), func(t *testing.T) {
			defer func() {
				if got := recover(); got != "ClientOptions.Mode is required and must be ModeCodingAgent or ModeEmpty" {
					t.Errorf("expected explicit-mode panic, got %v", got)
				}
			}()
			NewClient(opts)
		})
	}
}

func TestNewClient_modeCodingAgent(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	if c.options.Mode != ModeCodingAgent {
		t.Errorf("expected coding-agent mode, got %q", c.options.Mode)
	}
}

func TestValidateToolFilterList_rejectsBareWildcard(t *testing.T) {
	err := validateToolFilterList("availableTools", []string{"builtin:bash", "*"})
	if err == nil {
		t.Fatal("expected error for bare wildcard")
	}
	if !strings.Contains(err.Error(), "bare wildcard") {
		t.Errorf("expected message about bare wildcard, got %q", err.Error())
	}
}

func TestValidateToolFilterList_allowsSourceQualifiedWildcards(t *testing.T) {
	if err := validateToolFilterList("availableTools", []string{"builtin:*", "mcp:*", "custom:*"}); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestResolveToolFilterOptions_emptyModeRequiresAvailableTools(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	_, _, err := c.resolveToolFilterOptions(nil, nil)
	if err == nil {
		t.Fatal("expected error in empty mode without available tools")
	}
}

func TestResolveToolFilterOptions_preservesBothLists(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	avail, excl, err := c.resolveToolFilterOptions([]string{"builtin:*"}, []string{"builtin:bash"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !reflect.DeepEqual(avail, []string{"builtin:*"}) {
		t.Errorf("expected availableTools [builtin:*], got %v", avail)
	}
	if len(excl) != 1 || excl[0] != "builtin:bash" {
		t.Errorf("expected excludedTools [builtin:bash], got %v", excl)
	}
}

func TestSystemMessageForMode_emptyModeStripsEnvContextWhenNil(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	got := c.systemMessageForMode(nil)
	if got == nil || got.Mode != "customize" {
		t.Fatalf("expected customize mode, got %+v", got)
	}
	if action, ok := got.Sections["environment_context"]; !ok || action.Action != SectionActionRemove {
		t.Errorf("expected environment_context: remove, got %+v", got.Sections)
	}
}

func TestSystemMessageForMode_emptyModePromotesAppendToCustomize(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	got := c.systemMessageForMode(&SystemMessageConfig{Mode: "append", Content: "extra"})
	if got.Mode != "customize" {
		t.Errorf("expected customize, got %q", got.Mode)
	}
	if got.Content != "extra" {
		t.Errorf("expected content preserved, got %q", got.Content)
	}
	if action, ok := got.Sections["environment_context"]; !ok || action.Action != SectionActionRemove {
		t.Errorf("expected environment_context removed")
	}
}

func TestSystemMessageForMode_emptyModePreservesReplace(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	in := &SystemMessageConfig{Mode: "replace", Content: "whole prompt"}
	got := c.systemMessageForMode(in)
	if got != in {
		t.Errorf("expected verbatim passthrough for replace, got %+v", got)
	}
}

func TestSystemMessageForMode_emptyModeRespectsCallerSection(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	in := &SystemMessageConfig{
		Mode: "customize",
		Sections: map[string]SectionOverride{
			"environment_context": {Action: SectionActionReplace, Content: "custom"},
		},
	}
	got := c.systemMessageForMode(in)
	if got != in {
		t.Errorf("expected caller's section override preserved verbatim")
	}
}

func TestSystemMessageForMode_codingAgentPassthrough(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	in := &SystemMessageConfig{Mode: "append", Content: "x"}
	got := c.systemMessageForMode(in)
	if got != in {
		t.Errorf("non-empty mode must not alter system message")
	}
}

func TestApplyConfigDefaultsForMode_emptyDefaultsTelemetryFalse(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.EnableSessionTelemetry == nil || *cfg.EnableSessionTelemetry != false {
		t.Errorf("expected telemetry default false in empty mode, got %v", cfg.EnableSessionTelemetry)
	}
}

func TestApplyConfigDefaultsForMode_emptyDefaultsExperimentalModeFalse(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.EnableExperimentalMode == nil || *cfg.EnableExperimentalMode != false {
		t.Errorf("expected experimental mode default false in empty mode, got %v", cfg.EnableExperimentalMode)
	}
}

func TestApplyConfigDefaultsForMode_codingAgentLeavesExperimentalModeNil(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.EnableExperimentalMode != nil {
		t.Errorf("non-empty mode must not default experimental mode")
	}
}

func TestApplyConfigDefaultsForMode_emptyHonorsCallerTelemetry(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	trueVal := true
	cfg := (&CreateSessionOptions{GitHub: SessionGitHubOptions{TelemetryEnabled: &trueVal}}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.EnableSessionTelemetry == nil || *cfg.EnableSessionTelemetry != true {
		t.Errorf("caller-supplied telemetry must win")
	}
}

func TestApplyConfigDefaultsForMode_codingAgentLeavesNil(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.EnableSessionTelemetry != nil {
		t.Errorf("non-empty mode must not default telemetry")
	}
}

func TestApplyConfigDefaultsForMode_emptyDefaultsGranularFlags(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.SkipEmbeddingRetrieval == nil || *cfg.SkipEmbeddingRetrieval != true {
		t.Errorf("expected SkipEmbeddingRetrieval=true in empty mode, got %v", cfg.SkipEmbeddingRetrieval)
	}
	if cfg.EmbeddingCacheStorage == nil || *cfg.EmbeddingCacheStorage != *String("in-memory") {
		t.Errorf("expected EmbeddingCacheStorage=in-memory in empty mode, got %v", cfg.EmbeddingCacheStorage)
	}
	if cfg.EnableOnDemandInstructionDiscovery == nil || *cfg.EnableOnDemandInstructionDiscovery != false {
		t.Errorf("expected EnableOnDemandInstructionDiscovery=false in empty mode, got %v", cfg.EnableOnDemandInstructionDiscovery)
	}
	if cfg.EnableFileHooks == nil || *cfg.EnableFileHooks != false {
		t.Errorf("expected EnableFileHooks=false in empty mode, got %v", cfg.EnableFileHooks)
	}
	if cfg.EnableHostGitOperations == nil || *cfg.EnableHostGitOperations != false {
		t.Errorf("expected EnableHostGitOperations=false in empty mode, got %v", cfg.EnableHostGitOperations)
	}
	if cfg.EnableSessionStore == nil || *cfg.EnableSessionStore != false {
		t.Errorf("expected EnableSessionStore=false in empty mode, got %v", cfg.EnableSessionStore)
	}
	if cfg.EnableSkills == nil || *cfg.EnableSkills != false {
		t.Errorf("expected EnableSkills=false in empty mode, got %v", cfg.EnableSkills)
	}
	if cfg.Memory == nil || cfg.Memory.Enabled != false {
		t.Errorf("expected Memory.Enabled=false in empty mode, got %v", cfg.Memory)
	}
	if cfg.CustomAgentsLocalOnly == nil || !*cfg.CustomAgentsLocalOnly {
		t.Errorf("expected CustomAgentsLocalOnly=true in empty mode, got %v", cfg.CustomAgentsLocalOnly)
	}
}

func TestApplyConfigDefaultsForMode_emptyHonorsCallerGranularFlags(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	falseVal := false
	trueVal := true
	cfg := (&CreateSessionOptions{State: SessionStateOptions{SkipEmbeddingRetrieval: &falseVal, EmbeddingCacheStorage: String("persistent"), EnableCrossSessionStore: &trueVal, Memory: &MemoryConfiguration{Enabled: true}}, Instructions: CreateSessionInstructionOptions{OnDemandDiscovery: &trueVal}, Hooks: SessionHookOptions{EnableRepositoryHooks: &trueVal}, Git: SessionGitOptions{EnableHostGitOperations: &trueVal}, Skills: SessionSkillOptions{Enabled: &trueVal}, Agents: SessionAgentOptions{LocalDiscoveryOnly: &falseVal}}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if *cfg.SkipEmbeddingRetrieval != false {
		t.Errorf("caller-supplied SkipEmbeddingRetrieval must win")
	}
	if *cfg.EmbeddingCacheStorage != *String("persistent") {
		t.Errorf("caller-supplied EmbeddingCacheStorage must win")
	}
	if *cfg.EnableOnDemandInstructionDiscovery != true {
		t.Errorf("caller-supplied EnableOnDemandInstructionDiscovery must win")
	}
	if *cfg.EnableFileHooks != true {
		t.Errorf("caller-supplied EnableFileHooks must win")
	}
	if *cfg.EnableHostGitOperations != true {
		t.Errorf("caller-supplied EnableHostGitOperations must win")
	}
	if *cfg.EnableSessionStore != true {
		t.Errorf("caller-supplied EnableSessionStore must win")
	}
	if *cfg.EnableSkills != true {
		t.Errorf("caller-supplied EnableSkills must win")
	}
	if cfg.Memory == nil || cfg.Memory.Enabled != true {
		t.Errorf("caller-supplied Memory must win")
	}
	if cfg.CustomAgentsLocalOnly == nil || *cfg.CustomAgentsLocalOnly {
		t.Errorf("caller-supplied CustomAgentsLocalOnly must win")
	}
}

func TestApplyConfigDefaultsForMode_codingAgentLeavesGranularFlagsNil(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.SkipEmbeddingRetrieval != nil {
		t.Errorf("non-empty mode must not default SkipEmbeddingRetrieval")
	}
	if cfg.EnableOnDemandInstructionDiscovery != nil {
		t.Errorf("non-empty mode must not default EnableOnDemandInstructionDiscovery")
	}
	if cfg.EnableFileHooks != nil {
		t.Errorf("non-empty mode must not default EnableFileHooks")
	}
	if cfg.EnableHostGitOperations != nil {
		t.Errorf("non-empty mode must not default EnableHostGitOperations")
	}
	if cfg.EnableSessionStore != nil {
		t.Errorf("non-empty mode must not default EnableSessionStore")
	}
	if cfg.EnableSkills != nil {
		t.Errorf("non-empty mode must not default EnableSkills")
	}
	if cfg.Memory != nil {
		t.Errorf("non-empty mode must not default Memory")
	}
	if cfg.CustomAgentsLocalOnly != nil {
		t.Errorf("non-empty mode must not default CustomAgentsLocalOnly")
	}
}

func TestApplyResumeDefaultsForMode_customAgentsLocalOnly(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})

	cfg := (&ResumeSessionOptions{}).normalize()
	c.applyResumeDefaultsForMode(cfg)
	if cfg.CustomAgentsLocalOnly == nil || !*cfg.CustomAgentsLocalOnly {
		t.Errorf("expected CustomAgentsLocalOnly=true in empty mode, got %v", cfg.CustomAgentsLocalOnly)
	}

	cfg = (&ResumeSessionOptions{Agents: SessionAgentOptions{LocalDiscoveryOnly: Bool(false)}}).normalize()
	c.applyResumeDefaultsForMode(cfg)
	if cfg.CustomAgentsLocalOnly == nil || *cfg.CustomAgentsLocalOnly {
		t.Errorf("caller-supplied CustomAgentsLocalOnly must win")
	}
}

func TestApplyConfigDefaultsForMode_emptyDefaultsMCPOAuthTokenStorage(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.MCPOAuthTokenStorage != "in-memory" {
		t.Errorf("expected MCPOAuthTokenStorage 'in-memory' in empty mode, got %q", cfg.MCPOAuthTokenStorage)
	}
}

func TestApplyConfigDefaultsForMode_emptyHonorsCallerMCPOAuthTokenStorage(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&CreateSessionOptions{MCP: SessionMCPOptions{OAuthTokenStorage: "persistent"}}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.MCPOAuthTokenStorage != "persistent" {
		t.Errorf("caller-supplied MCPOAuthTokenStorage must win, got %q", cfg.MCPOAuthTokenStorage)
	}
}

func TestApplyConfigDefaultsForMode_codingAgentLeavesMCPOAuthTokenStorageEmpty(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	cfg := (&CreateSessionOptions{}).normalize()
	c.applyConfigDefaultsForMode(cfg)
	if cfg.MCPOAuthTokenStorage != "" {
		t.Errorf("non-empty mode must not default MCPOAuthTokenStorage, got %q", cfg.MCPOAuthTokenStorage)
	}
}

func TestApplyResumeDefaultsForMode_emptyDefaultsExperimentalModeFalse(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeEmpty, Runtime: SpawnedStdioConnection{BaseDirectory: t.TempDir()}})
	cfg := (&ResumeSessionOptions{}).normalize()
	c.applyResumeDefaultsForMode(cfg)
	if cfg.EnableExperimentalMode == nil || *cfg.EnableExperimentalMode != false {
		t.Errorf("expected experimental mode default false in empty mode, got %v", cfg.EnableExperimentalMode)
	}
}

func TestApplyResumeDefaultsForMode_codingAgentLeavesExperimentalModeNil(t *testing.T) {
	c := NewClient(&ClientOptions{Mode: ModeCodingAgent})
	cfg := (&ResumeSessionOptions{}).normalize()
	c.applyResumeDefaultsForMode(cfg)
	if cfg.EnableExperimentalMode != nil {
		t.Errorf("non-empty mode must not default experimental mode")
	}
}
