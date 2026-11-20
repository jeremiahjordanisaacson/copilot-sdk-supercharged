// Copyright (c) Microsoft Corporation. All rights reserved.

package rpc

import (
	"context"
	"encoding/json"
	"net"
	"strconv"
	"testing"
	"time"

	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
)

// wirePath returns the JSON at path inside raw, where a numeric segment indexes an array.
func wirePath(t *testing.T, raw json.RawMessage, path ...string) (string, bool) {
	t.Helper()
	value := raw
	for _, segment := range path {
		if index, err := strconv.Atoi(segment); err == nil {
			var items []json.RawMessage
			if err := json.Unmarshal(value, &items); err != nil || index >= len(items) {
				return "", false
			}
			value = items[index]
			continue
		}
		var fields map[string]json.RawMessage
		if err := json.Unmarshal(value, &fields); err != nil {
			return "", false
		}
		next, ok := fields[segment]
		if !ok {
			return "", false
		}
		value = next
	}
	return string(value), true
}

func wireString(s string) *string { return &s }

type wirePathWant struct {
	path []string
	want string // "" means the key is absent
}

func checkWirePaths(t *testing.T, raw json.RawMessage, wants []wirePathWant) {
	t.Helper()
	for _, want := range wants {
		got, ok := wirePath(t, raw, want.path...)
		if want.want == "" {
			if ok {
				t.Errorf("%v = %s, want it absent; params %s", want.path, got, raw)
			}
			continue
		}
		if !ok || got != want.want {
			t.Errorf("%v = %s (present %v), want %s; params %s", want.path, got, ok, want.want, raw)
		}
	}
}

func emptyAt(path ...string) wirePathWant { return wirePathWant{path: path, want: "[]"} }

// A required list inside a value the request holds, left nil, for every kind of container a
// public request uses to hold it.
func TestNestedRequiredListsNilSentAsEmpty(t *testing.T) {
	for _, test := range []struct {
		name   string
		method string
		wants  []wirePathWant
		call   wireCall
	}{
		{"permission rules approved", "session.permissions.configure", []wirePathWant{emptyAt("rules", "approved"), {[]string{"rules", "denied", "0", "kind"}, `"shell"`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{Rules: &PermissionRulesSet{Denied: []PermissionRule{{Kind: "shell", Argument: wireString("rm")}}}})
			return err
		}},
		{"permission rules denied", "session.permissions.configure", []wirePathWant{emptyAt("rules", "denied")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{Rules: &PermissionRulesSet{Approved: []PermissionRule{}}})
			return err
		}},
		{"authority grant statements", "session.permissions.configure", []wirePathWant{emptyAt("authenticatedAuthority", "grant", "statements")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{AuthenticatedAuthority: &AuthenticatedAuthorityUpdate{
				Source: "host", Revision: 1, Grant: &AuthenticatedAuthorityGrant{GrantID: "g", Source: "host", UserMessages: []string{"m"}},
			}})
			return err
		}},
		{"content exclusion policy rules", "session.permissions.configure", []wirePathWant{emptyAt("additionalContentExclusionPolicies", "0", "rules")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{AdditionalContentExclusionPolicies: []PermissionsConfigureAdditionalContentExclusionPolicy{
				{Scope: PermissionsConfigureAdditionalContentExclusionPolicyScopeAll},
			}})
			return err
		}},
		{"content exclusion rule paths", "session.permissions.configure", []wirePathWant{emptyAt("additionalContentExclusionPolicies", "0", "rules", "0", "paths")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{AdditionalContentExclusionPolicies: []PermissionsConfigureAdditionalContentExclusionPolicy{
				{Rules: []PermissionsConfigureAdditionalContentExclusionPolicyRule{{}}, Scope: PermissionsConfigureAdditionalContentExclusionPolicyScopeAll},
			}})
			return err
		}},
		{"options update policy rules", "session.options.update", []wirePathWant{emptyAt("policy", "additionalContentExclusionPolicies", "0", "rules")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*OptionsAPI)(ses).Update(ctx, &SessionUpdateOptionsParams{Policy: &SessionUpdatePolicyOptions{AdditionalContentExclusionPolicies: []OptionsUpdateAdditionalContentExclusionPolicy{{}}}})
			return err
		}},
		{"options update masked env var hosts", "session.options.update", []wirePathWant{emptyAt("permissions", "sandbox", "credentials", "envVars", "TOKEN", "injectHosts")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*OptionsAPI)(ses).Update(ctx, &SessionUpdateOptionsParams{Permissions: &SessionUpdatePermissionOptions{Sandbox: &SandboxConfig{Credentials: &SandboxCredentialsConfig{EnvVars: map[string]SandboxMaskedEnvVar{"TOKEN": {}}}}}})
			return err
		}},
		{"open create value variant", "sessions.open", []wirePathWant{{[]string{"kind"}, `"create"`}, emptyAt("options", "policy", "additionalContentExclusionPolicies", "0", "rules")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			var params SessionOpenParams = SessionsOpenCreate{Options: &SessionOpenOptions{Policy: &SessionOpenPolicyOptions{AdditionalContentExclusionPolicies: []SessionOpenOptionsAdditionalContentExclusionPolicy{{}}}}}
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, params)
			return err
		}},
		{"open handoff pointer variant", "sessions.open", []wirePathWant{{[]string{"kind"}, `"handoff"`}, emptyAt("metadata", "remoteSessionIds")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			var params SessionOpenParams = &SessionsOpenHandoff{}
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, params)
			return err
		}},
		{"open resume masked env var hosts", "sessions.open", []wirePathWant{emptyAt("options", "permissions", "sandbox", "credentials", "envVars", "TOKEN", "injectHosts")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			var params SessionOpenParams = SessionsOpenResume{SessionID: "s1", Options: &SessionOpenOptions{Permissions: &SessionOpenPermissionOptions{Sandbox: &SandboxConfig{Credentials: &SandboxCredentialsConfig{EnvVars: map[string]SandboxMaskedEnvVar{"TOKEN": {}}}}}}}
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, params)
			return err
		}},
		{"proxy CA masked env var hosts", "sandbox.proxyCa.create", []wirePathWant{emptyAt("sandboxConfig", "credentials", "envVars", "TOKEN", "injectHosts")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerSandboxProxyCaAPI)(srv).Create(ctx, &SandboxProxyCaRequest{SandboxConfig: &SandboxConfig{Credentials: &SandboxCredentialsConfig{EnvVars: map[string]SandboxMaskedEnvVar{"TOKEN": {}}}}})
			return err
		}},
		{"catalog contract capabilities", "catalog.search", []wirePathWant{emptyAt("contract", "requiredCapabilities")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerCatalogAPI)(srv).Search(ctx, &CatalogSearchRequest{Query: "q"})
			return err
		}},
		{"skill installations contract capabilities", "skills.installations.list", []wirePathWant{emptyAt("contract", "requiredCapabilities")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerSkillsInstallationsAPI)(srv).List(ctx, &SkillInstallationsRequest{PolicySessionID: "s1"})
			return err
		}},
		{"decision questions in a slice of a union", "decisions.ask", []wirePathWant{emptyAt("questions", "0", "options"), emptyAt("questions", "1", "levels")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerDecisionsAPI)(srv).Ask(ctx, &DecisionsAskRequest{
				Context:   DecisionContextText{Text: "t"},
				Questions: []DecisionQuestion{DecisionQuestionChoice{Name: "a"}, &DecisionQuestionScore{Name: "b"}},
			})
			return err
		}},
		{"read-only directory decision", "session.permissions.handlePendingPermissionRequest", []wirePathWant{emptyAt("result", "directories")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: PermissionDecisionApproveReadOnlyForSession{}})
			return err
		}},
		{"session approval of commands", "session.permissions.handlePendingPermissionRequest", []wirePathWant{emptyAt("result", "approval", "commandIdentifiers")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: PermissionDecisionApproveForSession{Approval: PermissionDecisionApproveForSessionApprovalCommands{}}})
			return err
		}},
		{"location approval of environment variables", "session.permissions.handlePendingPermissionRequest", []wirePathWant{emptyAt("result", "approval", "environmentVariables")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: &PermissionDecisionApproveForLocation{
				LocationKey: "l", Approval: &PermissionDecisionApproveForLocationApprovalExtensionEnvAccess{ExtensionName: "e"},
			}})
			return err
		}},
		{"denied by rules decision", "session.permissions.handlePendingPermissionRequest", []wirePathWant{emptyAt("result", "rules")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: PermissionDecisionDeniedByRules{}})
			return err
		}},
		{"location tool approval of commands", "session.permissions.locations.addToolApproval", []wirePathWant{emptyAt("approval", "commandIdentifiers")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsLocationsAPI)(ses).AddToolApproval(ctx, &PermissionLocationAddToolApprovalParams{LocationKey: "l", Approval: PermissionsLocationsAddToolApprovalDetailsCommands{}})
			return err
		}},
		{"discovered model lists", "session.providers.models.prepareConfiguration", []wirePathWant{emptyAt("model", "warnings"), emptyAt("model", "capabilities", "limits", "vision", "supported_media_types")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ProvidersModelsAPI)(ses).PrepareConfiguration(ctx, &ModelProviderPrepareConfigurationRequest{Model: DiscoveredModel{
				ID: "m", Capabilities: ModelCapabilities{Limits: &ModelCapabilitiesLimits{Vision: &ModelCapabilitiesLimitsVision{}}},
			}})
			return err
		}},
		{"shell descriptor lines", "session.tools.getBuiltinDescriptors", []wirePathWant{emptyAt("shellConfig", "descriptionLines")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ToolsAPI)(ses).GetBuiltinDescriptors(ctx, &ToolsGetBuiltinDescriptorsRequest{ShellConfig: &ToolsShellDescriptorConfig{ShellType: "bash"}})
			return err
		}},
		{"elicitation fields in a map of a union", "session.ui.elicitation", []wirePathWant{
			emptyAt("requestedSchema", "properties", "a", "enum"),
			emptyAt("requestedSchema", "properties", "b", "oneOf"),
			emptyAt("requestedSchema", "properties", "c", "items", "enum"),
			emptyAt("requestedSchema", "properties", "d", "items", "anyOf"),
		}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*UIAPI)(ses).Elicitation(ctx, &UIElicitationRequest{Message: "m", RequestedSchema: UIElicitationSchema{Properties: map[string]UIElicitationSchemaProperty{
				"a": UIElicitationStringEnumField{},
				"b": &UIElicitationStringOneOfField{},
				"c": UIElicitationArrayEnumField{},
				"d": UIElicitationArrayAnyOfField{},
			}}})
			return err
		}},
		{"previous trajectory result", "taskAnalytics.classifyTrajectory", []wirePathWant{emptyAt("previousResult", "classifications"), emptyAt("previousResult", "validation", "errors")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerTaskAnalyticsAPI)(srv).ClassifyTrajectory(ctx, &TaskAnalyticsClassifyTrajectoryRequest{Events: []any{}, PreviousResult: &TaskAnalyticsClassifyTrajectoryResult{Validation: &TaskAnalyticsTrajectoryValidation{}}})
			return err
		}},
		// These two types already write [] from their own MarshalJSON; the bytes stay the same.
		{"feature flag patch keys", "session.featureFlags.update", []wirePathWant{emptyAt("configPatch", "undefinedKeys")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*FeatureFlagsAPI)(ses).Update(ctx, &SessionFeatureFlagsUpdateRequest{ConfigPatch: &FeatureFlagConfigPatch{Values: map[string]any{}}})
			return err
		}},
		{"skill removal inventory", "skills.removeSource", []wirePathWant{emptyAt("inventory", "personalSkillsDirs"), emptyAt("inventory", "skills")}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerSkillsAPI)(srv).RemoveSource(ctx, &SkillsRemoveSourceRequest{Cwd: "/w", Target: "t", SkillDirectories: []string{}, Inventory: &SkillsRemoveSourceInventory{}})
			return err
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			checkWirePaths(t, captureParams(t, test.method, test.call), test.wants)
		})
	}
}

// Rows the nested coercion must leave as they are.
func TestNestedRequestWireUnchanged(t *testing.T) {
	for _, test := range []struct {
		name   string
		method string
		wants  []wirePathWant
		call   wireCall
	}{
		{"nullable pointer in a nested list", "session.permissions.configure", []wirePathWant{{[]string{"rules", "denied", "0", "argument"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{Rules: &PermissionRulesSet{Approved: []PermissionRule{}, Denied: []PermissionRule{{Kind: "memory"}}}})
			return err
		}},
		{"empty and populated nested lists", "session.permissions.configure", []wirePathWant{
			{[]string{"rules", "approved"}, `[]`},
			{[]string{"rules", "denied"}, `[{"argument":"git status","kind":"shell"}]`},
		}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{Rules: &PermissionRulesSet{Approved: []PermissionRule{}, Denied: []PermissionRule{{Kind: "shell", Argument: wireString("git status")}}}})
			return err
		}},
		{"optional lists in nested values", "session.permissions.configure", []wirePathWant{
			{[]string{"additionalContentExclusionPolicies", "0", "rules", "0", "ifAnyMatch"}, ""},
			{[]string{"additionalContentExclusionPolicies", "0", "rules", "0", "ifNoneMatch"}, ""},
			{[]string{"additionalContentExclusionPolicies", "0", "rules", "0", "paths"}, `["a"]`},
			{[]string{"additionalContentExclusionPolicies", "0", "last_updated_at"}, `null`},
			{[]string{"authenticatedAuthority", "grant", "userMessages"}, ""},
			{[]string{"authenticatedAuthority", "grant", "statements"}, `["s"]`},
			{[]string{"authenticatedAuthority", "grant", "scope"}, `null`},
		}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{
				AdditionalContentExclusionPolicies: []PermissionsConfigureAdditionalContentExclusionPolicy{{
					Rules: []PermissionsConfigureAdditionalContentExclusionPolicyRule{{Paths: []string{"a"}}}, Scope: PermissionsConfigureAdditionalContentExclusionPolicyScopeAll,
				}},
				AuthenticatedAuthority: &AuthenticatedAuthorityUpdate{Source: "host", Revision: 1, Grant: &AuthenticatedAuthorityGrant{GrantID: "g", Source: "host", Statements: []string{"s"}}},
			})
			return err
		}},
		{"nil optional containers", "session.permissions.configure", []wirePathWant{
			{[]string{"rules"}, ""},
			{[]string{"additionalContentExclusionPolicies"}, ""},
			{[]string{"authenticatedAuthority"}, ""},
		}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{})
			return err
		}},
		{"nil grant in an authority update", "session.permissions.configure", []wirePathWant{{[]string{"authenticatedAuthority", "grant"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{AuthenticatedAuthority: &AuthenticatedAuthorityUpdate{Source: "host", Revision: 1}})
			return err
		}},
		// A null map can mean something an empty one does not, so nested required maps keep null too.
		{"nested required map", "session.options.update", []wirePathWant{{[]string{"permissions", "sandbox", "credentials", "envVars"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*OptionsAPI)(ses).Update(ctx, &SessionUpdateOptionsParams{Permissions: &SessionUpdatePermissionOptions{Sandbox: &SandboxConfig{Credentials: &SandboxCredentialsConfig{}}}})
			return err
		}},
		{"nested required map of a union", "session.ui.elicitation", []wirePathWant{{[]string{"requestedSchema", "properties"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*UIAPI)(ses).Elicitation(ctx, &UIElicitationRequest{Message: "m"})
			return err
		}},
		{"nil union", "session.permissions.handlePendingPermissionRequest", []wirePathWant{{[]string{"result"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r"})
			return err
		}},
		{"raw union variant", "session.permissions.handlePendingPermissionRequest", []wirePathWant{{[]string{"result", "directories"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: RawPermissionDecisionData{
				Discriminator: PermissionDecisionKindApproveReadOnlyForSession,
				Raw:           json.RawMessage(`{"kind":"approve-read-only-for-session","directories":null}`),
			}})
			return err
		}},
		{"union variant without required lists", "session.permissions.handlePendingPermissionRequest", []wirePathWant{{[]string{"result"}, `{"kind":"approve-once"}`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*PermissionsAPI)(ses).HandlePendingPermissionRequest(ctx, &PermissionDecisionRequest{RequestID: "r", Result: PermissionDecisionApproveOnce{}})
			return err
		}},
		{"open nil options", "sessions.open", []wirePathWant{{[]string{"kind"}, `"create"`}, {[]string{"options"}, ""}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			var params SessionOpenParams = SessionsOpenCreate{}
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, params)
			return err
		}},
		{"open nil optional policy list", "sessions.open", []wirePathWant{{[]string{"options", "policy", "additionalContentExclusionPolicies"}, ""}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			var params SessionOpenParams = SessionsOpenCreate{Options: &SessionOpenOptions{Policy: &SessionOpenPolicyOptions{}}}
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, params)
			return err
		}},
		{"open nil params", "sessions.open", []wirePathWant{{nil, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerSessionsAPI)(srv).Open(ctx, nil)
			return err
		}},
		{"proxy CA nil sandbox config", "sandbox.proxyCa.create", []wirePathWant{{nil, `{}`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*ServerSandboxProxyCaAPI)(srv).Create(ctx, &SandboxProxyCaRequest{})
			return err
		}},
		{"internal method nested required list", "mcp.resolveGitHubConfig", []wirePathWant{{[]string{"workspace", "alreadyDisabled"}, `null`}}, func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
			_, err := (*InternalServerMCPAPI)(&internalServerAPI{client: srv.client}).ResolveGitHubConfig(ctx, &MCPResolveGitHubConfigRequest{Workspace: &MCPGitHubWorkspace{}})
			return err
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			checkWirePaths(t, captureParams(t, test.method, test.call), test.wants)
		})
	}
}

func TestNestedRequiredListNilLeavesCallerRequest(t *testing.T) {
	rules := &PermissionRulesSet{}
	grant := &AuthenticatedAuthorityGrant{GrantID: "g", Source: "host"}
	policies := []PermissionsConfigureAdditionalContentExclusionPolicy{{Rules: []PermissionsConfigureAdditionalContentExclusionPolicyRule{{}}}}
	captureParams(t, "session.permissions.configure", func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
		_, err := (*PermissionsAPI)(ses).Configure(ctx, &PermissionsConfigureParams{
			Rules:                              rules,
			AuthenticatedAuthority:             &AuthenticatedAuthorityUpdate{Source: "host", Revision: 1, Grant: grant},
			AdditionalContentExclusionPolicies: policies,
		})
		return err
	})
	if rules.Approved != nil || rules.Denied != nil {
		t.Errorf("caller's rules = %#v, want nil lists", rules)
	}
	if grant.Statements != nil {
		t.Errorf("caller's grant statements = %#v, want nil", grant.Statements)
	}
	if policies[0].Rules[0].Paths != nil {
		t.Errorf("caller's policy rule paths = %#v, want nil", policies[0].Rules[0].Paths)
	}

	envVars := map[string]SandboxMaskedEnvVar{"TOKEN": {}}
	captureParams(t, "sandbox.proxyCa.create", func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
		_, err := (*ServerSandboxProxyCaAPI)(srv).Create(ctx, &SandboxProxyCaRequest{SandboxConfig: &SandboxConfig{Credentials: &SandboxCredentialsConfig{EnvVars: envVars}}})
		return err
	})
	if envVars["TOKEN"].InjectHosts != nil {
		t.Errorf("caller's injectHosts = %#v, want nil", envVars["TOKEN"].InjectHosts)
	}

	handoff := &SessionsOpenHandoff{}
	var open SessionOpenParams = handoff
	captureParams(t, "sessions.open", func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
		_, err := (*ServerSessionsAPI)(srv).Open(ctx, open)
		return err
	})
	if open != handoff || handoff.Metadata.RemoteSessionIDs != nil {
		t.Errorf("caller's open params = %#v, want the same handoff with nil remoteSessionIds", open)
	}

	score := &DecisionQuestionScore{Name: "b"}
	questions := []DecisionQuestion{score}
	captureParams(t, "decisions.ask", func(ctx context.Context, srv *serverAPI, ses *sessionAPI) error {
		_, err := (*ServerDecisionsAPI)(srv).Ask(ctx, &DecisionsAskRequest{Context: DecisionContextText{Text: "t"}, Questions: questions})
		return err
	})
	if questions[0] != score || score.Levels != nil {
		t.Errorf("caller's question = %#v, want the same score question with nil levels", questions[0])
	}
}

// A host that encodes a request type itself still gets what encoding/json writes for it.
func TestNestedRequestTypesMarshalUnchanged(t *testing.T) {
	for _, test := range []struct {
		value any
		want  string
	}{
		{PermissionRulesSet{}, `{"approved":null,"denied":null}`},
		{AuthenticatedAuthorityGrant{}, `{"expiresAt":"","grantId":"","scope":null,"source":"","statements":null}`},
		{SandboxMaskedEnvVar{}, `{"injectHosts":null}`},
		{CatalogClientContract{}, `{"protocolVersion":0,"requiredCapabilities":null}`},
	} {
		got, err := json.Marshal(test.value)
		if err != nil || string(got) != test.want {
			t.Errorf("json.Marshal(%T{}) = %s, %v; want %s", test.value, got, err, test.want)
		}
	}
}

// A client handler's result is sent the way a request is: a nil required list, at any depth, goes
// out as an empty list, and the handler's own value is not modified.
type sqliteTransactionHandler struct {
	SessionFSHandler
	result *SessionFSSqliteTransactionResult
}

func (h sqliteTransactionHandler) SqliteTransaction(*SessionFSSqliteTransactionRequest) (*SessionFSSqliteTransactionResult, error) {
	return h.result, nil
}

func TestClientHandlerResultNilRequiredListsSentAsEmpty(t *testing.T) {
	for _, test := range []struct {
		name   string
		result *SessionFSSqliteTransactionResult
		wants  []wirePathWant
	}{
		{"nested lists", &SessionFSSqliteTransactionResult{Results: []SessionFSSqliteQueryResult{{RowsAffected: 1}}},
			[]wirePathWant{emptyAt("results", "0", "columns"), emptyAt("results", "0", "rows"), {[]string{"results", "0", "rowsAffected"}, "1"}}},
		{"own list", &SessionFSSqliteTransactionResult{}, []wirePathWant{emptyAt("results")}},
		{"filled lists", &SessionFSSqliteTransactionResult{Results: []SessionFSSqliteQueryResult{{Columns: []string{"a"}, Rows: []map[string]any{{"a": float64(1)}}}}},
			[]wirePathWant{{[]string{"results", "0", "columns", "0"}, `"a"`}, {[]string{"results", "0", "rows", "0", "a"}, "1"}}},
	} {
		t.Run(test.name, func(t *testing.T) {
			clientConn, serverConn := net.Pipe()
			defer clientConn.Close()
			defer serverConn.Close()
			client := jsonrpc2.NewClient(clientConn, clientConn)
			runtime := jsonrpc2.NewClient(serverConn, serverConn)
			handlers := &ClientSessionAPIHandlers{SessionFS: sqliteTransactionHandler{result: test.result}}
			RegisterClientSessionAPIHandlers(client, func(string) *ClientSessionAPIHandlers { return handlers })
			client.Start()
			runtime.Start()
			defer client.Stop()
			defer runtime.Stop()
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			raw, err := runtime.Request(ctx, "sessionFs.sqliteTransaction", map[string]any{"sessionId": "s1", "statements": []any{}})
			if err != nil {
				t.Fatal(err)
			}
			checkWirePaths(t, raw, test.wants)
			if test.name == "nested lists" && test.result.Results[0].Columns != nil {
				t.Errorf("handler's result was modified: %#v", test.result.Results[0])
			}
		})
	}
}
