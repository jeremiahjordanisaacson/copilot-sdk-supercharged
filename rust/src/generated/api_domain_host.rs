//! Auto-generated from api.schema.json — do not edit manually.

#![allow(clippy::large_enum_variant)]
#![allow(deprecated)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(rustdoc::invalid_html_tags)]

use std::collections::HashMap;

pub(crate) use lifecycle_diagnostics::LifecycleDiagnosticEvent;
use serde::{Deserialize, Serialize};
pub use session_events::{
    AbortReason, AgentModelPolicy, AssistedApprovalAssessment, AutoTier, BlockedPermissionAction,
    ContextTier, IdeConnectedInfo, IdeSelection, IndexedSearchState, ManagedPermissionsContext,
    ManagedSettingsResolvedSource, McpOauthHttpResponse, McpOauthWWWAuthenticateParams,
    McpServerMetadata, McpServerSource, McpServerStatus, McpSkillOrigin, ModelChangeSource,
    ModelProviderKind, ModelProviderRef, OmittedBinaryOmittedReason, PermissionDecisionSource,
    PermissionMode, PermissionPromptRequest, PermissionRule, PermissionSandboxPathGrant,
    PolicyHelperCapabilities, PolicyHelperFailure, ProviderQuotaState, ReasoningSummary,
    RemediationAction, SessionLimitsConfig, SessionMode, ShutdownType, SkillSource, TaskBlocker,
    TaskCompletionOutcome, UsageGetMetricsResult, UserToolSessionApproval, Verbosity,
};

use super::sdk_api_shared::*;
use crate::generated::{lifecycle_diagnostics, session_events};
use crate::types::{RequestId, SessionEvent, SessionId};

/// Authenticated account entry returned by `account.getAllUsers`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AccountAllUsers {
    /// Authentication information for this user
    pub auth_info: AuthInfo,
    /// Opaque identifier accepted by account and model selection APIs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selection_id: Option<String>,
    /// Associated token, if available
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token: Option<String>,
}

/// The authentication identity whose shared wording should be rendered.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountDescribeAuthInfoRequest {
    /// Identity to describe.
    pub identity: AuthIdentity,
}
crate::record_serde::wire_record!(AccountDescribeAuthInfoRequest {
    identity: "identity"
});

/// The wording the runtime uses for an authentication identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountDescribeAuthInfoResult {
    /// One-line description of the identity, suitable after a verb such as "Switched to".
    pub description: String,
    /// Sign-in banner announcing the authenticated user.
    pub message: String,
}
crate::record_serde::wire_record!(AccountDescribeAuthInfoResult {
    description: "description",
    message: "message"
});

/// Current authentication state
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountGetCurrentAuthResult {
    /// Authentication errors from the last auth attempt, if any
    pub auth_errors: Option<Vec<String>>,
    /// Current authentication information, if authenticated
    pub auth_info: Option<AuthInfo>,
}
crate::record_serde::wire_record!(AccountGetCurrentAuthResult { auth_errors: "authErrors" if is_none, auth_info: "authInfo" if is_none });

/// The authentication identity whose gist support should be reported.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetGistSupportRequest {
    /// Identity to evaluate.
    pub identity: AuthIdentity,
}
crate::record_serde::wire_record!(AccountGetGistSupportRequest {
    identity: "identity"
});

/// Whether the identity can create gists, and why not when it cannot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetGistSupportResult {
    /// Machine-readable reason category when unsupported.
    pub kind: Option<String>,
    /// Human-readable explanation to show where the gist action would have been.
    pub reason: Option<String>,
    /// True when the identity can create gists.
    pub supported: bool,
}
crate::record_serde::wire_record!(AccountGetGistSupportResult { kind: "kind" if is_none, reason: "reason" if is_none, supported: "supported" });

/// Configured names of the GitHub host to resolve.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetHostUrlRequest {
    /// Host from the Copilot-specific setting, which wins when both are set. A bare hostname is normalized to `https://`.
    pub copilot_gh_host: Option<String>,
    /// Host from the general GitHub CLI setting, used when the Copilot-specific one is absent.
    pub gh_host: Option<String>,
}
crate::record_serde::wire_record!(AccountGetHostUrlRequest { copilot_gh_host: "copilotGhHost" if is_none, gh_host: "ghHost" if is_none });

/// Absolute base URL of the GitHub host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetHostUrlResult {
    /// Absolute base URL, for example `https://github.com`.
    pub url: String,
}
crate::record_serde::wire_record!(AccountGetHostUrlResult { url: "url" });

/// Optional opaque account selection or compatibility GitHub token used to look up quota.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountGetQuotaRequest {
    /// GitHub token accepted for compatibility with existing SDK clients. When provided, resolves this token instead of using the current account.
    pub git_hub_token: Option<String>,
    /// Opaque account identifier returned by `account.getAllUsers`. When omitted, the current account is used.
    pub selection_id: Option<String>,
}
crate::record_serde::wire_record!(AccountGetQuotaRequest { git_hub_token: "gitHubToken" if is_none, selection_id: "selectionId" if is_none });

/// Quota usage snapshot for a Copilot quota type, including entitlement, used requests, overage, reset date, and remaining percentage.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountQuotaSnapshot {
    /// Number of requests included in the entitlement, or -1 for unlimited entitlements
    pub entitlement_requests: i64,
    /// Whether the user has an unlimited usage entitlement
    pub is_unlimited_entitlement: bool,
    /// Number of additional usage requests made this period
    pub overage: f64,
    /// Whether additional usage is allowed when quota is exhausted
    pub overage_allowed_with_exhausted_quota: bool,
    /// Percentage of entitlement remaining
    pub remaining_percentage: f64,
    /// Date when the quota resets (ISO 8601 string)
    pub reset_date: Option<String>,
    /// Whether usage is still permitted after quota exhaustion
    pub usage_allowed_with_exhausted_quota: bool,
    /// Number of requests used so far this period
    pub used_requests: i64,
}
crate::record_serde::wire_record!(AccountQuotaSnapshot { entitlement_requests: "entitlementRequests", is_unlimited_entitlement: "isUnlimitedEntitlement", overage: "overage", overage_allowed_with_exhausted_quota: "overageAllowedWithExhaustedQuota", remaining_percentage: "remainingPercentage", reset_date: "resetDate" if is_none, usage_allowed_with_exhausted_quota: "usageAllowedWithExhaustedQuota", used_requests: "usedRequests" });

/// Quota usage snapshots for the resolved user, keyed by quota type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountGetQuotaResult {
    /// Quota snapshots keyed by type (e.g., chat, completions, premium_interactions)
    pub quota_snapshots: HashMap<String, AccountQuotaSnapshot>,
}
crate::record_serde::wire_record!(AccountGetQuotaResult {
    quota_snapshots: "quotaSnapshots"
});

/// The environment variable supplying a classic personal access token, when one is set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetTokenEnvVarResult {
    /// Variable name, for example `GH_TOKEN`. Absent when no classic token is present in the environment.
    pub name: Option<String>,
}
crate::record_serde::wire_record!(AccountGetTokenEnvVarResult { name: "name" if is_none });

/// A path to resolve against the selected GitHub host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetWebUrlRequest {
    /// Opaque selector for a live host-owned credential binding. Cannot be combined with identity.
    pub auth_owner_id: Option<String>,
    /// Identity whose host the path resolves against. Omit both identity and authOwnerId to use the public github.com host.
    pub identity: Option<AuthIdentity>,
    /// Host-relative path, without a leading slash, for example `settings/copilot/features`.
    pub path: String,
}
crate::record_serde::wire_record!(AccountGetWebUrlRequest { auth_owner_id: "authOwnerId" if is_none, identity: "identity" if is_none, path: "path" });

/// An absolute URL on the selected GitHub host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountGetWebUrlResult {
    /// Absolute URL for the requested path.
    pub url: String,
}
crate::record_serde::wire_record!(AccountGetWebUrlResult { url: "url" });

/// The immutable configuration for one pre-session authentication owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ProcessLoginContext {
    /// The original configuration directory. Omission preserves the default root.
    pub config_dir: Option<String>,
    /// The host environment snapshot used throughout credential resolution.
    pub environment: HashMap<String, String>,
}
crate::record_serde::wire_record!(ProcessLoginContext { config_dir: "configDir" if is_none, environment: "environment" });

/// Creates a connection-owned authentication handle without starting a login flow.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestOpen {
    /// The original immutable authentication configuration.
    #[doc(hidden)]
    pub(crate) context: ProcessLoginContext,
    pub operation: AccountLoginProcessRequestOpenOperation,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestOpen {
    context: "context",
    operation: "operation"
});

/// Validates and persists a token through the original authentication handle.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestToken {
    /// The normalized GitHub host.
    pub host: String,
    pub operation: AccountLoginProcessRequestTokenOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
    /// The user-supplied token. It is never returned.
    pub token: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestToken {
    host: "host",
    operation: "operation",
    owner_id: "ownerId",
    token: "token"
});

/// Starts the original device-code flow.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestStartDevice {
    /// The normalized GitHub host.
    pub host: String,
    pub operation: AccountLoginProcessRequestStartDeviceOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestStartDevice {
    host: "host",
    operation: "operation",
    owner_id: "ownerId"
});

/// Starts the original loopback browser flow with its existing deadline and PKCE behavior.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestStartWeb {
    /// The normalized GitHub host.
    pub host: String,
    pub operation: AccountLoginProcessRequestStartWebOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestStartWeb {
    host: "host",
    operation: "operation",
    owner_id: "ownerId"
});

/// Completes the owner's current authorization flow.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestComplete {
    pub operation: AccountLoginProcessRequestCompleteOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestComplete {
    operation: "operation",
    owner_id: "ownerId"
});

/// Opens the authorization page for the owner's current flow.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestOpenBrowser {
    pub operation: AccountLoginProcessRequestOpenBrowserOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestOpenBrowser {
    operation: "operation",
    owner_id: "ownerId"
});

/// Continues the same pending persistence operation after plaintext consent.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestContinue {
    /// Whether the user consented to plaintext credential storage.
    pub allow_plaintext: bool,
    pub operation: AccountLoginProcessRequestContinueOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestContinue {
    allow_plaintext: "allowPlaintext",
    operation: "operation",
    owner_id: "ownerId"
});

/// The host facts used for the post-login organization policy evaluation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ProcessLoginPolicyContext {
    /// The current host environment, including policy provenance restrictions.
    pub environment: HashMap<String, String>,
    /// The original user's home directory.
    pub home_directory: String,
    /// The original host platform, such as win32, darwin, or linux.
    pub platform: String,
    /// The original host's Node.js version for the existing policy user agent.
    pub process_version: String,
}
crate::record_serde::wire_record!(ProcessLoginPolicyContext {
    environment: "environment",
    home_directory: "homeDirectory",
    platform: "platform",
    process_version: "processVersion"
});

/// Evaluates both organization policy channels against the owner's current credential.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestPolicy {
    /// The original host's current policy inputs.
    #[doc(hidden)]
    pub(crate) context: ProcessLoginPolicyContext,
    pub operation: AccountLoginProcessRequestPolicyOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestPolicy {
    context: "context",
    operation: "operation",
    owner_id: "ownerId"
});

/// Removes only the credential that this owner currently resolves.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestDiscard {
    pub operation: AccountLoginProcessRequestDiscardOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestDiscard {
    operation: "operation",
    owner_id: "ownerId"
});

/// Cancels pending authorization and releases this owner.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessRequestClose {
    pub operation: AccountLoginProcessRequestCloseOperation,
    /// The opaque handle returned by open.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessRequestClose {
    operation: "operation",
    owner_id: "ownerId"
});

/// The original authentication owner is ready.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultOpened {
    pub kind: AccountLoginProcessResultOpenedKind,
    /// The opaque owner identifier, usable only by this connection.
    pub owner_id: String,
}
crate::record_serde::wire_record!(AccountLoginProcessResultOpened {
    kind: "kind",
    owner_id: "ownerId"
});

/// The device-code instructions to present to the user.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultDeviceCode {
    pub kind: AccountLoginProcessResultDeviceCodeKind,
    /// The code the user enters.
    pub user_code: String,
    /// The authorization page.
    pub verification_uri: String,
}
crate::record_serde::wire_record!(AccountLoginProcessResultDeviceCode {
    kind: "kind",
    user_code: "userCode",
    verification_uri: "verificationUri"
});

/// The authorization page for the original loopback flow.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultWeb {
    pub kind: AccountLoginProcessResultWebKind,
    /// The authorization URL.
    pub url: String,
}
crate::record_serde::wire_record!(AccountLoginProcessResultWeb {
    kind: "kind",
    url: "url"
});

/// An account offered by the original login persistence operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ProcessLoginAccount {
    /// The account host.
    pub host: String,
    /// The account provider kind.
    pub kind: String,
    /// The account login.
    pub login: String,
    /// The opaque account selector.
    pub selection_id: String,
}
crate::record_serde::wire_record!(ProcessLoginAccount {
    host: "host",
    kind: "kind",
    login: "login",
    selection_id: "selectionId"
});

/// The original login persistence result, without credentials.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ProcessLoginPersistence {
    /// The offered accounts, when account selection is required.
    #[doc(hidden)]
    pub(crate) accounts: Option<Vec<ProcessLoginAccount>>,
    /// The runtime's original user description, when host and login are available.
    pub description: Option<String>,
    /// The authenticated host, when available.
    pub host: Option<String>,
    /// The authenticated login, when available.
    pub login: Option<String>,
    /// The original completed, needs-plaintext-consent, needs-account-selection, or declined status.
    pub status: String,
}
crate::record_serde::wire_record!(ProcessLoginPersistence { accounts: "accounts" if is_none, description: "description" if is_none, host: "host" if is_none, login: "login" if is_none, status: "status" });

/// A persistence operation completed or requires user input.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultPersistence {
    pub kind: AccountLoginProcessResultPersistenceKind,
    /// The original persistence result.
    #[doc(hidden)]
    pub(crate) result: ProcessLoginPersistence,
}
crate::record_serde::wire_record!(AccountLoginProcessResultPersistence {
    kind: "kind",
    result: "result"
});

/// The organization policy verdict.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultPolicy {
    /// The verdict from both original policy channels.
    #[doc(hidden)]
    pub(crate) decision: ForceLoginOrgsDecision,
    pub kind: AccountLoginProcessResultPolicyKind,
    /// Original policy warnings for the host's configured logger.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(AccountLoginProcessResultPolicy {
    decision: "decision",
    kind: "kind",
    warnings: "warnings"
});

/// The current credential removal result.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultDiscarded {
    pub kind: AccountLoginProcessResultDiscardedKind,
    /// Whether a credential was removed.
    pub removed: bool,
}
crate::record_serde::wire_record!(AccountLoginProcessResultDiscarded {
    kind: "kind",
    removed: "removed"
});

/// The requested browser or cleanup action completed.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountLoginProcessResultDone {
    pub kind: AccountLoginProcessResultDoneKind,
}
crate::record_serde::wire_record!(AccountLoginProcessResultDone { kind: "kind" });

/// Credentials to validate and store. Omit login to resolve the authenticated user from the token.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountLoginRequest {
    /// GitHub host URL
    pub host: String,
    /// User login/username. When omitted, the runtime validates the token and resolves the login from GitHub.
    pub login: Option<String>,
    /// GitHub authentication token
    pub token: String,
}
crate::record_serde::wire_record!(AccountLoginRequest { host: "host", login: "login" if is_none, token: "token" });

/// Result of a successful login; throws on failure
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountLoginResult {
    /// Whether the credential was persisted to a secure store (system keychain, or the config file when plaintext storage is enabled). False when no secure store was available and the token was not saved, so the consumer can decide how to proceed.
    pub stored_in_vault: bool,
}
crate::record_serde::wire_record!(AccountLoginResult {
    stored_in_vault: "storedInVault"
});

/// User to log out
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountLogoutRequest {
    /// Authentication information for the user to log out
    pub auth_info: Option<AuthInfo>,
    /// Opaque account identifier returned by `account.getAllUsers`
    pub selection_id: Option<String>,
}
crate::record_serde::wire_record!(AccountLogoutRequest { auth_info: "authInfo" if is_none, selection_id: "selectionId" if is_none });

/// Logout result indicating if more users remain
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountLogoutResult {
    /// Whether other authenticated users remain after logout
    pub has_more_users: bool,
}
crate::record_serde::wire_record!(AccountLogoutResult {
    has_more_users: "hasMoreUsers"
});

/// The selected account and its credential-free identity metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountsGetActiveResult {
    /// The selected account, or null when none is active.
    pub account: Option<Account>,
    /// Credential-free identity metadata for the active account, including resolved Copilot user information when available.
    pub auth_info: Option<AuthIdentity>,
    /// Opaque credential selection id for the active authentication. This remains available when the canonical account is not currently switchable.
    pub auth_selection_id: Option<String>,
}
crate::record_serde::wire_record!(AccountsGetActiveResult { account: "account", auth_info: "authInfo" if is_none, auth_selection_id: "authSelectionId" if is_none });

/// Options for listing the canonical account inventory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountsListRequest {
    /// Whether to discard the cached inventory and re-run credential reads and availability checks for every account before returning the canonical account projection.
    pub refresh: Option<bool>,
}
crate::record_serde::wire_record!(AccountsListRequest { refresh: "refresh" if is_none });

/// Log out a signed-in account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountsLogoutRequest {
    /// Stable account id of the account to log out. Use this for canonical account entries that are not switchable.
    pub account_id: Option<String>,
    /// Opaque selection id of the account to log out; absent logs out the active account.
    pub selection_id: Option<String>,
}
crate::record_serde::wire_record!(AccountsLogoutRequest { account_id: "accountId" if is_none, selection_id: "selectionId" if is_none });

/// A GitHub credential to install and persist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountsSetCredentialsRequest {
    /// GitHub authentication host URL.
    pub host: String,
    /// GitHub login/username for the credential.
    pub login: String,
    /// GitHub authentication token to install.
    pub token: String,
}
crate::record_serde::wire_record!(AccountsSetCredentialsRequest {
    host: "host",
    login: "login",
    token: "token"
});

/// Select a signed-in account for this session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountsSwitchActiveRequest {
    /// Opaque selection id of the account to make active.
    pub selection_id: String,
}
crate::record_serde::wire_record!(AccountsSwitchActiveRequest {
    selection_id: "selectionId"
});

/// The authentication identity to enroll in Copilot Free.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountSubscribeCopilotFreeRequest {
    /// Identity to enroll, in the shape `session.gitHubAuth.getCurrentAuthInfo` returns. The runtime resolves the credential for it, so no credential is sent.
    pub identity: AuthIdentity,
}
crate::record_serde::wire_record!(AccountSubscribeCopilotFreeRequest {
    identity: "identity"
});

/// What came of a Copilot Free enrollment attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AccountSubscribeCopilotFreeResult {
    /// `subscribed` when GitHub answered, and `status` then carries its answer. `no-token` when no credential could be resolved for the identity, which is the caller's cue to ask the user to sign in again rather than to report a failure. `failed` when the attempt itself errored, and `message` then explains it.
    pub kind: String,
    /// Explanation to show the user. Present on `failed`, and optionally alongside a `subscribed` status.
    pub message: Option<String>,
    /// GitHub's enrollment status when `kind` is `subscribed`, for example `subscribed` or `already_subscribed`. Absent otherwise.
    pub status: Option<String>,
}
crate::record_serde::wire_record!(AccountSubscribeCopilotFreeResult { kind: "kind", message: "message" if is_none, status: "status" if is_none });

/// Advance an in-flight login flow, optionally fulfilling an input-required step.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginAdvanceRequest {
    /// Opaque flow id from begin.
    pub flow_id: String,
    /// Neutral input fulfilling a preceding input-required step (e.g. a GHEC host); ignored otherwise.
    pub input: Option<String>,
}
crate::record_serde::wire_record!(AuthLoginAdvanceRequest { flow_id: "flowId", input: "input" if is_none });

/// Begin an interactive login flow for a provider kind, optionally reusing a cached base identity or explicitly requesting provider account selection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginBeginRequest {
    /// Opaque cached base identity id returned by accounts.login.list. Valid only with default mode; consumers must not parse it.
    pub account_id: Option<String>,
    /// The provider kind to sign in with.
    pub kind: LoginProviderKind,
    /// How to authorize: `web` (default) or `device-code`. Device code is offered for `githubDotCom` and `proxima`; `proxima` still asks for its host through `input-required` first.
    pub method: Option<AuthLoginMethod>,
    /// Neutral identity-selection intent. Omitted is equivalent to default.
    pub mode: Option<AuthLoginMode>,
}
crate::record_serde::wire_record!(AuthLoginBeginRequest { account_id: "accountId" if is_none, kind: "kind", method: "method" if is_none, mode: "mode" if is_none });

/// A started login flow: its opaque id and first step.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthLoginBegun {
    /// Opaque flow id used to advance or cancel this login.
    pub flow_id: String,
    /// The first step of the flow.
    pub step: AuthLoginStep,
}

/// Cancel an in-flight login flow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginCancelRequest {
    /// Opaque flow id from begin.
    pub flow_id: String,
}
crate::record_serde::wire_record!(AuthLoginCancelRequest { flow_id: "flowId" });

/// The provider whose cached base identities should be listed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginListRequest {
    /// Login provider whose reusable base identities should be listed.
    pub provider: LoginProviderKind,
}
crate::record_serde::wire_record!(AuthLoginListRequest {
    provider: "provider"
});

/// Runtime-to-owner cancellation request for a client-owned task.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ClientTaskCancelRequest {
    /// Opaque identifier shared by coalesced cancellation callers
    pub cancellation_id: String,
    /// Owner-scoped task key included for correlation
    pub client_task_id: String,
    /// Canonical runtime-generated task identifier
    pub id: String,
    /// Reason the runtime requests cancellation
    pub reason: ClientTaskCancelReason,
    /// Session that owns the client task
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(ClientTaskCancelRequest {
    cancellation_id: "cancellationId",
    client_task_id: "clientTaskId",
    id: "id",
    reason: "reason",
    session_id: "sessionId"
});

/// Whether the client authoritatively confirmed its external work stopped.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ClientTaskCancelResult {
    /// True only when the owner confirms that external work stopped before responding
    pub cancelled: bool,
}
crate::record_serde::wire_record!(ClientTaskCancelResult {
    cancelled: "cancelled"
});

/// One retained session-scoped diagnostic record. Potentially content-bearing diagnostic data is opt-in and must not be exported automatically as telemetry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticEntry {
    /// Agent identifier for a subagent host. Omitted for the root agent.
    pub agent_id: Option<String>,
    /// Typed MCP diagnostic detail.
    pub details: McpDiagnosticDetails,
    /// Severity of this emitted diagnostic record.
    pub level: DiagnosticSeverity,
    /// Human-readable diagnostic summary.
    pub message: String,
    /// Original byte count when a known-size message or data value was truncated.
    pub original_bytes: Option<i64>,
    /// Diagnostic source identifying the typed details payload.
    pub source: DiagnosticEntrySource,
    /// UTC RFC 3339 timestamp captured at the diagnostic source.
    pub timestamp: String,
    /// Whether message or data was truncated to the record-size bound.
    pub truncated: Option<bool>,
}
crate::record_serde::wire_record!(DiagnosticEntry { agent_id: "agentId" if is_none, details: "details", level: "level", message: "message", original_bytes: "originalBytes" if is_none, source: "source", timestamp: "timestamp", truncated: "truncated" if is_none });

/// Per-source session diagnostics configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticsConfiguration {
    /// Diagnostic thresholds keyed by supported source.
    pub sources: DiagnosticSourcesConfiguration,
}
crate::record_serde::wire_record!(DiagnosticsConfiguration { sources: "sources" });

/// Patch session diagnostic thresholds for explicitly supplied sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticsConfigureRequest {
    /// Sources to configure. At least one supported source must be supplied.
    pub sources: DiagnosticSourcesConfiguration,
}
crate::record_serde::wire_record!(DiagnosticsConfigureRequest { sources: "sources" });

/// Cursor-based request for session diagnostics. The default limit is 100 (maximum 500); the default waitMs is zero (maximum 30000).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticsReadRequest {
    /// Opaque cursor returned by an earlier read. Omit to start at the oldest retained record.
    pub cursor: Option<String>,
    /// Maximum number of records to return, from 1 through 500. Omit for 100.
    pub max: Option<i64>,
    /// Nonempty selection of sources to read. Each source may be listed once.
    pub sources: Vec<DiagnosticSource>,
    /// Maximum time in milliseconds to wait for a new record, from 0 through 30000.
    pub wait_ms: Option<i32>,
}
crate::record_serde::wire_record!(DiagnosticsReadRequest { cursor: "cursor" if is_none, max: "max" if is_none, sources: "sources", wait_ms: "waitMs" if is_none });

#[derive(Debug, Clone, Default)]
pub struct DiagnosticsReadResultEntriesItem {
    /// Agent identifier for a subagent host. Omitted for the root agent.
    pub agent_id: Option<String>,
    /// Typed MCP diagnostic detail.
    pub details: McpDiagnosticDetails,
    /// Severity of this emitted diagnostic record.
    pub level: DiagnosticSeverity,
    /// Human-readable diagnostic summary.
    pub message: String,
    /// Original byte count when a known-size message or data value was truncated.
    pub original_bytes: Option<i64>,
    /// Diagnostic source identifying the typed details payload.
    pub source: DiagnosticsReadResultEntriesItemSource,
    /// UTC RFC 3339 timestamp captured at the diagnostic source.
    pub timestamp: String,
    /// Whether message or data was truncated to the record-size bound.
    pub truncated: Option<bool>,
}
crate::record_serde::wire_record!(DiagnosticsReadResultEntriesItem { agent_id: "agentId" if is_none, details: "details", level: "level", message: "message", original_bytes: "originalBytes" if is_none, source: "source", timestamp: "timestamp", truncated: "truncated" if is_none });

/// One cursor-addressed page of retained session diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticsReadResult {
    /// Opaque cursor for the next independent read.
    pub cursor: String,
    /// Whether the requested cursor remained within the retained buffer window.
    pub cursor_status: DiagnosticCursorStatus,
    /// Number of records lost before this page when known. Omitted when a buffer generation change makes the count unknowable.
    pub dropped_count: Option<i64>,
    /// Retained records beginning at the requested cursor.
    pub entries: Vec<DiagnosticsReadResultEntriesItem>,
    /// Whether additional retained records follow this page.
    pub has_more: bool,
}
crate::record_serde::wire_record!(DiagnosticsReadResult { cursor: "cursor", cursor_status: "cursorStatus", dropped_count: "droppedCount" if is_none, entries: "entries", has_more: "hasMore" });

/// Opens the exact output target granted by the native host, never a request-selected path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FileLogOpenRequest {
    /// Additional environment variable names that enable debug output when set to true or 1.
    pub debug_environment_variables: Option<Vec<String>>,
    /// Explicit verbosity. If absent, the runtime applies its debug environment and default level.
    pub level: Option<FileLogLevel>,
    /// Additional secrets registered with the shared process filter before any log write.
    pub secret_values: Option<Vec<String>>,
}
crate::record_serde::wire_record!(FileLogOpenRequest { debug_environment_variables: "debugEnvironmentVariables" if is_none, level: "level" if is_none, secret_values: "secretValues" if is_none });

/// Identifies a file logger owned by the originating SDK connection generation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FileLogResource {
    /// Opaque identifier returned by logging.file.open.
    pub log_id: String,
}
crate::record_serde::wire_record!(FileLogResource { log_id: "logId" });

/// Appends a redacted record to an owned file logger.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FileLogWriteRequest {
    /// Optional verbosity, with the runtime's existing group-level semantics.
    pub group_level: Option<FileLogLevel>,
    /// Opaque connection-owned logger identifier.
    pub log_id: String,
    /// Text to redact and append. Use an empty string for endGroup.
    pub message: String,
    /// Operation whose level and category the runtime applies.
    pub operation: FileLogOperation,
}
crate::record_serde::wire_record!(FileLogWriteRequest { group_level: "groupLevel" if is_none, log_id: "logId", message: "message", operation: "operation" });

/// The answer to a repository predicate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitBooleanResult {
    /// Whether the predicate holds.
    pub value: bool,
}
crate::record_serde::wire_record!(GitBooleanResult { value: "value" });

/// A branch in a local repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitBranchRequest {
    /// Branch name to check.
    pub branch: String,
    /// Absolute repository root.
    pub git_root: String,
}
crate::record_serde::wire_record!(GitBranchRequest {
    branch: "branch",
    git_root: "gitRoot"
});

/// Counts of index, checkout, and untracked changes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitChangeCountsResult {
    /// Whether any status entries exist.
    pub has_changes: bool,
    /// Number of index changes.
    pub staged: i64,
    /// Number of tracked checkout changes.
    pub unstaged: i64,
    /// Number of untracked entries.
    pub untracked: i64,
}
crate::record_serde::wire_record!(GitChangeCountsResult {
    has_changes: "hasChanges",
    staged: "staged",
    unstaged: "unstaged",
    untracked: "untracked"
});

/// Stage and commit all changes in a checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCommitAllRequest {
    /// Absolute checkout directory.
    pub cwd: String,
    /// Commit message, passed as one Git argument.
    pub message: String,
}
crate::record_serde::wire_record!(GitCommitAllRequest {
    cwd: "cwd",
    message: "message"
});

/// The created commit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCommitAllResult {
    /// Short commit hash.
    pub commit_hash: String,
    /// Commit message.
    pub commit_message: String,
}
crate::record_serde::wire_record!(GitCommitAllResult {
    commit_hash: "commitHash",
    commit_message: "commitMessage"
});

/// Directory context with optional expensive branch and base probes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitContextRequest {
    /// Absolute working-tree directory.
    pub cwd: String,
    /// Include merge-base probes. Defaults to true; false supports fast startup.
    pub include_base_commit: Option<bool>,
    /// Include the default branch. Defaults to false.
    pub include_default_branch: Option<bool>,
}
crate::record_serde::wire_record!(GitContextRequest { cwd: "cwd", include_base_commit: "includeBaseCommit" if is_none, include_default_branch: "includeDefaultBranch" if is_none });

/// Create a branch at HEAD and push it with upstream tracking.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreateAndPushBranchRequest {
    /// New branch name.
    pub branch: String,
    /// Absolute checkout directory.
    pub cwd: String,
    /// Remote name. Omission selects origin.
    pub remote: Option<String>,
}
crate::record_serde::wire_record!(GitCreateAndPushBranchRequest { branch: "branch", cwd: "cwd", remote: "remote" if is_none });

/// A new worktree and the base ref actually used.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreatedWorktree {
    /// Actual base ref, including the HEAD fallback.
    pub base_ref: String,
    /// Actual branch name, including any collision suffix.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
}
crate::record_serde::wire_record!(GitCreatedWorktree {
    base_ref: "baseRef",
    branch: "branch",
    path: "path"
});

/// The remote the checked-out branch tracks.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitCurrentBranchRemoteResult {
    /// Name of the tracked remote. Reports `origin` whenever the working tree has no tracking configuration to read, including on a detached HEAD, so this is never null and never empty.
    pub remote: String,
}
crate::record_serde::wire_record!(GitCurrentBranchRemoteResult { remote: "remote" });

/// Working-tree path a git query applies to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitCwdRequest {
    /// Absolute path to a directory inside the git working tree to query.
    pub cwd: String,
}
crate::record_serde::wire_record!(GitCwdRequest { cwd: "cwd" });

/// Repository and optional remote whose default branch to resolve.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDefaultBranchRequest {
    /// Absolute working-tree directory.
    pub cwd: String,
    /// Remote name. Omission selects the helper's normal remote.
    pub remote: Option<String>,
}
crate::record_serde::wire_record!(GitDefaultBranchRequest { cwd: "cwd", remote: "remote" if is_none });

/// Directory or already-discovered root whose remotes to classify.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDetectRemoteHostsRequest {
    /// Absolute directory, or the discovered root when fromRoot is true.
    pub cwd: String,
    /// Whether prior discovery found a root. False returns no known hosts.
    pub found: Option<bool>,
    /// Skip root discovery. Defaults to false.
    pub from_root: Option<bool>,
}
crate::record_serde::wire_record!(GitDetectRemoteHostsRequest { cwd: "cwd", found: "found" if is_none, from_root: "fromRoot" if is_none });

/// Working-tree changes to collect.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDiffChangesRequest {
    /// Absolute working-tree directory.
    pub cwd: String,
    /// Omit whitespace-only changes. Defaults to false.
    pub ignore_whitespace: Option<bool>,
}
crate::record_serde::wire_record!(GitDiffChangesRequest { cwd: "cwd", ignore_whitespace: "ignoreWhitespace" if is_none });

/// One changed file and its bounded diff.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDiffFileChange {
    /// Change kind: added, modified, deleted, or renamed.
    pub change_type: GitDiffChangeType,
    /// Unified diff.
    pub diff: String,
    /// Whether the diff exceeds its size budget.
    pub is_truncated: Option<bool>,
    /// Previous path for a rename.
    pub old_path: Option<String>,
    /// Repository-relative file path.
    pub path: String,
}
crate::record_serde::wire_record!(GitDiffFileChange { change_type: "changeType", diff: "diff", is_truncated: "isTruncated" if is_none, old_path: "oldPath" if is_none, path: "path" });

/// Changed files in a checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDiffChangesResult {
    /// Changes in repository order.
    pub changes: Vec<GitDiffFileChange>,
}
crate::record_serde::wire_record!(GitDiffChangesResult { changes: "changes" });

/// Parameters for an unstaged repository diff without a live model session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDiffRequest {
    /// Absolute working directory. Changes cover its entire Git working tree.
    pub cwd: String,
    /// Ignore whitespace-only changes. Defaults to false.
    pub ignore_whitespace: Option<bool>,
}
crate::record_serde::wire_record!(GitDiffRequest { cwd: "cwd", ignore_whitespace: "ignoreWhitespace" if is_none });

/// The directory and root-resolution policy for repository discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitFindRootRequest {
    /// Absolute directory from which to discover the repository.
    pub cwd: String,
    /// Resolve a linked worktree to its main repository. Defaults to false.
    pub resolve_worktrees: Option<bool>,
}
crate::record_serde::wire_record!(GitFindRootRequest { cwd: "cwd", resolve_worktrees: "resolveWorktrees" if is_none });

/// One check reported against a pull request's head commit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubPullRequestCheck {
    /// Name the check reports itself under.
    pub name: String,
    /// Rolled-up state of the check.
    #[doc(hidden)]
    pub(crate) status: GitHubPullRequestCheckStatus,
}
crate::record_serde::wire_record!(GitHubPullRequestCheck {
    name: "name",
    status: "status"
});

/// The pull request a branch is open against, with the review state a caller needs to summarize it without a second request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubBranchPullRequest {
    /// Lines added across the pull request.
    pub additions: i64,
    /// Pull request description body. Empty when the pull request has none.
    pub body: String,
    /// Number of files the pull request changes.
    pub changed_files: i64,
    /// Checks reported against the pull request's head commit.
    #[doc(hidden)]
    pub(crate) checks: Vec<GitHubPullRequestCheck>,
    /// Number of comments on the pull request.
    pub comments: i64,
    /// Lines deleted across the pull request.
    pub deletions: i64,
    /// Head branch name the pull request was opened from.
    pub head_ref_name: String,
    /// Whether the pull request is a draft.
    pub is_draft: bool,
    /// Label names applied to the pull request.
    pub labels: Vec<String>,
    /// Pull request number within its repository.
    pub number: i64,
    /// Owner login of the repository the pull request was opened against.
    pub owner: String,
    /// Name of the repository the pull request was opened against.
    pub repo: String,
    /// Logins of the requested reviewers.
    pub reviewers: Vec<String>,
    /// Pull request state as GitHub reports it, such as `OPEN`, `CLOSED` or `MERGED`.
    pub state: String,
    /// Pull request title.
    pub title: String,
    /// Web URL of the pull request.
    pub url: String,
}
crate::record_serde::wire_record!(GitHubBranchPullRequest {
    additions: "additions",
    body: "body",
    changed_files: "changedFiles",
    checks: "checks",
    comments: "comments",
    deletions: "deletions",
    head_ref_name: "headRefName",
    is_draft: "isDraft",
    labels: "labels",
    number: "number",
    owner: "owner",
    repo: "repo",
    reviewers: "reviewers",
    state: "state",
    title: "title",
    url: "url"
});

/// Owner and name of a GitHub repository. Deliberately distinct from `GitHubRepositoryIdentity`, which also carries a host: a by-id lookup takes the host as an input and the API's answer reports none, so there is no host here to report faithfully.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryOwnerName {
    /// Repository name, without the owner prefix or the `.git` suffix.
    pub name: String,
    /// Repository owner login (user or organization).
    pub owner: String,
}
crate::record_serde::wire_record!(GitHubRepositoryOwnerName {
    name: "name",
    owner: "owner"
});

/// Selects the pull request a branch is open against, and the credential to look it up under. The branch is searched across every repository in `baseRepositories`, which lets a fork's branch find the pull request it opened against upstream.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubBranchPullRequestLookupRequest {
    /// The credential the lookup runs under, carried opaquely because its shape is the host's own and the runtime only resolves a token and a GitHub host from it. Required: the search answers per-account, so a pull request invisible to this credential is reported as absent rather than refused.
    pub auth_info: serde_json::Value,
    /// Repositories to search for the pull request, in preference order. A fork supplies both its own repository and its upstream, because the branch may have opened the pull request against either.
    #[doc(hidden)]
    pub(crate) base_repositories: Vec<GitHubRepositoryOwnerName>,
    /// Head branch name to find the pull request for, without a `refs/heads/` prefix.
    pub branch: String,
    /// Login that must own the pull request's head branch. A branch name alone is ambiguous across forks, so a candidate whose head is owned by anyone else is discarded.
    pub head_owner: String,
    /// Whether a closed or merged pull request counts as a match. False considers only open ones, so a caller acting on the branch's current pull request does not resurface a merged one.
    pub include_closed: bool,
}
crate::record_serde::wire_record!(GitHubBranchPullRequestLookupRequest {
    auth_info: "authInfo",
    base_repositories: "baseRepositories",
    branch: "branch",
    head_owner: "headOwner",
    include_closed: "includeClosed"
});

/// Outcome of a branch pull-request lookup. `status` carries why, because 'not authenticated', 'no pull request' and 'the search failed' are three different answers and only the first two are conclusive.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubBranchPullRequestLookupResult {
    /// The matching pull request, or null for every status other than `found`.
    pub pull_request: Option<GitHubBranchPullRequest>,
    /// Why the lookup reported what it did.
    #[doc(hidden)]
    pub(crate) status: GitHubBranchPullRequestLookupStatus,
}
crate::record_serde::wire_record!(GitHubBranchPullRequestLookupResult { pull_request: "pullRequest" if is_none, status: "status" });

/// The text content of one secret gist file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubSecretGistFile {
    /// The complete text content of the file.
    pub content: String,
}
crate::record_serde::wire_record!(GitHubSecretGistFile { content: "content" });

/// Creates a secret gist through the bound session's original credential owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubCreateSecretGistRequest {
    /// The description that GitHub stores with the gist.
    pub description: String,
    /// File names mapped to their text content.
    #[doc(hidden)]
    pub(crate) files: HashMap<String, GitHubSecretGistFile>,
    /// The purpose that selects account restrictions and error messages.
    #[doc(hidden)]
    pub(crate) use_case: GitHubSecretGistUseCase,
}
crate::record_serde::wire_record!(GitHubCreateSecretGistRequest {
    description: "description",
    files: "files",
    use_case: "useCase"
});

/// The URL returned by GitHub after it creates the secret gist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubCreateSecretGistResult {
    /// The URL of the new secret gist.
    pub url: String,
}
crate::record_serde::wire_record!(GitHubCreateSecretGistResult { url: "url" });

/// Whether cancellation reached a registered GitHub request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchCancelResult {
    /// True when cancellation reached a registered request.
    pub canceled: bool,
}
crate::record_serde::wire_record!(GitHubFetchCancelResult {
    canceled: "canceled"
});

/// One HTTP header in an ordered list.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchHeader {
    /// HTTP header name. The list preserves order and repeated names.
    pub name: String,
    /// HTTP header value.
    pub value: String,
}
crate::record_serde::wire_record!(GitHubFetchHeader {
    name: "name",
    value: "value"
});

/// A user-facing HTTP failure formatted in the runtime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchHttpError {
    /// Complete user-facing error text.
    pub message: String,
    /// Parsed response body serialized as JSON, when available.
    pub response_body_json: Option<String>,
}
crate::record_serde::wire_record!(GitHubFetchHttpError { message: "message", response_body_json: "responseBodyJson" if is_none });

/// Arguments for a named authenticated operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchOperationRequest {
    /// Opaque selector for a live host-owned credential binding. Only the owning host connection may use it.
    pub auth_owner_id: Option<String>,
    /// JSON request body, preserved as supplied.
    pub body_json: Option<String>,
    /// The operation to execute.
    #[doc(hidden)]
    pub(crate) operation: GitHubFetchOperation,
    /// Named operation arguments, such as owner, repo, or jobId.
    pub params: HashMap<String, String>,
    /// A fresh request id.
    pub request_id: i64,
    /// Session whose auth manager to use. Omission selects the process manager.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(GitHubFetchOperationRequest { auth_owner_id: "authOwnerId" if is_none, body_json: "bodyJson" if is_none, operation: "operation", params: "params", request_id: "requestId", session_id: "sessionId" if is_none });

/// A pre-resolved address for a GitHub request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubFetchPinnedAddress {
    /// Address the network transport must use.
    pub address: String,
}
crate::record_serde::wire_record!(GitHubFetchPinnedAddress { address: "address" });

/// Proxy environment for one authenticated GitHub request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubFetchProxyEnvironment {
    /// Proxy for HTTP requests.
    pub http_proxy: Option<String>,
    /// Proxy for HTTPS requests.
    pub https_proxy: Option<String>,
    /// Hosts that bypass the proxy.
    pub no_proxy: Option<String>,
}
crate::record_serde::wire_record!(GitHubFetchProxyEnvironment { http_proxy: "httpProxy" if is_none, https_proxy: "httpsProxy" if is_none, no_proxy: "noProxy" if is_none });

/// Optional overrides for the runtime retry policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchRetryConfig {
    /// Delay multiplier; the runtime default is two.
    pub backoff_factor: Option<f64>,
    /// Initial retry delay; the runtime default is five seconds.
    pub default_retry_delay_seconds: Option<f64>,
    /// Whether repeated failures open the runtime circuit breaker. Defaults to true.
    pub enable_circuit_breaker: Option<bool>,
    /// Maximum retries; the runtime default is five.
    pub max_retries: Option<i64>,
}
crate::record_serde::wire_record!(GitHubFetchRetryConfig { backoff_factor: "backoffFactor" if is_none, default_retry_delay_seconds: "defaultRetryDelaySeconds" if is_none, enable_circuit_breaker: "enableCircuitBreaker" if is_none, max_retries: "maxRetries" if is_none });

/// A GitHub API request without caller-supplied auth identity or credential fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchRequest {
    /// Whether the request permits loopback addresses. Defaults to true.
    pub allow_localhost: Option<bool>,
    /// API label used in runtime diagnostics.
    pub api_name: String,
    /// Opaque selector for a live host-owned credential binding. Only the owning host connection may use it.
    pub auth_owner_id: Option<String>,
    /// Request bytes encoded as base64. Absent for a request without a body.
    pub body_base64: Option<String>,
    /// Maximum response body size in bytes.
    pub body_limit_bytes: Option<i64>,
    /// Whether the request bypasses idle connections.
    pub disable_connection_reuse: Option<bool>,
    /// Caller-supplied HTTP headers.
    #[doc(hidden)]
    pub(crate) headers: Vec<GitHubFetchHeader>,
    /// HTTP method.
    pub method: String,
    /// Addresses to pin for this request, without another DNS lookup.
    pub pinned_addresses: Option<Vec<GitHubFetchPinnedAddress>>,
    /// Explicit proxy environment. Omission uses the process environment.
    pub proxy_env: Option<GitHubFetchProxyEnvironment>,
    /// Whether the proxy uses Negotiate authentication.
    pub proxy_negotiate_auth: Option<bool>,
    /// Optional service principal for proxy Negotiate authentication.
    pub proxy_negotiate_spn: Option<String>,
    /// Response read timeout in milliseconds. Defaults to 700000.
    pub read_timeout_ms: Option<i64>,
    /// Redirect behavior. Defaults to following redirects.
    pub redirect: Option<String>,
    /// A fresh request id.
    pub request_id: i64,
    /// Optional retry overrides.
    #[doc(hidden)]
    pub(crate) retry_options: Option<GitHubFetchRetryConfig>,
    /// Session whose auth manager to use. Omission selects the process manager.
    pub session_id: Option<SessionId>,
    /// Total request timeout in milliseconds.
    pub timeout_ms: Option<i64>,
    /// Absolute URL on the resolved GitHub API origin.
    pub url: String,
}
crate::record_serde::wire_record!(GitHubFetchRequest { allow_localhost: "allowLocalhost" if is_none, api_name: "apiName", auth_owner_id: "authOwnerId" if is_none, body_base64: "bodyBase64" if is_none, body_limit_bytes: "bodyLimitBytes" if is_none, disable_connection_reuse: "disableConnectionReuse" if is_none, headers: "headers", method: "method", pinned_addresses: "pinnedAddresses" if is_none, proxy_env: "proxyEnv" if is_none, proxy_negotiate_auth: "proxyNegotiateAuth" if is_none, proxy_negotiate_spn: "proxyNegotiateSpn" if is_none, read_timeout_ms: "readTimeoutMs" if is_none, redirect: "redirect" if is_none, request_id: "requestId", retry_options: "retryOptions" if is_none, session_id: "sessionId" if is_none, timeout_ms: "timeoutMs" if is_none, url: "url" });

/// One registered GitHub request, shared by execution and cancellation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchRequestId {
    /// One fresh id from `gitHubFetch.allocateRequestId`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubFetchRequestId {
    request_id: "requestId"
});

#[derive(Debug, Clone, Default)]
pub struct GitHubFetchResultResponse {
    /// Complete response bytes encoded as base64.
    pub body_base64: String,
    /// Response headers.
    #[doc(hidden)]
    pub(crate) headers: Vec<GitHubFetchHeader>,
    /// Formatted non-success response for a named operation.
    #[doc(hidden)]
    pub(crate) http_error: Option<GitHubFetchHttpError>,
    pub kind: GitHubFetchResultResponseKind,
    /// HTTP status, including non-success responses.
    pub status: i32,
    /// HTTP status text.
    pub status_text: String,
    /// Final response URL.
    pub url: String,
}
crate::record_serde::wire_record!(GitHubFetchResultResponse { body_base64: "bodyBase64", headers: "headers", http_error: "httpError" if is_none, kind: "kind", status: "status", status_text: "statusText", url: "url" });

#[derive(Debug, Clone, Default)]
pub struct GitHubFetchResultCircuitBreaker {
    /// Host whose breaker is open.
    pub host: String,
    pub kind: GitHubFetchResultCircuitBreakerKind,
    /// Complete circuit-breaker message.
    pub message: String,
    /// Remaining backoff in milliseconds.
    pub time_until_retry_ms: i64,
}
crate::record_serde::wire_record!(GitHubFetchResultCircuitBreaker {
    host: "host",
    kind: "kind",
    message: "message",
    time_until_retry_ms: "timeUntilRetryMs"
});

#[derive(Debug, Clone, Default)]
pub struct GitHubFetchResultProxyResponse {
    pub kind: GitHubFetchResultProxyResponseKind,
    /// Complete proxy failure message.
    pub message: String,
    /// HTTP proxy status.
    pub status: i32,
}
crate::record_serde::wire_record!(GitHubFetchResultProxyResponse {
    kind: "kind",
    message: "message",
    status: "status"
});

#[derive(Debug, Clone, Default)]
pub struct GitHubFetchResultFailed {
    pub kind: GitHubFetchResultFailedKind,
    /// Authentication, transport, or cancellation error.
    pub message: String,
}
crate::record_serde::wire_record!(GitHubFetchResultFailed {
    kind: "kind",
    message: "message"
});

/// A GitHub login the authenticated user may act as: their own account, or an organization they belong to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnerOption {
    /// The owner's GitHub login.
    pub login: String,
    /// Which kind of owner this is. The authenticated user's own account is always reported as `user`.
    pub r#type: String,
}
crate::record_serde::wire_record!(GitHubOwnerOption {
    login: "login",
    r#type: "type"
});

/// The owner listing to abandon.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersCancelRequest {
    /// Request id the listing was started with.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubOwnersCancelRequest {
    request_id: "requestId"
});

/// Whether the id named a running owner listing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersCancelResult {
    /// True when a listing with the id was running and the cancel stopped it. False when the id was never registered, was registered but unused, was released after being abandoned, or its listing had ended. An unused id is released and cannot start a later listing.
    pub canceled: bool,
}
crate::record_serde::wire_record!(GitHubOwnersCancelResult {
    canceled: "canceled"
});

/// Credential to list owners under, and the request id that makes the listing cancellable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersListRequest {
    /// The credential the listing runs under, carried opaquely because its shape is the host's own and the runtime only resolves a token and a GitHub host from it. No credential travels: this selects one the runtime already holds.
    pub auth_info: serde_json::Value,
    /// Request id from `gitHubOwners.nextRequestId`. An id that was never registered, canceled before use, released after being abandoned, or already used is refused rather than silently running uncancellable.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubOwnersListRequest {
    auth_info: "authInfo",
    request_id: "requestId"
});

/// Outcome of an owner listing. Exactly one of `owners` and `message` is present, except that `throwError` reports a failure the caller is expected to raise rather than render.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersListResult {
    /// Why no owners could be listed, phrased for a user. Present when the listing failed in a way the caller should render rather than raise.
    pub message: Option<String>,
    /// The owners, on success: the authenticated user first, then the organizations they belong to.
    #[doc(hidden)]
    pub(crate) owners: Option<Vec<GitHubOwnerOption>>,
    /// A malformed request or an unreadable credential, which the caller raises instead of rendering. Kept a field rather than a dispatch error so it stays distinct from `message`, which the caller renders.
    pub throw_error: Option<String>,
    /// A line the caller should log. Present only alongside `message`, and only for failures worth recording.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(GitHubOwnersListResult { message: "message" if is_none, owners: "owners" if is_none, throw_error: "throwError" if is_none, warning: "warning" if is_none });

/// A freshly registered request id. Registering it before the listing starts is what lets a cancel that races the request still find the owner listing slot. The id serves one listing only. Long-abandoned unused ids can be released by later allocations.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersRequestIdResult {
    /// Request id to pass to `gitHubOwners.list` and, to abandon it, `gitHubOwners.cancel`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubOwnersRequestIdResult {
    request_id: "requestId"
});

/// An unused release allocation that this caller will never dispatch. This is separate from cancellation before a request arrives.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseAbandonRequestParams {
    /// The unused request id owned by this connection.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubReleaseAbandonRequestParams {
    request_id: "requestId"
});

/// Whether the runtime retired an unused allocation without affecting active work.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseAbandonRequestResult {
    /// True only when this connection owned an unclaimed allocation with no active guard. False for active, foreign, unknown or previously retired ids. A successful abandonment prevents any later use of that id.
    pub abandoned: bool,
}
crate::record_serde::wire_record!(GitHubReleaseAbandonRequestResult {
    abandoned: "abandoned"
});

/// The release lookup or asset download to abandon.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseCancelRequest {
    /// Request id the lookup or download was started with.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubReleaseCancelRequest {
    request_id: "requestId"
});

/// Whether the cancel reached live work.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseCancelResult {
    /// True when the cancel reached a registered lookup or download; that call's own result still says how it ended. False when the id names none, which is the normal answer for work that already finished.
    pub canceled: bool,
}
crate::record_serde::wire_record!(GitHubReleaseCancelResult {
    canceled: "canceled"
});

/// Which release asset to download, the credential to download it under, and the request id that makes the download cancellable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseDownloadAssetRequest {
    /// The asset's `browser_download_url` from the release JSON: an HTTPS release download URL of a repository that `gitHubRelease.lookup` serves.
    pub asset_url: String,
    /// The credential the download runs under, carried opaquely because its shape is the host's own and the runtime only resolves a token from it. No credential travels: this selects one the runtime already holds. Omitted when the caller has no credential, which still succeeds against the public release repository at a lower rate limit.
    pub auth_info: Option<serde_json::Value>,
    /// Expected SHA-256 of the asset: 64 hex digits in either case, or the release asset's `digest` value as is (`sha256:` and the hex digits). When given and the downloaded bytes differ, the file is removed and the result is `digest-mismatch`.
    pub expected_sha256: Option<String>,
    /// Request id from `gitHubRelease.allocateRequestId`. An id that was never registered, or one already used, is refused rather than silently running uncancellable.
    pub request_id: i64,
    /// Snapshot request id from this connection. The download retains that snapshot's original credential source instead of authInfo. Missing, foreign, canceled, or revoked references fail without fallback. Omission preserves ordinary downloads. The download still uses its own requestId for progress and cancellation.
    pub snapshot_request_id: Option<i64>,
}
crate::record_serde::wire_record!(GitHubReleaseDownloadAssetRequest { asset_url: "assetUrl", auth_info: "authInfo" if is_none, expected_sha256: "expectedSha256" if is_none, request_id: "requestId", snapshot_request_id: "snapshotRequestId" if is_none });

/// Outcome of an asset download. Each field is present only for the kinds its description names.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseDownloadAssetResult {
    /// How the download ended.
    #[doc(hidden)]
    pub(crate) kind: GitHubReleaseDownloadKind,
    /// Why the download produced no file, phrased for a user, for `digest-mismatch` and `failed`.
    pub message: Option<String>,
    /// Absolute path of the downloaded file, for `downloaded`. The file is alone in a fresh directory that the caller deletes once it has used the file.
    pub path: Option<String>,
    /// SHA-256 of the downloaded bytes as lowercase hex, for `downloaded` and `digest-mismatch`.
    pub sha256: Option<String>,
    /// Size of the downloaded file in bytes, for `downloaded`.
    pub size_bytes: Option<i64>,
    /// HTTP status of the final response, for `http-error`.
    pub status: Option<i64>,
    /// HTTP status text of the final response, for `http-error`.
    pub status_text: Option<String>,
}
crate::record_serde::wire_record!(GitHubReleaseDownloadAssetResult { kind: "kind", message: "message" if is_none, path: "path" if is_none, sha256: "sha256" if is_none, size_bytes: "sizeBytes" if is_none, status: "status" if is_none, status_text: "statusText" if is_none });

/// The download whose progress to report.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseDownloadProgressRequest {
    /// Request id the download was started with.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubReleaseDownloadProgressRequest {
    request_id: "requestId"
});

/// Byte progress of a running download.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseDownloadProgressResult {
    /// Bytes written so far.
    pub received_bytes: i64,
    /// Size the server announced for the body. Omitted when it announced none.
    pub total_bytes: Option<i64>,
}
crate::record_serde::wire_record!(GitHubReleaseDownloadProgressResult { received_bytes: "receivedBytes", total_bytes: "totalBytes" if is_none });

/// Which release to look up, the credential to look it up under, and the request id that makes the lookup cancellable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseLookupRequest {
    /// The credential the lookup runs under, carried opaquely because its shape is the host's own and the runtime only resolves a token and a GitHub host from it. No credential travels: this selects one the runtime already holds. Omitted when the caller has no credential, which still succeeds against the public release repository at a lower rate limit.
    pub auth_info: Option<serde_json::Value>,
    /// Which release to look up.
    #[doc(hidden)]
    pub(crate) query: GitHubReleaseQuery,
    /// Request id from `gitHubRelease.allocateRequestId`. An id that was never registered, or one already used, is refused rather than silently running uncancellable.
    pub request_id: i64,
    /// The tag to look up. Required by the `tag` query and ignored by the others.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(GitHubReleaseLookupRequest { auth_info: "authInfo" if is_none, query: "query", request_id: "requestId", value: "value" if is_none });

/// Outcome of a release lookup. `releaseJson` is present only for `found`, and `message` only for the kinds that carry an explanation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseLookupResult {
    /// How the lookup ended.
    #[doc(hidden)]
    pub(crate) kind: GitHubReleaseLookupKind,
    /// Why the lookup produced no release, phrased for a user.
    pub message: Option<String>,
    /// The release as GitHub's own JSON, verbatim. Carried as text rather than a modelled type so the contract does not have to track every field of GitHub's release payload, and so a caller reads the same shape the REST API documents.
    pub release_json: Option<String>,
}
crate::record_serde::wire_record!(GitHubReleaseLookupResult { kind: "kind", message: "message" if is_none, release_json: "releaseJson" if is_none });

/// A freshly registered request id. Registering it before the work starts is what lets a cancel that races the request still find something to cancel.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseRequestIdResult {
    /// Request id to pass to `gitHubRelease.lookup` or `gitHubRelease.downloadAsset` and, to abandon the work, `gitHubRelease.cancel`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubReleaseRequestIdResult {
    request_id: "requestId"
});

/// An asset in the release snapshot. GitHub field names remain unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseSnapshotAsset {
    /// The published download URL.
    pub browser_download_url: String,
    /// The published asset filename.
    pub name: String,
}
crate::record_serde::wire_record!(GitHubReleaseSnapshotAsset {
    browser_download_url: "browser_download_url",
    name: "name"
});

/// Release facts needed for update selection and admission. GitHub field names remain unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseSnapshotEntry {
    /// The published release assets.
    #[doc(hidden)]
    pub(crate) assets: Vec<GitHubReleaseSnapshotAsset>,
    /// Whether GitHub marks this release as a draft.
    pub draft: bool,
    /// Whether GitHub marks this release as a prerelease.
    pub prerelease: bool,
    /// The release tag.
    pub tag_name: String,
}
crate::record_serde::wire_record!(GitHubReleaseSnapshotEntry {
    assets: "assets",
    draft: "draft",
    prerelease: "prerelease",
    tag_name: "tag_name"
});

/// Immutable options for one connection-owned release snapshot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseSnapshotRequest {
    /// The version embedded in the caller's build.
    pub build_version: String,
    /// Optional runtime version override. Invalid or noncanonical values do not replace buildVersion.
    pub cli_version_override: Option<String>,
    /// The embedded package identity. Omission uses the existing development-version fallback rule.
    #[doc(hidden)]
    pub(crate) package_identity: Option<GitHubReleasePackageIdentity>,
    /// An unused id from gitHubRelease.allocateRequestId. Reuse the same id and options to read the same snapshot.
    pub request_id: i64,
    /// The release channel used for update selection.
    #[doc(hidden)]
    pub(crate) update_channel: GitHubReleaseUpdateChannel,
}
crate::record_serde::wire_record!(GitHubReleaseSnapshotRequest { build_version: "buildVersion", cli_version_override: "cliVersionOverride" if is_none, package_identity: "packageIdentity" if is_none, request_id: "requestId", update_channel: "updateChannel" });

/// The release lookup succeeded.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseSnapshotResultSuccess {
    /// The newest release in the selected update channel, when one exists.
    #[doc(hidden)]
    pub(crate) automatic_update_candidate: Option<GitHubReleaseSnapshotEntry>,
    /// Whether the running release is eligible for release admission.
    pub eligible: bool,
    /// Up to three stable releases, newest first.
    #[doc(hidden)]
    pub(crate) latest_stable_releases: Vec<GitHubReleaseSnapshotEntry>,
    /// The original lookup duration in milliseconds.
    pub lookup_duration_ms: f64,
    pub outcome: GitHubReleaseSnapshotResultSuccessOutcome,
    /// Prereleases newer than the newest stable release, newest first.
    #[doc(hidden)]
    pub(crate) prereleases_newer_than_latest_stable: Vec<GitHubReleaseSnapshotEntry>,
    /// The effective running version after the existing override rules.
    pub running_version: String,
    /// The original update channel.
    #[doc(hidden)]
    pub(crate) update_channel: GitHubReleaseUpdateChannel,
}
crate::record_serde::wire_record!(GitHubReleaseSnapshotResultSuccess { automatic_update_candidate: "automaticUpdateCandidate" if is_none, eligible: "eligible", latest_stable_releases: "latestStableReleases", lookup_duration_ms: "lookupDurationMs", outcome: "outcome", prereleases_newer_than_latest_stable: "prereleasesNewerThanLatestStable", running_version: "runningVersion", update_channel: "updateChannel" });

/// The release lookup failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseSnapshotResultFailed {
    /// The original user-facing lookup failure.
    pub error: String,
    /// The original lookup duration in milliseconds.
    pub lookup_duration_ms: f64,
    pub outcome: GitHubReleaseSnapshotResultFailedOutcome,
    /// The original update channel.
    #[doc(hidden)]
    pub(crate) update_channel: GitHubReleaseUpdateChannel,
}
crate::record_serde::wire_record!(GitHubReleaseSnapshotResultFailed {
    error: "error",
    lookup_duration_ms: "lookupDurationMs",
    outcome: "outcome",
    update_channel: "updateChannel"
});

/// Working-tree path whose owning GitHub repository should be resolved.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryAtPathRequest {
    /// Absolute path to a directory inside the git working tree to resolve.
    pub path: String,
}
crate::record_serde::wire_record!(GitHubRepositoryAtPathRequest { path: "path" });

/// Owner, name, and host of a GitHub repository, as resolved from a git remote URL.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubRepositoryIdentity {
    /// Host the remote points at, for example `github.com` or a GitHub Enterprise hostname.
    pub host: String,
    /// Repository name, without the owner prefix or the `.git` suffix.
    pub name: String,
    /// Repository owner login (user or organization).
    pub owner: String,
}
crate::record_serde::wire_record!(GitHubRepositoryIdentity {
    host: "host",
    name: "name",
    owner: "owner"
});

/// The GitHub repository that owns the requested path, when the selected remote (`origin`, else the first) is on a GitHub host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryAtPathResult {
    /// Resolved repository identity, or null when the selected remote resolves to no GitHub host.
    pub repository: Option<GitHubRepositoryIdentity>,
}
crate::record_serde::wire_record!(GitHubRepositoryAtPathResult { repository: "repository" if is_none });

/// Selects a GitHub repository by its numeric repository id, and the credential to look it up under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryByIdRequest {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the lookup runs under, carried opaquely because its shape is the host's own and the runtime only resolves a token and a GitHub host from it. Required: the endpoint answers per-account, so a repository invisible to this credential is reported as absent rather than refused.
    pub auth_info: serde_json::Value,
    /// GitHub's own numeric identifier for the repository, as carried by API payloads that reference a repository by id rather than by owner and name.
    pub repository_id: i64,
}
crate::record_serde::wire_record!(GitHubRepositoryByIdRequest { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", repository_id: "repositoryId" });

/// The repository the requested id names, when the credential can see one.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryByIdResult {
    /// Owner and name of the repository, or null when the id names no repository visible to the credential.
    pub repository: Option<GitHubRepositoryOwnerName>,
}
crate::record_serde::wire_record!(GitHubRepositoryByIdResult { repository: "repository" if is_none });

/// Client environment metadata describing the process that produced a telemetry event.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubTelemetryClientInfo {
    /// Copilot CLI version string.
    pub cli_version: String,
    /// Name of the client application.
    pub client_name: Option<String>,
    /// Type of client.
    pub client_type: Option<String>,
    /// Copilot subscription plan, when known.
    pub copilot_plan: Option<String>,
    /// Number of logical CPU cores on the host.
    pub cpu_count: Option<i64>,
    /// Distinct CPU model names for the host, comma-separated.
    pub cpu_model: Option<String>,
    /// Stable machine identifier for the device.
    pub dev_device_id: Option<String>,
    /// Whether the user is a GitHub/Microsoft staff member.
    pub is_staff: Option<bool>,
    /// Node.js runtime version string.
    pub node_version: String,
    /// Operating system architecture (e.g. arm64, x64).
    pub os_arch: String,
    /// Operating system platform (e.g. darwin, linux, win32).
    pub os_platform: String,
    /// Operating system version string.
    pub os_version: String,
}
crate::record_serde::wire_record!(GitHubTelemetryClientInfo { cli_version: "cli_version", client_name: "client_name" if is_none, client_type: "client_type" if is_none, copilot_plan: "copilot_plan" if is_none, cpu_count: "cpu_count" if is_none, cpu_model: "cpu_model" if is_none, dev_device_id: "dev_device_id" if is_none, is_staff: "is_staff" if is_none, node_version: "node_version", os_arch: "os_arch", os_platform: "os_platform", os_version: "os_version" });

/// A single telemetry event in the runtime's native GitHub-shaped telemetry format, forwarded verbatim to opted-in hosts. The `restricted` flag on the enclosing GitHubTelemetryNotification distinguishes standard from restricted events; the payload shape is identical for both.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubTelemetryEvent {
    /// Client environment metadata.
    pub client: Option<GitHubTelemetryClientInfo>,
    /// Copilot tracking ID for user-level attribution.
    pub copilot_tracking_id: Option<String>,
    /// Timestamp when the event was created (ISO 8601 format).
    pub created_at: Option<String>,
    /// Experiment assignment context.
    pub exp_assignment_context: Option<String>,
    /// Feature flags enabled for this session, as a map from flag to value.
    pub features: Option<HashMap<String, String>>,
    /// Event type/kind (e.g. get_completion_with_tools_turn, tool_call_executed).
    pub kind: String,
    /// Numeric metrics as a map from key to value.
    pub metrics: HashMap<String, f64>,
    /// Reference to the model call that produced this event.
    pub model_call_id: Option<String>,
    /// String-valued properties as a map from key to value.
    pub properties: HashMap<String, String>,
    /// Session identifier the event belongs to.
    pub session_id: Option<String>,
}
crate::record_serde::wire_record!(GitHubTelemetryEvent { client: "client" if is_none, copilot_tracking_id: "copilot_tracking_id" if is_none, created_at: "created_at" if is_none, exp_assignment_context: "exp_assignment_context" if is_none, features: "features" if is_none, kind: "kind", metrics: "metrics", model_call_id: "model_call_id" if is_none, properties: "properties", session_id: "session_id" if is_none });

/// Payload for a `gitHubTelemetry.event` notification: a single GitHub telemetry event the runtime forwards to a host connection that opted into telemetry forwarding during the `server.connect` handshake.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubTelemetryNotification {
    /// The telemetry event, in the runtime's native GitHub-shaped telemetry format.
    pub event: GitHubTelemetryEvent,
    /// Whether this is a restricted telemetry event (cli.restricted_telemetry). Hosts must route restricted events to first-party Microsoft stores only.
    pub restricted: bool,
    /// Session the telemetry event belongs to, when it is session-scoped. Omitted for sessionless events (for example, `server.sendTelemetry` calls with no session id), which are still forwarded to opted-in connections.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(GitHubTelemetryNotification { event: "event", restricted: "restricted", session_id: "sessionId" if is_none });

/// Asks the SDK client to acquire a GitHub access token from an opaque callback registration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubTokenAcquireRequest {
    /// Effective GitHub host for which the callback must return a token.
    pub host: String,
    /// Why the runtime is requesting a GitHub credential.
    pub reason: GitHubTokenAcquireReason,
    /// Opaque identifier generated by the SDK for this callback registration.
    pub registration_id: String,
    /// Session receiving the token. Absent only before a cloud session has been assigned its id.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(GitHubTokenAcquireRequest { host: "host", reason: "reason", registration_id: "registrationId", session_id: "sessionId" if is_none });

#[derive(Debug, Clone, Default)]
pub struct GitHubTokenAcquireResultToken {
    /// GitHub access token acquired by the SDK host.
    pub access_token: String,
    /// Remaining token lifetime in seconds when callback execution completes. It must exceed the one-hour preflight refresh threshold, and still exceed it after the runtime validates the token: validation time counts against this lifetime, so a value just above 3600 can be rejected.
    pub expires_in: i64,
    /// GitHub credential response variant discriminator.
    pub kind: GitHubTokenAcquireResultTokenKind,
    /// OAuth token type. Defaults to bearer when omitted.
    pub token_type: Option<String>,
}
crate::record_serde::wire_record!(GitHubTokenAcquireResultToken { access_token: "accessToken", expires_in: "expiresIn", kind: "kind", token_type: "tokenType" if is_none });

#[derive(Debug, Clone, Default)]
pub struct GitHubTokenAcquireResultCancelled {
    /// GitHub credential response variant discriminator.
    pub kind: GitHubTokenAcquireResultCancelledKind,
}
crate::record_serde::wire_record!(GitHubTokenAcquireResultCancelled { kind: "kind" });

/// Carry persisted trust from a repository root to a new worktree, without granting trust from a narrower subdirectory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitInheritTrustRequest {
    /// Host configuration directory. Omission uses the normal configuration root.
    pub config_dir: Option<String>,
    /// Destination worktree directory.
    pub destination: String,
    /// Source checkout directory; resolved to the main repository root before the trust check.
    pub source: String,
}
crate::record_serde::wire_record!(GitInheritTrustRequest { config_dir: "configDir" if is_none, destination: "destination", source: "source" });

/// A worktree with restored local changes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitMovedWorktree {
    /// Actual branch name.
    pub branch: String,
    /// Untracked nested repositories that remain in the source checkout.
    pub left_behind_repositories: Vec<String>,
    /// Submodules whose local changes remain in the source checkout.
    pub left_behind_submodules: Vec<String>,
    /// Whether changes moved.
    pub moved_changes: bool,
    /// Destination directory.
    pub path: String,
}
crate::record_serde::wire_record!(GitMovedWorktree {
    branch: "branch",
    left_behind_repositories: "leftBehindRepositories",
    left_behind_submodules: "leftBehindSubmodules",
    moved_changes: "movedChanges",
    path: "path"
});

/// A worktree to receive checkout changes from the current HEAD.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitMoveWorktreeRequest {
    /// Permit a numeric suffix on branch collisions.
    pub allow_suffix: bool,
    /// Requested branch name.
    pub branch: String,
    /// Absolute repository root.
    pub git_root: String,
    /// Worktree path template. Omission uses the normal sibling layout.
    pub path_template: Option<String>,
}
crate::record_serde::wire_record!(GitMoveWorktreeRequest { allow_suffix: "allowSuffix", branch: "branch", git_root: "gitRoot", path_template: "pathTemplate" if is_none });

/// A completed move or an actionable recovery message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitMoveWorktreeResult {
    /// Recovery instructions when the move failed.
    pub failure_message: Option<String>,
    /// Successful move, when present.
    pub moved: Option<GitMovedWorktree>,
}
crate::record_serde::wire_record!(GitMoveWorktreeResult { failure_message: "failureMessage" if is_none, moved: "moved" if is_none });

/// A named worktree to create, reuse, or check for collisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitNamedWorktreeRequest {
    /// Base ref. Omission resolves the default branch with a HEAD fallback.
    pub base_ref: Option<String>,
    /// Absolute repository root.
    pub git_root: String,
    /// Worktree branch name.
    pub name: String,
    /// Worktree path template.
    pub path_template: Option<String>,
}
crate::record_serde::wire_record!(GitNamedWorktreeRequest { base_ref: "baseRef" if is_none, git_root: "gitRoot", name: "name", path_template: "pathTemplate" if is_none });

/// A Git value, or null when the requested identity does not exist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitOptionalStringResult {
    /// The resolved value. Null represents an absent root or branch identity.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(GitOptionalStringResult { value: "value" if is_none });

/// A created or reused worktree.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitPreparedWorktree {
    /// Actual branch name.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
    /// Whether an existing worktree was reused.
    pub reused: bool,
}
crate::record_serde::wire_record!(GitPreparedWorktree {
    branch: "branch",
    path: "path",
    reused: "reused"
});

/// Read a file from the HEAD tree, without changing the checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitReadHeadFileRequest {
    /// Absolute checkout directory.
    pub cwd: String,
    /// Repository-relative file path.
    pub path: String,
}
crate::record_serde::wire_record!(GitReadHeadFileRequest {
    cwd: "cwd",
    path: "path"
});

/// Known forge hosts reachable through repository remotes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRemoteHostDetection {
    /// The Azure DevOps URL, when present.
    pub azure_dev_ops_url: Option<String>,
    /// Whether a remote points to Azure DevOps.
    pub has_azure_dev_ops: bool,
    /// Whether a remote points to GitHub.
    pub has_git_hub: bool,
}
crate::record_serde::wire_record!(GitRemoteHostDetection { azure_dev_ops_url: "azureDevOpsUrl" if is_none, has_azure_dev_ops: "hasAzureDevOps", has_git_hub: "hasGitHub" });

/// A GitHub repository one of a working tree's remotes points at.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitRemoteRepository {
    /// GitHub host serving the repository, which is not `github.com` for a GitHub Enterprise remote.
    pub host: String,
    /// Repository name, without the owner.
    pub name: String,
    /// Account or organization owning the repository.
    pub owner: String,
    /// Name of the first remote that produced this distinct repository entry, such as `origin` or `upstream`.
    pub remote_name: String,
}
crate::record_serde::wire_record!(GitRemoteRepository {
    host: "host",
    name: "name",
    owner: "owner",
    remote_name: "remoteName"
});

/// The remote names configured in a checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRemotesResult {
    /// Remote names in Git's order.
    pub remotes: Vec<String>,
}
crate::record_serde::wire_record!(GitRemotesResult { remotes: "remotes" });

/// Repository identity and forge host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRepoIdentifierInfo {
    /// Forge host.
    pub host: String,
    /// Forge type: github or ado.
    pub host_type: SessionWorkingDirectoryContextHostType,
    /// Repository identifier.
    pub identifier: String,
}
crate::record_serde::wire_record!(GitRepoIdentifierInfo {
    host: "host",
    host_type: "hostType",
    identifier: "identifier"
});

/// Repository identity, or null when no supported forge is configured.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRepoIdentifierResult {
    /// The detected repository.
    pub repository: Option<GitRepoIdentifierInfo>,
}
crate::record_serde::wire_record!(GitRepoIdentifierResult { repository: "repository" if is_none });

/// Git working tree whose GitHub remotes should be listed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitReposFromRemotesRequest {
    /// Absolute path to the root of the git working tree.
    pub git_root: String,
}
crate::record_serde::wire_record!(GitReposFromRemotesRequest {
    git_root: "gitRoot"
});

/// The GitHub repositories a working tree's remotes point at.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitReposFromRemotesResult {
    /// One entry per distinct GitHub repository, in the order git reports the first remote for each repository. Empty when no remote points at a GitHub host, which a caller should read as `not connected to GitHub`. Failing to read the remotes is an error, not an empty list.
    #[doc(hidden)]
    pub(crate) repositories: Vec<GitRemoteRepository>,
}
crate::record_serde::wire_record!(GitReposFromRemotesResult {
    repositories: "repositories"
});

/// A worktree for an issue or pull request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitResourceWorktreeRequest {
    /// Absolute repository root.
    pub git_root: String,
    /// Issue or pull request number.
    pub number: i64,
    /// Worktree path template.
    pub path_template: Option<String>,
    /// Remote to fetch. Omission uses the normal remote.
    pub remote: Option<String>,
    /// Resource title used in the branch name.
    pub title: String,
}
crate::record_serde::wire_record!(GitResourceWorktreeRequest { git_root: "gitRoot", number: "number", path_template: "pathTemplate" if is_none, remote: "remote" if is_none, title: "title" });

/// Repository discovery, including the explicit not-found result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRootResult {
    /// Whether the directory belongs to a repository.
    pub found: bool,
    /// Root path, or the resolver's empty path when no repository exists.
    pub git_root: String,
}
crate::record_serde::wire_record!(GitRootResult {
    found: "found",
    git_root: "gitRoot"
});

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitStringResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitStringResult { value: "value" });

/// Tracked and untracked changes in a checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorkingTreeStatus {
    /// Whether the index has staged changes.
    pub has_staged_changes: bool,
    /// Whether tracked files have unstaged changes.
    pub has_unstaged_changes: bool,
    /// Whether untracked files exist.
    pub has_untracked_files: bool,
}
crate::record_serde::wire_record!(GitWorkingTreeStatus {
    has_staged_changes: "hasStagedChanges",
    has_unstaged_changes: "hasUnstagedChanges",
    has_untracked_files: "hasUntrackedFiles"
});

/// Working-tree status and explicit failure policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorkingTreeStatusRequest {
    /// Absolute working-tree directory.
    pub cwd: String,
    /// Return a clean status on failure. Defaults to false, which propagates errors.
    pub fallback_to_clean: Option<bool>,
}
crate::record_serde::wire_record!(GitWorkingTreeStatusRequest { cwd: "cwd", fallback_to_clean: "fallbackToClean" if is_none });

/// A worktree to create from a selected base.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorktreeRequest {
    /// Permit a numeric suffix on branch collisions.
    pub allow_suffix: bool,
    /// Base ref. Omission resolves the default branch, with a HEAD fallback.
    pub base_ref: Option<String>,
    /// Requested branch name.
    pub branch: String,
    /// Absolute repository root.
    pub git_root: String,
    /// Worktree path template. Omission uses the normal sibling layout.
    pub path_template: Option<String>,
}
crate::record_serde::wire_record!(GitWorktreeRequest { allow_suffix: "allowSuffix", base_ref: "baseRef" if is_none, branch: "branch", git_root: "gitRoot", path_template: "pathTemplate" if is_none });

/// Checkout changes to summarize.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorktreeSummaryRequest {
    /// Absolute repository root.
    pub git_root: String,
    /// Maximum number of files to include.
    pub max_files: i64,
}
crate::record_serde::wire_record!(GitWorktreeSummaryRequest {
    git_root: "gitRoot",
    max_files: "maxFiles"
});

/// Attaches Mission Control polling and steering to this locally retained remote-session facade. Credentials remain with the runtime owner and resolve for each request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RemoteAttachMissionControlRequest {
    /// Registered credential owner, when the host uses one.
    pub auth_context_id: Option<String>,
    /// Identity whose credential and endpoints the runtime resolves.
    pub auth_info: AuthIdentity,
    /// Explicit Mission Control endpoint; omission derives it from the identity.
    pub base_url: Option<String>,
    /// Explicit frontend origin; omission derives it from the identity.
    pub frontend_base_url: Option<String>,
    /// Integration identifier sent to Mission Control.
    pub integration_id: String,
    /// Previously observed events, including events excluded from replay.
    pub observed_events_json: String,
    /// Poll interval in milliseconds; omission uses the runtime default.
    pub poll_interval_ms: Option<i64>,
    /// Public facade identifier used for the host's frontend URL registration.
    pub public_session_id: String,
    /// Whether this facade permits steering.
    pub remote_steerable: bool,
    /// Repository name associated with the remote task.
    pub repository_name: Option<String>,
    /// Repository owner associated with the remote task.
    pub repository_owner: Option<String>,
    /// Whether the attachment provides remote JSON-RPC commands.
    pub rpc_enabled: bool,
    /// Cloud task whose events and commands this session follows.
    pub task_id: String,
}
crate::record_serde::wire_record!(RemoteAttachMissionControlRequest { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, frontend_base_url: "frontendBaseUrl" if is_none, integration_id: "integrationId", observed_events_json: "observedEventsJson", poll_interval_ms: "pollIntervalMs" if is_none, public_session_id: "publicSessionId", remote_steerable: "remoteSteerable", repository_name: "repositoryName" if is_none, repository_owner: "repositoryOwner" if is_none, rpc_enabled: "rpcEnabled", task_id: "taskId" });

/// Reports whether this call created the attachment. An existing attachment remains unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RemoteAttachMissionControlResult {
    /// True when this call starts polling; false when the session already has an attachment.
    pub started: bool,
}
crate::record_serde::wire_record!(RemoteAttachMissionControlResult { started: "started" });

/// Remote control is connected to a local session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStatusActive {
    /// Session id remote control is pointed at.
    pub attached_session_id: String,
    /// True while a read-only/session-sync export is deferred, awaiting the first `user.message` before its MC session exists. Marked internal: this field is excluded from the public SDK surface and is populated only on the CLI in-process path.
    #[doc(hidden)]
    pub(crate) awaiting_first_message: Option<bool>,
    /// MC frontend URL for this session, when known.
    pub frontend_url: Option<String>,
    /// Whether the MC session may steer this session.
    pub is_steerable: bool,
    /// In-process prompt-manager handle (CLI-only optimization). Marked internal: this field is excluded from the public SDK surface. Retained as an optional compatibility field; native remote control does not populate or consume it.
    #[doc(hidden)]
    pub(crate) prompt_manager: Option<serde_json::Value>,
    /// Remote control state tag: active.
    pub state: RemoteControlStatusActiveState,
}
crate::record_serde::wire_record!(RemoteControlStatusActive { attached_session_id: "attachedSessionId", awaiting_first_message: "awaitingFirstMessage" if is_none, frontend_url: "frontendUrl" if is_none, is_steerable: "isSteerable", prompt_manager: "promptManager" if is_none, state: "state" });

/// Remote control is in the middle of initial setup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStatusConnecting {
    /// Session id the connection is attaching to.
    pub attached_session_id: String,
    /// Remote control state tag: connecting.
    pub state: RemoteControlStatusConnectingState,
}
crate::record_serde::wire_record!(RemoteControlStatusConnecting {
    attached_session_id: "attachedSessionId",
    state: "state"
});

/// The last setup attempt failed. The singleton is otherwise off.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStatusError {
    /// Session id the failing setup attempt targeted, when known.
    pub attached_session_id: Option<String>,
    /// Human-readable error message from the last setup attempt.
    pub error: String,
    /// Remote control state tag: setup failed.
    pub state: RemoteControlStatusErrorState,
}
crate::record_serde::wire_record!(RemoteControlStatusError { attached_session_id: "attachedSessionId" if is_none, error: "error", state: "state" });

/// Remote control is not connected.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStatusOff {
    /// Remote control state tag: not connected.
    pub state: RemoteControlStatusOffState,
}
crate::record_serde::wire_record!(RemoteControlStatusOff { state: "state" });

/// Wrapper for the singleton's current status.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStatusResult {
    /// State of the runtime-managed remote-control singleton.
    pub status: serde_json::Value,
}
crate::record_serde::wire_record!(RemoteControlStatusResult { status: "status" });

/// Outcome of a stopRemoteControl call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlStopResult {
    /// State of the runtime-managed remote-control singleton.
    pub status: serde_json::Value,
    /// Whether the singleton was actually torn down by this call.
    pub stopped: bool,
}
crate::record_serde::wire_record!(RemoteControlStopResult {
    status: "status",
    stopped: "stopped"
});

/// Outcome of a transferRemoteControl call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteControlTransferResult {
    /// State of the runtime-managed remote-control singleton.
    pub status: serde_json::Value,
    /// Whether the rebinding actually happened.
    pub transferred: bool,
}
crate::record_serde::wire_record!(RemoteControlTransferResult {
    status: "status",
    transferred: "transferred"
});

/// Optional remote session mode ("off", "export", or "on"); defaults to enabling both export and remote steering.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteEnableRequest {
    /// Per-session remote mode. "off" disables remote, "export" exports session events to GitHub without enabling remote steering, "on" enables both export and remote steering.
    pub mode: Option<RemoteSessionMode>,
}
crate::record_serde::wire_record!(RemoteEnableRequest { mode: "mode" if is_none });

/// GitHub URL and remote export and steering state, with an optional warning when cloud session storage policy refuses export.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteEnableResult {
    /// Whether remote steering is enabled
    pub remote_steerable: bool,
    /// Whether this request accepted or retained remote export. False when export is disabled or refused; initialization may still be pending when true.
    pub started: Option<bool>,
    /// GitHub frontend URL for this session
    pub url: Option<String>,
    /// Warning explaining why cloud session storage policy prevented export. Absent when no policy warning applies.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(RemoteEnableResult { remote_steerable: "remoteSteerable", started: "started" if is_none, url: "url" if is_none, warning: "warning" if is_none });

/// New remote-steerability state to persist as a `session.remote_steerable_changed` event.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteNotifySteerableChangedRequest {
    /// Whether the session now supports remote steering via GitHub. The runtime persists this as a `session.remote_steerable_changed` event so resume/replay sees the up-to-date capability.
    pub remote_steerable: bool,
}
crate::record_serde::wire_record!(RemoteNotifySteerableChangedRequest {
    remote_steerable: "remoteSteerable"
});

/// Persist a steerability change as a `session.remote_steerable_changed` event. Used by the host (CLI / SDK consumer) when it has just finished enabling or disabling steering on a remote exporter that the runtime does not directly own.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteNotifySteerableChangedResult {}

/// Remote session connection result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RemoteSessionConnectionResult {
    /// Metadata for a connected remote session.
    pub metadata: ConnectedRemoteSessionMetadata,
    /// SDK session ID for the connected remote session.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(RemoteSessionConnectionResult {
    metadata: "metadata",
    session_id: "sessionId"
});

/// Agents discovered across user, project, plugin, and remote sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ServerAgentList {
    /// All discovered agents across all sources
    pub agents: Vec<AgentInfo>,
}
crate::record_serde::wire_record!(ServerAgentList { agents: "agents" });

/// Instruction sources discovered across user, repository, and plugin sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ServerInstructionSourceList {
    /// All discovered instruction sources
    pub sources: Vec<InstructionSource>,
}
crate::record_serde::wire_record!(ServerInstructionSourceList { sources: "sources" });

/// Skills discovered across global and project sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ServerSkillList {
    /// Messages for skills that failed to load (e.g. malformed SKILL.md). Empty when host skills are excluded so host-local paths are not disclosed to multitenant callers.
    pub errors: Option<Vec<String>>,
    /// All discovered skills across all sources
    pub skills: Vec<ServerSkill>,
}
crate::record_serde::wire_record!(ServerSkillList { errors: "errors" if is_none, skills: "skills" });

/// Workspace diff result for the requested mode.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspaceDiffResult {
    /// Default branch used for a branch diff, when branch mode was requested.
    pub base_branch: Option<String>,
    /// Changed files and their unified diffs.
    pub changes: Vec<WorkspaceDiffFileChange>,
    /// Whether the requested diff fell back to unstaged changes, either because branch diff failed or session diff was unavailable.
    pub is_fallback: bool,
    /// Effective mode used for the returned changes.
    pub mode: WorkspaceDiffMode,
    /// Diff mode requested by the client.
    pub requested_mode: WorkspaceDiffMode,
    /// Why the session diff could not be produced, when applicable. Set only when `session` mode was requested and `isFallback` is true, so a client can tell the permanent `file-change-tracking-disabled` apart from the transient `session-busy`, which the same request answers once the session settles. Never set for `unstaged` or `branch` mode, and never `unsupported-remote-session`: a remote session's captures live on its own host, so a `session`-mode diff is rejected for one rather than answered with a controller-side fallback.
    pub unavailable_reason: Option<HistoryRewindUnavailableReason>,
}
crate::record_serde::wire_record!(WorkspaceDiffResult { base_branch: "baseBranch" if is_none, changes: "changes", is_fallback: "isFallback", mode: "mode", requested_mode: "requestedMode", unavailable_reason: "unavailableReason" if is_none });

/// Compaction summary checkpoint to persist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesAddSummaryRequest {
    /// Markdown summary content to persist.
    pub content: String,
    /// Summary title shown in checkpoint listings.
    pub title: String,
}
crate::record_serde::wire_record!(WorkspacesAddSummaryRequest {
    content: "content",
    title: "title"
});

/// Persisted summary metadata and refreshed workspace metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesAddSummaryResult {
    /// Metadata for the persisted summary.
    pub summary: Option<serde_json::Value>,
    /// Refreshed metadata for the containing workspace.
    pub workspace: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkspacesAddSummaryResult { summary: "summary" if is_none, workspace: "workspace" if is_none });

/// Whether the autopilot objective file exists.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesAutopilotObjectiveExistsResult {
    /// True when the objective file exists.
    pub exists: bool,
}
crate::record_serde::wire_record!(WorkspacesAutopilotObjectiveExistsResult { exists: "exists" });

/// Directory to create within the session workspace files directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesCreateDirectoryRequest {
    /// Slash-separated relative path within the workspace files directory
    pub path: String,
    /// Whether to create missing parent directories. Defaults to false.
    pub recursive: Option<bool>,
}
crate::record_serde::wire_record!(WorkspacesCreateDirectoryRequest { path: "path", recursive: "recursive" if is_none });

/// Relative path and UTF-8 content for the workspace file to create or overwrite.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesCreateFileRequest {
    /// File content to write as a UTF-8 string
    pub content: String,
    /// Slash-separated relative path within the workspace files directory
    pub path: String,
}
crate::record_serde::wire_record!(WorkspacesCreateFileRequest {
    content: "content",
    path: "path"
});

/// Result of deleting the autopilot objective file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesDeleteAutopilotObjectiveResult {
    /// True when a file was deleted.
    pub deleted: bool,
}
crate::record_serde::wire_record!(WorkspacesDeleteAutopilotObjectiveResult { deleted: "deleted" });

/// Parameters for computing a workspace diff.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesDiffRequest {
    /// When true, ignore whitespace-only changes (git `--ignore-all-space`). Defaults to false.
    pub ignore_whitespace: Option<bool>,
    /// Include the full before/after text used to compute each session diff. Defaults to false; true is accepted only for session mode. Existing capture/read limits still apply, and binary or unavailable contents are not returned. This can substantially increase response size.
    pub include_contents: Option<bool>,
    /// Diff mode requested by the client.
    pub mode: WorkspaceDiffMode,
}
crate::record_serde::wire_record!(WorkspacesDiffRequest { ignore_whitespace: "ignoreWhitespace" if is_none, include_contents: "includeContents" if is_none, mode: "mode" });

/// Optional session context used when creating a local workspace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesEnsureRequest {
    /// Opaque workspace context supplied by the session host.
    pub context: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkspacesEnsureRequest { context: "context" if is_none });

#[derive(Debug, Clone, Default)]
pub struct WorkspacesGetWorkspaceResultWorkspace {
    /// Current Git branch.
    pub branch: Option<String>,
    /// Whether the per-session Chronicle upgrade prompt was dismissed for the workspace.
    pub chronicle_sync_dismissed: Option<bool>,
    /// Name of the client that created the workspace.
    pub client_name: Option<String>,
    /// Timestamp when the workspace was created.
    pub created_at: Option<String>,
    /// Current working directory associated with the workspace.
    pub cwd: Option<String>,
    /// Git repository root associated with the workspace.
    pub git_root: Option<String>,
    /// Allowed values for the `WorkspacesWorkspaceDetailsHostType` enumeration.
    pub host_type: Option<WorkspacesWorkspaceDetailsHostType>,
    /// Stable workspace identifier.
    pub id: String,
    /// Most recent Mission Control event identifier observed for the workspace.
    pub mc_last_event_id: Option<String>,
    /// Mission Control session identifier associated with the workspace.
    pub mc_session_id: Option<String>,
    /// Mission Control task identifier associated with the workspace.
    pub mc_task_id: Option<String>,
    /// Workspace display name.
    pub name: Option<String>,
    /// Whether the workspace session can be steered remotely.
    pub remote_steerable: Option<bool>,
    /// Repository identifier associated with the workspace.
    pub repository: Option<String>,
    /// Number of persisted summaries in the workspace.
    pub summary_count: Option<i64>,
    /// Timestamp when the workspace was last updated.
    pub updated_at: Option<String>,
    /// Whether the workspace name was explicitly chosen by the user.
    pub user_named: Option<bool>,
}
crate::record_serde::wire_record!(WorkspacesGetWorkspaceResultWorkspace { branch: "branch" if is_none, chronicle_sync_dismissed: "chronicle_sync_dismissed" if is_none, client_name: "client_name" if is_none, created_at: "created_at" if is_none, cwd: "cwd" if is_none, git_root: "git_root" if is_none, host_type: "host_type" if is_none, id: "id", mc_last_event_id: "mc_last_event_id" if is_none, mc_session_id: "mc_session_id" if is_none, mc_task_id: "mc_task_id" if is_none, name: "name" if is_none, remote_steerable: "remote_steerable" if is_none, repository: "repository" if is_none, summary_count: "summary_count" if is_none, updated_at: "updated_at" if is_none, user_named: "user_named" if is_none });

/// Current workspace metadata for the session, including its absolute filesystem path when available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesGetWorkspaceResult {
    /// Absolute filesystem path to the workspace directory. Omitted when the session has no workspace (e.g. remote sessions).
    pub path: Option<String>,
    /// Current workspace metadata, or null if not available
    pub workspace: Option<WorkspacesGetWorkspaceResultWorkspace>,
}
crate::record_serde::wire_record!(WorkspacesGetWorkspaceResult { path: "path" if is_none, workspace: "workspace" });

/// Workspace checkpoints in chronological order; empty when the workspace is not enabled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesListCheckpointsResult {
    /// Workspace checkpoints in chronological order. Empty when workspace is not enabled.
    pub checkpoints: Vec<WorkspacesCheckpoints>,
}
crate::record_serde::wire_record!(WorkspacesListCheckpointsResult {
    checkpoints: "checkpoints"
});

/// Relative paths of files stored in the session workspace files directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesListFilesResult {
    /// Slash-separated relative file paths in the workspace files directory
    pub files: Vec<String>,
}
crate::record_serde::wire_record!(WorkspacesListFilesResult { files: "files" });

/// Autopilot objective file content, or null when missing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesReadAutopilotObjectiveResult {
    /// Autopilot objective file content, or null when missing.
    pub content: Option<String>,
}
crate::record_serde::wire_record!(WorkspacesReadAutopilotObjectiveResult { content: "content" });

/// Checkpoint number to read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesReadCheckpointRequest {
    /// Checkpoint number to read
    pub number: i64,
}
crate::record_serde::wire_record!(WorkspacesReadCheckpointRequest { number: "number" });

/// Checkpoint content as a UTF-8 string, or null when the checkpoint or workspace is missing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesReadCheckpointResult {
    /// Checkpoint content as a UTF-8 string, or null when the checkpoint or workspace is missing
    pub content: Option<String>,
}
crate::record_serde::wire_record!(WorkspacesReadCheckpointResult { content: "content" });

/// Relative path of the workspace file to read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesReadFileRequest {
    /// Slash-separated relative path within the workspace files directory
    pub path: String,
}
crate::record_serde::wire_record!(WorkspacesReadFileRequest { path: "path" });

/// Contents of the requested workspace file as a UTF-8 string.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesReadFileResult {
    /// File content as a UTF-8 string
    pub content: String,
}
crate::record_serde::wire_record!(WorkspacesReadFileResult { content: "content" });

/// File or directory to remove from the session workspace files directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesRemovePathRequest {
    /// Whether a missing path should be treated as success. Defaults to false.
    pub force: Option<bool>,
    /// Slash-separated relative path within the workspace files directory
    pub path: String,
    /// Whether to remove directory contents recursively. Defaults to false.
    pub recursive: Option<bool>,
}
crate::record_serde::wire_record!(WorkspacesRemovePathRequest { force: "force" if is_none, path: "path", recursive: "recursive" if is_none });

/// Source and destination paths for a rename within the session workspace files directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesRenamePathRequest {
    /// Slash-separated destination path relative to the workspace files directory
    pub destination: String,
    /// Slash-separated source path relative to the workspace files directory
    pub source: String,
}
crate::record_serde::wire_record!(WorkspacesRenamePathRequest {
    destination: "destination",
    source: "source"
});

/// Pasted content to save as a UTF-8 file in the session workspace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesSaveLargePasteRequest {
    /// Pasted content to save as a UTF-8 file
    pub content: String,
}
crate::record_serde::wire_record!(WorkspacesSaveLargePasteRequest { content: "content" });

#[derive(Debug, Clone, Default)]
pub struct WorkspacesSaveLargePasteResultSaved {
    /// Filename within the workspace files directory
    pub filename: String,
    /// Absolute filesystem path to the saved paste file
    pub file_path: String,
    /// Size of the saved file in bytes
    pub size_bytes: i64,
}
crate::record_serde::wire_record!(WorkspacesSaveLargePasteResultSaved {
    filename: "filename",
    file_path: "filePath",
    size_bytes: "sizeBytes"
});

/// Descriptor for the saved paste file, or null when the workspace is unavailable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesSaveLargePasteResult {
    /// Saved-paste descriptor, or null when the workspace is unavailable (e.g. CCA runtime, non-infinite sessions, remote sessions)
    pub saved: Option<WorkspacesSaveLargePasteResultSaved>,
}
crate::record_serde::wire_record!(WorkspacesSaveLargePasteResult { saved: "saved" });

/// Relative path of the workspace file or directory to inspect.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesStatFileRequest {
    /// Slash-separated relative path within the workspace files directory
    pub path: String,
}
crate::record_serde::wire_record!(WorkspacesStatFileRequest { path: "path" });

/// Filesystem metadata for a path in the session workspace files directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesStatFileResult {
    /// Creation time in Unix epoch milliseconds
    pub birthtime_ms: f64,
    /// Whether the path identifies a directory
    pub is_directory: bool,
    /// Whether the path identifies a regular file
    pub is_file: bool,
    /// Last modification time in Unix epoch milliseconds
    pub mtime_ms: f64,
    /// Size in bytes
    pub size: f64,
}
crate::record_serde::wire_record!(WorkspacesStatFileResult {
    birthtime_ms: "birthtimeMs",
    is_directory: "isDirectory",
    is_file: "isFile",
    mtime_ms: "mtimeMs",
    size: "size"
});

/// Rollback point for local workspace summaries.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesTruncateSummariesRequest {
    /// Highest checkpoint number to keep. Checkpoints 1 through keepCount (the oldest) are kept and every later checkpoint is deleted; 0 deletes all.
    pub keep_count: i64,
}
crate::record_serde::wire_record!(WorkspacesTruncateSummariesRequest {
    keep_count: "keepCount"
});

/// Public-facing workspace metadata for this session, or null if the session has no associated workspace. Excludes runtime-internal fields (GitHub IDs, summary count, internal flags).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspaceSummary {
    /// Branch checked out at session start, if any
    pub branch: Option<String>,
    /// ISO 8601 timestamp when the workspace was created
    pub created_at: Option<String>,
    /// Current working directory at session start
    pub cwd: Option<String>,
    /// Resolved git root for cwd, if any
    pub git_root: Option<String>,
    /// Repository host type, if known
    pub host_type: Option<WorkspaceSummaryHostType>,
    /// Workspace identifier (1:1 with sessionId)
    pub id: String,
    /// Display name for the session, if set
    pub name: Option<String>,
    /// Repository identifier in 'owner/repo' or 'org/project/repo' format, if any
    pub repository: Option<String>,
    /// ISO 8601 timestamp when the workspace was last updated
    pub updated_at: Option<String>,
    /// Whether the display name was explicitly set by the user
    pub user_named: Option<bool>,
}
crate::record_serde::wire_record!(WorkspaceSummary { branch: "branch" if is_none, created_at: "created_at" if is_none, cwd: "cwd" if is_none, git_root: "git_root" if is_none, host_type: "host_type" if is_none, id: "id", name: "name" if is_none, repository: "repository" if is_none, updated_at: "updated_at" if is_none, user_named: "user_named" if is_none });

/// Workspace metadata fields to update.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesUpdateMetadataRequest {
    /// Opaque workspace context supplied by the session host.
    pub context: Option<serde_json::Value>,
    /// Optional workspace display name override.
    pub name: Option<String>,
}
crate::record_serde::wire_record!(WorkspacesUpdateMetadataRequest { context: "context" if is_none, name: "name" if is_none });

/// Autopilot objective file content to persist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesWriteAutopilotObjectiveRequest {
    /// Autopilot objective file content.
    pub content: String,
}
crate::record_serde::wire_record!(WorkspacesWriteAutopilotObjectiveRequest { content: "content" });

/// Result of writing the autopilot objective file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesWriteAutopilotObjectiveResult {
    /// Filesystem operation performed.
    pub operation: String,
}
crate::record_serde::wire_record!(WorkspacesWriteAutopilotObjectiveResult {
    operation: "operation"
});

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitBranchNameResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitBranchNameResult { value: "value" });

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRemoteUrlResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitRemoteUrlResult { value: "value" });

/// The answer to a repository predicate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitBranchExistsResult {
    /// Whether the predicate holds.
    pub value: bool,
}
crate::record_serde::wire_record!(GitBranchExistsResult { value: "value" });

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitReadHeadFileResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitReadHeadFileResult { value: "value" });

/// A created or reused worktree.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreateWorktreeResult {
    /// Actual branch name.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
    /// Whether an existing worktree was reused.
    pub reused: bool,
}
crate::record_serde::wire_record!(GitCreateWorktreeResult {
    branch: "branch",
    path: "path",
    reused: "reused"
});

/// The answer to a repository predicate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorktreeNameAvailableResult {
    /// Whether the predicate holds.
    pub value: bool,
}
crate::record_serde::wire_record!(GitWorktreeNameAvailableResult { value: "value" });

/// Repository discovery, including the explicit not-found result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitFindRootResult {
    /// Whether the directory belongs to a repository.
    pub found: bool,
    /// Root path, or the resolver's empty path when no repository exists.
    pub git_root: String,
}
crate::record_serde::wire_record!(GitFindRootResult {
    found: "found",
    git_root: "gitRoot"
});

/// A Git value, or null when the requested identity does not exist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitFindRootCheckedResult {
    /// The resolved value. Null represents an absent root or branch identity.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(GitFindRootCheckedResult { value: "value" if is_none });

/// A Git value, or null when the requested identity does not exist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCurrentBranchResult {
    /// The resolved value. Null represents an absent root or branch identity.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(GitCurrentBranchResult { value: "value" if is_none });

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDefaultBranchResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitDefaultBranchResult { value: "value" });

/// Known forge hosts reachable through repository remotes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDetectRemoteHostsResult {
    /// The Azure DevOps URL, when present.
    pub azure_dev_ops_url: Option<String>,
    /// Whether a remote points to Azure DevOps.
    pub has_azure_dev_ops: bool,
    /// Whether a remote points to GitHub.
    pub has_git_hub: bool,
}
crate::record_serde::wire_record!(GitDetectRemoteHostsResult { azure_dev_ops_url: "azureDevOpsUrl" if is_none, has_azure_dev_ops: "hasAzureDevOps", has_git_hub: "hasGitHub" });

/// Repository identity, or null when no supported forge is configured.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitRepositoryIdentifierResult {
    /// The detected repository.
    pub repository: Option<GitRepoIdentifierInfo>,
}
crate::record_serde::wire_record!(GitRepositoryIdentifierResult { repository: "repository" if is_none });

/// Tracked and untracked changes in a checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorkingTreeStatusResult {
    /// Whether the index has staged changes.
    pub has_staged_changes: bool,
    /// Whether tracked files have unstaged changes.
    pub has_unstaged_changes: bool,
    /// Whether untracked files exist.
    pub has_untracked_files: bool,
}
crate::record_serde::wire_record!(GitWorkingTreeStatusResult {
    has_staged_changes: "hasStagedChanges",
    has_unstaged_changes: "hasUnstagedChanges",
    has_untracked_files: "hasUntrackedFiles"
});

/// A textual result of a Git operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorktreeChangeSummaryResult {
    /// The operation's text.
    pub value: String,
}
crate::record_serde::wire_record!(GitWorktreeChangeSummaryResult { value: "value" });

/// The answer to a repository predicate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitIsValidBranchNameResult {
    /// Whether the predicate holds.
    pub value: bool,
}
crate::record_serde::wire_record!(GitIsValidBranchNameResult { value: "value" });

/// A completed move or an actionable recovery message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitMoveWorktreeChangesResult {
    /// Recovery instructions when the move failed.
    pub failure_message: Option<String>,
    /// Successful move, when present.
    pub moved: Option<GitMovedWorktree>,
}
crate::record_serde::wire_record!(GitMoveWorktreeChangesResult { failure_message: "failureMessage" if is_none, moved: "moved" if is_none });

/// A new worktree and the base ref actually used.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreateWorktreeFromBaseResult {
    /// Actual base ref, including the HEAD fallback.
    pub base_ref: String,
    /// Actual branch name, including any collision suffix.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
}
crate::record_serde::wire_record!(GitCreateWorktreeFromBaseResult {
    base_ref: "baseRef",
    branch: "branch",
    path: "path"
});

/// A created or reused worktree.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreatePullRequestWorktreeResult {
    /// Actual branch name.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
    /// Whether an existing worktree was reused.
    pub reused: bool,
}
crate::record_serde::wire_record!(GitCreatePullRequestWorktreeResult {
    branch: "branch",
    path: "path",
    reused: "reused"
});

/// A created or reused worktree.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitCreateIssueWorktreeResult {
    /// Actual branch name.
    pub branch: String,
    /// Worktree directory.
    pub path: String,
    /// Whether an existing worktree was reused.
    pub reused: bool,
}
crate::record_serde::wire_record!(GitCreateIssueWorktreeResult {
    branch: "branch",
    path: "path",
    reused: "reused"
});

/// Workspace diff result for the requested mode.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitDiffResult {
    /// Default branch used for a branch diff, when branch mode was requested.
    pub base_branch: Option<String>,
    /// Changed files and their unified diffs.
    pub changes: Vec<WorkspaceDiffFileChange>,
    /// Whether the requested diff fell back to unstaged changes, either because branch diff failed or session diff was unavailable.
    pub is_fallback: bool,
    /// Effective mode used for the returned changes.
    pub mode: WorkspaceDiffMode,
    /// Diff mode requested by the client.
    pub requested_mode: WorkspaceDiffMode,
    /// Why the session diff could not be produced, when applicable. Set only when `session` mode was requested and `isFallback` is true, so a client can tell the permanent `file-change-tracking-disabled` apart from the transient `session-busy`, which the same request answers once the session settles. Never set for `unstaged` or `branch` mode, and never `unsupported-remote-session`: a remote session's captures live on its own host, so a `session`-mode diff is rejected for one rather than answered with a controller-side fallback.
    pub unavailable_reason: Option<HistoryRewindUnavailableReason>,
}
crate::record_serde::wire_record!(GitDiffResult { base_branch: "baseBranch" if is_none, changes: "changes", is_fallback: "isFallback", mode: "mode", requested_mode: "requestedMode", unavailable_reason: "unavailableReason" if is_none });

/// Session host's working directory and optional repository context.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitWorkingDirectoryContextResult {
    /// Merge-base commit, when available.
    pub base_commit: Option<String>,
    /// Current branch, when available.
    pub branch: Option<String>,
    /// Requested working directory.
    pub cwd: String,
    /// Default branch, only when requested and available.
    pub default_branch: Option<String>,
    /// Repository working tree root.
    pub git_root: Option<String>,
    /// Current HEAD commit, when available.
    pub head_commit: Option<String>,
    /// Recognized repository host platform.
    pub host_type: Option<SessionWorkingDirectoryContextHostType>,
    /// Repository identifier from the existing Git owner.
    pub repository: Option<String>,
    /// Repository remote host.
    pub repository_host: Option<String>,
}
crate::record_serde::wire_record!(GitWorkingDirectoryContextResult { base_commit: "baseCommit" if is_none, branch: "branch" if is_none, cwd: "cwd", default_branch: "defaultBranch" if is_none, git_root: "gitRoot" if is_none, head_commit: "headCommit" if is_none, host_type: "hostType" if is_none, repository: "repository" if is_none, repository_host: "repositoryHost" if is_none });

/// The repository the requested id names, when the credential can see one.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryGetByIdResult {
    /// Owner and name of the repository, or null when the id names no repository visible to the credential.
    pub repository: Option<GitHubRepositoryOwnerName>,
}
crate::record_serde::wire_record!(GitHubRepositoryGetByIdResult { repository: "repository" if is_none });

/// Outcome of a branch pull-request lookup. `status` carries why, because 'not authenticated', 'no pull request' and 'the search failed' are three different answers and only the first two are conclusive.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubRepositoryLookupBranchPullRequestResult {
    /// The matching pull request, or null for every status other than `found`.
    pub pull_request: Option<GitHubBranchPullRequest>,
    /// Why the lookup reported what it did.
    #[doc(hidden)]
    pub(crate) status: GitHubBranchPullRequestLookupStatus,
}
crate::record_serde::wire_record!(GitHubRepositoryLookupBranchPullRequestResult { pull_request: "pullRequest" if is_none, status: "status" });

/// A freshly registered request id. Registering it before the listing starts is what lets a cancel that races the request still find the owner listing slot. The id serves one listing only. Long-abandoned unused ids can be released by later allocations.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubOwnersNextRequestIdResult {
    /// Request id to pass to `gitHubOwners.list` and, to abandon it, `gitHubOwners.cancel`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubOwnersNextRequestIdResult {
    request_id: "requestId"
});

/// One registered GitHub request, shared by execution and cancellation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubFetchAllocateRequestIdResult {
    /// One fresh id from `gitHubFetch.allocateRequestId`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubFetchAllocateRequestIdResult {
    request_id: "requestId"
});

/// A freshly registered request id. Registering it before the work starts is what lets a cancel that races the request still find something to cancel.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseAllocateRequestIdResult {
    /// Request id to pass to `gitHubRelease.lookup` or `gitHubRelease.downloadAsset` and, to abandon the work, `gitHubRelease.cancel`.
    pub request_id: i64,
}
crate::record_serde::wire_record!(GitHubReleaseAllocateRequestIdResult {
    request_id: "requestId"
});

/// Byte progress of a running download.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubReleaseGetDownloadProgressResult {
    /// Bytes written so far.
    pub received_bytes: i64,
    /// Size the server announced for the body. Omitted when it announced none.
    pub total_bytes: Option<i64>,
}
crate::record_serde::wire_record!(GitHubReleaseGetDownloadProgressResult { received_bytes: "receivedBytes", total_bytes: "totalBytes" if is_none });

/// One request-owned admission decision, sent only to its originating SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RemoteCheckSendAdmissionParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Invocation identity supplied by the same client that owns the send.
    pub admission_id: String,
    /// Recheck a deferred guard, or acknowledge the final irreversible admission.
    pub phase: AhpSessionAdmissionPhase,
}
crate::record_serde::wire_record!(RemoteCheckSendAdmissionParams {
    session_id: "sessionId",
    admission_id: "admissionId",
    phase: "phase"
});

/// A decision for one callback, with JavaScript errors retained by its originating client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RemoteCheckSendAdmissionResult {
    /// Whether this invocation may cross the requested admission boundary.
    pub allowed: bool,
}
crate::record_serde::wire_record!(RemoteCheckSendAdmissionResult { allowed: "allowed" });

/// Signed-in accounts; empty when no accounts are available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AccountsList = Vec<Account>;

/// Cached base identities reusable by the selected login provider, active first.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AuthLoginIdentityList = Vec<AuthLoginIdentity>;

/// Authentication providers offered for interactive login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AuthProvidersList = Vec<AuthProviderDescriptor>;

/// Validation errors from the most recent authentication attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AuthValidationErrors = Vec<AuthValidationError>;

/// Client-owned, case-sensitive string metadata persisted with a local session. Clients should namespace keys by owner. Keys must be non-empty and at most 256 UTF-8 bytes; keys under `copilot/` and `github/` are reserved. Values may contain at most 16 KiB of UTF-8 data. A bag may contain at most 128 entries and its serialized sidecar may contain at most 64 KiB. The runtime stores but never interprets these values.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type ClientMetadata = HashMap<String, String>;

/// List of all authenticated users
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AccountGetAllUsersResult = Vec<AccountAllUsers>;

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestOpenOperation {
    #[serde(rename = "open")]
    #[default]
    Open,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestOpenOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestOpenOperation",
            &["open"],
            None,
        )?;
        Ok(Self::Open)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestTokenOperation {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestTokenOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestTokenOperation",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestStartDeviceOperation {
    #[serde(rename = "startDevice")]
    #[default]
    StartDevice,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestStartDeviceOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestStartDeviceOperation",
            &["startDevice"],
            None,
        )?;
        Ok(Self::StartDevice)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestStartWebOperation {
    #[serde(rename = "startWeb")]
    #[default]
    StartWeb,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestStartWebOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestStartWebOperation",
            &["startWeb"],
            None,
        )?;
        Ok(Self::StartWeb)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestCompleteOperation {
    #[serde(rename = "complete")]
    #[default]
    Complete,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestCompleteOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestCompleteOperation",
            &["complete"],
            None,
        )?;
        Ok(Self::Complete)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestOpenBrowserOperation {
    #[serde(rename = "openBrowser")]
    #[default]
    OpenBrowser,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestOpenBrowserOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestOpenBrowserOperation",
            &["openBrowser"],
            None,
        )?;
        Ok(Self::OpenBrowser)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestContinueOperation {
    #[serde(rename = "continue")]
    #[default]
    Continue,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestContinueOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestContinueOperation",
            &["continue"],
            None,
        )?;
        Ok(Self::Continue)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestPolicyOperation {
    #[serde(rename = "policy")]
    #[default]
    Policy,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestPolicyOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestPolicyOperation",
            &["policy"],
            None,
        )?;
        Ok(Self::Policy)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestDiscardOperation {
    #[serde(rename = "discard")]
    #[default]
    Discard,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestDiscardOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestDiscardOperation",
            &["discard"],
            None,
        )?;
        Ok(Self::Discard)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessRequestCloseOperation {
    #[serde(rename = "close")]
    #[default]
    Close,
}
impl<'de> Deserialize<'de> for AccountLoginProcessRequestCloseOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessRequestCloseOperation",
            &["close"],
            None,
        )?;
        Ok(Self::Close)
    }
}

/// A pre-session login action. Every action after open retains the same credential and persistence owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum AccountLoginProcessRequest {
    Open(AccountLoginProcessRequestOpen),
    Token(AccountLoginProcessRequestToken),
    StartDevice(AccountLoginProcessRequestStartDevice),
    StartWeb(AccountLoginProcessRequestStartWeb),
    Complete(AccountLoginProcessRequestComplete),
    OpenBrowser(AccountLoginProcessRequestOpenBrowser),
    Continue(AccountLoginProcessRequestContinue),
    Policy(AccountLoginProcessRequestPolicy),
    Discard(AccountLoginProcessRequestDiscard),
    Close(AccountLoginProcessRequestClose),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultOpenedKind {
    #[serde(rename = "opened")]
    #[default]
    Opened,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultOpenedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultOpenedKind",
            &["opened"],
            None,
        )?;
        Ok(Self::Opened)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultDeviceCodeKind {
    #[serde(rename = "deviceCode")]
    #[default]
    DeviceCode,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultDeviceCodeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultDeviceCodeKind",
            &["deviceCode"],
            None,
        )?;
        Ok(Self::DeviceCode)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultWebKind {
    #[serde(rename = "web")]
    #[default]
    Web,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultWebKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultWebKind",
            &["web"],
            None,
        )?;
        Ok(Self::Web)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultPersistenceKind {
    #[serde(rename = "persistence")]
    #[default]
    Persistence,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultPersistenceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultPersistenceKind",
            &["persistence"],
            None,
        )?;
        Ok(Self::Persistence)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultPolicyKind {
    #[serde(rename = "policy")]
    #[default]
    Policy,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultPolicyKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultPolicyKind",
            &["policy"],
            None,
        )?;
        Ok(Self::Policy)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultDiscardedKind {
    #[serde(rename = "discarded")]
    #[default]
    Discarded,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultDiscardedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultDiscardedKind",
            &["discarded"],
            None,
        )?;
        Ok(Self::Discarded)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AccountLoginProcessResultDoneKind {
    #[serde(rename = "done")]
    #[default]
    Done,
}
impl<'de> Deserialize<'de> for AccountLoginProcessResultDoneKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountLoginProcessResultDoneKind",
            &["done"],
            None,
        )?;
        Ok(Self::Done)
    }
}

/// The result of one connection-owned pre-session login action.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum AccountLoginProcessResult {
    Opened(AccountLoginProcessResultOpened),
    DeviceCode(AccountLoginProcessResultDeviceCode),
    Web(AccountLoginProcessResultWeb),
    Persistence(AccountLoginProcessResultPersistence),
    Policy(AccountLoginProcessResultPolicy),
    Discarded(AccountLoginProcessResultDiscarded),
    Done(AccountLoginProcessResultDone),
}

/// How an interactive GitHub login is authorized.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginMethod {
    /// Browser authorization with a loopback callback; the flow surfaces an `open-url` step.
    #[serde(rename = "web")]
    Web,
    /// OAuth device authorization; the flow surfaces a `device-code` step with a one-time code, for terminals whose browser cannot reach a loopback callback.
    #[serde(rename = "device-code")]
    DeviceCode,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AuthLoginMethod {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginMethod",
            &["web", "device-code", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Web,
            1 => Self::DeviceCode,
            _ => Self::Unknown,
        })
    }
}

/// How an interactive provider login should choose its base identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginMode {
    /// Use normal provider behavior, optionally pinned to accountId.
    #[serde(rename = "default")]
    Default,
    /// Explicitly ask the provider to present its account-selection UI instead of serving from a retained identity.
    #[serde(rename = "selectAccount")]
    SelectAccount,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AuthLoginMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginMode",
            &["default", "selectAccount", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Default,
            1 => Self::SelectAccount,
            _ => Self::Unknown,
        })
    }
}

/// Why the runtime requests client-task cancellation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ClientTaskCancelReason {
    /// A caller requested task cancellation.
    #[serde(rename = "cancel_requested")]
    CancelRequested,
    /// The session is shutting down.
    #[serde(rename = "session_shutdown")]
    SessionShutdown,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ClientTaskCancelReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ClientTaskCancelReason",
            &["cancel_requested", "session_shutdown", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::CancelRequested,
            1 => Self::SessionShutdown,
            _ => Self::Unknown,
        })
    }
}

/// Diagnostic source identifying the typed details payload.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticEntrySource {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for DiagnosticEntrySource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticEntrySource",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// A supported diagnostic source.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticSource {
    #[serde(rename = "mcp")]
    Mcp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiagnosticSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticSource",
            &["mcp", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Mcp,
            _ => Self::Unknown,
        })
    }
}

/// Diagnostic source identifying the typed details payload.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticsReadResultEntriesItemSource {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for DiagnosticsReadResultEntriesItemSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticsReadResultEntriesItemSource",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// Verbosity for a runtime-owned file logger.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FileLogLevel {
    /// Disables level-filtered records. Unconditional log operations still write.
    #[serde(rename = "none")]
    None,
    /// Includes error records.
    #[serde(rename = "error")]
    Error,
    /// Includes error, warning, and notice records.
    #[serde(rename = "warning")]
    Warning,
    /// Includes informational records and higher severities.
    #[serde(rename = "info")]
    Info,
    /// Includes debug records and higher severities.
    #[serde(rename = "debug")]
    Debug,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FileLogLevel {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FileLogLevel",
            &["none", "error", "warning", "info", "debug", "Unknown"],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::Error,
            2 => Self::Warning,
            3 => Self::Info,
            4 => Self::Debug,
            _ => Self::Unknown,
        })
    }
}

/// Operation applied by the runtime logger, including its filtering and category formatting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FileLogOperation {
    /// Writes an unconditional LOG record.
    #[serde(rename = "log")]
    Log,
    /// Writes a DEBUG record at debug severity.
    #[serde(rename = "debug")]
    Debug,
    /// Writes an INFO record at informational severity or the supplied group level.
    #[serde(rename = "info")]
    Info,
    /// Writes a NOTICE record at warning severity.
    #[serde(rename = "notice")]
    Notice,
    /// Writes a WARNING record at warning severity.
    #[serde(rename = "warning")]
    Warning,
    /// Writes an ERROR record at error severity.
    #[serde(rename = "error")]
    Error,
    /// Writes a START-GROUP record at informational severity or the supplied group level.
    #[serde(rename = "startGroup")]
    StartGroup,
    /// Writes an END-GROUP record at informational severity or the supplied group level.
    #[serde(rename = "endGroup")]
    EndGroup,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FileLogOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FileLogOperation",
            &[
                "log",
                "debug",
                "info",
                "notice",
                "warning",
                "error",
                "startGroup",
                "endGroup",
                "Unknown",
            ],
            Some(8),
        )?;
        Ok(match index {
            0 => Self::Log,
            1 => Self::Debug,
            2 => Self::Info,
            3 => Self::Notice,
            4 => Self::Warning,
            5 => Self::Error,
            6 => Self::StartGroup,
            7 => Self::EndGroup,
            _ => Self::Unknown,
        })
    }
}

/// The change made to a checkout file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitDiffChangeType {
    /// A new file.
    #[serde(rename = "added")]
    Added,
    /// An edited file.
    #[serde(rename = "modified")]
    Modified,
    /// A removed file.
    #[serde(rename = "deleted")]
    Deleted,
    /// A moved file.
    #[serde(rename = "renamed")]
    Renamed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitDiffChangeType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitDiffChangeType",
            &["added", "modified", "deleted", "renamed", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Added,
            1 => Self::Modified,
            2 => Self::Deleted,
            3 => Self::Renamed,
            _ => Self::Unknown,
        })
    }
}

/// Rolled-up state of one pull-request check. GitHub's own conclusions are collapsed to these three so a caller renders a check without enumerating every API conclusion.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubPullRequestCheckStatus {
    /// The check concluded successfully, or was neutral or skipped.
    #[serde(rename = "pass")]
    Pass,
    /// The check concluded unsuccessfully, timed out, or was cancelled.
    #[serde(rename = "fail")]
    Fail,
    /// The check has not concluded, or reported a conclusion with no settled meaning.
    #[serde(rename = "pending")]
    Pending,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubPullRequestCheckStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubPullRequestCheckStatus",
            &["pass", "fail", "pending", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Pass,
            1 => Self::Fail,
            2 => Self::Pending,
            _ => Self::Unknown,
        })
    }
}

/// Why a branch pull-request lookup reported what it did. The three unsuccessful outcomes are kept apart because each one asks the caller for something different.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubBranchPullRequestLookupStatus {
    /// A matching pull request was found and is carried in `pullRequest`.
    #[serde(rename = "found")]
    Found,
    /// The credential resolved to no usable token, so the caller must authenticate.
    #[serde(rename = "no-token")]
    NoToken,
    /// The search succeeded and the branch has no matching pull request.
    #[serde(rename = "no-pr")]
    NoPr,
    /// The search itself could not be completed, so whether a pull request exists is unknown.
    #[serde(rename = "lookup-failed")]
    LookupFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubBranchPullRequestLookupStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubBranchPullRequestLookupStatus",
            &["found", "no-token", "no-pr", "lookup-failed", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Found,
            1 => Self::NoToken,
            2 => Self::NoPr,
            3 => Self::LookupFailed,
            _ => Self::Unknown,
        })
    }
}

/// The export purpose that selects existing account policy and error messages.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubSecretGistUseCase {
    /// Export a session transcript under the existing session account policy.
    #[serde(rename = "session")]
    Session,
    /// Export a research report under the existing research account policy.
    #[serde(rename = "research")]
    Research,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubSecretGistUseCase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubSecretGistUseCase",
            &["session", "research", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Session,
            1 => Self::Research,
            _ => Self::Unknown,
        })
    }
}

/// A runtime-owned GitHub or Copilot operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubFetchOperation {
    /// Check repository access.
    #[serde(rename = "repository-access")]
    RepositoryAccess,
    /// Look up a branch.
    #[serde(rename = "branch-exists")]
    BranchExists,
    /// Create a Copilot job.
    #[serde(rename = "job-create")]
    JobCreate,
    /// Read a Copilot job.
    #[serde(rename = "job-status")]
    JobStatus,
    /// List remote sessions.
    #[serde(rename = "sessions-list")]
    SessionsList,
    /// Read one remote session.
    #[serde(rename = "session-status")]
    SessionStatus,
    /// Resolve pull requests through GraphQL.
    #[serde(rename = "pull-requests-graphql")]
    PullRequestsGraphql,
    /// Read a pull request.
    #[serde(rename = "pull-request")]
    PullRequest,
    /// Read pull request files.
    #[serde(rename = "pull-request-files")]
    PullRequestFiles,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubFetchOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubFetchOperation",
            &[
                "repository-access",
                "branch-exists",
                "job-create",
                "job-status",
                "sessions-list",
                "session-status",
                "pull-requests-graphql",
                "pull-request",
                "pull-request-files",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::RepositoryAccess,
            1 => Self::BranchExists,
            2 => Self::JobCreate,
            3 => Self::JobStatus,
            4 => Self::SessionsList,
            5 => Self::SessionStatus,
            6 => Self::PullRequestsGraphql,
            7 => Self::PullRequest,
            8 => Self::PullRequestFiles,
            _ => Self::Unknown,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubFetchResultResponseKind {
    #[serde(rename = "response")]
    #[default]
    Response,
}
impl<'de> Deserialize<'de> for GitHubFetchResultResponseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubFetchResultResponseKind",
            &["response"],
            None,
        )?;
        Ok(Self::Response)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubFetchResultCircuitBreakerKind {
    #[serde(rename = "circuit-breaker")]
    #[default]
    CircuitBreaker,
}
impl<'de> Deserialize<'de> for GitHubFetchResultCircuitBreakerKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubFetchResultCircuitBreakerKind",
            &["circuit-breaker"],
            None,
        )?;
        Ok(Self::CircuitBreaker)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubFetchResultProxyResponseKind {
    #[serde(rename = "proxy-response")]
    #[default]
    ProxyResponse,
}
impl<'de> Deserialize<'de> for GitHubFetchResultProxyResponseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubFetchResultProxyResponseKind",
            &["proxy-response"],
            None,
        )?;
        Ok(Self::ProxyResponse)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubFetchResultFailedKind {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for GitHubFetchResultFailedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubFetchResultFailedKind",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// The HTTP response or a classified request failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum GitHubFetchResult {
    Response(GitHubFetchResultResponse),
    CircuitBreaker(GitHubFetchResultCircuitBreaker),
    ProxyResponse(GitHubFetchResultProxyResponse),
    Failed(GitHubFetchResultFailed),
}

/// How an asset download ended. A caller tells a refused response from a transport failure or a cancellation by this alone, without parsing `message`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseDownloadKind {
    /// The asset is on disk at `path`.
    #[serde(rename = "downloaded")]
    Downloaded,
    /// GitHub answered with a status other than 2xx, including after the retry without the credential. `status` and `statusText` describe it.
    #[serde(rename = "http-error")]
    HttpError,
    /// The downloaded bytes did not match `expectedSha256`, and the file was removed.
    #[serde(rename = "digest-mismatch")]
    DigestMismatch,
    /// `gitHubRelease.cancel` abandoned the download, and any partial file was removed.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// The download failed for any other reason, described by `message`.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubReleaseDownloadKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseDownloadKind",
            &[
                "downloaded",
                "http-error",
                "digest-mismatch",
                "cancelled",
                "failed",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Downloaded,
            1 => Self::HttpError,
            2 => Self::DigestMismatch,
            3 => Self::Cancelled,
            4 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// How a release lookup ended. A caller tells a conclusive absence from a transport error by this alone, without parsing `message`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseLookupKind {
    /// A release was found, and `releaseJson` carries it.
    #[serde(rename = "found")]
    Found,
    /// The tag names no release. Conclusive, not a failure.
    #[serde(rename = "not-found")]
    NotFound,
    /// The repository publishes no release matching the query at all.
    #[serde(rename = "no-release")]
    NoRelease,
    /// GitHub refused the request for rate limiting. `message` is phrased for the user and already accounts for whether the lookup ran authenticated.
    #[serde(rename = "rate-limited")]
    RateLimited,
    /// The lookup failed for any other reason, described by `message`.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubReleaseLookupKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseLookupKind",
            &[
                "found",
                "not-found",
                "no-release",
                "rate-limited",
                "failed",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Found,
            1 => Self::NotFound,
            2 => Self::NoRelease,
            3 => Self::RateLimited,
            4 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Which release to look up. Enumerated rather than free text because the runtime accepts exactly these and rejects anything else.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseQuery {
    /// The newest stable command-line release.
    #[serde(rename = "latest-stable")]
    LatestStable,
    /// The newest prerelease command-line release, which is found by scanning recent releases rather than by asking for the latest one.
    #[serde(rename = "latest-prerelease")]
    LatestPrerelease,
    /// The newest release of the desktop application.
    #[serde(rename = "latest-app")]
    LatestApp,
    /// The release a specific tag names, supplied in `value`.
    #[serde(rename = "tag")]
    Tag,
    /// Recent releases for process-scoped update and metrics decisions.
    #[serde(rename = "release-list")]
    ReleaseList,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubReleaseQuery {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseQuery",
            &[
                "latest-stable",
                "latest-prerelease",
                "latest-app",
                "tag",
                "release-list",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::LatestStable,
            1 => Self::LatestPrerelease,
            2 => Self::LatestApp,
            3 => Self::Tag,
            4 => Self::ReleaseList,
            _ => Self::Unknown,
        })
    }
}

/// Whether the caller runs a development build or a release package.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleasePackageIdentity {
    /// Development builds are not eligible for release admission.
    #[serde(rename = "dev")]
    Dev,
    /// The caller runs a release package.
    #[serde(rename = "release")]
    Release,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubReleasePackageIdentity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleasePackageIdentity",
            &["dev", "release", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Dev,
            1 => Self::Release,
            _ => Self::Unknown,
        })
    }
}

/// The release channel used for update selection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseUpdateChannel {
    /// Select the newest stable release.
    #[serde(rename = "stable")]
    Stable,
    /// Select the newest published release marked as a prerelease. A stable-only feed has no candidate.
    #[serde(rename = "prerelease")]
    Prerelease,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubReleaseUpdateChannel {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseUpdateChannel",
            &["stable", "prerelease", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Stable,
            1 => Self::Prerelease,
            _ => Self::Unknown,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseSnapshotResultSuccessOutcome {
    #[serde(rename = "success")]
    #[default]
    Success,
}
impl<'de> Deserialize<'de> for GitHubReleaseSnapshotResultSuccessOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseSnapshotResultSuccessOutcome",
            &["success"],
            None,
        )?;
        Ok(Self::Success)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum GitHubReleaseSnapshotResultFailedOutcome {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for GitHubReleaseSnapshotResultFailedOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubReleaseSnapshotResultFailedOutcome",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// The shared release result with its original lookup duration. Runtime worker faults remain RPC errors.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum GitHubReleaseSnapshotResult {
    Success(GitHubReleaseSnapshotResultSuccess),
    Failed(GitHubReleaseSnapshotResultFailed),
}

/// Why the runtime is requesting a GitHub credential.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubTokenAcquireReason {
    /// The runtime is acquiring the registration's first credential.
    #[serde(rename = "initial")]
    Initial,
    /// The runtime is replacing a credential that is approaching expiry.
    #[serde(rename = "refresh")]
    Refresh,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for GitHubTokenAcquireReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubTokenAcquireReason",
            &["initial", "refresh", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Initial,
            1 => Self::Refresh,
            _ => Self::Unknown,
        })
    }
}

/// GitHub credential response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubTokenAcquireResultTokenKind {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for GitHubTokenAcquireResultTokenKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubTokenAcquireResultTokenKind",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

/// GitHub credential response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GitHubTokenAcquireResultCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for GitHubTokenAcquireResultCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GitHubTokenAcquireResultCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// SDK host response to a GitHub credential request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum GitHubTokenAcquireResult {
    Token(GitHubTokenAcquireResultToken),
    Cancelled(GitHubTokenAcquireResultCancelled),
}

/// Remote control state tag: active.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemoteControlStatusActiveState {
    #[serde(rename = "active")]
    #[default]
    Active,
}
impl<'de> Deserialize<'de> for RemoteControlStatusActiveState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "RemoteControlStatusActiveState",
            &["active"],
            None,
        )?;
        Ok(Self::Active)
    }
}

/// Remote control state tag: connecting.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemoteControlStatusConnectingState {
    #[serde(rename = "connecting")]
    #[default]
    Connecting,
}
impl<'de> Deserialize<'de> for RemoteControlStatusConnectingState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "RemoteControlStatusConnectingState",
            &["connecting"],
            None,
        )?;
        Ok(Self::Connecting)
    }
}

/// Remote control state tag: setup failed.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemoteControlStatusErrorState {
    #[serde(rename = "error")]
    #[default]
    Error,
}
impl<'de> Deserialize<'de> for RemoteControlStatusErrorState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "RemoteControlStatusErrorState",
            &["error"],
            None,
        )?;
        Ok(Self::Error)
    }
}

/// Remote control state tag: not connected.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemoteControlStatusOffState {
    #[serde(rename = "off")]
    #[default]
    Off,
}
impl<'de> Deserialize<'de> for RemoteControlStatusOffState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "RemoteControlStatusOffState",
            &["off"],
            None,
        )?;
        Ok(Self::Off)
    }
}

/// Per-session remote mode. "off" disables remote, "export" exports session events to GitHub without enabling remote steering, "on" enables both export and remote steering.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemoteSessionMode {
    /// Disable remote session export and steering.
    #[serde(rename = "off")]
    Off,
    /// Export session events to GitHub without enabling remote steering.
    #[serde(rename = "export")]
    Export,
    /// Enable both remote session export and remote steering.
    #[serde(rename = "on")]
    On,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for RemoteSessionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "RemoteSessionMode",
            &["off", "export", "on", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Off,
            1 => Self::Export,
            2 => Self::On,
            _ => Self::Unknown,
        })
    }
}
