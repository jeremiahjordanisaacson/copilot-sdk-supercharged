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

use crate::generated::{lifecycle_diagnostics, session_events};
use crate::types::{RequestId, SessionEvent, SessionId};

/// One canonical signed-in account entry shared by account-list and account-switch surfaces.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Account {
    /// Stable opaque account id used for forest lineage and account management.
    pub account_id: Option<String>,
    /// Whether this is the active account.
    pub active: bool,
    /// Human-readable credential source used to authenticate this account, such as `gh` or `GITHUB_TOKEN`.
    pub auth_source: Option<String>,
    /// Opaque accountId of the account this one was derived from; absent for a root account.
    pub derived_from: Option<String>,
    /// Canonical availability treatment for this account.
    pub health: Option<AccountHealth>,
    /// Authentication host URL.
    pub host: String,
    /// The provider kind of this account.
    pub kind: AccountKind,
    /// Human-readable identity and provider label owned by the runtime.
    pub label: Option<String>,
    /// Authenticated login/username.
    pub login: String,
    /// Opaque selection id accepted by switchActive. Absent when switching is not currently valid.
    pub selection_id: Option<String>,
    /// Human-readable availability or recovery reason.
    pub status_message: Option<String>,
    /// Whether the account is currently a valid switch target.
    pub switchable: Option<bool>,
}
crate::record_serde::wire_record!(Account { account_id: "accountId" if is_none, active: "active", auth_source: "authSource" if is_none, derived_from: "derivedFrom" if is_none, health: "health" if is_none, host: "host", kind: "kind", label: "label" if is_none, login: "login", selection_id: "selectionId" if is_none, status_message: "statusMessage" if is_none, switchable: "switchable" if is_none });

/// Endpoint URLs from the raw Copilot `/copilot_internal/v2/token` user-response passthrough.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseEndpoints {
    /// Copilot API endpoint URL.
    pub api: Option<String>,
    /// Experimental-service endpoint URL.
    pub exp: Option<String>,
    /// Origin-tracker endpoint URL.
    pub origin_tracker: Option<String>,
    /// Copilot proxy endpoint URL.
    pub proxy: Option<String>,
    /// Copilot telemetry endpoint URL.
    pub telemetry: Option<String>,
}
crate::record_serde::wire_record!(CopilotUserResponseEndpoints { api: "api" if is_none, exp: "exp" if is_none, origin_tracker: "origin-tracker" if is_none, proxy: "proxy" if is_none, telemetry: "telemetry" if is_none });

#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseEnterpriseListItem {
    /// JavaScript-safe numeric database ID of the enterprise.
    pub id: Option<i64>,
}
crate::record_serde::wire_record!(CopilotUserResponseEnterpriseListItem { id: "id" if is_none });

/// Chat quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseQuotaSnapshotsChat {
    /// Number of requests/units included in the entitlement for this period; `-1` denotes an unlimited entitlement.
    pub entitlement: Option<f64>,
    /// Whether the user currently has quota available; when `false` and not unlimited, further requests are blocked until the quota resets.
    pub has_quota: Option<bool>,
    /// Count of additional pay-per-request usage consumed this period beyond the entitlement.
    pub overage_count: Option<f64>,
    /// Whether usage may continue at pay-per-request rates once the entitlement is exhausted.
    pub overage_permitted: Option<bool>,
    /// Percentage of the entitlement remaining at the snapshot timestamp.
    pub percent_remaining: Option<f64>,
    /// Identifier of the quota bucket this snapshot describes.
    pub quota_id: Option<String>,
    /// Amount of quota remaining at the snapshot timestamp.
    pub quota_remaining: Option<f64>,
    /// Unix epoch time, in seconds, when this quota next resets.
    pub quota_reset_at: Option<f64>,
    /// Remaining entitlement/quota amount at the snapshot timestamp.
    pub remaining: Option<f64>,
    /// UTC timestamp when this snapshot was captured.
    pub timestamp_utc: Option<String>,
    /// Whether this category uses usage-based (token/AI-credit) billing rather than a fixed premium-request count.
    pub token_based_billing: Option<bool>,
    /// Whether the entitlement for this category is unlimited.
    pub unlimited: Option<bool>,
}
crate::record_serde::wire_record!(CopilotUserResponseQuotaSnapshotsChat { entitlement: "entitlement" if is_none, has_quota: "has_quota" if is_none, overage_count: "overage_count" if is_none, overage_permitted: "overage_permitted" if is_none, percent_remaining: "percent_remaining" if is_none, quota_id: "quota_id" if is_none, quota_remaining: "quota_remaining" if is_none, quota_reset_at: "quota_reset_at" if is_none, remaining: "remaining" if is_none, timestamp_utc: "timestamp_utc" if is_none, token_based_billing: "token_based_billing" if is_none, unlimited: "unlimited" if is_none });

/// Completions quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseQuotaSnapshotsCompletions {
    /// Number of requests/units included in the entitlement for this period; `-1` denotes an unlimited entitlement.
    pub entitlement: Option<f64>,
    /// Whether the user currently has quota available; when `false` and not unlimited, further requests are blocked until the quota resets.
    pub has_quota: Option<bool>,
    /// Count of additional pay-per-request usage consumed this period beyond the entitlement.
    pub overage_count: Option<f64>,
    /// Whether usage may continue at pay-per-request rates once the entitlement is exhausted.
    pub overage_permitted: Option<bool>,
    /// Percentage of the entitlement remaining at the snapshot timestamp.
    pub percent_remaining: Option<f64>,
    /// Identifier of the quota bucket this snapshot describes.
    pub quota_id: Option<String>,
    /// Amount of quota remaining at the snapshot timestamp.
    pub quota_remaining: Option<f64>,
    /// Unix epoch time, in seconds, when this quota next resets.
    pub quota_reset_at: Option<f64>,
    /// Remaining entitlement/quota amount at the snapshot timestamp.
    pub remaining: Option<f64>,
    /// UTC timestamp when this snapshot was captured.
    pub timestamp_utc: Option<String>,
    /// Whether this category uses usage-based (token/AI-credit) billing rather than a fixed premium-request count.
    pub token_based_billing: Option<bool>,
    /// Whether the entitlement for this category is unlimited.
    pub unlimited: Option<bool>,
}
crate::record_serde::wire_record!(CopilotUserResponseQuotaSnapshotsCompletions { entitlement: "entitlement" if is_none, has_quota: "has_quota" if is_none, overage_count: "overage_count" if is_none, overage_permitted: "overage_permitted" if is_none, percent_remaining: "percent_remaining" if is_none, quota_id: "quota_id" if is_none, quota_remaining: "quota_remaining" if is_none, quota_reset_at: "quota_reset_at" if is_none, remaining: "remaining" if is_none, timestamp_utc: "timestamp_utc" if is_none, token_based_billing: "token_based_billing" if is_none, unlimited: "unlimited" if is_none });

/// Premium-interactions quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseQuotaSnapshotsPremiumInteractions {
    /// Number of requests/units included in the entitlement for this period; `-1` denotes an unlimited entitlement.
    pub entitlement: Option<f64>,
    /// Whether the user currently has quota available; when `false` and not unlimited, further requests are blocked until the quota resets.
    pub has_quota: Option<bool>,
    /// Count of additional pay-per-request usage consumed this period beyond the entitlement.
    pub overage_count: Option<f64>,
    /// Whether usage may continue at pay-per-request rates once the entitlement is exhausted.
    pub overage_permitted: Option<bool>,
    /// Percentage of the entitlement remaining at the snapshot timestamp.
    pub percent_remaining: Option<f64>,
    /// Identifier of the quota bucket this snapshot describes.
    pub quota_id: Option<String>,
    /// Amount of quota remaining at the snapshot timestamp.
    pub quota_remaining: Option<f64>,
    /// Unix epoch time, in seconds, when this quota next resets.
    pub quota_reset_at: Option<f64>,
    /// Remaining entitlement/quota amount at the snapshot timestamp.
    pub remaining: Option<f64>,
    /// UTC timestamp when this snapshot was captured.
    pub timestamp_utc: Option<String>,
    /// Whether this category uses usage-based (token/AI-credit) billing rather than a fixed premium-request count.
    pub token_based_billing: Option<bool>,
    /// Whether the entitlement for this category is unlimited.
    pub unlimited: Option<bool>,
}
crate::record_serde::wire_record!(CopilotUserResponseQuotaSnapshotsPremiumInteractions { entitlement: "entitlement" if is_none, has_quota: "has_quota" if is_none, overage_count: "overage_count" if is_none, overage_permitted: "overage_permitted" if is_none, percent_remaining: "percent_remaining" if is_none, quota_id: "quota_id" if is_none, quota_remaining: "quota_remaining" if is_none, quota_reset_at: "quota_reset_at" if is_none, remaining: "remaining" if is_none, timestamp_utc: "timestamp_utc" if is_none, token_based_billing: "token_based_billing" if is_none, unlimited: "unlimited" if is_none });

/// Quota snapshot map from the raw Copilot user-response passthrough, with chat, completions, premium-interactions, and other entries.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponseQuotaSnapshots {
    /// Chat quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
    pub chat: Option<CopilotUserResponseQuotaSnapshotsChat>,
    /// Completions quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
    pub completions: Option<CopilotUserResponseQuotaSnapshotsCompletions>,
    /// Premium-interactions quota snapshot from the raw Copilot user-response passthrough, with entitlement, overage, remaining quota, reset, and billing fields.
    pub premium_interactions: Option<CopilotUserResponseQuotaSnapshotsPremiumInteractions>,
}
crate::record_serde::wire_record!(CopilotUserResponseQuotaSnapshots { chat: "chat" if is_none, completions: "completions" if is_none, premium_interactions: "premium_interactions" if is_none });

/// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotUserResponse {
    /// Copilot access SKU identifier (e.g. `free_limited_copilot`, `copilot_for_business_seat_quota`) used to gate model and feature access.
    pub access_type_sku: Option<String>,
    /// Opaque analytics tracking identifier for the user, forwarded from the Copilot API.
    pub analytics_tracking_id: Option<String>,
    /// Date the Copilot seat was assigned to the user, if applicable.
    pub assigned_date: Option<serde_json::Value>,
    /// Whether the user is eligible to sign up for the free/limited Copilot tier.
    pub can_signup_for_limited: Option<bool>,
    /// Whether the user is able to upgrade their Copilot plan.
    pub can_upgrade_plan: Option<bool>,
    /// Whether Copilot chat is enabled for the user.
    pub chat_enabled: Option<bool>,
    /// Whether CLI remote control is enabled for the user.
    pub cli_remote_control_enabled: Option<bool>,
    /// Whether cloud session storage is enabled for the user.
    pub cloud_session_storage_enabled: Option<bool>,
    /// Whether the Codex agent is enabled for the user.
    pub codex_agent_enabled: Option<bool>,
    /// Copilot plan name for the user (e.g. `individual`, `business`, `enterprise`).
    pub copilot_plan: Option<String>,
    /// Whether `.copilotignore` content-exclusion support is enabled for the user.
    pub copilotignore_enabled: Option<bool>,
    /// Endpoint URLs from the raw Copilot `/copilot_internal/v2/token` user-response passthrough.
    pub endpoints: Option<CopilotUserResponseEndpoints>,
    /// Enterprises that provide the user's Copilot license; malformed entries are normalized to null or ID-less shapes.
    pub enterprise_list: Option<Vec<Option<CopilotUserResponseEnterpriseListItem>>>,
    /// Whether MCP (Model Context Protocol) support is enabled for the user.
    pub is_mcp_enabled: Option<serde_json::Value>,
    /// Whether the user is a GitHub/Microsoft staff member.
    pub is_staff: Option<bool>,
    /// Per-category quota allotments for free/limited-tier users, keyed by quota category.
    pub limited_user_quotas: Option<HashMap<String, f64>>,
    /// Date the free/limited-tier user's quotas next reset, as a raw string from the Copilot API.
    pub limited_user_reset_date: Option<String>,
    /// GitHub login of the authenticated user.
    pub login: Option<String>,
    /// Per-category monthly quota allotments, keyed by quota category.
    pub monthly_quotas: Option<HashMap<String, f64>>,
    /// Organizations the user belongs to, each with an optional ID, login, and display name.
    pub organization_list: Option<serde_json::Value>,
    /// Logins of the organizations the user belongs to.
    pub organization_login_list: Option<Vec<String>>,
    /// Date the user's usage quota next resets, as a raw string from the Copilot API; see `quota_reset_date_utc` for the UTC-normalized value.
    pub quota_reset_date: Option<String>,
    /// UTC-normalized form of `quota_reset_date` (the date the user's usage quota next resets).
    pub quota_reset_date_utc: Option<String>,
    /// Quota snapshot map from the raw Copilot user-response passthrough, with chat, completions, premium-interactions, and other entries.
    pub quota_snapshots: Option<CopilotUserResponseQuotaSnapshots>,
    /// Whether the user's telemetry is subject to restricted-data handling.
    pub restricted_telemetry: Option<bool>,
    /// Raw passthrough of the Copilot API `te` flag for the user (an opaque server-side eligibility signal surfaced in telemetry); not otherwise interpreted by the runtime.
    pub te: Option<bool>,
    /// Whether the account is on usage-based (token/AI-credit) billing rather than a fixed premium-request quota.
    pub token_based_billing: Option<bool>,
}
crate::record_serde::wire_record!(CopilotUserResponse { access_type_sku: "access_type_sku" if is_none, analytics_tracking_id: "analytics_tracking_id" if is_none, assigned_date: "assigned_date" if is_none, can_signup_for_limited: "can_signup_for_limited" if is_none, can_upgrade_plan: "can_upgrade_plan" if is_none, chat_enabled: "chat_enabled" if is_none, cli_remote_control_enabled: "cli_remote_control_enabled" if is_none, cloud_session_storage_enabled: "cloud_session_storage_enabled" if is_none, codex_agent_enabled: "codex_agent_enabled" if is_none, copilot_plan: "copilot_plan" if is_none, copilotignore_enabled: "copilotignore_enabled" if is_none, endpoints: "endpoints" if is_none, enterprise_list: "enterprise_list" if is_none, is_mcp_enabled: "is_mcp_enabled" if is_none, is_staff: "is_staff" if is_none, limited_user_quotas: "limited_user_quotas" if is_none, limited_user_reset_date: "limited_user_reset_date" if is_none, login: "login" if is_none, monthly_quotas: "monthly_quotas" if is_none, organization_list: "organization_list" if is_none, organization_login_list: "organization_login_list" if is_none, quota_reset_date: "quota_reset_date" if is_none, quota_reset_date_utc: "quota_reset_date_utc" if is_none, quota_snapshots: "quota_snapshots" if is_none, restricted_telemetry: "restricted_telemetry" if is_none, te: "te" if is_none, token_based_billing: "token_based_billing" if is_none });

/// Authentication-info input variant for GitHub-internal HMAC auth, carrying the public GitHub host and HMAC secret.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HMACAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// HMAC secret used to sign requests.
    pub hmac: String,
    /// Authentication host. HMAC auth always targets the public GitHub host.
    pub host: HMACAuthInfoHost,
    /// HMAC-based authentication used by GitHub-internal services.
    pub r#type: HMACAuthInfoType,
}
crate::record_serde::wire_record!(HMACAuthInfo { copilot_user: "copilotUser" if is_none, hmac: "hmac", host: "host", r#type: "type" });

/// Authentication-info input variant for a token sourced from an environment variable, with host, optional login, token, and env var name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Name of the environment variable the token was sourced from.
    pub env_var: String,
    /// Authentication host (e.g. https://github.com or a GHES host).
    pub host: String,
    /// User login associated with the token. Undefined for server-to-server tokens (those starting with `ghs_`).
    pub login: Option<String>,
    /// The token value itself. Treat as a secret.
    pub token: String,
    /// Personal access token (PAT) or server-to-server token sourced from an environment variable.
    pub r#type: EnvAuthInfoType,
}
crate::record_serde::wire_record!(EnvAuthInfo { copilot_user: "copilotUser" if is_none, env_var: "envVar", host: "host", login: "login" if is_none, token: "token", r#type: "type" });

/// Authentication-info input variant for SDK-configured token authentication, carrying host and the secret token value.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct TokenAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host.
    pub host: String,
    /// Opaque native GitHub credential registration backing this token identity, when applicable.
    pub registration_id: Option<String>,
    /// The token value itself. Treat as a secret.
    pub token: String,
    /// SDK-side token authentication; the host configured the token directly via the SDK.
    pub r#type: TokenAuthInfoType,
}
crate::record_serde::wire_record!(TokenAuthInfo { copilot_user: "copilotUser" if is_none, host: "host", registration_id: "registrationId" if is_none, token: "token", r#type: "type" });

/// Authentication-info variant backed by an SDK GitHub token callback. It carries routing metadata but never a plaintext token.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct TokenProviderAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host.
    pub host: String,
    /// Opaque SDK callback registration identifier.
    pub registration_id: String,
    /// SDK callback-backed GitHub token authentication.
    pub r#type: TokenProviderAuthInfoType,
}
crate::record_serde::wire_record!(TokenProviderAuthInfo { copilot_user: "copilotUser" if is_none, host: "host", registration_id: "registrationId", r#type: "type" });

/// Authentication-info variant for direct Copilot API token auth sourced from environment variables, with public GitHub host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CopilotApiTokenAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host (always the public GitHub host).
    pub host: CopilotApiTokenAuthInfoHost,
    /// Direct Copilot API authentication via the `GITHUB_COPILOT_API_TOKEN` + `COPILOT_API_URL` environment-variable pair. The token itself is read from the environment by the runtime, not carried in this struct.
    pub r#type: CopilotApiTokenAuthInfoType,
}
crate::record_serde::wire_record!(CopilotApiTokenAuthInfo { copilot_user: "copilotUser" if is_none, host: "host", r#type: "type" });

/// Authentication-info variant for OAuth user auth, with host and login; the token remains in the runtime secret store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UserAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host.
    pub host: String,
    /// OAuth user login.
    pub login: String,
    /// OAuth user authentication. The token itself is held in the runtime's secret token store (keyed by host+login) and is NOT carried in this struct.
    pub r#type: UserAuthInfoType,
}
crate::record_serde::wire_record!(UserAuthInfo { copilot_user: "copilotUser" if is_none, host: "host", login: "login", r#type: "type" });

/// An interactive account whose model provider owns its credentials. It carries no GitHub credential.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AccountAuthInfo {
    /// Host coordinate owned by the account's model provider.
    pub host: String,
    /// Login identifying the provider-owned account.
    pub login: String,
    /// Provider-owned account authentication.
    pub r#type: AccountAuthInfoType,
}
crate::record_serde::wire_record!(AccountAuthInfo {
    host: "host",
    login: "login",
    r#type: "type"
});

/// Authentication-info input variant for GitHub CLI credentials, carrying host, login, and the `gh auth token` value.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GhCliAuthInfo {
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host.
    pub host: String,
    /// User login as reported by `gh auth status`.
    pub login: String,
    /// The token returned by `gh auth token`. Treat as a secret.
    pub token: String,
    /// Authentication via the `gh` CLI's saved credentials.
    pub r#type: GhCliAuthInfoType,
}
crate::record_serde::wire_record!(GhCliAuthInfo { copilot_user: "copilotUser" if is_none, host: "host", login: "login", token: "token", r#type: "type" });

/// Authentication-info input variant for API-key authentication to a non-GitHub LLM provider, carrying the secret `apiKey` and host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ApiKeyAuthInfo {
    /// The API key. Treat as a secret.
    pub api_key: String,
    /// Snapshot of the authenticated user's Copilot subscription info, if known. Mirrors the GitHub API `/copilot_internal/v2/token` user response shape — the runtime trusts this verbatim and does not re-fetch when set.
    pub copilot_user: Option<CopilotUserResponse>,
    /// Authentication host.
    pub host: String,
    /// API-key authentication for non-GitHub LLM providers (e.g. when running BYOM-style).
    pub r#type: ApiKeyAuthInfoType,
}
crate::record_serde::wire_record!(ApiKeyAuthInfo { api_key: "apiKey", copilot_user: "copilotUser" if is_none, host: "host", r#type: "type" });

/// Credential-free authentication identity safe to expose to hosts and user interfaces.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthIdentity {
    /// Snapshot of the authenticated user's Copilot subscription info, if known
    pub copilot_user: Option<CopilotUserResponse>,
    /// Name of the environment variable that supplied the credential, when applicable
    pub env_var: Option<String>,
    /// Authentication host
    pub host: String,
    /// Authenticated login, when available
    pub login: Option<String>,
    /// Opaque SDK GitHub credential registration backing this identity. Routing metadata only; never a credential.
    pub registration_id: Option<String>,
    /// Authentication type
    pub r#type: AuthInfoType,
}
crate::record_serde::wire_record!(AuthIdentity { copilot_user: "copilotUser" if is_none, env_var: "envVar" if is_none, host: "host", login: "login" if is_none, registration_id: "registrationId" if is_none, r#type: "type" });

/// Agent metadata, including identifiers, display details, source, tools, model, models, reasoning effort, MCP servers, skills, and file path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentInfo {
    /// Description of the agent's purpose
    pub description: String,
    /// Whether model-driven invocation is disabled for this agent.
    pub disable_model_invocation: Option<bool>,
    /// Human-readable display name
    pub display_name: String,
    /// Declarative hook actions keyed by event name, executed only while this agent is active.
    pub hooks: Option<HashMap<String, Vec<serde_json::Value>>>,
    /// Stable identifier for selection. For most agents this is the same as `name`; for plugin/builtin agents it may differ. Always populated; defaults to `name` when no distinct id was assigned.
    pub id: String,
    /// MCP server configurations attached to this agent, keyed by server name. Server config shape mirrors the MCP `mcpServers` schema.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub mcp_servers: Option<HashMap<String, serde_json::Value>>,
    /// Authored preferred model id for this agent. Runtime model selection may choose a different model; omitted means no authored preference.
    pub model: Option<String>,
    /// Whether authored models are preferences or required constraints.
    pub model_policy: Option<AgentModelPolicy>,
    /// Authored preferred model ids for this agent, in priority order. Runtime model selection chooses the first available model; omitted means no authored preference.
    pub models: Option<Vec<String>>,
    /// Name of the agent. Use `id` as the stable selection identifier.
    pub name: String,
    /// Absolute local file path of the agent definition. Only set for file-based agents loaded from disk; remote agents do not have a path.
    pub path: Option<String>,
    /// Opaque identity of the plugin owning this agent. Matches plugins.list().plugins[].id on the same runtime host. Compare the complete value without parsing it. Omitted for non-plugin agents.
    pub plugin_id: Option<String>,
    /// Authored base prompt for the agent. Runtime prompt assembly may add dynamic context at invocation time. Omitted from `session.agent.list` unless `includePrompt` is true.
    pub prompt: Option<String>,
    /// Authored reasoning effort for this agent. Applied on selection to models that support it; omitted means no authored preference.
    pub reasoning_effort: Option<String>,
    /// Skill names preloaded into this agent's context. Omitted means none.
    pub skills: Option<Vec<String>>,
    /// Where the agent definition was loaded from
    pub source: Option<AgentInfoSource>,
    /// Allowed tool names for this agent. Empty array means none; omitted means inherit defaults.
    pub tools: Option<Vec<String>>,
    /// Whether the agent can be selected directly by the user. Agents marked `false` are subagent-only.
    pub user_invocable: Option<bool>,
}
crate::record_serde::wire_record!(AgentInfo { description: "description", disable_model_invocation: "disableModelInvocation" if is_none, display_name: "displayName", hooks: "hooks" if is_none, id: "id", mcp_servers: "mcpServers" if is_none, model: "model" if is_none, model_policy: "modelPolicy" if is_none, models: "models" if is_none, name: "name", path: "path" if is_none, plugin_id: "pluginId" if is_none, prompt: "prompt" if is_none, reasoning_effort: "reasoningEffort" if is_none, skills: "skills" if is_none, source: "source" if is_none, tools: "tools" if is_none, user_invocable: "userInvocable" if is_none });

/// Vision-specific limits
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideLimitsVision {
    /// Maximum image size in bytes
    pub max_prompt_image_size: Option<i64>,
    /// Maximum number of images per prompt
    pub max_prompt_images: Option<i64>,
    /// MIME types the model accepts
    pub supported_media_types: Option<Vec<String>>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideLimitsVision { max_prompt_image_size: "max_prompt_image_size" if is_none, max_prompt_images: "max_prompt_images" if is_none, supported_media_types: "supported_media_types" if is_none });

/// Token limits for prompts, outputs, and context window
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideLimits {
    /// Maximum total context window size in tokens
    pub max_context_window_tokens: Option<i64>,
    /// Maximum number of output/completion tokens
    pub max_output_tokens: Option<i64>,
    /// Maximum number of prompt/input tokens
    pub max_prompt_tokens: Option<i64>,
    /// Vision-specific limits
    pub vision: Option<ModelCapabilitiesOverrideLimitsVision>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideLimits { max_context_window_tokens: "max_context_window_tokens" if is_none, max_output_tokens: "max_output_tokens" if is_none, max_prompt_tokens: "max_prompt_tokens" if is_none, vision: "vision" if is_none });

/// Feature flags indicating what the model supports
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideSupports {
    /// Resolved Anthropic adaptive-thinking capability — unsupported / optional / required / adaptive_only. 'required' models reject thinking.type='enabled' with HTTP 400 but still accept 'disabled' (e.g. opus-4.7/4.8/5, sonnet-5); 'adaptive_only' models accept nothing but 'adaptive' (e.g. fable, mythos).
    pub adaptive_thinking: Option<AdaptiveThinkingSupport>,
    /// Whether this model supports reasoning effort configuration
    pub reasoning_effort: Option<bool>,
    /// Whether this model supports canonical tool calling
    pub tool_calls: Option<bool>,
    /// Whether this model supports vision/image input
    pub vision: Option<bool>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideSupports { adaptive_thinking: "adaptive_thinking" if is_none, reasoning_effort: "reasoningEffort" if is_none, tool_calls: "toolCalls" if is_none, vision: "vision" if is_none });

/// Optional capability overrides (vision, tool_calls, reasoning, etc.).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverride {
    /// Token limits for prompts, outputs, and context window
    pub limits: Option<ModelCapabilitiesOverrideLimits>,
    /// Feature flags indicating what the model supports
    pub supports: Option<ModelCapabilitiesOverrideSupports>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverride { limits: "limits" if is_none, supports: "supports" if is_none });

/// A BYOK model definition referencing a named provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProviderModelConfig {
    /// Forward-compatible persisted on-device Auto annotation. The live counterpart is metadata.autoLocal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auto_local: Option<serde_json::Value>,
    /// Optional capability overrides (vision, tool_calls, reasoning, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<ModelCapabilitiesOverride>,
    /// Provider-local model id, unique within its provider. The session-wide selection id (shown in the model list and passed to switchTo) is the provider-qualified `provider/id`.
    pub id: String,
    /// Maximum context window tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_context_window_tokens: Option<i64>,
    /// Maximum output tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_output_tokens: Option<i64>,
    /// Maximum prompt/input tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_prompt_tokens: Option<i64>,
    /// Provider-published model metadata, preserved verbatim as the public Model.metadata object.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// Well-known base model id used for behavior/capability/config lookup. Defaults to `id`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_id: Option<String>,
    /// Display name for model pickers. Defaults to the provider-qualified selection id (`provider/id`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Name of the configured provider that serves this model.
    pub provider: String,
    /// System-message configuration used when the runtime builds the standard prompt for this provider-qualified model, including general-purpose subagents. It uses the same object hierarchy as session-level systemMessage configuration, except transform actions are rejected because the current callback protocol is not model-scoped. When present, it overrides the session-wide configuration on those prompt paths. Selected custom-agent and specialized-subagent prompts remain authoritative.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<serde_json::Value>,
    /// The model name sent to the provider API for inference. Defaults to `id`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wire_model: Option<String>,
}

impl ProviderModelConfig {
    fn deserialize_token_limit<'de, D>(deserializer: D) -> Result<Option<i64>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let Some(number) = Option::<serde_json::Number>::deserialize(deserializer)? else {
            return Ok(None);
        };
        if let Some(value) = number.as_i64() {
            return Ok(Some(value));
        }
        if let Some(value) = number.as_f64()
            && value.fract() == 0.0
            // i64::MAX rounds up to 2^63 as f64, so the upper bound is exclusive.
            && ((i64::MIN as f64)..-(i64::MIN as f64)).contains(&value)
        {
            return Ok(Some(value as i64));
        }
        Err(serde::de::Error::custom(
            "expected an integer token limit in the i64 range",
        ))
    }
}

/// Azure-specific provider options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderConfigAzure {
    /// API version. When set, uses the versioned deployment route. When omitted, uses the GA versionless v1 route.
    pub api_version: Option<String>,
}
crate::record_serde::wire_record!(ProviderConfigAzure { api_version: "apiVersion" if is_none });

/// Editable provider settings, without credential values or custom headers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationProviderInput {
    /// Safe Azure connection settings, including API version.
    pub azure: Option<ProviderConfigAzure>,
    /// Provider endpoint.
    pub base_url: String,
    /// Non-secret local model runtime options, such as the WinML device.
    pub local_model: Option<serde_json::Value>,
    /// Serving product used for attribution.
    pub model_provider: Option<ProviderConfigModelProvider>,
    /// Provider name used to qualify model selection ids.
    pub name: String,
    /// HTTP or WebSocket transport.
    pub transport: Option<ProviderConfigTransport>,
    /// Provider protocol family.
    pub r#type: Option<SavedProviderConfigurationType>,
    /// OpenAI-compatible wire API.
    pub wire_api: Option<ProviderConfigWireApi>,
}
crate::record_serde::wire_record!(SavedProviderConfigurationProviderInput { azure: "azure" if is_none, base_url: "baseUrl", local_model: "localModel" if is_none, model_provider: "modelProvider" if is_none, name: "name", transport: "transport" if is_none, r#type: "type" if is_none, wire_api: "wireApi" if is_none });

/// Pointer to a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubRepoRef {
    /// Numeric GitHub repository id
    pub id: Option<i64>,
    /// Repository name (without owner)
    pub name: String,
    /// Repository owner login (user or organization)
    pub owner: String,
}
crate::record_serde::wire_record!(GitHubRepoRef { id: "id" if is_none, name: "name", owner: "owner" });

/// Credential-free identity metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthIdentityMetadata {
    /// Identity host.
    pub host: String,
    /// User login.
    pub login: String,
    /// Authentication type.
    pub r#type: AuthInfoType,
}
crate::record_serde::wire_record!(AuthIdentityMetadata {
    host: "host",
    login: "login",
    r#type: "type"
});

/// A credential-free account choice after sign-in.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginAccount {
    /// Host coordinate owned by the selected account's provider.
    pub host: String,
    /// Provider kind that owns this account choice.
    pub kind: AccountKind,
    /// Human-readable login for the account choice.
    pub login: String,
    /// Opaque identifier supplied to the next login step to select this account.
    pub selection_id: String,
}
crate::record_serde::wire_record!(AuthLoginAccount {
    host: "host",
    kind: "kind",
    login: "login",
    selection_id: "selectionId"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepOpenUrl {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepOpenUrlKind,
    /// Authorize URL the consumer should open in a browser (consumer-driven browser-open).
    pub url: String,
}
crate::record_serde::wire_record!(AuthLoginStepOpenUrl {
    kind: "kind",
    url: "url"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepInputRequired {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepInputRequiredKind,
    /// Prompt for the value the provider needs; the consumer supplies it as advance input (e.g. a GitHub Enterprise Cloud host, *.ghe.com).
    pub prompt: String,
}
crate::record_serde::wire_record!(AuthLoginStepInputRequired {
    kind: "kind",
    prompt: "prompt"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepAwaiting {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepAwaitingKind,
}
crate::record_serde::wire_record!(AuthLoginStepAwaiting { kind: "kind" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepNeedsInteraction {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepNeedsInteractionKind,
}
crate::record_serde::wire_record!(AuthLoginStepNeedsInteraction { kind: "kind" });

/// Result of an interactive login flow. Pending consent or account selection is not terminal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginResultDto {
    /// Available accounts when sign-in is awaiting account selection, ordered with Microsoft 365 first.
    pub accounts: Option<Vec<AuthLoginAccount>>,
    /// Host that was signed in, when completed.
    pub host: Option<String>,
    /// Login that was signed in, when completed.
    pub login: Option<String>,
    /// Current disposition of the login, including pending user decisions.
    pub status: AuthLoginResultStatus,
}
crate::record_serde::wire_record!(AuthLoginResultDto { accounts: "accounts" if is_none, host: "host" if is_none, login: "login" if is_none, status: "status" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepCompleted {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepCompletedKind,
    /// Login result. When status is needs-plaintext-consent or needs-account-selection, advance with the user's decision to continue.
    pub result: AuthLoginResultDto,
}
crate::record_serde::wire_record!(AuthLoginStepCompleted {
    kind: "kind",
    result: "result"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepError {
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepErrorKind,
    /// Human-readable failure message.
    pub message: String,
}
crate::record_serde::wire_record!(AuthLoginStepError {
    kind: "kind",
    message: "message"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginStepDeviceCode {
    /// Seconds until the one-time code expires, measured from when this step was produced. Consumers can show a countdown or pick their own timeout instead of blocking until the provider rejects the code.
    pub expires_in: i64,
    /// Minimum seconds between authorization polls required by the provider. The runtime performs the polling itself for the duration of the single `advance` call, so this is informational (for example to drive a progress indicator); repeating `advance` is rejected as already advancing.
    pub interval: i64,
    /// Login flow step variant discriminator.
    pub kind: AuthLoginStepDeviceCodeKind,
    /// One-time code the user enters at the verification URI.
    pub user_code: String,
    /// Page where the user enters the one-time code (consumer-driven browser-open).
    pub verification_uri: String,
}
crate::record_serde::wire_record!(AuthLoginStepDeviceCode {
    expires_in: "expiresIn",
    interval: "interval",
    kind: "kind",
    user_code: "userCode",
    verification_uri: "verificationUri"
});

/// A cached base identity a provider can reuse for login without asking the user to choose an account again.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthLoginIdentity {
    /// Opaque provider-owned identity id. Pass it back to accounts.login.begin; consumers must not parse it.
    pub account_id: String,
    /// Whether this base identity owns the session's active derived account.
    pub active: bool,
    /// Human-readable sign-in label owned by the runtime.
    pub label: String,
}
crate::record_serde::wire_record!(AuthLoginIdentity {
    account_id: "accountId",
    active: "active",
    label: "label"
});

/// An authentication provider offered for interactive login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthProviderDescriptor {
    /// Whether this transport currently supports signing in with this provider.
    pub available: bool,
    /// The authentication provider kind.
    pub kind: LoginProviderKind,
    /// Human-readable menu label, owned by the runtime so every consumer renders identical text.
    pub label: String,
}
crate::record_serde::wire_record!(AuthProviderDescriptor {
    available: "available",
    kind: "kind",
    label: "label"
});

/// Validation error from an authentication attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AuthValidationError {
    /// Optional message returned by GitHub
    pub github_message: Option<String>,
    /// Authentication validation error message
    pub message: String,
}
crate::record_serde::wire_record!(AuthValidationError { github_message: "githubMessage" if is_none, message: "message" });

/// Current per-window credit limit and consumption for an autopilot objective.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotObjectiveCreditLimit {
    /// Configured AI-credit cap, when one is set.
    pub credits: Option<f64>,
    /// Window consumption in fractional AI credits, for display.
    pub credits_used: f64,
    /// Exact window consumption in non-negative integer nano-AIU, encoded as a decimal string.
    pub credits_used_nano_aiu: String,
}
crate::record_serde::wire_record!(AutopilotObjectiveCreditLimit { credits: "credits" if is_none, credits_used: "creditsUsed", credits_used_nano_aiu: "creditsUsedNanoAiu" });

/// Public, persistence-independent projection of an autopilot objective.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotObjectiveState {
    /// Optional summary recorded when the objective completed.
    pub completion_summary: Option<String>,
    /// Exact lifetime AI-credit consumption in non-negative integer nano-AIU, encoded as a decimal string.
    pub credit_count_nano_aiu: String,
    /// Current per-window consumption and optional cap, when a credit-tracking window is present.
    pub credit_limit: Option<AutopilotObjectiveCreditLimit>,
    /// Session-local objective identifier.
    pub id: i64,
    /// User-provided objective text.
    pub objective: String,
    /// Optional reason the objective is paused.
    pub pause_reason: Option<String>,
    /// Current normalized lifecycle status.
    pub status: AutopilotObjectiveStatus,
    /// Number of objective turns started.
    pub turn_count: i64,
}
crate::record_serde::wire_record!(AutopilotObjectiveState { completion_summary: "completionSummary" if is_none, credit_count_nano_aiu: "creditCountNanoAiu", credit_limit: "creditLimit" if is_none, id: "id", objective: "objective", pause_reason: "pauseReason" if is_none, status: "status", turn_count: "turnCount" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotOptions {
    /// Enable runtime-owned automatic continuation while in Autopilot mode. Defaults to true for SDK sessions; does not change the session mode.
    pub continuation_enabled: Option<bool>,
    /// Maximum automatic continuations outside an active objective. Zero allows none; defaults to five. Omitted values preserve the current limit on update or warm resume.
    pub max_continues: Option<i64>,
}
crate::record_serde::wire_record!(AutopilotOptions { continuation_enabled: "continuationEnabled" if is_none, max_continues: "maxContinues" if is_none });

/// Server-driven promotion for an Auto routing preference. It can be announced before it starts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutoTierBillingPromo {
    /// Percentage discount (0-100) while the promotion is active. Absent or 0 means the promotion is an announcement only: do not present a discount.
    pub discount_percent: Option<f64>,
    /// UTC ISO 8601 timestamp marking when the promotion ends. Absent means open-ended. The runtime omits promotions that have already ended.
    pub ends_at: Option<String>,
    /// Stable identifier for the promotion campaign.
    pub id: Option<String>,
    /// Human-readable promotion message. Does not include the start or end timestamps.
    pub message: Option<String>,
    /// Whether the service asked hosts to give this promotion a prominent surface, such as a banner. Absent means the service expressed no preference, so hosts should apply their own default rather than read it as `false`.
    pub show_banner: Option<bool>,
    /// UTC ISO 8601 timestamp marking when the promotion starts. The service can announce a promotion before it starts, so do not present the discount as active before this time. Absent means it has already started.
    pub starts_at: Option<String>,
}
crate::record_serde::wire_record!(AutoTierBillingPromo { discount_percent: "discountPercent" if is_none, ends_at: "endsAt" if is_none, id: "id" if is_none, message: "message" if is_none, show_banner: "showBanner" if is_none, starts_at: "startsAt" if is_none });

/// Billing information for a server-advertised routing preference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutoTierBilling {
    /// Promotion the service attached to this preference, if any.
    pub promo: Option<AutoTierBillingPromo>,
}
crate::record_serde::wire_record!(AutoTierBilling { promo: "promo" if is_none });

/// Availability of a server-advertised routing preference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutoTierStatus {
    /// Whether the provider permits selecting this preference.
    pub enabled: bool,
    /// Human-readable explanation of availability.
    pub message: Option<String>,
    /// Whether hosts should list this preference in a picker. Absent means visible. A hidden preference can still be requested explicitly through the SDK.
    pub picker_visible: Option<bool>,
    /// Extensible machine-readable unavailability reason.
    pub reason: Option<String>,
}
crate::record_serde::wire_record!(AutoTierStatus { enabled: "enabled", message: "message" if is_none, picker_visible: "pickerVisible" if is_none, reason: "reason" if is_none });

/// A server-advertised routing preference. Identifiers and execution types are extensible.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutoTierDescriptor {
    /// Billing information for this preference, such as an announced or active promotion.
    pub billing: Option<AutoTierBilling>,
    /// Description displayed beside the preference.
    pub description: String,
    /// Human-readable label, not a routing identifier.
    pub display_name: String,
    /// Runtime-owned managed Fusion tier represented by this execution descriptor, if any.
    pub fusion_tier: Option<String>,
    /// Opaque routing identifier transmitted unchanged to the provider.
    pub id: String,
    /// Current account-specific availability.
    pub status: AutoTierStatus,
    /// Execution kind; supported preferences use `auto` or the runtime's `fusion` modes.
    pub r#type: String,
}
crate::record_serde::wire_record!(AutoTierDescriptor { billing: "billing" if is_none, description: "description", display_name: "displayName", fusion_tier: "fusionTier" if is_none, id: "id", status: "status", r#type: "type" });

/// Account-bound discovery metadata for the virtual `auto` model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutoTierMetadata {
    /// Provider-default preference, used only when no explicit preference exists.
    pub default_tier: String,
    /// Provider that supplied this metadata, when the catalog is provider-attributed.
    pub provider_id: Option<String>,
    /// Routing preferences in the server's presentation order.
    pub tiers: Vec<AutoTierDescriptor>,
}
crate::record_serde::wire_record!(AutoTierMetadata { default_tier: "defaultTier", provider_id: "providerId" if is_none, tiers: "tiers" });

/// External SDK input for a named custom model provider. Ingested by the native protocol boundary before host dispatch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NamedProviderConfig {
    /// Static API key used to authenticate provider requests.
    pub api_key: Option<String>,
    /// Command run before each request to acquire a fresh API key. Failure
    /// fails the request without static-key or anonymous fallback. Bearer
    /// authentication takes precedence.
    pub api_key_command: Option<String>,
    /// Azure authentication configuration for the provider.
    pub azure: Option<ProviderConfigAzure>,
    /// Base URL for provider API requests.
    pub base_url: String,
    /// Static bearer token used to authenticate provider requests.
    pub bearer_token: Option<String>,
    /// Whether the host supplies bearer tokens dynamically.
    pub has_bearer_token_provider: Option<bool>,
    /// Additional HTTP headers included with provider requests.
    pub headers: Option<HashMap<String, String>>,
    /// Local model runtime options for a WinML provider whose base URL is its GGUF path.
    pub local_model: Option<serde_json::Value>,
    /// The product serving the provider's models, reported in telemetry as `model_provider`. Only affects telemetry.
    pub model_provider: Option<ProviderConfigModelProvider>,
    /// Unique provider name used to qualify model selection IDs.
    pub name: String,
    /// Transport used to communicate with the provider.
    pub transport: Option<ProviderConfigTransport>,
    /// Provider protocol family.
    pub r#type: Option<ProviderConfigType>,
    /// Wire API used to communicate with the provider.
    pub wire_api: Option<ProviderConfigWireApi>,
}
crate::record_serde::wire_record!(NamedProviderConfig { api_key: "apiKey" if is_none, api_key_command: "apiKeyCommand" if is_none, azure: "azure" if is_none, base_url: "baseUrl", bearer_token: "bearerToken" if is_none, has_bearer_token_provider: "hasBearerTokenProvider" if is_none, headers: "headers" if is_none, local_model: "localModel" if is_none, model_provider: "modelProvider" if is_none, name: "name", transport: "transport" if is_none, r#type: "type" if is_none, wire_api: "wireApi" if is_none });

/// Provider and model rows prepared by provider discovery to save for future sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderSaveConfigurationRequest {
    /// Discovered model definition. If both saved rows exist, neither row changes.
    pub model: ProviderModelConfig,
    /// Discovered provider connection. An existing saved provider remains unchanged.
    pub provider: NamedProviderConfig,
}
crate::record_serde::wire_record!(ModelProviderSaveConfigurationRequest {
    model: "model",
    provider: "provider"
});

/// A well-known model in the runtime's built-in catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct BuiltInModelCatalogEntry {
    /// Well-known runtime model ID suitable for provider or provider-model metadata. This is not necessarily the provider-facing deployment or model name and does not indicate CAPI entitlement or provider availability.
    pub id: String,
}
crate::record_serde::wire_record!(BuiltInModelCatalogEntry { id: "id" });

/// Canvas action that the agent or host can invoke. To discover the input schema for a particular action, call the list_canvas_capabilities tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasAction {
    /// Description of the action
    pub description: Option<String>,
    /// JSON Schema for the action input
    pub input_schema: Option<serde_json::Value>,
    /// Action name exposed by the canvas provider
    pub name: String,
}
crate::record_serde::wire_record!(CanvasAction { description: "description" if is_none, input_schema: "inputSchema" if is_none, name: "name" });

/// Canvas available in the current session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredCanvas {
    /// Actions the agent or host may invoke on an open instance
    pub actions: Option<Vec<CanvasAction>>,
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Short, single-sentence description shown to the agent in canvas catalogs.
    pub description: String,
    /// Human-readable canvas name
    pub display_name: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Owning extension display name, when available
    pub extension_name: Option<String>,
    /// Host-local PNG path for the canvas icon, when supplied
    pub icon: Option<String>,
    /// JSON Schema for canvas open input
    pub input_schema: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(DiscoveredCanvas { actions: "actions" if is_none, canvas_id: "canvasId", description: "description", display_name: "displayName", extension_id: "extensionId", extension_name: "extensionName" if is_none, icon: "icon" if is_none, input_schema: "inputSchema" if is_none });

/// Open canvas instance snapshot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct OpenCanvasInstance {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Owning extension display name, when available
    pub extension_name: Option<String>,
    /// Host-local PNG path for the canvas icon, when supplied
    pub icon: Option<String>,
    /// Input supplied when the instance was opened
    pub input: Option<serde_json::Value>,
    /// Stable caller-supplied canvas instance identifier
    pub instance_id: String,
    /// Provider-supplied status text
    pub status: Option<String>,
    /// Rendered title
    pub title: Option<String>,
    /// URL for web-rendered canvases
    pub url: Option<String>,
}
crate::record_serde::wire_record!(OpenCanvasInstance { canvas_id: "canvasId", extension_id: "extensionId", extension_name: "extensionName" if is_none, icon: "icon" if is_none, input: "input" if is_none, instance_id: "instanceId", status: "status" if is_none, title: "title" if is_none, url: "url" if is_none });

/// Options scoped to the built-in CAPI (Copilot API) provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CapiSessionOptions {
    /// Routing preference for sessions whose model is `auto`. On create or cold resume, this establishes the preference sent as `tier` on CAPI `/auto` requests; when omitted on cold resume, the runtime restores the last committed preference. On resident resume, a different value requests a safe switch after resume succeeds and cannot change an in-flight turn. Successful switches are persisted for later cold resume. When no preference is supplied or restored, CAPI default routing is used. `fast` is an integrator-only latency preset, not a first-party GitHub Copilot product preference. For Copilot Fusion, prefer `fusionTier`: the `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces remain accepted but are deprecated.
    pub auto_tier: Option<AutoTier>,
    /// Enables the session's built-in Copilot model catalog, authentication and
    /// inference, including subagent and ancillary model work. Defaults to true
    /// on create; omission on resume preserves the policy. Does not disable
    /// GitHub identity, tools, MCP, managed settings or separate telemetry.
    pub enabled: Option<bool>,
    /// Whether to use WebSocket transport for the CAPI Responses API. Enabled by default when the model advertises `ws:/responses` support; set to `false` to force the HTTP Responses transport in environments where WebSockets are blocked (e.g. behind a proxy). Setting this to `false` is equivalent to the `COPILOT_CLI_DISABLE_WEBSOCKET_RESPONSES` environment variable.
    pub enable_web_socket_responses: Option<bool>,
    /// Copilot Fusion tier for sessions whose model is `fusion` or `auto`, matching the CLI's `--fusion-tier`. The runtime resolves the tier to the routing preference it stands for and then applies it exactly like `autoTier`, so `session.model.getCurrent` and `session.model_change` report that preference as `autoTier`; `session.model.getCurrent` also reports the resolved tier as `fusionTier` once the session's Auto tier metadata is known. Cannot be combined with `autoTier`.
    pub fusion_tier: Option<FusionTier>,
}
crate::record_serde::wire_record!(CapiSessionOptions { auto_tier: "autoTier" if is_none, enabled: "enabled" if is_none, enable_web_socket_responses: "enableWebSocketResponses" if is_none, fusion_tier: "fusionTier" if is_none });

/// An optional catalog authentication exchange did not establish the caller's identity. Anonymous search remains supported; this refusal is reserved for an operation that cannot continue after the attempted exchange. It is distinct from `policy-rejected` and from a network failure, and the reason identifies the recovery action.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogAuthenticationRequiredError {
    /// Discriminator: the caller is not authenticated
    pub kind: CatalogAuthenticationRequiredErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a credential or token, nor a query, URL, handle, or secret.
    pub message: String,
    /// Why authentication failed. Only an expired credential justifies attempting a silent refresh; an absent or rejected credential requires sign-in.
    pub reason: CatalogAuthenticationRequiredReason,
}
crate::record_serde::wire_record!(CatalogAuthenticationRequiredError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// The protocol version and capability set a caller requires, supplied on every catalog request so negotiation cannot be skipped by omission.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogClientContract {
    /// SDK protocol version the caller was generated against. A caller below the runtime's minimum supported version is refused rather than served a partial result.
    pub protocol_version: i64,
    /// Wire features the caller requires the runtime to understand. Identifiers are bounded but extensible so a newer caller can negotiate with an older runtime. Requiring an unknown feature yields a typed refusal listing what is understood, never a partial grant. A grant does not promise that a deployment has enabled the operation; typed unavailable results report that separately.
    pub required_capabilities: Vec<String>,
}
crate::record_serde::wire_record!(CatalogClientContract {
    protocol_version: "protocolVersion",
    required_capabilities: "requiredCapabilities"
});

/// An upstream catalog response broke the wire contract. Most importantly, every result must carry exactly one of a URL or embedded data: a result carrying both, or neither, is refused here rather than being guessed at.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogContractViolationError {
    /// Discriminator: the upstream response broke the contract
    pub kind: CatalogContractViolationErrorKind,
    /// Human-readable explanation, safe to surface. Never echoes response content, nor a query, URL, handle, or secret.
    pub message: String,
    /// Which rule the response broke.
    pub reason: CatalogContractViolationReason,
}
crate::record_serde::wire_record!(CatalogContractViolationError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// The protocol version and capability set the runtime actually honoured for a successful catalog operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogNegotiatedContract {
    /// Wire features the runtime understood for this operation. Includes the five original catalog capabilities and only explicitly requested supported additions, in supported order without duplicates. Capabilities that introduce new success-union variants or operations are therefore included only when explicitly required, preserving older protocol-v3 clients. Always a superset of the caller's required features, because any shortfall is a refusal instead. Operation availability remains a separate typed result.
    pub granted_capabilities: Vec<CatalogCapability>,
    /// Protocol version of the runtime that served the request.
    pub runtime_protocol_version: i64,
}
crate::record_serde::wire_record!(CatalogNegotiatedContract {
    granted_capabilities: "grantedCapabilities",
    runtime_protocol_version: "runtimeProtocolVersion"
});

/// The request was rejected because a bounded field fell outside its permitted range or a required field was unusable. Pagination may also be rejected by the authority after a continuation request; repeat the search without page.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogInvalidRequestError {
    /// Which request field was rejected.
    pub field: CatalogInvalidRequestField,
    /// Discriminator: the request itself was invalid
    pub kind: CatalogInvalidRequestErrorKind,
    /// Human-readable explanation, safe to surface. Never echoes the offending value, nor a query, URL, handle, or secret.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogInvalidRequestError {
    field: "field",
    kind: "kind",
    message: "message"
});

/// A card could not be parsed or did not satisfy its declared media type's schema.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogMalformedCardError {
    /// Discriminator: the card was malformed
    pub kind: CatalogMalformedCardErrorKind,
    /// Media type the card was interpreted as, when it declared one this runtime recognises.
    pub media_type: Option<CatalogMediaType>,
    /// Human-readable explanation, safe to surface. Never echoes card content, nor a query, URL, handle, or secret.
    pub message: String,
    /// How the card failed validation.
    pub reason: CatalogMalformedCardReason,
}
crate::record_serde::wire_record!(CatalogMalformedCardError { kind: "kind", media_type: "mediaType" if is_none, message: "message", reason: "reason" });

/// The caller's protocol version or required capabilities cannot be honoured. Returned instead of a partial or ambiguous success.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogNegotiationRefusedError {
    /// Discriminator: capability or protocol-version negotiation failed
    pub kind: CatalogNegotiationRefusedErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Lowest caller protocol version this runtime will serve.
    pub minimum_supported_protocol_version: i64,
    /// Whether the version or the capability set was the problem.
    pub reason: CatalogNegotiationRefusedReason,
    /// Protocol version of the runtime that refused the request.
    pub runtime_protocol_version: i64,
    /// Capabilities this runtime can safely advertise to this caller. The complete five-capability protocol-3 legacy set is always present; every capability added after that baseline appears only when the caller required it, so an older closed-enum decoder can still consume a refusal. This list does not imply that every deployment has enabled every operation.
    pub supported_capabilities: Vec<String>,
    /// The subset of the caller's bounded extensible capability identifiers this runtime cannot honour.
    pub unsupported_capabilities: Vec<String>,
}
crate::record_serde::wire_record!(CatalogNegotiationRefusedError {
    kind: "kind",
    message: "message",
    minimum_supported_protocol_version: "minimumSupportedProtocolVersion",
    reason: "reason",
    runtime_protocol_version: "runtimeProtocolVersion",
    supported_capabilities: "supportedCapabilities",
    unsupported_capabilities: "unsupportedCapabilities"
});

/// The runtime could not reach the catalog authority or retrieve a card. Covers being offline as well as transport-level failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogNetworkFailureError {
    /// Discriminator: the network operation failed
    pub kind: CatalogNetworkFailureErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Categorised failure, low cardinality so it can be aggregated without carrying a URL.
    pub reason: CatalogNetworkFailureReason,
    /// Bounded cooldown in seconds before another catalog request should be attempted, when the authority supplied a numeric Retry-After value or the runtime applied its documented fallback.
    pub retry_after_seconds: Option<i32>,
    /// HTTP status code, when the failure was a rejected response.
    pub status_code: Option<i32>,
}
crate::record_serde::wire_record!(CatalogNetworkFailureError { kind: "kind", message: "message", reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none, status_code: "statusCode" if is_none });

/// Registry or enterprise policy refused the operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogPolicyRejectedError {
    /// Discriminator: policy refused the operation
    pub kind: CatalogPolicyRejectedErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Which authority produced the decision.
    pub source: McpPlanPolicySource,
}
crate::record_serde::wire_record!(CatalogPolicyRejectedError {
    kind: "kind",
    message: "message",
    source: "source"
});

/// Catalogue identity retained from a bound candidate or plan at installation time.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstallationCatalogueIdentity {
    /// Catalogue description retained at install planning time.
    pub description: Option<String>,
    /// Human display name retained from the catalogue candidate.
    pub display_name: String,
    /// HTTPS URL of the resource's icon on raw.githubusercontent.com, retained from the catalogue
    /// candidate. Untrusted external content: load it without credentials. Omitted when the
    /// candidate had none.
    pub icon_url: Option<String>,
    /// Catalogue item URL when supplied by the authority.
    pub item_url: Option<String>,
    /// Catalogue publisher retained at install planning time.
    pub publisher: Option<String>,
    /// Authority resource identifier when supplied by the catalogue.
    pub resource_id: Option<String>,
    /// Catalogue authority/source string that supplied the candidate.
    pub source: String,
    /// Catalogue trust observation retained at install planning time.
    pub trust_at_install: Option<serde_json::Value>,
    /// Catalogue version retained at install planning time.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(InstallationCatalogueIdentity { description: "description" if is_none, display_name: "displayName", icon_url: "iconUrl" if is_none, item_url: "itemUrl" if is_none, publisher: "publisher" if is_none, resource_id: "resourceId" if is_none, source: "source", trust_at_install: "trustAtInstall" if is_none, version: "version" if is_none });

/// One reviewed Skill file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationFileReview {
    /// SHA-256 digest of the exact file bytes.
    pub digest: String,
    /// Whether the file is installed with executable permissions.
    pub executable: bool,
    /// Declared media type for the file.
    pub media_type: String,
    /// Relative file path within the Skill root.
    pub path: String,
    /// Exact reviewed file size in bytes.
    pub size_bytes: i64,
}
crate::record_serde::wire_record!(SkillInstallationFileReview {
    digest: "digest",
    executable: "executable",
    media_type: "mediaType",
    path: "path",
    size_bytes: "sizeBytes"
});

/// Source identity retained from Agent Finder and the pinned GitHub descriptor.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationSource {
    /// Digest of the descriptor's bundle manifest.
    pub bundle_digest: String,
    /// Agent Finder materialisation revision identifier.
    pub catalog_revision_id: String,
    /// Digest of the canonical materialisation descriptor.
    pub descriptor_digest: String,
    /// Repository full name, for example owner/name.
    pub repository: String,
    /// GitHub repository database identifier.
    pub repository_id: String,
    /// Agent Finder resource identifier.
    pub resource_id: String,
    /// Pinned Git commit revision.
    pub revision: String,
    /// Root path within the pinned repository.
    pub root: String,
}
crate::record_serde::wire_record!(SkillInstallationSource {
    bundle_digest: "bundleDigest",
    catalog_revision_id: "catalogRevisionId",
    descriptor_digest: "descriptorDigest",
    repository: "repository",
    repository_id: "repositoryId",
    resource_id: "resourceId",
    revision: "revision",
    root: "root"
});

/// A user-facing personal Skill installation location without absolute host paths.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationLocation {
    /// Diagnostics-only absolute host path. Hosts must not display it by default.
    pub diagnostics_absolute_path: Option<String>,
    /// Safe display label, for example ~/.copilot/skills/run-checks.
    pub display_label: String,
    /// Path relative to the Copilot home.
    pub relative_path: String,
    /// Installation scope. Agent Finder Skills are installed in the user's personal Copilot home.
    pub scope: SkillInstallationScope,
}
crate::record_serde::wire_record!(SkillInstallationLocation { diagnostics_absolute_path: "diagnosticsAbsolutePath" if is_none, display_label: "displayLabel", relative_path: "relativePath", scope: "scope" });

/// Review for installing a verified Skill.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationReviewInstall {
    /// Exact reviewed Skill installation action.
    pub action: SkillInstallationReviewInstallAction,
    /// Catalogue identity retained from the bound candidate before consent.
    pub catalogue: InstallationCatalogueIdentity,
    /// Skill description from SKILL.md when present.
    pub description: Option<String>,
    /// Complete verified SKILL.md content. Planning refuses with review-too-large
    /// when this exceeds 262144 UTF-8 bytes; it is never truncated.
    pub entrypoint_content: String,
    /// Relative path of the verified Skill entrypoint.
    pub entrypoint_path: String,
    /// Reviewed files and digests.
    pub files: Vec<SkillInstallationFileReview>,
    /// Installing never grants immediate use; the Skill is written disabled.
    pub installs_disabled: bool,
    /// Skill invocation name from SKILL.md.
    pub name: String,
    /// Exact verified source identity.
    pub source: SkillInstallationSource,
    /// Exact user-scope target location without an absolute host path.
    pub target: SkillInstallationLocation,
    /// Total reviewed payload size in bytes.
    pub total_bytes: i64,
}
crate::record_serde::wire_record!(SkillInstallationReviewInstall { action: "action", catalogue: "catalogue", description: "description" if is_none, entrypoint_content: "entrypointContent", entrypoint_path: "entrypointPath", files: "files", installs_disabled: "installsDisabled", name: "name", source: "source", target: "target", total_bytes: "totalBytes" });

/// Durable verified Skill ownership summary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationSummary {
    /// Catalogue identity retained at install time.
    pub catalogue: InstallationCatalogueIdentity,
    /// Persisted enablement requested for this installation.
    pub configured_enabled: bool,
    /// Exact durable installation receipt identity.
    pub installation_id: String,
    /// ISO 8601 wall-clock installation time.
    pub installed_at: String,
    /// Skill invocation name.
    pub name: String,
    /// Operation that installed this Skill.
    pub operation_id: String,
    /// Ownership state observed from files and receipts.
    pub ownership_state: SkillInstallationOwnershipState,
    /// Bound-session load observation.
    pub session_state: SkillInstallationSessionState,
    /// Exact retained verified source identity.
    pub source: SkillInstallationSource,
    /// User-facing installation location without an absolute host path.
    pub target: SkillInstallationLocation,
}
crate::record_serde::wire_record!(SkillInstallationSummary {
    catalogue: "catalogue",
    configured_enabled: "configuredEnabled",
    installation_id: "installationId",
    installed_at: "installedAt",
    name: "name",
    operation_id: "operationId",
    ownership_state: "ownershipState",
    session_state: "sessionState",
    source: "source",
    target: "target"
});

/// Review for uninstalling an owned verified Skill.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationReviewUninstall {
    /// Exact reviewed Skill installation action.
    pub action: SkillInstallationReviewUninstallAction,
    /// Catalogue identity retained at install time.
    pub catalogue: InstallationCatalogueIdentity,
    /// Files recorded by the installation receipt.
    pub files: Vec<SkillInstallationFileReview>,
    /// Whether current files differ from the receipt. Apply refuses drift.
    pub files_modified: bool,
    /// Owned installation being removed.
    pub installation: SkillInstallationSummary,
    /// Total receipt-owned payload size in bytes.
    pub total_bytes: i64,
}
crate::record_serde::wire_record!(SkillInstallationReviewUninstall {
    action: "action",
    catalogue: "catalogue",
    files: "files",
    files_modified: "filesModified",
    installation: "installation",
    total_bytes: "totalBytes"
});

/// A computed Skill install plan. Nothing has been applied.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallPlan {
    /// Original wall-clock expiry as an ISO 8601 timestamp.
    pub expires_at: String,
    /// Original operation identifier returned before confirmation.
    pub operation_id: String,
    /// One-use plan handle, bound to the original candidate authority.
    pub plan_handle: String,
    /// Safe review to present before applying the plan.
    pub review: SkillInstallationReview,
}

/// One reviewed plugin file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationFileReview {
    /// SHA-256 digest of the exact file bytes.
    pub digest: String,
    /// Whether the file is installed with executable permissions.
    pub executable: bool,
    /// Declared media type for the file.
    pub media_type: String,
    /// Relative file path within the plugin root.
    pub path: String,
    /// Exact reviewed file size in bytes.
    pub size_bytes: i64,
}
crate::record_serde::wire_record!(PluginInstallationFileReview {
    digest: "digest",
    executable: "executable",
    media_type: "mediaType",
    path: "path",
    size_bytes: "sizeBytes"
});

/// One MCP server a reviewed plugin declares.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationMcpServerReview {
    /// Host of a remote server's URL.
    pub host: Option<String>,
    /// Server name as the plugin declares it.
    pub name: String,
    /// `remote` for an HTTP or SSE server, `local` for a command the plugin runs.
    pub transport: PluginInstallationMcpTransport,
}
crate::record_serde::wire_record!(PluginInstallationMcpServerReview { host: "host" if is_none, name: "name", transport: "transport" });

/// Source identity retained from Agent Finder and the pinned GitHub descriptor.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationSource {
    /// Digest of the descriptor's bundle manifest.
    pub bundle_digest: String,
    /// Agent Finder materialisation revision identifier.
    pub catalog_revision_id: String,
    /// Digest of the canonical materialisation descriptor.
    pub descriptor_digest: String,
    /// Repository full name, for example owner/name.
    pub repository: String,
    /// GitHub repository database identifier.
    pub repository_id: String,
    /// Agent Finder resource identifier.
    pub resource_id: String,
    /// Pinned Git commit revision.
    pub revision: String,
    /// Plugin root within the pinned repository. Empty for the repository root.
    pub root: String,
}
crate::record_serde::wire_record!(PluginInstallationSource {
    bundle_digest: "bundleDigest",
    catalog_revision_id: "catalogRevisionId",
    descriptor_digest: "descriptorDigest",
    repository: "repository",
    repository_id: "repositoryId",
    resource_id: "resourceId",
    revision: "revision",
    root: "root"
});

/// Where a plugin is installed, without an absolute host path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationLocation {
    /// Safe display label, for example ~/.copilot/installed-plugins/_direct/owner--repo--plugins-work-hub.
    pub display_label: String,
    /// Path relative to the Copilot home, under installed-plugins/_direct like any direct plugin install.
    pub relative_path: String,
}
crate::record_serde::wire_record!(PluginInstallationLocation {
    display_label: "displayLabel",
    relative_path: "relativePath"
});

/// Review for installing a verified Agent Plugin.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallPlanReview {
    /// Exact reviewed Agent Plugin installation action.
    pub action: PluginInstallPlanReviewAction,
    /// Catalogue identity retained from the bound candidate, or verified data for a reference.
    pub catalogue: InstallationCatalogueIdentity,
    /// Manifest description when present.
    pub description: Option<String>,
    /// Reviewed files and digests.
    pub files: Vec<PluginInstallationFileReview>,
    /// Installing never grants immediate use; the plugin is registered disabled.
    pub installs_disabled: bool,
    /// Complete verified manifest content. Planning refuses with review-too-large
    /// when this exceeds 262144 UTF-8 bytes; it is never truncated.
    pub manifest_content: String,
    /// Relative path of the verified plugin manifest.
    pub manifest_path: String,
    /// MCP servers the verified files declare, exactly as an install of these files loads
    /// them. Omitted when the plugin declares none.
    pub mcp_servers: Option<Vec<PluginInstallationMcpServerReview>>,
    /// Installed plugin name, derived from the verified manifest exactly as a direct install derives it.
    pub name: String,
    /// Exact verified source identity.
    pub source: PluginInstallationSource,
    /// Exact install location without an absolute host path.
    pub target: PluginInstallationLocation,
    /// Total reviewed payload size in bytes.
    pub total_bytes: i64,
    /// Manifest version when present.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(PluginInstallPlanReview { action: "action", catalogue: "catalogue", description: "description" if is_none, files: "files", installs_disabled: "installsDisabled", manifest_content: "manifestContent", manifest_path: "manifestPath", mcp_servers: "mcpServers" if is_none, name: "name", source: "source", target: "target", total_bytes: "totalBytes", version: "version" if is_none });

/// A computed Agent Plugin install plan. Nothing has been applied.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallPlan {
    /// Original wall-clock expiry as an ISO 8601 timestamp.
    pub expires_at: String,
    /// Original operation identifier returned before confirmation.
    pub operation_id: String,
    /// One-use plan handle, bound to the original authority.
    pub plan_handle: String,
    /// Safe review to present before applying the plan.
    pub review: PluginInstallPlanReview,
}
crate::record_serde::wire_record!(PluginInstallPlan {
    expires_at: "expiresAt",
    operation_id: "operationId",
    plan_handle: "planHandle",
    review: "review"
});

/// Retrieval was refused by the runtime's hardened fetch boundary before any request left the process, or before a redirect was followed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogUnsafeRetrievalError {
    /// Discriminator: retrieval was refused as unsafe
    pub kind: CatalogUnsafeRetrievalErrorKind,
    /// Human-readable explanation, safe to surface. Never contains the refused URL, nor a query, handle, or secret.
    pub message: String,
    /// Which control refused the retrieval, low cardinality so it can be aggregated without carrying a URL.
    pub reason: CatalogUnsafeRetrievalReason,
}
crate::record_serde::wire_record!(CatalogUnsafeRetrievalError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// The operation is not available on this runtime. Distinct from a network failure: nothing was attempted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogUnavailableError {
    /// Discriminator: the operation is not available
    pub kind: CatalogUnavailableErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Why the operation is unavailable.
    pub reason: CatalogUnavailableReason,
}
crate::record_serde::wire_record!(CatalogUnavailableError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// Authority-reported navigation metadata, returned only to callers requiring catalog-search-pagination and only when a supported token is present. Tokenless first-page and continuation responses omit this object; no counts are inferred from candidates. The opaque token may be retained for previous or numbered navigation even when hasNextPage is false.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSearchPagination {
    /// One-based page returned by the authority.
    pub current_page: i32,
    /// Whether the authority token advertises a valid next target within the navigation window. Not inferred from token presence, truncated, or currentPage being less than pageCount.
    pub has_next_page: bool,
    /// Navigation window ceiling ceil(1000 / pageSize), not the number of existing pages. Legal targets must not exceed this ceiling or the token's signed pageCount.
    pub max_page: i32,
    /// Backend-reported page count, which may exceed maxPage. Present pagination metadata always describes a multi-page result; zero- and single-page responses omit pagination. Navigation targets must also be within the signed pageCount carried by the supplied token.
    pub page_count: i64,
    /// Page size bound to the search, equal to the effective request limit.
    pub page_size: i32,
    /// Opaque pagination token. Session-bound search returns an expiring runtime-owned reference retaining the exact private authority token, original search and authority. Legacy unbound search returns the authority token unchanged, without a runtime-created expiry. Only the runtime unwraps tokens or changes targetPage; SDK consumers must not decode, modify or log them.
    pub token: String,
    /// Backend-reported count for this response, not the number of returned candidates. Its relationship to the full query result set is unknown.
    pub total_count: i64,
    /// The relationship of totalCount to the complete query result set is unknown; neither exactness nor a lower-bound guarantee is implied.
    pub total_count_relation: CatalogSearchTotalCountRelation,
}
crate::record_serde::wire_record!(CatalogSearchPagination {
    current_page: "currentPage",
    has_next_page: "hasNextPage",
    max_page: "maxPage",
    page_count: "pageCount",
    page_size: "pageSize",
    token: "token",
    total_count: "totalCount",
    total_count_relation: "totalCountRelation"
});

/// A literal choice the command input accepts, with a human-facing description
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandInputChoice {
    /// Human-readable description shown alongside the choice
    pub description: String,
    /// The literal choice value (e.g. 'on', 'off', 'show')
    pub name: String,
}
crate::record_serde::wire_record!(SlashCommandInputChoice {
    description: "description",
    name: "name"
});

/// Optional unstructured input hint
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandInput {
    /// Optional literal choices the input accepts, each with a human-facing description; clients may render these as selectable options
    pub choices: Option<Vec<SlashCommandInputChoice>>,
    /// Optional completion hint for the input (e.g. 'directory' for filesystem path completion)
    pub completion: Option<SlashCommandInputCompletion>,
    /// Hint to display when command input has not been provided
    pub hint: String,
    /// When true, clients should pass the full text after the command name as a single argument rather than splitting on whitespace
    pub preserve_multiline_input: Option<bool>,
    /// When true, the command requires non-empty input; clients should render the input hint as required
    pub required: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandInput { choices: "choices" if is_none, completion: "completion" if is_none, hint: "hint", preserve_multiline_input: "preserveMultilineInput" if is_none, required: "required" if is_none });

/// Slash-command metadata with name, aliases, description, kind, input hint, execution allowance, and schedulability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandInfo {
    /// Canonical aliases without leading slashes
    pub aliases: Option<Vec<String>>,
    /// Whether the command may run while an agent turn is active
    pub allow_during_agent_execution: bool,
    /// Human-readable command description
    pub description: String,
    /// Whether the command is experimental
    pub experimental: Option<bool>,
    /// Optional unstructured input hint
    pub input: Option<SlashCommandInput>,
    /// Coarse command category for grouping and behavior: runtime built-in, skill-backed command, or SDK/client-owned command
    pub kind: SlashCommandKind,
    /// Canonical command name without a leading slash
    pub name: String,
    /// Whether the command may be the target of `/every` / `/after` schedules. Resolution happens at every tick, so only set this when the command is safe to re-invoke and produces an agent prompt.
    pub schedulable: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandInfo { aliases: "aliases" if is_none, allow_during_agent_execution: "allowDuringAgentExecution", description: "description", experimental: "experimental" if is_none, input: "input" if is_none, kind: "kind", name: "name", schedulable: "schedulable" if is_none });

/// A single host-driven completion. Accepting an item replaces `[rangeStart, rangeEnd)` (UTF-16 code units) in the composer with `insertText`; when the range is absent, the active token around the cursor is replaced.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCompletionItem {
    /// Text spliced into the composer when the item is accepted.
    pub insert_text: String,
    /// Render-kind hint for the picker row (e.g. `"document"`, `"directory"`), derived from the host's display kind.
    pub kind: Option<String>,
    /// Primary display label for the picker row. Falls back to `insertText` when absent.
    pub label: Option<String>,
    /// End (exclusive) of the replacement range in `text`, in UTF-16 code units.
    pub range_end: Option<i64>,
    /// Start of the replacement range in `text`, in UTF-16 code units.
    pub range_start: Option<i64>,
}
crate::record_serde::wire_record!(SessionCompletionItem { insert_text: "insertText", kind: "kind" if is_none, label: "label" if is_none, range_end: "rangeEnd" if is_none, range_start: "rangeStart" if is_none });

/// Repository associated with the connected remote session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectedRemoteSessionMetadataRepository {
    /// Branch associated with the remote session.
    pub branch: String,
    /// Repository name.
    pub name: String,
    /// Repository owner or organization login.
    pub owner: String,
}
crate::record_serde::wire_record!(ConnectedRemoteSessionMetadataRepository {
    branch: "branch",
    name: "name",
    owner: "owner"
});

/// Metadata for a connected remote session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectedRemoteSessionMetadata {
    /// Neutral SDK discriminator for the connected remote session kind.
    pub kind: ConnectedRemoteSessionMetadataKind,
    /// Last session update time as an ISO 8601 string.
    pub modified_time: String,
    /// Optional friendly session name.
    pub name: Option<String>,
    /// Pull request number associated with the session.
    pub pull_request_number: Option<i64>,
    /// Repository associated with the connected remote session.
    pub repository: ConnectedRemoteSessionMetadataRepository,
    /// Original remote resource identifier.
    pub resource_id: Option<String>,
    /// SDK session ID for the connected remote session.
    pub session_id: SessionId,
    /// Remote session staleness deadline as an ISO 8601 string.
    pub stale_at: Option<String>,
    /// Session start time as an ISO 8601 string.
    pub start_time: String,
    /// Remote session state returned by the backing service.
    pub state: Option<String>,
    /// Optional session summary.
    pub summary: Option<String>,
}
crate::record_serde::wire_record!(ConnectedRemoteSessionMetadata { kind: "kind", modified_time: "modifiedTime", name: "name" if is_none, pull_request_number: "pullRequestNumber" if is_none, repository: "repository", resource_id: "resourceId" if is_none, session_id: "sessionId", stale_at: "staleAt" if is_none, start_time: "startTime", state: "state" if is_none, summary: "summary" if is_none });

/// Account-targeted authorization update required by the Connector service. The account ID is an opaque host routing identifier; no credential is included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorAuthorizationRequirement {
    /// Exact opaque account selection that made the Connector request.
    pub account_id: String,
    /// Stable OAuth scope the selected account must grant.
    pub scope: ConnectorAuthorizationScope,
}
crate::record_serde::wire_record!(ConnectorAuthorizationRequirement {
    account_id: "accountId",
    scope: "scope"
});

/// Credential-free Connector catalog entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorCatalogEntry {
    /// Untrusted service description, when present.
    pub description: Option<String>,
    /// Untrusted display label from the service.
    pub display_name: String,
    /// Optional catalog logo.
    pub logo: Option<String>,
    /// Canonical Connector name used by lifecycle methods.
    pub name: String,
    /// Optional catalog release tag.
    pub release_tag: Option<String>,
    /// Opaque stable runtime IDs currently projected into the session for this Connector.
    pub runtime_server_ids: Vec<String>,
    /// Current authoritative service connection state.
    pub status: ConnectorCatalogStatus,
    /// Optional catalog tier.
    pub tier: Option<String>,
}
crate::record_serde::wire_record!(ConnectorCatalogEntry { description: "description" if is_none, display_name: "displayName", logo: "logo" if is_none, name: "name", release_tag: "releaseTag" if is_none, runtime_server_ids: "runtimeServerIds", status: "status", tier: "tier" if is_none });

/// Validated Connector catalog snapshot cached by the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorCatalogResult {
    /// Validated catalog entries in service order.
    pub connectors: Vec<ConnectorCatalogEntry>,
    /// Unix epoch milliseconds when this snapshot was accepted.
    pub refreshed_at_ms: i64,
    /// Monotonically increasing session-local catalog revision.
    pub revision: i64,
}
crate::record_serde::wire_record!(ConnectorCatalogResult {
    connectors: "connectors",
    refreshed_at_ms: "refreshedAtMs",
    revision: "revision"
});

/// Live status of one session-owned MCP projection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorRuntimeStatus {
    /// Canonical Connector name that owns this server.
    pub connector_name: String,
    /// Opaque runtime server ID.
    pub runtime_server_id: String,
    /// Current live MCP host status.
    pub status: ConnectorMcpStatus,
}
crate::record_serde::wire_record!(ConnectorRuntimeStatus {
    connector_name: "connectorName",
    runtime_server_id: "runtimeServerId",
    status: "status"
});

/// Authoritative session connector state. Account IDs are opaque routing identifiers and credentials are never included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorStatus {
    /// Opaque account selection pinned to this session, when one has been selected.
    pub account_id: Option<String>,
    /// Connector API contract version.
    pub api_version: i64,
    /// Exact selected account and stable scope requiring an authorization update, when proven by the Connector service.
    pub authorization_requirement: Option<ConnectorAuthorizationRequirement>,
    /// Current feature and session availability.
    pub availability: ConnectorAvailability,
    /// Latest validated catalog snapshot, when available.
    pub catalog: Option<ConnectorCatalogResult>,
    /// Number of active opaque connection continuations.
    pub pending_connections: i64,
    /// Live MCP status for every Connector-owned runtime server.
    pub runtime_servers: Vec<ConnectorRuntimeStatus>,
}
crate::record_serde::wire_record!(ConnectorStatus { account_id: "accountId" if is_none, api_version: "apiVersion", authorization_requirement: "authorizationRequirement" if is_none, availability: "availability", catalog: "catalog" if is_none, pending_connections: "pendingConnections", runtime_servers: "runtimeServers" });

/// Session account selection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorSessionAccount {
    /// Opaque session-scoped account selection ID.
    pub account_id: String,
    /// Credential-free identity metadata.
    pub auth_info: AuthIdentityMetadata,
}
crate::record_serde::wire_record!(ConnectorSessionAccount {
    account_id: "accountId",
    auth_info: "authInfo"
});

/// Content-exclusion decision for one requested path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContentExclusionPathCheck {
    /// Whether the session's complete content-exclusion policy excludes the path.
    pub excluded: bool,
    /// The path supplied by the caller.
    pub path: String,
}
crate::record_serde::wire_record!(ContentExclusionPathCheck {
    excluded: "excluded",
    path: "path"
});

/// A single large message currently in context.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContextHeaviestMessage {
    /// Stable identifier for this message within the snapshot.
    pub id: String,
    /// Human-readable source label, e.g. `tool: bash` or `skill: tmux`. Presentation-only.
    pub label: String,
    /// Role of the chat message (`user`, `assistant`, or `tool`).
    pub role: String,
    /// Token count currently in context for this individual message.
    pub tokens: i64,
}
crate::record_serde::wire_record!(ContextHeaviestMessage {
    id: "id",
    label: "label",
    role: "role",
    tokens: "tokens"
});

/// The session's authoritative model snapshot. Auto preference fields are configuration for the virtual `auto` model and do not change the selected model identifier. The context tier reflects `Session.getContextTier()`, restored from the session journal on resume.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CurrentModel {
    /// Auto preference currently claimed by an in-progress activation. Null means the activation is returning to provider-default routing.
    pub activating_auto_tier: Option<AutoTier>,
    /// Auto preference currently committed for the session. This can remain available while another model is selected so a later switch to `auto` can reuse it.
    pub auto_tier: Option<AutoTier>,
    /// Context tier for models that support multiple context-window sizes.
    pub context_tier: Option<ContextTier>,
    /// Copilot Fusion tier that the committed autoTier serves, resolved from the session's Auto tier metadata. An open string: known values include `standard`, `high`, and `max`; other provider-defined values may appear. Omitted when autoTier is unset, is not a Fusion tier, or Auto metadata has not been discovered (for example after cold resume, before session.model.list). This snapshot reports only the committed Fusion tier; resolve pendingAutoTier and activatingAutoTier against auto.tiers[].fusionTier from session.model.list.
    pub fusion_tier: Option<String>,
    /// Currently active model identifier
    pub model_id: Option<String>,
    /// Latest unclaimed Auto preference waiting for a future user turn. Null means the pending request is returning to provider-default routing.
    pub pending_auto_tier: Option<AutoTier>,
    /// Captured base model to restore when leaving plan mode. Omitted outside plan mode or when no plan override has captured a base model. Persistent agent model requirements apply to this model rather than the temporary plan model.
    pub plan_base_model_id: Option<String>,
    /// Provider of planBaseModelId, when that saved selection is provider-qualified.
    pub plan_base_provider_id: Option<String>,
    /// Provider selected for this model. Together with modelId, identifies a catalog entry even when providers serve the same model. Omitted for unattributed legacy selections.
    pub provider_id: Option<String>,
    /// Reasoning effort level currently applied to the active model, when one is set. Reads `Session.getReasoningEffort()` synchronously after `getSelectedModel()` resolves so the two values are reported as a snapshot.
    pub reasoning_effort: Option<String>,
}
crate::record_serde::wire_record!(CurrentModel { activating_auto_tier: "activatingAutoTier" if is_none, auto_tier: "autoTier" if is_none, context_tier: "contextTier" if is_none, fusion_tier: "fusionTier" if is_none, model_id: "modelId" if is_none, pending_auto_tier: "pendingAutoTier" if is_none, plan_base_model_id: "planBaseModelId" if is_none, plan_base_provider_id: "planBaseProviderId" if is_none, provider_id: "providerId" if is_none, reasoning_effort: "reasoningEffort" if is_none });

/// A file included in the session debug bundle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsCollectedEntry {
    /// Relative path of the file in the staged bundle/archive.
    pub bundle_path: String,
    /// Redacted output size in bytes.
    pub size_bytes: i64,
    /// Source category for this entry.
    pub source: DebugCollectLogsSource,
}
crate::record_serde::wire_record!(DebugCollectLogsCollectedEntry {
    bundle_path: "bundlePath",
    size_bytes: "sizeBytes",
    source: "source"
});

/// An optional debug bundle entry that could not be included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsSkippedEntry {
    /// Relative path requested for this bundle entry.
    pub bundle_path: String,
    /// Server-local source path that could not be read.
    pub path: Option<String>,
    /// Reason the entry was skipped.
    pub reason: String,
}
crate::record_serde::wire_record!(DebugCollectLogsSkippedEntry { bundle_path: "bundlePath", path: "path" if is_none, reason: "reason" });

/// MCP-specific detail for a source-discriminated diagnostic entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiagnosticDetails {
    /// Fresh identifier for the MCP connection attempt, including failed starts.
    pub connection_id: String,
    /// Serialized diagnostic detail. Protocol and HTTP records use JSON when detail is present.
    pub data: Option<String>,
    /// Protocol-frame direction when kind is protocol.
    pub direction: Option<McpDiagnosticDirection>,
    /// Diagnostic record category.
    pub kind: McpDiagnosticKind,
    /// Configured MCP server name.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpDiagnosticDetails { connection_id: "connectionId", data: "data" if is_none, direction: "direction" if is_none, kind: "kind", server_name: "serverName" });

/// MCP diagnostic source configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiagnosticSourceConfiguration {
    /// Threshold to apply to MCP diagnostic producers in this session.
    pub level: DiagnosticLogLevel,
}
crate::record_serde::wire_record!(McpDiagnosticSourceConfiguration { level: "level" });

/// Typed diagnostic source configuration. At least one source is required by diagnostics configuration methods.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiagnosticSourcesConfiguration {
    /// MCP diagnostic capture threshold. Omit to leave the current threshold unchanged.
    pub mcp: Option<McpDiagnosticSourceConfiguration>,
}
crate::record_serde::wire_record!(DiagnosticSourcesConfiguration { mcp: "mcp" if is_none });

/// Installed plugin that contributes a discovered extension.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredExtensionPlugin {
    /// Opaque identity of the owning plugin. Matches plugins.list().plugins[].id on the same runtime host. Compare the complete value without parsing it. May be absent on older runtimes.
    pub id: Option<String>,
    /// Installed plugin name
    pub name: String,
}
crate::record_serde::wire_record!(DiscoveredExtensionPlugin { id: "id" if is_none, name: "name" });

/// Discovered extension metadata and persistent enablement state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredExtension {
    /// Whether this extension's persistent per-ID preference is enabled
    pub enabled: bool,
    /// Source-qualified ID accepted by both server and session extension enablement methods
    pub id: String,
    /// Human-readable extension name
    pub name: String,
    /// Absolute path to the extension entry module, suitable for revealing it in a file manager
    pub path: String,
    /// Containing plugin metadata for plugin-contributed extensions
    pub plugin: Option<DiscoveredExtensionPlugin>,
    /// Discovery source
    pub source: DiscoveredExtensionSource,
}
crate::record_serde::wire_record!(DiscoveredExtension { enabled: "enabled", id: "id", name: "name", path: "path", plugin: "plugin" if is_none, source: "source" });

/// Vision-specific limits
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesLimitsVision {
    /// Maximum image size in bytes
    pub max_prompt_image_size: i64,
    /// Maximum number of images per prompt
    pub max_prompt_images: i64,
    /// MIME types the model accepts
    pub supported_media_types: Vec<String>,
}
crate::record_serde::wire_record!(ModelCapabilitiesLimitsVision {
    max_prompt_image_size: "max_prompt_image_size",
    max_prompt_images: "max_prompt_images",
    supported_media_types: "supported_media_types"
});

/// Token limits for prompts, outputs, and context window
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesLimits {
    /// Maximum total context window size in tokens
    pub max_context_window_tokens: Option<i64>,
    /// Maximum number of output/completion tokens
    pub max_output_tokens: Option<i64>,
    /// Maximum number of prompt/input tokens
    pub max_prompt_tokens: Option<i64>,
    /// Vision-specific limits
    pub vision: Option<ModelCapabilitiesLimitsVision>,
}
crate::record_serde::wire_record!(ModelCapabilitiesLimits { max_context_window_tokens: "max_context_window_tokens" if is_none, max_output_tokens: "max_output_tokens" if is_none, max_prompt_tokens: "max_prompt_tokens" if is_none, vision: "vision" if is_none });

/// Feature flags indicating what the model supports
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesSupports {
    /// Resolved Anthropic adaptive-thinking capability — unsupported / optional / required / adaptive_only. 'required' models reject thinking.type='enabled' with HTTP 400 but still accept 'disabled' (e.g. opus-4.7/4.8/5, sonnet-5); 'adaptive_only' models accept nothing but 'adaptive' (e.g. fable, mythos).
    pub adaptive_thinking: Option<AdaptiveThinkingSupport>,
    /// Whether this model supports reasoning effort configuration
    pub reasoning_effort: Option<bool>,
    /// Whether the model supports provider-native thinking. Independent of configurable reasoning effort; omission means unknown.
    pub thinking: Option<bool>,
    /// Whether this model supports canonical tool calling
    pub tool_calls: Option<bool>,
    /// Whether this model supports vision/image input
    pub vision: Option<bool>,
}
crate::record_serde::wire_record!(ModelCapabilitiesSupports { adaptive_thinking: "adaptive_thinking" if is_none, reasoning_effort: "reasoningEffort" if is_none, thinking: "thinking" if is_none, tool_calls: "toolCalls" if is_none, vision: "vision" if is_none });

/// Model capabilities and limits
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilities {
    /// Token limits for prompts, outputs, and context window
    pub limits: Option<ModelCapabilitiesLimits>,
    /// Feature flags indicating what the model supports
    pub supports: Option<ModelCapabilitiesSupports>,
}
crate::record_serde::wire_record!(ModelCapabilities { limits: "limits" if is_none, supports: "supports" if is_none });

/// Provider-reported model artifact metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelArtifactDetails {
    /// Provider-reported model architecture.
    pub architecture: Option<String>,
    /// Provider-reported model families.
    pub families: Option<Vec<String>>,
    /// Primary model family.
    pub family: Option<String>,
    /// Artifact format, such as `gguf`.
    pub format: Option<String>,
    /// Provider-reported parameter count label.
    pub parameter_size: Option<String>,
    /// Provider-reported quantization label.
    pub quantization: Option<String>,
    /// Provider-reported tokenizer.
    pub tokenizer: Option<String>,
}
crate::record_serde::wire_record!(ModelArtifactDetails { architecture: "architecture" if is_none, families: "families" if is_none, family: "family" if is_none, format: "format" if is_none, parameter_size: "parameterSize" if is_none, quantization: "quantization" if is_none, tokenizer: "tokenizer" if is_none });

/// Attribution for the adapter that produced a provider row.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderProvenance {
    /// Stable opaque adapter identity from the effective catalog. Treat this as a whole identifier, not a parseable owner or kind.
    pub adapter_id: String,
    /// Human-readable contributor name, not the adapter display name.
    pub owner_display_name: Option<String>,
    /// Stable contributor identifier when the adapter has an owner outside the runtime. Independent of the contribution mechanism and not a routing key.
    pub owner_id: Option<String>,
    /// Descriptive provider family that produced this row; not a routing key.
    pub provider_kind: String,
    /// Kind of component that supplied the adapter.
    pub source: ModelProviderProvenanceSource,
}
crate::record_serde::wire_record!(ModelProviderProvenance { adapter_id: "adapterId", owner_display_name: "ownerDisplayName" if is_none, owner_id: "ownerId" if is_none, provider_kind: "providerKind", source: "source" });

/// A non-fatal provider observation warning.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderWarning {
    /// Machine-readable warning code.
    pub code: String,
    /// Human-readable warning message.
    pub message: String,
}
crate::record_serde::wire_record!(ModelProviderWarning {
    code: "code",
    message: "message"
});

/// A model offered for agent conversations. Missing capability metadata does not disqualify a candidate. Models known to be incompatible, such as embedding-only models, are excluded by the adapter.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredModel {
    /// Provider-reported model capabilities. Omitted capability fields are unknown; explicit false values are preserved.
    pub capabilities: ModelCapabilities,
    /// Provider-reported model artifact details.
    pub details: ModelArtifactDetails,
    /// Provider-reported artifact digest.
    pub digest: Option<String>,
    /// Provider-native model identifier.
    pub id: String,
    /// Provider-reported last-modified timestamp.
    pub modified_at: Option<String>,
    /// Provider-reported display name.
    pub name: Option<String>,
    /// Attribution for the adapter that produced this model row.
    pub provenance: ModelProviderProvenance,
    /// Provider-reported artifact size in bytes.
    pub size_bytes: Option<i64>,
    /// Non-fatal warnings encountered while enriching this model.
    pub warnings: Vec<ModelProviderWarning>,
}
crate::record_serde::wire_record!(DiscoveredModel { capabilities: "capabilities", details: "details", digest: "digest" if is_none, id: "id", modified_at: "modifiedAt" if is_none, name: "name" if is_none, provenance: "provenance", size_bytes: "sizeBytes" if is_none, warnings: "warnings" });

/// Typed provider-operation outcome. Use the code for control flow and the optional message for display.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderOperationOutcome {
    /// Machine-readable operation outcome.
    pub code: ModelProviderOperationOutcomeCode,
    /// Human-readable detail for non-success outcomes.
    pub message: Option<String>,
}
crate::record_serde::wire_record!(ModelProviderOperationOutcome { code: "code", message: "message" if is_none });

/// An executable extension entry with runtime-verified plugin provenance.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EligiblePluginExtensionEntry {
    /// Runtime-discovered extension identifier.
    pub id: String,
    /// Absolute executable module path discovered under an eligible plugin root.
    pub module_path: String,
}
crate::record_serde::wire_record!(EligiblePluginExtensionEntry {
    id: "id",
    module_path: "modulePath"
});

/// Discovered extension metadata, including source-qualified ID, name, discovery source, status, and optional process ID.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Extension {
    /// Source-qualified ID (e.g., 'project:my-ext', 'user:auth-helper', 'plugin:my-plugin:my-ext')
    pub id: String,
    /// Extension name (directory name)
    pub name: String,
    /// Process ID if the extension is running
    pub pid: Option<i64>,
    /// Discovery source: project (.github/extensions/), user (~/.copilot/extensions/), plugin (installed plugin), or session (session-state/`<id>`/extensions/)
    pub source: ExtensionSource,
    /// Current status: running, dormant, disabled, failed, or starting
    pub status: ExtensionStatus,
}
crate::record_serde::wire_record!(Extension { id: "id", name: "name", pid: "pid" if is_none, source: "source", status: "status" });

/// Binary result returned by a tool for the model
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmBinaryResultsForLlm {
    /// Base64-encoded binary data
    pub data: String,
    /// Human-readable description of the binary data
    pub description: Option<String>,
    /// Optional metadata from the producing tool.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// MIME type of the binary data
    pub mime_type: String,
    /// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
    pub r#type: ExternalToolTextResultForLlmBinaryResultsForLlmType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmBinaryResultsForLlm { data: "data", description: "description" if is_none, metadata: "metadata" if is_none, mime_type: "mimeType", r#type: "type" });

/// An explicit replacement or clear of the current assignments.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagAssignmentsUpdate {
    /// The original assignment response; null clears delivered assignments.
    pub response: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(FeatureFlagAssignmentsUpdate {
    response: "response"
});

/// A patch to retained user settings, without a disk write.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagConfigPatch {
    /// Top-level keys to remove after the merge.
    pub undefined_keys: Vec<String>,
    /// Settings keys to merge. Null has the original empty-patch semantics.
    pub values: serde_json::Value,
}
crate::record_serde::wire_record!(FeatureFlagConfigPatch {
    undefined_keys: "undefinedKeys",
    values: "values"
});

/// One persisted feature preference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagEnabledOverride {
    /// Null removes the preference and restores lower-precedence behavior.
    pub enabled: Option<bool>,
    /// A runtime-defined feature name. Unknown names fail.
    pub name: String,
}
crate::record_serde::wire_record!(FeatureFlagEnabledOverride {
    enabled: "enabled",
    name: "name"
});

/// A boolean experiment value with a static feature fallback.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagExpOverrideQuery {
    /// Experiment key whose boolean value takes precedence.
    pub exp_flag: String,
    /// Static feature name.
    pub flag: String,
}
crate::record_serde::wire_record!(FeatureFlagExpOverrideQuery {
    exp_flag: "expFlag",
    flag: "flag"
});

/// The runtime's interpretation of the reasoning-summary arm.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagReasoningSummariesArm {
    /// Whether the hidden-summary hint is enabled.
    pub hint_enabled: bool,
    /// Whether summaries are hidden by default.
    pub off_by_default: bool,
}
crate::record_serde::wire_record!(FeatureFlagReasoningSummariesArm {
    hint_enabled: "hintEnabled",
    off_by_default: "offByDefault"
});

/// Current service state, without configuration documents or credentials.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagServiceSnapshot {
    /// Whether the service remains active.
    pub active: bool,
    /// Latest nonempty assignment context.
    pub assignment_context: Option<String>,
    /// Organization targeting derived from the authenticated account.
    pub auto_approval_targeted: bool,
    /// Validated instruction-retrieval arm, when present.
    pub dynamic_instructions_retrieval_arm: Option<String>,
    /// Effective experiment values.
    pub exp_flags: HashMap<String, serde_json::Value>,
    /// Whether the current audience assignment has resolved.
    pub exp_response_resolved: bool,
    /// Static flags after availability, environment, settings, and explicit overrides.
    pub flags: HashMap<String, bool>,
    /// Original runtime interpretation of the GPT default-model treatment.
    pub gpt_default_model_enabled: bool,
    /// Original runtime interpretation of the reasoning-summary treatment.
    pub reasoning_summaries_arm: FeatureFlagReasoningSummariesArm,
    /// Original assignment document, including its optional fields.
    pub response: serde_json::Value,
    /// Service revision, with unsigned 32-bit wraparound.
    pub revision: i64,
    /// Latest nonempty secondary assignment context.
    pub secondary_assignment_context: Option<String>,
    /// Decimal instance identity, without integer precision loss.
    pub service_id: String,
}
crate::record_serde::wire_record!(FeatureFlagServiceSnapshot { active: "active", assignment_context: "assignmentContext" if is_none, auto_approval_targeted: "autoApprovalTargeted", dynamic_instructions_retrieval_arm: "dynamicInstructionsRetrievalArm" if is_none, exp_flags: "expFlags", exp_response_resolved: "expResponseResolved", flags: "flags", gpt_default_model_enabled: "gptDefaultModelEnabled", reasoning_summaries_arm: "reasoningSummariesArm", response: "response", revision: "revision", secondary_assignment_context: "secondaryAssignmentContext" if is_none, service_id: "serviceId" });

/// Existing persisted restoration metadata for one proposal file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForgeProposalManifestEntry {
    /// Expected content hash after generation; null means deletion.
    #[serde(rename = "after_hash", skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ForgeProposalManifestEntry::deserialize_present_nullable"
    )]
    pub after_hash: Option<Option<String>>,
    /// Captured content before generation. Absence remains distinct from explicit null.
    #[serde(rename = "before_content", skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ForgeProposalManifestEntry::deserialize_present_nullable"
    )]
    pub before_content: Option<Option<String>>,
    /// Content hash before generation; null means that no file existed.
    #[serde(rename = "before_hash", skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ForgeProposalManifestEntry::deserialize_present_nullable"
    )]
    pub before_hash: Option<Option<String>>,
    /// File-change classification.
    #[serde(rename = "change_type")]
    pub change_type: ForgeProposalChangeType,
    /// Original path of a renamed file.
    #[serde(rename = "old_path", skip_serializing_if = "Option::is_none")]
    pub old_path: Option<String>,
    /// Path in the proposal's working tree.
    pub path: String,
}

impl ForgeProposalManifestEntry {
    fn deserialize_present_nullable<'de, D, T>(
        deserializer: D,
    ) -> Result<Option<Option<T>>, D::Error>
    where
        D: serde::Deserializer<'de>,
        T: Deserialize<'de>,
    {
        Option::<T>::deserialize(deserializer).map(Some)
    }
}

/// Existing persisted counts for a proposal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalSummary {
    /// Added files.
    pub added_count: i64,
    /// Deleted files.
    pub deleted_count: i64,
    /// Total changed files.
    pub file_count: i64,
    /// Modified files.
    pub modified_count: i64,
    /// Renamed files.
    pub renamed_count: i64,
}
crate::record_serde::wire_record!(ForgeProposalSummary {
    added_count: "added_count",
    deleted_count: "deleted_count",
    file_count: "file_count",
    modified_count: "modified_count",
    renamed_count: "renamed_count"
});

/// Canonical Forge proposal with the unchanged persisted field names.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalRecord {
    /// Proposal branch.
    pub branch_name: String,
    /// Creation time in ISO 8601 format.
    pub created_at: String,
    /// Existing failure classification.
    pub failure_reason: Option<String>,
    /// Content-addressed proposal identity.
    pub fingerprint: Option<String>,
    /// Working tree root in persisted path format.
    pub git_root_path: String,
    /// Proposal identifier assigned at generation admission.
    pub id: String,
    /// Ordered restoration metadata.
    pub manifest: Vec<ForgeProposalManifestEntry>,
    /// Repository name when known.
    pub repo_name: Option<String>,
    /// Repository owner when known.
    pub repo_owner: Option<String>,
    /// Existing persisted proposal state.
    pub status: ForgeProposalStatus,
    /// File-change counts when generation produced them.
    pub summary: Option<ForgeProposalSummary>,
    /// Replacement proposal identifier.
    pub superseded_by: Option<String>,
    /// Original generation trigger.
    pub trigger_mode: ForgeProposalTriggerMode,
    /// Last transition time in ISO 8601 format.
    pub updated_at: String,
}
crate::record_serde::wire_record!(ForgeProposalRecord { branch_name: "branch_name", created_at: "created_at", failure_reason: "failure_reason" if is_none, fingerprint: "fingerprint" if is_none, git_root_path: "git_root_path", id: "id", manifest: "manifest", repo_name: "repo_name" if is_none, repo_owner: "repo_owner" if is_none, status: "status", summary: "summary" if is_none, superseded_by: "superseded_by" if is_none, trigger_mode: "trigger_mode", updated_at: "updated_at" });

/// Options for the built-in GitHub MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GitHubMcpOptions {
    /// Additional tools to expose.
    pub additional_tools: Option<Vec<String>>,
    /// Additional toolsets to expose.
    pub additional_toolsets: Option<Vec<String>>,
    /// Integration attribution for requests.
    pub copilot_integration_id: Option<String>,
    /// Disable deferred form loading.
    pub disable_form_deferral: Option<bool>,
    /// Expose all supported tools.
    pub enable_all_tools: Option<bool>,
    /// Enable FIDES tools.
    pub enable_fides_ifc: Option<bool>,
    /// Enable insiders mode.
    pub enable_insiders_mode: Option<bool>,
    /// Enable MCP app responses.
    pub enable_mcp_apps: Option<bool>,
    /// Exclude tools that the host's gh CLI can replace.
    pub exclude_gh_replaceable_tools: Option<bool>,
}
crate::record_serde::wire_record!(GitHubMcpOptions { additional_tools: "additionalTools" if is_none, additional_toolsets: "additionalToolsets" if is_none, copilot_integration_id: "copilotIntegrationId" if is_none, disable_form_deferral: "disableFormDeferral" if is_none, enable_all_tools: "enableAllTools" if is_none, enable_fides_ifc: "enableFidesIfc" if is_none, enable_insiders_mode: "enableInsidersMode" if is_none, enable_mcp_apps: "enableMcpApps" if is_none, exclude_gh_replaceable_tools: "excludeGhReplaceableTools" if is_none });

/// Installed plugin record from global state, with marketplace, version, install time, enabled state, cache path, and source.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPlugin {
    /// Path where the plugin is cached locally
    pub cache_path: Option<String>,
    /// Whether the plugin is currently enabled
    pub enabled: bool,
    /// Installation timestamp
    pub installed_at: String,
    /// Absolute path of the marketplace directory a live plugin was resolved from. Present only on live, never-persisted records — those synthesized at session start for a directory/local marketplace, whose cache_path points at the real plugin directory on disk rather than a copy under the installed-plugins cache. Its presence is what marks a record as live, and no record carrying it is ever written to the persisted installedPlugins key.
    pub installed_from: Option<String>,
    /// Installation-time marketplace or direct source, independent of mutable marketplace aliases. Local paths are absolute. Absent for older installs; reinstall to record provenance.
    pub installed_source: Option<serde_json::Value>,
    /// Marketplace the plugin came from (empty string for direct repo installs)
    pub marketplace: String,
    /// Plugin name
    pub name: String,
    /// Source for direct repo installs (when marketplace is empty)
    pub source: Option<serde_json::Value>,
    /// Per-plugin source fingerprint (a SHA-256 hash of the plugin's catalog source spec plus its resolved source subtree — NOT a Git commit SHA) captured at marketplace install/update time. Auto-update compares it against the freshly recomputed fingerprint to detect a content change that does not bump the version. Absent for pre-existing installs and for direct (non-marketplace) installs.
    pub source_sha: Option<String>,
    /// Version installed (if available)
    pub version: Option<String>,
}
crate::record_serde::wire_record!(InstalledPlugin { cache_path: "cache_path" if is_none, enabled: "enabled", installed_at: "installed_at", installed_from: "installed_from" if is_none, installed_source: "installed_source" if is_none, marketplace: "marketplace", name: "name", source: "source" if is_none, source_sha: "source_sha" if is_none, version: "version" if is_none });

/// Post-compaction context window usage breakdown
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryCompactContextWindow {
    /// Token count from non-system messages (user, assistant, tool)
    pub conversation_tokens: Option<i64>,
    /// Current total tokens in the context window (system + conversation + tool definitions)
    pub current_tokens: i64,
    /// Current number of messages in the conversation
    pub messages_length: i64,
    /// Token count from system message(s)
    pub system_tokens: Option<i64>,
    /// Maximum token count for the model's context window
    pub token_limit: i64,
    /// Token count from tool definitions
    pub tool_definitions_tokens: Option<i64>,
}
crate::record_serde::wire_record!(HistoryCompactContextWindow { conversation_tokens: "conversationTokens" if is_none, current_tokens: "currentTokens", messages_length: "messagesLength", system_tokens: "systemTokens" if is_none, token_limit: "tokenLimit", tool_definitions_tokens: "toolDefinitionsTokens" if is_none });

/// A root user turn that the session can rewind to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryRewindPoint {
    /// Whether at least one file in this turn or a later turn can be restored.
    pub can_restore_files: bool,
    /// ID of the user.message event that begins the discarded suffix.
    pub event_id: String,
    /// Number of unique files in this turn and all later turns that have captured changes.
    pub file_count: i64,
    /// Whether this turn was an automatically injected autopilot continuation.
    pub is_autopilot_continuation: bool,
    /// Lines added by this turn's captured file changes.
    pub lines_added: i64,
    /// Lines removed by this turn's captured file changes.
    pub lines_removed: i64,
    /// ISO timestamp of the user turn.
    pub timestamp: String,
    /// Whether this turn itself captured any file changes.
    pub turn_changed_files: bool,
    /// User-visible message text for the turn.
    pub user_message: String,
}
crate::record_serde::wire_record!(HistoryRewindPoint {
    can_restore_files: "canRestoreFiles",
    event_id: "eventId",
    file_count: "fileCount",
    is_autopilot_continuation: "isAutopilotContinuation",
    lines_added: "linesAdded",
    lines_removed: "linesRemoved",
    timestamp: "timestamp",
    turn_changed_files: "turnChangedFiles",
    user_message: "userMessage"
});

/// A file that a conversation-and-files rewind would restore.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryRewindFilePreview {
    /// Aggregate change made across the discarded turns.
    pub change_type: HistoryRewindChangeType,
    /// Lines added across the discarded turns.
    pub lines_added: i64,
    /// Lines removed across the discarded turns.
    pub lines_removed: i64,
    /// Absolute path of the captured file.
    pub path: String,
}
crate::record_serde::wire_record!(HistoryRewindFilePreview {
    change_type: "changeType",
    lines_added: "linesAdded",
    lines_removed: "linesRemoved",
    path: "path"
});

/// A captured file that rewind intentionally left unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistorySkippedFileRestore {
    /// Absolute path of the skipped file.
    pub path: String,
    /// Reason the file was not restored.
    pub reason: HistoryFileRestoreSkipReason,
}
crate::record_serde::wire_record!(HistorySkippedFileRestore {
    path: "path",
    reason: "reason"
});

/// An optional plugin default from managed settings. Explicit user choices override this default.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ManagedPluginAssignment {
    /// Administrator-provided explanation from the same winning policy entry as the state.
    pub reason: Option<String>,
    /// Assignment state. The runtime currently supports defaultEnabled.
    pub state: String,
}
crate::record_serde::wire_record!(ManagedPluginAssignment { reason: "reason" if is_none, state: "state" });

/// The GitHub repository and commit an installed plugin is pinned to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPluginVerifiedSource {
    /// GitHub repository in `owner/repo` form.
    pub repository: String,
    /// Full 40-character hexadecimal commit SHA the plugin is pinned to.
    pub revision: String,
}
crate::record_serde::wire_record!(InstalledPluginVerifiedSource {
    repository: "repository",
    revision: "revision"
});

/// Information about an installed plugin tracked in global state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPluginInfo {
    /// Soft assignment from effective managed settings. This is not a mandatory enabled-state restriction.
    pub assignment: Option<ManagedPluginAssignment>,
    /// Manifest description. Present for a direct install pinned to one GitHub commit, or for bundled and external plugins in invocation queries.
    pub description: Option<String>,
    /// Opaque, stable hash identifying a direct (non-marketplace) install source. Present only for direct repo / URL / local installs; absent for marketplace plugins. Same source yields the same id; distinct sources never collide.
    pub direct_source_id: Option<String>,
    /// Whether the plugin is currently enabled for new sessions
    pub enabled: bool,
    /// HTTPS URL of the plugin's icon on raw.githubusercontent.com, retained from the catalogue entry it was installed from. Untrusted external content: load it without credentials. Present only for a direct install pinned to one GitHub commit whose catalogue entry had an icon.
    pub icon_url: Option<String>,
    /// Opaque plugin identity in this runtime's inventory. Matches MCP effectiveSource.plugin.id for the same plugin. Stable across enablement changes, updates and restarts while the installation source is unchanged; not a cross-host identifier or a mutation argument. Compare the complete value without parsing it. May be absent on older runtimes.
    pub id: Option<String>,
    /// Whether the managed desired plugin currently has an installed or live record. Set to false for a managed desired entry retained in the listing after installation or reconciliation failed.
    pub installed: Option<bool>,
    /// Absolute path of the marketplace directory a live plugin was resolved from. Present only on live, never-persisted records — a plugin belonging to a directory/local marketplace, which is loaded from its real directory on every pass instead of a copy under the installed-plugins cache. Its presence is what marks a listed plugin as live: such a plugin is always present on disk, so `enabled` is its only meaningful state and it is never "not installed".
    pub installed_from: Option<String>,
    /// Whether enterprise managed settings control this plugin's enabled state.
    pub managed: Option<bool>,
    /// The enabled state required by enterprise managed settings, when this plugin spec is managed.
    pub managed_desired_enabled: Option<bool>,
    /// Marketplace the plugin came from. Empty string ("") for direct repo / URL / local installs.
    pub marketplace: String,
    /// Plugin name
    pub name: String,
    /// Absolute local directory containing this plugin's installed content, including disabled installations. Absent when the runtime has no resolved content directory. This is the plugin root, not its marketplace directory; clients must not forward it back as an additional plugin.
    pub root_path: Option<String>,
    /// Runtime-reported plugin provenance. Set to "builtin" for trusted host bundles, or "external" for invocation plugin directories. Absent for installed, marketplace, direct, and live plugins.
    pub source: Option<String>,
    /// True when the signed-in account's GitHub Team assigns this plugin, whatever default enabled state the assignment carries. This reports provenance only: it does not mean the Team installed the plugin or controls whether it is enabled, and the user's own choices still apply. When `managed` is also true, enterprise managed settings decide the plugin's state.
    pub team: Option<bool>,
    /// The GitHub repository and commit a direct install is pinned to, from its install record. Present only for a direct install pinned to one GitHub commit, such as a verified install from GitHub; absent for marketplace, local and unpinned installs.
    pub verified_source: Option<InstalledPluginVerifiedSource>,
    /// Installed version (when reported by the plugin manifest)
    pub version: Option<String>,
}
crate::record_serde::wire_record!(InstalledPluginInfo { assignment: "assignment" if is_none, description: "description" if is_none, direct_source_id: "directSourceId" if is_none, enabled: "enabled", icon_url: "iconUrl" if is_none, id: "id" if is_none, installed: "installed" if is_none, installed_from: "installedFrom" if is_none, managed: "managed" if is_none, managed_desired_enabled: "managedDesiredEnabled" if is_none, marketplace: "marketplace", name: "name", root_path: "rootPath" if is_none, source: "source" if is_none, team: "team" if is_none, verified_source: "verifiedSource" if is_none, version: "version" if is_none });

/// Loaded instruction source for a session, including path, content, category, location, applicability, and optional description.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionSource {
    /// Glob pattern(s) from frontmatter — when set, this instruction applies only to matching files
    pub apply_to: Option<Vec<String>>,
    /// Raw content of the instruction file
    pub content: String,
    /// When true, this source starts disabled and must be toggled on by the user
    pub default_disabled: Option<bool>,
    /// Short description (body after frontmatter) for use in instruction tables
    pub description: Option<String>,
    /// Unique identifier for this source (used for toggling)
    pub id: String,
    /// Human-readable label
    pub label: String,
    /// Where this source lives — used for UI grouping
    pub location: InstructionSourceLocation,
    /// Opaque identity of the plugin owning this instruction source. Matches plugins.list().plugins[].id on the same runtime host. Compare the complete value without parsing it. Omitted for non-plugin sources.
    pub plugin_id: Option<String>,
    /// The project path this source was discovered from. Only set by sessionless discovery for repository, working-directory, and project-scoped plugin sources, where it disambiguates sources across multiple workspace roots. The session-scoped getSources leaves it unset.
    pub project_path: Option<String>,
    /// File path relative to repo or absolute for home
    pub source_path: String,
    /// Category of instruction source — used for merge logic
    pub r#type: InstructionSourceType,
}
crate::record_serde::wire_record!(InstructionSource { apply_to: "applyTo" if is_none, content: "content", default_disabled: "defaultDisabled" if is_none, description: "description" if is_none, id: "id", label: "label", location: "location", plugin_id: "pluginId" if is_none, project_path: "projectPath" if is_none, source_path: "sourcePath", r#type: "type" });

/// A project configuration that the tolerant loader could not use.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspConfigurationProblem {
    /// Configuration read or validation failure.
    pub error: String,
    /// Configuration file path.
    pub path: String,
}
crate::record_serde::wire_record!(LspConfigurationProblem {
    error: "error",
    path: "path"
});

/// One language server in the runtime's configuration inventory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspServerSummary {
    /// Whether the effective configuration disables this entry.
    pub disabled: bool,
    /// File suffixes handled by the server.
    pub file_extensions: HashMap<String, String>,
    /// Configured server identifier.
    pub id: String,
    /// Protocol initialization deadline in milliseconds.
    pub initialization_timeout_ms: Option<f64>,
    /// Platform-selected launch script for a user entry.
    pub launch_description: Option<String>,
    /// Request deadline in milliseconds.
    pub request_timeout_ms: Option<f64>,
    /// Project root relative to the repository root.
    pub root_uri: Option<String>,
    /// Plugin that supplies the server, when applicable.
    pub source_plugin: Option<String>,
    /// Process startup deadline in milliseconds.
    pub spawn_timeout_ms: Option<f64>,
    /// Warmup deadline in milliseconds.
    pub warmup_timeout_ms: Option<f64>,
}
crate::record_serde::wire_record!(LspServerSummary { disabled: "disabled", file_extensions: "fileExtensions", id: "id", initialization_timeout_ms: "initializationTimeoutMs" if is_none, launch_description: "launchDescription" if is_none, request_timeout_ms: "requestTimeoutMs" if is_none, root_uri: "rootUri" if is_none, source_plugin: "sourcePlugin" if is_none, spawn_timeout_ms: "spawnTimeoutMs" if is_none, warmup_timeout_ms: "warmupTimeoutMs" if is_none });

/// Process exit information from a startup probe.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspProbeExitStatus {
    /// Exit code, or null when the process ended through a signal.
    pub code: Option<i64>,
    /// Failure to observe the child process.
    pub error: Option<String>,
    /// Termination signal, or null for a normal exit.
    pub signal: Option<String>,
}
crate::record_serde::wire_record!(LspProbeExitStatus { code: "code", error: "error" if is_none, signal: "signal" });

/// A server whose warmup failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspWarmupError {
    /// Server startup failure.
    pub error: String,
    /// Configured server identifier.
    pub server_id: String,
}
crate::record_serde::wire_record!(LspWarmupError {
    error: "error",
    server_id: "serverId"
});

/// Effective enterprise managed settings and contributing channels. Session events report applied policy; sessionless resolve reports an account/device snapshot, and compose reports a non-applying preview of candidate documents. Device values take precedence over server values, then the policy helper, per ordinary key, while permissions compose restrictively. Session-local SDK-client policy is included only in session results. Marked experimental while the managed-settings surface stabilizes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ManagedSettingsResolvedData {
    /// Whether an explicit enterprise policy restriction disables bypass-permissions ("yolo") mode for this session. Deny-wins across layers; an unresolved policy does not force this on by itself.
    pub bypass_permissions_disabled: bool,
    /// Whether a session-local permissions layer injected by the SDK host was present
    pub client_managed: Option<bool>,
    /// Whether an actual device MDM/plist/registry/file managed-settings layer was present
    pub device_managed: bool,
    /// Whether managed policy could not be determined (e.g. a failed server fetch) and unresolved-policy safeguards remain active. This does not by itself disable bypass-permissions; `bypassPermissionsDisabled` reports only an explicit policy restriction.
    pub fail_closed: bool,
    /// The setting keys under enterprise management in the effective managed settings (e.g. `model`, `enabledPlugins`, `permissions`). Empty when no managed settings are in force.
    pub managed_keys: Vec<String>,
    /// Whether at least two managed sources supplied permission allowlists, so enforcement intersects them and the flattened settings payload omits `permissions.allow`.
    pub permissions_allow_intersected: Option<bool>,
    /// Reusable retained managed permission policy for managedSettings.permissions.evaluate, including source composition and session-local injection. Provided by live session snapshots; absent in older events. Refresh the context when managed policy changes.
    pub permissions_context: Option<ManagedPermissionsContext>,
    /// Explicit Policy Helper structured-failure and recheck capabilities supported by this runtime.
    pub policy_helper_capabilities: Option<PolicyHelperCapabilities>,
    /// The latest unresolved Policy Helper failure for this exact policy context. It remains present across retained-policy refresh failures and clears only after fresh success or authoritative removal.
    pub policy_helper_failure: Option<PolicyHelperFailure>,
    /// Whether the policy-helper managed-settings layer was present. The policy helper is the weakest channel: it fills keys no enterprise source set and can never replace one.
    pub policy_helper_managed: Option<bool>,
    /// Whether the effective sandbox policy forces the sandbox on *only* because managed policy could not be determined, rather than because the policy requires it. Lets clients tell a user whose `--no-sandbox` was overridden that the sandbox stayed on as a fail-closed fallback, instead of attributing it to an administrator who set no such policy.
    pub sandbox_enabled_by_undetermined_policy: Option<bool>,
    /// Whether the server (account/org) managed-settings layer was present
    pub server_managed: bool,
    /// The effective (resolved) managed settings values, so clients can render exactly what is enforced. Absent when no managed policy is in force.
    pub settings: Option<serde_json::Value>,
    /// Channel summary: `server`, `device`, `client`, or `policyHelper` when exactly one channel contributed; `mixed` when multiple channels contributed; otherwise `none`. Consult the per-channel booleans for exact provenance.
    pub source: ManagedSettingsResolvedSource,
}
crate::record_serde::wire_record!(ManagedSettingsResolvedData { bypass_permissions_disabled: "bypassPermissionsDisabled", client_managed: "clientManaged" if is_none, device_managed: "deviceManaged", fail_closed: "failClosed", managed_keys: "managedKeys", permissions_allow_intersected: "permissionsAllowIntersected" if is_none, permissions_context: "permissionsContext" if is_none, policy_helper_capabilities: "policyHelperCapabilities" if is_none, policy_helper_failure: "policyHelperFailure" if is_none, policy_helper_managed: "policyHelperManaged" if is_none, sandbox_enabled_by_undetermined_policy: "sandboxEnabledByUndeterminedPolicy" if is_none, server_managed: "serverManaged", settings: "settings" if is_none, source: "source" });

/// Plugin entry advertised by a marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplacePluginInfo {
    /// Soft assignment from effective managed settings for this marketplace entry.
    pub assignment: Option<ManagedPluginAssignment>,
    /// Short description from the marketplace catalog, when present
    pub description: Option<String>,
    /// The enabled state required by enterprise managed settings, when this plugin spec is managed. False means the plugin cannot be installed or enabled. Absent when managed settings do not control this plugin.
    pub managed_desired_enabled: Option<bool>,
    /// Plugin name as listed in the marketplace catalog
    pub name: String,
}
crate::record_serde::wire_record!(MarketplacePluginInfo { assignment: "assignment" if is_none, description: "description" if is_none, managed_desired_enabled: "managedDesiredEnabled" if is_none, name: "name" });

/// Registered marketplace summary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceInfo {
    /// Whether the managed marketplace currently resolved into the runtime marketplace registry. Set to false when the desired managed entry is retained for governance visibility after loading or reconciliation failed.
    pub available: Option<bool>,
    /// True when this is a default marketplace shipped with the runtime. Defaults are not removable.
    pub is_default: Option<bool>,
    /// Whether enterprise managed settings provide and control this marketplace entry.
    pub managed: Option<bool>,
    /// Marketplace name (matches the @marketplace suffix in plugin specs)
    pub name: String,
    /// Human-readable description of where the marketplace data is fetched from (e.g. "GitHub: owner/repo").
    pub source: String,
    /// True when the signed-in account's GitHub Team provides this marketplace and no user, repository, or default entry of the same name exists. Team marketplaces are not persisted, so they cannot be removed.
    pub team: Option<bool>,
}
crate::record_serde::wire_record!(MarketplaceInfo { available: "available" if is_none, is_default: "isDefault" if is_none, managed: "managed" if is_none, name: "name", source: "source", team: "team" if is_none });

/// Per-marketplace refresh result, including marketplace name, success flag, and optional failure error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceRefreshEntry {
    /// Error message (failure only)
    pub error: Option<String>,
    /// Marketplace name that was refreshed
    pub name: String,
    /// Whether the refresh succeeded
    pub success: bool,
}
crate::record_serde::wire_record!(MarketplaceRefreshEntry { error: "error" if is_none, name: "name", success: "success" });

/// MCP server allowed by policy, with server name and optional PII-free explanatory note.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAllowedServer {
    /// Allowed server name
    pub name: String,
    /// PII-free note explaining why the server was allowed
    pub redacted_note: Option<String>,
}
crate::record_serde::wire_record!(McpAllowedServer { name: "name", redacted_note: "redactedNote" if is_none });

/// Capability negotiation snapshot
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsDiagnoseCapability {
    /// Whether the runtime advertises `extensions.io.modelcontextprotocol/ui` to MCP servers
    pub advertised: bool,
    /// Whether the MCP_APPS feature flag (or COPILOT_MCP_APPS env override) is on
    pub feature_flag_enabled: bool,
    /// Whether the session has the `mcp-apps` capability
    pub session_has_mcp_apps: bool,
}
crate::record_serde::wire_record!(McpAppsDiagnoseCapability {
    advertised: "advertised",
    feature_flag_enabled: "featureFlagEnabled",
    session_has_mcp_apps: "sessionHasMcpApps"
});

/// What the server returned for this session
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsDiagnoseServer {
    /// Whether the named server is currently connected
    pub connected: bool,
    /// Up to 5 tool names with `_meta.ui` for quick inspection
    pub sample_tool_names: Vec<String>,
    /// Total tools returned by the server's tools/list
    pub tool_count: f64,
    /// Tools whose `_meta.ui` is populated (resourceUri and/or visibility set)
    pub tools_with_ui_meta: f64,
}
crate::record_serde::wire_record!(McpAppsDiagnoseServer {
    connected: "connected",
    sample_tool_names: "sampleToolNames",
    tool_count: "toolCount",
    tools_with_ui_meta: "toolsWithUiMeta"
});

/// Current host context
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsHostContextDetails {
    /// Display modes the host supports
    pub available_display_modes: Option<Vec<McpAppsHostContextDetailsAvailableDisplayMode>>,
    /// Current display mode (SEP-1865)
    pub display_mode: Option<McpAppsHostContextDetailsDisplayMode>,
    /// BCP-47 locale, e.g. 'en-US'
    pub locale: Option<String>,
    /// Platform type for responsive design
    pub platform: Option<McpAppsHostContextDetailsPlatform>,
    /// UI theme preference per SEP-1865
    pub theme: Option<McpAppsHostContextDetailsTheme>,
    /// IANA timezone, e.g. 'America/New_York'
    pub time_zone: Option<String>,
    /// Host application identifier
    pub user_agent: Option<String>,
}
crate::record_serde::wire_record!(McpAppsHostContextDetails { available_display_modes: "availableDisplayModes" if is_none, display_mode: "displayMode" if is_none, locale: "locale" if is_none, platform: "platform" if is_none, theme: "theme" if is_none, time_zone: "timeZone" if is_none, user_agent: "userAgent" if is_none });

/// MCP Apps resource content with URI, optional MIME type, text or base64 blob, and resource metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsResourceContent {
    /// Resource-level metadata (CSP, permissions, etc.)
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Base64-encoded binary content
    pub blob: Option<String>,
    /// MIME type of the content
    pub mime_type: Option<String>,
    /// Text content (e.g. HTML)
    pub text: Option<String>,
    /// The resource URI (typically ui://...)
    pub uri: String,
}
crate::record_serde::wire_record!(McpAppsResourceContent { meta: "_meta" if is_none, blob: "blob" if is_none, mime_type: "mimeType" if is_none, text: "text" if is_none, uri: "uri" });

/// Observational state for a matching already materialized MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfiguredServerState {
    /// Observed connection error, when the materialized server failed.
    pub error: Option<String>,
    /// Observed connection status. This is not a configuration or readiness guarantee.
    pub status: McpServerStatus,
    /// Whether the server's current connection has loaded its tool list (from tools/list or a cached snapshot). A connected server without loaded tools is still loading them; the change is signalled by session.mcp_server_tools_loaded. Absent when the runtime does not report it.
    pub tools_loaded: Option<bool>,
}
crate::record_serde::wire_record!(McpConfiguredServerState { error: "error" if is_none, status: "status", tools_loaded: "toolsLoaded" if is_none });

/// Effective MCP configuration entry. Configuration enablement is distinct from the optional live observation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfiguredServer {
    /// Human-readable display name supplied by configuration.
    pub display_name: Option<String>,
    /// Whether this configured server is enabled after session configuration and policy filtering.
    pub enabled: bool,
    /// Observed state from an already materialized matching server. Omitted when no live graph has this configured server; it never determines configuration enablement.
    pub live: McpConfiguredServerState,
    /// Server name (config key)
    pub name: String,
    /// Configuration provenance: user, workspace, plugin, builtin, or managed.
    pub source: Option<McpServerSource>,
    /// Plugin name that provided this server, when source is plugin.
    pub source_plugin: Option<String>,
    /// Plugin version that provided this server, when source is plugin.
    pub source_plugin_version: Option<String>,
}
crate::record_serde::wire_record!(McpConfiguredServer { display_name: "displayName" if is_none, enabled: "enabled", live: "live", name: "name", source: "source" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_version: "sourcePluginVersion" if is_none });

/// MCP server whose connection attempt failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpFailedServer {
    /// The captured connection failure detail.
    pub error: Option<String>,
    /// The config key of the server that failed to connect.
    pub name: String,
}
crate::record_serde::wire_record!(McpFailedServer { error: "error" if is_none, name: "name" });

/// MCP server filtered by policy, with name, reason, and optional redacted reason.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpFilteredServer {
    /// Filtered server name
    pub name: String,
    /// Human-readable filter reason
    pub reason: String,
    /// PII-free filter reason
    pub redacted_reason: Option<String>,
}
crate::record_serde::wire_record!(McpFilteredServer { name: "name", reason: "reason", redacted_reason: "redactedReason" if is_none });

/// Recorded MCP server connection failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerFailureInfo {
    /// Failure message produced when the MCP server connection failed.
    pub message: String,
    /// epoch-ms timestamp at which the failure was recorded.
    pub timestamp: i64,
}
crate::record_serde::wire_record!(McpServerFailureInfo {
    message: "message",
    timestamp: "timestamp"
});

/// Recorded MCP server pending-auth state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerNeedsAuthInfo {
    /// epoch-ms timestamp at which the server signalled it needs authentication.
    pub timestamp: i64,
}
crate::record_serde::wire_record!(McpServerNeedsAuthInfo {
    timestamp: "timestamp"
});

/// Host-level state, omitted when no MCP host is initialized.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpHostState {
    /// Names of currently-connected MCP clients.
    pub clients: Vec<String>,
    /// Configured servers that are explicitly disabled.
    pub disabled_servers: Vec<String>,
    /// Map of server name to recorded connection failure.
    pub failed_servers: HashMap<String, McpServerFailureInfo>,
    /// Configured servers filtered out by MCP server policy.
    pub filtered_servers: Vec<String>,
    /// Whether third-party MCP servers are policy-enabled for this session.
    pub mcp3p_enabled: bool,
    /// Map of server name to recorded pending-auth state.
    pub needs_auth_servers: HashMap<String, McpServerNeedsAuthInfo>,
    /// Names of servers with in-flight connection attempts.
    pub pending_connections: Vec<String>,
}
crate::record_serde::wire_record!(McpHostState {
    clients: "clients",
    disabled_servers: "disabledServers",
    failed_servers: "failedServers",
    filtered_servers: "filteredServers",
    mcp3p_enabled: "mcp3pEnabled",
    needs_auth_servers: "needsAuthServers",
    pending_connections: "pendingConnections"
});

/// Normalized MCP Apps discovery metadata from a tool's `_meta.ui` block.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpToolUi {
    /// URI of the tool's MCP App resource, typically a `ui://` resource identifier. Use `session.mcp.resources.read` to fetch its HTML and resource metadata.
    pub resource_uri: Option<String>,
    /// Tool visibility advertised by the server. When absent, MCP Apps defaults apply.
    pub visibility: Option<Vec<McpToolUiVisibility>>,
}
crate::record_serde::wire_record!(McpToolUi { resource_uri: "resourceUri" if is_none, visibility: "visibility" if is_none });

/// MCP tool metadata with tool name, optional description, and normalized MCP Apps discovery metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpTools {
    /// Tool description, when provided.
    pub description: Option<String>,
    /// Tool name.
    pub name: String,
    /// Normalized MCP Apps discovery metadata. An empty object indicates that a valid `_meta.ui` block was present without recognized fields.
    pub ui: Option<McpToolUi>,
}
crate::record_serde::wire_record!(McpTools { description: "description" if is_none, name: "name", ui: "ui" if is_none });

/// A pre-registered OAuth client for this attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthFlowStaticClientConfig {
    /// Pre-registered client identifier.
    pub client_id: String,
    /// Ephemeral secret that the runtime never persists.
    pub client_secret: Option<String>,
    /// OAuth grant override.
    pub grant_type: Option<McpOauthLoginGrantType>,
    /// Whether this client has no secret. Defaults to true.
    pub public_client: Option<bool>,
    /// Scope used when the server omits its challenge scope.
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(McpOauthFlowStaticClientConfig { client_id: "clientId", client_secret: "clientSecret" if is_none, grant_type: "grantType" if is_none, public_client: "publicClient" if is_none, scope: "scope" if is_none });

/// Use the current session authorizer's store.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreSession {
    pub kind: McpOauthFlowStoreSessionKind,
}
crate::record_serde::wire_record!(McpOauthFlowStoreSession { kind: "kind" });

/// Retain an isolated in-memory store until disposal or requester detachment.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreMemory {
    pub kind: McpOauthFlowStoreMemoryKind,
    /// Identity chosen by the host, scoped to this session requester.
    pub store_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStoreMemory {
    kind: "kind",
    store_id: "storeId"
});

/// Use the host's configured profile directory.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStorePersistent {
    /// Profile directory containing the OAuth credential store.
    pub config_dir: String,
    pub kind: McpOauthFlowStorePersistentKind,
    /// Identity chosen by the host, scoped to this session requester.
    pub store_id: String,
    /// Original store's effective token session. An empty value selects unscoped storage.
    pub token_session_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStorePersistent {
    config_dir: "configDir",
    kind: "kind",
    store_id: "storeId",
    token_session_id: "tokenSessionId"
});

/// Call the original requester's OAuth store host effects.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreCallback {
    pub kind: McpOauthFlowStoreCallbackKind,
    /// Host-owned callback store identity.
    pub store_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStoreCallback {
    kind: "kind",
    store_id: "storeId"
});

/// An argument accepted by an MCP prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptArgument {
    /// Argument-level metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard argument fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Description of the argument
    pub description: Option<String>,
    /// Name of the argument
    pub name: String,
    /// Whether the argument is required; omission is distinct from false
    pub required: Option<bool>,
}
crate::record_serde::wire_record!(McpPromptArgument { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, description: "description" if is_none, name: "name", required: "required" if is_none });

/// An MCP prompt icon with standard size hints and preserved non-standard fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptIcon {
    /// Server-provided non-standard icon fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Icon MIME type, when known
    pub mime_type: Option<String>,
    /// Icon sizes, such as `48x48` or `any`
    pub sizes: Option<Vec<String>>,
    /// Icon URI
    pub src: String,
    /// Theme hint for this icon
    pub theme: Option<String>,
}
crate::record_serde::wire_record!(McpPromptIcon { additional_properties: "additionalProperties" if is_none, mime_type: "mimeType" if is_none, sizes: "sizes" if is_none, src: "src", theme: "theme" if is_none });

/// An MCP prompt descriptor. Server-provided non-standard fields are exposed under `additionalProperties`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPrompt {
    /// Prompt-level metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard descriptor fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Arguments accepted by the prompt
    pub arguments: Option<Vec<McpPromptArgument>>,
    /// Description of what this prompt provides
    pub description: Option<String>,
    /// Icons associated with this prompt
    pub icons: Option<Vec<McpPromptIcon>>,
    /// The programmatic name of the prompt
    pub name: String,
    /// Human-readable display title
    pub title: Option<String>,
}
crate::record_serde::wire_record!(McpPrompt { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, arguments: "arguments" if is_none, description: "description" if is_none, icons: "icons" if is_none, name: "name", title: "title" if is_none });

/// An MCP prompt message with opaque JSON content preserved without flattening or content-type filtering.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptMessage {
    /// Message-level metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard message fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// The original MCP content block, including nested metadata and unfamiliar content types
    pub content: serde_json::Value,
    /// The role of the message sender
    pub role: McpPromptRole,
}
crate::record_serde::wire_record!(McpPromptMessage { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, content: "content", role: "role" });

/// Standard MCP resource annotations plus preserved non-standard annotation fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourceAnnotations {
    /// Server-provided non-standard annotation fields preserved from the MCP response
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Intended audience roles for this resource
    pub audience: Option<Vec<String>>,
    /// Last-modified timestamp hint
    pub last_modified: Option<String>,
    /// Priority hint for model/client use
    pub priority: Option<f64>,
}
crate::record_serde::wire_record!(McpResourceAnnotations { additional_properties: "additionalProperties" if is_none, audience: "audience" if is_none, last_modified: "lastModified" if is_none, priority: "priority" if is_none });

/// A resource icon descriptor plus preserved non-standard icon fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourceIcon {
    /// Server-provided non-standard icon fields preserved from the MCP response
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Icon MIME type, when known
    pub mime_type: Option<String>,
    /// Icon sizes hint
    pub sizes: Option<String>,
    /// Icon URI
    pub src: String,
    /// Theme hint for this icon
    pub theme: Option<String>,
}
crate::record_serde::wire_record!(McpResourceIcon { additional_properties: "additionalProperties" if is_none, mime_type: "mimeType" if is_none, sizes: "sizes" if is_none, src: "src", theme: "theme" if is_none });

/// An MCP resource descriptor (spec `Resource`): URI, name, and optional title, description, MIME type, size, icons, annotations, and metadata. Server-provided fields outside the standard descriptor shape are exposed under `additionalProperties`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResource {
    /// Resource-level metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard descriptor fields preserved from the MCP response
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Model/client annotations associated with this resource
    pub annotations: Option<McpResourceAnnotations>,
    /// Optional description of what this resource represents
    pub description: Option<String>,
    /// Icons associated with this resource
    pub icons: Option<Vec<McpResourceIcon>>,
    /// MIME type of the resource, if known
    pub mime_type: Option<String>,
    /// The programmatic name of the resource
    pub name: String,
    /// Resource size in bytes, when known
    pub size: Option<i64>,
    /// Optional human-readable display title
    pub title: Option<String>,
    /// The resource URI (e.g. ui://... or file:///...)
    pub uri: String,
}
crate::record_serde::wire_record!(McpResource { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, annotations: "annotations" if is_none, description: "description" if is_none, icons: "icons" if is_none, mime_type: "mimeType" if is_none, name: "name", size: "size" if is_none, title: "title" if is_none, uri: "uri" });

/// MCP resource content with URI, optional MIME type, text or base64 blob, and resource metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourceContent {
    /// Resource-level metadata (CSP, permissions, etc.)
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Base64-encoded binary content
    pub blob: Option<String>,
    /// MIME type of the content
    pub mime_type: Option<String>,
    /// Text content (e.g. HTML)
    pub text: Option<String>,
    /// The resource URI
    pub uri: String,
}
crate::record_serde::wire_record!(McpResourceContent { meta: "_meta" if is_none, blob: "blob" if is_none, mime_type: "mimeType" if is_none, text: "text" if is_none, uri: "uri" });

/// An MCP resource template descriptor (spec `ResourceTemplate`): an RFC 6570 URI template, name, and optional title, description, MIME type, icons, annotations, and metadata. Server-provided fields outside the standard descriptor shape are exposed under `additionalProperties`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourceTemplate {
    /// Resource-template-level metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard descriptor fields preserved from the MCP response
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Model/client annotations associated with this template
    pub annotations: Option<McpResourceAnnotations>,
    /// Optional description of what this template is for
    pub description: Option<String>,
    /// Icons associated with resources matching this template
    pub icons: Option<Vec<McpResourceIcon>>,
    /// MIME type for resources matching this template, if uniform
    pub mime_type: Option<String>,
    /// The programmatic name of the resource template
    pub name: String,
    /// Optional human-readable display title
    pub title: Option<String>,
    /// An RFC 6570 URI template for constructing resource URIs
    pub uri_template: String,
}
crate::record_serde::wire_record!(McpResourceTemplate { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, annotations: "annotations" if is_none, description: "description" if is_none, icons: "icons" if is_none, mime_type: "mimeType" if is_none, name: "name", title: "title" if is_none, uri_template: "uriTemplate" });

/// Owned installation that a listed MCP server's live configuration came from.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerOwnership {
    /// Stable installation identifier from the owned installation receipt.
    pub installation_id: String,
}
crate::record_serde::wire_record!(McpServerOwnership {
    installation_id: "installationId"
});

/// MCP server status entry, including config source/plugin source and any connection error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServer {
    /// Human-readable display name supplied by a managed server catalog.
    pub display_name: Option<String>,
    /// Error message if the server failed to connect
    pub error: Option<String>,
    /// Server name (config key)
    pub name: String,
    /// Owned installation this entry's live configuration came from. Absent for manual, workspace, plugin, builtin and same-name servers, and on runtimes without owned installations.
    pub owned: Option<McpServerOwnership>,
    /// Server-advertised metadata for a connected server. Omitted when no live connection metadata is available, including while pending or when failed, disabled, stopped, or not configured.
    pub server_metadata: Option<McpServerMetadata>,
    /// Configuration source: user, workspace, plugin, builtin, or managed
    pub source: Option<McpServerSource>,
    /// Plugin name that provided this server, when source is plugin.
    pub source_plugin: Option<String>,
    /// Plugin version that provided this server, when source is plugin.
    pub source_plugin_version: Option<String>,
    /// Connection status: connected, failed, needs-auth, pending, disabled, stopped, or not_configured
    pub status: McpServerStatus,
    /// Configured URL for an HTTP/SSE server, regardless of configuration source. Omitted for local and in-memory servers.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(McpServer { display_name: "displayName" if is_none, error: "error" if is_none, name: "name", owned: "owned" if is_none, server_metadata: "serverMetadata" if is_none, source: "source" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_version: "sourcePluginVersion" if is_none, status: "status", url: "url" if is_none });

/// Context tokens one MCP tool definition costs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpToolTokenCount {
    /// True when the tool definition is deferred and not sent to the model up front.
    pub deferred: bool,
    /// Tool name as the MCP server reports it.
    pub mcp_tool_name: String,
    /// Tool name as the model sees it, copied verbatim from the session's tool metadata. Usually namespaced with the server, but that is not guaranteed: use `serverName` rather than parsing this name.
    pub name: String,
    /// Share of the server's total tokens attributed to this tool; 0 when deferred. The tools of one server sum to that server's `totalTokens`.
    pub tokens: i64,
}
crate::record_serde::wire_record!(McpToolTokenCount {
    deferred: "deferred",
    mcp_tool_name: "mcpToolName",
    name: "name",
    tokens: "tokens"
});

/// Context tokens one MCP server's tool definitions cost.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerToolTokenBreakdown {
    /// Server name the tools are namespaced under.
    pub server_name: String,
    /// Per-tool breakdown, in the session's tool order.
    pub tools: Vec<McpToolTokenCount>,
    /// Tokens the server's non-deferred tool definitions cost together for the model the breakdown was counted with.
    pub total_tokens: i64,
}
crate::record_serde::wire_record!(McpServerToolTokenBreakdown {
    server_name: "serverName",
    tools: "tools",
    total_tokens: "totalTokens"
});

/// Active server-driven promotion for a model, including its discount and optional expiry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelBillingPromo {
    /// Percentage discount (0-100) applied while the promotion is active. May be fractional.
    pub discount_percent: Option<f64>,
    /// UTC ISO 8601 timestamp marking when the promotion ends. Optional: an open-ended promotion omits this field. When present, the API only surfaces a promo whose expiry parses and is in the future, so consumers should treat a past value as expired.
    pub ends_at: Option<String>,
    /// Stable identifier for the promotion campaign.
    pub id: Option<String>,
    /// Human-readable promotion message. Does not include the expiry timestamp; consumers may format endsAt and append it when present.
    pub message: Option<String>,
    /// Whether the service asked hosts to give this promotion a prominent surface, such as a dedicated banner, in addition to listing it with the model. `true` requests that surface and `false` asks for the model list only. Absent means the service expressed no preference — for example a response that predates the field — so hosts should apply their own default rather than read it as `false`.
    pub show_banner: Option<bool>,
}
crate::record_serde::wire_record!(ModelBillingPromo { discount_percent: "discountPercent" if is_none, ends_at: "endsAt" if is_none, id: "id" if is_none, message: "message" if is_none, show_banner: "showBanner" if is_none });

/// Long context tier pricing (available for models with extended context windows)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelBillingTokenPricesLongContext {
    /// AI Credits cost per billing batch of cached (read) tokens
    pub cache_read_price: Option<f64>,
    /// AI Credits cost per billing batch of 1-hour cache-write (cache creation) tokens.
    pub cache_write1h_price: Option<f64>,
    /// AI Credits cost per billing batch of cache-write (cache creation) tokens.
    pub cache_write_price: Option<f64>,
    /// AI Credits cost per billing batch of input tokens
    pub input_price: Option<f64>,
    /// Prompt token budget for the long context tier. The total context window is this value plus the model's max_output_tokens.
    pub max_prompt_tokens: Option<i64>,
    /// AI Credits cost per billing batch of output tokens
    pub output_price: Option<f64>,
}
crate::record_serde::wire_record!(ModelBillingTokenPricesLongContext { cache_read_price: "cacheReadPrice" if is_none, cache_write1h_price: "cacheWrite1hPrice" if is_none, cache_write_price: "cacheWritePrice" if is_none, input_price: "inputPrice" if is_none, max_prompt_tokens: "maxPromptTokens" if is_none, output_price: "outputPrice" if is_none });

/// Token-level pricing information for this model
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelBillingTokenPrices {
    /// Number of tokens per standard billing batch
    pub batch_size: Option<i64>,
    /// Cost per billing batch of cached (read) tokens. The unit is reported by models.listTokenPriceUnits.
    pub cache_read_price: Option<f64>,
    /// AI Credits cost per billing batch of 1-hour cache-write (cache creation) tokens.
    pub cache_write1h_price: Option<f64>,
    /// AI Credits cost per billing batch of cache-write (cache creation) tokens.
    pub cache_write_price: Option<f64>,
    /// Cost per billing batch of input tokens. The unit is reported by models.listTokenPriceUnits.
    pub input_price: Option<f64>,
    /// Long context tier pricing (available for models with extended context windows)
    pub long_context: Option<ModelBillingTokenPricesLongContext>,
    /// Prompt token budget for the default tier. The total context window is this value plus the model's max_output_tokens.
    pub max_prompt_tokens: Option<i64>,
    /// Cost per billing batch of output tokens. The unit is reported by models.listTokenPriceUnits.
    pub output_price: Option<f64>,
}
crate::record_serde::wire_record!(ModelBillingTokenPrices { batch_size: "batchSize" if is_none, cache_read_price: "cacheReadPrice" if is_none, cache_write1h_price: "cacheWrite1hPrice" if is_none, cache_write_price: "cacheWritePrice" if is_none, input_price: "inputPrice" if is_none, long_context: "longContext" if is_none, max_prompt_tokens: "maxPromptTokens" if is_none, output_price: "outputPrice" if is_none });

/// Billing information
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelBilling {
    /// Whole-number percentage discount (0-100) applied to usage billed through this model. Populated for the synthetic `auto` model, where requests routed by auto-mode are billed at a reduced rate; absent for concrete models.
    pub discount_percent: Option<i32>,
    /// Billing cost multiplier relative to the base rate
    pub multiplier: Option<f64>,
    /// Active server-driven promotion for this model, if any. Present when the model is being promoted with a discount, which may be time-boxed or open-ended.
    pub promo: Option<ModelBillingPromo>,
    /// Token-level pricing information for this model
    pub token_prices: Option<ModelBillingTokenPrices>,
}
crate::record_serde::wire_record!(ModelBilling { discount_percent: "discountPercent" if is_none, multiplier: "multiplier" if is_none, promo: "promo" if is_none, token_prices: "tokenPrices" if is_none });

/// A service-published message about a model, carrying a stable machine-readable code alongside human-readable text.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelMessage {
    /// Stable machine-readable identifier for the message, such as `client_version_deprecated`. Hosts can key custom presentation off this; unrecognized codes should fall back to displaying `message`.
    pub code: String,
    /// Human-readable message text intended for display to the user.
    pub message: String,
}
crate::record_serde::wire_record!(ModelMessage {
    code: "code",
    message: "message"
});

/// Policy state (if applicable)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelPolicy {
    /// Current policy state for this model
    pub state: ModelPolicyState,
    /// Usage terms or conditions for this model
    pub terms: Option<String>,
}
crate::record_serde::wire_record!(ModelPolicy { state: "state", terms: "terms" if is_none });

/// Service-published warning text that hosts should display when presenting a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelWarningText {
    /// Data-retention warning for the model. The text may contain Markdown links and should be rendered as Markdown when supported.
    pub data_retention: Option<String>,
}
crate::record_serde::wire_record!(ModelWarningText { data_retention: "dataRetention" if is_none });

/// Copilot model metadata, including identifier, display name, capabilities, policy, billing, reasoning efforts, and picker categories.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Model {
    /// Billing information
    pub billing: Option<ModelBilling>,
    /// Model capabilities and limits
    pub capabilities: ModelCapabilities,
    /// Default reasoning effort level (only present if model supports reasoning effort)
    pub default_reasoning_effort: Option<String>,
    /// Model identifier (e.g., "claude-sonnet-4.5")
    pub id: String,
    /// Informational notices the service published for this model, such as an upcoming change or a recommended alternative. Present only when the service published at least one notice. Hosts should surface these without implying anything is wrong with the model.
    pub info_messages: Option<Vec<ModelMessage>>,
    /// Provider-supplied model metadata. Keys and JSON-compatible values are preserved unchanged. This is factual metadata published by the model provider; it carries no picker or UX semantics.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// Model capability category for grouping in the model picker
    pub model_picker_category: Option<ModelPickerCategory>,
    /// Relative cost tier for token-based billing users
    pub model_picker_price_category: Option<ModelPickerPriceCategory>,
    /// Display name
    pub name: String,
    /// Policy state (if applicable)
    pub policy: Option<ModelPolicy>,
    /// The native model provider that produced this model, as a neutral reference (opaque id, human-readable label, and provider kind). Preserved by `session.model.list` and `server.models.list` when supplied by the catalog producer; omitted for unattributed and configured BYOK models. This identifies the model source, not an account or agent harness.
    pub provider: Option<ModelProviderRef>,
    /// Context-window tiers this model offers, when the provider advertises them independently of tiered token pricing. Copilot models carry their tiers in `billing.tokenPrices`; a provider that has no pricing to publish (an agent host reached over AHP, for example) declares them here instead, so the model picker can still offer the tier toggle.
    pub supported_context_tiers: Option<Vec<String>>,
    /// Supported reasoning effort levels (only present if model supports reasoning effort)
    pub supported_reasoning_efforts: Option<Vec<String>>,
    /// Model vendor as the Copilot API reports it, for example "Anthropic" or "Azure OpenAI". Open vocabulary, passed through unchanged. It can name the vendor that serves the model instead of the one that built it, or a label that is not a vendor, such as "Experimental". Absent when the Copilot API reports no vendor.
    pub vendor: Option<String>,
    /// Warnings the service published for this model, such as a deprecated client version. Present only when the service published at least one warning. The model remains usable; hosts should surface these as advisory rather than blocking.
    pub warning_messages: Option<Vec<ModelMessage>>,
    /// Warning text the service requires hosts to surface for this model. Present only when the service published at least one warning.
    pub warning_text: Option<ModelWarningText>,
}
crate::record_serde::wire_record!(Model { billing: "billing" if is_none, capabilities: "capabilities", default_reasoning_effort: "defaultReasoningEffort" if is_none, id: "id", info_messages: "infoMessages" if is_none, metadata: "metadata" if is_none, model_picker_category: "modelPickerCategory" if is_none, model_picker_price_category: "modelPickerPriceCategory" if is_none, name: "name", policy: "policy" if is_none, provider: "provider" if is_none, supported_context_tiers: "supportedContextTiers" if is_none, supported_reasoning_efforts: "supportedReasoningEfforts" if is_none, vendor: "vendor" if is_none, warning_messages: "warningMessages" if is_none, warning_text: "warningText" if is_none });

/// The runtime's classification and explanation of a model authorization failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelLoadAuthorizationError {
    /// Authorization failure classification, such as trial-paused or not-authorized.
    pub kind: String,
    /// The runtime's explanation of the authorization failure.
    pub message: String,
}
crate::record_serde::wire_record!(ModelLoadAuthorizationError {
    kind: "kind",
    message: "message"
});

/// Filesystem and environment context used to resolve model-picker settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelPickerSettingsContext {
    /// Optional Copilot configuration directory containing persisted settings.
    pub config_dir: Option<String>,
    /// Environment variables consulted while resolving model-picker settings.
    pub environment: serde_json::Value,
    /// User home directory used when resolving persisted settings.
    pub home_directory: String,
}
crate::record_serde::wire_record!(ModelPickerSettingsContext { config_dir: "configDir" if is_none, environment: "environment", home_directory: "homeDirectory" });

/// Adapter-declared policy that tells clients whether discovery may run automatically.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAutomaticDiscoveryPolicy {
    /// Whether automatic discovery is allowed, limited to configured providers, or explicit-only.
    pub mode: ModelProviderAutomaticDiscoveryMode,
    /// Maximum network scope used by this adapter during discovery.
    pub network_scope: ModelProviderDiscoveryNetworkScope,
    /// True when discovery requires non-null caller input. Omission or null is rejected before adapter execution. When false, omitted or null input selects adapter defaults without schema validation.
    pub requires_input: bool,
    /// True when the adapter must be enabled by a trusted owner, such as a trusted extension, before automatic discovery may run.
    pub requires_trust: bool,
}
crate::record_serde::wire_record!(ModelProviderAutomaticDiscoveryPolicy {
    mode: "mode",
    network_scope: "networkScope",
    requires_input: "requiresInput",
    requires_trust: "requiresTrust"
});

/// An operation supported by a model-provider adapter.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAdapterOperationDescriptor {
    /// Optional self-contained JSON Schema Draft 7 for non-null discovery input. Only supported on discover. No external references are resolved. Omitted or null input selects defaults when requiresInput is false. Without a schema, the adapter validates supplied input.
    pub input_schema: Option<serde_json::Value>,
    /// Supported operation name: `discover`, `getStatus`, or `models.list`. Unknown names and duplicate declarations are rejected.
    pub name: String,
}
crate::record_serde::wire_record!(ModelProviderAdapterOperationDescriptor { input_schema: "inputSchema" if is_none, name: "name" });

/// Contributor attribution, independent of routing identity and authorization.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAttribution {
    /// Human-readable contributor name, not the adapter display name.
    pub owner_display_name: Option<String>,
    /// Stable contributor identifier. Required and nonblank for extension, plugin, and custom sources; optional for built-in and configured sources. Does not grant authority.
    pub owner_id: Option<String>,
    /// Kind of component that supplied the adapter. Attribution does not confer authority.
    pub source: ModelProviderProvenanceSource,
}
crate::record_serde::wire_record!(ModelProviderAttribution { owner_display_name: "ownerDisplayName" if is_none, owner_id: "ownerId" if is_none, source: "source" });

/// A normalized model-provider adapter in the session's effective catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAdapterDescriptor {
    /// Stable opaque identity for routing to this adapter. Unique in the effective catalog, independent of live registration generations.
    pub adapter_id: String,
    /// Adapter-declared policy for passive and automatic discovery.
    pub automatic_discovery: ModelProviderAutomaticDiscoveryPolicy,
    /// Human-readable provider name.
    pub display_name: String,
    /// Operations supported by this provider adapter.
    pub operations: Vec<ModelProviderAdapterOperationDescriptor>,
    /// Attribution for the adapter itself.
    pub provenance: ModelProviderAttribution,
    /// Descriptive provider family, such as `ollama`. Different adapters may have the same family; use adapterId for routing.
    pub provider_kind: String,
}
crate::record_serde::wire_record!(ModelProviderAdapterDescriptor {
    adapter_id: "adapterId",
    automatic_discovery: "automaticDiscovery",
    display_name: "displayName",
    operations: "operations",
    provenance: "provenance",
    provider_kind: "providerKind"
});

/// Serializable reference to a discovered provider instance.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderInstanceReference {
    /// Stable opaque identity of the adapter that owns this reference. Must be present in the target session's effective catalog.
    pub adapter_id: String,
    /// Stable instance identifier derived by the provider adapter, such as `ollama:{normalizedEndpoint}`.
    pub id: String,
    /// Absolute provider management URI. The adapter validates normalization, supported schemes, and permission to access it against its bound configuration; a reference does not grant authority.
    pub management_endpoint: String,
    /// Descriptive provider family. Must match the selected adapter; not a routing key.
    pub provider_kind: String,
}
crate::record_serde::wire_record!(ModelProviderInstanceReference {
    adapter_id: "adapterId",
    id: "id",
    management_endpoint: "managementEndpoint",
    provider_kind: "providerKind"
});

/// A normalized model-provider instance discovered by the runtime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderInstance {
    /// Human-readable instance name.
    pub display_name: String,
    /// Inference API endpoint when the provider exposes one separately from its management endpoint.
    pub inference_endpoint: Option<String>,
    /// Transport to use for inference against this instance.
    pub inference_transport: Option<ProviderEndpointTransport>,
    /// Provider family to use for inference against this instance.
    pub inference_type: Option<ProviderEndpointType>,
    /// Wire API to use for inference against this instance, when required by the provider family.
    pub inference_wire_api: Option<ProviderEndpointWireApi>,
    /// Attribution for the adapter that produced this instance.
    pub provenance: ModelProviderProvenance,
    /// Self-contained reference for subsequent provider operations.
    pub reference: ModelProviderInstanceReference,
}
crate::record_serde::wire_record!(ModelProviderInstance { display_name: "displayName", inference_endpoint: "inferenceEndpoint" if is_none, inference_transport: "inferenceTransport" if is_none, inference_type: "inferenceType" if is_none, inference_wire_api: "inferenceWireApi" if is_none, provenance: "provenance", reference: "reference" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchConfirmation {
    /// Current conversation token count before switching models.
    pub current_tokens: f64,
    /// Effective prompt-token limit with the target model's long_context tier, present only when that tier can fit the conversation and managed policy permits it. Resubmit the switch with contextTier set to long_context to avoid compaction.
    pub long_context_limit: Option<f64>,
    /// Target model token limit used by the compaction preflight.
    pub target_limit: f64,
    /// Display name of the model that requires compaction confirmation.
    pub target_model_display_name: String,
}
crate::record_serde::wire_record!(ModelSwitchConfirmation { current_tokens: "currentTokens", long_context_limit: "longContextLimit" if is_none, target_limit: "targetLimit", target_model_display_name: "targetModelDisplayName" });

/// External tool call of the session or one of its sub-agents that is still waiting for session.tools.handlePendingToolCall.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PendingExternalToolRequest {
    /// Sub-agent instance identifier (the envelope agentId of its events) of the agent that issued the call; absent for calls issued by the root agent
    pub agent_id: Option<String>,
    /// Arguments to pass to the external tool
    pub arguments: Option<serde_json::Value>,
    /// Stable identity of the provider that offered the tool, for hosts that route extension-owned tools by provider
    pub provider_id: Option<String>,
    /// Request ID to pass to session.tools.handlePendingToolCall
    pub request_id: RequestId,
    /// Tool call ID assigned to this external tool invocation
    pub tool_call_id: String,
    /// Name of the external tool to invoke
    pub tool_name: String,
}
crate::record_serde::wire_record!(PendingExternalToolRequest { agent_id: "agentId" if is_none, arguments: "arguments" if is_none, provider_id: "providerId" if is_none, request_id: "requestId", tool_call_id: "toolCallId", tool_name: "toolName" });

/// Pending permission prompt reconstructed from event history, with request ID and user-facing prompt details.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PendingPermissionRequest {
    /// Canonical runtime-owned Assisted Permissions assessment captured on the originating permission request. Absent for historical events.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assisted_approval_assessment: Option<AssistedApprovalAssessment>,
    /// Permission-recovery episode that authorized this request to surface for interactive attention
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recovery_episode_id: Option<String>,
    /// The user-facing permission prompt details (commands, write, read, mcp, url, memory, custom-tool, path, hook)
    pub request: PermissionPromptRequest,
    /// Unique identifier for the pending permission request
    pub request_id: RequestId,
}

/// Who answered a prompt and how. A label saying a person answered (`source` `human_response`) gives the answer human authority for Assisted Permissions: for permission prompts it becomes `permission.completed.decisionSource`, and with `responseCapability` `interactive` it also covers `ask_user`, user-input and plan-review answers. The runtime accepts that claim only from the session's authenticated owning, non-extension connection, the in-process host, or a Mission Control answer the session owner wrote; any other answer settles without it. Other fields attribute the decision in telemetry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionDecisionContext {
    /// Disposition of the permission request as observed by the responding client.
    pub outcome: PermissionDecisionOutcome,
    /// Whether the responding client could ask a user interactively, was running headlessly, or had no response path. Omit when the client cannot determine this authoritatively.
    pub response_capability: Option<PermissionResponseCapability>,
    /// Controlled reason or actor responsible for the response.
    pub source: PermissionDecisionSource,
    /// Client surface that submitted the response.
    pub surface: PermissionDecisionSurface,
    /// Optional user-authored explanation of this decision, independent of approval scope. Collected only in restricted telemetry for accepted human responses to Assisted Permissions prompts. Never persisted in session history, sent to the agent or judge, or used to create approval rules. Existing rejection feedback is separate.
    pub user_feedback: Option<String>,
}
crate::record_serde::wire_record!(PermissionDecisionContext { outcome: "outcome", response_capability: "responseCapability" if is_none, source: "source", surface: "surface", user_feedback: "userFeedback" if is_none });

/// A single todo row read from the session SQL `todos` table. All fields are optional because the SQL schema is best-effort and the agent may not have populated every column.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanSqlTodosRow {
    /// Todo creation time, as stored by the session SQL schema's `datetime('now')` default: `YYYY-MM-DD HH:MM:SS` in UTC. Lets clients attribute todos to the work item that created them (e.g. scoping a goal's progress to the todos it produced) rather than to the whole session.
    pub created_at: Option<String>,
    /// Todo description.
    pub description: Option<String>,
    /// Todo identifier.
    pub id: Option<String>,
    /// Todo status.
    pub status: Option<String>,
    /// Todo title.
    pub title: Option<String>,
}
crate::record_serde::wire_record!(PlanSqlTodosRow { created_at: "createdAt" if is_none, description: "description" if is_none, id: "id" if is_none, status: "status" if is_none, title: "title" if is_none });

/// A single dependency edge read from the session SQL `todo_deps` table, indicating that one todo must complete before another.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanSqlTodoDependency {
    /// ID of the todo it depends on.
    pub depends_on: String,
    /// ID of the todo that has the dependency.
    pub todo_id: String,
}
crate::record_serde::wire_record!(PlanSqlTodoDependency {
    depends_on: "dependsOn",
    todo_id: "todoId"
});

/// Session plugin metadata, with name, marketplace, optional version, and enabled state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Plugin {
    /// Soft assignment from effective managed settings. This does not mark the plugin as hard-managed.
    pub assignment: Option<ManagedPluginAssignment>,
    /// Opaque stable identity for a direct plugin source.
    pub direct_source_id: Option<String>,
    /// Whether the plugin is currently enabled
    pub enabled: bool,
    /// Opaque plugin identity in this runtime's inventory. Matches global inventory id and MCP effectiveSource.plugin.id for the same plugin. Stable across enablement changes, updates and restarts while the source is unchanged. Session-only plugin identities are scoped to their session; not a cross-host identifier or a mutation argument. Compare the complete value without parsing it. May be absent on older runtimes.
    pub id: Option<String>,
    /// Whether this managed desired plugin has an installed or live record.
    pub installed: Option<bool>,
    /// Absolute marketplace directory for a live plugin.
    pub installed_from: Option<String>,
    /// Whether enterprise managed settings control this plugin.
    pub managed: Option<bool>,
    /// Enabled state required by enterprise managed settings.
    pub managed_desired_enabled: Option<bool>,
    /// Marketplace the plugin came from
    pub marketplace: String,
    /// Plugin name
    pub name: String,
    /// Absolute local directory containing this plugin's resolved content, including disabled installations. Absent when no content directory is resolved. This is the plugin root, not its marketplace directory; clients must not forward it back as an additional plugin.
    pub root_path: Option<String>,
    /// Runtime plugin provenance, such as "builtin".
    pub source: Option<String>,
    /// True when the signed-in account's GitHub Team assigns this plugin, whatever default enabled state the assignment carries. This reports provenance only: it does not mean the Team installed the plugin or controls whether it is enabled, and the user's own choices still apply. When `managed` is also true, enterprise managed settings decide the plugin's state.
    pub team: Option<bool>,
    /// The GitHub repository and full commit SHA from a pinned direct installation. Absent for marketplace, local and unpinned installations.
    pub verified_source: Option<InstalledPluginVerifiedSource>,
    /// Installed version
    pub version: Option<String>,
}
crate::record_serde::wire_record!(Plugin { assignment: "assignment" if is_none, direct_source_id: "directSourceId" if is_none, enabled: "enabled", id: "id" if is_none, installed: "installed" if is_none, installed_from: "installedFrom" if is_none, managed: "managed" if is_none, managed_desired_enabled: "managedDesiredEnabled" if is_none, marketplace: "marketplace", name: "name", root_path: "rootPath" if is_none, source: "source" if is_none, team: "team" if is_none, verified_source: "verifiedSource" if is_none, version: "version" if is_none });

/// Runtime activation and source for a directory-backed marketplace plugin.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginMarketplaceLiveState {
    /// Whether this plugin is active in the session's current policy context.
    pub enabled: bool,
    /// The configured directory marketplace that supplies this plugin.
    pub installed_from: String,
}
crate::record_serde::wire_record!(PluginMarketplaceLiveState {
    enabled: "enabled",
    installed_from: "installedFrom"
});

/// A marketplace plugin with its installation state in the session's profile.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PluginMarketplaceSearchEntry {
    /// Description declared by the marketplace entry.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Whether this profile has an installation of this plugin from this marketplace.
    pub installed: bool,
    /// Plugin source accepted by session.plugins.install.
    pub install_spec: String,
    /// Runtime activation state for a plugin read directly from its marketplace directory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub live: Option<PluginMarketplaceLiveState>,
    /// Configured marketplace name.
    pub marketplace: String,
    /// Plugin-supplied catalog metadata. These fields grant no session or managed-policy authority.
    pub metadata: serde_json::Value,
    /// Name declared by the marketplace entry.
    pub name: String,
    /// Marketplace source shown for this catalog entry, when available.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_url: Option<String>,
    /// Version declared by the marketplace entry, when present.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
}

/// A partial catalog failure. Other marketplace results remain available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginMarketplaceSearchError {
    /// Explanation of the failure.
    pub error: String,
    /// Marketplace name, or "*" for a failure shared by the search.
    pub marketplace: String,
    /// "installed-inventory" means installation state is unavailable and must not drive installation controls.
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(PluginMarketplaceSearchError { error: "error", marketplace: "marketplace", scope: "scope" if is_none });

/// Opt into native CAPI image generation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolImageGenerationOptions {
    /// Disabled by default for SDK sessions. CAPI model access and permissions still apply.
    pub enabled: Option<bool>,
    /// Experimental. Save each generated or edited image under the session-state
    /// directory's `images` folder and add a `saved_to: <path>` line per image to
    /// the tool result the model sees. Defaults to true; set false to opt out.
    /// Subagents follow their
    /// parent's value unless they set their own. A failed save still returns the
    /// image, without `saved_to`. Sessions without a local session-state
    /// directory, such as those using a session filesystem provider, never save.
    /// Re-supply after a cold resume. Does not enable image generation.
    /// This option may change or be removed in a future release.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub save_to_disk: Option<bool>,
    /// Experimental. Request a visible "Made with AI" label on generated and edited images.
    /// This option may change or be removed in a future release.
    /// Overrides the user's imageGenerationWatermark preference for this session;
    /// otherwise that preference applies, defaulting to false. Re-supply after a
    /// cold resume. Does not enable image generation or enforce enterprise policy.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub watermark: Option<bool>,
}
crate::record_serde::wire_record!(ProtocolImageGenerationOptions { enabled: "enabled" if is_none, save_to_disk: "saveToDisk" if is_none, watermark: "watermark" if is_none });

/// Short-lived, rotating credential the caller must send on every request, in addition to `apiKey` if one is present. Omitted when the endpoint does not require one.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderSessionToken {
    /// When the token expires, if known. Callers should refresh by calling `getEndpoint` again before this time, or reactively on any 401/403 response from `baseUrl`.
    pub expires_at: Option<String>,
    /// HTTP header name the token must be sent under.
    pub header: String,
    /// The model the token is bound to, when applicable. When set, the token is only valid for requests against this model.
    pub model: Option<String>,
    /// The short-lived token value.
    pub token: String,
}
crate::record_serde::wire_record!(ProviderSessionToken { expires_at: "expiresAt" if is_none, header: "header", model: "model" if is_none, token: "token" });

/// User-facing pending queue entry, with kind and display text for a queued message, slash command, or model change.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueuePendingItems {
    /// Agent mode stored on this queued entry, as stamped when it was enqueued. Items without an explicit mode report interactive. This is not necessarily the mode that will constrain the turn: a plan or autopilot session applies its own write gate, continuation loop and permission posture to every drained item regardless of the mode stored here.
    pub agent_mode: SendAgentMode,
    /// Human-readable text to display for this queue entry in the UI
    pub display_text: String,
    /// Stable opaque id for the canonical queued item. Batch rows share one id.
    pub id: String,
    /// Whether this item is a queued user message or a queued slash command / model change
    pub kind: QueuePendingItemsKind,
    /// Stable identity of the queued user message. Present for message rows and absent for slash commands and model changes.
    pub message_id: Option<String>,
    /// Optional source tag associated with this pending queue entry. This is an open string, not authenticated authorship. In particular, `user` does not prove that a person typed the message. If the source is absent or unrecognized, consumers must not infer human or agent authorship and should handle the entry neutrally. Consumers should tolerate future source values.
    pub source: Option<String>,
}
crate::record_serde::wire_record!(QueuePendingItems { agent_mode: "agentMode", display_text: "displayText", id: "id", kind: "kind", message_id: "messageId" if is_none, source: "source" if is_none });

/// A pending session quota warning, consumed once by takeWarnings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QuotaWarningProjection {
    /// User-facing warning text.
    pub message: String,
    /// Related account or upgrade link.
    pub url: Option<String>,
    /// Existing warning category.
    pub warning_type: String,
}
crate::record_serde::wire_record!(QuotaWarningProjection { message: "message", url: "url" if is_none, warning_type: "warningType" });

/// Credential-injection capability flags applied while the sandbox is enabled. For the same capability independent of sandboxing, and matched to the credential's GitHub host, see `shell.credentials`; the two are additive.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigAuth {
    /// Whether to authenticate sandboxed gh through the local masking proxy. The child receives a fake GH_TOKEN; its real value is substituted only at github.com, api.github.com and uploads.github.com (github.com because gh repo clone authenticates git through gh auth git-credential). The repository's GitHub account takes precedence over the Copilot login. Default: false (opt-in).
    pub gh: Option<bool>,
    /// Whether to authenticate sandboxed HTTPS git through the local masking proxy. The child receives a fake `http.<url>.extraheader`; the real Authorization header is substituted only at its original HTTPS host, port, and repository path scope. github.com uses the Copilot token; other forges use credentials resolved from the user's own helper on the host. Default: false (opt-in).
    pub git: Option<bool>,
}
crate::record_serde::wire_record!(SandboxConfigAuth { gh: "gh" if is_none, git: "git" if is_none });

/// Destinations authorized to receive one masked environment credential.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxMaskedEnvVar {
    /// Nonempty list of HTTPS injection hostnames or *.example.com patterns. Bare * is not accepted. These grants never override the sandbox network policy. Values in plaintext HTTP requests, URLs, bodies, encoded credentials, and signed requests are not substituted.
    pub inject_hosts: Vec<String>,
}
crate::record_serde::wire_record!(SandboxMaskedEnvVar {
    inject_hosts: "injectHosts"
});

/// Whole-value environment credential masking for sandboxed children.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxCredentialsConfig {
    /// Environment variable names and their HTTPS injection destinations. Absent variables stay absent. No real values or sentinels are stored in this map.
    pub env_vars: HashMap<String, SandboxMaskedEnvVar>,
}
crate::record_serde::wire_record!(SandboxCredentialsConfig {
    env_vars: "envVars"
});

/// Filesystem rules to merge into the base policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigUserPolicyFilesystem {
    /// Paths explicitly denied.
    pub denied_paths: Option<Vec<String>>,
    /// Paths granted enumerate-only access: sandboxed commands can list these directories without reading the files in them. Honored only on Windows hosts with the `filesystem_enumeration` capability; ignored elsewhere. An entry at or under a read-only, read-write, or denied path is ignored, so it never narrows a grant or reopens a denial.
    pub enumerate_paths: Option<Vec<String>>,
    /// Paths granted read-only access.
    pub readonly_paths: Option<Vec<String>>,
    /// Paths granted read/write access.
    pub readwrite_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(SandboxConfigUserPolicyFilesystem { denied_paths: "deniedPaths" if is_none, enumerate_paths: "enumeratePaths" if is_none, readonly_paths: "readonlyPaths" if is_none, readwrite_paths: "readwritePaths" if is_none });

/// HTTP proxy configuration for sandboxed traffic.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigUserPolicyNetworkProxy {
    /// Optional password for proxy authentication, combined with the URL at spawn time. The persisted value may be a literal password, a `${secret:…}` reference resolved from the OS keychain, or a `${VAR}`/`$VAR` environment reference; it is resolved just before the sandboxed process routes through the proxy. The /sandbox dialog stores a real password in the OS keychain and persists only a `${secret:…}` placeholder (never plaintext in settings.json); the field is masked in the dialog and redacted by /settings show.
    pub password: Option<String>,
    /// Proxy URL (e.g. http://proxy.example.com:8080). The port is optional and defaults to the scheme's standard port when omitted; an explicit port must be between 1 and 65535. Credentials must not be embedded here — a `user:pass@` authority is rejected; put them in the separate `username`/`password` fields. A credential-free http:// loopback proxy URL is routed through the localhost proxy automatically; loopback covers localhost and any *.localhost subdomain, the whole 127.0.0.0/8 range, ::1, and IPv4-mapped loopback (::ffff:127.0.0.1). An https:// URL, or one with a username/password set, is used as-is.
    pub url: String,
    /// Optional username for proxy authentication. Combined with the URL (and `password`) into `user:pass@host` when the sandboxed process routes through the proxy.
    pub username: Option<String>,
}
crate::record_serde::wire_record!(SandboxConfigUserPolicyNetworkProxy { password: "password" if is_none, url: "url", username: "username" if is_none });

/// Network rules to merge into the base policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigUserPolicyNetwork {
    /// Destinations allowed through the built-in sandbox proxy. Supports hostnames, IP addresses, IPv4/IPv6 CIDRs (10.0.0.0/8, fd00::/8), and *.example.com for strict subdomains. CIDR host bits must be zero; invalid entries suggest the network address. Within each nonempty list, matching names or addresses are alternative grants; every user and managed list must permit every resolved address. Empty lists add no restriction. Host rules never override outbound or local-network toggles. An upstream proxy owns DNS: CIDR-only grants cannot authorize unresolved names, and address denials cannot constrain its resolver.
    pub allowed_hosts: Option<Vec<String>>,
    /// Whether host-local destinations, including loopback and unspecified addresses, are allowed. Private networks follow outbound and host rules. Host rules still apply.
    pub allow_local_network: Option<bool>,
    /// Whether outbound network destinations, including private networks, are allowed. Host-loopback access is controlled separately by allowLocalNetwork. Host rules still apply.
    pub allow_outbound: Option<bool>,
    /// Destinations denied by the built-in sandbox proxy. Deny overrides allowedHosts, including CIDR denials against allowed names and their resolved addresses. Supports exact IPs and IPv4/IPv6 CIDRs with zero host bits. A denied domain also denies its subdomains; *.example.com matches strict subdomains, and * denies every host. With upstream-owned DNS, address denials cannot constrain remotely resolved names.
    pub blocked_hosts: Option<Vec<String>>,
    /// HTTP(S) proxy for sandboxed traffic. This is the built-in local proxy's upstream: every sandboxed command reaches it through a loopback listener, so credentials stay in the runtime and never reach the child. On Windows the sandbox also needs local network access, because it reaches that listener over host loopback. Configure credentials in the separate username/password fields. The transient local listener URL is never persisted.
    pub proxy: Option<SandboxConfigUserPolicyNetworkProxy>,
}
crate::record_serde::wire_record!(SandboxConfigUserPolicyNetwork { allowed_hosts: "allowedHosts" if is_none, allow_local_network: "allowLocalNetwork" if is_none, allow_outbound: "allowOutbound" if is_none, blocked_hosts: "blockedHosts" if is_none, proxy: "proxy" if is_none });

/// macOS seatbelt-specific options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigUserPolicySeatbelt {
    /// Exact macOS Mach service names that sandboxed processes may look up. Patterns are not supported.
    pub extra_mach_lookups: Option<Vec<String>>,
    /// Whether the macOS seatbelt profile may access the keychain.
    pub keychain_access: Option<bool>,
}
crate::record_serde::wire_record!(SandboxConfigUserPolicySeatbelt { extra_mach_lookups: "extraMachLookups" if is_none, keychain_access: "keychainAccess" if is_none });

/// User-managed sandbox policy fragment merged into the auto-discovered base policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfigUserPolicy {
    /// Additional Windows OS capability names. Default: ["packageQuery"]; set [] to opt out. Only packageQuery is accepted; other names are ignored. Applies to every sandboxed child and its descendants, including sandboxed MCP, LSP and extension processes. Does not grant filesystem access to returned package locations. Managed lists constrain default and requested names by exact intersection; an empty managed list denies all additional capabilities. Ignored on other platforms. Learning-mode capabilities are not accepted.
    pub capabilities: Option<Vec<String>>,
    /// Filesystem rules to merge into the base policy.
    pub filesystem: Option<SandboxConfigUserPolicyFilesystem>,
    /// Network rules to merge into the base policy.
    pub network: Option<SandboxConfigUserPolicyNetwork>,
    /// macOS seatbelt options to merge into the base policy.
    pub seatbelt: Option<SandboxConfigUserPolicySeatbelt>,
}
crate::record_serde::wire_record!(SandboxConfigUserPolicy { capabilities: "capabilities" if is_none, filesystem: "filesystem" if is_none, network: "network" if is_none, seatbelt: "seatbelt" if is_none });

/// Resolved sandbox configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConfig {
    /// Whether to auto-add the current working directory to readwritePaths. Default: true.
    pub add_current_working_directory: Option<bool>,
    /// Whether the agent may request that an individual command run outside the sandbox, which the host then approves or denies through the usual permission flow. A host capability flag rather than part of the policy: it is stripped from the effective spawn policy and only has an effect while `enabled` is true. Fail-closed, unlike the opt-out flags on this object: omitting it offers no bypass. Default: false (opt-in).
    pub allow_bypass: Option<bool>,
    /// Whether to auto-grant read access to tool directories discovered on PATH and in toolchain environment variables (GOROOT, JAVA_HOME, VIRTUAL_ENV, and similar), and to common developer-tool caches, config, and toolchains. Writable grants cover scratch caches, the Unix GitHub CLI cache, and Cargo's whole home (~/.cargo or a relocated CARGO_HOME), including its config, credentials.toml, and bin. Set to false to disable every grant listed above; user-installed toolchains and caches then need explicit userPolicy.filesystem readonlyPaths and readwritePaths entries. The working directory (see addCurrentWorkingDirectory), temporary storage (see allowTempStorage), session log paths, and system locations follow their own rules; this setting does not remove them. Default: true (enabled by default; set to false to opt out).
    pub allow_dev_tool_access: Option<bool>,
    /// Whether to automatically grant read-write access to the system temporary directory: `$TMPDIR` (`/tmp` when unset on Linux, the per-user container on macOS) or `%TEMP%`/`%TMP%` on Windows. Set to false to withhold this runtime-derived grant. On Linux and macOS, hosts can give the session its own scratch directory with userPolicy.filesystem readwritePaths and set `TMPDIR` for the processes that need one. On Windows, sandboxed processes keep the AppContainer's redirected `TEMP` and `TMP`; the runtime does not point those variables at a host-provided scratch directory. Only this grant is removed: the working directory (see addCurrentWorkingDirectory), the session's own files directory, developer-tool grants (see allowDevToolAccess), and every literal readwritePaths and readonlyPaths entry are unaffected, and deniedPaths still apply. SDK hosts can observe this setting in the `github.copilot.sandbox.policy_resolved` OpenTelemetry span event through `github.copilot.sandbox.allow_temp_storage`; the internal `sandbox.decision` record is not part of the public SDK event contract. Resolved paths are included only when content capture is enabled. Default: true (enabled by default; set to false to opt out).
    pub allow_temp_storage: Option<bool>,
    /// Credential-injection capability flags.
    pub auth: Option<SandboxConfigAuth>,
    /// Opt-in whole-value environment masking for sandboxed shell, MCP, and LSP children. Configured names get random sentinels; the local proxy substitutes them only in HTTPS request headers at their injection hosts. Approved bypasses skip masking and the sandbox proxy, so bypassed shells may receive the real environment values. Disabled or explicitly opted-out routes are not protected. No credential values are stored in this configuration.
    pub credentials: Option<SandboxCredentialsConfig>,
    /// Whether sandboxing is enabled for the session.
    pub enabled: bool,
    /// The `sandboxExtensions` counterpart of `managedMcpRoutingLocked`.
    #[doc(hidden)]
    pub(crate) managed_extensions_routing_locked: Option<bool>,
    /// The `sandboxLspServers` counterpart of `managedMcpRoutingLocked`.
    #[doc(hidden)]
    pub(crate) managed_lsp_routing_locked: Option<bool>,
    /// Set by the runtime when a managed policy forced `sandboxMcpServers` on and took the local opt-out away. Provenance rather than policy: it lets a sandbox startup failure point at the administrator instead of a setting the next managed merge would override, and it is ignored when comparing two configs for change. Only the managed merge may set it; a caller-supplied value is stripped.
    #[doc(hidden)]
    pub(crate) managed_mcp_routing_locked: Option<bool>,
    /// Whether extension processes the session launches are confined by the sandbox. Only an explicit `false` opts out. Ignored while `enabled` is false. Default: true (enabled by default; set to false to opt out).
    pub sandbox_extensions: Option<bool>,
    /// Whether language servers the session launches are confined by the sandbox. Only an explicit `false` opts out. Ignored while `enabled` is false. Default: true (enabled by default; set to false to opt out).
    pub sandbox_lsp_servers: Option<bool>,
    /// Whether MCP servers the session launches are confined by the sandbox. Only an explicit `false` opts out; doing so also lets remote-MCP egress leave the sandbox, so the flag and `enabled` are always read together. Ignored while `enabled` is false. Default: true (enabled by default; set to false to opt out).
    pub sandbox_mcp_servers: Option<bool>,
    /// User-managed sandbox policy fragment merged into the auto-discovered base policy.
    pub user_policy: Option<SandboxConfigUserPolicy>,
}
crate::record_serde::wire_record!(SandboxConfig { add_current_working_directory: "addCurrentWorkingDirectory" if is_none, allow_bypass: "allowBypass" if is_none, allow_dev_tool_access: "allowDevToolAccess" if is_none, allow_temp_storage: "allowTempStorage" if is_none, auth: "auth" if is_none, credentials: "credentials" if is_none, enabled: "enabled", managed_extensions_routing_locked: "managedExtensionsRoutingLocked" if is_none, managed_lsp_routing_locked: "managedLspRoutingLocked" if is_none, managed_mcp_routing_locked: "managedMcpRoutingLocked" if is_none, sandbox_extensions: "sandboxExtensions" if is_none, sandbox_lsp_servers: "sandboxLspServers" if is_none, sandbox_mcp_servers: "sandboxMcpServers" if is_none, user_policy: "userPolicy" if is_none });

/// A possible secret-bearing environment variable and optional locally suggested HTTPS injection hosts. This is a draft for user review, not an active grant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxCredentialSuggestion {
    /// Environment variable name. The secret value is never returned.
    pub name: String,
    /// HTTPS hostnames suggested by a local known-provider mapping. An empty list requires the user to supply injection hosts before adding a masking entry. These suggestions do not allow network access.
    pub suggested_inject_hosts: Vec<String>,
}
crate::record_serde::wire_record!(SandboxCredentialSuggestion {
    name: "name",
    suggested_inject_hosts: "suggestedInjectHosts"
});

/// One local RPC attachment, not the current route for a session ID.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxTransportOwnerLocalRpc {
    /// Identity of the original authenticated attachment.
    pub attach_id: i64,
    /// Original sandbox transport owner discriminator.
    pub kind: SandboxTransportOwnerLocalRpcKind,
}
crate::record_serde::wire_record!(SandboxTransportOwnerLocalRpc {
    attach_id: "attachId",
    kind: "kind"
});

/// One AHP connection and its runtime-bound session channel.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxTransportOwnerAhp {
    /// Original sandbox transport owner discriminator.
    pub kind: SandboxTransportOwnerAhpKind,
    /// Identity of the original relay connection.
    pub relay_id: i64,
    /// Owner-side session channel bound to the local presentation session.
    pub session_uri: String,
}
crate::record_serde::wire_record!(SandboxTransportOwnerAhp {
    kind: "kind",
    relay_id: "relayId",
    session_uri: "sessionUri"
});

/// The filesystem and environment facts a user-settings file is resolved from. Passed explicitly rather than read from the runtime's own process, so a host whose configuration directory differs from its process environment resolves the file it means.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UserSettingsHostContext {
    /// The Copilot configuration directory, when the host set one. Takes precedence over `COPILOT_HOME` and the home directory.
    pub config_dir: Option<String>,
    /// The host's environment variables. `COPILOT_HOME` is read from here, as are the variables a settings hook may expand.
    pub environment: serde_json::Value,
    /// The user's home directory, used when neither a configuration directory nor `COPILOT_HOME` is set.
    pub home_directory: String,
}
crate::record_serde::wire_record!(UserSettingsHostContext { config_dir: "configDir" if is_none, environment: "environment", home_directory: "homeDirectory" });

/// A saved provider's editable settings and credential status, never secret values.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationProvider {
    /// Credential availability without disclosing its value.
    pub credential_status: SavedProviderCredentialStatus,
    /// Safe connection settings; commands, headers, credentials, and internal storage metadata are omitted.
    pub provider: SavedProviderConfigurationProviderInput,
}
crate::record_serde::wire_record!(SavedProviderConfigurationProvider {
    credential_status: "credentialStatus",
    provider: "provider"
});

/// Scheduled prompt entry with ID, timing (`intervalMs`, `cron`, or `at`), prompt text, recurrence, and next run time.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleEntry {
    /// Absolute fire time (epoch milliseconds) for a one-shot calendar schedule.
    pub at: Option<i64>,
    /// 5-field cron expression for a recurring calendar schedule, evaluated in `tz`.
    pub cron: Option<String>,
    /// Display-only label for the prompt as shown in the UI (e.g. `/skill-name` for a skill-invocation schedule). The actual enqueued prompt is `prompt`.
    pub display_prompt: Option<String>,
    /// Sequential id assigned by the runtime within the session. Stable across resumes (rebuilt from the event log).
    pub id: i64,
    /// Interval between scheduled ticks, in milliseconds (relative-interval schedules).
    pub interval_ms: Option<i64>,
    /// ISO 8601 timestamp when the next tick is scheduled to fire.
    pub next_run_at: String,
    /// Prompt text that gets enqueued on every tick.
    pub prompt: String,
    /// Whether the schedule re-arms after each tick (`/every`) or fires once (`/after`).
    pub recurring: bool,
    /// True for a self-paced (`dynamic`) schedule: no fixed cadence; the model arms each next run via the `manage_schedule` `wakeup` action. `nextRunAt` is model-controlled.
    pub self_paced: Option<bool>,
    /// IANA timezone the `cron` expression is evaluated in.
    pub tz: Option<String>,
}
crate::record_serde::wire_record!(ScheduleEntry { at: "at" if is_none, cron: "cron" if is_none, display_prompt: "displayPrompt" if is_none, id: "id", interval_ms: "intervalMs" if is_none, next_run_at: "nextRunAt", prompt: "prompt", recurring: "recurring", self_paced: "selfPaced" if is_none, tz: "tz" if is_none });

/// Server-side skill metadata, including name, description, source, enabled/invocable state, path, project path, and argument hint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ServerSkill {
    /// Optional freeform hint describing the skill's expected arguments, from the `argument-hint` frontmatter field
    pub argument_hint: Option<String>,
    /// Canonical slash command name used to invoke the skill, without the leading '/'
    pub command_name: Option<String>,
    /// Description of what the skill does
    pub description: String,
    /// Whether the skill is currently enabled (based on global config)
    pub enabled: bool,
    /// Unique identifier for the skill
    pub name: String,
    /// Absolute path to the skill file
    pub path: Option<String>,
    /// Opaque identity of the plugin owning this skill. Matches plugins.list().plugins[].id on the same runtime host. Compare the complete value without parsing it. Omitted for non-plugin skills.
    pub plugin_id: Option<String>,
    /// The project path this skill belongs to (only for project/inherited skills)
    pub project_path: Option<String>,
    /// Source location type (e.g., project, personal-copilot, plugin, builtin)
    pub source: SkillSource,
    /// Whether the skill can be invoked by the user as a slash command
    pub user_invocable: bool,
}
crate::record_serde::wire_record!(ServerSkill { argument_hint: "argumentHint" if is_none, command_name: "commandName" if is_none, description: "description", enabled: "enabled", name: "name", path: "path" if is_none, plugin_id: "pluginId" if is_none, project_path: "projectPath" if is_none, source: "source", user_invocable: "userInvocable" });

/// Command-scoped GitHub credential injection for the shell commands an agent runs.
///
/// Each channel is opt-in and independent, and injection is scoped to the individual command
/// spawn: the credential is resolved from the session's *current* authentication at every spawn
/// and reaches only spawns whose script actually invokes `git` or `gh`. Because nothing is
/// retained between spawns, replacing the session credential (`session.gitHubAuth.setCredentials`)
/// changes what the next spawned command presents — which seeding a credential into the runtime
/// process's own environment cannot do, since a child's environment is fixed at `exec`.
///
/// The credential is matched to the host it authenticates to, so a github.com credential is never
/// presented to a GitHub Enterprise host and vice versa. Where a channel cannot express that
/// boundary it injects nothing rather than crossing it -- see `gh` below.
///
/// This is independent of `sandboxConfig`: it is a decision about which identity the agent
/// presents, not about what the agent may touch, and it works on every platform whether or not
/// an OS sandboxing backend is available. `sandboxConfig.auth` remains the sandbox-scoped
/// spelling and is additive with this one.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellCredentials {
    /// Whether to authenticate the agent's `gh` commands as the session's GitHub credential, by
    /// exporting `GH_TOKEN` to a spawn that runs `gh`. Any inherited `gh` credential is removed from
    /// spawns that do not, so the credential stays command-scoped.
    ///
    /// Applies to a github.com credential only. `gh` picks its credential variable from the host a
    /// command targets rather than the one the credential belongs to, and the command can choose that
    /// target, so `GH_ENTERPRISE_TOKEN` would offer a single-tenant enterprise credential to every
    /// other enterprise host. A session whose credential is enterprise-scoped therefore runs `gh`
    /// unauthenticated; its `git` commands are unaffected, because `http.<host>.extraheader` is scoped
    /// to one host by construction. Default: false (opt-in).
    pub gh: Option<bool>,
    /// Whether to authenticate the agent's `git` commands as the session's GitHub credential, by
    /// injecting an `http.<host>.extraheader` (plus `insteadOf` rewrites so SSH-spelled remotes for
    /// that host use the authenticated HTTPS transport). Applied only to a spawn that runs a
    /// remote-contacting `git` subcommand. Default: false (opt-in).
    pub git: Option<bool>,
}
crate::record_serde::wire_record!(ShellCredentials { gh: "gh" if is_none, git: "git" if is_none });

/// A host-provided script sourced before each built-in shell command when its shell target matches the active shell.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellInitScript {
    /// Path to the script to source.
    pub path: String,
    /// Built-in shell that may source this script.
    pub shell: ShellInitScriptShell,
}
crate::record_serde::wire_record!(ShellInitScript {
    path: "path",
    shell: "shell"
});

/// Per-session settings for built-in shell tools.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellOptions {
    /// Command-scoped GitHub credential injection for shell commands.
    pub credentials: Option<ShellCredentials>,
    /// Controls automatic non-interactive profile loading where supported. Explicit initScripts are unaffected.
    pub init_profile: Option<ShellInitProfile>,
    /// Ordered host-provided script paths sourced before each built-in shell command when the
    /// entry's shell target matches the active shell. Use these for rc files, environment setup scripts,
    /// or other custom scripts. A script that returns a nonzero status is reported, and later scripts
    /// and the user command continue while the shell remains running. Because scripts are sourced into
    /// the command shell, `exit`, `exec`, failures under `set -e`, or other shell-terminating behavior
    /// can prevent continuation. Script standard output is preserved; Bash script stderr is discarded,
    /// PowerShell exception messages are replaced, and runtime-generated failure notices omit
    /// configured script paths. When sandboxing is enabled, each script must already be readable under
    /// the active sandbox filesystem policy. Pass an empty array to clear the list.
    pub init_scripts: Option<Vec<ShellInitScript>>,
    /// Flags passed to the active built-in shell process on startup, replacing its default flags.
    /// When omitted, the built-in Bash shell uses `--norc --noprofile`,
    /// and the built-in PowerShell shell uses `-NoProfile -NoLogo`.
    pub process_flags: Option<Vec<String>>,
}
crate::record_serde::wire_record!(ShellOptions { credentials: "credentials" if is_none, init_profile: "initProfile" if is_none, init_scripts: "initScripts" if is_none, process_flags: "processFlags" if is_none });

/// Skill metadata available to a session, with name, description, source, enabled/invocable state, path, plugin, and argument hint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Skill {
    /// Optional freeform hint describing the skill's expected arguments, from the `argument-hint` frontmatter field
    pub argument_hint: Option<String>,
    /// Canonical slash command name used to invoke the skill, without the leading '/'
    pub command_name: Option<String>,
    /// Original frontmatter name for an MCP skill, for display only. Use name for identity and enable/disable operations.
    pub declared_name: Option<String>,
    /// Description of what the skill does
    pub description: String,
    /// Whether the skill is currently enabled
    pub enabled: bool,
    /// MCP server and skill resource address when source is 'mcp'. Contains no activation grants or internal verification metadata.
    pub mcp_origin: Option<McpSkillOrigin>,
    /// Unique identifier for the skill
    pub name: String,
    /// Absolute path to the skill file
    pub path: Option<String>,
    /// Opaque identity of the plugin owning this skill. Matches plugins.list().plugins[].id on the same runtime host. Compare the complete value without parsing it. Omitted for non-plugin skills.
    pub plugin_id: Option<String>,
    /// Name of the plugin that provides the skill, when source is 'plugin'
    pub plugin_name: Option<String>,
    /// Source location type (e.g., project, personal-copilot, plugin, builtin)
    pub source: SkillSource,
    /// Whether the skill can be invoked by the user as a slash command
    pub user_invocable: bool,
}
crate::record_serde::wire_record!(Skill { argument_hint: "argumentHint" if is_none, command_name: "commandName" if is_none, declared_name: "declaredName" if is_none, description: "description", enabled: "enabled", mcp_origin: "mcpOrigin" if is_none, name: "name", path: "path" if is_none, plugin_id: "pluginId" if is_none, plugin_name: "pluginName" if is_none, source: "source", user_invocable: "userInvocable" });

/// Skill invocation record with name, path, content, allowed tools, and turn number.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsInvokedSkill {
    /// Tools that should be auto-approved when this skill is active, captured at invocation time
    pub allowed_tools: Option<Vec<String>>,
    /// Exact invocation arguments when recorded, retained separately from the reusable skill body
    pub arguments: Option<String>,
    /// Full content of the skill file
    pub content: String,
    /// Whether model invocation was disabled when this skill was invoked
    pub disable_model_invocation: Option<bool>,
    /// Turn number when the skill was invoked
    pub invoked_at_turn: i64,
    /// Unique identifier for the skill
    pub name: String,
    /// Path to the SKILL.md file, or an empty string for an SDK-provided skill without a filesystem identity
    pub path: String,
}
crate::record_serde::wire_record!(SkillsInvokedSkill { allowed_tools: "allowedTools" if is_none, arguments: "arguments" if is_none, content: "content", disable_model_invocation: "disableModelInvocation" if is_none, invoked_at_turn: "invokedAtTurn", name: "name", path: "path" });

/// Subagent model, reasoning effort, context tier, and auto-invocation settings
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SubagentSettingsEntry {
    /// Whether this agent's runtime-defined proactive invocation prompting is enabled, if supported. Currently consumed by the built-in rubber-duck agent.
    pub auto_invoke: Option<bool>,
    /// Context tier override for matching subagents
    pub context_tier: Option<SubagentSettingsEntryContextTier>,
    /// Reasoning effort override for matching subagents
    pub effort_level: Option<String>,
    /// Model override for matching subagents
    pub model: Option<String>,
    /// Whether the configured model strategy is preferred or required
    pub model_policy: Option<AgentModelPolicy>,
    /// Provider identifier for the configured model override
    pub provider_id: Option<String>,
}
crate::record_serde::wire_record!(SubagentSettingsEntry { auto_invoke: "autoInvoke" if is_none, context_tier: "contextTier" if is_none, effort_level: "effortLevel" if is_none, model: "model" if is_none, model_policy: "modelPolicy" if is_none, provider_id: "providerId" if is_none });

/// Configured per-agent subagent overrides
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SubagentSettingsConfig {
    /// Per-agent settings keyed by subagent agent_type
    pub agents: Option<HashMap<String, SubagentSettingsEntry>>,
    /// Configured opt-in for subagent context management tools; applied from persisted settings when the session starts
    pub context_management_tools: Option<bool>,
    /// Names of subagents the user has turned off; they cannot be dispatched
    pub disabled_subagents: Option<Vec<String>>,
    /// Maximum number of subagents that can run concurrently; applies to usage-based billing users only
    pub max_concurrency: Option<i32>,
    /// Maximum subagent nesting depth; applies to usage-based billing users only
    pub max_depth: Option<i32>,
}
crate::record_serde::wire_record!(SubagentSettingsConfig { agents: "agents" if is_none, context_management_tools: "contextManagementTools" if is_none, disabled_subagents: "disabledSubagents" if is_none, max_concurrency: "maxConcurrency" if is_none, max_depth: "maxDepth" if is_none });

/// Requested and resolved context configuration observed when a direct workflow agent launched.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentContextObservation {
    /// Full context-window capacity resolved for the initialized child session.
    pub max_context_window_tokens: Option<i64>,
    /// Effective prompt-token input budget resolved for the initialized child session.
    pub prompt_token_limit: Option<i64>,
    /// Context-tier override explicitly supplied by the workflow call.
    pub requested_tier: Option<ContextTier>,
    /// Effective context-tier identifier reported by the initialized child session. Native sessions currently report `default` or `long_context`; remote participants may report another supported identifier.
    pub resolved_tier: Option<String>,
}
crate::record_serde::wire_record!(WorkflowAgentContextObservation { max_context_window_tokens: "maxContextWindowTokens" if is_none, prompt_token_limit: "promptTokenLimit" if is_none, requested_tier: "requestedTier" if is_none, resolved_tier: "resolvedTier" if is_none });

/// Requested and resolved model observations for a direct workflow agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentModelObservation {
    /// Non-empty model override explicitly supplied by the workflow call.
    pub requested: Option<String>,
    /// Concrete model reported by the initialized child session.
    pub resolved: Option<String>,
}
crate::record_serde::wire_record!(WorkflowAgentModelObservation { requested: "requested" if is_none, resolved: "resolved" if is_none });

/// Requested and resolved reasoning-effort observations for a direct workflow agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentReasoningEffortObservation {
    /// Non-empty reasoning-effort override explicitly supplied by the workflow call.
    pub requested: Option<String>,
    /// Effective reasoning effort reported by the initialized child session.
    pub resolved: Option<String>,
}
crate::record_serde::wire_record!(WorkflowAgentReasoningEffortObservation { requested: "requested" if is_none, resolved: "resolved" if is_none });

/// Prompt-safe durable identity and live status for a direct workflow agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentSummary {
    /// Accumulated active agent time in milliseconds.
    pub active_ms: i64,
    /// Prompt-safe live activity text.
    pub activity: Option<String>,
    /// Stable direct-agent identifier.
    pub agent_id: String,
    /// Registered agent type.
    pub agent_type: String,
    /// Epoch milliseconds when the agent completed.
    pub completed_at: Option<i64>,
    /// Known AI-credit spend, in nano-AIU, produced by this direct agent and its descendants. Omitted for historical agents when unattributed charges prevent a known zero.
    pub consumed_nano_aiu: Option<i64>,
    /// Requested and resolved context observations, when available.
    pub context: Option<WorkflowAgentContextObservation>,
    /// Friendly, non-unique name intended for display
    pub display_name: Option<String>,
    /// Friendly, non-unique name intended for display
    pub label: String,
    /// Requested and resolved model observations, when available.
    pub model: Option<WorkflowAgentModelObservation>,
    /// Phase identifier active when the agent was launched, or null.
    pub phase_id: Option<String>,
    /// Requested and resolved reasoning-effort observations, when available.
    pub reasoning_effort: Option<WorkflowAgentReasoningEffortObservation>,
    /// Owning workflow run identifier.
    pub run_id: String,
    /// Epoch milliseconds when the agent started.
    pub started_at: Option<i64>,
    /// Current durable or live agent status.
    pub status: String,
    /// Tool-call identifier that launched the agent.
    pub tool_call_id: String,
}
crate::record_serde::wire_record!(WorkflowAgentSummary { active_ms: "activeMs", activity: "activity" if is_none, agent_id: "agentId", agent_type: "agentType", completed_at: "completedAt" if is_none, consumed_nano_aiu: "consumedNanoAiu" if is_none, context: "context" if is_none, display_name: "displayName" if is_none, label: "label", model: "model" if is_none, phase_id: "phaseId", reasoning_effort: "reasoningEffort" if is_none, run_id: "runId", started_at: "startedAt" if is_none, status: "status", tool_call_id: "toolCallId" });

/// Current workflow phase identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowCurrentPhase {
    /// Current phase identifier.
    pub id: String,
    /// Zero-based declared phase ordinal, or null for an undeclared phase.
    pub ordinal: Option<i64>,
}
crate::record_serde::wire_record!(WorkflowCurrentPhase {
    id: "id",
    ordinal: "ordinal"
});

/// Declared or approved workflow resource ceilings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowDeclaredLimits {
    /// Maximum AI credits consumed by subagents and descendants.
    pub max_ai_credits: Option<f64>,
    /// Maximum concurrently active subagents.
    pub max_concurrent_subagents: Option<i64>,
    /// Maximum total subagents spawned by the run.
    pub max_total_subagents: Option<i64>,
    /// Maximum accumulated active execution time in seconds.
    pub timeout_seconds: Option<f64>,
}
crate::record_serde::wire_record!(WorkflowDeclaredLimits { max_ai_credits: "maxAiCredits" if is_none, max_concurrent_subagents: "maxConcurrentSubagents" if is_none, max_total_subagents: "maxTotalSubagents" if is_none, timeout_seconds: "timeoutSeconds" if is_none });

/// Durable workflow resource consumption.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunConsumed {
    /// Accumulated active execution time in milliseconds.
    pub active_ms: i64,
    /// AI usage consumed by the run in nano-AIU.
    pub nano_aiu: i64,
    /// Total subagents spawned by the run.
    pub subagents: i64,
}
crate::record_serde::wire_record!(WorkflowRunConsumed {
    active_ms: "activeMs",
    nano_aiu: "nanoAiu",
    subagents: "subagents"
});

/// Prompt-safe terminal workflow outcome.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunTerminal {
    /// Human-readable terminal error.
    pub error: Option<String>,
    /// Machine-readable terminal failure.
    pub failure: Option<serde_json::Value>,
    /// Pause initiator metadata, or null when the run did not pause.
    pub pause_info: Option<serde_json::Value>,
    /// Human-readable terminal reason.
    pub reason: Option<String>,
    /// Prompt-safe preview of the completed result.
    pub result_preview: Option<String>,
}
crate::record_serde::wire_record!(WorkflowRunTerminal { error: "error" if is_none, failure: "failure" if is_none, pause_info: "pauseInfo", reason: "reason" if is_none, result_preview: "resultPreview" if is_none });

/// Durable workflow run summary with read-time live overlays.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunSummary {
    /// Epoch milliseconds when the current active segment started, or null while inactive.
    pub active_segment_started_at: Option<i64>,
    /// Approved effective resource ceilings, or null until approved.
    pub approved: Option<WorkflowDeclaredLimits>,
    /// Whether the durable run state currently passes runtime resume eligibility checks.
    pub can_resume: bool,
    /// Epoch milliseconds when the run completed, or null while nonterminal.
    pub completed_at: Option<i64>,
    /// Durable resource consumption.
    pub consumed: WorkflowRunConsumed,
    /// Epoch milliseconds when the run was created.
    pub created_at: i64,
    /// Current phase identity, or null before any phase is entered.
    pub current_phase: Option<WorkflowCurrentPhase>,
    /// Resource ceilings declared by the workflow.
    pub declared_limits: WorkflowDeclaredLimits,
    /// Number of phases declared by the workflow.
    pub declared_phase_count: i64,
    /// Human-readable workflow description.
    pub description: String,
    /// Number of direct workflow agents currently live.
    pub live_agent_count: i64,
    /// Epoch milliseconds when this live-overlay snapshot was observed.
    pub observed_at: i64,
    /// Monotonic durable run revision.
    pub revision: i64,
    /// Workflow run identifier.
    pub run_id: String,
    /// Epoch milliseconds when execution first started, or null before start.
    pub started_at: Option<i64>,
    /// Current workflow run status.
    pub status: WorkflowRunStatus,
    /// Terminal run outcome, or null while nonterminal.
    pub terminal: Option<WorkflowRunTerminal>,
    /// Total direct workflow agents spawned across all attempts.
    pub total_spawned_agent_count: i64,
    /// Epoch milliseconds when the durable run was last updated.
    pub updated_at: i64,
    /// Registered workflow name.
    pub workflow_name: String,
}
crate::record_serde::wire_record!(WorkflowRunSummary {
    active_segment_started_at: "activeSegmentStartedAt",
    approved: "approved",
    can_resume: "canResume",
    completed_at: "completedAt",
    consumed: "consumed",
    created_at: "createdAt",
    current_phase: "currentPhase",
    declared_limits: "declaredLimits",
    declared_phase_count: "declaredPhaseCount",
    description: "description",
    live_agent_count: "liveAgentCount",
    observed_at: "observedAt",
    revision: "revision",
    run_id: "runId",
    started_at: "startedAt",
    status: "status",
    terminal: "terminal",
    total_spawned_agent_count: "totalSpawnedAgentCount",
    updated_at: "updatedAt",
    workflow_name: "workflowName"
});

/// Durable lifecycle and timing for one workflow phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowPhaseObservation {
    /// Completed active time accumulated by this phase in milliseconds.
    pub accumulated_active_ms: i64,
    /// Epoch milliseconds when this phase completed; for a skipped phase, the synthetic skip timestamp (equal to `startedAt`).
    pub completed_at: Option<i64>,
    /// Current live active time for this phase in milliseconds.
    pub current_active_ms: i64,
    /// Optional human-readable phase detail.
    pub detail: Option<String>,
    /// Number of times execution entered this phase.
    pub entry_count: i64,
    /// Phase identifier.
    pub id: String,
    /// Most recent run attempt that entered this phase, or `0` if the phase has never been entered.
    pub last_entered_run_attempt: i64,
    /// Direct agents in this phase that are currently live.
    pub live_agent_count: i64,
    /// Zero-based declared phase ordinal, or null for an undeclared phase.
    pub ordinal: Option<i64>,
    /// Epoch milliseconds when this phase first started; for a skipped phase, the synthetic skip timestamp (equal to `completedAt`).
    pub started_at: Option<i64>,
    /// Derived lifecycle state of the phase.
    pub status: WorkflowPhaseStatus,
    /// Human-readable phase title.
    pub title: String,
    /// Total direct agents associated with this phase.
    pub total_agent_count: i64,
}
crate::record_serde::wire_record!(WorkflowPhaseObservation { accumulated_active_ms: "accumulatedActiveMs", completed_at: "completedAt" if is_none, current_active_ms: "currentActiveMs", detail: "detail" if is_none, entry_count: "entryCount", id: "id", last_entered_run_attempt: "lastEnteredRunAttempt", live_agent_count: "liveAgentCount", ordinal: "ordinal", started_at: "startedAt" if is_none, status: "status", title: "title", total_agent_count: "totalAgentCount" });

/// One durable workflow progress record.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowProgressLine {
    /// Resume attempt that emitted this record.
    pub attempt: i64,
    /// Progress record kind.
    pub kind: WorkflowLogLineKind,
    /// Phase active when the record was emitted, or null before any phase.
    pub phase_id: Option<String>,
    /// Epoch milliseconds when the record was persisted.
    pub recorded_at: i64,
    /// Global monotonic sequence number within the run.
    pub seq: i64,
    /// Prompt-safe progress text.
    pub text: String,
}
crate::record_serde::wire_record!(WorkflowProgressLine {
    attempt: "attempt",
    kind: "kind",
    phase_id: "phaseId",
    recorded_at: "recordedAt",
    seq: "seq",
    text: "text"
});

/// A bidirectional page of workflow progress.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowProgressPage {
    /// Whether progress records newer than this page exist.
    pub has_more_newer: bool,
    /// Whether progress records older than this page exist.
    pub has_more_older: bool,
    /// Newest sequence number in this page, or null when empty.
    pub newest_seq: Option<i64>,
    /// Oldest sequence number in this page, or null when empty.
    pub oldest_seq: Option<i64>,
    /// Progress records in sequence order.
    pub records: Vec<WorkflowProgressLine>,
    /// Run revision reflected by this page.
    pub revision: i64,
}
crate::record_serde::wire_record!(WorkflowProgressPage {
    has_more_newer: "hasMoreNewer",
    has_more_older: "hasMoreOlder",
    newest_seq: "newestSeq",
    oldest_seq: "oldestSeq",
    records: "records",
    revision: "revision"
});

/// Complete current or terminal workflow run envelope.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunResult {
    /// One-based execution attempt represented by this envelope. Absent before the first attempt starts or when returned by an older runtime.
    pub attempt: Option<i64>,
    /// Error message for an errored run.
    pub error: Option<String>,
    /// Machine-readable failure details for a halted or errored run.
    pub failure: Option<serde_json::Value>,
    /// Structured pause initiator metadata for a paused attempt.
    pub pause_info: Option<serde_json::Value>,
    /// Reason for a halted or cancelled run.
    pub reason: Option<String>,
    /// Completed workflow result.
    pub result: Option<serde_json::Value>,
    /// Workflow run identifier.
    pub run_id: String,
    /// Partial journal and progress snapshot for a halted, cancelled, or errored run.
    pub snapshot: Option<serde_json::Value>,
    /// Current or terminal workflow run status.
    pub status: WorkflowRunStatus,
}
crate::record_serde::wire_record!(WorkflowRunResult { attempt: "attempt" if is_none, error: "error" if is_none, failure: "failure" if is_none, pause_info: "pauseInfo" if is_none, reason: "reason" if is_none, result: "result" if is_none, run_id: "runId", snapshot: "snapshot" if is_none, status: "status" });

/// Complete text used to generate one session diff. These are display contents, with the same text decoding as the patch, not a file-restore contract.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspaceDiffContents {
    /// Complete current text read when computing the diff. Omitted for a deleted file; an empty string represents an existing empty file.
    pub after: Option<String>,
    /// Complete text before the session first changed the file. Omitted when the file did not exist; an empty string represents an existing empty file.
    pub before: Option<String>,
}
crate::record_serde::wire_record!(WorkspaceDiffContents { after: "after" if is_none, before: "before" if is_none });

/// A single changed file and its unified diff.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspaceDiffFileChange {
    /// Type of change represented by this file diff.
    pub change_type: WorkspaceDiffFileChangeType,
    /// Full text used for this patch, only when includeContents was requested for session mode. Omitted for binary, oversized or unavailable contents, and for fallback results. Read isFallback and isTruncated before treating an absent value as a missing file.
    pub contents: Option<WorkspaceDiffContents>,
    /// Unified diff content for the file. Empty when the diff was truncated.
    pub diff: String,
    /// Whether the diff content was omitted because it exceeded the per-file size limit.
    pub is_truncated: Option<bool>,
    /// Original file path for renamed files.
    pub old_path: Option<String>,
    /// Path to the changed file, relative to the workspace root when the file lives under it. A file changed outside the workspace root keeps a `../`-relative path, or an absolute path when no relative path exists (for example a different Windows drive).
    pub path: String,
}
crate::record_serde::wire_record!(WorkspaceDiffFileChange { change_type: "changeType", contents: "contents" if is_none, diff: "diff", is_truncated: "isTruncated" if is_none, old_path: "oldPath" if is_none, path: "path" });

/// Workspace checkpoint metadata with assigned number, human-readable title, and checkpoint filename.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkspacesCheckpoints {
    /// Filename of the checkpoint within the workspace checkpoints directory
    pub filename: String,
    /// Checkpoint number assigned by the workspace manager
    pub number: i64,
    /// Human-readable checkpoint title
    pub title: String,
}
crate::record_serde::wire_record!(WorkspacesCheckpoints {
    filename: "filename",
    number: "number",
    title: "title"
});

/// MCP CreateMessageResult payload (with optional 'tools' extension), present when action='success'. Treated as opaque at the schema layer; consumers should construct/consume it per the MCP CreateMessageResult shape.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type McpExecuteSamplingResult = HashMap<String, serde_json::Value>;

/// A standard MCP tool result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type McpToolResult = HashMap<String, serde_json::Value>;

/// Canonical availability treatment for an account row.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AccountHealth {
    /// The account is available and may be switchable.
    #[serde(rename = "available")]
    Available,
    /// The account is intentionally unavailable, for example because access is disabled or the platform is unsupported.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Availability is unknown because a transient or retryable check failed.
    #[serde(rename = "warning")]
    Warning,
    /// Authentication or saved credentials require user action.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AccountHealth {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountHealth",
            &["available", "unavailable", "warning", "error", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Available,
            1 => Self::Unavailable,
            2 => Self::Warning,
            3 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// The provider kind of a signed-in account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AccountKind {
    /// An OAuth github.com account.
    #[serde(rename = "gitHub")]
    GitHub,
    /// A GitHub Enterprise Cloud account on a non-github.com host, e.g. *.ghe.com.
    #[serde(rename = "gitHubEnterpriseCloud")]
    GitHubEnterpriseCloud,
    /// A GitHub (EMU) account derived from a base Entra identity.
    #[serde(rename = "entraEmu")]
    EntraEmu,
    /// A base Microsoft Entra identity.
    #[serde(rename = "entra")]
    Entra,
    /// A selectable Microsoft 365 Copilot inference account, backed by a base Entra identity or an external bearer.
    #[serde(rename = "microsoft365Copilot")]
    Microsoft365Copilot,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AccountKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountKind",
            &[
                "gitHub",
                "gitHubEnterpriseCloud",
                "entraEmu",
                "entra",
                "microsoft365Copilot",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::GitHub,
            1 => Self::GitHubEnterpriseCloud,
            2 => Self::EntraEmu,
            3 => Self::Entra,
            4 => Self::Microsoft365Copilot,
            _ => Self::Unknown,
        })
    }
}

/// Authentication host. HMAC auth always targets the public GitHub host.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HMACAuthInfoHost {
    #[serde(rename = "https://github.com")]
    #[default]
    HttpsGitHubCom,
}
impl<'de> Deserialize<'de> for HMACAuthInfoHost {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "HMACAuthInfoHost",
            &["https://github.com"],
            None,
        )?;
        Ok(Self::HttpsGitHubCom)
    }
}

/// HMAC-based authentication used by GitHub-internal services.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HMACAuthInfoType {
    #[serde(rename = "hmac")]
    #[default]
    Hmac,
}
impl<'de> Deserialize<'de> for HMACAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(deserializer, "HMACAuthInfoType", &["hmac"], None)?;
        Ok(Self::Hmac)
    }
}

/// Personal access token (PAT) or server-to-server token sourced from an environment variable.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EnvAuthInfoType {
    #[serde(rename = "env")]
    #[default]
    Env,
}
impl<'de> Deserialize<'de> for EnvAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(deserializer, "EnvAuthInfoType", &["env"], None)?;
        Ok(Self::Env)
    }
}

/// SDK-side token authentication; the host configured the token directly via the SDK.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TokenAuthInfoType {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for TokenAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "TokenAuthInfoType",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

/// SDK callback-backed GitHub token authentication.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TokenProviderAuthInfoType {
    #[serde(rename = "token-provider")]
    #[default]
    TokenProvider,
}
impl<'de> Deserialize<'de> for TokenProviderAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "TokenProviderAuthInfoType",
            &["token-provider"],
            None,
        )?;
        Ok(Self::TokenProvider)
    }
}

/// Authentication host (always the public GitHub host).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CopilotApiTokenAuthInfoHost {
    #[serde(rename = "https://github.com")]
    #[default]
    HttpsGitHubCom,
}
impl<'de> Deserialize<'de> for CopilotApiTokenAuthInfoHost {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CopilotApiTokenAuthInfoHost",
            &["https://github.com"],
            None,
        )?;
        Ok(Self::HttpsGitHubCom)
    }
}

/// Direct Copilot API authentication via the `GITHUB_COPILOT_API_TOKEN` + `COPILOT_API_URL` environment-variable pair. The token itself is read from the environment by the runtime, not carried in this struct.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CopilotApiTokenAuthInfoType {
    #[serde(rename = "copilot-api-token")]
    #[default]
    CopilotApiToken,
}
impl<'de> Deserialize<'de> for CopilotApiTokenAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CopilotApiTokenAuthInfoType",
            &["copilot-api-token"],
            None,
        )?;
        Ok(Self::CopilotApiToken)
    }
}

/// OAuth user authentication. The token itself is held in the runtime's secret token store (keyed by host+login) and is NOT carried in this struct.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserAuthInfoType {
    #[serde(rename = "user")]
    #[default]
    User,
}
impl<'de> Deserialize<'de> for UserAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(deserializer, "UserAuthInfoType", &["user"], None)?;
        Ok(Self::User)
    }
}

/// Provider-owned account authentication.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AccountAuthInfoType {
    #[serde(rename = "account")]
    #[default]
    Account,
}
impl<'de> Deserialize<'de> for AccountAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AccountAuthInfoType",
            &["account"],
            None,
        )?;
        Ok(Self::Account)
    }
}

/// Authentication via the `gh` CLI's saved credentials.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum GhCliAuthInfoType {
    #[serde(rename = "gh-cli")]
    #[default]
    GhCli,
}
impl<'de> Deserialize<'de> for GhCliAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "GhCliAuthInfoType",
            &["gh-cli"],
            None,
        )?;
        Ok(Self::GhCli)
    }
}

/// API-key authentication for non-GitHub LLM providers (e.g. when running BYOM-style).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ApiKeyAuthInfoType {
    #[serde(rename = "api-key")]
    #[default]
    ApiKey,
}
impl<'de> Deserialize<'de> for ApiKeyAuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ApiKeyAuthInfoType",
            &["api-key"],
            None,
        )?;
        Ok(Self::ApiKey)
    }
}

/// Authentication credentials accepted only at native protocol ingress. Runtime outputs use credential-free `AuthIdentity` metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AuthInfo {
    Hmac(HMACAuthInfo),
    Env(EnvAuthInfo),
    Token(TokenAuthInfo),
    TokenProvider(TokenProviderAuthInfo),
    CopilotApiToken(CopilotApiTokenAuthInfo),
    User(UserAuthInfo),
    Account(AccountAuthInfo),
    GhCli(GhCliAuthInfo),
    ApiKey(ApiKeyAuthInfo),
}

/// Authentication type
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthInfoType {
    /// Authentication provided by a GitHub App HMAC credential.
    #[serde(rename = "hmac")]
    Hmac,
    /// Authentication resolved from environment-provided credentials.
    #[serde(rename = "env")]
    Env,
    /// Authentication from an interactive user sign-in.
    #[serde(rename = "user")]
    User,
    /// Authentication from a selected provider-owned account, without a GitHub credential.
    #[serde(rename = "account")]
    Account,
    /// Authentication delegated to the GitHub CLI.
    #[serde(rename = "gh-cli")]
    GhCli,
    /// Authentication from an API key credential.
    #[serde(rename = "api-key")]
    ApiKey,
    /// Authentication from a GitHub token.
    #[serde(rename = "token")]
    Token,
    /// Authentication from an SDK GitHub token callback.
    #[serde(rename = "token-provider")]
    TokenProvider,
    /// Authentication from a Copilot API token.
    #[serde(rename = "copilot-api-token")]
    CopilotApiToken,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AuthInfoType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthInfoType",
            &[
                "hmac",
                "env",
                "user",
                "account",
                "gh-cli",
                "api-key",
                "token",
                "token-provider",
                "copilot-api-token",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::Hmac,
            1 => Self::Env,
            2 => Self::User,
            3 => Self::Account,
            4 => Self::GhCli,
            5 => Self::ApiKey,
            6 => Self::Token,
            7 => Self::TokenProvider,
            8 => Self::CopilotApiToken,
            _ => Self::Unknown,
        })
    }
}

/// Resolved Anthropic adaptive-thinking capability for a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AdaptiveThinkingSupport {
    /// The model does not accept thinking.type='adaptive'
    #[serde(rename = "unsupported")]
    Unsupported,
    /// The model accepts adaptive thinking but also accepts thinking.type='enabled'
    #[serde(rename = "optional")]
    Optional,
    /// The model defaults to adaptive thinking and rejects thinking.type='enabled' with HTTP 400, but still accepts thinking.type='disabled' (e.g. opus-4.7/4.8/5, sonnet-5)
    #[serde(rename = "required")]
    Required,
    /// The model accepts only thinking.type='adaptive'; 'enabled', 'disabled', and an omitted thinking block all fail with HTTP 400 (e.g. fable, mythos)
    #[serde(rename = "adaptive_only")]
    AdaptiveOnly,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AdaptiveThinkingSupport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AdaptiveThinkingSupport",
            &[
                "unsupported",
                "optional",
                "required",
                "adaptive_only",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Unsupported,
            1 => Self::Optional,
            2 => Self::Required,
            3 => Self::AdaptiveOnly,
            _ => Self::Unknown,
        })
    }
}

/// Where the agent definition was loaded from
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentInfoSource {
    /// Agent loaded from the user's personal agent configuration.
    #[serde(rename = "user")]
    User,
    /// Agent loaded from the current project's repository configuration.
    #[serde(rename = "project")]
    Project,
    /// Agent inherited from a parent project or workspace.
    #[serde(rename = "inherited")]
    Inherited,
    /// Agent provided by a remote runtime or service.
    #[serde(rename = "remote")]
    Remote,
    /// Agent contributed by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Agent built into the Copilot runtime.
    #[serde(rename = "builtin")]
    Builtin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentInfoSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentInfoSource",
            &[
                "user",
                "project",
                "inherited",
                "remote",
                "plugin",
                "builtin",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Project,
            2 => Self::Inherited,
            3 => Self::Remote,
            4 => Self::Plugin,
            5 => Self::Builtin,
            _ => Self::Unknown,
        })
    }
}

/// The request-local callback's admission boundary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum AhpSessionAdmissionPhase {
    /// Recheck the original guard after runtime-owned deferred work.
    #[serde(rename = "check")]
    Check,
    /// Acknowledge admission immediately before the runtime commits the host write.
    #[serde(rename = "admit")]
    Admit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AhpSessionAdmissionPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AhpSessionAdmissionPhase",
            &["check", "admit", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Check,
            1 => Self::Admit,
            _ => Self::Unknown,
        })
    }
}

/// The product serving the model, reported in telemetry as `model_provider`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigModelProvider {
    /// OpenAI API.
    #[serde(rename = "openai")]
    Openai,
    /// Anthropic API.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Azure OpenAI Service.
    #[serde(rename = "azure_openai")]
    AzureOpenai,
    /// Ollama.
    #[serde(rename = "ollama")]
    Ollama,
    /// LM Studio.
    #[serde(rename = "lm_studio")]
    LmStudio,
    /// Foundry Local.
    #[serde(rename = "foundry_local")]
    FoundryLocal,
    /// llama.cpp server.
    #[serde(rename = "llama_cpp")]
    LlamaCpp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigModelProvider {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigModelProvider",
            &[
                "openai",
                "anthropic",
                "azure_openai",
                "ollama",
                "lm_studio",
                "foundry_local",
                "llama_cpp",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Anthropic,
            2 => Self::AzureOpenai,
            3 => Self::Ollama,
            4 => Self::LmStudio,
            5 => Self::FoundryLocal,
            6 => Self::LlamaCpp,
            _ => Self::Unknown,
        })
    }
}

/// Provider transport. Defaults to "http".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigTransport {
    /// HTTP request/streaming transport.
    #[serde(rename = "http")]
    Http,
    /// WebSocket transport.
    #[serde(rename = "websockets")]
    Websockets,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigTransport",
            &["http", "websockets", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websockets,
            _ => Self::Unknown,
        })
    }
}

/// Provider families supported by saved configuration, including managed local models.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SavedProviderConfigurationType {
    /// Generic OpenAI-compatible API.
    #[serde(rename = "openai")]
    Openai,
    /// Azure OpenAI Service endpoint.
    #[serde(rename = "azure")]
    Azure,
    /// Anthropic API endpoint.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Experimental: Google Gemini API endpoint, served over the native Gemini Interactions API.
    #[serde(rename = "google")]
    Google,
    /// Runtime-managed local model server.
    #[serde(rename = "local_model")]
    LocalModel,
    /// Windows ML local model provider.
    #[serde(rename = "winml")]
    Winml,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SavedProviderConfigurationType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SavedProviderConfigurationType",
            &[
                "openai",
                "azure",
                "anthropic",
                "google",
                "local_model",
                "winml",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Azure,
            2 => Self::Anthropic,
            3 => Self::Google,
            4 => Self::LocalModel,
            5 => Self::Winml,
            _ => Self::Unknown,
        })
    }
}

/// Wire API format (openai/azure only). Defaults to "completions".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigWireApi {
    /// OpenAI Chat Completions wire format.
    #[serde(rename = "completions")]
    Completions,
    /// OpenAI Responses API wire format.
    #[serde(rename = "responses")]
    Responses,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigWireApi {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigWireApi",
            &["completions", "responses", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Completions,
            1 => Self::Responses,
            _ => Self::Unknown,
        })
    }
}

/// A provider a consumer may interactively sign in with.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LoginProviderKind {
    /// OAuth github.com sign-in, in the browser (web loopback + PKCE) or by device code — see the login method.
    #[serde(rename = "gitHub")]
    GitHub,
    /// A GitHub Enterprise Cloud account on a non-github.com host, e.g. *.ghe.com; the host is supplied interactively through the input-required step.
    #[serde(rename = "gitHubEnterpriseCloud")]
    GitHubEnterpriseCloud,
    /// Microsoft Entra sign-in that derives service accounts from a base identity.
    #[serde(rename = "entra")]
    Entra,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LoginProviderKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LoginProviderKind",
            &["gitHub", "gitHubEnterpriseCloud", "entra", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::GitHub,
            1 => Self::GitHubEnterpriseCloud,
            2 => Self::Entra,
            _ => Self::Unknown,
        })
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepOpenUrlKind {
    #[serde(rename = "open-url")]
    #[default]
    OpenUrl,
}
impl<'de> Deserialize<'de> for AuthLoginStepOpenUrlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepOpenUrlKind",
            &["open-url"],
            None,
        )?;
        Ok(Self::OpenUrl)
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepInputRequiredKind {
    #[serde(rename = "input-required")]
    #[default]
    InputRequired,
}
impl<'de> Deserialize<'de> for AuthLoginStepInputRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepInputRequiredKind",
            &["input-required"],
            None,
        )?;
        Ok(Self::InputRequired)
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepAwaitingKind {
    #[serde(rename = "awaiting")]
    #[default]
    Awaiting,
}
impl<'de> Deserialize<'de> for AuthLoginStepAwaitingKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepAwaitingKind",
            &["awaiting"],
            None,
        )?;
        Ok(Self::Awaiting)
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepNeedsInteractionKind {
    #[serde(rename = "needs-interaction")]
    #[default]
    NeedsInteraction,
}
impl<'de> Deserialize<'de> for AuthLoginStepNeedsInteractionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepNeedsInteractionKind",
            &["needs-interaction"],
            None,
        )?;
        Ok(Self::NeedsInteraction)
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepCompletedKind {
    #[serde(rename = "completed")]
    #[default]
    Completed,
}
impl<'de> Deserialize<'de> for AuthLoginStepCompletedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepCompletedKind",
            &["completed"],
            None,
        )?;
        Ok(Self::Completed)
    }
}

/// Disposition of a login attempt, including pending user decisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginResultStatus {
    /// The credential was persisted and the selected account is signed in.
    #[serde(rename = "completed")]
    Completed,
    /// Persistence needs explicit consent to store the token in plaintext.
    #[serde(rename = "needs-plaintext-consent")]
    NeedsPlaintextConsent,
    /// Credentials are saved; select an account using a returned selectionId as advance input to complete sign-in.
    #[serde(rename = "needs-account-selection")]
    NeedsAccountSelection,
    /// The user declined plaintext persistence.
    #[serde(rename = "declined")]
    Declined,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AuthLoginResultStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginResultStatus",
            &[
                "completed",
                "needs-plaintext-consent",
                "needs-account-selection",
                "declined",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Completed,
            1 => Self::NeedsPlaintextConsent,
            2 => Self::NeedsAccountSelection,
            3 => Self::Declined,
            _ => Self::Unknown,
        })
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepErrorKind {
    #[serde(rename = "error")]
    #[default]
    Error,
}
impl<'de> Deserialize<'de> for AuthLoginStepErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepErrorKind",
            &["error"],
            None,
        )?;
        Ok(Self::Error)
    }
}

/// Login flow step variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AuthLoginStepDeviceCodeKind {
    #[serde(rename = "device-code")]
    #[default]
    DeviceCode,
}
impl<'de> Deserialize<'de> for AuthLoginStepDeviceCodeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AuthLoginStepDeviceCodeKind",
            &["device-code"],
            None,
        )?;
        Ok(Self::DeviceCode)
    }
}

/// One step in an interactive login flow. The consumer acts on the step and calls advance to proceed. Browser-open is encoded as two distinct steps by design: `open-url` is CONSUMER-driven (the provider surfaces the authorize URL and the consumer opens it — github.com/GHEC web), while `needs-interaction` is PROVIDER-driven (the provider opens the browser or broker UI itself and does not surface a URL — Entra). A device-code flow surfaces its one-time code as `device-code`; the consumer shows the code, may open the verification URI, and calls advance, which resolves once the user has authorized the device.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AuthLoginStep {
    OpenUrl(AuthLoginStepOpenUrl),
    InputRequired(AuthLoginStepInputRequired),
    Awaiting(AuthLoginStepAwaiting),
    NeedsInteraction(AuthLoginStepNeedsInteraction),
    Completed(AuthLoginStepCompleted),
    Error(AuthLoginStepError),
    DeviceCode(AuthLoginStepDeviceCode),
}

/// Outcome of applying the session-idle Autopilot continuation policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutopilotEvaluateIdleContinuationOutcome {
    /// The policy admitted and scheduled another Autopilot turn.
    #[serde(rename = "continued")]
    Continued,
    /// No Autopilot continuation was scheduled after the policy was applied.
    #[serde(rename = "stopped")]
    Stopped,
    /// The session was not eligible for an Autopilot continuation.
    #[serde(rename = "notApplicable")]
    NotApplicable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutopilotEvaluateIdleContinuationOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutopilotEvaluateIdleContinuationOutcome",
            &["continued", "stopped", "notApplicable", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Continued,
            1 => Self::Stopped,
            2 => Self::NotApplicable,
            _ => Self::Unknown,
        })
    }
}

/// Current normalized autopilot objective lifecycle status.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutopilotObjectiveStatus {
    /// The objective is actively running.
    #[serde(rename = "active")]
    Active,
    /// The objective is paused and may be resumed.
    #[serde(rename = "paused")]
    Paused,
    /// The objective completed.
    #[serde(rename = "completed")]
    Completed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutopilotObjectiveStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutopilotObjectiveStatus",
            &["active", "paused", "completed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Active,
            1 => Self::Paused,
            2 => Self::Completed,
            _ => Self::Unknown,
        })
    }
}

/// Provider type. Defaults to "openai" for generic OpenAI-compatible APIs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigType {
    /// Generic OpenAI-compatible API.
    #[serde(rename = "openai")]
    Openai,
    /// Azure OpenAI Service endpoint.
    #[serde(rename = "azure")]
    Azure,
    /// Anthropic API endpoint.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Experimental: Google Gemini API endpoint, served over the native Gemini Interactions API.
    #[serde(rename = "google")]
    Google,
    /// On-device Windows ML model with a local GGUF path.
    #[serde(rename = "winml")]
    Winml,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigType",
            &["openai", "azure", "anthropic", "google", "winml", "Unknown"],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Azure,
            2 => Self::Anthropic,
            3 => Self::Google,
            4 => Self::Winml,
            _ => Self::Unknown,
        })
    }
}

/// Copilot Fusion tier, matching the CLI's `--fusion-tier`. The runtime resolves each tier to the Auto routing preference it stands for, using the same runtime-owned bindings as the CLI; reads report that preference as `autoTier`, and `session.model.getCurrent` and Auto switch results also report the resolved tier once the session's Auto tier metadata is known.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionTier {
    /// Copilot Fusion Standard, which balances capability, speed, and cost.
    #[serde(rename = "standard")]
    Standard,
    /// Copilot Fusion High.
    #[serde(rename = "high")]
    High,
    /// Copilot Fusion Max, which prioritizes quality.
    #[serde(rename = "max")]
    Max,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionTier",
            &["standard", "high", "max", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Standard,
            1 => Self::High,
            2 => Self::Max,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the caller is not authenticated
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAuthenticationRequiredErrorKind {
    #[serde(rename = "authentication-required")]
    #[default]
    AuthenticationRequired,
}
impl<'de> Deserialize<'de> for CatalogAuthenticationRequiredErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAuthenticationRequiredErrorKind",
            &["authentication-required"],
            None,
        )?;
        Ok(Self::AuthenticationRequired)
    }
}

/// Why the catalog authority did not accept the caller's identity
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAuthenticationRequiredReason {
    /// No credential was presented, so there is nothing to refresh and the caller must sign in.
    #[serde(rename = "no-credential")]
    NoCredential,
    /// A credential was presented and its lifetime has elapsed. A silent refresh is worth attempting before prompting anyone.
    #[serde(rename = "credential-expired")]
    CredentialExpired,
    /// A credential was presented and the authority refused it, for example because it was revoked, malformed, or issued for another audience. Refreshing the same rejected credential is not useful; the caller must sign in again.
    #[serde(rename = "credential-rejected")]
    CredentialRejected,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAuthenticationRequiredReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAuthenticationRequiredReason",
            &[
                "no-credential",
                "credential-expired",
                "credential-rejected",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::NoCredential,
            1 => Self::CredentialExpired,
            2 => Self::CredentialRejected,
            _ => Self::Unknown,
        })
    }
}

/// JSON MCP card media type accepted for install planning
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerCardMediaType {
    /// The current MCP server card media type.
    #[serde(rename = "application/mcp-server-card+json")]
    ApplicationMcpServerCardJson,
    /// The legacy MCP server card media type, accepted for compatibility.
    #[serde(rename = "application/mcp-server+json")]
    ApplicationMcpServerJson,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerCardMediaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerCardMediaType",
            &[
                "application/mcp-server-card+json",
                "application/mcp-server+json",
                "Unknown",
            ],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::ApplicationMcpServerCardJson,
            1 => Self::ApplicationMcpServerJson,
            _ => Self::Unknown,
        })
    }
}

/// A wire feature a caller can require of the catalog surface, negotiated per request. A grant means the runtime understands the feature's contract, not that the deployment has enabled the operation; typed unavailable results report availability separately.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogCapability {
    /// Understands the current `application/mcp-server-card+json` media type.
    #[serde(rename = "mcp-server-card")]
    McpServerCard,
    /// Understands the legacy `application/mcp-server+json` media type.
    #[serde(rename = "legacy-mcp-server-card")]
    LegacyMcpServerCard,
    /// Understands `application/ai-skill` candidates as discovery-only and typed non-installable.
    #[serde(rename = "ai-skill-discovery")]
    AiSkillDiscovery,
    /// Understands opt-in `application/vnd.github.copilot-plugin` candidates and their typed identity, version, source, and compatibility fields.
    #[serde(rename = "agent-plugin-discovery")]
    AgentPluginDiscovery,
    /// Extends agent-plugin-discovery to Claude plugin (`application/vnd.anthropic.claude-plugin+json`) and portable Agent Plugin (`application/agent-plugin+json`) candidates, each reporting its format as its media type. Requires agent-plugin-discovery; without it, plugin searches return only Copilot plugins.
    #[serde(rename = "agent-plugin-formats")]
    AgentPluginFormats,
    /// Understands side-effect-free MCP install-plan requests, results, and plan handles; `planning-unavailable` separately reports that planning is not enabled.
    #[serde(rename = "mcp-install-planning")]
    McpInstallPlanning,
    /// Understands plans that enumerate every eligible transport rather than a single preferred one.
    #[serde(rename = "multiple-transport-choice")]
    MultipleTransportChoice,
    /// Understands explicit numbered navigation and authority-reported pagination metadata with opaque tokens. Advertised and granted only when requested.
    #[serde(rename = "catalog-search-pagination")]
    CatalogSearchPagination,
    /// Understands versioned candidate trust snapshots. Protocol-3 callers must require this capability before the runtime adds the optional snapshot field.
    #[serde(rename = "trust-snapshot")]
    TrustSnapshot,
    /// Understands exact candidate selection through model-safe opaque references and host-only candidate-handle hand-off.
    #[serde(rename = "catalog-selection")]
    CatalogSelection,
    /// Requires an eligible credential for the selected GitHub.com account before search egress and prohibits client-side anonymous retry, including after HTTP 401 or 403. The credential is scoped to the fixed catalog authority without redirect forwarding. Neither a grant nor successful response proves that the authority accepted the identity or selected a particular backend. Preserve this requirement on every page and retry; callers omitting it retain optional authentication.
    #[serde(rename = "catalog-search-credential-required")]
    CatalogSearchCredentialRequired,
    /// Captures the exact existing native session, account, host and connection for authenticated catalogue search, selection and planning. Requires catalog-search-credential-required; does not grant installation or create a session.
    #[serde(rename = "catalog-search-session-bound")]
    CatalogSearchSessionBound,
    /// Understands initial-page official-product recommendations for a full task and product with fixed pageSize 10. Requires session-bound, credential-required catalogue support. No anonymous retry, broad-search fallback or continuation. Authentication uses the selected GitHub.com user credential; successful capability negotiation does not establish service acceptance.
    #[serde(rename = "catalog-recommend")]
    CatalogRecommend,
    /// Understands effect-free preparation, exact human-confirmed apply and owned removal for fully resolved personal remote MCP choices without supplied inputs or configured secrets. Advertised only when the real producer and lower owned admission are linked; requires original connection and bound session authority for new work.
    #[serde(rename = "mcp-confirmed-remote-installation")]
    McpConfirmedRemoteInstallation,
    /// Extends mcp-confirmed-remote-installation to declared non-secret header and URL values and receipt-owned header secrets for personal remote MCP choices. Requires mcp-confirmed-remote-installation and bound session authority. Secret values are written only to the reviewed backend after confirmation and are never returned; package and stdio choices remain unsupported. Advertised only when owned secret effects and owned secret activation are linked.
    #[serde(rename = "mcp-configured-remote-installation")]
    McpConfiguredRemoteInstallation,
    /// Understands verified Agent Finder Skill install, uninstall, recovery and installation-scoped enablement APIs. Advertised only by runtimes with the Skill installation engine linked; acquisition may still be refused as feature-disabled per selected session.
    #[serde(rename = "skill-confirmed-installation")]
    SkillConfirmedInstallation,
    /// Understands `installability`, the reviewable `installPlan` and the optional materialisation `handle` on Agent Plugin candidates, and the verified Agent Plugin installation APIs plugins.planInstall and plugins.applyInstall, and Agent Plugin plans from catalog.planInstallFromReference. Requires agent-plugin-discovery; without it, plugin candidates are unchanged. Advertised only by runtimes with the plugin installer linked; acquisition may still be refused as feature-disabled per selected session.
    #[serde(rename = "agent-plugin-confirmed-installation")]
    AgentPluginConfirmedInstallation,
    /// Understands catalog.planInstallFromReference, which plans installing whatever resource an Agent Finder materialisation reference pins. It plans only kinds whose installation capability the caller also requires: skill-confirmed-installation for Skills, agent-plugin-confirmed-installation for Agent Plugins. Advertised only by runtimes with the Skill installation engine linked.
    #[serde(rename = "catalog-reference-installation")]
    CatalogReferenceInstallation,
    /// Understands catalog.getSavedFeed and catalog.getGeneratedFeed, which retrieve a saved or generated feed with entries projected exactly like catalog.search candidates. Requires catalog-search-credential-required; add catalog-search-session-bound to bind the retrieval to a selected session.
    #[serde(rename = "catalog-feeds")]
    CatalogFeeds,
    /// Understands the optional `categories` filter on catalog.search, which the runtime forwards to the catalog authority. Required to send the filter, so an older runtime refuses the request instead of silently returning unfiltered results.
    #[serde(rename = "catalog-search-categories")]
    CatalogSearchCategories,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogCapability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogCapability",
            &[
                "mcp-server-card",
                "legacy-mcp-server-card",
                "ai-skill-discovery",
                "agent-plugin-discovery",
                "agent-plugin-formats",
                "mcp-install-planning",
                "multiple-transport-choice",
                "catalog-search-pagination",
                "trust-snapshot",
                "catalog-selection",
                "catalog-search-credential-required",
                "catalog-search-session-bound",
                "catalog-recommend",
                "mcp-confirmed-remote-installation",
                "mcp-configured-remote-installation",
                "skill-confirmed-installation",
                "agent-plugin-confirmed-installation",
                "catalog-reference-installation",
                "catalog-feeds",
                "catalog-search-categories",
                "Unknown",
            ],
            Some(20),
        )?;
        Ok(match index {
            0 => Self::McpServerCard,
            1 => Self::LegacyMcpServerCard,
            2 => Self::AiSkillDiscovery,
            3 => Self::AgentPluginDiscovery,
            4 => Self::AgentPluginFormats,
            5 => Self::McpInstallPlanning,
            6 => Self::MultipleTransportChoice,
            7 => Self::CatalogSearchPagination,
            8 => Self::TrustSnapshot,
            9 => Self::CatalogSelection,
            10 => Self::CatalogSearchCredentialRequired,
            11 => Self::CatalogSearchSessionBound,
            12 => Self::CatalogRecommend,
            13 => Self::McpConfirmedRemoteInstallation,
            14 => Self::McpConfiguredRemoteInstallation,
            15 => Self::SkillConfirmedInstallation,
            16 => Self::AgentPluginConfirmedInstallation,
            17 => Self::CatalogReferenceInstallation,
            18 => Self::CatalogFeeds,
            19 => Self::CatalogSearchCategories,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the upstream response broke the contract
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogContractViolationErrorKind {
    #[serde(rename = "contract-violation")]
    #[default]
    ContractViolation,
}
impl<'de> Deserialize<'de> for CatalogContractViolationErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogContractViolationErrorKind",
            &["contract-violation"],
            None,
        )?;
        Ok(Self::ContractViolation)
    }
}

/// Which wire-contract rule an upstream response broke
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogContractViolationReason {
    /// A result carried both a URL and embedded data, when exactly one is permitted.
    #[serde(rename = "both-url-and-data")]
    BothUrlAndData,
    /// A result carried neither a URL nor embedded data, when exactly one is required.
    #[serde(rename = "neither-url-nor-data")]
    NeitherUrlNorData,
    /// Two results claimed the same collision key: normalised identity for existing kinds, or normalised identity and declared version for Agent Plugins.
    #[serde(rename = "duplicate-identity")]
    DuplicateIdentity,
    /// A result declared no media type, or one this contract does not model.
    #[serde(rename = "unknown-media-type")]
    UnknownMediaType,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogContractViolationReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogContractViolationReason",
            &[
                "both-url-and-data",
                "neither-url-nor-data",
                "duplicate-identity",
                "unknown-media-type",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::BothUrlAndData,
            1 => Self::NeitherUrlNorData,
            2 => Self::DuplicateIdentity,
            3 => Self::UnknownMediaType,
            _ => Self::Unknown,
        })
    }
}

/// Which request field was rejected locally or by the catalog authority
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogInvalidRequestField {
    /// The selected existing attached session was missing, malformed or unavailable.
    #[serde(rename = "policySessionId")]
    PolicySessionId,
    /// The search query was empty or longer than permitted.
    #[serde(rename = "query")]
    Query,
    /// The requested recommendation product was missing, blank or longer than permitted.
    #[serde(rename = "product")]
    Product,
    /// The requested result count fell outside its permitted range.
    #[serde(rename = "limit")]
    Limit,
    /// The requested candidate kinds were empty or contained a duplicate.
    #[serde(rename = "kinds")]
    Kinds,
    /// The negotiation block was missing or malformed.
    #[serde(rename = "contract")]
    Contract,
    /// The plan source was missing or malformed.
    #[serde(rename = "source")]
    Source,
    /// The supplied card was missing its media type, URL, or data.
    #[serde(rename = "card")]
    Card,
    /// The requested configuration scope is not one this runtime writes.
    #[serde(rename = "scope")]
    Scope,
    /// The pagination token or target was invalid, or the authority rejected the continuation. Repeat the search without page.
    #[serde(rename = "page")]
    Page,
    /// The locally owned session identifier was missing or malformed.
    #[serde(rename = "sessionId")]
    SessionId,
    /// The opaque selection reference was missing or malformed.
    #[serde(rename = "selectionRef")]
    SelectionRef,
    /// The terminal selection outcome was missing or unsupported.
    #[serde(rename = "outcome")]
    Outcome,
    /// The saved feed ID, or the generated feed strategy or key, was missing or malformed.
    #[serde(rename = "feed")]
    Feed,
    /// The trust tier filter was empty, repeated a tier or named an unknown tier.
    #[serde(rename = "trustTiers")]
    TrustTiers,
    /// The category filter was empty, too long, repeated a category or contained a value that is not a lowercase slug.
    #[serde(rename = "categories")]
    Categories,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogInvalidRequestField {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogInvalidRequestField",
            &[
                "policySessionId",
                "query",
                "product",
                "limit",
                "kinds",
                "contract",
                "source",
                "card",
                "scope",
                "page",
                "sessionId",
                "selectionRef",
                "outcome",
                "feed",
                "trustTiers",
                "categories",
                "Unknown",
            ],
            Some(16),
        )?;
        Ok(match index {
            0 => Self::PolicySessionId,
            1 => Self::Query,
            2 => Self::Product,
            3 => Self::Limit,
            4 => Self::Kinds,
            5 => Self::Contract,
            6 => Self::Source,
            7 => Self::Card,
            8 => Self::Scope,
            9 => Self::Page,
            10 => Self::SessionId,
            11 => Self::SelectionRef,
            12 => Self::Outcome,
            13 => Self::Feed,
            14 => Self::TrustTiers,
            15 => Self::Categories,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the request itself was invalid
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogInvalidRequestErrorKind {
    #[serde(rename = "invalid-request")]
    #[default]
    InvalidRequest,
}
impl<'de> Deserialize<'de> for CatalogInvalidRequestErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogInvalidRequestErrorKind",
            &["invalid-request"],
            None,
        )?;
        Ok(Self::InvalidRequest)
    }
}

/// Discriminator: the card was malformed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogMalformedCardErrorKind {
    #[serde(rename = "malformed-card")]
    #[default]
    MalformedCard,
}
impl<'de> Deserialize<'de> for CatalogMalformedCardErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogMalformedCardErrorKind",
            &["malformed-card"],
            None,
        )?;
        Ok(Self::MalformedCard)
    }
}

/// Media type a catalog card is interpreted as
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogMediaType {
    /// The current MCP server card media type.
    #[serde(rename = "application/mcp-server-card+json")]
    ApplicationMcpServerCardJson,
    /// The legacy MCP server card media type, accepted for compatibility.
    #[serde(rename = "application/mcp-server+json")]
    ApplicationMcpServerJson,
    /// An AI skill card. Representable and searchable, but typed non-installable.
    #[serde(rename = "application/ai-skill")]
    ApplicationAiSkill,
    /// An inert Agent Plugin descriptor.
    #[serde(rename = "application/vnd.github.copilot-plugin")]
    ApplicationVndGitHubCopilotPlugin,
    /// An inert Claude plugin descriptor, surfaced only under the agent-plugin-formats capability.
    #[serde(rename = "application/vnd.anthropic.claude-plugin+json")]
    ApplicationVndAnthropicClaudePluginJson,
    /// An inert portable Agent Plugin descriptor, surfaced only under the agent-plugin-formats capability.
    #[serde(rename = "application/agent-plugin+json")]
    ApplicationAgentPluginJson,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogMediaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogMediaType",
            &[
                "application/mcp-server-card+json",
                "application/mcp-server+json",
                "application/ai-skill",
                "application/vnd.github.copilot-plugin",
                "application/vnd.anthropic.claude-plugin+json",
                "application/agent-plugin+json",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::ApplicationMcpServerCardJson,
            1 => Self::ApplicationMcpServerJson,
            2 => Self::ApplicationAiSkill,
            3 => Self::ApplicationVndGitHubCopilotPlugin,
            4 => Self::ApplicationVndAnthropicClaudePluginJson,
            5 => Self::ApplicationAgentPluginJson,
            _ => Self::Unknown,
        })
    }
}

/// How a card failed validation
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogMalformedCardReason {
    /// The document is not well-formed JSON.
    #[serde(rename = "invalid-json")]
    InvalidJson,
    /// The document does not satisfy its media type's schema.
    #[serde(rename = "schema-violation")]
    SchemaViolation,
    /// The declared media type is not one this runtime understands.
    #[serde(rename = "unsupported-media-type")]
    UnsupportedMediaType,
    /// A field the media type requires is absent.
    #[serde(rename = "missing-required-field")]
    MissingRequiredField,
    /// The document exceeded the permitted size.
    #[serde(rename = "size-limit-exceeded")]
    SizeLimitExceeded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogMalformedCardReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogMalformedCardReason",
            &[
                "invalid-json",
                "schema-violation",
                "unsupported-media-type",
                "missing-required-field",
                "size-limit-exceeded",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::InvalidJson,
            1 => Self::SchemaViolation,
            2 => Self::UnsupportedMediaType,
            3 => Self::MissingRequiredField,
            4 => Self::SizeLimitExceeded,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: capability or protocol-version negotiation failed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNegotiationRefusedErrorKind {
    #[serde(rename = "negotiation-refused")]
    #[default]
    NegotiationRefused,
}
impl<'de> Deserialize<'de> for CatalogNegotiationRefusedErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNegotiationRefusedErrorKind",
            &["negotiation-refused"],
            None,
        )?;
        Ok(Self::NegotiationRefused)
    }
}

/// Why capability and protocol-version negotiation refused a caller
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNegotiationRefusedReason {
    /// The caller's protocol version is below the lowest this runtime serves.
    #[serde(rename = "unsupported-protocol-version")]
    UnsupportedProtocolVersion,
    /// The caller requires at least one capability this runtime cannot honour.
    #[serde(rename = "unsupported-capability")]
    UnsupportedCapability,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogNegotiationRefusedReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNegotiationRefusedReason",
            &[
                "unsupported-protocol-version",
                "unsupported-capability",
                "Unknown",
            ],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::UnsupportedProtocolVersion,
            1 => Self::UnsupportedCapability,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the network operation failed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNetworkFailureErrorKind {
    #[serde(rename = "network-failure")]
    #[default]
    NetworkFailure,
}
impl<'de> Deserialize<'de> for CatalogNetworkFailureErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNetworkFailureErrorKind",
            &["network-failure"],
            None,
        )?;
        Ok(Self::NetworkFailure)
    }
}

/// Categorised network failure, low cardinality so it can be aggregated without carrying a URL
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNetworkFailureReason {
    /// No network is available, so nothing was attempted.
    #[serde(rename = "offline")]
    Offline,
    /// The authority's name could not be resolved.
    #[serde(rename = "dns")]
    Dns,
    /// The request exceeded its time budget.
    #[serde(rename = "timeout")]
    Timeout,
    /// The TLS handshake or certificate validation failed.
    #[serde(rename = "tls")]
    Tls,
    /// The connection was refused or reset.
    #[serde(rename = "connection-refused")]
    ConnectionRefused,
    /// The configured proxy returned 407 and requires authentication.
    #[serde(rename = "proxy-authentication-required")]
    ProxyAuthenticationRequired,
    /// The authority rate-limited requests and supplied or implied a bounded cooldown.
    #[serde(rename = "rate-limited")]
    RateLimited,
    /// The authority returned a transient 5xx response.
    #[serde(rename = "service-unavailable")]
    ServiceUnavailable,
    /// The authority returned another status the runtime treats as a failure.
    #[serde(rename = "http-status")]
    HttpStatus,
    /// The response exceeded the permitted size.
    #[serde(rename = "response-too-large")]
    ResponseTooLarge,
    /// A redirect was refused by the runtime's redirect policy.
    #[serde(rename = "redirect-rejected")]
    RedirectRejected,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogNetworkFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNetworkFailureReason",
            &[
                "offline",
                "dns",
                "timeout",
                "tls",
                "connection-refused",
                "proxy-authentication-required",
                "rate-limited",
                "service-unavailable",
                "http-status",
                "response-too-large",
                "redirect-rejected",
                "Unknown",
            ],
            Some(11),
        )?;
        Ok(match index {
            0 => Self::Offline,
            1 => Self::Dns,
            2 => Self::Timeout,
            3 => Self::Tls,
            4 => Self::ConnectionRefused,
            5 => Self::ProxyAuthenticationRequired,
            6 => Self::RateLimited,
            7 => Self::ServiceUnavailable,
            8 => Self::HttpStatus,
            9 => Self::ResponseTooLarge,
            10 => Self::RedirectRejected,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: policy refused the operation
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogPolicyRejectedErrorKind {
    #[serde(rename = "policy-rejected")]
    #[default]
    PolicyRejected,
}
impl<'de> Deserialize<'de> for CatalogPolicyRejectedErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogPolicyRejectedErrorKind",
            &["policy-rejected"],
            None,
        )?;
        Ok(Self::PolicyRejected)
    }
}

/// Which authority produced a policy decision
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanPolicySource {
    /// No policy applied, so the server is permitted by default.
    #[serde(rename = "none")]
    None,
    /// An enterprise allowlist evaluated the server.
    #[serde(rename = "enterprise-allowlist")]
    EnterpriseAllowlist,
    /// The registry the card came from evaluated the server.
    #[serde(rename = "registry-policy")]
    RegistryPolicy,
    /// Local trust settings evaluated the server.
    #[serde(rename = "local-trust")]
    LocalTrust,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanPolicySource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanPolicySource",
            &[
                "none",
                "enterprise-allowlist",
                "registry-policy",
                "local-trust",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::EnterpriseAllowlist,
            2 => Self::RegistryPolicy,
            3 => Self::LocalTrust,
            _ => Self::Unknown,
        })
    }
}

/// Exact reviewed Skill installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationReviewInstallAction {
    #[serde(rename = "install")]
    #[default]
    Install,
}
impl<'de> Deserialize<'de> for SkillInstallationReviewInstallAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationReviewInstallAction",
            &["install"],
            None,
        )?;
        Ok(Self::Install)
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationScope {
    /// The user's personal Copilot home.
    #[serde(rename = "personal")]
    Personal,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallationScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationScope",
            &["personal", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Personal,
            _ => Self::Unknown,
        })
    }
}

/// Exact reviewed Skill installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationReviewUninstallAction {
    #[serde(rename = "uninstall")]
    #[default]
    Uninstall,
}
impl<'de> Deserialize<'de> for SkillInstallationReviewUninstallAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationReviewUninstallAction",
            &["uninstall"],
            None,
        )?;
        Ok(Self::Uninstall)
    }
}

/// Owned Skill state observed from files and receipts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOwnershipState {
    /// Owned files and receipt evidence match.
    #[serde(rename = "intact")]
    Intact,
    /// Owned files no longer match the receipt.
    #[serde(rename = "modified")]
    Modified,
    /// Ownership evidence requires recovery before mutation.
    #[serde(rename = "recovery-required")]
    RecoveryRequired,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallationOwnershipState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOwnershipState",
            &["intact", "modified", "recovery-required", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Intact,
            1 => Self::Modified,
            2 => Self::RecoveryRequired,
            _ => Self::Unknown,
        })
    }
}

/// Bound-session observation after reconciling persisted enablement.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationSessionState {
    /// The selected session has loaded this Skill and it is enabled.
    #[serde(rename = "loaded-enabled")]
    LoadedEnabled,
    /// The selected session has loaded this Skill or settings and it is disabled.
    #[serde(rename = "loaded-disabled")]
    LoadedDisabled,
    /// The selected session has not loaded Skills after the latest change.
    #[serde(rename = "not-loaded")]
    NotLoaded,
    /// The selected session could not be inspected.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallationSessionState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationSessionState",
            &[
                "loaded-enabled",
                "loaded-disabled",
                "not-loaded",
                "unknown",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::LoadedEnabled,
            1 => Self::LoadedDisabled,
            2 => Self::NotLoaded,
            3 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Safe verified Skill review fields. No raw credential, candidate handle or plan handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationReview {
    Install(SkillInstallationReviewInstall),
    Uninstall(SkillInstallationReviewUninstall),
}

/// Exact reviewed Agent Plugin installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallPlanReviewAction {
    #[serde(rename = "install")]
    #[default]
    Install,
}
impl<'de> Deserialize<'de> for PluginInstallPlanReviewAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallPlanReviewAction",
            &["install"],
            None,
        )?;
        Ok(Self::Install)
    }
}

/// How a reviewed plugin's MCP server is reached.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationMcpTransport {
    /// An HTTP or SSE server reached over the network, which may ask the user to sign in.
    #[serde(rename = "remote")]
    Remote,
    /// A command the plugin runs on this machine.
    #[serde(rename = "local")]
    Local,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PluginInstallationMcpTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationMcpTransport",
            &["remote", "local", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Remote,
            1 => Self::Local,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: retrieval was refused as unsafe
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnsafeRetrievalErrorKind {
    #[serde(rename = "unsafe-retrieval")]
    #[default]
    UnsafeRetrieval,
}
impl<'de> Deserialize<'de> for CatalogUnsafeRetrievalErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnsafeRetrievalErrorKind",
            &["unsafe-retrieval"],
            None,
        )?;
        Ok(Self::UnsafeRetrieval)
    }
}

/// Which hardened-fetch control refused a retrieval
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnsafeRetrievalReason {
    /// The URL used a scheme the runtime refuses to fetch.
    #[serde(rename = "blocked-scheme")]
    BlockedScheme,
    /// The URL embedded credentials.
    #[serde(rename = "credentials-in-url")]
    CredentialsInUrl,
    /// The URL resolved to a loopback, private, link-local, or cloud metadata address.
    #[serde(rename = "blocked-address")]
    BlockedAddress,
    /// A redirect target resolved to a blocked address.
    #[serde(rename = "redirect-to-blocked-address")]
    RedirectToBlockedAddress,
    /// The configured proxy policy refused the request.
    #[serde(rename = "proxy-rejected")]
    ProxyRejected,
    /// The authority is not permitted for card retrieval.
    #[serde(rename = "host-not-permitted")]
    HostNotPermitted,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogUnsafeRetrievalReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnsafeRetrievalReason",
            &[
                "blocked-scheme",
                "credentials-in-url",
                "blocked-address",
                "redirect-to-blocked-address",
                "proxy-rejected",
                "host-not-permitted",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::BlockedScheme,
            1 => Self::CredentialsInUrl,
            2 => Self::BlockedAddress,
            3 => Self::RedirectToBlockedAddress,
            4 => Self::ProxyRejected,
            5 => Self::HostNotPermitted,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the operation is not available
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnavailableErrorKind {
    #[serde(rename = "unavailable")]
    #[default]
    Unavailable,
}
impl<'de> Deserialize<'de> for CatalogUnavailableErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnavailableErrorKind",
            &["unavailable"],
            None,
        )?;
        Ok(Self::Unavailable)
    }
}

/// Why a catalog operation is not available on this runtime
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnavailableReason {
    /// Bounded search is not wired up on this runtime build.
    #[serde(rename = "search-unavailable")]
    SearchUnavailable,
    /// Install planning is not wired up on this runtime build.
    #[serde(rename = "planning-unavailable")]
    PlanningUnavailable,
    /// Exact candidate selection is not available in this session or runtime.
    #[serde(rename = "selection-unavailable")]
    SelectionUnavailable,
    /// No catalog authority is configured for this runtime.
    #[serde(rename = "authority-not-configured")]
    AuthorityNotConfigured,
    /// The surface is disabled by policy on this runtime.
    #[serde(rename = "disabled-by-policy")]
    DisabledByPolicy,
    /// The requested saved feed, or generated feed strategy or key, does not exist.
    #[serde(rename = "feed-not-found")]
    FeedNotFound,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogUnavailableReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnavailableReason",
            &[
                "search-unavailable",
                "planning-unavailable",
                "selection-unavailable",
                "authority-not-configured",
                "disabled-by-policy",
                "feed-not-found",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::SearchUnavailable,
            1 => Self::PlanningUnavailable,
            2 => Self::SelectionUnavailable,
            3 => Self::AuthorityNotConfigured,
            4 => Self::DisabledByPolicy,
            5 => Self::FeedNotFound,
            _ => Self::Unknown,
        })
    }
}

/// Relationship of the backend-reported count to the complete query result set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSearchTotalCountRelation {
    /// No exact/full-query or lower-bound guarantee is available.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogSearchTotalCountRelation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSearchTotalCountRelation",
            &["unknown", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Optional completion hint for the input (e.g. 'directory' for filesystem path completion)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandInputCompletion {
    /// Input should complete filesystem directories.
    #[serde(rename = "directory")]
    Directory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SlashCommandInputCompletion {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandInputCompletion",
            &["directory", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Directory,
            _ => Self::Unknown,
        })
    }
}

/// Coarse command category for grouping and behavior: runtime built-in, skill-backed command, or SDK/client-owned command
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandKind {
    /// Command implemented by the runtime.
    #[serde(rename = "builtin")]
    Builtin,
    /// Command backed by a skill.
    #[serde(rename = "skill")]
    Skill,
    /// Command registered by an SDK client or extension.
    #[serde(rename = "client")]
    Client,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SlashCommandKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandKind",
            &["builtin", "skill", "client", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Builtin,
            1 => Self::Skill,
            2 => Self::Client,
            _ => Self::Unknown,
        })
    }
}

/// Neutral SDK discriminator for the connected remote session kind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectedRemoteSessionMetadataKind {
    /// Remote CLI session.
    #[serde(rename = "remote-session")]
    RemoteSession,
    /// GitHub Copilot coding agent session.
    #[serde(rename = "coding-agent")]
    CodingAgent,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectedRemoteSessionMetadataKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectedRemoteSessionMetadataKind",
            &["remote-session", "coding-agent", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::RemoteSession,
            1 => Self::CodingAgent,
            _ => Self::Unknown,
        })
    }
}

/// Stable OAuth scope whose absence prevents Connector management.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorAuthorizationScope {
    /// Allows Copilot to manage Connector connections and use their tools.
    #[serde(rename = "write_plugin_gateway_connections")]
    WritePluginGatewayConnections,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectorAuthorizationScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorAuthorizationScope",
            &["write_plugin_gateway_connections", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::WritePluginGatewayConnections,
            _ => Self::Unknown,
        })
    }
}

/// Availability of the EXPERIMENTAL session connector API.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorAvailability {
    /// The resolved Connector feature is enabled and Connector requests are permitted.
    #[serde(rename = "enabled")]
    Enabled,
    /// The resolved Connector feature is off. No Connector service request is made while disabled.
    #[serde(rename = "disabled")]
    Disabled,
    /// The session has no eligible host-owned GitHub account or does not support local Connector projection.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectorAvailability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorAvailability",
            &["enabled", "disabled", "unavailable", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Enabled,
            1 => Self::Disabled,
            2 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Authoritative service connection state for one Connector.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorCatalogStatus {
    /// The Connector is available but not connected.
    #[serde(rename = "not_connected")]
    NotConnected,
    /// The Connector service is still completing connection or consent.
    #[serde(rename = "pending")]
    Pending,
    /// The Connector is connected and may contribute MCP servers.
    #[serde(rename = "connected")]
    Connected,
    /// The Connector service reports an unusable connection.
    #[serde(rename = "error")]
    Error,
    /// The service returned a future or unrecognized state.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectorCatalogStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorCatalogStatus",
            &[
                "not_connected",
                "pending",
                "connected",
                "error",
                "unknown",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::NotConnected,
            1 => Self::Pending,
            2 => Self::Connected,
            3 => Self::Error,
            4 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Live MCP status of one Connector-owned runtime server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorMcpStatus {
    /// The server is connected and its tools are available.
    #[serde(rename = "connected")]
    Connected,
    /// The server connection is still being established.
    #[serde(rename = "pending")]
    Pending,
    /// The server requires refreshed GitHub authorization.
    #[serde(rename = "needs_auth")]
    NeedsAuth,
    /// The server failed to connect or initialize.
    #[serde(rename = "failed")]
    Failed,
    /// The server is intentionally stopped, including when managed policy blocks it.
    #[serde(rename = "stopped")]
    Stopped,
    /// The server is configured but explicitly disabled.
    #[serde(rename = "disabled")]
    Disabled,
    /// The Connector currently has no live server configuration.
    #[serde(rename = "not_configured")]
    NotConfigured,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectorMcpStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorMcpStatus",
            &[
                "connected",
                "pending",
                "needs_auth",
                "failed",
                "stopped",
                "disabled",
                "not_configured",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Connected,
            1 => Self::Pending,
            2 => Self::NeedsAuth,
            3 => Self::Failed,
            4 => Self::Stopped,
            5 => Self::Disabled,
            6 => Self::NotConfigured,
            _ => Self::Unknown,
        })
    }
}

/// Source category for a collected debug bundle entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsSource {
    /// Session event log.
    #[serde(rename = "events")]
    Events,
    /// Process log for the session.
    #[serde(rename = "process-log")]
    ProcessLog,
    /// Interactive shell log for the session.
    #[serde(rename = "shell-log")]
    ShellLog,
    /// Caller-provided diagnostic entry.
    #[serde(rename = "additional")]
    Additional,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DebugCollectLogsSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsSource",
            &[
                "events",
                "process-log",
                "shell-log",
                "additional",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Events,
            1 => Self::ProcessLog,
            2 => Self::ShellLog,
            3 => Self::Additional,
            _ => Self::Unknown,
        })
    }
}

/// Destination kind that was written.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsResultKind {
    /// A .tgz archive was written.
    #[serde(rename = "archive")]
    Archive,
    /// A directory containing the collected files was written.
    #[serde(rename = "directory")]
    Directory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DebugCollectLogsResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsResultKind",
            &["archive", "directory", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Archive,
            1 => Self::Directory,
            _ => Self::Unknown,
        })
    }
}

/// Whether the supplied diagnostic cursor remained within the retained buffer window.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticCursorStatus {
    /// The cursor is valid for the current retained window.
    #[serde(rename = "ok")]
    Ok,
    /// The cursor no longer addresses retained records; reading resumes at the oldest retained record.
    #[serde(rename = "expired")]
    Expired,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiagnosticCursorStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticCursorStatus",
            &["ok", "expired", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Ok,
            1 => Self::Expired,
            _ => Self::Unknown,
        })
    }
}

/// Direction of an observed MCP protocol frame.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpDiagnosticDirection {
    /// Frame emitted by the Copilot MCP client.
    #[serde(rename = "client-to-server")]
    ClientToServer,
    /// Frame received from the MCP server.
    #[serde(rename = "server-to-client")]
    ServerToClient,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpDiagnosticDirection {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpDiagnosticDirection",
            &["client-to-server", "server-to-client", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::ClientToServer,
            1 => Self::ServerToClient,
            _ => Self::Unknown,
        })
    }
}

/// Category for an MCP diagnostic record.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpDiagnosticKind {
    /// Connection lifecycle transition or failure.
    #[serde(rename = "lifecycle")]
    Lifecycle,
    /// JSON-RPC protocol frame.
    #[serde(rename = "protocol")]
    Protocol,
    /// HTTP request or response metadata.
    #[serde(rename = "http")]
    Http,
    /// Local MCP server standard-error output.
    #[serde(rename = "stderr")]
    Stderr,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpDiagnosticKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpDiagnosticKind",
            &["lifecycle", "protocol", "http", "stderr", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Lifecycle,
            1 => Self::Protocol,
            2 => Self::Http,
            3 => Self::Stderr,
            _ => Self::Unknown,
        })
    }
}

/// Severity of an emitted diagnostic record.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticSeverity {
    /// Failure that prevented or interrupted communication.
    #[serde(rename = "error")]
    Error,
    /// A recoverable warning or server standard-error output.
    #[serde(rename = "warning")]
    Warning,
    /// Lifecycle transition.
    #[serde(rename = "info")]
    Info,
    /// Protocol-frame or launch diagnostic.
    #[serde(rename = "debug")]
    Debug,
    /// HTTP metadata diagnostic.
    #[serde(rename = "trace")]
    Trace,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiagnosticSeverity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticSeverity",
            &["error", "warning", "info", "debug", "trace", "Unknown"],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Error,
            1 => Self::Warning,
            2 => Self::Info,
            3 => Self::Debug,
            4 => Self::Trace,
            _ => Self::Unknown,
        })
    }
}

/// Session-scoped diagnostic threshold. Capture is disabled by default and is never persisted with the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiagnosticLogLevel {
    /// Disable capture and clear retained diagnostics.
    #[serde(rename = "off")]
    Off,
    /// Capture failures only.
    #[serde(rename = "error")]
    Error,
    /// Capture failures, warnings, and stderr.
    #[serde(rename = "warning")]
    Warning,
    /// Capture lifecycle diagnostics.
    #[serde(rename = "info")]
    Info,
    /// Capture protocol frames and launch diagnostics.
    #[serde(rename = "debug")]
    Debug,
    /// Capture HTTP metadata in addition to debug diagnostics.
    #[serde(rename = "trace")]
    Trace,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiagnosticLogLevel {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiagnosticLogLevel",
            &[
                "off", "error", "warning", "info", "debug", "trace", "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Off,
            1 => Self::Error,
            2 => Self::Warning,
            3 => Self::Info,
            4 => Self::Debug,
            5 => Self::Trace,
            _ => Self::Unknown,
        })
    }
}

/// Persisted extension discovery source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiscoveredExtensionSource {
    /// Extension discovered from the user's extensions directory.
    #[serde(rename = "user")]
    User,
    /// Extension contributed by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiscoveredExtensionSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiscoveredExtensionSource",
            &["user", "plugin", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// Effective extension loading and agent-management mode
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiscoveredExtensionMode {
    /// Extensions are not loaded.
    #[serde(rename = "disabled")]
    Disabled,
    /// Extensions are loaded, but the agent cannot create, reload, or manage them.
    #[serde(rename = "load_only")]
    LoadOnly,
    /// Extensions are loaded and the agent can create, reload, and manage them.
    #[serde(rename = "load_and_augment")]
    LoadAndAugment,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiscoveredExtensionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiscoveredExtensionMode",
            &["disabled", "load_only", "load_and_augment", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Disabled,
            1 => Self::LoadOnly,
            2 => Self::LoadAndAugment,
            _ => Self::Unknown,
        })
    }
}

/// Kind of component that supplied a provider adapter or row. Attribution does not confer authority.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderProvenanceSource {
    /// Built into the runtime.
    #[serde(rename = "builtIn")]
    BuiltIn,
    /// Derived from existing user configuration.
    #[serde(rename = "configured")]
    Configured,
    /// Supplied by an extension.
    #[serde(rename = "extension")]
    Extension,
    /// Supplied by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Supplied by another trusted contributor.
    #[serde(rename = "custom")]
    Custom,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderProvenanceSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderProvenanceSource",
            &[
                "builtIn",
                "configured",
                "extension",
                "plugin",
                "custom",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::BuiltIn,
            1 => Self::Configured,
            2 => Self::Extension,
            3 => Self::Plugin,
            4 => Self::Custom,
            _ => Self::Unknown,
        })
    }
}

/// Typed outcome for a provider operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderOperationOutcomeCode {
    /// The operation completed successfully; an empty inventory is valid.
    #[serde(rename = "success")]
    Success,
    /// The provider or instance is absent during discovery, status, or model listing. Distinct from a successful empty inventory.
    #[serde(rename = "absent")]
    Absent,
    /// The provider is configured or expected but could not be reached.
    #[serde(rename = "unreachable")]
    Unreachable,
    /// The operation failed for a reason other than absence or reachability.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderOperationOutcomeCode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderOperationOutcomeCode",
            &["success", "absent", "unreachable", "failed", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Absent,
            2 => Self::Unreachable,
            3 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Direction to page through the session's persisted event history. 'forward' pages from the cursor toward newer events; 'backward' returns the newest window first (tail-first) and pages toward older events. Events within a returned batch are always chronological (oldest-to-newest), even for a backward read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EventsReadDirection {
    /// Page from the cursor toward newer events (default).
    #[serde(rename = "forward")]
    Forward,
    /// Tail-first: return the newest events and page toward older events.
    #[serde(rename = "backward")]
    Backward,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EventsReadDirection {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EventsReadDirection",
            &["forward", "backward", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Forward,
            1 => Self::Backward,
            _ => Self::Unknown,
        })
    }
}

/// Cursor status: 'ok' means the read succeeded against the requested history; 'expired' means the requested continuation is unavailable. Recovery is endpoint-specific: session.eventLog.read returns a boundary window of remaining active history that may overlap prior pages, while sessions.readPersistedEvents returns an empty terminal page and never switches journal generations. An expired persisted read is not successful completion; a complete persisted snapshot requires cursorStatus 'ok' and hasMore false.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EventsCursorStatus {
    /// The read succeeded against the requested history.
    #[serde(rename = "ok")]
    Ok,
    /// The requested continuation is unavailable; see the endpoint's recovery semantics.
    #[serde(rename = "expired")]
    Expired,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EventsCursorStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EventsCursorStatus",
            &["ok", "expired", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Ok,
            1 => Self::Expired,
            _ => Self::Unknown,
        })
    }
}

/// Discovery source: project (.github/extensions/), user (~/.copilot/extensions/), plugin (installed plugin), or session (session-state/<id>/extensions/)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExtensionSource {
    /// Extension discovered from the current project's .github/extensions directory.
    #[serde(rename = "project")]
    Project,
    /// Extension discovered from the user's ~/.copilot/extensions directory.
    #[serde(rename = "user")]
    User,
    /// Extension contributed by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Extension discovered from the current session's state directory (loaded only for this session).
    #[serde(rename = "session")]
    Session,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExtensionSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExtensionSource",
            &["project", "user", "plugin", "session", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Project,
            1 => Self::User,
            2 => Self::Plugin,
            3 => Self::Session,
            _ => Self::Unknown,
        })
    }
}

/// Current status: running, dormant, disabled, failed, or starting
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExtensionStatus {
    /// The extension process is running.
    #[serde(rename = "running")]
    Running,
    /// The extension is enabled and discoverable but not yet requested.
    #[serde(rename = "dormant")]
    Dormant,
    /// The extension is installed but disabled.
    #[serde(rename = "disabled")]
    Disabled,
    /// The extension failed to start or crashed.
    #[serde(rename = "failed")]
    Failed,
    /// The extension process is starting.
    #[serde(rename = "starting")]
    Starting,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExtensionStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExtensionStatus",
            &[
                "running", "dormant", "disabled", "failed", "starting", "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Running,
            1 => Self::Dormant,
            2 => Self::Disabled,
            3 => Self::Failed,
            4 => Self::Starting,
            _ => Self::Unknown,
        })
    }
}

/// Runtime-derived standalone extension eligibility.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum StandaloneExtensionAdmission {
    /// Policy is resolved and permits standalone extensions.
    #[serde(rename = "allowed")]
    Allowed,
    /// Standalone extensions are prohibited; eligible plugin extensions remain governed by existing plugin controls.
    #[serde(rename = "plugin_only")]
    PluginOnly,
    /// Managed policy is unresolved; standalone extensions must not execute.
    #[serde(rename = "pending")]
    Pending,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for StandaloneExtensionAdmission {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "StandaloneExtensionAdmission",
            &["allowed", "plugin_only", "pending", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Allowed,
            1 => Self::PluginOnly,
            2 => Self::Pending,
            _ => Self::Unknown,
        })
    }
}

/// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmBinaryResultsForLlmType {
    /// Binary image data.
    #[serde(rename = "image")]
    Image,
    /// Other binary resource data.
    #[serde(rename = "resource")]
    Resource,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmBinaryResultsForLlmType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmBinaryResultsForLlmType",
            &["image", "resource", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Image,
            1 => Self::Resource,
            _ => Self::Unknown,
        })
    }
}

/// Existing persisted file-change classification.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ForgeProposalChangeType {
    /// The file was added.
    #[serde(rename = "added")]
    Added,
    /// The file was modified.
    #[serde(rename = "modified")]
    Modified,
    /// The file was deleted.
    #[serde(rename = "deleted")]
    Deleted,
    /// The file was renamed.
    #[serde(rename = "renamed")]
    Renamed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ForgeProposalChangeType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ForgeProposalChangeType",
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

/// Original proposal generation trigger.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ForgeProposalTriggerMode {
    /// Generation started automatically when the session closed.
    #[serde(rename = "auto_close")]
    AutoClose,
    /// Generation started at the user's request.
    #[serde(rename = "on_demand")]
    OnDemand,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ForgeProposalTriggerMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ForgeProposalTriggerMode",
            &["auto_close", "on_demand", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AutoClose,
            1 => Self::OnDemand,
            _ => Self::Unknown,
        })
    }
}

/// Persisted proposal lifecycle state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ForgeProposalStatus {
    /// The proposal generation is in progress.
    #[serde(rename = "generating")]
    Generating,
    /// The proposal is ready for review.
    #[serde(rename = "ready")]
    Ready,
    /// The user opened the proposal for review.
    #[serde(rename = "reviewing")]
    Reviewing,
    /// The user accepted the proposal.
    #[serde(rename = "accepted")]
    Accepted,
    /// The user rejected the proposal.
    #[serde(rename = "rejected")]
    Rejected,
    /// A newer proposal replaced this proposal.
    #[serde(rename = "superseded")]
    Superseded,
    /// The proposal generation failed.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ForgeProposalStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ForgeProposalStatus",
            &[
                "generating",
                "ready",
                "reviewing",
                "accepted",
                "rejected",
                "superseded",
                "failed",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Generating,
            1 => Self::Ready,
            2 => Self::Reviewing,
            3 => Self::Accepted,
            4 => Self::Rejected,
            5 => Self::Superseded,
            6 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Hosting platform type of the repository
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SessionWorkingDirectoryContextHostType {
    /// The working directory repository is hosted on GitHub.
    #[serde(rename = "github")]
    GitHub,
    /// The working directory repository is hosted on Azure DevOps.
    #[serde(rename = "ado")]
    Ado,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SessionWorkingDirectoryContextHostType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SessionWorkingDirectoryContextHostType",
            &["github", "ado", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::GitHub,
            1 => Self::Ado,
            _ => Self::Unknown,
        })
    }
}

/// Reason a captured file was not restored.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryFileRestoreSkipReason {
    /// The file changed after Copilot's last captured write.
    #[serde(rename = "user-modified")]
    UserModified,
    /// A faithful preimage was not captured.
    #[serde(rename = "skipped-capture")]
    SkippedCapture,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryFileRestoreSkipReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryFileRestoreSkipReason",
            &["user-modified", "skipped-capture", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::UserModified,
            1 => Self::SkippedCapture,
            _ => Self::Unknown,
        })
    }
}

/// Reason a rewind read (rewind points, file-restore preview, or session diff) could not be answered from the session's file-change captures.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryRewindUnavailableReason {
    /// The session did not opt into file-change tracking before its first turn.
    #[serde(rename = "file-change-tracking-disabled")]
    FileChangeTrackingDisabled,
    /// The session still has work that may mutate files or history. Transient: the same request succeeds once the session settles, so callers should retry rather than treat it as a failure.
    #[serde(rename = "session-busy")]
    SessionBusy,
    /// Remote-backed rewind routing is not supported.
    #[serde(rename = "unsupported-remote-session")]
    UnsupportedRemoteSession,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryRewindUnavailableReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryRewindUnavailableReason",
            &[
                "file-change-tracking-disabled",
                "session-busy",
                "unsupported-remote-session",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::FileChangeTrackingDisabled,
            1 => Self::SessionBusy,
            2 => Self::UnsupportedRemoteSession,
            _ => Self::Unknown,
        })
    }
}

/// Aggregate file change represented by a rewind preview.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryRewindChangeType {
    /// The discarded turns created the file.
    #[serde(rename = "created")]
    Created,
    /// The discarded turns deleted the file.
    #[serde(rename = "deleted")]
    Deleted,
    /// The discarded turns modified the file.
    #[serde(rename = "modified")]
    Modified,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryRewindChangeType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryRewindChangeType",
            &["created", "deleted", "modified", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Created,
            1 => Self::Deleted,
            2 => Self::Modified,
            _ => Self::Unknown,
        })
    }
}

/// Outcome of a rewind request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryRewindOutcome {
    /// The requested rewind completed; reachable in either mode.
    #[serde(rename = "success")]
    Success,
    /// The session still has work that may mutate files or history; reachable in either mode.
    #[serde(rename = "session-busy")]
    SessionBusy,
    /// A conversation-and-files rewind was requested for a session that did not enable capture; conversation-only rewinds never produce this.
    #[serde(rename = "file-change-tracking-disabled")]
    FileChangeTrackingDisabled,
    /// Remote-backed rewind routing is not supported; reachable in either mode.
    #[serde(rename = "unsupported-remote-session")]
    UnsupportedRemoteSession,
    /// File restore failed and all applied file changes were rolled back; only conversation-and-files rewinds produce this.
    #[serde(rename = "files-rolled-back")]
    FilesRolledBack,
    /// File restore failed and its rollback could not fully restore the pre-rewind state; only conversation-and-files rewinds produce this.
    #[serde(rename = "rollback-incomplete")]
    RollbackIncomplete,
    /// Conversation truncation failed. In conversation-and-files mode any files that were restored are left in place because conversation history cannot be un-truncated; in conversation-only mode no files are restored. Consult restoredFiles for what, if anything, was applied.
    #[serde(rename = "truncation-failed")]
    TruncationFailed,
    /// The conversation was rewound (and, in conversation-and-files mode, captured files were restored), but persisted checkpoints could not be cleaned up; reachable in either mode.
    #[serde(rename = "checkpoint-cleanup-failed")]
    CheckpointCleanupFailed,
    /// Files and conversation were rewound, but obsolete file snapshots could not be removed; only conversation-and-files rewinds produce this.
    #[serde(rename = "snapshot-prune-failed")]
    SnapshotPruneFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryRewindOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryRewindOutcome",
            &[
                "success",
                "session-busy",
                "file-change-tracking-disabled",
                "unsupported-remote-session",
                "files-rolled-back",
                "rollback-incomplete",
                "truncation-failed",
                "checkpoint-cleanup-failed",
                "snapshot-prune-failed",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::SessionBusy,
            2 => Self::FileChangeTrackingDisabled,
            3 => Self::UnsupportedRemoteSession,
            4 => Self::FilesRolledBack,
            5 => Self::RollbackIncomplete,
            6 => Self::TruncationFailed,
            7 => Self::CheckpointCleanupFailed,
            8 => Self::SnapshotPruneFailed,
            _ => Self::Unknown,
        })
    }
}

/// Where this source lives — used for UI grouping
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionSourceLocation {
    /// Instructions live in user-level configuration.
    #[serde(rename = "user")]
    User,
    /// Instructions live in repository-level configuration.
    #[serde(rename = "repository")]
    Repository,
    /// Instructions live under the current working directory.
    #[serde(rename = "working-directory")]
    WorkingDirectory,
    /// Instructions live in plugin-provided configuration.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionSourceLocation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionSourceLocation",
            &[
                "user",
                "repository",
                "working-directory",
                "plugin",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Repository,
            2 => Self::WorkingDirectory,
            3 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// Category of instruction source — used for merge logic
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionSourceType {
    /// Instructions loaded from the user's home configuration.
    #[serde(rename = "home")]
    Home,
    /// Instructions loaded from repository-scoped files.
    #[serde(rename = "repo")]
    Repo,
    /// Instructions loaded from model-specific files.
    #[serde(rename = "model")]
    Model,
    /// Instructions loaded from VS Code instruction files.
    #[serde(rename = "vscode")]
    Vscode,
    /// Instructions discovered from nested agent files.
    #[serde(rename = "nested-agents")]
    NestedAgents,
    /// Instructions inherited from child instruction files.
    #[serde(rename = "child-instructions")]
    ChildInstructions,
    /// Instructions supplied by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionSourceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionSourceType",
            &[
                "home",
                "repo",
                "model",
                "vscode",
                "nested-agents",
                "child-instructions",
                "plugin",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Home,
            1 => Self::Repo,
            2 => Self::Model,
            3 => Self::Vscode,
            4 => Self::NestedAgents,
            5 => Self::ChildInstructions,
            6 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// The result of the guarded retired marketplace migration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum MarketplaceMigrationStatus {
    /// The migration already ran for this profile in this process.
    #[serde(rename = "alreadyRan")]
    AlreadyRan,
    /// This profile has no retained install that needs migration.
    #[serde(rename = "notApplicable")]
    NotApplicable,
    /// The user already registered the marketplace.
    #[serde(rename = "alreadyRegistered")]
    AlreadyRegistered,
    /// Managed policy controls the marketplace name.
    #[serde(rename = "blockedByManagedPolicy")]
    BlockedByManagedPolicy,
    /// The migration registered the marketplace and persisted its completion.
    #[serde(rename = "migrated")]
    Migrated,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for MarketplaceMigrationStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "MarketplaceMigrationStatus",
            &[
                "alreadyRan",
                "notApplicable",
                "alreadyRegistered",
                "blockedByManagedPolicy",
                "migrated",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::AlreadyRan,
            1 => Self::NotApplicable,
            2 => Self::AlreadyRegistered,
            3 => Self::BlockedByManagedPolicy,
            4 => Self::Migrated,
            _ => Self::Unknown,
        })
    }
}

/// Allowed values for the `McpAppsHostContextDetailsAvailableDisplayMode` enumeration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsHostContextDetailsAvailableDisplayMode {
    /// Rendered inline within the host conversation surface
    #[serde(rename = "inline")]
    Inline,
    /// Rendered as a fullscreen overlay
    #[serde(rename = "fullscreen")]
    Fullscreen,
    /// Rendered as a picture-in-picture floating panel
    #[serde(rename = "pip")]
    Pip,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsHostContextDetailsAvailableDisplayMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsHostContextDetailsAvailableDisplayMode",
            &["inline", "fullscreen", "pip", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Inline,
            1 => Self::Fullscreen,
            2 => Self::Pip,
            _ => Self::Unknown,
        })
    }
}

/// Current display mode (SEP-1865)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsHostContextDetailsDisplayMode {
    /// Rendered inline within the host conversation surface
    #[serde(rename = "inline")]
    Inline,
    /// Rendered as a fullscreen overlay
    #[serde(rename = "fullscreen")]
    Fullscreen,
    /// Rendered as a picture-in-picture floating panel
    #[serde(rename = "pip")]
    Pip,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsHostContextDetailsDisplayMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsHostContextDetailsDisplayMode",
            &["inline", "fullscreen", "pip", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Inline,
            1 => Self::Fullscreen,
            2 => Self::Pip,
            _ => Self::Unknown,
        })
    }
}

/// Platform type for responsive design
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsHostContextDetailsPlatform {
    /// Host runs in a web browser
    #[serde(rename = "web")]
    Web,
    /// Host runs as a desktop application
    #[serde(rename = "desktop")]
    Desktop,
    /// Host runs on a mobile device
    #[serde(rename = "mobile")]
    Mobile,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsHostContextDetailsPlatform {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsHostContextDetailsPlatform",
            &["web", "desktop", "mobile", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Web,
            1 => Self::Desktop,
            2 => Self::Mobile,
            _ => Self::Unknown,
        })
    }
}

/// UI theme preference per SEP-1865
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsHostContextDetailsTheme {
    /// Light UI theme
    #[serde(rename = "light")]
    Light,
    /// Dark UI theme
    #[serde(rename = "dark")]
    Dark,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsHostContextDetailsTheme {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsHostContextDetailsTheme",
            &["light", "dark", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Light,
            1 => Self::Dark,
            _ => Self::Unknown,
        })
    }
}

/// Consumer allowed to call an MCP tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpToolUiVisibility {
    /// The model may call the tool.
    #[serde(rename = "model")]
    Model,
    /// An MCP App view may call the tool.
    #[serde(rename = "app")]
    App,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpToolUiVisibility {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpToolUiVisibility",
            &["model", "app", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Model,
            1 => Self::App,
            _ => Self::Unknown,
        })
    }
}

/// OAuth grant type override for this login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthLoginGrantType {
    /// Interactive browser-based OAuth flow using an authorization code, typically with PKCE.
    #[serde(rename = "authorization_code")]
    AuthorizationCode,
    /// Headless OAuth flow where a confidential client authenticates directly with a client secret.
    #[serde(rename = "client_credentials")]
    ClientCredentials,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthLoginGrantType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthLoginGrantType",
            &["authorization_code", "client_credentials", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AuthorizationCode,
            1 => Self::ClientCredentials,
            _ => Self::Unknown,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreSessionKind {
    #[serde(rename = "session")]
    #[default]
    Session,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreSessionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreSessionKind",
            &["session"],
            None,
        )?;
        Ok(Self::Session)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreMemoryKind {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreMemoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreMemoryKind",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStorePersistentKind {
    #[serde(rename = "persistent")]
    #[default]
    Persistent,
}
impl<'de> Deserialize<'de> for McpOauthFlowStorePersistentKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStorePersistentKind",
            &["persistent"],
            None,
        )?;
        Ok(Self::Persistent)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreCallbackKind {
    #[serde(rename = "callback")]
    #[default]
    Callback,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreCallbackKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreCallbackKind",
            &["callback"],
            None,
        )?;
        Ok(Self::Callback)
    }
}

/// Credential owner for a host-driven OAuth flow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum McpOauthFlowStore {
    Session(McpOauthFlowStoreSession),
    Memory(McpOauthFlowStoreMemory),
    Persistent(McpOauthFlowStorePersistent),
    Callback(McpOauthFlowStoreCallback),
}

/// Outcome of starting the original prepared owned login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOwnedOauthLoginStatus {
    /// The original requester may open the returned authorisation URL.
    #[serde(rename = "awaiting-browser")]
    AwaitingBrowser,
    /// Cached credentials were accepted and the original server finished reconnecting.
    #[serde(rename = "connected")]
    Connected,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOwnedOauthLoginStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOwnedOauthLoginStatus",
            &["awaiting-browser", "connected", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AwaitingBrowser,
            1 => Self::Connected,
            _ => Self::Unknown,
        })
    }
}

/// Remote protocol selected for the tokenless handshake.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum McpOauthTestConnectionTransport {
    /// Streamable HTTP.
    #[serde(rename = "http")]
    Http,
    /// Legacy HTTP with server-sent events.
    #[serde(rename = "sse")]
    Sse,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthTestConnectionTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthTestConnectionTransport",
            &["http", "sse", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Sse,
            _ => Self::Unknown,
        })
    }
}

/// The sender role of an MCP prompt message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPromptRole {
    /// A message from the user.
    #[serde(rename = "user")]
    User,
    /// A message from the assistant.
    #[serde(rename = "assistant")]
    Assistant,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPromptRole {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPromptRole",
            &["user", "assistant", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Assistant,
            _ => Self::Unknown,
        })
    }
}

/// Outcome of the sampling inference. 'success' produced a response; 'failure' encountered an error (including agent-side rejection by content filter or criteria); 'cancelled' the caller cancelled this execution via cancelSamplingExecution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpSamplingExecutionAction {
    /// The sampling inference completed and produced a result.
    #[serde(rename = "success")]
    Success,
    /// The sampling inference failed or was rejected.
    #[serde(rename = "failure")]
    Failure,
    /// The sampling inference was cancelled before completion.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpSamplingExecutionAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpSamplingExecutionAction",
            &["success", "failure", "cancelled", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Failure,
            2 => Self::Cancelled,
            _ => Self::Unknown,
        })
    }
}

/// How environment-variable values supplied to MCP servers are resolved. "direct" passes literal string values; "indirect" treats values as references (e.g. names of environment variables on the host) that the runtime resolves before launch. Defaults to the runtime's startup mode; clients that intentionally launch MCP servers with literal values (e.g. CLI prompt mode and ACP) set this to "direct".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpSetEnvValueModeDetails {
    /// Treat MCP server environment values as literal strings.
    #[serde(rename = "direct")]
    Direct,
    /// Treat MCP server environment values as host-side references to resolve before launch.
    #[serde(rename = "indirect")]
    Indirect,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpSetEnvValueModeDetails {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpSetEnvValueModeDetails",
            &["direct", "indirect", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Direct,
            1 => Self::Indirect,
            _ => Self::Unknown,
        })
    }
}

/// Controls how availableTools (allowlist) and excludedTools (denylist) combine when both are set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum OptionsUpdateToolFilterPrecedence {
    /// If availableTools is set, it is the only constraint that applies (excludedTools is ignored). Explicitly opts into allowlist precedence.
    #[serde(rename = "available")]
    Available,
    /// A tool is enabled if and only if it matches the allowlist (or the allowlist is unset) AND it does not match the denylist. Default for new sessions.
    #[serde(rename = "excluded")]
    Excluded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for OptionsUpdateToolFilterPrecedence {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "OptionsUpdateToolFilterPrecedence",
            &["available", "excluded", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Available,
            1 => Self::Excluded,
            _ => Self::Unknown,
        })
    }
}

/// Model capability category for grouping in the model picker
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelPickerCategory {
    /// Lightweight model category optimized for faster, lower-cost interactions.
    #[serde(rename = "lightweight")]
    Lightweight,
    /// Versatile model category suitable for a broad range of tasks.
    #[serde(rename = "versatile")]
    Versatile,
    /// Powerful model category optimized for complex tasks.
    #[serde(rename = "powerful")]
    Powerful,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelPickerCategory {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelPickerCategory",
            &["lightweight", "versatile", "powerful", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Lightweight,
            1 => Self::Versatile,
            2 => Self::Powerful,
            _ => Self::Unknown,
        })
    }
}

/// Relative cost tier for token-based billing users
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelPickerPriceCategory {
    /// Lowest relative token cost tier.
    #[serde(rename = "low")]
    Low,
    /// Medium relative token cost tier.
    #[serde(rename = "medium")]
    Medium,
    /// High relative token cost tier.
    #[serde(rename = "high")]
    High,
    /// Highest relative token cost tier.
    #[serde(rename = "very_high")]
    VeryHigh,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelPickerPriceCategory {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelPickerPriceCategory",
            &["low", "medium", "high", "very_high", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Low,
            1 => Self::Medium,
            2 => Self::High,
            3 => Self::VeryHigh,
            _ => Self::Unknown,
        })
    }
}

/// Current policy state for this model
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelPolicyState {
    /// The model is enabled by policy.
    #[serde(rename = "enabled")]
    Enabled,
    /// The model is disabled by policy.
    #[serde(rename = "disabled")]
    Disabled,
    /// No explicit policy is configured for the model.
    #[serde(rename = "unconfigured")]
    Unconfigured,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelPolicyState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelPolicyState",
            &["enabled", "disabled", "unconfigured", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Enabled,
            1 => Self::Disabled,
            2 => Self::Unconfigured,
            _ => Self::Unknown,
        })
    }
}

/// When the runtime may run an adapter without an explicit user action.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderAutomaticDiscoveryMode {
    /// The adapter declares that automatic discovery is safe when the other policy fields are satisfied.
    #[serde(rename = "automatic")]
    Automatic,
    /// The adapter may refresh instances the user already configured, but must not scan for new instances automatically.
    #[serde(rename = "configuredOnly")]
    ConfiguredOnly,
    /// The adapter must run only after an explicit user action.
    #[serde(rename = "explicit")]
    Explicit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderAutomaticDiscoveryMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderAutomaticDiscoveryMode",
            &["automatic", "configuredOnly", "explicit", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Automatic,
            1 => Self::ConfiguredOnly,
            2 => Self::Explicit,
            _ => Self::Unknown,
        })
    }
}

/// Network reach an adapter may use during discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderDiscoveryNetworkScope {
    /// Discovery does not contact a network service.
    #[serde(rename = "none")]
    None,
    /// Discovery is limited to loopback addresses on the local machine.
    #[serde(rename = "loopbackOnly")]
    LoopbackOnly,
    /// Discovery contacts only endpoints the user already configured.
    #[serde(rename = "configuredEndpointOnly")]
    ConfiguredEndpointOnly,
    /// Discovery may scan or contact the local network.
    #[serde(rename = "localNetwork")]
    LocalNetwork,
    /// Discovery may contact remote internet services.
    #[serde(rename = "internet")]
    Internet,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderDiscoveryNetworkScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderDiscoveryNetworkScope",
            &[
                "none",
                "loopbackOnly",
                "configuredEndpointOnly",
                "localNetwork",
                "internet",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::LoopbackOnly,
            2 => Self::ConfiguredEndpointOnly,
            3 => Self::LocalNetwork,
            4 => Self::Internet,
            _ => Self::Unknown,
        })
    }
}

/// Whether a planned configuration entry is new or already present in the session registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderConfigurationDisposition {
    /// No matching entry is registered; the caller should add the entry.
    #[serde(rename = "create")]
    Create,
    /// An equivalent entry is already registered; the caller should reuse it rather than adding a duplicate.
    #[serde(rename = "alreadyConfigured")]
    AlreadyConfigured,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderConfigurationDisposition {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderConfigurationDisposition",
            &["create", "alreadyConfigured", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Create,
            1 => Self::AlreadyConfigured,
            _ => Self::Unknown,
        })
    }
}

/// Transport to be used for provider requests.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderEndpointTransport {
    /// HTTP request/streaming transport.
    #[serde(rename = "http")]
    Http,
    /// WebSocket transport.
    #[serde(rename = "websockets")]
    Websockets,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderEndpointTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderEndpointTransport",
            &["http", "websockets", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websockets,
            _ => Self::Unknown,
        })
    }
}

/// Provider family. Matches the `type` field of a BYOK provider config.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderEndpointType {
    /// OpenAI-compatible endpoint (use the OpenAI client library).
    #[serde(rename = "openai")]
    Openai,
    /// Azure OpenAI endpoint (use the OpenAI client library with the Azure base URL).
    #[serde(rename = "azure")]
    Azure,
    /// Anthropic endpoint (use the Anthropic client library).
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Experimental: Google Gemini API endpoint (use the Gemini Interactions API).
    #[serde(rename = "google")]
    Google,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderEndpointType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderEndpointType",
            &["openai", "azure", "anthropic", "google", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Azure,
            2 => Self::Anthropic,
            3 => Self::Google,
            _ => Self::Unknown,
        })
    }
}

/// Wire API to be used, when required for the provider type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderEndpointWireApi {
    /// Classic chat-completions request shape.
    #[serde(rename = "completions")]
    Completions,
    /// Newer responses request shape.
    #[serde(rename = "responses")]
    Responses,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderEndpointWireApi {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderEndpointWireApi",
            &["completions", "responses", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Completions,
            1 => Self::Responses,
            _ => Self::Unknown,
        })
    }
}

/// Whether the requested preference was already effective or was accepted for later transactional activation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelSwitchAutoTierStatus {
    /// The requested preference is already effective. No activation is pending for it, although this request may have cancelled an earlier unclaimed preference reported in `supersededAutoTier`.
    #[serde(rename = "unchanged")]
    Unchanged,
    /// The request was accepted but has not committed. A later user turn using the `auto` model must mint and validate the replacement before it becomes effective.
    #[serde(rename = "pending")]
    Pending,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelSwitchAutoTierStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelSwitchAutoTierStatus",
            &["unchanged", "pending", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Unchanged,
            1 => Self::Pending,
            _ => Self::Unknown,
        })
    }
}

/// Disposition of a permission request as observed by the responding client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDecisionOutcome {
    /// The request was approved automatically without a new human decision.
    #[serde(rename = "auto_approved")]
    AutoApproved,
    /// The request was denied without an interactive user decision; source records why.
    #[serde(rename = "autopilot_denied")]
    AutopilotDenied,
    /// The response came from an interactive user prompt.
    #[serde(rename = "prompted_user")]
    PromptedUser,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionDecisionOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDecisionOutcome",
            &[
                "auto_approved",
                "autopilot_denied",
                "prompted_user",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::AutoApproved,
            1 => Self::AutopilotDenied,
            2 => Self::PromptedUser,
            _ => Self::Unknown,
        })
    }
}

/// Response capability available to the client when it settled a permission request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionResponseCapability {
    /// The client could ask a user for this decision.
    #[serde(rename = "interactive")]
    Interactive,
    /// The client could return an automated response but could not ask a user.
    #[serde(rename = "headless")]
    Headless,
    /// The client had no response path available.
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionResponseCapability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionResponseCapability",
            &["interactive", "headless", "none", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Interactive,
            1 => Self::Headless,
            2 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// Client surface that submitted a permission response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDecisionSurface {
    /// The interactive Copilot CLI terminal UI.
    #[serde(rename = "tui")]
    Tui,
    /// The non-interactive Copilot CLI prompt mode.
    #[serde(rename = "prompt_mode")]
    PromptMode,
    /// The Copilot App client.
    #[serde(rename = "copilot_app")]
    CopilotApp,
    /// An Agent Client Protocol host.
    #[serde(rename = "acp")]
    Acp,
    /// A generic Copilot SDK client.
    #[serde(rename = "sdk")]
    Sdk,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionDecisionSurface {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDecisionSurface",
            &["tui", "prompt_mode", "copilot_app", "acp", "sdk", "Unknown"],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Tui,
            1 => Self::PromptMode,
            2 => Self::CopilotApp,
            3 => Self::Acp,
            4 => Self::Sdk,
            _ => Self::Unknown,
        })
    }
}

/// Where completed plugin content was staged before atomic promotion.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallStagingMode {
    /// A sibling of the installed-plugins root, outside the recursively watched tree.
    #[serde(rename = "external")]
    External,
    /// A sibling of the destination plugin directory, used when external staging is unavailable.
    #[serde(rename = "destination_sibling")]
    DestinationSibling,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PluginInstallStagingMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallStagingMode",
            &["external", "destination_sibling", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::External,
            1 => Self::DestinationSibling,
            _ => Self::Unknown,
        })
    }
}

/// The UI mode the agent was in when this message was sent. Defaults to the session's current mode.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SendAgentMode {
    /// The agent is responding interactively to the user.
    #[serde(rename = "interactive")]
    Interactive,
    /// The agent is preparing a plan before making changes.
    #[serde(rename = "plan")]
    Plan,
    /// The agent is working autonomously toward task completion.
    #[serde(rename = "autopilot")]
    Autopilot,
    /// The agent is in shell-focused UI mode.
    #[serde(rename = "shell")]
    Shell,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SendAgentMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SendAgentMode",
            &["interactive", "plan", "autopilot", "shell", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Interactive,
            1 => Self::Plan,
            2 => Self::Autopilot,
            3 => Self::Shell,
            _ => Self::Unknown,
        })
    }
}

/// Whether this item is a queued user message or a queued slash command / model change
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum QueuePendingItemsKind {
    /// A queued user message.
    #[serde(rename = "message")]
    Message,
    /// A queued slash command or model-change command.
    #[serde(rename = "command")]
    Command,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for QueuePendingItemsKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "QueuePendingItemsKind",
            &["message", "command", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Message,
            1 => Self::Command,
            _ => Self::Unknown,
        })
    }
}

/// One atomic settings mutation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum SettingsEditOperation {
    /// Set one scalar value.
    #[serde(rename = "set")]
    Set,
    /// Add one string to a list, unless present.
    #[serde(rename = "add-item")]
    AddItem,
    /// Remove one string from a list, unless absent.
    #[serde(rename = "remove-item")]
    RemoveItem,
    /// Remove the setting without supplying a value.
    #[serde(rename = "remove")]
    Remove,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SettingsEditOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SettingsEditOperation",
            &["set", "add-item", "remove-item", "remove", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Set,
            1 => Self::AddItem,
            2 => Self::RemoveItem,
            3 => Self::Remove,
            _ => Self::Unknown,
        })
    }
}

/// Origin of the sandbox choice supplied by the host. This value describes preference or session intent; it does not authorize bypassing managed policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxConfigSource {
    /// The client applied the default because no sandbox preference was configured.
    #[serde(rename = "never_configured")]
    NeverConfigured,
    /// The user's persisted settings enabled the sandbox.
    #[serde(rename = "user_enabled")]
    UserEnabled,
    /// The user's persisted settings disabled the sandbox.
    #[serde(rename = "user_disabled")]
    UserDisabled,
    /// An explicit session-scoped choice selected the sandbox state, such as a command-line flag.
    #[serde(rename = "session_flag")]
    SessionFlag,
    /// The user disabled the sandbox for the current session.
    #[serde(rename = "session_disabled")]
    SessionDisabled,
    /// The client disabled the sandbox because the host cannot enforce it.
    #[serde(rename = "unsupported_host")]
    UnsupportedHost,
    /// A repository policy selected the sandbox state.
    #[serde(rename = "repository_policy")]
    RepositoryPolicy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxConfigSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxConfigSource",
            &[
                "never_configured",
                "user_enabled",
                "user_disabled",
                "session_flag",
                "session_disabled",
                "unsupported_host",
                "repository_policy",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::NeverConfigured,
            1 => Self::UserEnabled,
            2 => Self::UserDisabled,
            3 => Self::SessionFlag,
            4 => Self::SessionDisabled,
            5 => Self::UnsupportedHost,
            6 => Self::RepositoryPolicy,
            _ => Self::Unknown,
        })
    }
}

/// Original sandbox transport owner discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum SandboxTransportOwnerLocalRpcKind {
    #[serde(rename = "localRpc")]
    #[default]
    LocalRpc,
}
impl<'de> Deserialize<'de> for SandboxTransportOwnerLocalRpcKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxTransportOwnerLocalRpcKind",
            &["localRpc"],
            None,
        )?;
        Ok(Self::LocalRpc)
    }
}

/// Original sandbox transport owner discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum SandboxTransportOwnerAhpKind {
    #[serde(rename = "ahp")]
    #[default]
    Ahp,
}
impl<'de> Deserialize<'de> for SandboxTransportOwnerAhpKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxTransportOwnerAhpKind",
            &["ahp"],
            None,
        )?;
        Ok(Self::Ahp)
    }
}

/// Original authenticated transport for an owner-side sandbox operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum SandboxTransportOwner {
    LocalRpc(SandboxTransportOwnerLocalRpc),
    Ahp(SandboxTransportOwnerAhp),
}

/// Credential availability, not provider authentication or health.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SavedProviderCredentialStatus {
    /// A secure or legacy credential source is configured.
    #[serde(rename = "present")]
    Present,
    /// No credential source is configured.
    #[serde(rename = "absent")]
    Absent,
    /// Secure credential storage could not be queried.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SavedProviderCredentialStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SavedProviderCredentialStatus",
            &["present", "absent", "unavailable", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Present,
            1 => Self::Absent,
            2 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Repository host type, if known
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkspaceSummaryHostType {
    /// Workspace summary repository is hosted on GitHub.
    #[serde(rename = "github")]
    GitHub,
    /// Workspace summary repository is hosted on Azure DevOps.
    #[serde(rename = "ado")]
    Ado,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkspaceSummaryHostType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkspaceSummaryHostType",
            &["github", "ado", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::GitHub,
            1 => Self::Ado,
            _ => Self::Unknown,
        })
    }
}

/// Controls automatic non-interactive profile loading where supported. Explicit initScripts are unaffected.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ShellInitProfile {
    /// Disable automatic non-interactive profile loading. Explicit initScripts still run.
    #[serde(rename = "none")]
    None,
    /// Allow automatic non-interactive profile loading when supported. Explicit initScripts still run.
    #[serde(rename = "non-interactive")]
    NonInteractive,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ShellInitProfile {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ShellInitProfile",
            &["none", "non-interactive", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::NonInteractive,
            _ => Self::Unknown,
        })
    }
}

/// Supported built-in shells for initialization scripts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ShellInitScriptShell {
    /// Source the script in the built-in Bash shell on macOS and Linux.
    #[serde(rename = "bash")]
    Bash,
    /// Source the script in the built-in PowerShell shell on Windows.
    #[serde(rename = "powershell")]
    Powershell,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ShellInitScriptShell {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ShellInitScriptShell",
            &["bash", "powershell", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Bash,
            1 => Self::Powershell,
            _ => Self::Unknown,
        })
    }
}

/// Sharing status for a synced session. "repo" makes the session visible to anyone with read access to the repository; "unshared" restricts it to the creator and collaborators.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SessionVisibilityStatus {
    /// The session is visible to repository readers.
    #[serde(rename = "repo")]
    Repo,
    /// The session is restricted to its creator and collaborators.
    #[serde(rename = "unshared")]
    Unshared,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SessionVisibilityStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SessionVisibilityStatus",
            &["repo", "unshared", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Repo,
            1 => Self::Unshared,
            _ => Self::Unknown,
        })
    }
}

/// Whether one atomic settings edit changed a file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum SettingsEditStatus {
    /// The edit changed the setting and was stored.
    #[serde(rename = "changed")]
    Changed,
    /// The setting already had the requested state.
    #[serde(rename = "unchanged")]
    Unchanged,
    /// The existing settings file could not be edited safely; nothing was written.
    #[serde(rename = "invalid")]
    Invalid,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SettingsEditStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SettingsEditStatus",
            &["changed", "unchanged", "invalid", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Changed,
            1 => Self::Unchanged,
            2 => Self::Invalid,
            _ => Self::Unknown,
        })
    }
}

/// Context tier override for matching subagents
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SubagentSettingsEntryContextTier {
    /// Inherit the parent session's effective context tier at dispatch time.
    #[serde(rename = "inherit")]
    Inherit,
    /// Use the model's default context window.
    #[serde(rename = "default")]
    Default,
    /// Pin the subagent to the long-context tier when supported.
    #[serde(rename = "long_context")]
    LongContext,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SubagentSettingsEntryContextTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SubagentSettingsEntryContextTier",
            &["inherit", "default", "long_context", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Inherit,
            1 => Self::Default,
            2 => Self::LongContext,
            _ => Self::Unknown,
        })
    }
}

/// The user's response: accept (submitted), decline (rejected), or cancel (dismissed)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationResponseAction {
    /// The user submitted the requested form values.
    #[serde(rename = "accept")]
    Accept,
    /// The user explicitly declined to provide the requested input.
    #[serde(rename = "decline")]
    Decline,
    /// The user dismissed the elicitation request.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIElicitationResponseAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationResponseAction",
            &["accept", "decline", "cancel", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Accept,
            1 => Self::Decline,
            2 => Self::Cancel,
            _ => Self::Unknown,
        })
    }
}

/// Current or terminal state of a workflow run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowRunStatus {
    /// The run was minted and is awaiting approval.
    #[serde(rename = "pending")]
    Pending,
    /// The run is executing.
    #[serde(rename = "running")]
    Running,
    /// The run completed successfully.
    #[serde(rename = "completed")]
    Completed,
    /// The run was interrupted while resource budget remained.
    #[serde(rename = "halted")]
    Halted,
    /// The current attempt stopped intentionally and the run may be resumed.
    #[serde(rename = "paused")]
    Paused,
    /// The run was cancelled before completion.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// The workflow body failed or reached a cumulative resource ceiling.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowRunStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowRunStatus",
            &[
                "pending",
                "running",
                "completed",
                "halted",
                "paused",
                "cancelled",
                "error",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Pending,
            1 => Self::Running,
            2 => Self::Completed,
            3 => Self::Halted,
            4 => Self::Paused,
            5 => Self::Cancelled,
            6 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Kind of workflow progress line.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowLogLineKind {
    /// A narrator log line.
    #[serde(rename = "log")]
    Log,
    /// A named workflow phase marker.
    #[serde(rename = "phase")]
    Phase,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowLogLineKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowLogLineKind",
            &["log", "phase", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Log,
            1 => Self::Phase,
            _ => Self::Unknown,
        })
    }
}

/// Action the runtime selected for a durable workflow pause checkpoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowPauseCheckpointAction {
    /// The checkpoint was committed by a prior paused attempt, so execution may continue.
    #[serde(rename = "continue")]
    Continue,
    /// This attempt claimed the checkpoint and must cooperatively stop.
    #[serde(rename = "pause")]
    Pause,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowPauseCheckpointAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowPauseCheckpointAction",
            &["continue", "pause", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Continue,
            1 => Self::Pause,
            _ => Self::Unknown,
        })
    }
}

/// Derived lifecycle state of a workflow phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowPhaseStatus {
    /// The phase has not been entered yet.
    #[serde(rename = "pending")]
    Pending,
    /// The phase is currently entered and accumulating active time.
    #[serde(rename = "active")]
    Active,
    /// The phase was entered and has since been closed.
    #[serde(rename = "completed")]
    Completed,
    /// The phase was never entered because a later phase was entered or the run reached a terminal state.
    #[serde(rename = "skipped")]
    Skipped,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowPhaseStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowPhaseStatus",
            &["pending", "active", "completed", "skipped", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Pending,
            1 => Self::Active,
            2 => Self::Completed,
            3 => Self::Skipped,
            _ => Self::Unknown,
        })
    }
}

/// Type of change represented by this file diff.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkspaceDiffFileChangeType {
    /// The file was added.
    #[serde(rename = "added")]
    Added,
    /// The file was modified.
    #[serde(rename = "modified")]
    Modified,
    /// The file was deleted.
    #[serde(rename = "deleted")]
    Deleted,
    /// The file was renamed.
    #[serde(rename = "renamed")]
    Renamed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkspaceDiffFileChangeType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkspaceDiffFileChangeType",
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

/// Diff mode requested by the client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkspaceDiffMode {
    /// Return staged, unstaged, and untracked working tree changes.
    #[serde(rename = "unstaged")]
    Unstaged,
    /// Return changes compared with the default branch.
    #[serde(rename = "branch")]
    Branch,
    /// Return the cumulative diff of files Copilot changed this session (used in non-git workspaces).
    #[serde(rename = "session")]
    Session,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkspaceDiffMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkspaceDiffMode",
            &["unstaged", "branch", "session", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Unstaged,
            1 => Self::Branch,
            2 => Self::Session,
            _ => Self::Unknown,
        })
    }
}

/// Allowed values for the `WorkspacesWorkspaceDetailsHostType` enumeration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkspacesWorkspaceDetailsHostType {
    /// Workspace repository is hosted on GitHub.
    #[serde(rename = "github")]
    GitHub,
    /// Workspace repository is hosted on Azure DevOps.
    #[serde(rename = "ado")]
    Ado,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkspacesWorkspaceDetailsHostType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkspacesWorkspaceDetailsHostType",
            &["github", "ado", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::GitHub,
            1 => Self::Ado,
            _ => Self::Unknown,
        })
    }
}
