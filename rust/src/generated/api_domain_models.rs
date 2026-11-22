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

/// Request limits for one decision model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionModelLimits {
    /// Token budget for the context plus any one question, when the model has one.
    pub context_plus_question_tokens: Option<i64>,
    /// Maximum nesting depth of JSON context.
    pub max_json_depth: i64,
    /// Maximum levels per score question.
    pub max_levels: i64,
    /// Maximum options per choice question.
    pub max_options: i64,
    /// Maximum questions per request.
    pub max_questions: i64,
    /// Maximum size of the serialized request, in bytes.
    pub max_request_bytes: i64,
    /// Minimum levels per score question.
    pub min_levels: i64,
    /// Minimum options per choice question.
    pub min_options: i64,
}
crate::record_serde::wire_record!(DecisionModelLimits { context_plus_question_tokens: "contextPlusQuestionTokens" if is_none, max_json_depth: "maxJsonDepth", max_levels: "maxLevels", max_options: "maxOptions", max_questions: "maxQuestions", max_request_bytes: "maxRequestBytes", min_levels: "minLevels", min_options: "minOptions" });

/// One callable decision model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionModelInfo {
    /// Model id to pass as `model` to `decisions.ask`.
    pub id: String,
    /// Whether `decisions.ask` uses this model when `model` is omitted.
    pub is_default: bool,
    /// Effective limits callers can rely on: the stricter of the backend's and the runtime's.
    pub limits: DecisionModelLimits,
    /// Display name.
    pub name: String,
    /// Whether the backend can refuse individual questions.
    pub supports_refusals: bool,
}
crate::record_serde::wire_record!(DecisionModelInfo {
    id: "id",
    is_default: "isDefault",
    limits: "limits",
    name: "name",
    supports_refusals: "supportsRefusals"
});

/// Default-tier text and image token prices reported for an image model, in AI credits per billing batch. Missing rates remain absent; these are not per-image quotes or actual billed usage.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ImageToolModelPricing {
    /// Number of tokens per billing batch.
    pub batch_size: i64,
    /// Distinct AI credits per batch of image-input tokens, when reported, including zero.
    pub image_input_price: Option<f64>,
    /// Distinct AI credits per batch of image-output tokens, when reported, including zero.
    pub image_output_price: Option<f64>,
    /// AI credits per batch of text-input tokens, including a reported zero.
    pub text_input_price: Option<f64>,
    /// AI credits per batch of text-output tokens, not generated image tokens.
    pub text_output_price: Option<f64>,
}
crate::record_serde::wire_record!(ImageToolModelPricing { batch_size: "batchSize", image_input_price: "imageInputPrice" if is_none, image_output_price: "imageOutputPrice" if is_none, text_input_price: "textInputPrice" if is_none, text_output_price: "textOutputPrice" if is_none });

/// The runtime-selected image model and its account availability. Availability is independent of per-session image-generation enablement and does not override session policy or permissions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ImageToolModel {
    /// Whether the resolved account can use this image model. This is not the session's enabled setting.
    pub availability: ImageToolModelAvailability,
    /// Image model identifier selected by the runtime.
    pub id: String,
    /// Whether this is the model the runtime currently uses for its native image tool.
    pub is_default: bool,
    /// Human-readable model name, supplied by the runtime when unavailable from the catalog.
    pub name: String,
    /// Catalog-reported token prices. Absent means prices were not reported.
    pub pricing: Option<ImageToolModelPricing>,
    /// Image operations supported by the model's available versioned endpoints.
    pub supported_operations: Vec<ImageToolModelOperation>,
}
crate::record_serde::wire_record!(ImageToolModel { availability: "availability", id: "id", is_default: "isDefault", name: "name", pricing: "pricing" if is_none, supported_operations: "supportedOperations" });

/// Managed, repository, and CLI model overrides to overlay onto the session at startup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelApplyStartupOverlayRequest {
    /// Effective default Auto routing preference from user and managed settings. Applies only to fresh sessions and never replaces a per-session selection.
    pub auto_tier: Option<AutoTier>,
    /// Model explicitly selected by the CLI, when provided.
    pub cli_model: Option<String>,
    /// Whether the overlay is being applied while resuming a deferred session.
    pub deferred_resume: Option<bool>,
    /// Model required by device-managed policy, when configured.
    pub device_managed_model: Option<String>,
    /// Context tier paired with the effective organization-managed model. Applies only when that concrete managed model is selected; it is ignored for Auto and for CLI, resume, or user overrides.
    pub managed_context_tier: Option<String>,
    /// Reasoning effort paired with the effective organization-managed model. Applies only when that concrete managed model is selected; it is ignored for Auto and for CLI, resume, or user overrides.
    pub managed_reasoning_effort: Option<String>,
    /// Startup default model from the enterprise policy helper, when configured. Weakest of the managed sources: it applies only when neither device nor server policy names a model, and an explicit user selection still wins.
    pub policy_helper_model: Option<String>,
    /// Auto routing preference selected by repository settings, when configured. Applied only when the overlay selects the Auto model; beside a concrete model it stays dormant.
    pub repo_auto_tier: Option<String>,
    /// Context tier selected by repository settings, when configured.
    pub repo_context_tier: Option<String>,
    /// Model selected by repository settings, when configured.
    pub repo_model: Option<String>,
    /// Provider owning the repository-selected model. Omitted repository provider identity retains legacy bare-model behavior.
    pub repo_model_provider_id: Option<String>,
    /// Reasoning effort selected by repository settings, when configured.
    pub repo_reasoning_effort: Option<String>,
    /// Model required by server-managed policy, when configured.
    pub server_managed_model: Option<String>,
}
crate::record_serde::wire_record!(ModelApplyStartupOverlayRequest { auto_tier: "autoTier" if is_none, cli_model: "cliModel" if is_none, deferred_resume: "deferredResume" if is_none, device_managed_model: "deviceManagedModel" if is_none, managed_context_tier: "managedContextTier" if is_none, managed_reasoning_effort: "managedReasoningEffort" if is_none, policy_helper_model: "policyHelperModel" if is_none, repo_auto_tier: "repoAutoTier" if is_none, repo_context_tier: "repoContextTier" if is_none, repo_model: "repoModel" if is_none, repo_model_provider_id: "repoModelProviderId" if is_none, repo_reasoning_effort: "repoReasoningEffort" if is_none, server_managed_model: "serverManagedModel" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ModelClearStartupSeedRequest {
    pub expected_model: String,
    /// Provider captured with the startup seed; omission matches only an unqualified seed.
    pub expected_provider_id: Option<String>,
}
crate::record_serde::wire_record!(ModelClearStartupSeedRequest { expected_model: "expectedModel", expected_provider_id: "expectedProviderId" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ModelClearStartupSeedResult {
    pub cleared: bool,
}
crate::record_serde::wire_record!(ModelClearStartupSeedResult { cleared: "cleared" });

/// Options for the client's session-authenticated description catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelDescribeClientCatalogRequest {
    /// Bypass the existing catalog cache. The default is false.
    pub skip_cache: Option<bool>,
}
crate::record_serde::wire_record!(ModelDescribeClientCatalogRequest { skip_cache: "skipCache" if is_none });

/// Raw CAPI model descriptions for client-side catalog enrichment.
/// Unlike `models.list` and `session.model.list`, this result does not normalize model fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelDescribeClientCatalogResult {
    /// Complete model descriptions with their original CAPI field names.
    /// These include `billing.token_prices`, `capabilities.supports.tool_calls`, and snake_case token limits.
    pub models: Vec<serde_json::Value>,
}
crate::record_serde::wire_record!(ModelDescribeClientCatalogResult { models: "models" });

/// Conversation and changes for a utility-model branch slug.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ModelGenerateBranchSlugRequest {
    /// Summary of the changes the branch will contain.
    pub change_summary: String,
    /// Conversation messages in their original order.
    pub conversation: Vec<String>,
    /// Prefix for the established derived-name fallback.
    pub fallback_prefix: String,
    /// Original task, when available.
    pub task_prompt: Option<String>,
}
crate::record_serde::wire_record!(ModelGenerateBranchSlugRequest { change_summary: "changeSummary", conversation: "conversation", fallback_prefix: "fallbackPrefix", task_prompt: "taskPrompt" if is_none });

/// A generated or derived branch slug.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ModelGenerateBranchSlugResult {
    /// Branch slug under the runtime's existing fallback policy.
    pub slug: String,
}
crate::record_serde::wire_record!(ModelGenerateBranchSlugResult { slug: "slug" });

/// Host presentation state the reasoning-summaries policy depends on.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelGetReasoningSummariesRequest {
    /// The host's persisted `showReasoning` user setting, omitted when unset.
    pub persisted_show_reasoning: Option<bool>,
    /// Whether the host is currently showing reasoning summaries. Defaults to false.
    pub summaries_visible: Option<bool>,
}
crate::record_serde::wire_record!(ModelGetReasoningSummariesRequest { persisted_show_reasoning: "persistedShowReasoning" if is_none, summaries_visible: "summariesVisible" if is_none });

/// Reasoning-summaries presentation policy for the session's current model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelGetReasoningSummariesResult {
    /// Whether the supplied `persistedShowReasoning` setting only restates the model default. This reports a redundancy, not a standing instruction to delete the setting: it stays true for as long as the setting and the model default agree, including for a user who later chose that value deliberately. It exists for a one-time migration of a legacy setting written before the model default made it redundant, so a host that acts on it owns the cleanup and must gate it behind its own persisted `done` marker, delete only a setting whose value still matches the one it read, and never repeat the deletion on a later launch. Under an off-by-default reasoning-summaries experiment an explicit `showReasoning: true` opt-in also reads as redundant on a summary-producing model, so an ungated host would keep deleting that opt-in and hide summaries again on every launch.
    pub clear_redundant_setting: bool,
    /// Whether the session's reasoning-summaries experiment assignment has settled. False means this read does not wait for a pending assignment and reports the experiment-derived fields conservatively: `showHiddenHint` is false and `visibility` falls back to `persistedShowReasoning`, or is omitted when none was supplied. A host that wants the assigned answer re-reads until this is true.
    pub experiment_settled: bool,
    /// Whether the selected model shows reasoning summaries by default.
    pub model_default_shows_summaries: bool,
    /// False until the session has a model catalog and a selected model. This read never resolves the catalog itself, and the runtime emits no readiness notification, so repeating this call alone leaves `ready` false indefinitely on a session that has neither listed models nor run a turn. `session.model.list` is the prerequisite and the signal: it resolves and caches the catalog for the session, so a successful return means the next read is ready. The exception is a session that resolves models without a Copilot catalog, that is a BYOK registry or an alternate provider: `session.model.list` returns an empty list without caching a catalog, so `ready` stays false and this policy is unavailable there. While false, every model-policy field is false and `visibility` is omitted; `experimentSettled` is independent of readiness and still reports the assignment state.
    pub ready: bool,
    /// Whether turns on this session produce reasoning summaries, honouring an explicit session reasoning-summary override.
    pub session_shows_summaries: bool,
    /// Whether to show the hint that reasoning summaries are hidden. Always false while `experimentSettled` is false.
    pub show_hidden_hint: bool,
    /// Whether the selected model emits reasoning headers.
    pub show_reasoning_headers: bool,
    /// Default reasoning visibility, and only meaningful once `ready` is true: false when the session produces no summaries, else the persisted setting, else the experiment's default. While `ready` is true it is omitted only when all three are unavailable, that is when the session produces summaries, no `persistedShowReasoning` was supplied, and `experimentSettled` is false. While `ready` is false there is no model policy to report and this field is always omitted, so an absent value alone does not imply a pending experiment.
    pub visibility: Option<bool>,
}
crate::record_serde::wire_record!(ModelGetReasoningSummariesResult { clear_redundant_setting: "clearRedundantSetting", experiment_settled: "experimentSettled", model_default_shows_summaries: "modelDefaultShowsSummaries", ready: "ready", session_shows_summaries: "sessionShowsSummaries", show_hidden_hint: "showHiddenHint", show_reasoning_headers: "showReasoningHeaders", visibility: "visibility" if is_none });

/// List of Copilot models available to the resolved user, including capabilities and billing metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelList {
    /// Ordered Auto routing preferences discovered for the resolved account. Present only
    /// when CAPI returned Auto metadata; same shape and semantics as `SessionModelList.auto`.
    pub auto: Option<AutoTierMetadata>,
    /// Decision models the resolved account can call with `decisions.ask`, separate from conversation models, with each model's effective limits. Absent when the `DECISIONS_API` feature flag is off or the runtime does not expose this catalog; empty when the account has none. These entries must not be selected as chat models.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub decision_models: Option<Vec<DecisionModelInfo>>,
    /// The runtime's default image model, separate from conversation models, with its availability for the resolved account. The default is reported even when unavailable; clients choose whether to display it. These entries must not be selected as chat models. Absent on runtimes that do not expose this catalog.
    pub image_models: Option<Vec<ImageToolModel>>,
    /// List of available models with full metadata
    pub models: Vec<Model>,
}
crate::record_serde::wire_record!(ModelList { auto: "auto" if is_none, decision_models: "decisionModels" if is_none, image_models: "imageModels" if is_none, models: "models" });

/// Optional listing options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelListRequest {
    /// If true, bypasses the per-session model list cache and re-fetches from CAPI.
    pub skip_cache: Option<bool>,
}
crate::record_serde::wire_record!(ModelListRequest { skip_cache: "skipCache" if is_none });

/// Model-catalog failure facts without host-specific presentation text.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelLoadErrorDetails {
    /// Classified authorization failure for an HTTP 403 response, when recognized.
    pub authorization: Option<ModelLoadAuthorizationError>,
    /// Request identifier extracted from the provider error.
    pub request_id: Option<RequestId>,
    /// Service request identifier extracted from the provider error.
    pub service_request_id: Option<String>,
    /// Copilot settings URL for the session's account when a policy refusal needs remediation.
    pub settings_url: Option<String>,
    /// HTTP status reported by the provider, if available.
    pub status: Option<i64>,
}
crate::record_serde::wire_record!(ModelLoadErrorDetails { authorization: "authorization" if is_none, request_id: "requestId" if is_none, service_request_id: "serviceRequestId" if is_none, settings_url: "settingsUrl" if is_none, status: "status" if is_none });

/// A serialized provider error from a failed model-catalog request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelLoadErrorRequest {
    /// Provider error JSON, when the catalog supplied structured error details.
    pub wire_error_json: Option<String>,
}
crate::record_serde::wire_record!(ModelLoadErrorRequest { wire_error_json: "wireErrorJson" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelPickerPersistenceRequest {
    /// Whether context tier was explicitly selected and should be persisted.
    pub context_tier_explicit: Option<bool>,
    /// Whether reasoning effort was explicitly selected and should be persisted.
    pub reasoning_effort_explicit: Option<bool>,
    /// Filesystem and environment context used to resolve settings persistence.
    pub settings_context: ModelPickerSettingsContext,
}
crate::record_serde::wire_record!(ModelPickerPersistenceRequest { context_tier_explicit: "contextTierExplicit" if is_none, reasoning_effort_explicit: "reasoningEffortExplicit" if is_none, settings_context: "settingsContext" });

/// Normalized model-provider adapter definitions available to the session, not discovered instances.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAdapterCatalog {
    /// Available provider adapters ordered by adapterId.
    pub providers: Vec<ModelProviderAdapterDescriptor>,
}
crate::record_serde::wire_record!(ModelProviderAdapterCatalog {
    providers: "providers"
});

/// Reads current cached availability without fetching a remote model catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAvailabilityRequest {
    /// Optional provider identity used to disambiguate a selection.
    pub provider_id: Option<String>,
    /// Provider-qualified model selection identifier.
    pub selection_id: String,
}
crate::record_serde::wire_record!(ModelProviderAvailabilityRequest { provider_id: "providerId" if is_none, selection_id: "selectionId" });

/// Action-boundary availability of a registered provider model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderAvailabilityResult {
    /// True for a selectable model, false for a blocked model, absent for a missing row.
    pub available: Option<bool>,
}
crate::record_serde::wire_record!(ModelProviderAvailabilityResult { available: "available" if is_none });

/// Provider configuration prepared from a discovered model. Preparing a plan changes nothing: it neither registers the model with the session nor writes durable configuration. To apply it, pass `provider` and `model` to `session.provider.add`, omitting whichever the dispositions report as already configured.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderConfigurationPlan {
    /// Model definition prepared from the discovered model. Capability fields the provider did not report stay omitted rather than being asserted false.
    pub model: ProviderModelConfig,
    /// Whether `model` still needs to be registered. When `alreadyConfigured`, `selectionId` is already registered and the caller can select it without adding anything.
    pub model_disposition: ModelProviderConfigurationDisposition,
    /// Provider connection prepared from the instance's inference metadata. Carries no credential; supply one if the endpoint requires it.
    pub provider: NamedProviderConfig,
    /// Whether `provider` still needs to be registered. When `alreadyConfigured`, a provider with the same endpoint is already registered and `provider` restates it under its existing name; omit it when applying the plan.
    pub provider_disposition: ModelProviderConfigurationDisposition,
    /// Provider-qualified selection id (`provider/id`) to pass to `switchTo` once the plan is applied.
    pub selection_id: String,
    /// Non-fatal warnings carried over from the discovered model, such as capabilities the provider did not report.
    pub warnings: Vec<ModelProviderWarning>,
}
crate::record_serde::wire_record!(ModelProviderConfigurationPlan {
    model: "model",
    model_disposition: "modelDisposition",
    provider: "provider",
    provider_disposition: "providerDisposition",
    selection_id: "selectionId",
    warnings: "warnings"
});

/// Provider discovery parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderDiscoverRequest {
    /// Opaque adapter identity returned by `session.providers.getCatalog`.
    pub adapter_id: String,
    /// Provider-specific JSON input. Omission or null selects adapter defaults unless requiresInput is true. Non-null input is validated against the advertised Draft 7 schema when present; otherwise validation belongs to the adapter.
    pub input: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(ModelProviderDiscoverRequest { adapter_id: "adapterId", input: "input" if is_none });

/// Provider instances found by a discovery operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderDiscoverResult {
    /// Discovered provider instances. Empty when passive default discovery finds no reachable provider.
    pub instances: Vec<ModelProviderInstance>,
    /// Typed operation outcome. Passive discovery can return `absent` with an empty instance list.
    pub outcome: ModelProviderOperationOutcome,
}
crate::record_serde::wire_record!(ModelProviderDiscoverResult {
    instances: "instances",
    outcome: "outcome"
});

/// Provider status request parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderGetStatusRequest {
    /// Provider instance reference returned by discovery.
    pub instance: ModelProviderInstanceReference,
}
crate::record_serde::wire_record!(ModelProviderGetStatusRequest {
    instance: "instance"
});

/// Provider model inventory request parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderModelsListRequest {
    /// Provider instance reference returned by discovery.
    pub instance: ModelProviderInstanceReference,
}
crate::record_serde::wire_record!(ModelProviderModelsListRequest {
    instance: "instance"
});

/// A discovered instance and one of its models to translate into provider configuration. Pass back the instance and model as returned by `session.providers.discover` and `session.providers.models.list`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderPrepareConfigurationRequest {
    /// The discovered instance that serves the model.
    pub instance: ModelProviderInstance,
    /// The discovered model to configure.
    pub model: DiscoveredModel,
}
crate::record_serde::wire_record!(ModelProviderPrepareConfigurationRequest {
    instance: "instance",
    model: "model"
});

/// Current health information for a provider instance.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderStatus {
    /// Normalized provider instance.
    pub instance: ModelProviderInstance,
    /// Typed operation outcome.
    pub outcome: ModelProviderOperationOutcome,
    /// Open provider status value, such as `healthy`, `unreachable`, or `notInstalled`.
    pub status: String,
    /// Provider-reported version.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(ModelProviderStatus { instance: "instance", outcome: "outcome", status: "status", version: "version" if is_none });

/// Host-supplied exact model selection IDs to allow for this running session. CAPI IDs are intersected with repository `.github/allowed_models.txt` policy; provider-qualified IDs remain exempt from repository-only policy but are restricted by this host list. Omit or pass null to clear the host restriction; an explicit empty or disjoint list is rejected. Validation and pre-selection fallback failures preserve the previous restriction. Failures after a fallback selection commits retain the new restriction and selected model; callers should inspect current session state after such an error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSetAllowedModelsRequest {
    /// Exact model IDs to permit, or null to clear the host restriction.
    pub allowed_models: Option<Vec<String>>,
}
crate::record_serde::wire_record!(ModelSetAllowedModelsRequest { allowed_models: "allowedModels" if is_none });

/// The applied host allowlist and effective session model policy after intersection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSetAllowedModelsResult {
    /// Normalized host allowlist. Omitted when the host restriction was cleared, or when a relay client does not return the host policy.
    pub allowed_models: Option<Vec<String>>,
    /// Effective exact IDs or repository policy patterns after applying the host restriction. Omitted by relay clients that do not return the host policy.
    pub effective_allowed_models: Option<Vec<String>>,
    /// Effective deterministic fallback model, when the policy defines one.
    pub fallback_model: Option<String>,
    /// Selected session model after reconciling a now-disallowed concrete selection.
    pub model_id: Option<String>,
}
crate::record_serde::wire_record!(ModelSetAllowedModelsResult { allowed_models: "allowedModels" if is_none, effective_allowed_models: "effectiveAllowedModels" if is_none, fallback_model: "fallbackModel" if is_none, model_id: "modelId" if is_none });

/// Reasoning effort level to apply to the currently selected model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSetReasoningEffortRequest {
    /// Reasoning effort level to apply to the currently selected model. The host is responsible for validating the value against the model's supported levels before calling.
    pub reasoning_effort: String,
}
crate::record_serde::wire_record!(ModelSetReasoningEffortRequest {
    reasoning_effort: "reasoningEffort"
});

/// Update the session's reasoning effort without changing the selected model. Use `switchTo` instead when you also need to change the model. The runtime applies the effort to the next new model request, including tool continuations, without changing retries of an already-started request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSetReasoningEffortResult {
    /// Reasoning effort level recorded on the session after the update
    pub reasoning_effort: String,
}
crate::record_serde::wire_record!(ModelSetReasoningEffortResult {
    reasoning_effort: "reasoningEffort"
});

/// Optional opaque account selection, compatibility GitHub token, or streamer-mode masking used to list models.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelsListRequest {
    /// GitHub token accepted for compatibility with existing SDK clients. When provided, resolves this token instead of using the current account.
    pub git_hub_token: Option<String>,
    /// Opaque account identifier returned by `account.getAllUsers`. When omitted, the current account is used.
    pub selection_id: Option<String>,
    /// When true, replaces the display names of preview and hidden models with `Hidden Model N`, and masks their promotion and data-retention text. Model ids are unchanged.
    pub streamer_mode: Option<bool>,
}
crate::record_serde::wire_record!(ModelsListRequest { git_hub_token: "gitHubToken" if is_none, selection_id: "selectionId" if is_none, streamer_mode: "streamerMode" if is_none });

/// Options for a host's startup model admission check.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelStartupAdmissionRequest {
    /// Claims the session's one-time enablement prompt only when its catalog is ready.
    pub claim_enablement: Option<bool>,
}
crate::record_serde::wire_record!(ModelStartupAdmissionRequest { claim_enablement: "claimEnablement" if is_none });

/// A cached startup model snapshot and an optional one-time enablement prompt. This check never resolves credentials or fetches a catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelStartupAdmissionResult {
    /// Whether the session has a resolved catalog, including a catalog-free provider.
    pub catalog_ready: bool,
    /// Whether this call consumed the session's startup enablement claim.
    pub claimed_enablement: bool,
    /// The selected model to enable, only for the first successful prompt claim.
    pub model_to_enable: Option<String>,
    /// Whether the cached selected model currently requires enablement.
    pub requires_enablement: bool,
    /// The selected model, when the session has one.
    pub selected_model: Option<String>,
}
crate::record_serde::wire_record!(ModelStartupAdmissionResult { catalog_ready: "catalogReady", claimed_enablement: "claimedEnablement", model_to_enable: "modelToEnable" if is_none, requires_enablement: "requiresEnablement", selected_model: "selectedModel" if is_none });

/// An Auto preference request for the session. This updates Auto configuration only; it does not change the selected model to `auto`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchAutoTierRequest {
    /// Auto preference to activate when a future user turn using the `auto` model safely mints a replacement model and token pair. Pass null to return to provider-default Auto routing. For Copilot Fusion, prefer `fusionTier`: the `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces remain accepted but are deprecated.
    pub auto_tier: Option<AutoTier>,
    /// Origin to record on the effective `session.model_change` event. Defaults to `sdk` when omitted.
    pub source: Option<ModelChangeSource>,
}
crate::record_serde::wire_record!(ModelSwitchAutoTierRequest { auto_tier: "autoTier", source: "source" if is_none });

/// Immediate acknowledgement and Auto preference snapshot after a switch request. This result never implies that a pending preference committed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchAutoTierResult {
    /// Auto preference currently claimed by an in-progress activation. Null means the activation is returning to provider-default routing.
    pub activating_auto_tier: Option<AutoTier>,
    /// Auto preference currently committed for the session.
    pub effective_auto_tier: Option<AutoTier>,
    /// Copilot Fusion tier that effectiveAutoTier serves, resolved from the session's Auto tier metadata. An open string: known values include `standard`, `high`, and `max`; other provider-defined values may appear. Omitted when no Auto preference is committed, it is not a Fusion tier, or Auto metadata has not been discovered (for example after cold resume, before session.model.list). Fusion fields cover only effective and pending preferences; resolve activatingAutoTier and supersededAutoTier against auto.tiers[].fusionTier from session.model.list.
    pub effective_fusion_tier: Option<String>,
    /// Latest unclaimed Auto preference waiting for a future user turn.
    pub pending_auto_tier: Option<AutoTier>,
    /// Copilot Fusion tier that pendingAutoTier serves, resolved from the session's Auto tier metadata. An open string: known values include `standard`, `high`, and `max`; other provider-defined values may appear. Omitted when no preference is pending, it is not a Fusion tier, or Auto metadata has not been discovered (for example after cold resume, before session.model.list).
    pub pending_fusion_tier: Option<String>,
    /// Immediate request status. `pending` means accepted but not committed.
    pub status: ModelSwitchAutoTierStatus,
    /// Earlier unclaimed preference replaced by this request. This can be present with either status, including when selecting the effective preference cancels pending work.
    pub superseded_auto_tier: Option<AutoTier>,
}
crate::record_serde::wire_record!(ModelSwitchAutoTierResult { activating_auto_tier: "activatingAutoTier" if is_none, effective_auto_tier: "effectiveAutoTier" if is_none, effective_fusion_tier: "effectiveFusionTier" if is_none, pending_auto_tier: "pendingAutoTier" if is_none, pending_fusion_tier: "pendingFusionTier" if is_none, status: "status", superseded_auto_tier: "supersededAutoTier" if is_none });

/// A Copilot Fusion tier request for the session, named as the CLI's `--fusion-tier` names it. The runtime resolves the tier to the Auto routing preference it stands for and requests that preference exactly as `switchAutoTier` does. This updates Auto configuration only; it does not change the selected model to `auto`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchFusionTierRequest {
    /// Copilot Fusion tier to activate when a future user turn using the `auto` model safely mints a replacement model and token pair. Use `switchAutoTier` to return to provider-default Auto routing.
    pub fusion_tier: FusionTier,
    /// Origin to record on the effective `session.model_change` event. Defaults to `sdk` when omitted.
    pub source: Option<ModelChangeSource>,
}
crate::record_serde::wire_record!(ModelSwitchFusionTierRequest { fusion_tier: "fusionTier", source: "source" if is_none });

/// Target model identifier and optional reasoning effort, summary, capability overrides, and context tier.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchToRequest {
    /// Optional Auto routing preference to stage atomically with selecting `auto`. Pass null to return to provider-default Auto routing. This field is rejected when `modelId` is not `auto`. For Copilot Fusion, prefer `fusionTier`: the `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces remain accepted but are deprecated.
    pub auto_tier: Option<AutoTier>,
    /// Explicit response to a model-switch compaction preflight. Omit to request a confirmation projection when compaction is necessary.
    pub compaction_decision: Option<String>,
    /// Explicit context tier for the selected model. `"default"` / `"long_context"` apply the requested tier; omit this field to use normal model behavior with no explicit tier.
    pub context_tier: Option<ContextTier>,
    /// When true, defer this switch (enqueue it) if another model change is already queued, even when no turn is active — so it drains last (FIFO) and wins over the already-queued change. Intended for genuine user-initiated model selections; internal restore/reapply switches omit it and apply immediately when no turn is active. When no other model change is queued this has no effect (a switch still applies immediately unless a turn is active).
    pub defer_if_model_change_queued: Option<bool>,
    /// Optional Copilot Fusion tier to stage atomically with selecting `fusion` or `auto`, matching the CLI's `--fusion-tier`. The runtime resolves it to the Auto routing preference it stands for and stages that preference exactly like `autoTier`. Cannot be combined with `autoTier`, and is rejected when `modelId` is not `fusion` or `auto`.
    pub fusion_tier: Option<FusionTier>,
    /// Override individual model capabilities resolved by the runtime
    pub model_capabilities: Option<ModelCapabilitiesOverride>,
    /// Settings scope used when persisting the selected model.
    pub model_change_scope: Option<String>,
    /// Model id to switch to, as returned by `list`. Include providerId to select an exact catalog entry when providers share the id. Without providerId, a bare id retains incumbent selection behavior; a registry-qualified id (`provider/id`, e.g. `acme/claude-sonnet`) targets a BYOK model.
    pub model_id: String,
    /// Optional settings context and explicit-override flags used to persist a picker selection.
    pub picker_persistence: Option<ModelPickerPersistenceRequest>,
    /// Provider id from the selected list entry's provider reference. Selects this exact provider/model pair; an unavailable pair fails rather than using another provider. Omit for deterministic legacy bare-model selection.
    pub provider_id: Option<String>,
    /// Reasoning effort level to use for the model. CAPI values are model-defined and validated against the selected model; BYOK providers may define additional values. "none" disables reasoning. Pass null to clear any session effort override and fall back to the model's default. When omitted, the session's current effort is kept.
    pub reasoning_effort: Option<String>,
    /// Reasoning summary mode to request for supported model clients
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Optional repository settings scope to persist after the switch commits.
    pub repo_scope: Option<String>,
    /// Require the target to be currently available and enabled before applying the switch.
    pub require_available: Option<bool>,
    /// When true, evaluate context-window compaction policy before applying the switch, including effort-only and unchanged selections while idle. An effort-only change applies immediately without compaction when a turn is active, including when the turn starts during preflight; an explicit `compactionDecision: "cancel"` still cancels the change. Other switches retain preflight and may be deferred until the active turn ends.
    pub run_compaction_preflight: Option<bool>,
    /// Origin to record on the effective `session.model_change` event for trusted in-process calls. Transport SDK calls are always recorded as `sdk`, regardless of this value.
    pub source: Option<ModelChangeSource>,
    /// Output verbosity level to request for supported models
    pub verbosity: Option<Verbosity>,
}
crate::record_serde::wire_record!(ModelSwitchToRequest { auto_tier: "autoTier" if is_none, compaction_decision: "compactionDecision" if is_none, context_tier: "contextTier" if is_none, defer_if_model_change_queued: "deferIfModelChangeQueued" if is_none, fusion_tier: "fusionTier" if is_none, model_capabilities: "modelCapabilities" if is_none, model_change_scope: "modelChangeScope" if is_none, model_id: "modelId", picker_persistence: "pickerPersistence" if is_none, provider_id: "providerId" if is_none, reasoning_effort: "reasoningEffort" if is_none, reasoning_summary: "reasoningSummary" if is_none, repo_scope: "repoScope" if is_none, require_available: "requireAvailable" if is_none, run_compaction_preflight: "runCompactionPreflight" if is_none, source: "source" if is_none, verbosity: "verbosity" if is_none });

/// The model identifier active on the session after the switch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelSwitchToResult {
    /// Compaction confirmation projection when status is confirmation_required
    pub confirmation: Option<ModelSwitchConfirmation>,
    /// True when the switch was deferred (enqueued as a cancellable `/model` command) because a turn was active or another model change was already queued, rather than applied immediately. When true, the session's live model is unchanged until the queued change drains.
    pub deferred: Option<bool>,
    /// Deprecation warnings associated with the selected model or options.
    pub deprecation_warnings: Option<Vec<String>>,
    /// User-facing outcome message for the model switch.
    pub message: Option<String>,
    /// Currently active model identifier after the switch
    pub model_id: Option<String>,
    /// Authoritative model and Auto preference state after an immediate switch. For deferred switches this remains the current state until the queued change drains.
    pub model_state: Option<CurrentModel>,
    /// Persistence failure encountered after applying the model switch.
    pub persistence_error: Option<String>,
    /// Stable queue item identifier when this request was enqueued. Remains present if the item drains before the response is returned.
    pub queue_id: Option<String>,
    /// Lifecycle result for the requested switch
    pub status: Option<String>,
    /// User-facing warning produced while applying the model switch.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(ModelSwitchToResult { confirmation: "confirmation" if is_none, deferred: "deferred" if is_none, deprecation_warnings: "deprecationWarnings" if is_none, message: "message" if is_none, model_id: "modelId" if is_none, model_state: "modelState" if is_none, persistence_error: "persistenceError" if is_none, queue_id: "queueId" if is_none, status: "status" if is_none, warning: "warning" if is_none });

/// The unit one model's published token prices are quoted in.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelTokenPriceUnit {
    /// Model ID the unit applies to, matching the id of a models.list entry.
    pub model_id: String,
    /// Unit this model's token prices are quoted in.
    pub unit: TokenPriceUnit,
}
crate::record_serde::wire_record!(ModelTokenPriceUnit {
    model_id: "modelId",
    unit: "unit"
});

/// Price units for every catalog model that publishes token prices.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelTokenPriceUnitList {
    /// One entry per model that publishes token prices, in catalog order. A model with no published prices is omitted.
    pub units: Vec<ModelTokenPriceUnit>,
}
crate::record_serde::wire_record!(ModelTokenPriceUnitList { units: "units" });

/// BYOK providers and/or models to add to the session's registry at runtime. Both fields are optional; provide providers, models, or both.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderAddRequest {
    /// BYOK model definitions to register. Each must reference a provider that is already registered or included in this same call. Selection ids (`provider/id`) must be unique across the registry.
    pub models: Option<Vec<ProviderModelConfig>>,
    /// Named BYOK provider connections to register, additive to the session registry. Names must not contain '/'. An existing name can be supplied only when this call adds new models for it and the connection settings, including headers, match the registered provider.
    pub providers: Option<Vec<NamedProviderConfig>>,
}
crate::record_serde::wire_record!(ProviderAddRequest { models: "models" if is_none, providers: "providers" if is_none });

/// The selectable model entries synthesized for the models added by this call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderAddResult {
    /// Synthesized selectable model entries for the newly added BYOK models, each under its provider-qualified selection id (`provider/id`). Empty when only providers were added.
    pub models: Vec<serde_json::Value>,
}
crate::record_serde::wire_record!(ProviderAddResult { models: "models" });

/// A snapshot of the provider endpoint the session is currently configured to talk to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderEndpoint {
    /// A credential the caller should use with this endpoint. Omitted only when the endpoint accepts unauthenticated requests.
    pub api_key: Option<String>,
    /// Base URL to pass to the LLM client library.
    pub base_url: String,
    /// HTTP headers the caller must include on every outbound request.
    pub headers: HashMap<String, String>,
    /// Short-lived, rotating credential the caller must send on every request, in addition to `apiKey` if one is present. Omitted when the endpoint does not require one.
    pub session_token: Option<ProviderSessionToken>,
    /// Transport to be used for provider requests.
    pub transport: Option<ProviderEndpointTransport>,
    /// Provider family. Matches the `type` field of a BYOK provider config.
    pub r#type: ProviderEndpointType,
    /// Wire API to be used, when required for the provider type.
    pub wire_api: Option<ProviderEndpointWireApi>,
}
crate::record_serde::wire_record!(ProviderEndpoint { api_key: "apiKey" if is_none, base_url: "baseUrl", headers: "headers", session_token: "sessionToken" if is_none, transport: "transport" if is_none, r#type: "type", wire_api: "wireApi" if is_none });

/// Optional model identifier to scope the endpoint snapshot to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderGetEndpointRequest {
    /// Model identifier the caller intends to use against the returned endpoint. Used to pick the correct wire shape. Omit to use whichever model the session is currently using.
    pub model_id: Option<String>,
}
crate::record_serde::wire_record!(ProviderGetEndpointRequest { model_id: "modelId" if is_none });

/// Credential-free provider configuration status for the selected model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderStatus {
    /// Whether the selected model uses a custom provider instead of requiring GitHub authentication.
    pub has_custom_provider: bool,
}
crate::record_serde::wire_record!(ProviderStatus {
    has_custom_provider: "hasCustomProvider"
});

/// Authoritative BYOK provider and model registry snapshot to apply atomically to the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderSyncRequest {
    /// BYOK model definition snapshot. Models absent from this list are removed.
    pub models: Option<Vec<ProviderModelConfig>>,
    /// Named BYOK provider connection snapshot. Providers absent from this list are removed.
    pub providers: Option<Vec<NamedProviderConfig>>,
}
crate::record_serde::wire_record!(ProviderSyncRequest { models: "models" if is_none, providers: "providers" if is_none });

/// The selectable model entries and selection ids synthesized for the synchronized BYOK models.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderSyncResult {
    /// True when synchronization withdrew the selected host-managed model, leaving the session with no explicit selection, so ordinary model resolution picks the session default. Synchronization never promotes a surviving host model in its place: publishing a model offers it, and the choice of which model to use stays with the user.
    pub model_deselected: Option<bool>,
    /// Synthesized selectable model entries for the synchronized BYOK models.
    pub models: Vec<serde_json::Value>,
    /// Provider-qualified model selection ids present after synchronization.
    pub selection_ids: Vec<String>,
}
crate::record_serde::wire_record!(ProviderSyncResult { model_deselected: "modelDeselected" if is_none, models: "models", selection_ids: "selectionIds" });

/// Asks the SDK client to acquire a bearer token for a BYOK provider whose config set `hasBearerTokenProvider: true`. Issued by the runtime before each outbound model request; the runtime does no caching, so this is sent once per request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderTokenAcquireRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Explicit name of the BYOK connection needing a token, including when it is the default connection.
    pub provider_name: String,
}
crate::record_serde::wire_record!(ProviderTokenAcquireRequest {
    session_id: "sessionId",
    provider_name: "providerName"
});

/// A bearer token supplied by the SDK client for a BYOK provider. The runtime sets it as `Authorization: Bearer <token>` on the outbound request and does no caching; the SDK consumer owns token caching and refresh.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderTokenAcquireResult {
    /// The bearer token value (without the `Bearer ` prefix).
    pub token: String,
}
crate::record_serde::wire_record!(ProviderTokenAcquireResult { token: "token" });

/// Request to update one provider-local model in a single session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderUpdateModelRequest {
    /// Replacement model definition. Its provider and id identify the session row to replace.
    pub model: ProviderModelConfig,
}
crate::record_serde::wire_record!(ProviderUpdateModelRequest { model: "model" });

/// Request to update one provider's connection settings in a single session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderUpdateProviderRequest {
    /// Replacement provider settings. Credentials and custom headers are not accepted in this session update.
    pub provider: SavedProviderConfigurationProviderInput,
}
crate::record_serde::wire_record!(ProviderUpdateProviderRequest {
    provider: "provider"
});

/// Host-managed model selection ids to withdraw from the session's BYOK registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderWithdrawRequest {
    /// Provider-qualified selection ids to withdraw. Ids that are not registered are ignored, so withdrawal is idempotent. A provider left with no models referencing it is removed too, unless it is the session's default provider.
    pub models: Vec<String>,
}
crate::record_serde::wire_record!(ProviderWithdrawRequest { models: "models" });

/// What the withdrawal actually removed from the registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderWithdrawResult {
    /// True when withdrawal removed the selected host-managed model, leaving the session with no explicit selection, so ordinary model resolution picks the session default. Withdrawal never promotes a surviving model in its place: the choice of which model to use stays with the user.
    pub model_deselected: Option<bool>,
    /// Providers removed because one of the withdrawn models was the last entry referencing them. The default provider and providers that merely have no models are not removed.
    pub providers_removed: Vec<String>,
    /// Selection ids that were registered and are now withdrawn. Excludes requested ids that were not present.
    pub withdrawn: Vec<String>,
}
crate::record_serde::wire_record!(ProviderWithdrawResult { model_deselected: "modelDeselected" if is_none, providers_removed: "providersRemoved", withdrawn: "withdrawn" });

/// Saved configuration. Reading does not activate, select, or load a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersConfigReadResult {
    /// Saved model definitions in file order.
    pub models: Vec<ProviderModelConfig>,
    /// Providers in saved order.
    pub providers: Vec<SavedProviderConfigurationProvider>,
}
crate::record_serde::wire_record!(ProvidersConfigReadResult {
    models: "models",
    providers: "providers"
});

/// Result of updating shared saved provider settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersConfigUpdateProviderResult {
    /// Name of the provider whose settings were updated.
    pub provider_name: String,
}
crate::record_serde::wire_record!(ProvidersConfigUpdateProviderResult {
    provider_name: "providerName"
});

/// Result of updating one saved provider model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersConfigUpdateModelResult {
    /// Provider-local id of the updated model.
    pub model_id: String,
    /// Name of the provider that owns the updated model.
    pub provider_name: String,
}
crate::record_serde::wire_record!(ProvidersConfigUpdateModelResult {
    model_id: "modelId",
    provider_name: "providerName"
});

/// Result of removing one saved provider model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersConfigRemoveModelResult {
    /// True when removing the final model also removed its provider and managed credential.
    pub provider_removed: bool,
}
crate::record_serde::wire_record!(ProvidersConfigRemoveModelResult {
    provider_removed: "providerRemoved"
});

/// Result of adding a model to the saved provider configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersConfigAddModelResult {
    /// True when this operation created the provider row.
    pub provider_added: bool,
}
crate::record_serde::wire_record!(ProvidersConfigAddModelResult {
    provider_added: "providerAdded"
});

/// List of Copilot models available to the resolved user, including capabilities and billing metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelsListResult {
    /// Ordered Auto routing preferences discovered for the resolved account. Present only
    /// when CAPI returned Auto metadata; same shape and semantics as `SessionModelList.auto`.
    pub auto: Option<AutoTierMetadata>,
    /// Decision models the resolved account can call with `decisions.ask`, separate from conversation models, with each model's effective limits. Absent when the `DECISIONS_API` feature flag is off or the runtime does not expose this catalog; empty when the account has none. These entries must not be selected as chat models.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub decision_models: Option<Vec<DecisionModelInfo>>,
    /// The runtime's default image model, separate from conversation models, with its availability for the resolved account. The default is reported even when unavailable; clients choose whether to display it. These entries must not be selected as chat models. Absent on runtimes that do not expose this catalog.
    pub image_models: Option<Vec<ImageToolModel>>,
    /// List of available models with full metadata
    pub models: Vec<Model>,
}
crate::record_serde::wire_record!(ModelsListResult { auto: "auto" if is_none, decision_models: "decisionModels" if is_none, image_models: "imageModels" if is_none, models: "models" });

/// The running runtime's complete catalog of well-known built-in model IDs, including supported models and additional IDs with built-in metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelsGetBuiltInCatalogResult {
    /// Built-in model entries.
    pub models: Vec<BuiltInModelCatalogEntry>,
}
crate::record_serde::wire_record!(ModelsGetBuiltInCatalogResult { models: "models" });

/// Price units for every catalog model that publishes token prices.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelsListTokenPriceUnitsResult {
    /// One entry per model that publishes token prices, in catalog order. A model with no published prices is omitted.
    pub units: Vec<ModelTokenPriceUnit>,
}
crate::record_serde::wire_record!(ModelsListTokenPriceUnitsResult { units: "units" });

/// Result of adding a model to the saved provider configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersSourceAddModelResult {
    /// True when this operation created the provider row.
    pub provider_added: bool,
}
crate::record_serde::wire_record!(ProvidersSourceAddModelResult {
    provider_added: "providerAdded"
});

/// A bearer token supplied by the SDK client for a BYOK provider. The runtime sets it as `Authorization: Bearer <token>` on the outbound request and does no caching; the SDK consumer owns token caching and refresh.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderTokenGetTokenResult {
    /// The bearer token value (without the `Bearer ` prefix).
    pub token: String,
}
crate::record_serde::wire_record!(ProviderTokenGetTokenResult { token: "token" });

/// Account availability of the runtime image model. Discovery errors are returned as errors, not converted to unavailable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ImageToolModelAvailability {
    /// The account has access to a supported image operation.
    #[serde(rename = "available")]
    Available,
    /// The runtime default is not available to this account; enabling the tool cannot grant access.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ImageToolModelAvailability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ImageToolModelAvailability",
            &["available", "unavailable", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Available,
            1 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// A supported image-service operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ImageToolModelOperation {
    /// Generate an image from a text prompt.
    #[serde(rename = "generate")]
    Generate,
    /// Generate or edit an image using explicitly selected image inputs.
    #[serde(rename = "edit")]
    Edit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ImageToolModelOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ImageToolModelOperation",
            &["generate", "edit", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Generate,
            1 => Self::Edit,
            _ => Self::Unknown,
        })
    }
}

/// Unit the catalog publishes a model's token prices in. The numeric fields of ModelBillingTokenPrices carry no unit of their own, so a client that renders a price needs this to scale it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TokenPriceUnit {
    /// Prices are already quoted in AI Credits and need no conversion.
    #[serde(rename = "ai_credits")]
    AiCredits,
    /// Prices are quoted in nano-AIU; divide by 1,000,000,000 to get AI Credits.
    #[serde(rename = "nano_aiu")]
    NanoAiu,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for TokenPriceUnit {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "TokenPriceUnit",
            &["ai_credits", "nano_aiu", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AiCredits,
            1 => Self::NanoAiu,
            _ => Self::Unknown,
        })
    }
}
