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
    TaskCompletionOutcome, UsageGetMetricsResult, UserInputImageAttachment,
    UserToolSessionApproval, Verbosity,
};

use super::sdk_api_shared::*;
use crate::generated::{lifecycle_diagnostics, session_events};
use crate::types::{RequestId, SessionEvent, SessionId};

/// Parameters for cooperatively aborting a workflow body.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAbortRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Workflow run identifier.
    pub run_id: String,
    /// Opaque token identifying the execution attempt to abort.
    pub execution_token: String,
}
crate::record_serde::wire_record!(WorkflowAbortRequest {
    session_id: "sessionId",
    run_id: "runId",
    execution_token: "executionToken"
});

/// Acknowledgement that a workflow request was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkflowAckResult {}

/// Options for one workflow-scoped subagent call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentOptions {
    /// Optional built-in or custom agent name whose definition configures the subagent.
    pub agent: Option<String>,
    /// Optional context tier override for the subagent.
    pub context_tier: Option<ContextTier>,
    /// Optional label distinguishing otherwise identical memoized agent calls.
    pub label: Option<String>,
    /// Optional model identifier for the subagent.
    pub model: Option<String>,
    /// Optional reasoning effort override for the subagent.
    pub reasoning_effort: Option<String>,
    /// Optional JSON Schema for structured agent output.
    pub schema: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkflowAgentOptions { agent: "agent" if is_none, context_tier: "contextTier" if is_none, label: "label" if is_none, model: "model" if is_none, reasoning_effort: "reasoningEffort" if is_none, schema: "schema" if is_none });

/// Parameters for one workflow-scoped subagent call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentRequest {
    /// Opaque token identifying the current workflow execution attempt.
    pub execution_token: String,
    /// Subagent execution options.
    pub opts: WorkflowAgentOptions,
    /// Prompt to send to the subagent.
    pub prompt: String,
    /// Workflow run identifier that owns the subagent.
    pub workflow_run_id: String,
}
crate::record_serde::wire_record!(WorkflowAgentRequest {
    execution_token: "executionToken",
    opts: "opts",
    prompt: "prompt",
    workflow_run_id: "workflowRunId"
});

/// Result of one workflow-scoped subagent call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowAgentResult {
    /// Agent result, omitted when the agent produced no result.
    pub result: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkflowAgentResult { result: "result" if is_none });

/// Parameters for cancelling a workflow run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowCancelRequest {
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowCancelRequest { run_id: "runId" });

/// Parameters sent to the owning extension to execute a workflow closure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowExecuteRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Registered workflow name.
    pub name: String,
    /// Workflow run identifier.
    pub run_id: String,
    /// Opaque token identifying this workflow execution attempt.
    pub execution_token: String,
    /// Workflow input value.
    pub args: serde_json::Value,
}
crate::record_serde::wire_record!(WorkflowExecuteRequest {
    session_id: "sessionId",
    name: "name",
    run_id: "runId",
    execution_token: "executionToken",
    args: "args"
});

/// Result returned by an extension workflow closure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowExecuteResult {
    /// Workflow result value.
    pub result: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkflowExecuteResult { result: "result" if is_none });

/// Parameters for paging workflow progress.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowGetRunProgressRequest {
    /// Exclusive forward cursor.
    pub after_seq: Option<i64>,
    /// Exclusive backward cursor.
    pub before_seq: Option<i64>,
    /// Maximum records to return. Defaults to 200 and is capped at 500.
    pub limit: Option<i32>,
    /// Optional phase identifier used to scope records and cursors.
    pub phase_id: Option<String>,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowGetRunProgressRequest { after_seq: "afterSeq" if is_none, before_seq: "beforeSeq" if is_none, limit: "limit" if is_none, phase_id: "phaseId" if is_none, run_id: "runId" });

/// Parameters for retrieving a workflow run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowGetRunRequest {
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowGetRunRequest { run_id: "runId" });

/// Parameters for reading a workflow journal entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowJournalGetRequest {
    /// Opaque token identifying the current workflow execution attempt.
    pub execution_token: String,
    /// Namespaced journal key.
    pub key: String,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowJournalGetRequest {
    execution_token: "executionToken",
    key: "key",
    run_id: "runId"
});

/// Result of reading a workflow journal entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowJournalGetResult {
    /// Whether the journal contained the requested key.
    pub hit: bool,
    /// Cached JSON result. The hit field distinguishes a cached JSON null from a miss.
    pub result_json: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(WorkflowJournalGetResult { hit: "hit", result_json: "resultJson" if is_none });

/// Parameters for storing a workflow journal entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowJournalPutRequest {
    /// Opaque token identifying the current workflow execution attempt.
    pub execution_token: String,
    /// Namespaced journal key.
    pub key: String,
    /// JSON result to memoize.
    pub result_json: serde_json::Value,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowJournalPutRequest {
    execution_token: "executionToken",
    key: "key",
    result_json: "resultJson",
    run_id: "runId"
});

/// Parameters for paging workflow runs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowListRunsRequest {
    /// Exclusive forward cursor.
    pub after_seq: Option<i64>,
    /// Exclusive backward cursor.
    pub before_seq: Option<i64>,
    /// Maximum terminal runs to return. Defaults to 200 and is capped at 500.
    pub limit: Option<i32>,
}
crate::record_serde::wire_record!(WorkflowListRunsRequest { after_seq: "afterSeq" if is_none, before_seq: "beforeSeq" if is_none, limit: "limit" if is_none });

/// A page of workflow runs in durable creation order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowListRunsResult {
    /// Whether terminal runs newer than this page exist.
    pub has_more_newer: Option<bool>,
    /// Newest terminal-run cursor in this page, or null when the terminal window is empty.
    pub newest_seq: Option<i64>,
    /// Oldest terminal-run cursor in this page, or null when the terminal window is empty.
    pub oldest_seq: Option<i64>,
    /// Number of terminal runs older than this page.
    pub omitted_older: Option<i64>,
    /// Workflow run summaries in durable creation order.
    pub runs: Vec<WorkflowRunSummary>,
}
crate::record_serde::wire_record!(WorkflowListRunsResult { has_more_newer: "hasMoreNewer" if is_none, newest_seq: "newestSeq" if is_none, oldest_seq: "oldestSeq" if is_none, omitted_older: "omittedOlder" if is_none, runs: "runs" });

/// One ordered workflow progress line.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowLogLine {
    /// Progress line kind.
    pub kind: WorkflowLogLineKind,
    /// Monotonic sequence number within the workflow run.
    pub seq: i64,
    /// Progress text.
    pub text: String,
}
crate::record_serde::wire_record!(WorkflowLogLine {
    kind: "kind",
    seq: "seq",
    text: "text"
});

/// Parameters for recording workflow progress.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowLogRequest {
    /// Opaque token identifying the current workflow execution attempt.
    pub execution_token: String,
    /// Ordered progress lines to append.
    pub lines: Vec<WorkflowLogLine>,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowLogRequest {
    execution_token: "executionToken",
    lines: "lines",
    run_id: "runId"
});

/// Parameters for an owned durable pause checkpoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowPauseCheckpointRequest {
    /// Opaque token identifying the execution attempt that reached the checkpoint.
    pub execution_token: String,
    /// Stable author-defined checkpoint key.
    pub key: String,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowPauseCheckpointRequest {
    execution_token: "executionToken",
    key: "key",
    run_id: "runId"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowPauseCheckpointResult {
    /// Whether this execution attempt must pause or may continue.
    pub action: WorkflowPauseCheckpointAction,
}
crate::record_serde::wire_record!(WorkflowPauseCheckpointResult { action: "action" });

/// Parameters for pausing a running workflow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowPauseRequest {
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowPauseRequest { run_id: "runId" });

/// Wire-only per-invocation workflow resource ceiling overrides.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunLimits {
    /// Maximum AI credits consumed by workflow subagents and their descendants. The post-paid ceiling is soft: parallel turns can settle beyond it before the run stops.
    pub max_ai_credits: Option<f64>,
    /// Maximum number of workflow subagents that may run concurrently.
    pub max_concurrent_subagents: Option<i64>,
    /// Maximum total number of workflow subagents that may be admitted.
    pub max_total_subagents: Option<i64>,
    /// Maximum accumulated active-execution time in seconds. Active execution includes the entire extension body, subprocess waits, queued-agent waits, and sleeps; time between resumed attempts is not counted.
    pub timeout_seconds: Option<f64>,
}
crate::record_serde::wire_record!(WorkflowRunLimits { max_ai_credits: "maxAiCredits" if is_none, max_concurrent_subagents: "maxConcurrentSubagents" if is_none, max_total_subagents: "maxTotalSubagents" if is_none, timeout_seconds: "timeoutSeconds" if is_none });

/// Parameters for resuming a workflow run from its persisted identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowResumeRequest {
    /// Optional per-invocation resource ceiling overrides.
    pub limits: Option<WorkflowRunLimits>,
    /// Whether to emit workflow phase names to the session transcript.
    pub log_phase_names: Option<bool>,
    /// Whether to notify the originating session when the workflow completes.
    pub notify_on_complete: Option<bool>,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowResumeRequest { limits: "limits" if is_none, log_phase_names: "logPhaseNames" if is_none, notify_on_complete: "notifyOnComplete" if is_none, run_id: "runId" });

/// Resolved persisted workflow identity and resumed run envelope.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowResumeResult {
    /// Terminal resumed run envelope.
    pub run: WorkflowRunResult,
    /// Persisted workflow name resolved for the resumed run.
    pub workflow_name: String,
}
crate::record_serde::wire_record!(WorkflowResumeResult {
    run: "run",
    workflow_name: "workflowName"
});

/// Full workflow run observability detail.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunDetail {
    /// Epoch milliseconds when the current active segment started, or null while inactive.
    pub active_segment_started_at: Option<i64>,
    /// Durable identities and live statuses for direct workflow agents.
    pub agents: Vec<WorkflowAgentSummary>,
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
    /// Lifecycle and timing observations for each workflow phase.
    pub phases: Vec<WorkflowPhaseObservation>,
    /// Bidirectional page of durable workflow progress.
    pub progress: WorkflowProgressPage,
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
crate::record_serde::wire_record!(WorkflowRunDetail {
    active_segment_started_at: "activeSegmentStartedAt",
    agents: "agents",
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
    phases: "phases",
    progress: "progress",
    revision: "revision",
    run_id: "runId",
    started_at: "startedAt",
    status: "status",
    terminal: "terminal",
    total_spawned_agent_count: "totalSpawnedAgentCount",
    updated_at: "updatedAt",
    workflow_name: "workflowName"
});

/// Options controlling workflow invocation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunOptions {
    /// Per-invocation resource ceiling overrides.
    pub limits: Option<WorkflowRunLimits>,
    /// Whether to emit workflow phase names to the session transcript.
    pub log_phase_names: Option<bool>,
    /// Whether to notify the originating session when the workflow completes.
    pub notify_on_complete: Option<bool>,
    /// Run identifier whose journal and progress should seed this resumed run.
    pub resume_from_run_id: Option<String>,
}
crate::record_serde::wire_record!(WorkflowRunOptions { limits: "limits" if is_none, log_phase_names: "logPhaseNames" if is_none, notify_on_complete: "notifyOnComplete" if is_none, resume_from_run_id: "resumeFromRunId" if is_none });

/// Parameters for invoking a registered workflow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunRequest {
    /// Workflow input value.
    pub args: serde_json::Value,
    /// Registered workflow name.
    pub name: String,
    /// Workflow invocation options.
    pub options: Option<WorkflowRunOptions>,
}
crate::record_serde::wire_record!(WorkflowRunRequest { args: "args", name: "name", options: "options" if is_none });

/// Internal parameters for resuming a workflow run from a tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct WorkflowToolResumeRequest {
    /// Optional per-invocation resource ceiling overrides.
    pub limits: Option<WorkflowRunLimits>,
    /// Workflow run identifier.
    pub run_id: String,
    /// Opaque identifier of the originating tool call.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(WorkflowToolResumeRequest { limits: "limits" if is_none, run_id: "runId", tool_call_id: "toolCallId" if is_none });

/// Options for an internal tool-originated workflow invocation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct WorkflowToolRunOptions {
    /// Per-invocation resource ceiling overrides.
    pub limits: Option<WorkflowRunLimits>,
    /// Run identifier whose journal and progress should seed this resumed run.
    pub resume_from_run_id: Option<String>,
}
crate::record_serde::wire_record!(WorkflowToolRunOptions { limits: "limits" if is_none, resume_from_run_id: "resumeFromRunId" if is_none });

/// Internal parameters for invoking a registered workflow from a tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct WorkflowToolRunRequest {
    /// Workflow input value.
    pub args: serde_json::Value,
    /// Registered workflow name.
    pub name: String,
    /// Tool-originated workflow invocation options.
    #[doc(hidden)]
    pub(crate) options: Option<WorkflowToolRunOptions>,
    /// Opaque identifier of the originating tool call.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(WorkflowToolRunRequest { args: "args", name: "name", options: "options" if is_none, tool_call_id: "toolCallId" if is_none });

/// Acknowledgement that a workflow request was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkflowAbortResult {}

/// Execution-critical workflow storage operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowDurableOperation {
    /// Creating the durable run and declared phases.
    #[serde(rename = "createRun")]
    CreateRun,
    /// Persisting the transition to running.
    #[serde(rename = "markRunStarted")]
    MarkRunStarted,
    /// Persisting the terminal run envelope.
    #[serde(rename = "finishRun")]
    FinishRun,
    /// Persisting subagent admission accounting.
    #[serde(rename = "reserveAgent")]
    ReserveAgent,
    /// Rolling back an uncommitted subagent admission.
    #[serde(rename = "releaseAgent")]
    ReleaseAgent,
    /// Persisting an idempotent model-usage charge.
    #[serde(rename = "chargeCredit")]
    ChargeCredit,
    /// Persisting active execution time.
    #[serde(rename = "addElapsed")]
    AddElapsed,
    /// Reading the authoritative AI-credit total.
    #[serde(rename = "reconcileCreditTotal")]
    ReconcileCreditTotal,
    /// Reading a journal entry without treating storage failure as a cache miss.
    #[serde(rename = "journalGet")]
    JournalGet,
    /// Persisting a journal entry before reporting success.
    #[serde(rename = "journalPut")]
    JournalPut,
    /// Renewing the durable owner lease that proves this process still owns the run.
    #[serde(rename = "refreshLease")]
    RefreshLease,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowDurableOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowDurableOperation",
            &[
                "createRun",
                "markRunStarted",
                "finishRun",
                "reserveAgent",
                "releaseAgent",
                "chargeCredit",
                "addElapsed",
                "reconcileCreditTotal",
                "journalGet",
                "journalPut",
                "refreshLease",
                "Unknown",
            ],
            Some(11),
        )?;
        Ok(match index {
            0 => Self::CreateRun,
            1 => Self::MarkRunStarted,
            2 => Self::FinishRun,
            3 => Self::ReserveAgent,
            4 => Self::ReleaseAgent,
            5 => Self::ChargeCredit,
            6 => Self::AddElapsed,
            7 => Self::ReconcileCreditTotal,
            8 => Self::JournalGet,
            9 => Self::JournalPut,
            10 => Self::RefreshLease,
            _ => Self::Unknown,
        })
    }
}

/// Cumulative resource ceiling that stopped a workflow run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowRunFailureKind {
    /// The run admitted the approved maximum total number of subagents.
    #[serde(rename = "maxTotalSubagents")]
    MaxTotalSubagents,
    /// The run reached the approved accumulated active-execution time in seconds.
    #[serde(rename = "timeoutSeconds")]
    TimeoutSeconds,
    /// The run's settled subagent model usage exceeded the approved AI-credit ceiling, or no headroom remained for another subagent.
    #[serde(rename = "maxAiCredits")]
    MaxAiCredits,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowRunFailureKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowRunFailureKind",
            &[
                "maxTotalSubagents",
                "timeoutSeconds",
                "maxAiCredits",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::MaxTotalSubagents,
            1 => Self::TimeoutSeconds,
            2 => Self::MaxAiCredits,
            _ => Self::Unknown,
        })
    }
}
