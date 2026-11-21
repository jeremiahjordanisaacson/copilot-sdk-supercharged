//! Auto-generated from session-events.schema.json — do not edit manually.

#![allow(deprecated)]

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::types::{RequestId, SessionId};

/// Identifies the kind of session event.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SessionEventType {
    #[serde(rename = "session.sandbox_state_changed")]
    SessionSandboxStateChanged,
    #[serde(rename = "ide.connection_changed")]
    IdeConnectionChanged,
    #[serde(rename = "ide.selection_changed")]
    IdeSelectionChanged,
    #[serde(rename = "ide.add_file_reference")]
    IdeAddFileReference,
    #[serde(rename = "ide.elicitation_complete")]
    IdeElicitationComplete,
    #[serde(rename = "session.journal_link")]
    SessionJournalLink,
    #[serde(rename = "session.start")]
    SessionStart,
    #[serde(rename = "session.resume")]
    SessionResume,
    #[serde(rename = "session.remote_steerable_changed")]
    SessionRemoteSteerableChanged,
    #[serde(rename = "session.error")]
    SessionError,
    #[serde(rename = "session.idle")]
    SessionIdle,
    #[serde(rename = "session.autopilot_stopped")]
    SessionAutopilotStopped,
    #[serde(rename = "session.title_changed")]
    SessionTitleChanged,
    #[serde(rename = "session.schedule_created")]
    SessionScheduleCreated,
    #[serde(rename = "session.schedule_cancelled")]
    SessionScheduleCancelled,
    #[serde(rename = "session.schedule_rearmed")]
    SessionScheduleRearmed,
    #[serde(rename = "session.autopilot_objective_changed")]
    SessionAutopilotObjectiveChanged,
    #[serde(rename = "session.info")]
    SessionInfo,
    #[serde(rename = "session.indexed_search")]
    SessionIndexedSearch,
    #[serde(rename = "session.warning")]
    SessionWarning,
    #[serde(rename = "session.model_change")]
    SessionModelChange,
    #[serde(rename = "session.model_deselected")]
    SessionModelDeselected,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.auto_tier_recommendation")]
    SessionAutoTierRecommendation,
    #[serde(rename = "session.auto_tier_switch_failed")]
    SessionAutoTierSwitchFailed,
    #[serde(rename = "session.mode_changed")]
    SessionModeChanged,
    #[serde(rename = "session.mode_notice_delivered")]
    SessionModeNoticeDelivered,
    #[serde(rename = "session.session_limits_changed")]
    SessionSessionLimitsChanged,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.permissions_changed")]
    SessionPermissionsChanged,
    #[serde(rename = "session.plan_changed")]
    SessionPlanChanged,
    #[serde(rename = "session.todos_changed")]
    SessionTodosChanged,
    #[serde(rename = "session.workspace_file_changed")]
    SessionWorkspaceFileChanged,
    #[serde(rename = "session.handoff")]
    SessionHandoff,
    #[serde(rename = "session.truncation")]
    SessionTruncation,
    #[serde(rename = "session.snapshot_rewind")]
    SessionSnapshotRewind,
    #[serde(rename = "session.shutdown")]
    SessionShutdown,
    #[serde(rename = "session.usage_checkpoint")]
    SessionUsageCheckpoint,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.quota_observation")]
    SessionQuotaObservation,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.sensitivity_label_observation")]
    SessionSensitivityLabelObservation,
    #[serde(rename = "session.context_changed")]
    SessionContextChanged,
    #[serde(rename = "session.usage_info")]
    SessionUsageInfo,
    #[serde(rename = "session.context_cleared")]
    SessionContextCleared,
    #[serde(rename = "session.compaction_start")]
    SessionCompactionStart,
    #[serde(rename = "session.compaction_summarized")]
    SessionCompactionSummarized,
    #[serde(rename = "session.compaction_complete")]
    SessionCompactionComplete,
    #[serde(rename = "session.task_complete")]
    SessionTaskComplete,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.completion_receipt")]
    SessionCompletionReceipt,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_route_started")]
    SessionFusionRouteStarted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_route_failed")]
    SessionFusionRouteFailed,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_resolved")]
    SessionFusionResolved,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_completed")]
    SessionFusionCompleted,
    #[serde(rename = "session.permission_recovery")]
    SessionPermissionRecovery,
    #[serde(rename = "user.message")]
    UserMessage,
    #[serde(rename = "pending_messages.modified")]
    PendingMessagesModified,
    #[serde(rename = "assistant.turn_start")]
    AssistantTurnStart,
    #[serde(rename = "assistant.turn_retry")]
    AssistantTurnRetry,
    #[serde(rename = "agent.interrupted")]
    AgentInterrupted,
    #[serde(rename = "assistant.intent")]
    AssistantIntent,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_started")]
    AssistantFusionPhaseStarted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_activity")]
    AssistantFusionPhaseActivity,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_completed")]
    AssistantFusionPhaseCompleted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_failed")]
    AssistantFusionPhaseFailed,
    #[serde(rename = "assistant.server_tool_progress")]
    AssistantServerToolProgress,
    #[serde(rename = "assistant.reasoning")]
    AssistantReasoning,
    #[serde(rename = "assistant.reasoning_delta")]
    AssistantReasoningDelta,
    #[serde(rename = "assistant.tool_call_delta")]
    AssistantToolCallDelta,
    #[serde(rename = "assistant.streaming_delta")]
    AssistantStreamingDelta,
    #[serde(rename = "assistant.message")]
    AssistantMessage,
    #[serde(rename = "assistant.message_start")]
    AssistantMessageStart,
    #[serde(rename = "assistant.message_delta")]
    AssistantMessageDelta,
    #[serde(rename = "assistant.turn_end")]
    AssistantTurnEnd,
    #[serde(rename = "assistant.idle")]
    AssistantIdle,
    #[serde(rename = "assistant.usage")]
    AssistantUsage,
    #[serde(rename = "prompt_cache_break")]
    PromptCacheBreak,
    #[serde(rename = "model.call_failure")]
    ModelCallFailure,
    #[serde(rename = "model.call_final_result")]
    ModelCallFinalResult,
    #[serde(rename = "model.call_finished")]
    ModelCallFinished,
    #[serde(rename = "model.call_start")]
    ModelCallStart,
    #[serde(rename = "abort")]
    Abort,
    #[serde(rename = "tool.user_requested")]
    ToolUserRequested,
    #[serde(rename = "tool.execution_start")]
    ToolExecutionStart,
    #[serde(rename = "tool.shell_output")]
    ToolShellOutput,
    #[serde(rename = "tool.execution_progress")]
    ToolExecutionProgress,
    #[serde(rename = "tool.execution_complete")]
    ToolExecutionComplete,
    #[serde(rename = "tool_search.activated")]
    ToolSearchActivated,
    #[serde(rename = "skill.invoked")]
    SkillInvoked,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "skill.invoked_ref")]
    SkillInvokedRef,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "skill.context_delivered")]
    SkillContextDelivered,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "skill.context_delivered_ref")]
    SkillContextDeliveredRef,
    #[serde(rename = "sandbox.decision")]
    SandboxDecision,
    #[serde(rename = "subagent.started")]
    SubagentStarted,
    #[serde(rename = "subagent.configured")]
    SubagentConfigured,
    #[serde(rename = "subagent.completed")]
    SubagentCompleted,
    #[serde(rename = "subagent.failed")]
    SubagentFailed,
    #[serde(rename = "subagent.selected")]
    SubagentSelected,
    #[serde(rename = "subagent.deselected")]
    SubagentDeselected,
    #[serde(rename = "hook.start")]
    HookStart,
    #[serde(rename = "hook.end")]
    HookEnd,
    #[serde(rename = "hook.progress")]
    HookProgress,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.binary_asset")]
    SessionBinaryAsset,
    #[serde(rename = "system.message")]
    SystemMessage,
    #[serde(rename = "instruction.update")]
    InstructionUpdate,
    #[serde(rename = "system.notification")]
    SystemNotification,
    #[serde(rename = "permission.requested")]
    PermissionRequested,
    #[serde(rename = "permission.completed")]
    PermissionCompleted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.carriedForward")]
    PermissionCarriedForward,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorization")]
    PermissionMessageAuthorization,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorizationRead")]
    PermissionMessageAuthorizationRead,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorizationDegraded")]
    PermissionMessageAuthorizationDegraded,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.assentDetected")]
    PermissionAssentDetected,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.contextualAuthorization")]
    PermissionContextualAuthorization,
    #[serde(rename = "user_input.requested")]
    UserInputRequested,
    #[serde(rename = "user_input.completed")]
    UserInputCompleted,
    #[serde(rename = "elicitation.requested")]
    ElicitationRequested,
    #[serde(rename = "elicitation.completed")]
    ElicitationCompleted,
    #[serde(rename = "sampling.requested")]
    SamplingRequested,
    #[serde(rename = "sampling.completed")]
    SamplingCompleted,
    #[serde(rename = "mcp.oauth_required")]
    McpOauthRequired,
    #[serde(rename = "mcp.oauth_completed")]
    McpOauthCompleted,
    #[serde(rename = "mcp.oauth_recovery_completed")]
    McpOauthRecoveryCompleted,
    #[serde(rename = "mcp.headers_refresh_required")]
    McpHeadersRefreshRequired,
    #[serde(rename = "mcp.headers_refresh_completed")]
    McpHeadersRefreshCompleted,
    #[serde(rename = "session.custom_notification")]
    SessionCustomNotification,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "ui.ephemeral_query")]
    UiEphemeralQuery,
    #[serde(rename = "external_tool.requested")]
    ExternalToolRequested,
    #[serde(rename = "external_tool.completed")]
    ExternalToolCompleted,
    #[serde(rename = "command.queued")]
    CommandQueued,
    #[serde(rename = "command.execute")]
    CommandExecute,
    #[serde(rename = "command.completed")]
    CommandCompleted,
    #[serde(rename = "auto_mode_switch.requested")]
    AutoModeSwitchRequested,
    #[serde(rename = "auto_mode_switch.completed")]
    AutoModeSwitchCompleted,
    #[serde(rename = "auto_mode_recovery_consent.requested")]
    AutoModeRecoveryConsentRequested,
    #[serde(rename = "auto_mode_recovery_consent.completed")]
    AutoModeRecoveryConsentCompleted,
    #[serde(rename = "auto_mode_recovery.outcome")]
    AutoModeRecoveryOutcome,
    #[serde(rename = "session_limits_exhausted.requested")]
    SessionLimitsExhaustedRequested,
    #[serde(rename = "session_limits_exhausted.completed")]
    SessionLimitsExhaustedCompleted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.auto_mode_resolved")]
    SessionAutoModeResolved,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_plugin_progress")]
    SessionManagedPluginProgress,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_settings_resolved")]
    SessionManagedSettingsResolved,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_settings_enforced")]
    SessionManagedSettingsEnforced,
    #[serde(rename = "commands.changed")]
    CommandsChanged,
    #[serde(rename = "capabilities.changed")]
    CapabilitiesChanged,
    #[serde(rename = "exit_plan_mode.requested")]
    ExitPlanModeRequested,
    #[serde(rename = "exit_plan_mode.completed")]
    ExitPlanModeCompleted,
    #[serde(rename = "human_response.recorded")]
    HumanResponseRecorded,
    #[serde(rename = "session.tools_updated")]
    SessionToolsUpdated,
    #[serde(rename = "session.background_tasks_changed")]
    SessionBackgroundTasksChanged,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_updated")]
    WorkflowRunUpdated,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_started")]
    WorkflowRunStarted,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_settled")]
    WorkflowRunSettled,
    #[serde(rename = "session.skills_loaded")]
    SessionSkillsLoaded,
    #[serde(rename = "session.custom_agents_updated")]
    SessionCustomAgentsUpdated,
    #[serde(rename = "session.mcp_servers_loaded")]
    SessionMcpServersLoaded,
    #[serde(rename = "session.mcp_server_status_changed")]
    SessionMcpServerStatusChanged,
    #[serde(rename = "session.mcp_server_tools_loaded")]
    SessionMcpServerToolsLoaded,
    #[serde(rename = "session.mcp_server_removed")]
    SessionMcpServerRemoved,
    #[serde(rename = "session.mcp_server_needs_reconnect")]
    SessionMcpServerNeedsReconnect,
    #[serde(rename = "mcp.tools.list_changed")]
    McpToolsListChanged,
    #[serde(rename = "mcp.resources.list_changed")]
    McpResourcesListChanged,
    #[serde(rename = "mcp.prompts.list_changed")]
    McpPromptsListChanged,
    #[serde(rename = "session.extensions_loaded")]
    SessionExtensionsLoaded,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.opened")]
    SessionCanvasOpened,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.registry_changed")]
    SessionCanvasRegistryChanged,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.closed")]
    SessionCanvasClosed,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.unavailable")]
    SessionCanvasUnavailable,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.recorded")]
    SessionCanvasRecorded,
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.removed")]
    SessionCanvasRemoved,
    #[serde(rename = "session.extensions.attachments_pushed")]
    SessionExtensionsAttachmentsPushed,
    #[serde(rename = "mcp_app.tool_call_complete")]
    McpAppToolCallComplete,
    /// Unknown event type for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}

/// Typed session event data, discriminated by the event `type` field.
///
/// Use with [`TypedSessionEvent`] for fully typed event handling.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum SessionEventData {
    #[serde(rename = "session.sandbox_state_changed")]
    SessionSandboxStateChanged(SessionSandboxStateChangedData),
    #[serde(rename = "ide.connection_changed")]
    IdeConnectionChanged(IdeConnectionChangedData),
    #[serde(rename = "ide.selection_changed")]
    IdeSelectionChanged(IdeSelectionChangedData),
    #[serde(rename = "ide.add_file_reference")]
    IdeAddFileReference(IdeAddFileReferenceData),
    #[serde(rename = "ide.elicitation_complete")]
    IdeElicitationComplete(IdeElicitationCompleteData),
    #[serde(rename = "session.journal_link")]
    SessionJournalLink(SessionJournalLinkData),
    #[serde(rename = "session.start")]
    SessionStart(SessionStartData),
    #[serde(rename = "session.resume")]
    SessionResume(SessionResumeData),
    #[serde(rename = "session.remote_steerable_changed")]
    SessionRemoteSteerableChanged(SessionRemoteSteerableChangedData),
    #[serde(rename = "session.error")]
    SessionError(SessionErrorData),
    #[serde(rename = "session.idle")]
    SessionIdle(SessionIdleData),
    #[serde(rename = "session.autopilot_stopped")]
    SessionAutopilotStopped(SessionAutopilotStoppedData),
    #[serde(rename = "session.title_changed")]
    SessionTitleChanged(SessionTitleChangedData),
    #[serde(rename = "session.schedule_created")]
    SessionScheduleCreated(SessionScheduleCreatedData),
    #[serde(rename = "session.schedule_cancelled")]
    SessionScheduleCancelled(SessionScheduleCancelledData),
    #[serde(rename = "session.schedule_rearmed")]
    SessionScheduleRearmed(SessionScheduleRearmedData),
    #[serde(rename = "session.autopilot_objective_changed")]
    SessionAutopilotObjectiveChanged(SessionAutopilotObjectiveChangedData),
    #[serde(rename = "session.info")]
    SessionInfo(SessionInfoData),
    #[serde(rename = "session.indexed_search")]
    SessionIndexedSearch(SessionIndexedSearchData),
    #[serde(rename = "session.warning")]
    SessionWarning(SessionWarningData),
    #[serde(rename = "session.model_change")]
    SessionModelChange(SessionModelChangeData),
    #[serde(rename = "session.model_deselected")]
    SessionModelDeselected(SessionModelDeselectedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.auto_tier_recommendation")]
    SessionAutoTierRecommendation(SessionAutoTierRecommendationData),
    #[serde(rename = "session.auto_tier_switch_failed")]
    SessionAutoTierSwitchFailed(SessionAutoTierSwitchFailedData),
    #[serde(rename = "session.mode_changed")]
    SessionModeChanged(SessionModeChangedData),
    #[serde(rename = "session.mode_notice_delivered")]
    SessionModeNoticeDelivered(SessionModeNoticeDeliveredData),
    #[serde(rename = "session.session_limits_changed")]
    SessionSessionLimitsChanged(SessionSessionLimitsChangedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.permissions_changed")]
    SessionPermissionsChanged(SessionPermissionsChangedData),
    #[serde(rename = "session.plan_changed")]
    SessionPlanChanged(SessionPlanChangedData),
    #[serde(rename = "session.todos_changed")]
    SessionTodosChanged(SessionTodosChangedData),
    #[serde(rename = "session.workspace_file_changed")]
    SessionWorkspaceFileChanged(SessionWorkspaceFileChangedData),
    #[serde(rename = "session.handoff")]
    SessionHandoff(SessionHandoffData),
    #[serde(rename = "session.truncation")]
    SessionTruncation(SessionTruncationData),
    #[serde(rename = "session.snapshot_rewind")]
    SessionSnapshotRewind(SessionSnapshotRewindData),
    #[serde(rename = "session.shutdown")]
    SessionShutdown(SessionShutdownData),
    #[serde(rename = "session.usage_checkpoint")]
    SessionUsageCheckpoint(SessionUsageCheckpointData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.quota_observation")]
    SessionQuotaObservation(SessionQuotaObservationData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.sensitivity_label_observation")]
    SessionSensitivityLabelObservation(SessionSensitivityLabelObservationData),
    #[serde(rename = "session.context_changed")]
    SessionContextChanged(SessionContextChangedData),
    #[serde(rename = "session.usage_info")]
    SessionUsageInfo(SessionUsageInfoData),
    #[serde(rename = "session.context_cleared")]
    SessionContextCleared(SessionContextClearedData),
    #[serde(rename = "session.compaction_start")]
    SessionCompactionStart(SessionCompactionStartData),
    #[serde(rename = "session.compaction_summarized")]
    SessionCompactionSummarized(SessionCompactionSummarizedData),
    #[serde(rename = "session.compaction_complete")]
    SessionCompactionComplete(SessionCompactionCompleteData),
    #[serde(rename = "session.task_complete")]
    SessionTaskComplete(SessionTaskCompleteData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.completion_receipt")]
    SessionCompletionReceipt(SessionCompletionReceiptData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_route_started")]
    SessionFusionRouteStarted(SessionFusionRouteStartedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_route_failed")]
    SessionFusionRouteFailed(SessionFusionRouteFailedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_resolved")]
    SessionFusionResolved(SessionFusionResolvedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.fusion_completed")]
    SessionFusionCompleted(SessionFusionCompletedData),
    #[serde(rename = "session.permission_recovery")]
    SessionPermissionRecovery(SessionPermissionRecoveryData),
    #[serde(rename = "user.message")]
    UserMessage(UserMessageData),
    #[serde(rename = "pending_messages.modified")]
    PendingMessagesModified(PendingMessagesModifiedData),
    #[serde(rename = "assistant.turn_start")]
    AssistantTurnStart(AssistantTurnStartData),
    #[serde(rename = "assistant.turn_retry")]
    AssistantTurnRetry(AssistantTurnRetryData),
    #[serde(rename = "agent.interrupted")]
    AgentInterrupted(AgentInterruptedData),
    #[serde(rename = "assistant.intent")]
    AssistantIntent(AssistantIntentData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_started")]
    AssistantFusionPhaseStarted(AssistantFusionPhaseStartedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_activity")]
    AssistantFusionPhaseActivity(AssistantFusionPhaseActivityData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_completed")]
    AssistantFusionPhaseCompleted(AssistantFusionPhaseCompletedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "assistant.fusion_phase_failed")]
    AssistantFusionPhaseFailed(AssistantFusionPhaseFailedData),
    #[serde(rename = "assistant.server_tool_progress")]
    AssistantServerToolProgress(AssistantServerToolProgressData),
    #[serde(rename = "assistant.reasoning")]
    AssistantReasoning(AssistantReasoningData),
    #[serde(rename = "assistant.reasoning_delta")]
    AssistantReasoningDelta(AssistantReasoningDeltaData),
    #[serde(rename = "assistant.tool_call_delta")]
    AssistantToolCallDelta(AssistantToolCallDeltaData),
    #[serde(rename = "assistant.streaming_delta")]
    AssistantStreamingDelta(AssistantStreamingDeltaData),
    #[serde(rename = "assistant.message")]
    AssistantMessage(AssistantMessageData),
    #[serde(rename = "assistant.message_start")]
    AssistantMessageStart(AssistantMessageStartData),
    #[serde(rename = "assistant.message_delta")]
    AssistantMessageDelta(AssistantMessageDeltaData),
    #[serde(rename = "assistant.turn_end")]
    AssistantTurnEnd(AssistantTurnEndData),
    #[serde(rename = "assistant.idle")]
    AssistantIdle(AssistantIdleData),
    #[serde(rename = "assistant.usage")]
    AssistantUsage(AssistantUsageData),
    #[serde(rename = "prompt_cache_break")]
    PromptCacheBreak(PromptCacheBreakData),
    #[serde(rename = "model.call_failure")]
    ModelCallFailure(ModelCallFailureData),
    #[serde(rename = "model.call_final_result")]
    ModelCallFinalResult(ModelCallFinalResultData),
    #[serde(rename = "model.call_finished")]
    ModelCallFinished(ModelCallFinishedData),
    #[serde(rename = "model.call_start")]
    ModelCallStart(ModelCallStartData),
    #[serde(rename = "abort")]
    Abort(AbortData),
    #[serde(rename = "tool.user_requested")]
    ToolUserRequested(ToolUserRequestedData),
    #[serde(rename = "tool.execution_start")]
    ToolExecutionStart(ToolExecutionStartData),
    #[serde(rename = "tool.shell_output")]
    ToolShellOutput(ToolShellOutputData),
    #[serde(rename = "tool.execution_progress")]
    ToolExecutionProgress(ToolExecutionProgressData),
    #[serde(rename = "tool.execution_complete")]
    ToolExecutionComplete(ToolExecutionCompleteData),
    #[serde(rename = "tool_search.activated")]
    ToolSearchActivated(ToolSearchActivatedData),
    #[serde(rename = "skill.invoked")]
    SkillInvoked(SkillInvokedData),
    #[serde(rename = "skill.invoked_ref")]
    SkillInvokedRef(SkillInvokedRefData),
    #[serde(rename = "skill.context_delivered")]
    SkillContextDelivered(SkillContextDeliveredData),
    #[serde(rename = "skill.context_delivered_ref")]
    SkillContextDeliveredRef(SkillContextDeliveredRefData),
    #[serde(rename = "sandbox.decision")]
    SandboxDecision(SandboxDecisionData),
    #[serde(rename = "subagent.started")]
    SubagentStarted(SubagentStartedData),
    #[serde(rename = "subagent.configured")]
    SubagentConfigured(SubagentConfiguredData),
    #[serde(rename = "subagent.completed")]
    SubagentCompleted(SubagentCompletedData),
    #[serde(rename = "subagent.failed")]
    SubagentFailed(SubagentFailedData),
    #[serde(rename = "subagent.selected")]
    SubagentSelected(SubagentSelectedData),
    #[serde(rename = "subagent.deselected")]
    SubagentDeselected(SubagentDeselectedData),
    #[serde(rename = "hook.start")]
    HookStart(HookStartData),
    #[serde(rename = "hook.end")]
    HookEnd(HookEndData),
    #[serde(rename = "hook.progress")]
    HookProgress(HookProgressData),
    #[serde(rename = "session.binary_asset")]
    SessionBinaryAsset(SessionBinaryAssetData),
    #[serde(rename = "system.message")]
    SystemMessage(SystemMessageData),
    #[serde(rename = "instruction.update")]
    InstructionUpdate(InstructionUpdateData),
    #[serde(rename = "system.notification")]
    SystemNotification(SystemNotificationData),
    #[serde(rename = "permission.requested")]
    PermissionRequested(PermissionRequestedData),
    #[serde(rename = "permission.completed")]
    PermissionCompleted(PermissionCompletedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.carriedForward")]
    PermissionCarriedForward(PermissionCarriedForwardData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorization")]
    PermissionMessageAuthorization(PermissionMessageAuthorizationData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorizationRead")]
    PermissionMessageAuthorizationRead(PermissionMessageAuthorizationReadData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.messageAuthorizationDegraded")]
    PermissionMessageAuthorizationDegraded(PermissionMessageAuthorizationDegradedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.assentDetected")]
    PermissionAssentDetected(PermissionAssentDetectedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "permission.contextualAuthorization")]
    PermissionContextualAuthorization(PermissionContextualAuthorizationData),
    #[serde(rename = "user_input.requested")]
    UserInputRequested(UserInputRequestedData),
    #[serde(rename = "user_input.completed")]
    UserInputCompleted(UserInputCompletedData),
    #[serde(rename = "elicitation.requested")]
    ElicitationRequested(ElicitationRequestedData),
    #[serde(rename = "elicitation.completed")]
    ElicitationCompleted(ElicitationCompletedData),
    #[serde(rename = "sampling.requested")]
    SamplingRequested(SamplingRequestedData),
    #[serde(rename = "sampling.completed")]
    SamplingCompleted(SamplingCompletedData),
    #[serde(rename = "mcp.oauth_required")]
    McpOauthRequired(McpOauthRequiredData),
    #[serde(rename = "mcp.oauth_completed")]
    McpOauthCompleted(McpOauthCompletedData),
    #[serde(rename = "mcp.oauth_recovery_completed")]
    McpOauthRecoveryCompleted(McpOauthRecoveryCompletedData),
    #[serde(rename = "mcp.headers_refresh_required")]
    McpHeadersRefreshRequired(McpHeadersRefreshRequiredData),
    #[serde(rename = "mcp.headers_refresh_completed")]
    McpHeadersRefreshCompleted(McpHeadersRefreshCompletedData),
    #[serde(rename = "session.custom_notification")]
    SessionCustomNotification(SessionCustomNotificationData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "ui.ephemeral_query")]
    UiEphemeralQuery(UiEphemeralQueryData),
    #[serde(rename = "external_tool.requested")]
    ExternalToolRequested(ExternalToolRequestedData),
    #[serde(rename = "external_tool.completed")]
    ExternalToolCompleted(ExternalToolCompletedData),
    #[serde(rename = "command.queued")]
    CommandQueued(CommandQueuedData),
    #[serde(rename = "command.execute")]
    CommandExecute(CommandExecuteData),
    #[serde(rename = "command.completed")]
    CommandCompleted(CommandCompletedData),
    #[serde(rename = "auto_mode_switch.requested")]
    AutoModeSwitchRequested(AutoModeSwitchRequestedData),
    #[serde(rename = "auto_mode_switch.completed")]
    AutoModeSwitchCompleted(AutoModeSwitchCompletedData),
    #[serde(rename = "auto_mode_recovery_consent.requested")]
    AutoModeRecoveryConsentRequested(AutoModeRecoveryConsentRequestedData),
    #[serde(rename = "auto_mode_recovery_consent.completed")]
    AutoModeRecoveryConsentCompleted(AutoModeRecoveryConsentCompletedData),
    #[serde(rename = "auto_mode_recovery.outcome")]
    AutoModeRecoveryOutcome(AutoModeRecoveryOutcomeData),
    #[serde(rename = "session_limits_exhausted.requested")]
    SessionLimitsExhaustedRequested(SessionLimitsExhaustedRequestedData),
    #[serde(rename = "session_limits_exhausted.completed")]
    SessionLimitsExhaustedCompleted(SessionLimitsExhaustedCompletedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.auto_mode_resolved")]
    SessionAutoModeResolved(SessionAutoModeResolvedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_plugin_progress")]
    SessionManagedPluginProgress(SessionManagedPluginProgressData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_settings_resolved")]
    SessionManagedSettingsResolved(SessionManagedSettingsResolvedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.managed_settings_enforced")]
    SessionManagedSettingsEnforced(SessionManagedSettingsEnforcedData),
    #[serde(rename = "commands.changed")]
    CommandsChanged(CommandsChangedData),
    #[serde(rename = "capabilities.changed")]
    CapabilitiesChanged(CapabilitiesChangedData),
    #[serde(rename = "exit_plan_mode.requested")]
    ExitPlanModeRequested(ExitPlanModeRequestedData),
    #[serde(rename = "exit_plan_mode.completed")]
    ExitPlanModeCompleted(ExitPlanModeCompletedData),
    #[serde(rename = "human_response.recorded")]
    HumanResponseRecorded(HumanResponseRecordedData),
    #[serde(rename = "session.tools_updated")]
    SessionToolsUpdated(SessionToolsUpdatedData),
    #[serde(rename = "session.background_tasks_changed")]
    SessionBackgroundTasksChanged(SessionBackgroundTasksChangedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_updated")]
    WorkflowRunUpdated(WorkflowRunUpdatedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_started")]
    WorkflowRunStarted(WorkflowRunStartedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "workflow.run_settled")]
    WorkflowRunSettled(WorkflowRunSettledData),
    #[serde(rename = "session.skills_loaded")]
    SessionSkillsLoaded(SessionSkillsLoadedData),
    #[serde(rename = "session.custom_agents_updated")]
    SessionCustomAgentsUpdated(SessionCustomAgentsUpdatedData),
    #[serde(rename = "session.mcp_servers_loaded")]
    SessionMcpServersLoaded(SessionMcpServersLoadedData),
    #[serde(rename = "session.mcp_server_status_changed")]
    SessionMcpServerStatusChanged(SessionMcpServerStatusChangedData),
    #[serde(rename = "session.mcp_server_tools_loaded")]
    SessionMcpServerToolsLoaded(SessionMcpServerToolsLoadedData),
    #[serde(rename = "session.mcp_server_removed")]
    SessionMcpServerRemoved(SessionMcpServerRemovedData),
    #[serde(rename = "session.mcp_server_needs_reconnect")]
    SessionMcpServerNeedsReconnect(SessionMcpServerNeedsReconnectData),
    #[serde(rename = "mcp.tools.list_changed")]
    McpToolsListChanged(McpToolsListChangedData),
    #[serde(rename = "mcp.resources.list_changed")]
    McpResourcesListChanged(McpResourcesListChangedData),
    #[serde(rename = "mcp.prompts.list_changed")]
    McpPromptsListChanged(McpPromptsListChangedData),
    #[serde(rename = "session.extensions_loaded")]
    SessionExtensionsLoaded(SessionExtensionsLoadedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.opened")]
    SessionCanvasOpened(SessionCanvasOpenedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.registry_changed")]
    SessionCanvasRegistryChanged(SessionCanvasRegistryChangedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.closed")]
    SessionCanvasClosed(SessionCanvasClosedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.unavailable")]
    SessionCanvasUnavailable(SessionCanvasUnavailableData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.recorded")]
    SessionCanvasRecorded(SessionCanvasRecordedData),
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(rename = "session.canvas.removed")]
    SessionCanvasRemoved(SessionCanvasRemovedData),
    #[serde(rename = "session.extensions.attachments_pushed")]
    SessionExtensionsAttachmentsPushed(SessionExtensionsAttachmentsPushedData),
    #[serde(rename = "mcp_app.tool_call_complete")]
    McpAppToolCallComplete(McpAppToolCallCompleteData),
}

/// A session event with typed data payload.
///
/// The common event fields (id, timestamp, parentId, ephemeral, agentId)
/// are available directly. The event-specific data is in the `payload`
/// field as a [`SessionEventData`] enum.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TypedSessionEvent {
    /// Unique event identifier (UUID v4).
    pub id: String,
    /// ISO 8601 timestamp when the event was created.
    pub timestamp: String,
    /// ID of the preceding event in the chain.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,
    /// When true, the event is transient and not persisted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ephemeral: Option<bool>,
    /// Sub-agent instance identifier. Absent for events from the root /
    /// main agent and session-level events.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_id: Option<String>,
    /// The typed event payload (discriminated by event type).
    #[serde(flatten)]
    pub payload: SessionEventData,
}

/// Session event "session.sandbox_state_changed". Read-only sandbox state after a live session policy transition.
#[derive(Debug, Clone, Default)]
pub struct SessionSandboxStateChangedData {
    /// Whether queued sandbox bypass requests may use the session opt-out.
    pub auto_approve_bypass: bool,
    /// Effective sandbox configuration, including the current managed floor.
    pub config: serde_json::Value,
    /// Exact runtime attribution of the effective sandbox choice.
    pub config_source: String,
    /// Whether this session holds an authorized or local session opt-out.
    pub disabled_for_session: bool,
    /// Runtime incarnation of the live session.
    pub incarnation: i64,
    /// Monotonic revision of this live session's sandbox projection.
    pub revision: i64,
    /// Explicit session enable choice, when one exists.
    pub session_override: Option<bool>,
}
crate::record_serde::wire_record!(SessionSandboxStateChangedData {
    auto_approve_bypass: "autoApproveBypass",
    config: "config",
    config_source: "configSource",
    disabled_for_session: "disabledForSession",
    incarnation: "incarnation",
    revision: "revision",
    session_override: "sessionOverride"
});

/// An IDE connection on the runtime host.
#[derive(Debug, Clone, Default)]
pub struct IdeConnectedInfo {
    /// IDE display name.
    pub ide_name: String,
    /// IDE lock file path on the runtime host.
    pub lock_file_path: String,
    /// IDE process identifier on the runtime host.
    pub pid: i64,
    /// Registered MCP server name.
    pub server_name: String,
    /// Absolute workspace path on the runtime host.
    pub workspace_folder: String,
}
crate::record_serde::wire_record!(IdeConnectedInfo {
    ide_name: "ideName",
    lock_file_path: "lockFilePath",
    pid: "pid",
    server_name: "serverName",
    workspace_folder: "workspaceFolder"
});

/// A zero-based location in an IDE document.
#[derive(Debug, Clone, Default)]
pub struct IdePosition {
    /// Zero-based character offset.
    pub character: f64,
    /// Zero-based line number.
    pub line: f64,
}
crate::record_serde::wire_record!(IdePosition {
    character: "character",
    line: "line"
});

/// The active selection range in an IDE document.
#[derive(Debug, Clone, Default)]
pub struct IdeSelectionRange {
    /// End position.
    pub end: IdePosition,
    /// Whether the selection contains no text.
    pub is_empty: bool,
    /// Start position.
    pub start: IdePosition,
}
crate::record_serde::wire_record!(IdeSelectionRange {
    end: "end",
    is_empty: "isEmpty",
    start: "start"
});

/// A validated live IDE selection.
#[derive(Debug, Clone, Default)]
pub struct IdeSelection {
    /// Whether the selection is current.
    pub current: Option<bool>,
    /// Document path on the runtime host.
    pub file_path: String,
    /// Document URL supplied by the IDE.
    pub file_url: String,
    /// Selection endpoints and empty state.
    pub selection: IdeSelectionRange,
    /// Selected document text.
    pub text: String,
}
crate::record_serde::wire_record!(IdeSelection { current: "current" if is_none, file_path: "filePath", file_url: "fileUrl", selection: "selection", text: "text" });

/// Session event "ide.connection_changed". Live IDE state. This state never restores a connection from session history.
#[derive(Debug, Clone, Default)]
pub struct IdeConnectionChangedData {
    /// Currently available IDE, absent during an outage.
    pub connected_ide: Option<IdeConnectedInfo>,
    /// IDE whose tool definitions remain registered, including during an outage.
    pub retained_ide: Option<IdeConnectedInfo>,
    /// Latest observed selection, absent after disconnect.
    pub selection: Option<IdeSelection>,
}
crate::record_serde::wire_record!(IdeConnectionChangedData { connected_ide: "connectedIde" if is_none, retained_ide: "retainedIde" if is_none, selection: "selection" if is_none });

/// Session event "ide.selection_changed". A validated selection from the live IDE, or an explicit clear.
#[derive(Debug, Clone, Default)]
pub struct IdeSelectionChangedData {
    /// Selection path, text, and source range, or null when the selection is cleared.
    pub selection: Option<IdeSelection>,
}
crate::record_serde::wire_record!(IdeSelectionChangedData {
    selection: "selection"
});

/// The endpoints of an IDE file reference.
#[derive(Debug, Clone, Default)]
pub struct IdeRange {
    /// End position.
    pub end: IdePosition,
    /// Start position.
    pub start: IdePosition,
}
crate::record_serde::wire_record!(IdeRange {
    end: "end",
    start: "start"
});

/// A validated IDE request to attach a file or selected text.
#[derive(Debug, Clone, Default)]
pub struct IdeFileReference {
    /// Document path on the runtime host.
    pub file_path: String,
    /// Document URL supplied by the IDE.
    pub file_url: String,
    /// Selected text, or null for a whole file.
    pub selected_text: Option<String>,
    /// Selected range, or null for a whole file.
    pub selection: Option<IdeRange>,
}
crate::record_serde::wire_record!(IdeFileReference {
    file_path: "filePath",
    file_url: "fileUrl",
    selected_text: "selectedText",
    selection: "selection"
});

/// Session event "ide.add_file_reference". An IDE request to attach a file or selection to the composer.
#[derive(Debug, Clone, Default)]
pub struct IdeAddFileReferenceData {
    /// Validated file reference or selected range.
    pub reference: IdeFileReference,
}
crate::record_serde::wire_record!(IdeAddFileReferenceData {
    reference: "reference"
});

/// Session event "ide.elicitation_complete". An IDE elicitation completion notification.
#[derive(Debug, Clone, Default)]
pub struct IdeElicitationCompleteData {
    /// Elicitation identifier returned by the IDE.
    pub elicitation_id: String,
}
crate::record_serde::wire_record!(IdeElicitationCompleteData {
    elicitation_id: "elicitationId"
});

/// Session event "session.journal_link". Links a standalone replay head to its complete preceding numbered journal archive.
#[derive(Debug, Clone, Default)]
pub struct SessionJournalLinkData {
    /// Numbered predecessor filename in the same session directory.
    pub previous_file: String,
    /// Session owning this journal chain.
    pub session_id: SessionId,
    /// Journal checkpoint format version.
    pub version: i64,
}
crate::record_serde::wire_record!(SessionJournalLinkData {
    previous_file: "previousFile",
    session_id: "sessionId",
    version: "version"
});

/// Working directory and git context at session start
#[derive(Debug, Clone, Default)]
pub struct WorkingDirectoryContext {
    /// Base commit of current git branch at session start time
    pub base_commit: Option<String>,
    /// Current git branch name
    pub branch: Option<String>,
    /// Current working directory path
    pub cwd: String,
    /// Root directory of the git repository, resolved via git rev-parse
    pub git_root: Option<String>,
    /// Head commit of current git branch at session start time
    pub head_commit: Option<String>,
    /// Hosting platform type of the repository (github or ado)
    pub host_type: Option<WorkingDirectoryContextHostType>,
    /// Set on the immediate preliminary event of a working-directory change, before the git context is resolved. A settled follow-up event (enriched with git context, or cwd-only for a non-repository) is always emitted afterward, so observers may defer to it. Absent on standalone/final events (e.g. relay context changes).
    pub pending_git_context: Option<bool>,
    /// Repository identifier derived from the git remote URL ("owner/name" for GitHub, "org/project/repo" for Azure DevOps)
    pub repository: Option<String>,
    /// Raw host string from the git remote URL (e.g. "github.com", "mycompany.ghe.com", "dev.azure.com")
    pub repository_host: Option<String>,
}
crate::record_serde::wire_record!(WorkingDirectoryContext { base_commit: "baseCommit" if is_none, branch: "branch" if is_none, cwd: "cwd", git_root: "gitRoot" if is_none, head_commit: "headCommit" if is_none, host_type: "hostType" if is_none, pending_git_context: "pendingGitContext" if is_none, repository: "repository" if is_none, repository_host: "repositoryHost" if is_none });

/// Per-session configuration for the built-in GitHub MCP server
#[derive(Debug, Clone, Default)]
pub struct GitHubMcpToolConfig {
    /// Additional GitHub MCP tools requested by the session
    pub additional_tools: Option<Vec<String>>,
    /// Additional GitHub MCP toolsets requested by the session
    pub additional_toolsets: Option<Vec<String>>,
    /// Whether to use the read-write endpoint and request all toolsets
    pub enable_all_tools: Option<bool>,
    /// Whether to request the GitHub MCP insiders build
    pub enable_insiders_mode: Option<bool>,
}
crate::record_serde::wire_record!(GitHubMcpToolConfig { additional_tools: "additionalTools" if is_none, additional_toolsets: "additionalToolsets" if is_none, enable_all_tools: "enableAllTools" if is_none, enable_insiders_mode: "enableInsidersMode" if is_none });

/// Optional session limits.
#[derive(Debug, Clone, Default)]
pub struct SessionLimitsConfig {
    /// Maximum AI Credits allowed across the session's current accounting window. When setting a limit, the value must be at least 30 and greater than the AI Credits the session has already used.
    pub max_ai_credits: Option<f64>,
}
crate::record_serde::wire_record!(SessionLimitsConfig { max_ai_credits: "maxAiCredits" if is_none });

/// Session event "session.start". Session initialization metadata including context and configuration
#[derive(Debug, Clone, Default)]
pub struct SessionStartData {
    /// Whether the session was already in use by another client at start time
    pub already_in_use: Option<bool>,
    /// Auto routing preference selected at session creation time
    pub auto_tier: Option<AutoTier>,
    /// True when autoTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub auto_tier_managed: Option<bool>,
    /// Working directory and git context at session start
    pub context: Option<WorkingDirectoryContext>,
    /// Context tier selected at session creation time for models with tiered context pricing; null when no tier is selected (e.g., non-tiered model)
    pub context_tier: Option<ContextTier>,
    /// True when contextTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub context_tier_managed: Option<bool>,
    /// Version string of the Copilot application
    pub copilot_version: String,
    /// When set, identifies a parent session whose context this session continues — e.g., a detached headless rem-agent run launched on the parent's interactive shutdown. Telemetry from this session is reported under the parent's session_id.
    pub detached_from_spawning_parent_session_id: Option<String>,
    /// Per-session GitHub MCP override persisted for cold resume
    pub github_mcp_tool_config: Option<GitHubMcpToolConfig>,
    /// Identifier of the software producing the events (e.g., "copilot-agent")
    pub producer: String,
    /// Provider of selectedModel at creation time, when explicitly selected.
    pub provider_id: Option<String>,
    /// Credential-free effective model routing policy restored on cold resume.
    pub provider_routing: Option<serde_json::Value>,
    /// Reasoning effort level used for model calls, if applicable (e.g. "none", "low", "medium", "high", "xhigh", "max")
    pub reasoning_effort: Option<String>,
    /// True when the reasoning effort is a managed-policy default bound to reasoningEffortModel. Omitted for agent-authored, user-authored, independent, and legacy effort.
    pub reasoning_effort_managed: Option<bool>,
    /// Model that owns effort embedded in an authored model selection. Omitted for independent reasoning-effort overrides and legacy events.
    pub reasoning_effort_model: Option<String>,
    /// Reasoning summary mode used for model calls, if applicable (e.g. "none", "concise", "detailed")
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Whether this session supports remote steering via GitHub
    pub remote_steerable: Option<bool>,
    /// Model selected at session creation time, if any
    pub selected_model: Option<String>,
    /// Unique identifier for the session
    pub session_id: SessionId,
    /// Session limits configured at session creation time, if any
    pub session_limits: Option<SessionLimitsConfig>,
    /// ISO 8601 timestamp when the session was created
    pub start_time: String,
    /// Output verbosity level used for model calls, if applicable (e.g. "low", "medium", "high")
    pub verbosity: Option<Verbosity>,
    /// Schema version number for the session event format
    pub version: i64,
}
crate::record_serde::wire_record!(SessionStartData { already_in_use: "alreadyInUse" if is_none, auto_tier: "autoTier" if is_none, auto_tier_managed: "autoTierManaged" if is_none, context: "context" if is_none, context_tier: "contextTier" if is_none, context_tier_managed: "contextTierManaged" if is_none, copilot_version: "copilotVersion", detached_from_spawning_parent_session_id: "detachedFromSpawningParentSessionId" if is_none, github_mcp_tool_config: "githubMcpToolConfig" if is_none, producer: "producer", provider_id: "providerId" if is_none, provider_routing: "providerRouting" if is_none, reasoning_effort: "reasoningEffort" if is_none, reasoning_effort_managed: "reasoningEffortManaged" if is_none, reasoning_effort_model: "reasoningEffortModel" if is_none, reasoning_summary: "reasoningSummary" if is_none, remote_steerable: "remoteSteerable" if is_none, selected_model: "selectedModel" if is_none, session_id: "sessionId", session_limits: "sessionLimits" if is_none, start_time: "startTime", verbosity: "verbosity" if is_none, version: "version" });

/// Session event "session.resume". Session resume metadata including current context and event count
#[derive(Debug, Clone, Default)]
pub struct SessionResumeData {
    /// Whether the session was already in use by another client at resume time
    pub already_in_use: Option<bool>,
    /// Auto routing preference active at resume time
    pub auto_tier: Option<AutoTier>,
    /// True when autoTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub auto_tier_managed: Option<bool>,
    /// Updated working directory and git context at resume time
    pub context: Option<WorkingDirectoryContext>,
    /// Context tier currently selected at resume time; null when no tier is active
    pub context_tier: Option<ContextTier>,
    /// True when contextTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub context_tier_managed: Option<bool>,
    /// When true, tool calls and permission requests left in flight by the previous session lifetime remain pending after resume and the agentic loop awaits their results. User sends are queued behind the pending work until all such requests reach a terminal state. When false or omitted, pending work is normally marked as interrupted unless the resume passively joined live work owned by another client; sessionWasActive distinguishes that case.
    pub continue_pending_work: Option<bool>,
    /// Total number of persisted events in the session at the time of resume
    pub event_count: i64,
    /// On-disk byte size of the session's persisted events.jsonl file at resume time; omitted when the file does not exist or cannot be stat'd
    pub events_file_size_bytes: Option<i64>,
    /// Provider of selectedModel at resume time, when explicitly selected.
    pub provider_id: Option<String>,
    /// Credential-free effective model routing policy restored on cold resume.
    pub provider_routing: Option<serde_json::Value>,
    /// Reasoning effort level used for model calls, if applicable (e.g. "none", "low", "medium", "high", "xhigh", "max")
    pub reasoning_effort: Option<String>,
    /// True when the reasoning effort is a managed-policy default bound to reasoningEffortModel. Omitted for agent-authored, user-authored, independent, and legacy effort.
    pub reasoning_effort_managed: Option<bool>,
    /// Model that owns effort embedded in an authored model selection. Omitted for independent reasoning-effort overrides and legacy events.
    pub reasoning_effort_model: Option<String>,
    /// Reasoning summary mode used for model calls, if applicable (e.g. "none", "concise", "detailed")
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Whether this session supports remote steering via GitHub
    pub remote_steerable: Option<bool>,
    /// Whether this resume kept the reasoning that Copilot's Responses API produced on earlier assistant messages in the history it replays. Replaying this event later reapplies the same choice, so the history is rebuilt as it was sent. Omitted for a resume that joined live work, and by resumes recorded before this field existed, which removed that reasoning.
    pub responses_reasoning_kept: Option<bool>,
    /// True when the COPILOT_STRIP_REASONING_ON_RESUME environment override, rather than the runtime's default for this session, decided responsesReasoningKept. Omitted otherwise.
    pub responses_reasoning_overridden: Option<bool>,
    /// ISO 8601 timestamp when the session was resumed
    pub resume_time: String,
    /// Model currently selected at resume time
    pub selected_model: Option<String>,
    /// Session limits currently configured at resume time; null when no limits are active
    pub session_limits: Option<SessionLimitsConfig>,
    /// True when this resume passively joined a session that already had live work running in the runtime - an agent turn, a native queue run, a queued resume continuation, or an in-flight send (for example, an extension joining a session another client was actively driving). False (or omitted) when the session had no live work or when the resume explicitly abandoned pending work, including cold resumes and suspended sessions that remain resident in memory.
    pub session_was_active: Option<bool>,
    /// Output verbosity level used for model calls, if applicable (e.g. "low", "medium", "high")
    pub verbosity: Option<Verbosity>,
}
crate::record_serde::wire_record!(SessionResumeData { already_in_use: "alreadyInUse" if is_none, auto_tier: "autoTier" if is_none, auto_tier_managed: "autoTierManaged" if is_none, context: "context" if is_none, context_tier: "contextTier" if is_none, context_tier_managed: "contextTierManaged" if is_none, continue_pending_work: "continuePendingWork" if is_none, event_count: "eventCount", events_file_size_bytes: "eventsFileSizeBytes" if is_none, provider_id: "providerId" if is_none, provider_routing: "providerRouting" if is_none, reasoning_effort: "reasoningEffort" if is_none, reasoning_effort_managed: "reasoningEffortManaged" if is_none, reasoning_effort_model: "reasoningEffortModel" if is_none, reasoning_summary: "reasoningSummary" if is_none, remote_steerable: "remoteSteerable" if is_none, responses_reasoning_kept: "responsesReasoningKept" if is_none, responses_reasoning_overridden: "responsesReasoningOverridden" if is_none, resume_time: "resumeTime", selected_model: "selectedModel" if is_none, session_limits: "sessionLimits" if is_none, session_was_active: "sessionWasActive" if is_none, verbosity: "verbosity" if is_none });

/// Session event "session.remote_steerable_changed". Notifies that the session's remote steering capability has changed
#[derive(Debug, Clone, Default)]
pub struct SessionRemoteSteerableChangedData {
    /// Whether this session now supports remote steering via GitHub
    pub remote_steerable: bool,
}
crate::record_serde::wire_record!(SessionRemoteSteerableChangedData {
    remote_steerable: "remoteSteerable"
});

/// Session event "session.error". Session error details for timeline display. Request-correlated completion RPC failures carry the terminal error's same fields in their error data, with stack and completionEventId. errorCode is an optional runtime or Copilot API classification, not a closed enum; clients must tolerate unknown values. CAPI HTTP 429 normalizes missing or unknown codes to rate_limited, while HTTP 402 forwards upstream codes or omits an absent code. Direct BYOK provider codes use the separate providerErrorCode field. A code alone does not define remediation or retry policy.
#[derive(Debug, Clone, Default)]
pub struct SessionErrorData {
    /// Typed DLP block, set only when providerErrorCode is a known DLP code. Classified from the exact code, never from the message or HTTP status. Other provider codes remain in providerErrorCode.
    pub dlp_block: Option<DlpBlockKind>,
    /// Only set on `errorType: "rate_limit"`. When `true`, the runtime will follow this error with an `auto_mode_switch.requested` event (or silently switch if `continueOnAutoMode` is enabled). UI clients can use this flag to suppress duplicate rendering of the rate-limit error when they show their own auto-mode-switch prompt.
    pub eligible_for_auto_switch: Option<bool>,
    /// Optional fine-grained runtime or Copilot API classification; an extensible string, not a closed enum. May be absent; clients must tolerate unknown values. Runtime-local codes include `ai_credit_soft_cap_exhausted` (`errorType: "session_limits"`) and `no_account` (`errorType: "authentication"`): the session has no account, or no credential resolves for its account, so no request was sent. Session RPC rejections for the same state carry `no_account` in their error data. Policy codes include `misalignment_policy_violation` and `cyber_policy`. CAPI rate limits normalize missing or unknown codes to `rate_limited`; recognized codes also include `user_model_rate_limited`, `user_global_rate_limited`, `user_weekly_rate_limited`, and `integration_rate_limited`. HTTP 402 quota failures forward upstream codes, including unknown values, or omit the field if no code is available. Other terminal CAPI query failures expose only reviewed codes. Direct BYOK provider codes use providerErrorCode instead. A code alone does not define remediation or retry policy.
    pub error_code: Option<String>,
    /// Category of error (e.g., "authentication", "authorization", "policy", "quota", "rate_limit", "context_limit", "query")
    pub error_type: String,
    /// Human-readable error message
    pub message: String,
    /// Original root user-message ID of the failed request, including failures before user.message is emitted. Absent for session-wide errors or legacy hosts.
    pub originating_message_id: Option<String>,
    /// GitHub request tracing ID (x-github-request-id header) for correlating with server-side logs
    pub provider_call_id: Option<String>,
    /// Exact error code from the model endpoint's structured error envelope (Copilot API, BYOK, or another provider), preserved verbatim when available; unset otherwise. Unlike errorCode, this is not a runtime or Copilot API classification and does not imply a remediation or retry policy.
    pub provider_error_code: Option<String>,
    /// Sanitized provider-defined error parameter preserved from a structured upstream error envelope, when available.
    pub provider_error_param: Option<String>,
    /// Provider-defined error type preserved from a structured upstream error envelope, when available (for example, `invalid_request_error`).
    pub provider_error_type: Option<String>,
    /// What the user must do to recover, when the runtime knows of an action. The `message` never names a client affordance, so a client that offers one — a slash command, a settings pane, a link — renders it from this value.
    pub remediation: Option<RemediationAction>,
    /// Copilot service request ID (x-copilot-service-request-id header) for CAPI log correlation
    pub service_request_id: Option<String>,
    /// Error stack trace, when available
    pub stack: Option<String>,
    /// HTTP status code from the upstream request, if applicable
    pub status_code: Option<i32>,
    /// Identifier of the agent-loop turn whose model call failed, matching that turn's assistant.turn_start turnId within the request named by originatingMessageId. Absent when the failure happened outside a model turn.
    pub turn_id: Option<String>,
    /// Optional URL associated with this error that the user can open in a browser
    pub url: Option<String>,
}
crate::record_serde::wire_record!(SessionErrorData { dlp_block: "dlpBlock" if is_none, eligible_for_auto_switch: "eligibleForAutoSwitch" if is_none, error_code: "errorCode" if is_none, error_type: "errorType", message: "message", originating_message_id: "originatingMessageId" if is_none, provider_call_id: "providerCallId" if is_none, provider_error_code: "providerErrorCode" if is_none, provider_error_param: "providerErrorParam" if is_none, provider_error_type: "providerErrorType" if is_none, remediation: "remediation" if is_none, service_request_id: "serviceRequestId" if is_none, stack: "stack" if is_none, status_code: "statusCode" if is_none, turn_id: "turnId" if is_none, url: "url" if is_none });

/// Session event "session.idle". Payload indicating the session is idle with no background agents or attached shell commands in flight
#[derive(Debug, Clone, Default)]
pub struct SessionIdleData {
    /// True when the preceding agentic loop was cancelled via abort signal
    pub aborted: Option<bool>,
    /// The session mode the agent was operating in when it went idle, when the mode is known. Lets turn-scoped consumers distinguish an autopilot continuation boundary (where the agent keeps working after this idle) from a genuine turn completion.
    pub mode: Option<SessionMode>,
}
crate::record_serde::wire_record!(SessionIdleData { aborted: "aborted" if is_none, mode: "mode" if is_none });

/// Session event "session.autopilot_stopped". The runtime has stopped continuing one Autopilot request. Attached background work may remain active; this is not session-wide idle.
#[derive(Debug, Clone, Default)]
pub struct SessionAutopilotStoppedData {
    /// The terminal policy reason, such as task_complete, aborted, error, or background_timeout.
    pub reason: String,
    /// Original root user message ID for this request, absent when no root request is available.
    pub request_id: Option<RequestId>,
}
crate::record_serde::wire_record!(SessionAutopilotStoppedData { reason: "reason", request_id: "requestId" if is_none });

/// Session event "session.title_changed". Session title change payload containing the new display title
#[derive(Debug, Clone, Default)]
pub struct SessionTitleChangedData {
    /// The new display title for the session
    pub title: String,
}
crate::record_serde::wire_record!(SessionTitleChangedData { title: "title" });

/// Session event "session.schedule_created". Scheduled prompt registered via /every or /after
#[derive(Debug, Clone, Default)]
pub struct SessionScheduleCreatedData {
    /// Absolute fire time (epoch milliseconds) for a one-shot calendar schedule
    pub at: Option<i64>,
    /// 5-field cron expression for a recurring calendar schedule, evaluated in `tz`
    pub cron: Option<String>,
    /// Optional user-facing label shown in the timeline instead of the actual prompt (e.g. `/skill-name args` when the prompt is a skill invocation expansion)
    pub display_prompt: Option<String>,
    /// Sequential id assigned to the scheduled prompt within the session
    pub id: i64,
    /// Interval between ticks in milliseconds (relative-interval schedules)
    pub interval_ms: Option<i64>,
    /// Who created the schedule (`user` or `model`). Persisted so a resumed session keeps gating non-user schedules from firing skills that opted out of model invocation. Absent on entries created before this field existed; a missing origin fails closed (treated the same as a non-user origin), so such a schedule may not resolve a `disable-model-invocation` skill.
    pub origin: Option<ScheduleOrigin>,
    /// Prompt text that gets enqueued on every tick
    pub prompt: String,
    /// Whether the schedule re-arms after each tick (`/every`) or fires once (`/after`)
    pub recurring: Option<bool>,
    /// True for a self-paced (`dynamic`) schedule: no fixed cadence; the model arms each next run via the `manage_schedule` `wakeup` action. `nextRunAt` is model-controlled rather than auto-computed.
    pub self_paced: Option<bool>,
    /// IANA timezone the `cron` expression is evaluated in
    pub tz: Option<String>,
}
crate::record_serde::wire_record!(SessionScheduleCreatedData { at: "at" if is_none, cron: "cron" if is_none, display_prompt: "displayPrompt" if is_none, id: "id", interval_ms: "intervalMs" if is_none, origin: "origin" if is_none, prompt: "prompt", recurring: "recurring" if is_none, self_paced: "selfPaced" if is_none, tz: "tz" if is_none });

/// Session event "session.schedule_cancelled". Scheduled prompt cancelled from the schedule manager dialog
#[derive(Debug, Clone, Default)]
pub struct SessionScheduleCancelledData {
    /// Id of the scheduled prompt that was cancelled
    pub id: i64,
}
crate::record_serde::wire_record!(SessionScheduleCancelledData { id: "id" });

/// Session event "session.schedule_rearmed". Self-paced schedule re-armed for its next run
#[derive(Debug, Clone, Default)]
pub struct SessionScheduleRearmedData {
    /// Id of the self-paced schedule that was re-armed
    pub id: i64,
    /// Absolute time (epoch milliseconds) the model armed the next run to fire
    pub next_run_at: i64,
}
crate::record_serde::wire_record!(SessionScheduleRearmedData {
    id: "id",
    next_run_at: "nextRunAt"
});

/// Session event "session.autopilot_objective_changed". Autopilot objective state file operation details indicating what changed
#[derive(Debug, Clone, Default)]
pub struct SessionAutopilotObjectiveChangedData {
    /// Current autopilot objective id, if one exists
    pub id: Option<i64>,
    /// The type of operation performed on the autopilot objective state file
    pub operation: AutopilotObjectiveChangedOperation,
    /// Current autopilot objective status, if one exists
    pub status: Option<AutopilotObjectiveChangedStatus>,
}
crate::record_serde::wire_record!(SessionAutopilotObjectiveChangedData { id: "id" if is_none, operation: "operation", status: "status" if is_none });

/// Original request-level and effective conversation reasoning effort for a provider history boundary; the historical type name is retained for compatibility
#[derive(Debug, Clone, Default)]
pub struct ResponsesReasoning {
    /// Effective effort selected before this message, independent of the response-level reasoning field
    pub effort: String,
    /// Original request-level effort, retained while replaying this conversation prefix
    pub initial_effort: String,
    /// Provider model whose reasoning settings this boundary records
    pub model: String,
}
crate::record_serde::wire_record!(ResponsesReasoning {
    effort: "effort",
    initial_effort: "initialEffort",
    model: "model"
});

/// Session event "session.info". Informational message for timeline display with categorization
#[derive(Debug, Clone, Default)]
pub struct SessionInfoData {
    /// Category of informational message (e.g., "notification", "timing", "context_window", "mcp", "snapshot", "configuration", "authentication", "model")
    pub info_type: String,
    /// Human-readable informational message for display in the timeline
    pub message: String,
    /// Credential-free provider routing policy applied at this configuration boundary.
    #[doc(hidden)]
    pub(crate) provider_routing: Option<serde_json::Value>,
    /// Reasoning configuration consumed at this conversation boundary. Present on reasoning_effort updates; preserves the original request baseline and applied effort without creating a user message.
    #[doc(hidden)]
    pub(crate) responses_reasoning: Option<ResponsesReasoning>,
    /// Optional actionable tip displayed with this message
    pub tip: Option<String>,
    /// Optional URL associated with this message that the user can open in a browser
    pub url: Option<String>,
}
crate::record_serde::wire_record!(SessionInfoData { info_type: "infoType", message: "message", provider_routing: "providerRouting" if is_none, responses_reasoning: "responsesReasoning" if is_none, tip: "tip" if is_none, url: "url" if is_none });

#[derive(Debug, Clone, Default)]
pub struct IndexedSearchDataStatus {
    /// Indexed-search event variant discriminator.
    pub kind: IndexedSearchDataStatusKind,
    /// Current indexed-search state for this session activation.
    pub state: IndexedSearchState,
}
crate::record_serde::wire_record!(IndexedSearchDataStatus {
    kind: "kind",
    state: "state"
});

#[derive(Debug, Clone, Default)]
pub struct IndexedSearchDataStartup {
    /// Why indexed search was disabled, when applicable.
    pub disabled_reason: Option<IndexedSearchDisabledReason>,
    /// Whether the repository meets the automatic indexing file-count threshold, when known.
    pub eligible: Option<bool>,
    /// Startup failure details. May contain sensitive user data; restricted telemetry only.
    pub error_message: Option<String>,
    /// Number of text files counted in the repository.
    pub file_count: Option<f64>,
    /// Whether indexed search was explicitly enabled through the environment.
    pub forced_by_env: bool,
    /// Indexed-search event variant discriminator.
    pub kind: IndexedSearchDataStartupKind,
    /// Outcome of this startup attempt.
    pub outcome: IndexedSearchOutcome,
    /// Wall-clock duration of startup in milliseconds.
    pub startup_duration_ms: f64,
    /// Whether waiting for index readiness was requested, including skipped attempts.
    pub warm_start: bool,
}
crate::record_serde::wire_record!(IndexedSearchDataStartup { disabled_reason: "disabledReason" if is_none, eligible: "eligible" if is_none, error_message: "errorMessage" if is_none, file_count: "fileCount" if is_none, forced_by_env: "forcedByEnv", kind: "kind", outcome: "outcome", startup_duration_ms: "startupDurationMs", warm_start: "warmStart" });

#[derive(Debug, Clone, Default)]
pub struct IndexedSearchDataServerError {
    /// Server failure details. May contain sensitive user data; restricted telemetry only.
    pub error_message: Option<String>,
    /// Category of the server failure.
    pub error_type: IndexedSearchErrorType,
    /// Process exit code, when available.
    pub exit_code: Option<f64>,
    /// Indexed-search event variant discriminator.
    pub kind: IndexedSearchDataServerErrorKind,
}
crate::record_serde::wire_record!(IndexedSearchDataServerError { error_message: "errorMessage" if is_none, error_type: "errorType", exit_code: "exitCode" if is_none, kind: "kind" });

#[derive(Debug, Clone, Default)]
pub struct IndexedSearchDataIncremental {
    /// Number of added files.
    pub added_file_count: Option<f64>,
    /// Number of modified files.
    pub changed_file_count: Option<f64>,
    /// Number of deleted files.
    pub deleted_file_count: Option<f64>,
    /// Indexed-search event variant discriminator.
    pub kind: IndexedSearchDataIncrementalKind,
    /// Phase of the incremental index update.
    pub phase: IndexedSearchIncrementalPhase,
    /// Total number of detected changes.
    pub total_change_count: Option<f64>,
    /// Total incremental indexing duration in milliseconds.
    pub total_duration_ms: Option<f64>,
    /// Index update duration in milliseconds.
    pub update_duration_ms: Option<f64>,
    /// Workspace scan duration in milliseconds.
    pub walk_duration_ms: Option<f64>,
}
crate::record_serde::wire_record!(IndexedSearchDataIncremental { added_file_count: "addedFileCount" if is_none, changed_file_count: "changedFileCount" if is_none, deleted_file_count: "deletedFileCount" if is_none, kind: "kind", phase: "phase", total_change_count: "totalChangeCount" if is_none, total_duration_ms: "totalDurationMs" if is_none, update_duration_ms: "updateDurationMs" if is_none, walk_duration_ms: "walkDurationMs" if is_none });

/// Session event "session.warning". Warning message for timeline display with categorization
#[derive(Debug, Clone, Default)]
pub struct SessionWarningData {
    /// Human-readable warning message for display in the timeline
    pub message: String,
    /// Logical ID returned by session.send for the request this warning concerns. Set for policy-blocked prompts, which do not emit a user.message event.
    pub originating_message_id: Option<String>,
    /// What the user must do to recover, when the runtime knows of an action. The `message` never names a client affordance, so a client that offers one — a slash command, a settings pane, a link — renders it from this value.
    pub remediation: Option<RemediationAction>,
    /// Optional URL associated with this warning that the user can open in a browser
    pub url: Option<String>,
    /// Category of warning (e.g., "subscription", "policy", "mcp")
    pub warning_type: String,
}
crate::record_serde::wire_record!(SessionWarningData { message: "message", originating_message_id: "originatingMessageId" if is_none, remediation: "remediation" if is_none, url: "url" if is_none, warning_type: "warningType" });

/// Session event "session.model_change". Model change details including previous and new model identifiers
#[derive(Debug, Clone, Default)]
pub struct SessionModelChangeData {
    /// Committed Auto preference after the model configuration change, when applicable.
    pub auto_tier: Option<AutoTier>,
    /// True when autoTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub auto_tier_managed: Option<bool>,
    /// Reason the change happened, when not user-initiated. `"rate_limit_auto_switch"` for changes triggered by the auto-mode-switch rate-limit recovery path, `"auto_mode_recovery"` when a prepared Auto route commits and a recovered inference is allowed to proceed before provider dispatch, or `"refusal_fallback"` when the active model declined a request (content refusal) and the runtime switched to the configured refusal-fallback model. UI clients can use this to render contextual copy.
    pub cause: Option<String>,
    /// Context tier after the model change; null explicitly clears a previously selected tier
    pub context_tier: Option<ContextTier>,
    /// True when contextTier is a managed-policy default. Omitted for user-authored and legacy values.
    pub context_tier_managed: Option<bool>,
    /// Newly selected model identifier
    pub new_model: String,
    /// Previously committed Auto preference, when one was explicitly selected.
    pub previous_auto_tier: Option<AutoTier>,
    /// Model that was previously selected, if any
    pub previous_model: Option<String>,
    /// Provider of previousModel, when known. A provider-only change is a model selection change even when the model identifiers are equal.
    pub previous_provider_id: Option<String>,
    /// Reasoning effort level before the model change, if applicable
    pub previous_reasoning_effort: Option<String>,
    /// Reasoning summary mode before the model change, if applicable
    pub previous_reasoning_summary: Option<ReasoningSummary>,
    /// Output verbosity level before the model change, if applicable
    pub previous_verbosity: Option<Verbosity>,
    /// Provider selected for newModel. Omitted for legacy or unattributed selections; never inferred from a later selection.
    pub provider_id: Option<String>,
    /// Credential-free provider routing policy and selected-model registration.
    pub provider_routing: Option<serde_json::Value>,
    /// Reasoning effort level after the model change, if applicable
    pub reasoning_effort: Option<String>,
    /// True when the reasoning effort is a managed-policy default bound to reasoningEffortModel. Omitted for agent-authored, user-authored, independent, and legacy effort.
    pub reasoning_effort_managed: Option<bool>,
    /// Model that owns effort embedded in an authored model selection. Omitted for independent reasoning-effort overrides and legacy events.
    pub reasoning_effort_model: Option<String>,
    /// True when prior reasoning is incompatible with the selected provider routing even though model and provider identifiers are unchanged. Omitted for compatible or legacy selections.
    pub reasoning_invalidated: Option<bool>,
    /// Reasoning summary mode after the model change, if applicable
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Origin of the effective model change, when known.
    pub source: Option<ModelChangeSource>,
    /// Output verbosity level after the model change, if applicable
    pub verbosity: Option<Verbosity>,
}
crate::record_serde::wire_record!(SessionModelChangeData { auto_tier: "autoTier" if is_none, auto_tier_managed: "autoTierManaged" if is_none, cause: "cause" if is_none, context_tier: "contextTier" if is_none, context_tier_managed: "contextTierManaged" if is_none, new_model: "newModel", previous_auto_tier: "previousAutoTier" if is_none, previous_model: "previousModel" if is_none, previous_provider_id: "previousProviderId" if is_none, previous_reasoning_effort: "previousReasoningEffort" if is_none, previous_reasoning_summary: "previousReasoningSummary" if is_none, previous_verbosity: "previousVerbosity" if is_none, provider_id: "providerId" if is_none, provider_routing: "providerRouting" if is_none, reasoning_effort: "reasoningEffort" if is_none, reasoning_effort_managed: "reasoningEffortManaged" if is_none, reasoning_effort_model: "reasoningEffortModel" if is_none, reasoning_invalidated: "reasoningInvalidated" if is_none, reasoning_summary: "reasoningSummary" if is_none, source: "source" if is_none, verbosity: "verbosity" if is_none });

/// Session event "session.model_deselected". The model the user had explicitly selected is no longer available, because the host that published it withdrew it, so the session no longer has an explicit selection. The next turn resolves a default as though the user had never chosen a model. Clients should stop presenting the previous model as selected. This event is durable because resume rebuilds the selected model from the event log; without it a resumed session would restore a model its provider no longer serves. Reasoning effort, verbosity, and other session-level preferences are deliberately unchanged, because they belong to the session rather than to the model.
#[derive(Debug, Clone, Default)]
pub struct SessionModelDeselectedData {
    /// Model that was selected before the host withdrew it.
    pub previous_model: String,
    /// Low-cardinality reason the selection was cleared.
    pub reason: ModelDeselectedReason,
}
crate::record_serde::wire_record!(SessionModelDeselectedData {
    previous_model: "previousModel",
    reason: "reason"
});

/// Session event "session.auto_tier_recommendation". Live-only Auto preference recommendation from Copilot API after a successful Auto model call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionAutoTierRecommendationData {
    /// Recommended Auto preference.
    pub recommended_auto_tier: RecommendedAutoTier,
}
crate::record_serde::wire_record!(SessionAutoTierRecommendationData {
    recommended_auto_tier: "recommendedAutoTier"
});

/// Session event "session.auto_tier_switch_failed". A transient Auto preference failure emitted when the runtime cannot mint or accept a usable model and token pair. The previously effective preference remains active, so SDK clients can surface a non-blocking failure without changing their committed-tier state. This event is ephemeral and is not persisted or replayed on resume.
#[derive(Debug, Clone, Default)]
pub struct SessionAutoTierSwitchFailedData {
    /// Auto preference that remains effective after the failed request.
    pub effective_auto_tier: Option<AutoTier>,
    /// Low-cardinality failure outcome reported by Auto resolution.
    pub reason: AutoTierSwitchFailureReason,
    /// Auto preference that failed to activate, or null when returning to provider-default routing failed.
    pub requested_auto_tier: Option<AutoTier>,
}
crate::record_serde::wire_record!(SessionAutoTierSwitchFailedData { effective_auto_tier: "effectiveAutoTier" if is_none, reason: "reason", requested_auto_tier: "requestedAutoTier" });

/// Session event "session.mode_changed". Agent mode change details including previous and new modes
#[derive(Debug, Clone, Default)]
pub struct SessionModeChangedData {
    /// The session mode the agent is operating in
    pub new_mode: SessionMode,
    /// The session mode the agent is operating in
    pub previous_mode: SessionMode,
}
crate::record_serde::wire_record!(SessionModeChangedData {
    new_mode: "newMode",
    previous_mode: "previousMode"
});

/// Session event "session.mode_notice_delivered". Records that a mode transition notice reached the model so cache-stable mode tools can remain offered across resume.
#[derive(Debug, Clone, Default)]
pub struct SessionModeNoticeDeliveredData {
    /// Model-visible transition notice persisted for a mid-turn delivery
    pub content: Option<String>,
    /// Whether the model-visible transition notice was persisted separately as an instruction.update event
    pub instruction_update_persisted: Option<bool>,
    /// Mode established by the delivered transition notice
    pub mode: SessionMode,
}
crate::record_serde::wire_record!(SessionModeNoticeDeliveredData { content: "content" if is_none, instruction_update_persisted: "instructionUpdatePersisted" if is_none, mode: "mode" });

/// Session event "session.session_limits_changed". Session limits update details. Null clears the limits.
#[derive(Debug, Clone, Default)]
pub struct SessionSessionLimitsChangedData {
    /// Current session limits, or null when no limits are active
    pub session_limits: Option<SessionLimitsConfig>,
}
crate::record_serde::wire_record!(SessionSessionLimitsChangedData {
    session_limits: "sessionLimits"
});

/// Session event "session.permissions_changed". Permission-mode transition details.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionPermissionsChangedData {
    /// Explicit LLM judge model override used by assisted mode; omitted when the provider default applies
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval_model: Option<String>,
    /// Permission mode after the change
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub mode: Option<PermissionMode>,
    /// Permission mode before the change
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub previous_mode: Option<PermissionMode>,
}
crate::record_serde::wire_record!(SessionPermissionsChangedData { assisted_approval_model: "assistedApprovalModel" if is_none, mode: "mode" if is_none, previous_mode: "previousMode" if is_none });

/// Session event "session.plan_changed". Plan file operation details indicating what changed
#[derive(Debug, Clone, Default)]
pub struct SessionPlanChangedData {
    /// The type of operation performed on the plan file
    pub operation: PlanChangedOperation,
}
crate::record_serde::wire_record!(SessionPlanChangedData {
    operation: "operation"
});

/// Session event "session.todos_changed". Signal-only event: the agent's todos or todo_deps table was written to. No payload — clients should call session.plan.readSqlTodosWithDependencies() to fetch the current state. Events arrive in order; clients can debounce on arrival if needed.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionTodosChangedData {}

/// Session event "session.workspace_file_changed". Workspace file change details including path and operation type
#[derive(Debug, Clone, Default)]
pub struct SessionWorkspaceFileChangedData {
    /// Whether the file was newly created or updated
    pub operation: WorkspaceFileChangedOperation,
    /// Relative path within the session workspace files directory
    pub path: String,
}
crate::record_serde::wire_record!(SessionWorkspaceFileChangedData {
    operation: "operation",
    path: "path"
});

/// Repository context for the handed-off session
#[derive(Debug, Clone, Default)]
pub struct HandoffRepository {
    /// Git branch name, if applicable
    pub branch: Option<String>,
    /// Repository name
    pub name: String,
    /// Repository owner (user or organization)
    pub owner: String,
}
crate::record_serde::wire_record!(HandoffRepository { branch: "branch" if is_none, name: "name", owner: "owner" });

/// Session event "session.handoff". Session handoff metadata including source, context, and repository information
#[derive(Debug, Clone, Default)]
pub struct SessionHandoffData {
    /// Additional context information for the handoff
    pub context: Option<String>,
    /// ISO 8601 timestamp when the handoff occurred
    pub handoff_time: String,
    /// GitHub host URL for the source session (e.g., https://github.com or https://tenant.ghe.com)
    pub host: Option<String>,
    /// Session ID of the remote session being handed off
    pub remote_session_id: Option<SessionId>,
    /// Repository context for the handed-off session
    pub repository: Option<HandoffRepository>,
    /// Origin type of the session being handed off
    pub source_type: HandoffSourceType,
    /// Summary of the work done in the source session
    pub summary: Option<String>,
}
crate::record_serde::wire_record!(SessionHandoffData { context: "context" if is_none, handoff_time: "handoffTime", host: "host" if is_none, remote_session_id: "remoteSessionId" if is_none, repository: "repository" if is_none, source_type: "sourceType", summary: "summary" if is_none });

/// Session event "session.truncation". Conversation truncation statistics including token counts and removed content metrics
#[derive(Debug, Clone, Default)]
pub struct SessionTruncationData {
    /// Identifier of the automatic compaction applied just before this truncation, when one was
    pub compaction_attempt_id: Option<String>,
    /// Number of messages removed by truncation
    pub messages_removed_during_truncation: i64,
    /// Identifier of the component that performed truncation (e.g., "BasicTruncator")
    pub performed_by: String,
    /// Number of conversation messages after truncation
    pub post_truncation_messages_length: i64,
    /// Total tokens in conversation messages after truncation
    pub post_truncation_tokens_in_messages: i64,
    /// Number of conversation messages before truncation
    pub pre_truncation_messages_length: i64,
    /// Total tokens in conversation messages before truncation
    pub pre_truncation_tokens_in_messages: i64,
    /// Maximum token count for the model's context window
    pub token_limit: i64,
    /// Number of tokens removed by truncation
    pub tokens_removed_during_truncation: i64,
}
crate::record_serde::wire_record!(SessionTruncationData { compaction_attempt_id: "compactionAttemptId" if is_none, messages_removed_during_truncation: "messagesRemovedDuringTruncation", performed_by: "performedBy", post_truncation_messages_length: "postTruncationMessagesLength", post_truncation_tokens_in_messages: "postTruncationTokensInMessages", pre_truncation_messages_length: "preTruncationMessagesLength", pre_truncation_tokens_in_messages: "preTruncationTokensInMessages", token_limit: "tokenLimit", tokens_removed_during_truncation: "tokensRemovedDuringTruncation" });

/// Session event "session.snapshot_rewind". Session rewind details including target event and count of removed events
#[derive(Debug, Clone, Default)]
pub struct SessionSnapshotRewindData {
    /// The removed events, starting with `upToEventId`. Later events not listed were kept, such as a background agent's events that interleaved with a withdrawn turn
    pub event_ids: Option<Vec<String>>,
    /// Number of events that were removed by the rewind
    pub events_removed: i64,
    /// First removed event. Without `eventIds`, it and every event after it were removed
    pub up_to_event_id: String,
}
crate::record_serde::wire_record!(SessionSnapshotRewindData { event_ids: "eventIds" if is_none, events_removed: "eventsRemoved", up_to_event_id: "upToEventId" });

/// Request count and cost metrics for this model
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsModelMetricRequests {
    /// User-initiated premium request cost (with multiplier applied)
    pub cost: f64,
    /// Number of API requests made with this model
    pub count: i64,
}
crate::record_serde::wire_record!(UsageMetricsModelMetricRequests {
    cost: "cost",
    count: "count"
});

/// Per-model token-detail entry containing the accumulated token count for one token type.
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsModelMetricTokenDetail {
    /// Accumulated token count for this token type
    pub token_count: i64,
}
crate::record_serde::wire_record!(UsageMetricsModelMetricTokenDetail {
    token_count: "tokenCount"
});

/// Token usage metrics for this model
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsModelMetricUsage {
    /// Total tokens read from prompt cache
    pub cache_read_tokens: i64,
    /// Total tokens written to prompt cache
    pub cache_write_tokens: i64,
    /// Total input tokens consumed
    pub input_tokens: i64,
    /// Total output tokens produced
    pub output_tokens: i64,
    /// Total output tokens used for reasoning
    pub reasoning_tokens: Option<i64>,
}
crate::record_serde::wire_record!(UsageMetricsModelMetricUsage { cache_read_tokens: "cacheReadTokens", cache_write_tokens: "cacheWriteTokens", input_tokens: "inputTokens", output_tokens: "outputTokens", reasoning_tokens: "reasoningTokens" if is_none });

/// Per-model usage metrics, including request counts/costs, token usage, nano-AI units, and per-token-type details.
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsModelMetric {
    /// Completeness of this model's reported AI-credit subtotal. Explicitly reported zero is complete, not unavailable.
    pub ai_credits_status: Option<AiCreditsStatus>,
    /// Latest known prompt-cache expiration for this model. A timestamp in the past indicates that the observed cache has expired.
    pub cache_expires_at: Option<String>,
    /// Request count and cost metrics for this model
    pub requests: UsageMetricsModelMetricRequests,
    /// Token count details per type
    pub token_details: Option<HashMap<String, UsageMetricsModelMetricTokenDetail>>,
    /// Accumulated nano-AI units cost for this model
    pub total_nano_aiu: Option<f64>,
    /// Token usage metrics for this model
    pub usage: UsageMetricsModelMetricUsage,
}
crate::record_serde::wire_record!(UsageMetricsModelMetric { ai_credits_status: "aiCreditsStatus" if is_none, cache_expires_at: "cacheExpiresAt" if is_none, requests: "requests", token_details: "tokenDetails" if is_none, total_nano_aiu: "totalNanoAiu" if is_none, usage: "usage" });

/// Usage attributed to one agent instance, including its identity, API duration, AI units, and per-model breakdown.
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsAgentMetric {
    /// Human-readable label for this subagent invocation, copied from the originating `subagent.started` event. For task-tool subagents this is the invocation's task description rather than the agent's configured display name, so group by `agentName` for stable per-agent labels.
    pub agent_display_name: Option<String>,
    /// Configured agent name, when this is a subagent
    pub agent_name: Option<String>,
    /// Per-model usage for this agent, keyed by model identifier
    pub model_metrics: HashMap<String, UsageMetricsModelMetric>,
    /// Time spent in model API calls by this agent, in milliseconds
    pub total_api_duration_ms: i64,
    /// Accumulated nano-AI units cost for this agent
    pub total_nano_aiu: f64,
}
crate::record_serde::wire_record!(UsageMetricsAgentMetric { agent_display_name: "agentDisplayName" if is_none, agent_name: "agentName" if is_none, model_metrics: "modelMetrics", total_api_duration_ms: "totalApiDurationMs", total_nano_aiu: "totalNanoAiu" });

/// Aggregated code change metrics
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsCodeChanges {
    /// Distinct file paths modified during the session
    pub files_modified: Vec<String>,
    /// Number of distinct files modified
    pub files_modified_count: i64,
    /// Total lines of code added
    pub lines_added: i64,
    /// Total lines of code removed
    pub lines_removed: i64,
}
crate::record_serde::wire_record!(UsageMetricsCodeChanges {
    files_modified: "filesModified",
    files_modified_count: "filesModifiedCount",
    lines_added: "linesAdded",
    lines_removed: "linesRemoved"
});

/// A neutral reference to the model provider that produced a model: an opaque id, a human-readable label, and the provider kind. Carried on each enumerated Model so consumers can group by provider without reaching into a provider-shaped internal type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelProviderRef {
    /// Opaque, stable id of the provider that produced this model. Matches the enumerated `ModelProviderDescriptor.id`.
    pub id: String,
    /// The provider kind.
    pub kind: ModelProviderKind,
    /// Human-readable provider label, owned by the runtime so every consumer renders identical text.
    pub label: String,
}
crate::record_serde::wire_record!(ModelProviderRef {
    id: "id",
    kind: "kind",
    label: "label"
});

/// Usage for one recorded provider and model, without merging identical model IDs across providers.
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsProviderModelMetric {
    /// Request, token, and cost totals for this provider/model.
    pub metrics: UsageMetricsModelMetric,
    /// Model display name captured at call time, when known.
    pub model_display_name: Option<String>,
    /// Model identity, or null for legacy aggregate-only usage.
    pub model_id: Option<String>,
    /// Provider identity and product label captured when the call was dispatched; null when unknown.
    pub provider: Option<ModelProviderRef>,
}
crate::record_serde::wire_record!(UsageMetricsProviderModelMetric { metrics: "metrics", model_display_name: "modelDisplayName" if is_none, model_id: "modelId", provider: "provider" });

/// Session-wide token-detail entry containing the accumulated token count for one token type.
#[derive(Debug, Clone, Default)]
pub struct UsageMetricsTokenDetail {
    /// Accumulated token count for this token type
    pub token_count: i64,
}
crate::record_serde::wire_record!(UsageMetricsTokenDetail {
    token_count: "tokenCount"
});

/// Accumulated session usage metrics, including premium request cost, token counts, model breakdown, and code-change totals.
#[derive(Debug, Clone, Default)]
pub struct UsageGetMetricsResult {
    /// Per-agent usage metrics, keyed by agent instance identifier. The main conversation uses the stable key `main`.
    pub agent_metrics: Option<HashMap<String, UsageMetricsAgentMetric>>,
    /// Completeness of totalNanoAiu. Numeric totals remain the reported subtotal; zero is not a claim of free usage when unavailable.
    pub ai_credits_status: Option<AiCreditsStatus>,
    /// Aggregated code change metrics
    pub code_changes: UsageMetricsCodeChanges,
    /// Currently active model identifier
    pub current_model: Option<String>,
    /// Input tokens from the most recent main-agent API call
    pub last_call_input_tokens: i64,
    /// Output tokens from the most recent main-agent API call
    pub last_call_output_tokens: i64,
    /// Per-model token and request metrics, keyed by model identifier
    pub model_metrics: HashMap<String, UsageMetricsModelMetric>,
    /// Provider-attributed model metrics, ordered by first observed provider and model.
    /// Null attribution preserves historical usage whose provider or model was not recorded.
    pub provider_model_metrics: Option<Vec<UsageMetricsProviderModelMetric>>,
    /// ISO 8601 timestamp when the session started
    pub session_start_time: String,
    /// Session-wide per-token-type accumulated token counts
    pub token_details: Option<HashMap<String, UsageMetricsTokenDetail>>,
    /// Total time spent in model API calls (milliseconds)
    pub total_api_duration_ms: i64,
    /// Session-wide accumulated nano-AI units cost
    pub total_nano_aiu: Option<f64>,
    /// Total user-initiated premium request cost across all models (may be fractional due to multipliers)
    pub total_premium_request_cost: f64,
    /// Raw count of user-initiated API requests
    pub total_user_requests: i64,
}
crate::record_serde::wire_record!(UsageGetMetricsResult { agent_metrics: "agentMetrics" if is_none, ai_credits_status: "aiCreditsStatus" if is_none, code_changes: "codeChanges", current_model: "currentModel" if is_none, last_call_input_tokens: "lastCallInputTokens", last_call_output_tokens: "lastCallOutputTokens", model_metrics: "modelMetrics", provider_model_metrics: "providerModelMetrics" if is_none, session_start_time: "sessionStartTime", token_details: "tokenDetails" if is_none, total_api_duration_ms: "totalApiDurationMs", total_nano_aiu: "totalNanoAiu" if is_none, total_premium_request_cost: "totalPremiumRequestCost", total_user_requests: "totalUserRequests" });

/// Request count and cost metrics
#[derive(Debug, Clone, Default)]
pub struct ShutdownModelMetricRequests {
    /// Cumulative cost multiplier for requests to this model
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub cost: Option<f64>,
    /// Total number of API requests made to this model
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub count: Option<i64>,
}
crate::record_serde::wire_record!(ShutdownModelMetricRequests { cost: "cost" if is_none, count: "count" if is_none });

/// A token-type entry in a shutdown model metric, storing the accumulated token count.
#[derive(Debug, Clone, Default)]
pub struct ShutdownModelMetricTokenDetail {
    /// Accumulated token count for this token type
    pub token_count: i64,
}
crate::record_serde::wire_record!(ShutdownModelMetricTokenDetail {
    token_count: "tokenCount"
});

/// Token usage breakdown
#[derive(Debug, Clone, Default)]
pub struct ShutdownModelMetricUsage {
    /// Total tokens read from prompt cache across all requests
    pub cache_read_tokens: i64,
    /// Total tokens written to prompt cache across all requests
    pub cache_write_tokens: i64,
    /// Total input tokens consumed across all requests to this model
    pub input_tokens: i64,
    /// Total output tokens produced across all requests to this model
    pub output_tokens: i64,
    /// Total reasoning tokens produced across all requests to this model
    pub reasoning_tokens: Option<i64>,
}
crate::record_serde::wire_record!(ShutdownModelMetricUsage { cache_read_tokens: "cacheReadTokens", cache_write_tokens: "cacheWriteTokens", input_tokens: "inputTokens", output_tokens: "outputTokens", reasoning_tokens: "reasoningTokens" if is_none });

/// Per-model shutdown metrics with request counts, token usage, nano-AI units, and token details.
#[derive(Debug, Clone, Default)]
pub struct ShutdownModelMetric {
    /// Request count and cost metrics
    pub requests: ShutdownModelMetricRequests,
    /// Token count details per type
    pub token_details: Option<HashMap<String, ShutdownModelMetricTokenDetail>>,
    /// Accumulated nano-AI units cost for this model
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub total_nano_aiu: Option<f64>,
    /// Token usage breakdown
    pub usage: ShutdownModelMetricUsage,
}
crate::record_serde::wire_record!(ShutdownModelMetric { requests: "requests", token_details: "tokenDetails" if is_none, total_nano_aiu: "totalNanoAiu" if is_none, usage: "usage" });

/// Usage attributed to one agent instance at session shutdown.
#[derive(Debug, Clone, Default)]
pub struct ShutdownAgentMetric {
    /// Human-readable label for this subagent invocation, copied from the originating `subagent.started` event. For task-tool subagents this is the invocation's task description rather than the agent's configured display name, so group by `agentName` for stable per-agent labels.
    pub agent_display_name: Option<String>,
    /// Configured agent name, when this is a subagent
    pub agent_name: Option<String>,
    /// Per-model usage for this agent, keyed by model identifier
    pub model_metrics: HashMap<String, ShutdownModelMetric>,
    /// Time spent in model API calls by this agent, in milliseconds
    pub total_api_duration_ms: i64,
    /// Accumulated nano-AI units cost for this agent
    pub total_nano_aiu: f64,
}
crate::record_serde::wire_record!(ShutdownAgentMetric { agent_display_name: "agentDisplayName" if is_none, agent_name: "agentName" if is_none, model_metrics: "modelMetrics", total_api_duration_ms: "totalApiDurationMs", total_nano_aiu: "totalNanoAiu" });

/// Aggregate code change metrics for the session
#[derive(Debug, Clone, Default)]
pub struct ShutdownCodeChanges {
    /// List of file paths that were modified during the session
    pub files_modified: Vec<String>,
    /// Total number of lines added during the session
    pub lines_added: i64,
    /// Total number of lines removed during the session
    pub lines_removed: i64,
}
crate::record_serde::wire_record!(ShutdownCodeChanges {
    files_modified: "filesModified",
    lines_added: "linesAdded",
    lines_removed: "linesRemoved"
});

/// A session-wide shutdown token-type entry storing the accumulated token count.
#[derive(Debug, Clone, Default)]
pub struct ShutdownTokenDetail {
    /// Accumulated token count for this token type
    pub token_count: i64,
}
crate::record_serde::wire_record!(ShutdownTokenDetail {
    token_count: "tokenCount"
});

/// Session event "session.shutdown". Session termination metrics including usage statistics, code changes, and shutdown reason
#[derive(Debug, Clone, Default)]
pub struct SessionShutdownData {
    /// Complete accounting state captured atomically with receipt watermarks.
    #[doc(hidden)]
    pub(crate) accounting_snapshot: Option<UsageGetMetricsResult>,
    /// Per-agent usage breakdown, keyed by agent instance identifier. The main conversation uses the stable key `main`.
    pub agent_metrics: Option<HashMap<String, ShutdownAgentMetric>>,
    /// Aggregate code change metrics for the session
    pub code_changes: ShutdownCodeChanges,
    /// Non-system message token count at shutdown
    pub conversation_tokens: Option<i64>,
    /// Model that was selected at the time of shutdown
    pub current_model: Option<String>,
    /// Total tokens in context window at shutdown
    pub current_tokens: Option<i64>,
    /// Error description when shutdownType is "error"
    pub error_reason: Option<String>,
    /// On-disk byte size of the session's persisted events.jsonl file at shutdown time; omitted when the file does not exist or cannot be stat'd
    pub events_file_size_bytes: Option<i64>,
    /// Per-model usage breakdown, keyed by model identifier
    pub model_metrics: HashMap<String, ShutdownModelMetric>,
    /// Provider/model accounting preserved for replay without consulting current provider selection.
    pub provider_model_metrics: Option<Vec<UsageMetricsProviderModelMetric>>,
    /// Unix timestamp (milliseconds) when the session started
    pub session_start_time: i64,
    /// Whether the session ended normally ("routine") or due to a crash/fatal error ("error")
    pub shutdown_type: ShutdownType,
    /// System message token count at shutdown
    pub system_tokens: Option<i64>,
    /// Session-wide per-token-type accumulated token counts
    pub token_details: Option<HashMap<String, ShutdownTokenDetail>>,
    /// Tool definitions token count at shutdown
    pub tool_definitions_tokens: Option<i64>,
    /// Runtime tool-operation identities already included in the final accounting snapshot.
    #[doc(hidden)]
    pub(crate) tool_usage_request_ids: Option<Vec<String>>,
    /// Cumulative time spent in API calls during the session, in milliseconds
    pub total_api_duration_ms: i64,
    /// Session-wide accumulated nano-AI units cost
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub total_nano_aiu: Option<f64>,
    /// Total number of premium API requests used during the session
    #[doc(hidden)]
    pub(crate) total_premium_requests: Option<f64>,
    /// Per-source accounting sequence watermarks subsuming preceding usage receipts.
    pub usage_accounting_watermarks: Option<HashMap<String, i64>>,
}
crate::record_serde::wire_record!(SessionShutdownData { accounting_snapshot: "accountingSnapshot" if is_none, agent_metrics: "agentMetrics" if is_none, code_changes: "codeChanges", conversation_tokens: "conversationTokens" if is_none, current_model: "currentModel" if is_none, current_tokens: "currentTokens" if is_none, error_reason: "errorReason" if is_none, events_file_size_bytes: "eventsFileSizeBytes" if is_none, model_metrics: "modelMetrics", provider_model_metrics: "providerModelMetrics" if is_none, session_start_time: "sessionStartTime", shutdown_type: "shutdownType", system_tokens: "systemTokens" if is_none, token_details: "tokenDetails" if is_none, tool_definitions_tokens: "toolDefinitionsTokens" if is_none, tool_usage_request_ids: "toolUsageRequestIds" if is_none, total_api_duration_ms: "totalApiDurationMs", total_nano_aiu: "totalNanoAiu" if is_none, total_premium_requests: "totalPremiumRequests" if is_none, usage_accounting_watermarks: "usageAccountingWatermarks" if is_none });

/// Internal prompt-cache expiration state for one model
#[derive(Debug, Clone, Default)]
pub(crate) struct UsageCheckpointModelCacheState {
    /// Latest known prompt-cache expiration
    pub cache_expires_at: String,
    /// Retained cache lifetime in seconds, used to refresh expiration after a cache read
    #[doc(hidden)]
    pub(crate) cache_ttl_seconds: i64,
    /// Model identifier associated with this cache state
    pub model_id: String,
}
crate::record_serde::wire_record!(UsageCheckpointModelCacheState {
    cache_expires_at: "cacheExpiresAt",
    cache_ttl_seconds: "cacheTtlSeconds",
    model_id: "modelId"
});

/// Session event "session.usage_checkpoint". Durable session usage checkpoint for reconstructing aggregate accounting on resume
#[derive(Debug, Clone, Default)]
pub struct SessionUsageCheckpointData {
    /// Complete accounting state captured atomically with the receipt watermarks.
    #[doc(hidden)]
    pub(crate) accounting_snapshot: Option<UsageGetMetricsResult>,
    /// Internal per-model prompt-cache state used to restore expiration tracking on resume
    #[doc(hidden)]
    pub(crate) model_cache_state: Option<Vec<UsageCheckpointModelCacheState>>,
    /// Internal per-conversation prompt-cache-break detector baselines restored on resume
    #[doc(hidden)]
    pub(crate) prompt_cache_break_state: Option<Vec<serde_json::Value>>,
    /// Provider/model accounting snapshot that subsumes preceding usage receipts.
    pub provider_model_metrics: Option<Vec<UsageMetricsProviderModelMetric>>,
    /// Runtime tool-operation identities already included in this checkpoint. Prevents duplicate charges when an operation is delivered again with a newer sequence.
    #[doc(hidden)]
    pub(crate) tool_usage_request_ids: Option<Vec<String>>,
    /// Session-wide accumulated nano-AI units cost at checkpoint time
    pub total_nano_aiu: f64,
    /// Total number of premium API requests used at checkpoint time
    #[doc(hidden)]
    pub(crate) total_premium_requests: Option<f64>,
    /// Highest accounted sequence per source session; no per-call deduplication history is retained.
    pub usage_accounting_watermarks: Option<HashMap<String, i64>>,
}
crate::record_serde::wire_record!(SessionUsageCheckpointData { accounting_snapshot: "accountingSnapshot" if is_none, model_cache_state: "modelCacheState" if is_none, prompt_cache_break_state: "promptCacheBreakState" if is_none, provider_model_metrics: "providerModelMetrics" if is_none, tool_usage_request_ids: "toolUsageRequestIds" if is_none, total_nano_aiu: "totalNanoAiu", total_premium_requests: "totalPremiumRequests" if is_none, usage_accounting_watermarks: "usageAccountingWatermarks" if is_none });

/// Authoritative budget measurements and policy metadata, independent of provider.
#[derive(Debug, Clone, Default)]
pub struct ProviderQuotaBudgetMetadata {
    /// Exact measured consumption in the budget's unit.
    pub consumed: f64,
    /// Exact budget entitlement, retaining incumbent fractional-unit compatibility.
    pub entitlement: f64,
    /// Usage beyond entitlement, in the budget's unit.
    pub overage: f64,
    /// Whether additional usage is allowed when the budget is exhausted.
    pub overage_allowed_when_exhausted: bool,
    /// Optional additional-usage budget cap.
    pub overage_limit: Option<f64>,
    /// Service-reported remaining percentage for authoritative budget presentation.
    pub remaining_percentage: f64,
    /// Reset instant in epoch milliseconds when this budget actually defines a window.
    pub reset_at_epoch_ms: Option<f64>,
    /// Whether the reset instant is an estimate.
    pub reset_estimated: Option<bool>,
    /// Whether this budget uses token-based billing.
    pub token_based_billing: Option<bool>,
    /// Whether the budget has unlimited entitlement.
    pub unlimited: bool,
    /// Whether service policy allows continued usage after exhaustion.
    pub usage_allowed_when_exhausted: bool,
}
crate::record_serde::wire_record!(ProviderQuotaBudgetMetadata { consumed: "consumed", entitlement: "entitlement", overage: "overage", overage_allowed_when_exhausted: "overageAllowedWhenExhausted", overage_limit: "overageLimit" if is_none, remaining_percentage: "remainingPercentage", reset_at_epoch_ms: "resetAtEpochMs" if is_none, reset_estimated: "resetEstimated" if is_none, token_based_billing: "tokenBasedBilling" if is_none, unlimited: "unlimited", usage_allowed_when_exhausted: "usageAllowedWhenExhausted" });

/// An authoritative monthly usage reading for the observation's provider, account, and service.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderMonthlyUsage {
    /// Nonnegative finite consumption reported by the service, preserving zero and fractions. Present only when state is available; never computed from tokens or balance differences.
    pub consumed_quantity: Option<f64>,
    /// Start of the service-reported monthly billing cycle, in UTC RFC 3339.
    pub cycle_start: Option<String>,
    /// Service read time in UTC RFC 3339, not a ledger reconciliation watermark or confirmation of the latest inference charge.
    pub queried_at: Option<String>,
    /// End of the service-reported monthly billing cycle, in UTC RFC 3339.
    pub reset_on: Option<String>,
    /// Identity scope of the reading within its provider and service.
    pub scope: ProviderMonthlyUsageScope,
    /// Availability of monthly consumption. Missing or unavailable usage must never be interpreted as zero.
    pub state: ProviderMonthlyUsageState,
    /// Unit of consumedQuantity; independent of the parent observation's balance unit.
    pub unit: ProviderQuotaUnit,
}
crate::record_serde::wire_record!(ProviderMonthlyUsage { consumed_quantity: "consumedQuantity" if is_none, cycle_start: "cycleStart" if is_none, queried_at: "queriedAt" if is_none, reset_on: "resetOn" if is_none, scope: "scope", state: "state", unit: "unit" });

/// An account quota reading. Absence of a quantity is unknown, never zero.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderQuotaState {
    /// Whether the service permits access, independently of balance.
    pub access_state: ProviderQuotaAccessState,
    /// Service error code or client acquisition category, separate from the business-state reason.
    pub acquisition_error: Option<String>,
    /// Acquisition outcome. Unavailable/failed readings have no service verdict; access/capacity unavailable are compatibility placeholders only.
    pub acquisition_status: Option<ProviderQuotaAcquisitionStatus>,
    /// Independently reported signed 64-bit available quantity. Zero does not override the service access/capacity verdict; omission and null are preserved.
    pub available_quantity: Option<i64>,
    /// Optional metadata for an authoritative budget. Advisory balances and admission-only observations do not populate this.
    pub budget_metadata: Option<ProviderQuotaBudgetMetadata>,
    /// Service capacity state; missing quantities do not imply exhaustion.
    pub capacity_state: ProviderQuotaCapacityState,
    /// Key for the backwards-compatible snapshots projection, when the authoritative budget supports that contract.
    pub compatibility_key: Option<String>,
    /// Independently reported signed 64-bit entitlement. -1 is an unlimited sentinel, not a capacity-state rewrite; omission and null are preserved.
    pub entitled_quantity: Option<i64>,
    /// Explicit service admission flag, when reported.
    pub has_quota: Option<bool>,
    /// HTTP status from acquisition, when available.
    pub http_status: Option<i64>,
    /// Service-reported monthly consumption, independent of quota balances and per-call or session cost. Omitted when the service does not report monthly usage.
    pub monthly_usage: Option<ProviderMonthlyUsage>,
    /// Whether this is a GET account reading or a pre-response admission observation. Observations are never merged across kinds.
    pub observation_kind: Option<ProviderQuotaObservationKind>,
    /// When the runtime observed this reading, not a charge timestamp or guarantee that consumption has settled.
    pub observed_at: Option<String>,
    /// Provider/account whose service reported this state.
    pub provider: ModelProviderRef,
    /// Whether quantities are authoritative budget measurements, advisory balances, or absent. Only authoritative budgets support percentage presentation.
    pub quantity_kind: Option<ProviderQuotaQuantityKind>,
    /// Service-owned quota identifier within this provider.
    pub quota_id: String,
    /// Service-reported explanation for the state.
    pub reason: Option<String>,
    /// Service name owning this reading.
    pub service: Option<String>,
    /// Service-reported quota source.
    pub source: Option<String>,
    /// Unit of entitledQuantity and availableQuantity.
    pub unit: ProviderQuotaUnit,
}
crate::record_serde::wire_record!(ProviderQuotaState { access_state: "accessState", acquisition_error: "acquisitionError" if is_none, acquisition_status: "acquisitionStatus" if is_none, available_quantity: "availableQuantity" if is_none, budget_metadata: "budgetMetadata" if is_none, capacity_state: "capacityState", compatibility_key: "compatibilityKey" if is_none, entitled_quantity: "entitledQuantity" if is_none, has_quota: "hasQuota" if is_none, http_status: "httpStatus" if is_none, monthly_usage: "monthlyUsage" if is_none, observation_kind: "observationKind" if is_none, observed_at: "observedAt" if is_none, provider: "provider", quantity_kind: "quantityKind" if is_none, quota_id: "quotaId", reason: "reason" if is_none, service: "service" if is_none, source: "source" if is_none, unit: "unit" });

/// Session event "session.quota_observation". A provider-owned quota observation, distinct from per-call usage and charge accounting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionQuotaObservationData {
    /// The admitted provider's state observation. Admission balances and percentages remain absent; separate monthly usage may report its own amount and cycle.
    pub observation: ProviderQuotaState,
}
crate::record_serde::wire_record!(SessionQuotaObservationData {
    observation: "observation"
});

/// Why the provider could not determine a sensitivity label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SensitivityLabelFailure {
    /// Provider-defined failure category.
    pub category: String,
    /// Whether the provider reported that retrying the request may determine a label.
    pub retryable: bool,
    /// Status code the provider reported for the failure, when present.
    pub status: Option<i32>,
}
crate::record_serde::wire_record!(SensitivityLabelFailure { category: "category", retryable: "retryable", status: "status" if is_none });

/// A sensitivity label the provider applied to one model request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SensitivityLabel {
    /// Display color of the label, when the provider supplied one.
    pub color: Option<String>,
    /// Label identifier, a lowercase hyphenated GUID.
    pub id: String,
    /// Whether the label encrypts content, when the provider stated it explicitly. Absent means unknown.
    pub is_encrypted: Option<bool>,
    /// Display name of the label.
    pub name: String,
    /// Tooltip text describing the label, when the provider supplied one.
    pub tooltip: Option<String>,
}
crate::record_serde::wire_record!(SensitivityLabel { color: "color" if is_none, id: "id", is_encrypted: "isEncrypted" if is_none, name: "name", tooltip: "tooltip" if is_none });

/// Session event "session.sensitivity_label_observation". A provider-reported sensitivity label observation for one model request. Each observation describes only its own request; it neither sets nor clears a session-level label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionSensitivityLabelObservationData {
    /// Why the label could not be determined. Present only when `outcome` is `indeterminate` and the provider described the failure.
    pub failure: Option<SensitivityLabelFailure>,
    /// Identifier of the user interaction that owns the model request, matching assistant.turn_start.interactionId when available.
    pub interaction_id: Option<String>,
    /// The inherited label. Present only when `outcome` is `inherited`.
    pub label: Option<SensitivityLabel>,
    /// How the provider resolved the label for this request.
    pub outcome: SensitivityLabelOutcome,
    /// Provider that served the model request.
    pub provider: ModelProviderRef,
    /// Provider request identifier of the response that carried the observation, when known.
    pub request_id: Option<RequestId>,
    /// Agent-loop iteration within the interaction that issued the model request.
    pub turn_id: String,
}
crate::record_serde::wire_record!(SessionSensitivityLabelObservationData { failure: "failure" if is_none, interaction_id: "interactionId" if is_none, label: "label" if is_none, outcome: "outcome", provider: "provider", request_id: "requestId" if is_none, turn_id: "turnId" });

/// Session event "session.context_changed". Updated working directory and git context after the change
#[derive(Debug, Clone, Default)]
pub struct SessionContextChangedData {
    /// Base commit of current git branch at session start time
    pub base_commit: Option<String>,
    /// Current git branch name
    pub branch: Option<String>,
    /// Current working directory path
    pub cwd: String,
    /// Root directory of the git repository, resolved via git rev-parse
    pub git_root: Option<String>,
    /// Head commit of current git branch at session start time
    pub head_commit: Option<String>,
    /// Hosting platform type of the repository (github or ado)
    pub host_type: Option<WorkingDirectoryContextHostType>,
    /// Set on the immediate preliminary event of a working-directory change, before the git context is resolved. A settled follow-up event (enriched with git context, or cwd-only for a non-repository) is always emitted afterward, so observers may defer to it. Absent on standalone/final events (e.g. relay context changes).
    pub pending_git_context: Option<bool>,
    /// Repository identifier derived from the git remote URL ("owner/name" for GitHub, "org/project/repo" for Azure DevOps)
    pub repository: Option<String>,
    /// Raw host string from the git remote URL (e.g. "github.com", "mycompany.ghe.com", "dev.azure.com")
    pub repository_host: Option<String>,
}
crate::record_serde::wire_record!(SessionContextChangedData { base_commit: "baseCommit" if is_none, branch: "branch" if is_none, cwd: "cwd", git_root: "gitRoot" if is_none, head_commit: "headCommit" if is_none, host_type: "hostType" if is_none, pending_git_context: "pendingGitContext" if is_none, repository: "repository" if is_none, repository_host: "repositoryHost" if is_none });

/// Session event "session.usage_info". Current context window usage statistics including token and message counts
#[derive(Debug, Clone, Default)]
pub struct SessionUsageInfoData {
    /// Token count from non-system messages (user, assistant, tool)
    pub conversation_tokens: Option<i64>,
    /// Current number of tokens in the context window
    pub current_tokens: i64,
    /// Whether this is the first usage_info event emitted in this session
    pub is_initial: Option<bool>,
    /// Current number of messages in the conversation
    pub messages_length: i64,
    /// Token count from system message(s)
    pub system_tokens: Option<i64>,
    /// Maximum token count for the model's context window
    pub token_limit: i64,
    /// Token count from tool definitions
    pub tool_definitions_tokens: Option<i64>,
}
crate::record_serde::wire_record!(SessionUsageInfoData { conversation_tokens: "conversationTokens" if is_none, current_tokens: "currentTokens", is_initial: "isInitial" if is_none, messages_length: "messagesLength", system_tokens: "systemTokens" if is_none, token_limit: "tokenLimit", tool_definitions_tokens: "toolDefinitionsTokens" if is_none });

/// Session event "session.context_cleared". Context-cleared details emitted when the conversation is cleared (the session.history.clearContext RPC, or the runtime starting a fresh context window)
#[derive(Debug, Clone, Default)]
pub struct SessionContextClearedData {
    /// Optional initial message set after clearing
    pub initial_message: Option<String>,
    /// Number of conversation messages that were cleared
    pub messages_cleared: i64,
}
crate::record_serde::wire_record!(SessionContextClearedData { initial_message: "initialMessage" if is_none, messages_cleared: "messagesCleared" });

/// Session event "session.compaction_start". Context window breakdown at the start of LLM-powered conversation compaction
#[derive(Debug, Clone, Default)]
pub struct SessionCompactionStartData {
    /// Identifier shared with the matching compaction completion event
    pub compaction_attempt_id: Option<String>,
    /// Token count from non-system messages (user, assistant, tool) at compaction start
    pub conversation_tokens: Option<i64>,
    /// Total context tokens (system + conversation + tool definitions) at compaction start, when known
    pub current_tokens: Option<i64>,
    /// How the compaction ran relative to the conversation: "background" (alongside the turn), "turn_blocking" (the turn waited for it), "pre_prompt" (a non-interactive run waited before recording its prompt), or "direct" (an explicit compaction request). Open set; readers must tolerate other values.
    #[doc(hidden)]
    pub(crate) execution: Option<String>,
    /// Model identifier used for compaction, when known
    pub model: Option<String>,
    /// For a context_limit_retry compaction, the client's estimate of the prompt tokens in the rejected request that forced it, when known
    #[doc(hidden)]
    pub(crate) rejected_request_tokens: Option<i64>,
    /// For a context_limit_retry compaction, how the provider rejected the request that forced it: "payload_too_large" (HTTP 413) or "context_window_exceeded" (a context-limit error message). Open set; readers must tolerate other values.
    #[doc(hidden)]
    pub(crate) rejection_kind: Option<String>,
    /// Number of trailing conversation messages (the turn's not-yet-answered input) kept verbatim after the summary instead of being compacted. Omitted when every message at compaction start is summarized.
    #[doc(hidden)]
    pub(crate) retained_input_messages: Option<i64>,
    /// Destination model for a model-switch compaction when it differs from the compacting model; for a HydraFusion turn, the routed root model
    pub switch_target_model: Option<String>,
    /// Token count from system message(s) at compaction start
    pub system_tokens: Option<i64>,
    /// Destination prompt-token budget when it differs from the effective compaction token limit
    pub target_prompt_token_limit: Option<i64>,
    /// Model context window token limit the compaction is targeting, when known
    pub token_limit: Option<i64>,
    /// Token count from tool definitions at compaction start
    pub tool_definitions_tokens: Option<i64>,
    /// What initiated this compaction, when known
    pub trigger: Option<CompactionTrigger>,
}
crate::record_serde::wire_record!(SessionCompactionStartData { compaction_attempt_id: "compactionAttemptId" if is_none, conversation_tokens: "conversationTokens" if is_none, current_tokens: "currentTokens" if is_none, execution: "execution" if is_none, model: "model" if is_none, rejected_request_tokens: "rejectedRequestTokens" if is_none, rejection_kind: "rejectionKind" if is_none, retained_input_messages: "retainedInputMessages" if is_none, switch_target_model: "switchTargetModel" if is_none, system_tokens: "systemTokens" if is_none, target_prompt_token_limit: "targetPromptTokenLimit" if is_none, token_limit: "tokenLimit" if is_none, tool_definitions_tokens: "toolDefinitionsTokens" if is_none, trigger: "trigger" if is_none });

/// Session event "session.compaction_summarized". A background compaction's summarizer settled; its summary is not applied to the conversation yet. The matching session.compaction_complete follows once it is applied (or discarded) and remains authoritative.
#[derive(Debug, Clone, Default)]
pub struct SessionCompactionSummarizedData {
    /// Always true: the settled outcome still awaits being applied, which session.compaction_complete reports
    pub apply_pending: bool,
    /// Identifier shared with the matching compaction start and completion events
    pub compaction_attempt_id: String,
    /// Error message if the summarizer failed
    pub error: Option<String>,
    /// How the compaction ran relative to the conversation, as on the start event ("background"). Open set; readers must tolerate other values.
    pub execution: String,
    /// Bounded reason for a failed summarizer, using the same values as the completion event's failureReason. Unknown future values are preserved.
    pub failure_reason: Option<String>,
    /// How the summarizer settled: "succeeded", "failed", or "cancelled". Open set; readers must tolerate other values.
    pub outcome: String,
    /// Whether the summarizer produced a summary that can be applied
    pub success: bool,
}
crate::record_serde::wire_record!(SessionCompactionSummarizedData { apply_pending: "applyPending", compaction_attempt_id: "compactionAttemptId", error: "error" if is_none, execution: "execution", failure_reason: "failureReason" if is_none, outcome: "outcome", success: "success" });

/// End-to-end timing of a compaction attempt and the configuration it ran under, measured from the start of the compaction to its terminal event.
#[derive(Debug, Clone, Default)]
pub(crate) struct CompactionCompleteAttemptMetrics {
    /// Time from the decision of the compaction's outcome to this event, in milliseconds. Absent when no outcome was decided.
    #[doc(hidden)]
    pub(crate) apply_delay_ms: Option<i64>,
    /// Time a turn, or a prompt-mode run's prompt, spent blocked waiting for this compaction, in milliseconds. Absent for compactions that block nothing.
    #[doc(hidden)]
    pub(crate) blocking_wait_ms: Option<i64>,
    /// Summarizer requests cancelled while in flight, one per model call cancellation reported for this compaction. Not counted in failedRequestCount. Absent when none was cancelled.
    #[doc(hidden)]
    pub(crate) cancelled_request_count: Option<i64>,
    /// Bounded outcome of the capacity catalog refresh: "not_applicable", "not_needed", "succeeded", "failed" or "capacity_unavailable".
    #[doc(hidden)]
    pub(crate) capacity_refresh_outcome: Option<String>,
    /// How the summarizer's context capacity was established: "authoritative" or "threshold_inferred".
    #[doc(hidden)]
    pub(crate) capacity_source: Option<String>,
    /// The deadline limit that ended the compaction, in milliseconds. Absent unless a deadline fired.
    #[doc(hidden)]
    pub(crate) deadline_limit_ms: Option<i64>,
    /// How long after its limit the deadline was observed to fire, in milliseconds. Absent unless a deadline fired.
    #[doc(hidden)]
    pub(crate) deadline_overshoot_ms: Option<i64>,
    /// Deadline policy bounding the summarizer, such as "fixed" or "progress_aware"
    #[doc(hidden)]
    pub(crate) deadline_policy: Option<String>,
    /// Time a background compaction's summarizer was held for the turn's in-flight request to start responding before it was sent, in milliseconds. The compaction's deadline starts when the summarizer is sent. Absent when the summarizer was not held.
    #[doc(hidden)]
    pub(crate) dispatch_wait_ms: Option<i64>,
    /// Time from the start of the compaction to this event, in milliseconds, whatever the outcome
    #[doc(hidden)]
    pub(crate) duration_ms: i64,
    /// Summarizer requests that failed with a provider or transport error. Absent when none failed.
    #[doc(hidden)]
    pub(crate) failed_request_count: Option<i64>,
    /// Sum of the sampled input token estimates of the summarizer requests that failed, whose usage the provider never reported. Absent when none failed.
    #[doc(hidden)]
    pub(crate) failed_request_input_tokens_estimate: Option<i64>,
    /// Step whose failure ended the compaction before or after its summarizer request: "session_read", "auth_hydration", "model_resolution", "context_breakdown", "token_limit", "start_publication", "pre_compact_hook", "output_hydration", "auth_missing", "agent_context_missing", "summarizer_setup", "summary_parse", "plan_read", "history_write" or "background_apply". Absent when the summarizer request itself failed, or nothing failed.
    #[doc(hidden)]
    pub(crate) failure_operation: Option<String>,
    /// Conservative context limit inferred from current history and the automatic compaction threshold. Absent unless capacitySource is "threshold_inferred".
    #[doc(hidden)]
    pub(crate) inferred_context_limit_tokens: Option<f64>,
    /// Retry-After of the latest failed summarizer request, in milliseconds. Absent when that request sent none.
    #[doc(hidden)]
    pub(crate) last_retry_after_ms: Option<i64>,
    /// Output token limit of the latest summarizer request. Absent when no request was sent.
    #[doc(hidden)]
    pub(crate) max_output_tokens: Option<f64>,
    /// Reasoning effort the summarizer would have used before any compaction-specific reduction
    #[doc(hidden)]
    pub(crate) requested_reasoning_effort: Option<String>,
    /// Total time from a failed summarizer request settling to the next one starting, which includes the retry backoff, in milliseconds. Absent when no request was retried.
    #[doc(hidden)]
    pub(crate) retry_wait_ms: Option<i64>,
    /// Historical attachments removed from the summarizer input when it was shrunk. Absent unless the input was shrunk.
    #[doc(hidden)]
    pub(crate) shrink_removed_attachments: Option<i64>,
    /// Historical reasoning items stripped from the summarizer input when it was shrunk. Absent unless the input was shrunk.
    #[doc(hidden)]
    pub(crate) shrink_stripped_reasoning: Option<i64>,
    /// Reasoning effort the summarizer requests were sent with
    #[doc(hidden)]
    pub(crate) summarizer_reasoning_effort: Option<String>,
    /// Estimated time the machine spent suspended or asleep during the compaction, in milliseconds
    #[doc(hidden)]
    pub(crate) suspended_ms: Option<i64>,
    /// Context utilization threshold, as a fraction of the token limit, whose crossing started this compaction. Absent for compactions not started by a threshold.
    #[doc(hidden)]
    pub(crate) trigger_threshold: Option<f64>,
}
crate::record_serde::wire_record!(CompactionCompleteAttemptMetrics { apply_delay_ms: "applyDelayMs" if is_none, blocking_wait_ms: "blockingWaitMs" if is_none, cancelled_request_count: "cancelledRequestCount" if is_none, capacity_refresh_outcome: "capacityRefreshOutcome" if is_none, capacity_source: "capacitySource" if is_none, deadline_limit_ms: "deadlineLimitMs" if is_none, deadline_overshoot_ms: "deadlineOvershootMs" if is_none, deadline_policy: "deadlinePolicy" if is_none, dispatch_wait_ms: "dispatchWaitMs" if is_none, duration_ms: "durationMs", failed_request_count: "failedRequestCount" if is_none, failed_request_input_tokens_estimate: "failedRequestInputTokensEstimate" if is_none, failure_operation: "failureOperation" if is_none, inferred_context_limit_tokens: "inferredContextLimitTokens" if is_none, last_retry_after_ms: "lastRetryAfterMs" if is_none, max_output_tokens: "maxOutputTokens" if is_none, requested_reasoning_effort: "requestedReasoningEffort" if is_none, retry_wait_ms: "retryWaitMs" if is_none, shrink_removed_attachments: "shrinkRemovedAttachments" if is_none, shrink_stripped_reasoning: "shrinkStrippedReasoning" if is_none, summarizer_reasoning_effort: "summarizerReasoningEffort" if is_none, suspended_ms: "suspendedMs" if is_none, trigger_threshold: "triggerThreshold" if is_none });

/// Token usage detail for a single billing category
#[derive(Debug, Clone, Default)]
pub struct CompactionCompleteCompactionTokensUsedCopilotUsageTokenDetail {
    /// Number of tokens in this billing batch
    pub batch_size: i64,
    /// Cost per batch of tokens
    pub cost_per_batch: i64,
    /// Model responsible for this billing entry
    pub model: Option<String>,
    /// Total token count for this entry
    pub token_count: i64,
    /// Token category (e.g., "input", "output")
    pub token_type: String,
}
crate::record_serde::wire_record!(CompactionCompleteCompactionTokensUsedCopilotUsageTokenDetail { batch_size: "batchSize", cost_per_batch: "costPerBatch", model: "model" if is_none, token_count: "tokenCount", token_type: "tokenType" });

/// Per-request cost and usage data from the CAPI copilot_usage response field
#[derive(Debug, Clone, Default)]
pub(crate) struct CompactionCompleteCompactionTokensUsedCopilotUsage {
    /// Default billing model for token details that do not identify their own model
    #[doc(hidden)]
    pub(crate) model: Option<String>,
    /// Itemized token usage breakdown
    #[doc(hidden)]
    pub(crate) token_details:
        Option<Vec<CompactionCompleteCompactionTokensUsedCopilotUsageTokenDetail>>,
    /// Total cost in nano-AI units for this request
    pub total_nano_aiu: f64,
}
crate::record_serde::wire_record!(CompactionCompleteCompactionTokensUsedCopilotUsage { model: "model" if is_none, token_details: "tokenDetails" if is_none, total_nano_aiu: "totalNanoAiu" });

/// Token usage breakdown for the compaction LLM call (aligned with assistant.usage format)
#[derive(Debug, Clone, Default)]
pub struct CompactionCompleteCompactionTokensUsed {
    /// Whether the compaction's credit amount was reported for all, some, or none of its calls.
    pub ai_credits_status: Option<AiCreditsStatus>,
    /// Cached input tokens reused in the compaction LLM call
    pub cache_read_tokens: Option<i64>,
    /// Tokens written to prompt cache in the compaction LLM call
    pub cache_write_tokens: Option<i64>,
    /// Per-request cost and usage data from the CAPI copilot_usage response field
    #[doc(hidden)]
    pub(crate) copilot_usage: Option<CompactionCompleteCompactionTokensUsedCopilotUsage>,
    /// Duration of the compaction LLM call in milliseconds
    pub duration: Option<i64>,
    /// Input tokens consumed by the compaction LLM call
    pub input_tokens: Option<i64>,
    /// Model identifier used for the compaction LLM call
    pub model: Option<String>,
    /// Model display name captured for the compaction call.
    pub model_display_name: Option<String>,
    /// Output tokens produced by the compaction LLM call
    pub output_tokens: Option<i64>,
    /// Provider captured for the compaction call; absent when attribution is unknown.
    pub provider: Option<ModelProviderRef>,
    /// Number of model requests represented by this aggregated compaction usage
    pub request_count: Option<i64>,
}
crate::record_serde::wire_record!(CompactionCompleteCompactionTokensUsed { ai_credits_status: "aiCreditsStatus" if is_none, cache_read_tokens: "cacheReadTokens" if is_none, cache_write_tokens: "cacheWriteTokens" if is_none, copilot_usage: "copilotUsage" if is_none, duration: "duration" if is_none, input_tokens: "inputTokens" if is_none, model: "model" if is_none, model_display_name: "modelDisplayName" if is_none, output_tokens: "outputTokens" if is_none, provider: "provider" if is_none, request_count: "requestCount" if is_none });

/// Evidence about the summarizer's latest model call when a compaction failed. Every value is a string so that new values stay readable by older runtimes.
#[derive(Debug, Clone, Default)]
pub(crate) struct CompactionCompleteFailureDiagnostics {
    /// Finish reason of the summarizer's latest successful response, such as stop, length, or content_filter
    #[doc(hidden)]
    pub(crate) finish_reason: Option<String>,
    /// Failure reason of the summarizer's latest failed model call, kept when a later attempt or the compaction deadline ended the compaction. Uses the failureReason values.
    #[doc(hidden)]
    pub(crate) last_attempt_failure_reason: Option<String>,
    /// Error code the provider returned for that call, or its error type when it gave no code
    #[doc(hidden)]
    pub(crate) provider_error_code: Option<String>,
    /// Bounded kind of that call's transport failure, such as dns, tls, proxy, connection_refused, connection_reset, connect_timeout, or timeout
    #[doc(hidden)]
    pub(crate) transport_error_kind: Option<String>,
}
crate::record_serde::wire_record!(CompactionCompleteFailureDiagnostics { finish_reason: "finishReason" if is_none, last_attempt_failure_reason: "lastAttemptFailureReason" if is_none, provider_error_code: "providerErrorCode" if is_none, transport_error_kind: "transportErrorKind" if is_none });

/// How far the compaction's summarizer model calls had progressed when the compaction's outcome was decided. Stream statistics describe the latest model request; durationMs and attemptCount cover every request.
#[derive(Debug, Clone, Default)]
pub(crate) struct CompactionCompleteSummarizerProgress {
    /// Characters of reply text the latest request streamed, counted as received, so it measures output even when the request was cut off before reporting usage. Tool-call arguments and reasoning are not counted. Absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) answer_chars: Option<i64>,
    /// Time since the latest request was dispatched, whether or not it had finished, in milliseconds. Absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) attempt_age_ms: Option<i64>,
    /// Number of summarizer model requests dispatched, including retries
    #[doc(hidden)]
    pub(crate) attempt_count: i64,
    /// Whether the latest request had already returned a response or an error. False when it was still in flight, such as when the compaction deadline cut it off; absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) attempt_settled: Option<bool>,
    /// Time from the start of the compaction to the decision of its outcome, in milliseconds
    #[doc(hidden)]
    pub(crate) duration_ms: i64,
    /// Time since the latest request's last stream event, or since its dispatch if none arrived, in milliseconds. Absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) idle_ms: Option<i64>,
    /// Whether historical attachments and reasoning were dropped from the compaction's input because the session's previous compaction ran out of time.
    #[doc(hidden)]
    pub(crate) input_shrunk: Option<bool>,
    /// Sampled estimate of the input tokens the latest request sent, taken at dispatch, so it is known even when the request was cut off before reporting usage. Absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) input_tokens_estimate: Option<i64>,
    /// Longest gap between two consecutive outputs of the latest request after its first reply text or tool-call output, in milliseconds. Excludes the still-open gap reported as sinceLastOutputMs; absent without such a gap.
    #[doc(hidden)]
    pub(crate) max_output_gap_after_answer_ms: Option<i64>,
    /// Longest gap between two consecutive outputs of the latest request up to its first reply text or tool-call output, including the gap that ends there, in milliseconds. Excludes the still-open gap reported as sinceLastOutputMs; absent without such a gap.
    #[doc(hidden)]
    pub(crate) max_output_gap_before_answer_ms: Option<i64>,
    /// Longest gap between two consecutive stream events of the latest request, in milliseconds. Excludes the still-open gap reported as idleMs; absent with fewer than two events.
    #[doc(hidden)]
    pub(crate) max_stream_event_gap_ms: Option<i64>,
    /// Stream events of the latest request that carried observable output
    #[doc(hidden)]
    pub(crate) output_event_count: i64,
    /// Characters of reasoning text, summarized or raw, the latest request streamed, counted as received. Encrypted reasoning is not counted. Absent when no request was dispatched.
    #[doc(hidden)]
    pub(crate) reasoning_chars: Option<i64>,
    /// Time since the latest request's last observable output, in milliseconds. Unlike idleMs, keepalive deltas do not reset it. Absent when no output arrived.
    #[doc(hidden)]
    pub(crate) since_last_output_ms: Option<i64>,
    /// Token-bearing stream events the latest request received, keepalive deltas included
    #[doc(hidden)]
    pub(crate) stream_event_count: i64,
    /// Time from dispatching the latest request to its first reply text or tool-call output, in milliseconds. Reasoning does not count. Absent when no such output arrived.
    #[doc(hidden)]
    pub(crate) time_to_first_answer_output_ms: Option<i64>,
    /// Time from dispatching the latest request to its first observable text, reasoning, or tool-call output, in milliseconds. Absent when no output arrived.
    #[doc(hidden)]
    pub(crate) time_to_first_output_ms: Option<i64>,
    /// Time from dispatching the latest request to its first token-bearing stream event, in milliseconds. Absent when no stream event arrived.
    #[doc(hidden)]
    pub(crate) time_to_first_token_ms: Option<i64>,
}
crate::record_serde::wire_record!(CompactionCompleteSummarizerProgress { answer_chars: "answerChars" if is_none, attempt_age_ms: "attemptAgeMs" if is_none, attempt_count: "attemptCount", attempt_settled: "attemptSettled" if is_none, duration_ms: "durationMs", idle_ms: "idleMs" if is_none, input_shrunk: "inputShrunk" if is_none, input_tokens_estimate: "inputTokensEstimate" if is_none, max_output_gap_after_answer_ms: "maxOutputGapAfterAnswerMs" if is_none, max_output_gap_before_answer_ms: "maxOutputGapBeforeAnswerMs" if is_none, max_stream_event_gap_ms: "maxStreamEventGapMs" if is_none, output_event_count: "outputEventCount", reasoning_chars: "reasoningChars" if is_none, since_last_output_ms: "sinceLastOutputMs" if is_none, stream_event_count: "streamEventCount", time_to_first_answer_output_ms: "timeToFirstAnswerOutputMs" if is_none, time_to_first_output_ms: "timeToFirstOutputMs" if is_none, time_to_first_token_ms: "timeToFirstTokenMs" if is_none });

/// Session event "session.compaction_complete". Conversation compaction results including success status, metrics, and optional error details
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionCompactionCompleteData {
    /// Authoritative active-workflow reminder appended to the compacted context
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) active_workflow_summary: Option<String>,
    /// How long the compaction attempt took end to end, how long it blocked the user, and the configuration it ran under
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) attempt_metrics: Option<Box<CompactionCompleteAttemptMetrics>>,
    /// Canonical model identifier used for model-specific behavior when replaying compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub behavior_model_id: Option<String>,
    /// Checkpoint snapshot number created for recovery
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checkpoint_number: Option<i64>,
    /// File path where the checkpoint was stored
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checkpoint_path: Option<String>,
    /// Client-generated request ID (X-Request-Id header) the runtime sent with the latest compaction LLM request. Present even when the request failed before any response arrived.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) client_request_id: Option<String>,
    /// Identifier shared with the matching compaction start event
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compaction_attempt_id: Option<String>,
    /// Token usage breakdown for the compaction LLM call (aligned with assistant.usage format)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compaction_tokens_used: Option<CompactionCompleteCompactionTokensUsed>,
    /// Token count from non-system messages (user, assistant, tool) after compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conversation_tokens: Option<i64>,
    /// User-supplied focus instructions provided to a manual `/compact` invocation. Omitted for automatic compaction and for manual compaction with no focus text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_instructions: Option<String>,
    /// Error message if compaction failed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// How the compaction ran relative to the conversation: "background" (alongside the turn), "turn_blocking" (the turn waited for it), "pre_prompt" (a non-interactive run waited before recording its prompt), or "direct" (an explicit compaction request). Open set; readers must tolerate other values.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) execution: Option<String>,
    /// Evidence about the summarizer's latest model call for a failed compaction, so a failure its reason alone does not explain can be diagnosed
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) failure_diagnostics: Option<CompactionCompleteFailureDiagnostics>,
    /// Bounded reason for a typed failed outcome. Unknown future values are preserved.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub failure_reason: Option<String>,
    /// Model identifier at admission when routing later changed to the reported compaction model
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initial_model: Option<String>,
    /// Number of messages removed during compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub messages_removed: Option<i64>,
    /// Model identifier used for compaction when provider usage does not already supply it
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    /// Typed terminal outcome for this observed compaction completion. Unknown future values are preserved.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outcome: Option<String>,
    /// Bounded reason for an expected non-application outcome such as cancellation or no applicable history. Unknown future values are preserved.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outcome_reason: Option<String>,
    /// Total tokens in conversation after compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub post_compaction_tokens: Option<i64>,
    /// Number of messages before compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pre_compaction_messages_length: Option<i64>,
    /// Total tokens in conversation before compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pre_compaction_tokens: Option<i64>,
    /// Reasoning effort level the compaction summarizer request used, if applicable (e.g. "none", "low", "medium", "high", "xhigh", "max")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
    /// GitHub request tracing ID (x-github-request-id header) for the compaction LLM call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_id: Option<RequestId>,
    /// Reasoning baseline on the replacement summary, preserved when replay skips the compacted history
    #[serde(skip_serializing_if = "Option::is_none")]
    pub responses_reasoning: Option<ResponsesReasoning>,
    /// Copilot service request ID (x-copilot-service-request-id header) for the compaction LLM call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service_request_id: Option<String>,
    /// For failed compaction only: the HTTP status code of the compaction LLM call failure, when it carried one. Absent for successful compaction and for failures without an HTTP status (e.g. an empty model response or a transport error).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_code: Option<i64>,
    /// Whether compaction completed successfully
    pub success: bool,
    /// How far the summarizer model calls had progressed when the outcome was decided, so a stalled summarizer can be told from a slow one
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) summarizer_progress: Option<CompactionCompleteSummarizerProgress>,
    /// LLM-generated summary of the compacted conversation history
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary_content: Option<String>,
    /// Destination model for a model-switch compaction when it differs from the compacting model; for a HydraFusion turn, the routed root model
    #[serde(skip_serializing_if = "Option::is_none")]
    pub switch_target_model: Option<String>,
    /// Token count from system message(s) after compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_tokens: Option<i64>,
    /// Destination prompt-token budget when it differs from the effective compaction token limit
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_prompt_token_limit: Option<i64>,
    /// Open accounting-basis identifier for trustworthy paired pre/post token measurements. The current runtime emits local_full_context_estimate.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token_accounting_basis: Option<String>,
    /// Model context window token limit the compaction was targeting, when known
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token_limit: Option<i64>,
    /// Number of tokens removed during compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tokens_removed: Option<i64>,
    /// Token count from tool definitions after compaction
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_definitions_tokens: Option<i64>,
    /// What initiated this compaction, when known
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trigger: Option<CompactionTrigger>,
}

/// Privacy-safe reference to a permission-protected action that remains blocked for the current Autopilot epoch
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct BlockedPermissionAction {
    /// Permission-recovery episode that owns this blocked action
    pub episode_id: String,
    /// Controlled permission request kind for the blocked action
    pub permission_kind: String,
    /// Recovery transition that made the action binding for the epoch
    pub recovery_reason: PermissionRecoveryReason,
    /// Tool call that produced the final blocked permission request. Consumers can join this identifier to labeled permission and tool events for user-facing details.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(BlockedPermissionAction { episode_id: "episodeId", permission_kind: "permissionKind", recovery_reason: "recoveryReason", tool_call_id: "toolCallId" if is_none });

#[derive(Debug, Clone, Default)]
pub struct PermissionRecoveryAttempt {
    /// Unique identifier for this attempt record
    pub attempt_id: String,
    /// How the runtime handled this attempt
    pub disposition: PermissionRecoveryAttemptDisposition,
    /// One-based position of this attempt in the episode
    pub ordinal: i64,
    /// Controlled permission request kind, such as shell, path, URL, or tool
    pub permission_kind: String,
    /// Controlled reason for the attempt disposition
    pub reason: PermissionRecoveryAttemptReason,
    /// Relationship between this attempt and earlier attempts in the episode
    pub relation: PermissionRecoveryAttemptRelation,
    /// SHA-256 fingerprint of normalized request data; raw permission arguments are not included
    pub request_fingerprint: String,
    /// Whether this deferral happened because the automatic permission review ended without a verdict (timeout, empty or unparseable reply). Such a deferral assesses nothing: it does not make a later identical request a repeated attempt or count toward the attempt limit. Older runtimes ignore it.
    pub review_incomplete: Option<bool>,
    /// Tool-call identifier associated with this attempt, when available
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRecoveryAttempt { attempt_id: "attemptId", disposition: "disposition", ordinal: "ordinal", permission_kind: "permissionKind", reason: "reason", relation: "relation", request_fingerprint: "requestFingerprint", review_incomplete: "reviewIncomplete" if is_none, tool_call_id: "toolCallId" if is_none });

/// Replay-safe epoch counters for Autopilot Assisted Permissions recovery
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionRecoveryRunState {
    /// Number of blocked recovery episodes since the latest successful permitted effectful tool call
    pub consecutive_blocked_episodes: i64,
    /// Whether the claimed epoch terminal is an authoritative permission-recovery block that remains latched until the next genuine root user message
    pub permission_recovery_terminal_claimed: Option<bool>,
    /// Whether a permission-recovery terminal completion has already been claimed for the epoch
    pub terminal_outcome_claimed: bool,
    /// Total deferred, denied, or blocked attempt transitions in the current genuine-user-message epoch
    pub total_denial_dispositions: i64,
}
crate::record_serde::wire_record!(PermissionRecoveryRunState { consecutive_blocked_episodes: "consecutiveBlockedEpisodes", permission_recovery_terminal_claimed: "permissionRecoveryTerminalClaimed" if is_none, terminal_outcome_claimed: "terminalOutcomeClaimed", total_denial_dispositions: "totalDenialDispositions" });

/// Authoritative snapshot of an Autopilot permission-recovery episode
#[derive(Debug, Clone, Default)]
pub struct PermissionRecoveryData {
    /// Ordered privacy-safe record of permission attempts and the successful alternative, when any
    pub attempts: Vec<PermissionRecoveryAttempt>,
    /// Stable identifier shared by every transition in this recovery episode
    pub episode_id: String,
    /// Maximum number of distinct autonomous permission attempts allowed before escalation
    pub max_attempts: i64,
    /// Policy selected from the current client's response capability; mode or client changes may update it during recovery
    pub on_blocked: PermissionRecoveryOnBlocked,
    /// Subagent identifier that owns this recovery episode when the request was delegated through a parent permission service
    pub owner_agent_id: Option<String>,
    /// Root or child execution-session identifier that owns this recovery episode; omitted for legacy snapshots
    pub owner_session_id: Option<String>,
    /// Controlled reason for the latest episode transition
    pub reason: PermissionRecoveryReason,
    /// Optional replay checkpoint for the epoch-scoped permission-recovery breaker. New runtimes persist it on state-changing recovery snapshots; older runtimes ignore it.
    pub run_state: Option<PermissionRecoveryRunState>,
    /// Current lifecycle state of the recovery episode
    pub status: PermissionRecoveryStatus,
}
crate::record_serde::wire_record!(PermissionRecoveryData { attempts: "attempts", episode_id: "episodeId", max_attempts: "maxAttempts", on_blocked: "onBlocked", owner_agent_id: "ownerAgentId" if is_none, owner_session_id: "ownerSessionId" if is_none, reason: "reason", run_state: "runState" if is_none, status: "status" });

/// Structured reason that the task cannot continue without intervention
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct TaskBlocker {
    /// Category of intervention that blocked the task
    pub kind: TaskBlockerKind,
    /// Permission-recovery episode that produced this blocker
    pub permission_recovery: PermissionRecoveryData,
    /// Controlled reason for the current blocked state
    pub reason: TaskBlockerReason,
    /// Whether a later user response or steering message can resume the task
    pub resumable: bool,
}
crate::record_serde::wire_record!(TaskBlocker {
    kind: "kind",
    permission_recovery: "permissionRecovery",
    reason: "reason",
    resumable: "resumable"
});

/// Session event "session.task_complete". Task completion notification with summary from the agent
#[derive(Debug, Clone, Default)]
pub struct SessionTaskCompleteData {
    /// Permission-protected actions that remained blocked for this genuine-user-message epoch. Present on successful completion when the objective was completed without them and on blocked completion when one or more were required.
    pub blocked_actions: Option<Vec<BlockedPermissionAction>>,
    /// Structured blocker details when outcome is blocked
    pub blocker: Option<TaskBlocker>,
    /// Active autopilot objective ID evaluated by the completion reviewer
    pub objective_id: Option<i64>,
    /// Semantic completion decision. Absent on legacy events and invalid tool calls
    pub outcome: Option<TaskCompletionOutcome>,
    /// Label-safe runtime rationale for the completion decision (e.g. a cancellation or pause/resume downgrade), when one applies. Reviewer-authored rationale is intentionally omitted here because this event has no IFC label channel; the reviewer's findings remain available through its own labeled sub-agent events
    pub reason: Option<String>,
    /// Whether the task was accepted as complete. False when validation failed or completion was rejected or blocked by the reviewer
    pub success: Option<bool>,
    /// Summary of the completed task, provided by the agent
    pub summary: Option<String>,
}
crate::record_serde::wire_record!(SessionTaskCompleteData { blocked_actions: "blockedActions" if is_none, blocker: "blocker" if is_none, objective_id: "objectiveId" if is_none, outcome: "outcome" if is_none, reason: "reason" if is_none, success: "success" if is_none, summary: "summary" if is_none });

/// Inclusive durable event range summarized by a completion receipt.
#[derive(Debug, Clone, Default)]
pub struct CompletionReceiptEventRange {
    /// Identifier of the assistant turn-end event that ends the covered exchange. Always equals the receipt's sourceEventId, so either field is a valid join key.
    pub end_event_id: String,
    /// Identifier of the user message that starts the covered exchange.
    pub start_event_id: String,
}
crate::record_serde::wire_record!(CompletionReceiptEventRange {
    end_event_id: "endEventId",
    start_event_id: "startEventId"
});

/// Final structured tool completion in the covered event range.
#[derive(Debug, Clone, Default)]
pub struct CompletionReceiptFinalTool {
    /// Process exit code from a structured shell result, when available.
    pub exit_code: Option<i64>,
    /// Structured success or failure status from the tool completion event.
    pub status: CompletionReceiptToolStatus,
    /// Unique identifier of the completed tool call.
    pub tool_call_id: String,
    /// Tool name from the matching tool execution start event, when available.
    pub tool_name: Option<String>,
}
crate::record_serde::wire_record!(CompletionReceiptFinalTool { exit_code: "exitCode" if is_none, status: "status", tool_call_id: "toolCallId", tool_name: "toolName" if is_none });

/// Session event "session.completion_receipt". Behavior-neutral record of structured runtime facts present when an agent completion decision is accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCompletionReceiptData {
    /// One-based accepted completion receipt ordinal in the durable session history.
    pub attempt: i64,
    /// Inclusive durable event range summarized by this receipt.
    pub event_range: CompletionReceiptEventRange,
    /// Number of failed structured tool completions in the covered range.
    pub failed_tool_count: i64,
    /// Final structured tool completion in the covered range, when one exists.
    pub final_tool: Option<CompletionReceiptFinalTool>,
    /// Version of the completion receipt payload.
    pub schema_version: i64,
    /// Identifier of the assistant turn-end event that supplied the accepted completion boundary. This is the receipt's idempotency key, and always equals eventRange.endEventId.
    pub source_event_id: String,
    /// Runtime reason the completion decision was accepted.
    pub stop_reason: CompletionReceiptStopReason,
    /// Number of successful structured tool completions in the covered range.
    pub successful_tool_count: i64,
}
crate::record_serde::wire_record!(SessionCompletionReceiptData { attempt: "attempt", event_range: "eventRange", failed_tool_count: "failedToolCount", final_tool: "finalTool" if is_none, schema_version: "schemaVersion", source_event_id: "sourceEventId", stop_reason: "stopReason", successful_tool_count: "successfulToolCount" });

/// Session event "session.fusion_route_started". Experimental transient signal that HydraFusion routing has started for an eligible turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionFusionRouteStartedData {
    /// Identifier for this routing attempt before a durable Fusion turn exists.
    pub attempt_id: String,
    /// HydraFusion routing policy requested for the turn.
    pub policy: Option<String>,
    /// Synthetic HydraFusion model selected for the session.
    pub synthetic_model: Option<String>,
    /// Kind of turn being routed.
    pub turn_kind: FusionTurnKind,
}
crate::record_serde::wire_record!(SessionFusionRouteStartedData { attempt_id: "attemptId", policy: "policy" if is_none, synthetic_model: "syntheticModel" if is_none, turn_kind: "turnKind" });

/// Session event "session.fusion_route_failed". Experimental durable HydraFusion routing failure and the deterministic concrete fallback selected for the turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionFusionRouteFailedData {
    /// Identifier of the routing attempt that failed.
    pub attempt_id: String,
    /// Provider or validation error detail, when available.
    pub error_message: Option<String>,
    /// Concrete model selected as the deterministic fallback.
    pub fallback_model: String,
    /// Identifier of the HydraFusion turn whose routing failed; its fallback phase runs under the same id. Absent on events written before it was recorded.
    pub fusion_id: Option<String>,
    /// HydraFusion routing policy requested for the turn.
    pub policy: String,
    /// Stable machine-readable reason for the routing failure.
    pub reason: String,
    /// Elapsed routing time in milliseconds before the failure.
    pub routing_latency_ms: Option<f64>,
    /// Synthetic HydraFusion model selected for the session.
    pub synthetic_model: String,
}
crate::record_serde::wire_record!(SessionFusionRouteFailedData { attempt_id: "attemptId", error_message: "errorMessage" if is_none, fallback_model: "fallbackModel", fusion_id: "fusionId" if is_none, policy: "policy", reason: "reason", routing_latency_ms: "routingLatencyMs" if is_none, synthetic_model: "syntheticModel" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionCritic {
    /// Concrete model selected for this critic.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub model: String,
    /// Unique execution phase identifier for this critic.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub phase_id: String,
    /// Explicit reasoning effort selected for this critic, if supplied.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub reasoning_effort: Option<String>,
}
crate::record_serde::wire_record!(FusionCritic { model: "model", phase_id: "phaseId", reasoning_effort: "reasoningEffort" if is_none });

/// Durable server recommendation for subsequent HydraFusion turns.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionFollowUpRecommendation {
    /// Recommended routing action for the next compaction turn.
    pub compaction_turn: FusionFollowUpAction,
    /// Recommended routing action for the next user-message turn.
    pub user_turn: FusionFollowUpAction,
}
crate::record_serde::wire_record!(FusionFollowUpRecommendation {
    compaction_turn: "compactionTurn",
    user_turn: "userTurn"
});

/// Presentation-neutral phase planned for a HydraFusion turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionPhasePlanStep {
    /// Whether the phase executes only when an earlier phase requests it.
    pub conditional: bool,
    /// Kind of phase that may execute.
    pub kind: FusionPhaseKind,
    /// Semantic role assigned to the phase.
    pub role: String,
    /// Conversation scope in which the phase executes.
    pub scope: FusionConversationScope,
}
crate::record_serde::wire_record!(FusionPhasePlanStep {
    conditional: "conditional",
    kind: "kind",
    role: "role",
    scope: "scope"
});

/// Validated HydraFusion routing capability scores.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionScores {
    /// Code-generation capability score returned by the authenticated router.
    pub code_gen: f64,
    /// Debugging capability score returned by the authenticated router.
    pub debugging: f64,
    /// Reasoning capability score returned by the authenticated router.
    pub reasoning: f64,
    /// Tool-use capability score returned by the authenticated router.
    pub tool_use: f64,
}
crate::record_serde::wire_record!(FusionScores {
    code_gen: "codeGen",
    debugging: "debugging",
    reasoning: "reasoning",
    tool_use: "toolUse"
});

/// Session event "session.fusion_resolved". Experimental durable validated HydraFusion route and turn policy. An empty phase plan with pattern single runs an ordinary streaming turn, without phase or commit events.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionFusionResolvedData {
    /// Version of the validated HydraFusion event contract.
    pub contract_version: i64,
    /// Planned Critique phase identities, including independent critics with repeated model IDs. May be absent in older events; consumers then use secondaryModel for the legacy critic.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub critics: Option<Vec<FusionCritic>>,
    /// Concrete model used when the planned primary model cannot execute.
    pub fallback_model: String,
    /// Router recommendation controlling reuse or rerouting on later turns.
    pub follow_up: Option<FusionFollowUpRecommendation>,
    /// Concrete model recommended for eligible follow-up turns.
    pub follow_up_model: String,
    /// Stable identifier for the resolved HydraFusion turn.
    pub fusion_id: String,
    /// Short human-readable summary of the selected workflow, suitable for immediate client display after routing. May be absent in older durable events; omit the explanation or derive one from pattern and phasePlan. Display text, not a stable machine-readable value.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub hint: Option<String>,
    /// Concrete model selected for Cascade escalation-gate calls, when required.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub judge_model: Option<String>,
    /// Version of the executable model universe used for selection.
    pub model_universe_version: Option<String>,
    /// Validated orchestration pattern selected for the turn.
    pub pattern: FusionPattern,
    /// Presentation-neutral phase plan for workflow progress. An empty plan with pattern single runs an ordinary streaming turn, without phase or commit events.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub phase_plan: Option<Vec<FusionPhasePlanStep>>,
    /// Version of the validated execution-plan format.
    pub plan_version: Option<String>,
    /// HydraFusion routing policy used to resolve the plan.
    pub policy: String,
    /// Version of the local routing policy.
    pub policy_version: Option<String>,
    /// Concrete model selected for the primary solver phase.
    pub primary_model: String,
    /// Concrete model selected for Cascade repair, when required.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub repair_model: Option<String>,
    /// Router implementation that supplied the plan.
    pub route_source: Option<String>,
    /// Elapsed time in milliseconds required to resolve and validate the route.
    pub routing_latency_ms: Option<f64>,
    /// Identifier of the local policy rule that matched.
    pub rule_id: Option<String>,
    /// Zero-based index of the local policy rule that matched.
    pub rule_index: Option<i64>,
    /// Human-readable name of the local policy rule that matched.
    pub rule_name: Option<String>,
    /// Validated capability scores used to select the route.
    pub scores: Option<FusionScores>,
    /// Concrete model selected for Critique review, or the legacy Cascade judge/repair model when role-specific fields are absent.
    pub secondary_model: Option<String>,
    /// Identifier of the server policy rule whose workflow the plan's steps describe, when CAPI's sticky root routing replaced the workflow of `ruleId`'s rule to keep the previous turn's root model. Absent when no substitution occurred.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub selected_rule_id: Option<String>,
    /// Synthetic HydraFusion model selected for the session.
    pub synthetic_model: String,
    /// Identifier of the session turn associated with the route.
    pub turn_id: String,
}
crate::record_serde::wire_record!(SessionFusionResolvedData { contract_version: "contractVersion", critics: "critics" if is_none, fallback_model: "fallbackModel", follow_up: "followUp" if is_none, follow_up_model: "followUpModel", fusion_id: "fusionId", hint: "hint" if is_none, judge_model: "judgeModel" if is_none, model_universe_version: "modelUniverseVersion" if is_none, pattern: "pattern", phase_plan: "phasePlan" if is_none, plan_version: "planVersion" if is_none, policy: "policy", policy_version: "policyVersion" if is_none, primary_model: "primaryModel", repair_model: "repairModel" if is_none, route_source: "routeSource" if is_none, routing_latency_ms: "routingLatencyMs" if is_none, rule_id: "ruleId" if is_none, rule_index: "ruleIndex" if is_none, rule_name: "ruleName" if is_none, scores: "scores" if is_none, secondary_model: "secondaryModel", selected_rule_id: "selectedRuleId" if is_none, synthetic_model: "syntheticModel", turn_id: "turnId" });

/// Session event "session.fusion_completed". Experimental durable aggregate outcome of a HydraFusion turn. Phase-free Single reports completed, failed, or cancelled; staged patterns emit this event only on successful or degraded completion.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionFusionCompletedData {
    /// True when the committed first pass ended with background work outstanding, so that work's completion notifications continue the conversation; absent otherwise.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub awaiting_background_work: Option<bool>,
    /// Total cached input tokens reported by the turn.
    pub cached_tokens: i64,
    /// Total tokens written to prompt cache by the turn.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_write_tokens: Option<i64>,
    /// Idempotency identifier for completion; a phase-free Single has no commit replay.
    pub commit_id: String,
    /// Reason the turn used a degraded route, when applicable.
    pub degraded_reason: Option<String>,
    /// Total elapsed execution time for the HydraFusion turn in milliseconds.
    pub duration_ms: f64,
    /// Error reported by a failed ordinary Single turn, when available.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_message: Option<String>,
    /// Failure category of an ordinary Single turn, which has no phase failure record.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub failure_reason: Option<String>,
    /// Concrete model that supplied the authoritative final content.
    pub final_source_model: Option<String>,
    /// Phase whose output supplied the final content, or null for a phase-free Single.
    pub final_source_phase_id: Option<String>,
    /// Concrete model recommended for eligible follow-up turns.
    pub follow_up_model: String,
    /// Stable identifier for the completed HydraFusion turn.
    pub fusion_id: String,
    /// Total input tokens consumed by the turn.
    pub input_tokens: i64,
    /// Stable aggregate outcome. Phase-free Single reports completed, failed, or cancelled; staged patterns emit this event only on successful or degraded completion.
    pub outcome: String,
    /// Total output tokens produced by the turn.
    pub output_tokens: i64,
    /// HydraFusion orchestration pattern executed for the turn.
    pub pattern: FusionPattern,
    /// Number of concrete phases attempted by the turn.
    pub phase_count: i64,
    /// Planned reasoning effort of an ordinary Single turn, which has no phase record.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
    /// Total concrete model requests made by the turn.
    pub request_count: i64,
    /// Synthetic HydraFusion model selected for the session.
    pub synthetic_model: String,
    /// Total normalized AI-unit cost reported by the turn, in nano-AIU.
    pub total_nano_aiu: f64,
    /// Identifier of the session turn associated with the completion.
    pub turn_id: String,
}

/// Session event "session.permission_recovery". Authoritative snapshot of an Autopilot permission-recovery episode
#[derive(Debug, Clone, Default)]
pub struct SessionPermissionRecoveryData {
    /// Ordered privacy-safe record of permission attempts and the successful alternative, when any
    pub attempts: Vec<PermissionRecoveryAttempt>,
    /// Stable identifier shared by every transition in this recovery episode
    pub episode_id: String,
    /// Maximum number of distinct autonomous permission attempts allowed before escalation
    pub max_attempts: i64,
    /// Policy selected from the current client's response capability; mode or client changes may update it during recovery
    pub on_blocked: PermissionRecoveryOnBlocked,
    /// Subagent identifier that owns this recovery episode when the request was delegated through a parent permission service
    pub owner_agent_id: Option<String>,
    /// Root or child execution-session identifier that owns this recovery episode; omitted for legacy snapshots
    pub owner_session_id: Option<String>,
    /// Controlled reason for the latest episode transition
    pub reason: PermissionRecoveryReason,
    /// Optional replay checkpoint for the epoch-scoped permission-recovery breaker. New runtimes persist it on state-changing recovery snapshots; older runtimes ignore it.
    pub run_state: Option<PermissionRecoveryRunState>,
    /// Current lifecycle state of the recovery episode
    pub status: PermissionRecoveryStatus,
}
crate::record_serde::wire_record!(SessionPermissionRecoveryData { attempts: "attempts", episode_id: "episodeId", max_attempts: "maxAttempts", on_blocked: "onBlocked", owner_agent_id: "ownerAgentId" if is_none, owner_session_id: "ownerSessionId" if is_none, reason: "reason", run_state: "runState" if is_none, status: "status" });

/// Session event "user.message". Payload of `user.message` with displayed and model-transformed content, attachments, source/delivery metadata, mode, and telemetry IDs.
#[derive(Debug, Clone, Default)]
pub struct UserMessageData {
    /// True when the host marked this message as written by an agent and delivered in the user role, such as the kickoff of a session another agent created. Only automatic permission decisions read it; absent in other messages and in older events.
    #[doc(hidden)]
    pub agent_authored: Option<bool>,
    /// The agent mode that was active when this message was sent
    pub agent_mode: Option<UserMessageAgentMode>,
    /// Files, selections, or GitHub references attached to the message
    pub attachments: Option<Vec<serde_json::Value>>,
    /// IDs of accepted assistant messages that caused this delivered inter-agent prompt, referring to assistant.message.data.messageId across the agent tree. Captured when sending and retained through queued or immediate delivery. An empty list means no model-produced dependency; absence means provenance is unavailable.
    pub causal_dependencies: Option<Vec<String>>,
    /// Opaque clientTurnId the caller supplied on the send that delivered this message, echoed when the message is consumed, whether it started a turn, drained from the queue or was consumed as steering. Absent when that send supplied none.
    pub client_turn_id: Option<String>,
    /// The user's message text as displayed in the timeline
    pub content: String,
    /// How this message was delivered to the agentic loop relative to loop state (idle-start vs. steering/queued while busy). The timing axis; combine with `source` (origin) for the full picture. Used for telemetry attribution.
    pub delivery: Option<UserMessageDelivery>,
    /// Whether the submitted prompt or attachments contain a task, before adding generated context such as timestamps; independent of timeline display text. Absent in older events.
    pub has_submitted_task: Option<bool>,
    /// CAPI interaction ID for correlating this user message with its turn
    pub interaction_id: Option<String>,
    /// True when this user message was auto-injected by autopilot's continuation loop rather than typed by the user; used to distinguish autopilot-driven turns in telemetry.
    pub is_autopilot_continuation: Option<bool>,
    /// Stable identity of the logical user message, matching the ID returned by send and retained by pending queue snapshots
    pub message_id: Option<String>,
    /// Path-backed native document attachments that stayed on the tagged_files path flow because native upload could not read them or would exceed the request size limit
    pub native_document_path_fallback_paths: Option<Vec<String>>,
    /// Parent agent task ID for background telemetry correlated to this user turn
    pub parent_agent_task_id: Option<String>,
    /// Provider reasoning settings anchored before this model-facing message for cache-stable replay; the historical responsesReasoning name is retained for compatibility
    pub responses_reasoning: Option<ResponsesReasoning>,
    /// Origin of this message, used for timeline filtering and attribution (e.g., `skill-pdf` for hidden skill injection or `agent-<agent-id>` for an inter-agent prompt)
    pub source: Option<String>,
    /// Normalized document MIME types that were sent natively instead of through tagged_files XML
    pub supported_native_document_mime_types: Option<Vec<String>>,
    /// Transformed version of the message sent to the model, with XML wrapping, timestamps, and other augmentations for prompt caching
    pub transformed_content: Option<String>,
    /// The agent-loop turn ID that consumed this message; absent when no agent-loop turn consumed it
    pub turn_id: Option<String>,
}
crate::record_serde::wire_record!(UserMessageData { agent_authored: "agentAuthored" if is_none, agent_mode: "agentMode" if is_none, attachments: "attachments" if is_none, causal_dependencies: "causalDependencies" if is_none, client_turn_id: "clientTurnId" if is_none, content: "content", delivery: "delivery" if is_none, has_submitted_task: "hasSubmittedTask" if is_none, interaction_id: "interactionId" if is_none, is_autopilot_continuation: "isAutopilotContinuation" if is_none, message_id: "messageId" if is_none, native_document_path_fallback_paths: "nativeDocumentPathFallbackPaths" if is_none, parent_agent_task_id: "parentAgentTaskId" if is_none, responses_reasoning: "responsesReasoning" if is_none, source: "source" if is_none, supported_native_document_mime_types: "supportedNativeDocumentMimeTypes" if is_none, transformed_content: "transformedContent" if is_none, turn_id: "turnId" if is_none });

/// Session event "pending_messages.modified". Empty payload; the event signals that the pending message queue has changed
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PendingMessagesModifiedData {}

/// Session event "assistant.turn_start". Turn initialization metadata including identifier and interaction tracking
#[derive(Debug, Clone, Default)]
pub struct AssistantTurnStartData {
    /// Agent mode whose tools this turn's run was offered. Absent on historical events and sub-agent turns.
    pub agent_mode: Option<SessionMode>,
    /// CAPI interaction ID for correlating this turn with upstream telemetry
    pub interaction_id: Option<String>,
    /// Model identifier used for this turn, when known
    pub model: Option<String>,
    /// Parent task tool call ID when this turn belongs to a sub-agent
    pub parent_tool_call_id: Option<String>,
    /// Identifier for this turn within the agentic loop, typically a stringified turn number
    pub turn_id: String,
}
crate::record_serde::wire_record!(AssistantTurnStartData { agent_mode: "agentMode" if is_none, interaction_id: "interactionId" if is_none, model: "model" if is_none, parent_tool_call_id: "parentToolCallId" if is_none, turn_id: "turnId" });

/// Session event "assistant.turn_retry". Metadata for an additional model inference attempt within an existing assistant turn
#[derive(Debug, Clone, Default)]
pub struct AssistantTurnRetryData {
    /// Model identifier used for this retry, when known
    pub model: Option<String>,
    /// Provider or runtime classification that caused the retry, when known
    pub reason: Option<String>,
    /// Identifier of the turn whose model inference is being retried
    pub turn_id: String,
}
crate::record_serde::wire_record!(AssistantTurnRetryData { model: "model" if is_none, reason: "reason" if is_none, turn_id: "turnId" });

/// Session event "agent.interrupted". Metadata for work the user interrupted while the agent was running
#[derive(Debug, Clone, Default)]
pub struct AgentInterruptedData {
    /// What the agent was doing when the user interrupted it
    pub activity: AgentInterruptedActivity,
    /// For an interrupted model call: the provider endpoint the request targeted
    pub api_endpoint: Option<String>,
    /// For an interrupted model call: whether the user interrupted before any token arrived or while the response was streaming
    pub cancel_phase: Option<AgentInterruptedCancelPhase>,
    /// How long the interrupted work had been running, in milliseconds
    pub elapsed_ms: f64,
    /// For an interrupted background-agent batch: how many background sub-agents the stop swept. Counts accepted cancellations, so an agent cancelled as a cascade of its interrupted parent is covered by that parent rather than counted again.
    pub interrupted_agent_count: Option<i64>,
    /// For an interrupted model call: the model the request targeted
    pub model: Option<String>,
    /// For a mid-stream interrupt: the observed time to first observable output, in milliseconds. Deliberately distinct from the `ttftMs` reported on a successful model call, which measures time to first stream event.
    pub output_ttft_ms: Option<f64>,
    /// For an interrupted model call: the reasoning effort the request asked for
    pub reasoning_effort: Option<String>,
    /// Subset of `toolNames` whose tool metadata marks the tool name as safe to record unhashed in telemetry.
    pub safe_tool_names: Option<Vec<String>>,
    /// Tool call identifiers that were still running
    pub tool_call_ids: Option<Vec<String>>,
    /// Names of the tools that were still running. More than one when the model requested a parallel fan-out.
    pub tool_names: Option<Vec<String>>,
    /// For an interrupted model call: the transport the request used
    pub transport: Option<ModelCallFailureTransport>,
    /// Zero-based agentic-loop iteration the interrupt landed in
    pub turn: i64,
}
crate::record_serde::wire_record!(AgentInterruptedData { activity: "activity", api_endpoint: "apiEndpoint" if is_none, cancel_phase: "cancelPhase" if is_none, elapsed_ms: "elapsedMs", interrupted_agent_count: "interruptedAgentCount" if is_none, model: "model" if is_none, output_ttft_ms: "outputTtftMs" if is_none, reasoning_effort: "reasoningEffort" if is_none, safe_tool_names: "safeToolNames" if is_none, tool_call_ids: "toolCallIds" if is_none, tool_names: "toolNames" if is_none, transport: "transport" if is_none, turn: "turn" });

/// Session event "assistant.intent". Agent intent description for current activity or plan
#[derive(Debug, Clone, Default)]
pub struct AssistantIntentData {
    /// Short description of what the agent is currently doing or planning to do
    pub intent: String,
}
crate::record_serde::wire_record!(AssistantIntentData { intent: "intent" });

/// Session event "assistant.fusion_phase_started". Experimental transient HydraFusion phase/model/role signal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantFusionPhaseStartedData {
    /// Conversation scope in which the phase executes.
    pub conversation_scope: FusionConversationScope,
    /// Identifier of the HydraFusion turn containing the phase.
    pub fusion_id: String,
    /// Concrete model executing the phase.
    pub model: String,
    /// HydraFusion orchestration pattern containing the phase.
    pub pattern: FusionPattern,
    /// Stable identifier for the concrete phase.
    pub phase_id: String,
    /// Kind of phase being executed.
    pub phase_kind: FusionPhaseKind,
    /// Explicit reasoning effort selected for this phase, if supplied.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub reasoning_effort: Option<String>,
    /// Semantic role assigned to the phase.
    pub role: String,
}
crate::record_serde::wire_record!(AssistantFusionPhaseStartedData { conversation_scope: "conversationScope", fusion_id: "fusionId", model: "model", pattern: "pattern", phase_id: "phaseId", phase_kind: "phaseKind", reasoning_effort: "reasoningEffort" if is_none, role: "role" });

/// Session event "assistant.fusion_phase_activity". Experimental content-safe activity signal for a running HydraFusion phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantFusionPhaseActivityData {
    /// Kind of real activity observed.
    pub activity: FusionPhaseActivityKind,
    /// Conversation scope in which the phase executes.
    pub conversation_scope: FusionConversationScope,
    /// Identifier of the HydraFusion turn containing the phase.
    pub fusion_id: String,
    /// HydraFusion orchestration pattern containing the phase.
    pub pattern: FusionPattern,
    /// Stable identifier for the concrete phase.
    pub phase_id: String,
    /// Kind of phase currently executing.
    pub phase_kind: FusionPhaseKind,
    /// Semantic role assigned to the phase.
    pub role: String,
    /// Opaque hashed correlation token for matching tool-started and tool-completed activity within this Fusion activity stream. It is not the tool call identifier exposed by tool lifecycle events.
    pub tool_call_id: Option<String>,
    /// Cumulative private response bytes observed for this model call. The event never includes response text.
    pub total_response_size_bytes: Option<i64>,
}
crate::record_serde::wire_record!(AssistantFusionPhaseActivityData { activity: "activity", conversation_scope: "conversationScope", fusion_id: "fusionId", pattern: "pattern", phase_id: "phaseId", phase_kind: "phaseKind", role: "role", tool_call_id: "toolCallId" if is_none, total_response_size_bytes: "totalResponseSizeBytes" if is_none });

/// Internal durable terminal request staged by a HydraFusion phase until an idempotent final commit selects it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FusionStagedTerminal {
    pub arguments: String,
    pub assistant_message: serde_json::Value,
    pub phase_id: String,
    pub tool_call_id: String,
    pub tool_name: String,
}
crate::record_serde::wire_record!(FusionStagedTerminal {
    arguments: "arguments",
    assistant_message: "assistantMessage",
    phase_id: "phaseId",
    tool_call_id: "toolCallId",
    tool_name: "toolName"
});

/// Aggregate concrete-model usage for one HydraFusion phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionPhaseUsage {
    /// Total cached input tokens reported for the phase.
    pub cached_tokens: i64,
    /// Total tokens written to prompt cache during the phase.
    pub cache_write_tokens: Option<i64>,
    /// Total input tokens consumed by the phase.
    pub input_tokens: i64,
    /// Total output tokens produced by the phase.
    pub output_tokens: i64,
    /// Number of concrete model requests made by the phase.
    pub request_count: i64,
    /// Total normalized AI-unit cost reported for the phase, in nano-AIU.
    pub total_nano_aiu: f64,
}
crate::record_serde::wire_record!(FusionPhaseUsage { cached_tokens: "cachedTokens", cache_write_tokens: "cacheWriteTokens" if is_none, input_tokens: "inputTokens", output_tokens: "outputTokens", request_count: "requestCount", total_nano_aiu: "totalNanoAiu" });

/// Session event "assistant.fusion_phase_completed". Experimental durable HydraFusion phase output and lossless replay checkpoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantFusionPhaseCompletedData {
    /// Provider-normalized textual output produced by the phase.
    pub content: String,
    /// Conversation scope in which the phase executed.
    pub conversation_scope: FusionConversationScope,
    /// Elapsed execution time for the phase in milliseconds.
    pub duration_ms: f64,
    /// True when a first pass (Cascade primary or Critique draft) ended with background work outstanding, whether or not the turn then committed it without review; absent otherwise.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub ended_with_background_work: Option<bool>,
    /// Identifier of the HydraFusion turn containing the phase.
    pub fusion_id: String,
    /// Concrete model that executed the phase.
    pub model: String,
    /// Stable identifier for the completed phase.
    pub phase_id: String,
    /// Kind of phase that completed.
    pub phase_kind: FusionPhaseKind,
    /// Exact provider-normalized message used to reconstruct canonical model history.
    #[doc(hidden)]
    pub(crate) projection_message: Option<serde_json::Value>,
    /// Projection action for the exact internal message.
    #[doc(hidden)]
    pub(crate) projection_mode: Option<FusionProjectionMode>,
    /// Explicit reasoning effort selected for this phase, if supplied.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub reasoning_effort: Option<String>,
    /// Semantic role assigned to the completed phase.
    pub role: String,
    /// Terminal request held outside canonical state until selected by the final commit.
    #[doc(hidden)]
    pub(crate) staged_terminal: Option<FusionStagedTerminal>,
    /// Durable outcome status of the phase.
    pub status: FusionPhaseStatus,
    /// Aggregate concrete-model usage consumed by the phase.
    pub usage: FusionPhaseUsage,
    /// Structured judge or critic verdict, when the phase produces one.
    pub verdict: Option<String>,
}
crate::record_serde::wire_record!(AssistantFusionPhaseCompletedData { content: "content", conversation_scope: "conversationScope", duration_ms: "durationMs", ended_with_background_work: "endedWithBackgroundWork" if is_none, fusion_id: "fusionId", model: "model", phase_id: "phaseId", phase_kind: "phaseKind", projection_message: "projectionMessage" if is_none, projection_mode: "projectionMode" if is_none, reasoning_effort: "reasoningEffort" if is_none, role: "role", staged_terminal: "stagedTerminal" if is_none, status: "status", usage: "usage", verdict: "verdict" });

/// Session event "assistant.fusion_phase_failed". Experimental durable typed HydraFusion phase failure and degradation transition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantFusionPhaseFailedData {
    /// Conversation scope in which the phase executed.
    pub conversation_scope: FusionConversationScope,
    /// Identifier of the fallback phase used to continue the turn after degradation.
    pub degraded_to_phase_id: Option<String>,
    /// Elapsed execution time before the phase failed, in milliseconds.
    pub duration_ms: f64,
    /// Provider or execution error detail, when available.
    pub error_message: Option<String>,
    /// Identifier of the HydraFusion turn containing the phase.
    pub fusion_id: String,
    /// Concrete model that attempted the phase.
    pub model: String,
    /// Stable identifier for the failed phase.
    pub phase_id: String,
    /// Kind of phase that failed.
    pub phase_kind: FusionPhaseKind,
    /// Stable machine-readable reason for the phase failure.
    pub reason: String,
    /// Explicit reasoning effort selected for this phase, if supplied.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub reasoning_effort: Option<String>,
    /// Semantic role assigned to the failed phase.
    pub role: String,
    /// Durable outcome status of the phase.
    pub status: FusionPhaseStatus,
    /// Aggregate concrete-model usage consumed before the failure.
    pub usage: FusionPhaseUsage,
}
crate::record_serde::wire_record!(AssistantFusionPhaseFailedData { conversation_scope: "conversationScope", degraded_to_phase_id: "degradedToPhaseId" if is_none, duration_ms: "durationMs", error_message: "errorMessage" if is_none, fusion_id: "fusionId", model: "model", phase_id: "phaseId", phase_kind: "phaseKind", reason: "reason", reasoning_effort: "reasoningEffort" if is_none, role: "role", status: "status", usage: "usage" });

/// Session event "assistant.server_tool_progress". Live progress signal for a provider-hosted server tool (e.g. hosted web search) while it runs, before the finalized serverTools envelope lands on the terminal assistant.message
#[derive(Debug, Clone, Default)]
pub struct AssistantServerToolProgressData {
    /// Kind of hosted server tool that is running. Only `web_search` is emitted today.
    pub kind: String,
    /// Position of the hosted tool call in the response output. Stable across the call's lifecycle events (unlike the provider's per-event item id, which CAPI rotates), so the host keys the live in-progress row on it.
    pub output_index: i64,
    /// Lifecycle status of the hosted call: `in_progress`, `searching`, or `completed`.
    pub status: String,
}
crate::record_serde::wire_record!(AssistantServerToolProgressData {
    kind: "kind",
    output_index: "outputIndex",
    status: "status"
});

/// Experimental attribution linking an ordinary event to the HydraFusion turn, phase, and concrete source that produced it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FusionAttribution {
    /// Idempotency identifier for the authoritative commit, when the event belongs to the selected output.
    pub commit_id: Option<String>,
    /// Conversation scope in which the concrete phase executed.
    pub conversation_scope: Option<String>,
    /// Stable identifier for the HydraFusion turn that produced the event.
    pub fusion_id: String,
    /// Whether this model request consumed a user steering message rather than only internal Fusion work.
    pub has_user_steering: Option<bool>,
    /// HydraFusion orchestration pattern selected for the turn.
    pub pattern: String,
    /// Identifier of the concrete phase that produced the event.
    pub phase_id: Option<String>,
    /// Kind of concrete phase that produced the event.
    pub phase_kind: Option<String>,
    /// HydraFusion routing policy used for the turn.
    pub policy: String,
    /// Semantic role assigned to the concrete phase.
    pub role: Option<String>,
    /// Concrete model that produced the attributed event.
    pub source_model: Option<String>,
    /// Phase whose output supplied the authoritative content, when different from the executing phase.
    pub source_phase_id: Option<String>,
    /// Synthetic HydraFusion model selected for the session.
    pub synthetic_model: String,
}
crate::record_serde::wire_record!(FusionAttribution { commit_id: "commitId" if is_none, conversation_scope: "conversationScope" if is_none, fusion_id: "fusionId", has_user_steering: "hasUserSteering" if is_none, pattern: "pattern", phase_id: "phaseId" if is_none, phase_kind: "phaseKind" if is_none, policy: "policy", role: "role" if is_none, source_model: "sourceModel" if is_none, source_phase_id: "sourcePhaseId" if is_none, synthetic_model: "syntheticModel" });

/// Session event "assistant.reasoning". Assistant reasoning content for timeline display with complete thinking text
#[derive(Debug, Clone, Default)]
pub struct AssistantReasoningData {
    /// Provider's completion / response identifier of the model call that produced this reasoning, matching the apiCallId of that call's assistant.message
    pub api_call_id: Option<String>,
    /// The complete extended thinking text from the model
    pub content: String,
    /// Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Whether the model call that produced this reasoning used a user-managed provider or an on-device model
    pub is_byok: Option<bool>,
    /// Model that produced this reasoning, if known
    pub model: Option<String>,
    /// Unique identifier for this reasoning block
    pub reasoning_id: String,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    pub rte: Option<bool>,
    /// Identifier for the agent loop turn that produced this reasoning, matching the corresponding assistant.turn_start event
    pub turn_id: Option<String>,
}
crate::record_serde::wire_record!(AssistantReasoningData { api_call_id: "apiCallId" if is_none, content: "content", fusion: "fusion" if is_none, is_byok: "isByok" if is_none, model: "model" if is_none, reasoning_id: "reasoningId", rte: "rte" if is_none, turn_id: "turnId" if is_none });

/// Session event "assistant.reasoning_delta". Streaming reasoning delta for incremental extended thinking updates
#[derive(Debug, Clone, Default)]
pub struct AssistantReasoningDeltaData {
    /// Incremental text chunk to append to the reasoning content
    pub delta_content: String,
    /// Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Reasoning block ID this delta belongs to, matching the corresponding assistant.reasoning event
    pub reasoning_id: String,
}
crate::record_serde::wire_record!(AssistantReasoningDeltaData { delta_content: "deltaContent", fusion: "fusion" if is_none, reasoning_id: "reasoningId" });

/// Session event "assistant.tool_call_delta". Streaming tool-call input delta for incremental tool-call updates
#[derive(Debug, Clone, Default)]
pub struct AssistantToolCallDeltaData {
    /// Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Raw provider tool input fragment to append for this tool call. Function/tool-use providers stream serialized JSON argument text (so newlines inside JSON string values may appear as escaped `\n` until the accumulated JSON is parsed); custom tool calls stream raw custom input.
    pub input_delta: String,
    /// Tool call ID this delta belongs to, matching the corresponding assistant.message tool request
    pub tool_call_id: String,
    /// Name of the tool being invoked, when known from the stream
    pub tool_name: Option<String>,
    /// Tool call type, when known from the stream
    pub tool_type: Option<AssistantMessageToolRequestType>,
}
crate::record_serde::wire_record!(AssistantToolCallDeltaData { fusion: "fusion" if is_none, input_delta: "inputDelta", tool_call_id: "toolCallId", tool_name: "toolName" if is_none, tool_type: "toolType" if is_none });

/// Session event "assistant.streaming_delta". Streaming response progress with cumulative byte count
#[derive(Debug, Clone, Default)]
pub struct AssistantStreamingDeltaData {
    /// Cumulative total bytes received from the streaming response so far
    pub total_response_size_bytes: i64,
}
crate::record_serde::wire_record!(AssistantStreamingDeltaData {
    total_response_size_bytes: "totalResponseSizeBytes"
});

/// A source that backs one or more cited spans in the assistant's response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CitationSource {
    /// Stable, turn-scoped identifier for this source, referenced by CitationReference.sourceId.
    pub id: String,
    /// File path relative to the agent's workspace root, when the source is a file.
    pub path: Option<String>,
    /// The system that produced this citation.
    pub provider: CitationProvider,
    /// Human-readable title of the source.
    pub title: Option<String>,
    /// URL of the source, when it is a web resource.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(CitationSource { id: "id", path: "path" if is_none, provider: "provider", title: "title" if is_none, url: "url" if is_none });

/// A single citation occurrence linking a span of generated text to a supporting source.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CitationReference {
    /// The exact text from the source that supports the cited span, when provided by the model.
    pub cited_text: Option<String>,
    /// Location within the source that supports the cited span, when the provider reports one.
    pub location: Option<serde_json::Value>,
    /// Provider-native citation correlation data (e.g. Anthropic search_result_index / document_index), passed through opaquely for debugging and forward compatibility.
    pub provider_metadata: Option<serde_json::Value>,
    /// Identifier of the CitationSource this reference points to (CitationSource.id).
    pub source_id: String,
}
crate::record_serde::wire_record!(CitationReference { cited_text: "citedText" if is_none, location: "location" if is_none, provider_metadata: "providerMetadata" if is_none, source_id: "sourceId" });

/// A contiguous span of generated assistant text and the source references that support it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CitationSpan {
    /// End offset of the cited span within the final assistant message content (UTF-16 code units, zero-based, exclusive).
    pub end_index: i64,
    /// The sources that support this span of generated text.
    pub references: Vec<CitationReference>,
    /// Start offset of the cited span within the final assistant message content (UTF-16 code units, zero-based, inclusive).
    pub start_index: i64,
}
crate::record_serde::wire_record!(CitationSpan {
    end_index: "endIndex",
    references: "references",
    start_index: "startIndex"
});

/// Provider-agnostic citations linking spans of the assistant's response to their supporting sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct Citations {
    /// Deduplicated set of sources referenced by the citation spans.
    pub sources: Vec<CitationSource>,
    /// Spans of generated text annotated with the sources that support them.
    pub spans: Vec<CitationSpan>,
}
crate::record_serde::wire_record!(Citations {
    sources: "sources",
    spans: "spans"
});

/// Neutral provider-tagged reasoning content blocks preserved verbatim for round-tripping
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageReasoningBlocks {
    /// Provider-native reasoning items or content blocks preserved verbatim, in order. A single response can carry several, and provider signatures or identifiers may depend on their exact content and ordering.
    pub blocks: Option<Vec<serde_json::Value>>,
    /// Anthropic Messages assistant block ordering preserved when the legacy reasoning-only representation cannot reproduce it exactly. Thinking and text blocks remain verbatim; tool-use entries retain identity and a payload fingerprint when later signed reasoning depends on them, and are hydrated from the message's tool requests during replay.
    pub ordered_blocks: Option<Vec<serde_json::Value>>,
    /// Model provider that produced these reasoning blocks.
    pub provider: String,
}
crate::record_serde::wire_record!(AssistantMessageReasoningBlocks { blocks: "blocks" if is_none, ordered_blocks: "orderedBlocks" if is_none, provider: "provider" });

/// Neutral provider-tagged server-side tool-use payload (tool search, advisor) for verbatim round-tripping
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageServerTools {
    /// Advisor model identifier associated with the server-tool payload.
    pub advisor_model: Option<String>,
    /// Provider function-call namespaces keyed by function-call identifier.
    pub function_call_namespaces: Option<HashMap<String, String>>,
    /// Provider-native server-tool call and output items preserved verbatim for replay.
    pub items: Option<Vec<serde_json::Value>>,
    /// Model provider that produced this server-tool payload.
    pub provider: String,
    /// Raw provider content blocks retained for verbatim round-tripping.
    pub raw_content_blocks: Option<Vec<serde_json::Value>>,
}
crate::record_serde::wire_record!(AssistantMessageServerTools { advisor_model: "advisorModel" if is_none, function_call_namespaces: "functionCallNamespaces" if is_none, items: "items" if is_none, provider: "provider", raw_content_blocks: "rawContentBlocks" if is_none });

/// Hosted program that requested this client tool call
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageToolRequestCaller {
    /// Provider-assigned identifier for the hosted caller.
    pub caller_id: String,
    /// Kind of hosted caller that requested the client tool call.
    pub r#type: AssistantMessageToolRequestCallerType,
}
crate::record_serde::wire_record!(AssistantMessageToolRequestCaller {
    caller_id: "callerId",
    r#type: "type"
});

/// A tool invocation request from the assistant
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageToolRequest {
    /// Arguments to pass to the tool, format depends on the tool
    pub arguments: Option<serde_json::Value>,
    /// Hosted program that requested this client tool call
    pub caller: Option<AssistantMessageToolRequestCaller>,
    /// Resolved intention summary describing what this specific call does
    pub intention_summary: Option<String>,
    /// Explicitly configured MCP server display name.
    pub mcp_server_display_name: Option<String>,
    /// Configured key of the MCP server hosting this tool. Legacy events may contain the display name instead.
    pub mcp_server_name: Option<String>,
    /// Original tool name on the MCP server, when the tool is an MCP tool
    pub mcp_tool_name: Option<String>,
    /// Name of the tool being invoked
    pub name: String,
    /// Unique identifier for this tool call
    pub tool_call_id: String,
    /// Human-readable display title for the tool
    pub tool_title: Option<String>,
    /// Tool call type: "function" for standard tool calls, "custom" for grammar-based tool calls. Defaults to "function" when absent.
    pub r#type: Option<AssistantMessageToolRequestType>,
}
crate::record_serde::wire_record!(AssistantMessageToolRequest { arguments: "arguments" if is_none, caller: "caller" if is_none, intention_summary: "intentionSummary" if is_none, mcp_server_display_name: "mcpServerDisplayName" if is_none, mcp_server_name: "mcpServerName" if is_none, mcp_tool_name: "mcpToolName" if is_none, name: "name", tool_call_id: "toolCallId", tool_title: "toolTitle" if is_none, r#type: "type" if is_none });

/// Session event "assistant.message". Assistant response containing text content, optional tool requests, and interaction metadata
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageData {
    /// Provider's completion / response identifier; shared across all chunks of a single API call. Used to group multi-chunk assistant utterances.
    pub api_call_id: Option<String>,
    /// Total messages the model call's response was split into, one per reasoning boundary. Absent for a single-message response; the last chunk is the one where chunkIndex is chunkCount - 1.
    pub chunk_count: Option<i64>,
    /// Zero-based position of this message within its model call's response. Absent when the response was not split into chunks.
    pub chunk_index: Option<i64>,
    /// Provider-agnostic citations linking spans of this message's content to the sources that support them. Experimental; only populated when citation emission is enabled.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub citations: Option<Citations>,
    /// Client-minted request id (x-request-id header) echoed by the server. Distinct from requestId (x-github-request-id) and serviceRequestId (x-copilot-service-request-id).
    pub client_request_id: Option<String>,
    /// The assistant's text response content
    pub content: String,
    /// Encrypted reasoning content from OpenAI models. Session-bound and stripped on resume.
    pub encrypted_content: Option<String>,
    /// Experimental HydraFusion source attribution for this ordinary authoritative assistant message.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// CAPI interaction ID for correlating this message with upstream telemetry
    pub interaction_id: Option<String>,
    /// Unique identifier for this assistant message
    pub message_id: String,
    /// Model that produced this assistant message, if known
    pub model: Option<String>,
    /// Logical ID of the primary user message that initiated this run, matching the messageId returned by session.send (or the last messageId of session.sendMessages). Stable across model/tool iterations, steering messages, and stop-hook corrections. Subagent runs use their own initiating message ID, not the parent's. Absent for runs without an associated initiating message, such as empty batches.
    pub originating_message_id: Option<String>,
    /// Output tokens this message adds to replayed context, for token accounting: the API response's completion_tokens, shared across the response's message chunks. Use assistant.usage for billed usage.
    pub output_tokens: Option<i64>,
    /// Generation phase for phased-output models (e.g., thinking vs. response phases)
    pub phase: Option<String>,
    /// Opaque origin provider identity captured at dispatch. Only an exact match with the target provider permits replay of reasoningOpaque, encryptedContent, reasoningBlocks, serverTools, and provider response/item identifiers. Matching model ids or wire protocols is insufficient. Omitted for legacy or unattributed messages; the runtime excludes unproven opaque state when projecting to a known provider. Text content and client tool requests/results remain portable semantic history.
    pub provider_id: Option<String>,
    /// Neutral provider-tagged reasoning content blocks preserved verbatim for round-tripping. `reasoningText` and `reasoningOpaque` are a lossy derived view of these blocks, retained for display.
    pub reasoning_blocks: Option<AssistantMessageReasoningBlocks>,
    /// Opaque/encrypted extended thinking data from Anthropic models. Session-bound and stripped on resume.
    pub reasoning_opaque: Option<String>,
    /// Readable reasoning text from the model's extended thinking
    pub reasoning_text: Option<String>,
    /// OpenAI-compatible wire field the provider used for reasoning (e.g. reasoning_content/reasoning). Populated only when non-canonical, so the dialect round-trips across turns.
    pub reasoning_wire_field: Option<String>,
    /// GitHub request tracing ID (x-github-request-id header) for correlating with server-side logs
    pub request_id: Option<RequestId>,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    pub rte: Option<bool>,
    /// Neutral provider-tagged server-side tool-use payload (tool search, advisor) for verbatim round-tripping
    pub server_tools: Option<AssistantMessageServerTools>,
    /// Copilot service request ID (x-copilot-service-request-id header) for CAPI log correlation
    pub service_request_id: Option<String>,
    /// Tool invocations requested by the assistant in this message
    pub tool_requests: Option<Vec<AssistantMessageToolRequest>>,
    /// Identifier for the agent loop turn that produced this message, matching the corresponding assistant.turn_start event
    pub turn_id: Option<String>,
}
crate::record_serde::wire_record!(AssistantMessageData { api_call_id: "apiCallId" if is_none, chunk_count: "chunkCount" if is_none, chunk_index: "chunkIndex" if is_none, citations: "citations" if is_none, client_request_id: "clientRequestId" if is_none, content: "content", encrypted_content: "encryptedContent" if is_none, fusion: "fusion" if is_none, interaction_id: "interactionId" if is_none, message_id: "messageId", model: "model" if is_none, originating_message_id: "originatingMessageId" if is_none, output_tokens: "outputTokens" if is_none, phase: "phase" if is_none, provider_id: "providerId" if is_none, reasoning_blocks: "reasoningBlocks" if is_none, reasoning_opaque: "reasoningOpaque" if is_none, reasoning_text: "reasoningText" if is_none, reasoning_wire_field: "reasoningWireField" if is_none, request_id: "requestId" if is_none, rte: "rte" if is_none, server_tools: "serverTools" if is_none, service_request_id: "serviceRequestId" if is_none, tool_requests: "toolRequests" if is_none, turn_id: "turnId" if is_none });

/// Session event "assistant.message_start". Streaming assistant message start metadata
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageStartData {
    /// Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Message ID this start event belongs to, matching subsequent deltas and assistant.message
    pub message_id: String,
    /// Generation phase this message belongs to for phased-output models
    pub phase: Option<String>,
}
crate::record_serde::wire_record!(AssistantMessageStartData { fusion: "fusion" if is_none, message_id: "messageId", phase: "phase" if is_none });

/// Session event "assistant.message_delta". Streaming assistant message delta for incremental response updates
#[derive(Debug, Clone, Default)]
pub struct AssistantMessageDeltaData {
    /// Incremental text chunk to append to the message content
    pub delta_content: String,
    /// Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Message ID this delta belongs to, matching the corresponding assistant.message event
    pub message_id: String,
}
crate::record_serde::wire_record!(AssistantMessageDeltaData { delta_content: "deltaContent", fusion: "fusion" if is_none, message_id: "messageId" });

/// Session event "assistant.turn_end". Turn completion metadata including the turn identifier
#[derive(Debug, Clone, Default)]
pub struct AssistantTurnEndData {
    /// Model identifier used for this turn, when known
    pub model: Option<String>,
    /// Parent task tool call ID when this turn belongs to a sub-agent
    pub parent_tool_call_id: Option<String>,
    /// Identifier of the turn that has ended, matching the corresponding assistant.turn_start event
    pub turn_id: String,
}
crate::record_serde::wire_record!(AssistantTurnEndData { model: "model" if is_none, parent_tool_call_id: "parentToolCallId" if is_none, turn_id: "turnId" });

/// Session event "assistant.idle". Payload emitted whenever the main agent's processing loop goes idle, including while related background work (running agents or in-flight attached shell commands) is still pending and the session-level idle event is therefore deferred
#[derive(Debug, Clone, Default)]
pub struct AssistantIdleData {
    /// True when the preceding agentic loop was cancelled via abort signal
    pub aborted: Option<bool>,
}
crate::record_serde::wire_record!(AssistantIdleData { aborted: "aborted" if is_none });

/// Ordered accounting identity assigned under the source session's emission lock.
#[derive(Debug, Clone, Default)]
pub struct UsageAccountingIdentity {
    /// Monotonically increasing sequence within the source session.
    pub sequence: i64,
    /// Session that assigned this accounting sequence.
    pub source_session_id: String,
    /// Existing API call identifier, or a runtime-generated identity when none was supplied.
    pub usage_id: String,
}
crate::record_serde::wire_record!(UsageAccountingIdentity {
    sequence: "sequence",
    source_session_id: "sourceSessionId",
    usage_id: "usageId"
});

/// Token usage detail for a single billing category
#[derive(Debug, Clone, Default)]
pub struct AssistantUsageCopilotUsageTokenDetail {
    /// Number of tokens in this billing batch
    pub batch_size: i64,
    /// Cost per batch of tokens
    pub cost_per_batch: i64,
    /// Model responsible for this billing entry
    pub model: Option<String>,
    /// Total token count for this entry
    pub token_count: i64,
    /// Token category (e.g., "input", "output")
    pub token_type: String,
}
crate::record_serde::wire_record!(AssistantUsageCopilotUsageTokenDetail { batch_size: "batchSize", cost_per_batch: "costPerBatch", model: "model" if is_none, token_count: "tokenCount", token_type: "tokenType" });

/// Per-request cost and usage data from the CAPI copilot_usage response field
#[derive(Debug, Clone, Default)]
pub struct AssistantUsageCopilotUsage {
    /// Default billing model for token details that do not identify their own model
    pub model: Option<String>,
    /// Itemized token usage breakdown
    #[doc(hidden)]
    pub(crate) token_details: Option<Vec<AssistantUsageCopilotUsageTokenDetail>>,
    /// Total cost in nano-AI units for this request
    pub total_nano_aiu: f64,
}
crate::record_serde::wire_record!(AssistantUsageCopilotUsage { model: "model" if is_none, token_details: "tokenDetails" if is_none, total_nano_aiu: "totalNanoAiu" });

/// Internal per-quota snapshot for assistant usage, including entitlement, consumed requests, overage, reset date, and remaining quota.
#[derive(Debug, Clone, Default)]
pub(crate) struct AssistantUsageQuotaSnapshot {
    /// Total requests allowed by the entitlement
    #[doc(hidden)]
    pub(crate) entitlement_requests: i64,
    /// Whether the user currently has quota available for use
    #[doc(hidden)]
    pub(crate) has_quota: Option<bool>,
    /// Whether the user has an unlimited usage entitlement
    #[doc(hidden)]
    pub(crate) is_unlimited_entitlement: bool,
    /// Number of additional usage requests made this period
    #[doc(hidden)]
    pub(crate) overage: f64,
    /// Whether additional usage is allowed when quota is exhausted
    #[doc(hidden)]
    pub(crate) overage_allowed_with_exhausted_quota: bool,
    /// Pay-as-you-go additional-usage budget cap in AI credits (1 credit = $0.01); present only when CAPI emits a finite value
    #[doc(hidden)]
    pub(crate) overage_entitlement: Option<f64>,
    /// Percentage of quota remaining (0 to 100)
    #[doc(hidden)]
    pub(crate) remaining_percentage: f64,
    /// Date when the quota resets
    #[doc(hidden)]
    pub(crate) reset_date: Option<String>,
    /// Whether this snapshot uses token-based billing (AI-credits allocation)
    #[doc(hidden)]
    pub(crate) token_based_billing: Option<bool>,
    /// Whether usage is still permitted after quota exhaustion
    #[doc(hidden)]
    pub(crate) usage_allowed_with_exhausted_quota: bool,
    /// Number of requests already consumed
    #[doc(hidden)]
    pub(crate) used_requests: i64,
}
crate::record_serde::wire_record!(AssistantUsageQuotaSnapshot { entitlement_requests: "entitlementRequests", has_quota: "hasQuota" if is_none, is_unlimited_entitlement: "isUnlimitedEntitlement", overage: "overage", overage_allowed_with_exhausted_quota: "overageAllowedWithExhaustedQuota", overage_entitlement: "overageEntitlement" if is_none, remaining_percentage: "remainingPercentage", reset_date: "resetDate" if is_none, token_based_billing: "tokenBasedBilling" if is_none, usage_allowed_with_exhausted_quota: "usageAllowedWithExhaustedQuota", used_requests: "usedRequests" });

/// Session event "assistant.usage". LLM API call usage metrics including tokens, costs, quotas, and billing information
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AssistantUsageData {
    /// Number of accepted speculative prediction tokens
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accepted_prediction_tokens: Option<i64>,
    /// Runtime-owned identity shared with the durable accounting receipt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounting: Option<UsageAccountingIdentity>,
    /// Availability of this call's reported AI-credit amount. Missing billing is unavailable, not a zero charge.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ai_credits_status: Option<AiCreditsStatus>,
    /// Completion ID from the model provider (e.g., chatcmpl-abc123)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_call_id: Option<String>,
    /// API endpoint used for this model call, matching CAPI supported_endpoints vocabulary
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_endpoint: Option<AssistantUsageApiEndpoint>,
    /// Number of tools available to the model for this call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) available_tool_count: Option<i64>,
    /// Where the bring-your-own-key model runs and who manages it: "local_managed" (on the device, managed by Copilot), "local_user" (on the device, managed by the user), or "remote_user" (off the device, managed by the user). Absent for Copilot-served models.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byok_kind: Option<String>,
    /// Whether the provider reported prompt-cache usage details for this call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) cache_details_reported: Option<bool>,
    /// Updated prompt-cache expiration for this model call. Present only when the call establishes or refreshes known cache state.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_expires_at: Option<String>,
    /// Request-scoped prompt-cache policy lifetime in seconds; the shortest lifetime for mixed known policies. Does not establish or refresh cache state. Omitted when the policy is unknown.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) cache_policy_ttl_seconds: Option<i64>,
    /// Number of tokens read from prompt cache
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_read_tokens: Option<i64>,
    /// Effective prompt-cache lifetime in seconds for this call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) cache_ttl_seconds: Option<i64>,
    /// Number of tokens written to prompt cache
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_write_tokens: Option<i64>,
    /// Whether the model response was blocked or truncated by content filtering (finish_reason === 'content_filter'). For Anthropic models this corresponds to a 'refusal' stop reason.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_filter_triggered: Option<bool>,
    /// Per-request cost and usage data from the CAPI copilot_usage response field
    #[serde(skip_serializing_if = "Option::is_none")]
    pub copilot_usage: Option<AssistantUsageCopilotUsage>,
    /// Model multiplier cost for billing purposes
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cost: Option<f64>,
    /// Free-form custom tool calls in the request history before request shaping, counted apart from named function calls
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) custom_tool_call_count: Option<i64>,
    /// Duration of the API call in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<i64>,
    /// Finish reason reported by the model for this API call (e.g. "stop", "length", "tool_calls", "content_filter"). Normalized to OpenAI vocabulary; for Anthropic models a "refusal" stop reason maps to "content_filter".
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
    /// How the prompt-cache frontier was determined for this call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) frontier_source: Option<String>,
    /// Experimental HydraFusion attribution for this concrete model call's usage.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fusion: Option<FusionAttribution>,
    /// Whether this dispatched request selected the managed inference gateway. Absent for older producers or when no request was dispatched; not proof of gateway receipt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inference_gateway: Option<bool>,
    /// What initiated this API call: "user" for a request that carries a user prompt, "agent" for an agent-driven request such as the follow-up after a tool result, or the delegating caller (e.g., "sub-agent", "mcp-sampling")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initiator: Option<String>,
    /// Number of input tokens consumed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_tokens: Option<i64>,
    /// Internal correlation identifier for the qualifying instruction-update delivery consumed by this model request.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) instruction_update_delivery_id: Option<String>,
    /// Event identifier of the latest canonical instruction update included in this model request.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) instruction_update_latest_event_id: Option<String>,
    /// Coarse classification of the interaction that produced this call, mirroring the session's per-request agent context (e.g. `conversation-agent`, `conversation-subagent`, `conversation-sampling`, `conversation-background`, `conversation-compaction`, `conversation-user`). Non-billing; lets consumers attribute a model call to a call class (e.g. sub-agent/sidekick) independently of the billing initiator. Absent when the runtime did not classify the request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interaction_type: Option<String>,
    /// Average inter-token latency in milliseconds. Only available for streaming requests
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inter_token_latency_ms: Option<f64>,
    /// Whether Auto mode was selected for this model call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_auto: Option<bool>,
    /// Whether this model call used a bring-your-own-key provider
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_byok: Option<bool>,
    /// Requested maximum output tokens used for this model call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<i64>,
    /// Effective maximum prompt-token limit used for this model call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<i64>,
    /// Model identifier used for this API call
    pub model: String,
    /// Model display name captured for this call, not derived from the current selection.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_display_name: Option<String>,
    /// Fixed-set provider family serving the bring-your-own-key model (for example "openai", "anthropic", "azure_openai", "ollama", "llama_cpp", or "other"). Never the caller-supplied provider name. Absent for Copilot-served models.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
    /// Number of tool calls returned by the model
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) num_tool_calls: Option<i64>,
    /// Tool calls in the request history before request shaping that no later tool result answers
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) orphan_tool_call_count: Option<i64>,
    /// Number of output tokens produced
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_tokens: Option<i64>,
    /// Time to first observable model output in milliseconds. Includes text, reasoning, and tool-call output; only available for streaming requests that produce observable output.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_ttft_ms: Option<f64>,
    /// Bucket of AI credits the session had billed before this call, for the adaptive Anthropic prompt-cache policy. Omitted outside the adaptive TTL experiment.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) prompt_cache_credits_bucket: Option<String>,
    /// Why the adaptive Anthropic prompt-cache policy chose this call's TTL: warmup, l1_first, l1_lapsed, l1_hot, g_guard, or ineligible. Omitted outside the adaptive TTL experiment.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) prompt_cache_ttl_reason: Option<String>,
    /// Provider identity and product label captured by the actual call's dispatch path.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<ModelProviderRef>,
    /// GitHub request tracing ID (x-github-request-id header) for server-side log correlation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider_call_id: Option<String>,
    /// Fixed-set configured provider type of the bring-your-own-key model: "openai", "azure", "anthropic", "google", "local_model" (a user-configured local model), "winml" (a Copilot-managed Windows ML model), or "other". Never the caller-supplied provider name. Absent for Copilot-served models.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) provider_type: Option<String>,
    /// Per-quota resource usage snapshots, keyed by quota identifier
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) quota_snapshots: Option<HashMap<String, AssistantUsageQuotaSnapshot>>,
    /// Reasoning effort level used for model calls, if applicable (e.g. "none", "low", "medium", "high", "xhigh", "max")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
    /// Reasoning summary mode used for this model call, if applicable
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Number of output tokens used for reasoning (e.g., chain-of-thought)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_tokens: Option<i64>,
    /// Number of rejected speculative prediction tokens
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rejected_prediction_tokens: Option<i64>,
    /// Serialized (uncompressed) byte length of the request body, before any Content-Encoding. A content-free size signal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_bytes: Option<i64>,
    /// Uncompressed fallback that produced this outcome; absent when the body was sent with the requested encoding
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_fallback: Option<ModelCallRequestBodyCompressionFallback>,
    /// Content-Encoding the request asked for before the uncompressed fallback; present only with requestBodyCompressionFallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_fallback_from: Option<ModelCallRequestBodyEncoding>,
    /// HTTP status that rejected the compressed body before the uncompressed resend; present only when requestBodyCompressionFallback is retried
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_rejected_status: Option<i32>,
    /// Content-Encoding applied to the request body of the attempt that produced this outcome; identity when uncompressed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_encoding: Option<ModelCallRequestBodyEncoding>,
    /// Byte length of the request body actually sent on the wire for the attempt that produced this outcome, after any Content-Encoding. Equals requestBodyBytes when the body was not compressed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_wire_bytes: Option<i64>,
    /// Model identifier returned by the provider, absent when the response does not identify it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub response_model: Option<String>,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rte: Option<bool>,
    /// Copilot service request ID (x-copilot-service-request-id header) for CAPI log correlation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service_request_id: Option<String>,
    /// Number of prior thinking blocks the provider dropped while transforming the request
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) thinking_dropped_blocks: Option<i64>,
    /// Recognized provider-reported reasons for dropped thinking blocks, in response order
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) thinking_dropped_reasons: Option<Vec<String>>,
    /// Time to first token in milliseconds. Only available for streaming requests
    #[serde(skip_serializing_if = "Option::is_none")]
    pub time_to_first_token_ms: Option<f64>,
    /// Tool-call counts keyed by tool name
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) tool_counts: Option<HashMap<String, i64>>,
    /// Number of tokens used by tool definitions for this call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) tool_token_count: Option<i64>,
    /// Transport used for this model call (http or websocket)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transport: Option<AssistantUsageTransport>,
    /// Tool results in the request history before request shaping that answer no earlier unanswered tool call
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) unmatched_tool_result_count: Option<i64>,
    /// Milliseconds spent on the WebSocket attempt before falling back to HTTP
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_after_ms: Option<i64>,
    /// WebSocket close code received before the HTTP fallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_close_code: Option<i32>,
    /// Normalized cause of the WebSocket failure that triggered the HTTP fallback; absent when no fallback occurred or the cause is unknown
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_error_kind: Option<ModelCallWebSocketFallbackErrorKind>,
    /// Why the call was carried by the HTTP fallback of a WebSocket-capable dispatcher; absent when no fallback occurred
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_reason: Option<ModelCallWebSocketFallbackReason>,
    /// Whether a WebSocket reconnect was attempted before falling back to HTTP
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_reconnect_attempted: Option<bool>,
    /// Whether the HTTP fallback started during this model call, rather than being inherited from an earlier call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_started_this_call: Option<bool>,
    /// HTTP status of the WebSocket upgrade rejection or WebSocket API error that triggered the HTTP fallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_status_code: Option<i32>,
}

/// Session event "prompt_cache_break". A detected loss of a previously cached prompt prefix
#[derive(Debug, Clone, Default)]
pub struct PromptCacheBreakData {
    /// Request state whose cached prefix fell short
    #[doc(hidden)]
    pub(crate) after_request: Option<serde_json::Value>,
    /// Name of the sub-agent whose conversation broke, stamped by the parent bridge
    #[doc(hidden)]
    pub(crate) agent_name: Option<String>,
    /// Request state that established the prior cache frontier
    #[doc(hidden)]
    pub(crate) before_request: Option<serde_json::Value>,
    /// Names of the cache-configuration fields that changed
    #[doc(hidden)]
    pub(crate) cache_config_changed_fields: Option<Vec<String>>,
    /// All reasons that contributed to the cache break, ordered by precedence
    pub contributing_reasons: Vec<String>,
    /// Prior cached prompt frontier in tokens
    pub frontier_tokens: i64,
    /// Model that held the prior cache frontier, when the call changed models
    #[doc(hidden)]
    pub(crate) model_from: Option<String>,
    /// Model this call targeted, when the call changed models
    #[doc(hidden)]
    pub(crate) model_to: Option<String>,
    /// The highest-precedence reason for the cache break
    pub primary_reason: String,
    /// Fraction of the prior cache frontier that survived
    pub retention_ratio: f64,
    /// Index of the first conversation message whose content changed
    #[doc(hidden)]
    pub(crate) rewrite_message_index: Option<i64>,
    /// Shape of the history rewrite, for example whether the history grew or shrank
    #[doc(hidden)]
    pub(crate) rewrite_shape: Option<String>,
    /// Subsystems that announced a history rewrite before this call, for example compaction or truncation
    #[doc(hidden)]
    pub(crate) rewrite_source: Option<Vec<String>>,
    /// Cached prefix tokens lost since the prior call
    pub shortfall_tokens: i64,
    /// Number of cached prefix tokens that survived
    pub survived_tokens: i64,
    /// Names of the system-prompt segments whose content changed
    #[doc(hidden)]
    pub(crate) system_segments_changed: Option<Vec<String>>,
    /// Telemetry-safe names of tools added since the prior call
    #[doc(hidden)]
    pub(crate) tools_added: Option<Vec<String>>,
    /// Raw names of tools added since the prior call, restricted because a tool name can be user-authored
    #[doc(hidden)]
    pub(crate) tools_added_raw: Option<Vec<String>>,
    /// Telemetry-safe names of tools whose definition changed since the prior call
    #[doc(hidden)]
    pub(crate) tools_redefined: Option<Vec<String>>,
    /// Changed definition parts of redefined tools, as `tool:part` entries; property-level parts only for telemetry-safe tools, whose other names are hashed
    #[doc(hidden)]
    pub(crate) tools_redefined_parts: Option<Vec<String>>,
    /// Raw names of tools redefined since the prior call, restricted because a tool name can be user-authored
    #[doc(hidden)]
    pub(crate) tools_redefined_raw: Option<Vec<String>>,
    /// Telemetry-safe names of tools removed since the prior call
    #[doc(hidden)]
    pub(crate) tools_removed: Option<Vec<String>>,
    /// Raw names of tools removed since the prior call, restricted because a tool name can be user-authored
    #[doc(hidden)]
    pub(crate) tools_removed_raw: Option<Vec<String>>,
    /// Whether the tool list kept its members but changed their order
    #[doc(hidden)]
    pub(crate) tools_reordered: Option<bool>,
}
crate::record_serde::wire_record!(PromptCacheBreakData { after_request: "afterRequest" if is_none, agent_name: "agentName" if is_none, before_request: "beforeRequest" if is_none, cache_config_changed_fields: "cacheConfigChangedFields" if is_none, contributing_reasons: "contributingReasons", frontier_tokens: "frontierTokens", model_from: "modelFrom" if is_none, model_to: "modelTo" if is_none, primary_reason: "primaryReason", retention_ratio: "retentionRatio", rewrite_message_index: "rewriteMessageIndex" if is_none, rewrite_shape: "rewriteShape" if is_none, rewrite_source: "rewriteSource" if is_none, shortfall_tokens: "shortfallTokens", survived_tokens: "survivedTokens", system_segments_changed: "systemSegmentsChanged" if is_none, tools_added: "toolsAdded" if is_none, tools_added_raw: "toolsAddedRaw" if is_none, tools_redefined: "toolsRedefined" if is_none, tools_redefined_parts: "toolsRedefinedParts" if is_none, tools_redefined_raw: "toolsRedefinedRaw" if is_none, tools_removed: "toolsRemoved" if is_none, tools_removed_raw: "toolsRemovedRaw" if is_none, tools_reordered: "toolsReordered" if is_none });

/// Content-free structural summary of the failing request for diagnosing malformed 4xx calls
#[derive(Debug, Clone, Default)]
pub struct ModelCallFailureRequestFingerprint {
    /// Free-form custom tool calls in the request history before request shaping, counted apart from named function calls (rejected by strict providers)
    pub custom_tool_call_count: Option<i64>,
    /// Summed byte length of opaque or encrypted reasoning payloads
    pub encrypted_content_bytes: Option<i64>,
    /// Summed byte length of inline image payloads (data URLs and base64 sources)
    pub image_bytes: Option<i64>,
    /// Total number of image content parts
    pub image_part_count: i64,
    /// Image parts whose media type cannot be determined (rejected by strict providers)
    pub image_parts_missing_media_type: i64,
    /// Role of the final message in the request
    pub last_message_role: Option<String>,
    /// Total number of messages in the request
    pub message_count: i64,
    /// Tool calls whose name is missing or empty (rejected by strict providers)
    pub nameless_tool_call_count: i64,
    /// Tool calls in the request history before request shaping that no later tool result message answers
    pub orphan_tool_call_count: Option<i64>,
    /// Number of messages carrying opaque or encrypted reasoning
    pub reasoning_item_count: Option<i64>,
    /// Total number of tool calls across assistant messages
    pub tool_call_count: i64,
    /// Number of "tool" result messages in the request
    pub tool_result_message_count: i64,
    /// Tool result messages in the request history before request shaping that answer no earlier unanswered tool call
    pub unmatched_tool_result_count: Option<i64>,
}
crate::record_serde::wire_record!(ModelCallFailureRequestFingerprint { custom_tool_call_count: "customToolCallCount" if is_none, encrypted_content_bytes: "encryptedContentBytes" if is_none, image_bytes: "imageBytes" if is_none, image_part_count: "imagePartCount", image_parts_missing_media_type: "imagePartsMissingMediaType", last_message_role: "lastMessageRole" if is_none, message_count: "messageCount", nameless_tool_call_count: "namelessToolCallCount", orphan_tool_call_count: "orphanToolCallCount" if is_none, reasoning_item_count: "reasoningItemCount" if is_none, tool_call_count: "toolCallCount", tool_result_message_count: "toolResultMessageCount", unmatched_tool_result_count: "unmatchedToolResultCount" if is_none });

/// Session event "model.call_failure". Failed LLM API call metadata for telemetry
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelCallFailureData {
    /// Completion ID from the model provider (e.g., chatcmpl-abc123)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_call_id: Option<String>,
    /// API endpoint used for this model call, matching CAPI supported_endpoints vocabulary
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_endpoint: Option<AssistantUsageApiEndpoint>,
    /// For HTTP 400 failures only: whether the response carried a structured CAPI error envelope (structured_error, a deterministic validation failure) or no error body (bodyless, the transient gateway/proxy signature). Absent for non-400 failures.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bad_request_kind: Option<ModelCallFailureBadRequestKind>,
    /// Where the bring-your-own-key model for the failed call runs and who manages it: "local_managed" (on the device, managed by Copilot), "local_user" (on the device, managed by the user), or "remote_user" (off the device, managed by the user). Absent for Copilot-served models.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byok_kind: Option<String>,
    /// Duration of the failed API call in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<i64>,
    /// The `code` from the CAPI error envelope of an HTTP failure of any status (e.g. 'model_max_prompt_tokens_exceeded' on a 400, 'model_overloaded' on a 5xx) identifying which upstream failure occurred. Raw server-controlled string, emitted only through restricted telemetry. Absent when the response carried no error envelope.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_code: Option<String>,
    /// Raw provider/runtime error message for restricted telemetry
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_message: Option<String>,
    /// The `type` from the CAPI error envelope of an HTTP failure of any status (e.g. 'websocket_error'), a coarser companion to errorCode for envelopes that carry no code. Raw server-controlled string, emitted only through restricted telemetry. Absent when the response carried no error envelope.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_type: Option<String>,
    /// Whether the failure originated from an API response or the request transport
    #[serde(skip_serializing_if = "Option::is_none")]
    pub failure_kind: Option<ModelCallFailureKind>,
    /// Experimental HydraFusion attribution for this failed concrete model call.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fusion: Option<FusionAttribution>,
    /// Why a chat-completions stream that the server ended could not be assembled into a response, as a fixed snake_case reason such as `missing_finish_reason`, `missing_tool_call_id` or `no_chunks`; absent for other failures. New reasons may be added
    #[serde(skip_serializing_if = "Option::is_none")]
    pub incomplete_stream_reason: Option<String>,
    /// Whether this failed dispatched request selected the managed inference gateway. Absent for older producers or when no request was dispatched; not proof of gateway receipt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inference_gateway: Option<bool>,
    /// What initiated this failed API call, when reported (e.g., "sub-agent", "mcp-sampling"); absent for main-loop failures, whether user- or agent-initiated
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initiator: Option<String>,
    /// Internal correlation identifier for the qualifying instruction-update delivery consumed by this failed model request.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) instruction_update_delivery_id: Option<String>,
    /// Event identifier of the latest canonical instruction update included in this failed model request.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) instruction_update_latest_event_id: Option<String>,
    /// Authoritative interaction classification for the failed call, matching `assistant.usage.interactionType` (for example `conversation-agent`, `conversation-subagent`, or `conversation-sampling`). Absent when the producer cannot classify the interaction.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interaction_type: Option<String>,
    /// Whether the session selected Auto mode for the failed call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_auto: Option<bool>,
    /// Whether the failed call used a bring-your-own-key provider
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_byok: Option<bool>,
    /// Effective maximum output-token limit for the failed call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<i64>,
    /// Effective maximum prompt-token limit for the failed call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<i64>,
    /// Model identifier used for the failed API call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    /// Fixed-set provider family serving the bring-your-own-key model for the failed call (for example "openai", "anthropic", "azure_openai", "ollama", "llama_cpp", or "other"). Never the caller-supplied provider name. Absent for Copilot-served models.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
    /// Parent task tool call ID when this failed model call belongs to a sub-agent
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_tool_call_id: Option<String>,
    /// Bounded product-state failure classified before telemetry projection.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) product_state_error: Option<ModelCallProductStateError>,
    /// GitHub request tracing ID (x-github-request-id header) for server-side log correlation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider_call_id: Option<String>,
    /// Fixed-set configured provider type of the bring-your-own-key model for the failed call: "openai", "azure", "anthropic", "google", "local_model" (a user-configured local model), "winml" (a Copilot-managed Windows ML model), or "other". Never the caller-supplied provider name. Absent for Copilot-served models.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) provider_type: Option<String>,
    /// Per-quota usage snapshots parsed from the failed response's quota headers, keyed by quota identifier. Present when the error response carried quota headers (e.g. a 402 once the additional spend limit is reached) so the UI can refresh the quota display on failure.
    #[doc(hidden)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) quota_snapshots: Option<HashMap<String, AssistantUsageQuotaSnapshot>>,
    /// Reasoning effort level used for the failed model call, if applicable
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
    /// Serialized (uncompressed) byte length of the failed request body, before any Content-Encoding. A content-free size signal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_bytes: Option<i64>,
    /// Uncompressed fallback that produced this outcome; absent when the body was sent with the requested encoding
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_fallback: Option<ModelCallRequestBodyCompressionFallback>,
    /// Content-Encoding the request asked for before the uncompressed fallback; present only with requestBodyCompressionFallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_fallback_from: Option<ModelCallRequestBodyEncoding>,
    /// HTTP status that rejected the compressed body before the uncompressed resend; present only when requestBodyCompressionFallback is retried
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_compression_rejected_status: Option<i32>,
    /// Content-Encoding applied to the failed request body; identity when uncompressed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_encoding: Option<ModelCallRequestBodyEncoding>,
    /// Byte length of the failed request body actually sent on the wire, after any Content-Encoding. Equals requestBodyBytes when the body was not compressed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body_wire_bytes: Option<i64>,
    /// Content-free structural summary of the failing request. Contains only counts and shape flags (no prompt content), so it is safe for unrestricted telemetry. Populated only for client-error (4xx) failures.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_fingerprint: Option<ModelCallFailureRequestFingerprint>,
    /// Zero-based orchestrator retry index of the failed attempt
    #[serde(skip_serializing_if = "Option::is_none")]
    pub retry_attempt: Option<i64>,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rte: Option<bool>,
    /// Copilot service request ID (x-copilot-service-request-id header) for CAPI log correlation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service_request_id: Option<String>,
    /// Where the failed model call originated
    pub source: ModelCallFailureSource,
    /// HTTP status code from the failed request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_code: Option<i32>,
    /// Whether the incomplete stream ended with the `[DONE]` sentinel; present only with incompleteStreamReason
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream_done_received: Option<bool>,
    /// Transport used for the failed model call (http or websocket)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transport: Option<ModelCallFailureTransport>,
    /// Milliseconds spent on the WebSocket attempt before falling back to HTTP
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_after_ms: Option<i64>,
    /// WebSocket close code received before the HTTP fallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_close_code: Option<i32>,
    /// Normalized cause of the WebSocket failure that triggered the HTTP fallback; absent when no fallback occurred or the cause is unknown
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_error_kind: Option<ModelCallWebSocketFallbackErrorKind>,
    /// Why the failed call was carried by the HTTP fallback of a WebSocket-capable dispatcher; absent when no fallback occurred
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_reason: Option<ModelCallWebSocketFallbackReason>,
    /// Whether a WebSocket reconnect was attempted before falling back to HTTP
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_reconnect_attempted: Option<bool>,
    /// Whether the HTTP fallback started during this model call, rather than being inherited from an earlier call
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_started_this_call: Option<bool>,
    /// HTTP status of the WebSocket upgrade rejection or WebSocket API error that triggered the HTTP fallback
    #[serde(skip_serializing_if = "Option::is_none")]
    pub websocket_fallback_status_code: Option<i32>,
}

/// Session event "model.call_final_result". Internal telemetry result for one logical model operation after all orchestrator-owned retries settle
#[derive(Debug, Clone, Default)]
pub struct ModelCallFinalResultData {
    /// Whether the final attempt used a bring-your-own-key provider
    pub is_byok: Option<bool>,
    /// Model identifier used by the final attempt
    pub model: String,
    /// Bounded result of the final attempt
    pub result: ModelCallFinalResult,
}
crate::record_serde::wire_record!(ModelCallFinalResultData { is_byok: "isByok" if is_none, model: "model", result: "result" });

/// Session event "model.call_finished". Final lifecycle outcome for one logical model dispatch. A logical dispatch may include internal reconnect or fallback work, so event count is not provider HTTP-request count.
#[derive(Debug, Clone, Default)]
pub struct ModelCallFinishedData {
    /// Whether an accepted successful response requested the exact name and command semantics of a built-in file edit tool, including an external tool explicitly replacing that built-in name. Absent when the logical dispatch did not produce an accepted response.
    pub contains_built_in_file_edit_request: Option<bool>,
    /// Monotonic elapsed time spent in the logical model dispatch, including any internal transport reconnect or fallback and excluding orchestrator retry backoff, tool execution, confirmations, and post-response processing
    pub dispatch_duration_ms: f64,
    /// Version of the built-in file-edit semantic classifier used for this event
    pub edit_classifier_version: i64,
    /// Identifier of the user interaction that owns the model dispatch, matching assistant.turn_start.interactionId when available
    pub interaction_id: Option<String>,
    /// Final outcome after post-response acceptance processing
    pub outcome: ModelCallFinishedOutcome,
    /// Agent-loop iteration within the interaction that initiated the model dispatch
    pub turn_id: String,
}
crate::record_serde::wire_record!(ModelCallFinishedData { contains_built_in_file_edit_request: "containsBuiltInFileEditRequest" if is_none, dispatch_duration_ms: "dispatchDurationMs", edit_classifier_version: "editClassifierVersion", interaction_id: "interactionId" if is_none, outcome: "outcome", turn_id: "turnId" });

/// Session event "model.call_start". Model API dispatch metadata for internal telemetry
#[derive(Debug, Clone, Default)]
pub struct ModelCallStartData {
    /// Experimental HydraFusion attribution for this concrete model call.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Model identifier used for this API call, when known
    pub model: Option<String>,
    /// Parent task tool call ID when this model call belongs to a sub-agent
    pub parent_tool_call_id: Option<String>,
    /// Previous response or interaction identifier included in the model request, when present
    #[doc(hidden)]
    pub(crate) previous_response_id: Option<String>,
    /// Identifier of the assistant turn that initiated the model call
    pub turn_id: String,
}
crate::record_serde::wire_record!(ModelCallStartData { fusion: "fusion" if is_none, model: "model" if is_none, parent_tool_call_id: "parentToolCallId" if is_none, previous_response_id: "previousResponseId" if is_none, turn_id: "turnId" });

/// Session event "abort". Turn abort information including the reason for termination
#[derive(Debug, Clone, Default)]
pub struct AbortData {
    /// Finite reason code describing why the current turn was aborted
    pub reason: AbortReason,
}
crate::record_serde::wire_record!(AbortData { reason: "reason" });

/// Session event "tool.user_requested". User-initiated tool invocation request with tool name and arguments
#[derive(Debug, Clone, Default)]
pub struct ToolUserRequestedData {
    /// Arguments for the tool invocation
    pub arguments: Option<serde_json::Value>,
    /// Unique identifier for this tool call
    pub tool_call_id: String,
    /// Name of the tool the user wants to invoke
    pub tool_name: String,
}
crate::record_serde::wire_record!(ToolUserRequestedData { arguments: "arguments" if is_none, tool_call_id: "toolCallId", tool_name: "toolName" });

/// Shell-aware path hints for a shell tool's command, captured at start time so consumers can snapshot a file's pre-image before the tool runs.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionStartShellToolInfo {
    /// The command with a redundant leading `cd` into the working directory removed, present only when there was one to remove. Computed with the same routine the shell driver applies before spawning, so a surface that renders this shows the text that actually runs. Consumers that display it should keep the original tool arguments available on demand.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub display_command: Option<String>,
    /// Whether the command includes a file write redirection (e.g., > or >>).
    pub has_write_file_redirection: bool,
    /// File paths the command may read or write, derived from the command at start time. Produced by the same shell-aware extractor as PermissionRequestShell.possiblePaths, so it is present even when the command is auto-approved and no permission request fires.
    pub possible_paths: Vec<String>,
}
crate::record_serde::wire_record!(ToolExecutionStartShellToolInfo { display_command: "displayCommand" if is_none, has_write_file_redirection: "hasWriteFileRedirection", possible_paths: "possiblePaths" });

/// MCP Apps tool `_meta.ui` resource URI and visibility captured on `tool.execution_start`.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionStartToolDescriptionMetaUI {
    /// URI of the UI resource
    pub resource_uri: Option<String>,
    /// Who can access this tool
    pub visibility: Option<Vec<ToolExecutionStartToolDescriptionMetaUIVisibility>>,
}
crate::record_serde::wire_record!(ToolExecutionStartToolDescriptionMetaUI { resource_uri: "resourceUri" if is_none, visibility: "visibility" if is_none });

/// MCP Apps metadata for UI resource association
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionStartToolDescriptionMeta {
    /// MCP Apps tool `_meta.ui` resource URI and visibility captured on `tool.execution_start`.
    pub ui: Option<ToolExecutionStartToolDescriptionMetaUI>,
}
crate::record_serde::wire_record!(ToolExecutionStartToolDescriptionMeta { ui: "ui" if is_none });

/// Tool definition metadata, present for MCP tools with MCP Apps support
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionStartToolDescription {
    /// MCP Apps metadata for UI resource association
    pub meta: Option<ToolExecutionStartToolDescriptionMeta>,
    /// Tool description
    pub description: Option<String>,
    /// Tool name
    pub name: String,
}
crate::record_serde::wire_record!(ToolExecutionStartToolDescription { meta: "_meta" if is_none, description: "description" if is_none, name: "name" });

/// Session event "tool.execution_start". Tool execution startup details including MCP server information when applicable
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionStartData {
    /// Arguments passed to the tool
    pub arguments: Option<serde_json::Value>,
    /// When true, the tool output should be displayed expanded (verbatim) in the CLI timeline
    pub display_verbatim: Option<bool>,
    /// Experimental HydraFusion attribution for this tool execution.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Configured MCP server key, retained for compatibility. New events write the same value as mcpServerName; on legacy events this field distinguishes the configured key from the display name in mcpServerName. Unrestricted provenance telemetry hashes this key to join with mcp_server_setup.
    pub mcp_config_server_name: Option<String>,
    /// Where the MCP server's configuration came from (`user`, `workspace`, `plugin`, or `builtin`), when the tool is an MCP tool and the server is configured
    pub mcp_config_source: Option<McpServerSource>,
    /// Explicitly configured MCP server display name.
    pub mcp_server_display_name: Option<String>,
    /// Configured key of the MCP server hosting this tool. Legacy events may contain the display name instead; prefer mcpConfigServerName for their identity when available.
    pub mcp_server_name: Option<String>,
    /// Original tool name on the MCP server, when the tool is an MCP tool
    pub mcp_tool_name: Option<String>,
    /// Transport the MCP server hosting this tool is connected over, when the tool is an MCP tool and the server is configured
    pub mcp_transport: Option<McpServerTransport>,
    /// Model identifier that generated this tool call
    pub model: Option<String>,
    /// Whether the selected tool descriptor explicitly classifies this call as read-only.
    pub read_only: Option<bool>,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    pub rte: Option<bool>,
    /// Shell-tool path hints derived from the command at start time for shell tools (bash/powershell/local_shell). Produced by the same shell-aware extractor as PermissionRequestShell.possiblePaths, so it is present even when the command is auto-approved and no permission request fires. Absent for non-shell tools.
    pub shell_tool_info: Option<ToolExecutionStartShellToolInfo>,
    /// Unique identifier for this tool call
    pub tool_call_id: String,
    /// Tool definition metadata, present for MCP tools with MCP Apps support
    pub tool_description: Option<ToolExecutionStartToolDescription>,
    /// Name of the tool being executed
    pub tool_name: String,
    /// Human-readable display title for the tool, when the selected tool descriptor has a non-empty title.
    pub tool_title: Option<String>,
    /// Identifier for the agent loop turn this tool was invoked in, matching the corresponding assistant.turn_start event
    pub turn_id: Option<String>,
}
crate::record_serde::wire_record!(ToolExecutionStartData { arguments: "arguments" if is_none, display_verbatim: "displayVerbatim" if is_none, fusion: "fusion" if is_none, mcp_config_server_name: "mcpConfigServerName" if is_none, mcp_config_source: "mcpConfigSource" if is_none, mcp_server_display_name: "mcpServerDisplayName" if is_none, mcp_server_name: "mcpServerName" if is_none, mcp_tool_name: "mcpToolName" if is_none, mcp_transport: "mcpTransport" if is_none, model: "model" if is_none, read_only: "readOnly" if is_none, rte: "rte" if is_none, shell_tool_info: "shellToolInfo" if is_none, tool_call_id: "toolCallId", tool_description: "toolDescription" if is_none, tool_name: "toolName", tool_title: "toolTitle" if is_none, turn_id: "turnId" if is_none });

/// Session event "tool.shell_output". Live, append-only shell output. Not persisted or replayed to late subscribers. Text is decoded and redacted per chunk; chunks need not contain complete lines.
#[derive(Debug, Clone, Default)]
pub struct ToolShellOutputData {
    /// Experimental HydraFusion attribution of the staged phase whose tool call produced this output. Absent outside a staged phase.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// Zero-based publication sequence across all output streams for this tool call. Not a byte offset or an OS write-order guarantee.
    pub sequence: i64,
    /// Output source. Omission means stdout. Terminal output has no separate stdout/stderr attribution.
    pub stream: Option<ToolShellOutputStream>,
    /// New output to append, without synthetic shell-result markers or stream-switch separators
    pub text: String,
    /// Tool call ID that owns this shell output
    pub tool_call_id: String,
}
crate::record_serde::wire_record!(ToolShellOutputData { fusion: "fusion" if is_none, sequence: "sequence", stream: "stream" if is_none, text: "text", tool_call_id: "toolCallId" });

/// Session event "tool.execution_progress". Tool execution progress notification with status message
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionProgressData {
    /// Human-readable progress status message (e.g., from an MCP server)
    pub progress_message: String,
    /// Client-only structured progress metadata. Not model-facing tool output.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub structured_content: Option<serde_json::Value>,
    /// Tool call ID this progress notification belongs to
    pub tool_call_id: String,
}
crate::record_serde::wire_record!(ToolExecutionProgressData { progress_message: "progressMessage", structured_content: "structuredContent" if is_none, tool_call_id: "toolCallId" });

/// Error details when the tool execution failed
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteError {
    /// Machine-readable error code
    pub code: Option<String>,
    /// Human-readable error message
    pub message: String,
    /// What the user must do to recover, when the runtime knows of an action. Set on sandbox policy denials, where `message` names the rule that blocked the call but never the client affordance that relaxes it.
    pub remediation: Option<RemediationAction>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteError { code: "code" if is_none, message: "message", remediation: "remediation" if is_none });

/// A file mutation that was actually committed by a built-in file editing tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteFileEdit {
    /// Kind of mutation committed at this path.
    pub kind: ToolExecutionCompleteFileEditKind,
    /// Absolute path in the session filesystem namespace.
    pub path: String,
}
crate::record_serde::wire_record!(ToolExecutionCompleteFileEdit {
    kind: "kind",
    path: "path"
});

/// Binary result returned by a tool for the model
#[derive(Debug, Clone, Default)]
pub struct PersistedBinaryImage {
    /// Base64-encoded binary data
    pub data: String,
    /// Human-readable description of the binary data
    pub description: Option<String>,
    /// Optional metadata from the producing tool.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// MIME type of the binary data
    pub mime_type: String,
    /// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
    pub r#type: PersistedBinaryImageType,
}
crate::record_serde::wire_record!(PersistedBinaryImage { data: "data", description: "description" if is_none, metadata: "metadata" if is_none, mime_type: "mimeType", r#type: "type" });

/// A binary result whose data was omitted from persistence due to the inline size limit
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct OmittedBinaryResult {
    /// Decoded byte length of the omitted binary data
    pub byte_length: i64,
    /// Human-readable description of the binary data
    pub description: Option<String>,
    /// Optional metadata from the producing tool.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// MIME type of the omitted binary data
    pub mime_type: String,
    /// Why the binary data is absent: it exceeded the inline size limit, or its asset was unavailable
    pub omitted_reason: OmittedBinaryOmittedReason,
    /// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
    pub r#type: OmittedBinaryType,
}
crate::record_serde::wire_record!(OmittedBinaryResult { byte_length: "byteLength", description: "description" if is_none, metadata: "metadata" if is_none, mime_type: "mimeType", omitted_reason: "omittedReason", r#type: "type" });

/// A reference to binary data persisted once on a session.binary_asset event and shared by id
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct BinaryAssetReference {
    /// Content-addressed id of the session.binary_asset event that holds this binary's bytes (e.g. "sha256:...").
    pub asset_id: String,
    /// Decoded byte length of the referenced binary data
    pub byte_length: i64,
    /// Human-readable description of the binary data
    pub description: Option<String>,
    /// Optional metadata from the producing tool.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// MIME type of the referenced binary data
    pub mime_type: String,
    /// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
    pub r#type: BinaryAssetReferenceType,
}
crate::record_serde::wire_record!(BinaryAssetReference { asset_id: "assetId", byte_length: "byteLength", description: "description" if is_none, metadata: "metadata" if is_none, mime_type: "mimeType", r#type: "type" });

/// A source supplied by a tool that should be made available to the model as citable content.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CitableSource {
    /// The source text made available to the model as citable content.
    pub content: String,
    /// Stable identifier for this source within the tool result. Used for deduplication and may be used by future provider integrations to correlate response citations back to the originating source.
    pub id: String,
    /// File path relative to the agent's workspace root, when the source is a file.
    pub path: Option<String>,
    /// Human-readable title of the source.
    pub title: Option<String>,
    /// URL of the source, when it is a web resource.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(CitableSource { content: "content", id: "id", path: "path" if is_none, title: "title" if is_none, url: "url" if is_none });

/// Plain text content block
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentText {
    /// The text content
    pub text: String,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentTextType,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentText {
    text: "text",
    r#type: "type"
});

/// Shell command exit metadata with optional output preview
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentShellExit {
    /// Working directory where the shell command was executed
    pub cwd: Option<String>,
    /// Exit code from the completed shell command
    pub exit_code: i64,
    /// Path reported in the shell session's filesystem namespace when shell output exceeded the configured large-output threshold.
    pub output_file_path: Option<String>,
    /// Output associated with this shell command, if available. May be partial, truncated, or a preview; not guaranteed to be full output.
    pub output_preview: Option<String>,
    /// Whether outputPreview is known to be incomplete or truncated
    pub output_truncated: Option<bool>,
    /// Shell id, as assigned by Copilot runtime
    pub shell_id: String,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentShellExitType,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentShellExit { cwd: "cwd" if is_none, exit_code: "exitCode", output_file_path: "outputFilePath" if is_none, output_preview: "outputPreview" if is_none, output_truncated: "outputTruncated" if is_none, shell_id: "shellId", r#type: "type" });

/// Image content block with base64-encoded data
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentImage {
    /// Base64-encoded image data
    pub data: String,
    /// MIME type of the image (e.g., image/png, image/jpeg)
    pub mime_type: String,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentImageType,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentImage {
    data: "data",
    mime_type: "mimeType",
    r#type: "type"
});

/// Audio content block with base64-encoded data
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentAudio {
    /// Base64-encoded audio data
    pub data: String,
    /// MIME type of the audio (e.g., audio/wav, audio/mpeg)
    pub mime_type: String,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentAudioType,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentAudio {
    data: "data",
    mime_type: "mimeType",
    r#type: "type"
});

/// Icon image for a resource
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentResourceLinkIcon {
    /// MIME type of the icon image
    pub mime_type: Option<String>,
    /// Available icon sizes (e.g., ['16x16', '32x32'])
    pub sizes: Option<Vec<String>>,
    /// URL or path to the icon image
    pub src: String,
    /// Theme variant this icon is intended for
    pub theme: Option<ToolExecutionCompleteContentResourceLinkIconTheme>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentResourceLinkIcon { mime_type: "mimeType" if is_none, sizes: "sizes" if is_none, src: "src", theme: "theme" if is_none });

/// Resource link content block referencing an external resource
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteContentResourceLink {
    /// Human-readable description of the resource
    pub description: Option<String>,
    /// Icons associated with this resource
    pub icons: Option<Vec<ToolExecutionCompleteContentResourceLinkIcon>>,
    /// MIME type of the resource content
    pub mime_type: Option<String>,
    /// Resource name identifier
    pub name: String,
    /// Size of the resource in bytes
    pub size: Option<i64>,
    /// Human-readable display title for the resource
    pub title: Option<String>,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentResourceLinkType,
    /// URI identifying the resource
    pub uri: String,
}
crate::record_serde::wire_record!(ToolExecutionCompleteContentResourceLink { description: "description" if is_none, icons: "icons" if is_none, mime_type: "mimeType" if is_none, name: "name", size: "size" if is_none, title: "title" if is_none, r#type: "type", uri: "uri" });

/// Embedded text resource contents identified by a URI, with an optional MIME type and a text payload.
#[derive(Debug, Clone, Default)]
pub struct EmbeddedTextResourceContents {
    /// MIME type of the text content
    pub mime_type: Option<String>,
    /// Text content of the resource
    pub text: String,
    /// URI identifying the resource
    pub uri: String,
}
crate::record_serde::wire_record!(EmbeddedTextResourceContents { mime_type: "mimeType" if is_none, text: "text", uri: "uri" });

/// Embedded binary resource contents identified by a URI, with an optional MIME type and a base64-encoded blob.
#[derive(Debug, Clone, Default)]
pub struct EmbeddedBlobResourceContents {
    /// Base64-encoded binary content of the resource
    pub blob: String,
    /// MIME type of the blob content
    pub mime_type: Option<String>,
    /// URI identifying the resource
    pub uri: String,
}
crate::record_serde::wire_record!(EmbeddedBlobResourceContents { blob: "blob", mime_type: "mimeType" if is_none, uri: "uri" });

/// Embedded resource content block with inline text or binary data
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolExecutionCompleteContentResource {
    /// The embedded resource contents, either text or base64-encoded binary
    pub resource: ToolExecutionCompleteContentResourceDetails,
    /// Content block type discriminator
    pub r#type: ToolExecutionCompleteContentResourceType,
}

/// CSP domain allowlists for an MCP Apps UI resource, including connect, resource, frame, and base URI domains.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteUIResourceMetaUICsp {
    /// Domains the UI resource may use as document base URIs.
    pub base_uri_domains: Option<Vec<String>>,
    /// Domains the UI resource may connect to.
    pub connect_domains: Option<Vec<String>>,
    /// Domains the UI resource may embed as nested frames.
    pub frame_domains: Option<Vec<String>>,
    /// Domains from which the UI resource may load scripts, styles, images, and other resources.
    pub resource_domains: Option<Vec<String>>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteUIResourceMetaUICsp { base_uri_domains: "baseUriDomains" if is_none, connect_domains: "connectDomains" if is_none, frame_domains: "frameDomains" if is_none, resource_domains: "resourceDomains" if is_none });

/// Marker object for camera permission on an MCP Apps UI resource.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolExecutionCompleteUIResourceMetaUIPermissionsCamera {}

/// Marker object for clipboard-write permission on an MCP Apps UI resource.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolExecutionCompleteUIResourceMetaUIPermissionsClipboardWrite {}

/// Marker object for geolocation permission on an MCP Apps UI resource.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolExecutionCompleteUIResourceMetaUIPermissionsGeolocation {}

/// Marker object for microphone permission on an MCP Apps UI resource.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolExecutionCompleteUIResourceMetaUIPermissionsMicrophone {}

/// Browser permission metadata for an MCP Apps UI resource, including camera, microphone, geolocation, and clipboard-write.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteUIResourceMetaUIPermissions {
    /// Marker object for camera permission on an MCP Apps UI resource.
    pub camera: Option<ToolExecutionCompleteUIResourceMetaUIPermissionsCamera>,
    /// Marker object for clipboard-write permission on an MCP Apps UI resource.
    pub clipboard_write: Option<ToolExecutionCompleteUIResourceMetaUIPermissionsClipboardWrite>,
    /// Marker object for geolocation permission on an MCP Apps UI resource.
    pub geolocation: Option<ToolExecutionCompleteUIResourceMetaUIPermissionsGeolocation>,
    /// Marker object for microphone permission on an MCP Apps UI resource.
    pub microphone: Option<ToolExecutionCompleteUIResourceMetaUIPermissionsMicrophone>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteUIResourceMetaUIPermissions { camera: "camera" if is_none, clipboard_write: "clipboardWrite" if is_none, geolocation: "geolocation" if is_none, microphone: "microphone" if is_none });

/// MCP Apps UI resource metadata for a completed tool result, including CSP, permissions, domain, and border preference.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteUIResourceMetaUI {
    /// CSP domain allowlists for an MCP Apps UI resource, including connect, resource, frame, and base URI domains.
    pub csp: Option<ToolExecutionCompleteUIResourceMetaUICsp>,
    /// Optional dedicated origin for the rendered MCP Apps UI resource.
    pub domain: Option<String>,
    /// Browser permission metadata for an MCP Apps UI resource, including camera, microphone, geolocation, and clipboard-write.
    pub permissions: Option<ToolExecutionCompleteUIResourceMetaUIPermissions>,
    /// Whether the host should render a border around the MCP Apps UI resource.
    pub prefers_border: Option<bool>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteUIResourceMetaUI { csp: "csp" if is_none, domain: "domain" if is_none, permissions: "permissions" if is_none, prefers_border: "prefersBorder" if is_none });

/// Resource-level UI metadata (CSP, permissions, visual preferences)
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteUIResourceMeta {
    /// MCP Apps UI resource metadata for a completed tool result, including CSP, permissions, domain, and border preference.
    pub ui: Option<ToolExecutionCompleteUIResourceMetaUI>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteUIResourceMeta { ui: "ui" if is_none });

/// MCP Apps UI resource content for rendering in a sandboxed iframe
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteUIResource {
    /// Resource-level UI metadata (CSP, permissions, visual preferences)
    pub meta: Option<ToolExecutionCompleteUIResourceMeta>,
    /// Base64-encoded HTML content
    pub blob: Option<String>,
    /// MIME type of the content
    pub mime_type: String,
    /// HTML content as a string
    pub text: Option<String>,
    /// The ui:// URI of the resource
    pub uri: String,
}
crate::record_serde::wire_record!(ToolExecutionCompleteUIResource { meta: "_meta" if is_none, blob: "blob" if is_none, mime_type: "mimeType", text: "text" if is_none, uri: "uri" });

/// Tool execution result on success
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteResult {
    /// Model-facing binary results (base64 inline or size-omitted markers) sent to the LLM for this tool call
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub binary_results_for_llm: Option<Vec<PersistedBinaryResult>>,
    /// Provider-neutral source material this tool makes available to the model as citable content. Persisted so it survives session resume. Experimental.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub citable_sources: Option<Vec<CitableSource>>,
    /// Concise tool result text sent to the LLM for chat completion, potentially truncated for token efficiency
    pub content: String,
    /// Structured content blocks (text, images, audio, resources) returned by the tool in their native format
    pub contents: Option<Vec<ToolExecutionCompleteContent>>,
    /// Detailed tool result for UI/timeline display, preserving complete content such as diffs for most tools. Successful skill invocations intentionally use the concise model-facing content here; the authoritative skill body is carried by the corresponding skill invocation event. Falls back to content when absent.
    pub detailed_content: Option<String>,
    /// FIDES IFC label projected from tool ingress metadata (MCP `CallToolResult._meta` or synthesized built-in ingress labels) — persisted as `{ ifc: ... }` (only the `ifc` key, not the whole `_meta`). Persisted so the FIDES IFC label survives session resume: the engine rehydrates accumulated taint by replaying these on load. Populated for ingress sources when FIDES IFC is on. Experimental.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub mcp_meta: Option<serde_json::Value>,
    /// MCP `CallToolResult._meta` returned verbatim by an MCP Apps tool, for delivery to the app UI only; never sent to the model. Experimental.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub mcp_result_meta: Option<HashMap<String, serde_json::Value>>,
    /// Structured content (arbitrary JSON) returned verbatim by the MCP tool
    pub structured_content: Option<serde_json::Value>,
    /// Tool names discovered by client-side tool search, preserved so replay restores the original tool-reference content. References do not grant execution permission.
    pub tool_references: Option<Vec<String>>,
    /// MCP Apps UI resource content for rendering in a sandboxed iframe
    pub ui_resource: Option<ToolExecutionCompleteUIResource>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteResult { binary_results_for_llm: "binaryResultsForLlm" if is_none, citable_sources: "citableSources" if is_none, content: "content", contents: "contents" if is_none, detailed_content: "detailedContent" if is_none, mcp_meta: "mcpMeta" if is_none, mcp_result_meta: "mcpResultMeta" if is_none, structured_content: "structuredContent" if is_none, tool_references: "toolReferences" if is_none, ui_resource: "uiResource" if is_none });

/// Experimental shell completion facts retained independently of the full tool result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteShellExecution {
    /// Process exit code reported by the shell driver.
    pub exit_code: i64,
}
crate::record_serde::wire_record!(ToolExecutionCompleteShellExecution {
    exit_code: "exitCode"
});

/// MCP Apps tool `_meta.ui` resource URI and visibility captured on `tool.execution_complete`.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteToolDescriptionMetaUI {
    /// URI of the UI resource
    pub resource_uri: Option<String>,
    /// Who can access this tool
    pub visibility: Option<Vec<ToolExecutionCompleteToolDescriptionMetaUIVisibility>>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteToolDescriptionMetaUI { resource_uri: "resourceUri" if is_none, visibility: "visibility" if is_none });

/// MCP Apps metadata for UI resource association
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteToolDescriptionMeta {
    /// MCP Apps tool `_meta.ui` resource URI and visibility captured on `tool.execution_complete`.
    pub ui: Option<ToolExecutionCompleteToolDescriptionMetaUI>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteToolDescriptionMeta { ui: "ui" if is_none });

/// Tool definition metadata, present for MCP tools with MCP Apps support
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteToolDescription {
    /// MCP Apps metadata for UI resource association
    pub meta: Option<ToolExecutionCompleteToolDescriptionMeta>,
    /// Tool description
    pub description: Option<String>,
    /// Tool name
    pub name: String,
}
crate::record_serde::wire_record!(ToolExecutionCompleteToolDescription { meta: "_meta" if is_none, description: "description" if is_none, name: "name" });

/// Provider-reported image and text token counts, which are subsets of the corresponding total rather than additional tokens.
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionUsageTokenDetails {
    /// Image tokens included in the corresponding input or output total.
    pub image_tokens: i64,
    /// Text tokens included in the corresponding input or output total.
    pub text_tokens: i64,
}
crate::record_serde::wire_record!(ToolExecutionUsageTokenDetails {
    image_tokens: "imageTokens",
    text_tokens: "textTokens"
});

/// Authoritative usage reported by a separately metered runtime-owned tool operation. May be present on a failed tool when the service reported usage before local result processing failed. Missing token counts or billing mean unavailable, not zero.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionUsage {
    /// Runtime-owned accounting order, preserved across session checkpoints and replay.
    pub accounting: Option<UsageAccountingIdentity>,
    /// Server-reported charge. Divide totalNanoAiu by 1,000,000,000 for AI credits; do not calculate customer cost from token prices.
    pub copilot_usage: Option<AssistantUsageCopilotUsage>,
    /// Total provider-reported input tokens across all modalities.
    pub input_tokens: Option<i64>,
    /// Optional input modality breakdown; these counts are already included in inputTokens.
    pub input_tokens_details: Option<ToolExecutionUsageTokenDetails>,
    /// Requested model for the tool operation, independent of the conversation model. Not a provider-confirmed serving model.
    pub model: String,
    /// Total provider-reported output tokens across all modalities.
    pub output_tokens: Option<i64>,
    /// Optional output modality breakdown; these counts are already included in outputTokens.
    pub output_tokens_details: Option<ToolExecutionUsageTokenDetails>,
    /// Provider responsible for the tool operation, captured independently of the conversation provider.
    pub provider: Option<ModelProviderRef>,
    /// Account quota snapshots, not per-operation cost deltas.
    #[doc(hidden)]
    pub(crate) quota_snapshots: Option<HashMap<String, serde_json::Value>>,
    /// Runtime-generated identifier for this metered operation. Deduplicates repeated delivery or replay; not an upstream billing identifier.
    pub request_id: RequestId,
    /// Provider-reported sum of input and output tokens.
    pub total_tokens: Option<i64>,
}
crate::record_serde::wire_record!(ToolExecutionUsage { accounting: "accounting" if is_none, copilot_usage: "copilotUsage" if is_none, input_tokens: "inputTokens" if is_none, input_tokens_details: "inputTokensDetails" if is_none, model: "model", output_tokens: "outputTokens" if is_none, output_tokens_details: "outputTokensDetails" if is_none, provider: "provider" if is_none, quota_snapshots: "quotaSnapshots" if is_none, request_id: "requestId", total_tokens: "totalTokens" if is_none });

/// Session event "tool.execution_complete". Tool execution completion results including success status, detailed output, and error information
#[derive(Debug, Clone, Default)]
pub struct ToolExecutionCompleteData {
    /// IDs of accepted assistant messages whose output this tool result consumed, referring to assistant.message.data.messageId across the agent tree. Captured from the returned snapshot, including partial agent reads. An empty list means no model-produced dependency; absence means provenance is unavailable.
    pub causal_dependencies: Option<Vec<String>>,
    /// Error details when the tool execution failed
    pub error: Option<ToolExecutionCompleteError>,
    /// Experimental. File mutations actually committed by a built-in file editing tool, in execution order. Present on successful edits and on partial failures when earlier mutations were committed. Paths are absolute in the session filesystem namespace.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub file_edits: Option<Vec<ToolExecutionCompleteFileEdit>>,
    /// Experimental HydraFusion attribution for this tool completion.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fusion: Option<FusionAttribution>,
    /// CAPI interaction ID for correlating this tool execution with upstream telemetry
    pub interaction_id: Option<String>,
    /// Whether this tool call was explicitly requested by the user rather than the assistant
    pub is_user_requested: Option<bool>,
    /// FIDES IFC label projected from tool ingress metadata (MCP `CallToolResult._meta` or synthesized built-in ingress labels). Persisted as `{ ifc: ... }` so the label survives session resume, including model-visible failure results. Experimental.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub mcp_meta: Option<serde_json::Value>,
    /// Model identifier that generated this tool call
    pub model: Option<String>,
    /// Tool execution result on success
    pub result: Option<ToolExecutionCompleteResult>,
    /// Per-request treatment/eligibility signal returned by the Copilot API in the `X-GitHub-Copilot-Request-TE` response header for the associated model call; `false` when the header was absent or unparseable.
    pub rte: Option<bool>,
    /// Whether this tool execution ran inside a sandbox container
    pub sandboxed: Option<bool>,
    /// Experimental shell completion facts captured before the persisted result contents are stripped.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub shell_execution: Option<ToolExecutionCompleteShellExecution>,
    /// Whether the tool execution completed successfully
    pub success: bool,
    /// Unique identifier for the completed tool call
    pub tool_call_id: String,
    /// Tool definition metadata, present for MCP tools with MCP Apps support
    pub tool_description: Option<ToolExecutionCompleteToolDescription>,
    /// Tool-specific telemetry data (e.g., CodeQL check counts, grep match counts)
    pub tool_telemetry: Option<HashMap<String, serde_json::Value>>,
    /// Provider-reported usage of a separately metered runtime-owned tool operation, independent of the conversation model. Present on success or failure when usage is known. Experimental.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub tool_usage: Option<Box<ToolExecutionUsage>>,
    /// Identifier for the agent loop turn this tool was invoked in, matching the corresponding assistant.turn_start event
    pub turn_id: Option<String>,
}
crate::record_serde::wire_record!(ToolExecutionCompleteData { causal_dependencies: "causalDependencies" if is_none, error: "error" if is_none, file_edits: "fileEdits" if is_none, fusion: "fusion" if is_none, interaction_id: "interactionId" if is_none, is_user_requested: "isUserRequested" if is_none, mcp_meta: "mcpMeta" if is_none, model: "model" if is_none, result: "result" if is_none, rte: "rte" if is_none, sandboxed: "sandboxed" if is_none, shell_execution: "shellExecution" if is_none, success: "success", tool_call_id: "toolCallId", tool_description: "toolDescription" if is_none, tool_telemetry: "toolTelemetry" if is_none, tool_usage: "toolUsage" if is_none, turn_id: "turnId" if is_none });

/// Session event "tool_search.activated". Persisted generic client-side tool activations restored when a session resumes.
#[derive(Debug, Clone, Default)]
pub struct ToolSearchActivatedData {
    /// Tool-search strategy that activated the definitions.
    pub strategy: String,
    /// Names of tool definitions activated by this search invocation.
    pub tool_names: Vec<String>,
}
crate::record_serde::wire_record!(ToolSearchActivatedData {
    strategy: "strategy",
    tool_names: "toolNames"
});

/// Session event "skill.invoked". Skill invocation details including content, allowed tools, and plugin metadata
#[derive(Debug, Clone, Default)]
pub struct SkillInvokedData {
    /// Tool names that should be auto-approved when this skill is active
    pub allowed_tools: Option<Vec<String>>,
    /// Exact invocation arguments when recorded, retained separately from the reusable skill body
    pub arguments: Option<String>,
    /// Full content of the skill file, injected into the conversation for the model
    pub content: String,
    /// Description of the skill from its SKILL.md frontmatter
    pub description: Option<String>,
    /// Whether model invocation is disabled for this skill
    pub disable_model_invocation: Option<bool>,
    /// Projected chat-message count when the skill was invoked. New writers persist this so replay does not need to reconstruct superseded history; readers derive it for legacy events when absent.
    pub invoked_at_turn: Option<i64>,
    /// MCP resource metadata associated with the verified skill content.
    pub mcp_meta: Option<serde_json::Value>,
    /// Held MCP skill origin and manifest. Remote execution restrictions survive replay while this provenance remains in the session.
    pub mcp_origin: Option<serde_json::Value>,
    /// Model identifier active when the skill was invoked, when known
    pub model: Option<String>,
    /// Name of the invoked skill
    pub name: String,
    /// File path to the SKILL.md definition, or an empty string for an SDK-provided skill without a filesystem identity
    pub path: String,
    /// Name of the plugin this skill originated from, when applicable
    pub plugin_name: Option<String>,
    /// Version of the plugin this skill originated from, when applicable
    pub plugin_version: Option<String>,
    /// Source identifier for where the skill was discovered. Known values include: project (workspace skill), inherited (parent-directory skill), personal-copilot (~/.copilot/skills), personal-agents (~/.agents/skills), custom (configured directory), plugin (installed plugin), builtin (bundled runtime skill), remote (org/enterprise skill), and sdk (SDK-provided skill)
    pub source: Option<String>,
    /// What triggered the skill invocation: `user-invoked` (explicit user action, such as via a slash command or UI affordance), `agent-invoked` (agent requested the skill), or `context-load` (loaded as part of another context, such as preloading skills configured on a custom agent or subagent)
    pub trigger: Option<SkillInvokedTrigger>,
}
crate::record_serde::wire_record!(SkillInvokedData { allowed_tools: "allowedTools" if is_none, arguments: "arguments" if is_none, content: "content", description: "description" if is_none, disable_model_invocation: "disableModelInvocation" if is_none, invoked_at_turn: "invokedAtTurn" if is_none, mcp_meta: "mcpMeta" if is_none, mcp_origin: "mcpOrigin" if is_none, model: "model" if is_none, name: "name", path: "path", plugin_name: "pluginName" if is_none, plugin_version: "pluginVersion" if is_none, source: "source" if is_none, trigger: "trigger" if is_none });

/// Session event "skill.invoked_ref". Internal durable skill invocation receipt whose content resolves from an earlier inline skill event in the same session.
#[derive(Debug, Clone, Default)]
pub struct SkillInvokedRefData {
    /// Tool names that should be auto-approved when this skill is active
    pub allowed_tools: Option<Vec<String>>,
    /// Exact invocation arguments preserved from this invocation's inline event data, not from the referenced reusable body
    pub arguments: Option<String>,
    /// Content identifier of an earlier inline skill event in this session, in the prefixed form `sha256:<lowercase hex digest>` over the UTF-8 bytes of that event's `content`
    pub content_id: String,
    /// UTF-16 code unit length of the referenced skill content. Derived from the referenced body and validated against it when the reference is expanded; a reference whose length disagrees with the body it names is rejected instead of expanded
    pub content_length: i64,
    /// Description of the skill from its SKILL.md frontmatter
    pub description: Option<String>,
    /// Whether model invocation is disabled for this skill
    pub disable_model_invocation: Option<bool>,
    /// Projected chat-message count when the skill was invoked. Preserved from the inline event data when the authored body is deduplicated.
    pub invoked_at_turn: Option<i64>,
    /// MCP resource metadata preserved from the inline invocation.
    pub mcp_meta: Option<serde_json::Value>,
    /// Held MCP skill origin and manifest, preserved from the inline invocation.
    pub mcp_origin: Option<serde_json::Value>,
    /// Model identifier active when the skill was invoked, when known
    pub model: Option<String>,
    /// Name of the invoked skill
    pub name: String,
    /// File path to the SKILL.md definition, or an empty string for an SDK-provided skill without a filesystem identity
    pub path: String,
    /// Name of the plugin this skill originated from, when applicable
    pub plugin_name: Option<String>,
    /// Version of the plugin this skill originated from, when applicable
    pub plugin_version: Option<String>,
    /// Source identifier for where the skill was discovered
    pub source: Option<String>,
    /// What triggered the skill invocation
    pub trigger: Option<SkillInvokedTrigger>,
}
crate::record_serde::wire_record!(SkillInvokedRefData { allowed_tools: "allowedTools" if is_none, arguments: "arguments" if is_none, content_id: "contentId", content_length: "contentLength", description: "description" if is_none, disable_model_invocation: "disableModelInvocation" if is_none, invoked_at_turn: "invokedAtTurn" if is_none, mcp_meta: "mcpMeta" if is_none, mcp_origin: "mcpOrigin" if is_none, model: "model" if is_none, name: "name", path: "path", plugin_name: "pluginName" if is_none, plugin_version: "pluginVersion" if is_none, source: "source" if is_none, trigger: "trigger" if is_none });

/// Session event "skill.context_delivered". Exact skill context delivered to the model during a tool phase. This is not a user submission or another skill invocation.
#[derive(Debug, Clone, Default)]
pub struct SkillContextDeliveredData {
    /// Exact model-facing skill wrapper, including its invocation-time file context
    pub content: String,
    /// Interaction that delivered this context, when known
    pub interaction_id: Option<String>,
    /// Unmodified injection provenance, in the form skill-`<invocation-name>`
    pub source: String,
}
crate::record_serde::wire_record!(SkillContextDeliveredData { content: "content", interaction_id: "interactionId" if is_none, source: "source" });

/// Session event "skill.context_delivered_ref". Internal durable receipt that reconstructs exact model-visible skill context from earlier session content.
#[derive(Debug, Clone, Default)]
pub struct SkillContextDeliveredRefData {
    /// Content identifier of an earlier inline skill event in this session, in the prefixed form `sha256:<lowercase hex digest>` over the UTF-8 bytes of that event's `content`
    pub content_id: String,
    /// Interaction that delivered this context, when known
    pub interaction_id: Option<String>,
    /// Exact text preceding the referenced content in the delivered wrapper
    pub prefix: Option<String>,
    /// Unmodified injection provenance, in the form skill-`<invocation-name>`
    pub source: String,
    /// Exact text following the referenced content in the delivered wrapper
    pub suffix: Option<String>,
}
crate::record_serde::wire_record!(SkillContextDeliveredRefData { content_id: "contentId", interaction_id: "interactionId" if is_none, prefix: "prefix" if is_none, source: "source", suffix: "suffix" if is_none });

/// Effective sandbox filesystem rules, in policy order. Only populated when content capture is enabled, since these are real host paths.
#[derive(Debug, Clone, Default)]
pub struct SandboxFilesystemPolicyDetails {
    /// Paths the sandboxed process may not access at all
    pub denied_paths: Vec<String>,
    /// Paths the sandboxed process may read but not write
    pub readonly_paths: Vec<String>,
    /// Paths the sandboxed process may read and write
    pub readwrite_paths: Vec<String>,
}
crate::record_serde::wire_record!(SandboxFilesystemPolicyDetails {
    denied_paths: "deniedPaths",
    readonly_paths: "readonlyPaths",
    readwrite_paths: "readwritePaths"
});

#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataPolicyResolved {
    /// Whether the current working directory was granted automatically
    pub add_current_working_directory: bool,
    /// Whether callers may opt an individual command out of the sandbox
    pub allow_bypass: bool,
    /// Whether the sandboxed process may reach loopback and private-range addresses
    pub allow_local_network: bool,
    /// Whether the sandboxed process may open outbound network connections
    pub allow_outbound: bool,
    /// Whether the system temporary directory is granted read-write automatically. False when the host set `allowTempStorage` to false. Omitted by runtimes that predate the option, which always grant it. The resolved path appears in `effectiveFilesystemPolicy.readwritePaths` when content capture is enabled.
    pub allow_temp_storage: Option<bool>,
    /// Process-containment implementation backing the sandbox
    pub backend: SandboxBackend,
    /// Enforcement mechanism this decision describes
    pub control: SandboxControl,
    /// Why enforcement is weaker than configured, when it is
    pub degradation_reason: Option<SandboxDegradationReason>,
    /// Number of denied path rules in the effective policy
    pub denied_paths_count: i64,
    /// Effective filesystem rules. Populated only when content capture is enabled; the counts above are always present.
    pub effective_filesystem_policy: Option<SandboxFilesystemPolicyDetails>,
    /// Runtime subsystem that applied the policy
    pub enforcement_point: SandboxEnforcementPoint,
    /// Whether the sandbox policy permits GitHub CLI credentials inside the sandbox. A policy capability, not proof that a credential was injected into this spawn: injection is per-command
    pub gh_auth: bool,
    /// Whether the sandbox policy permits git credentials inside the sandbox. A policy capability, not proof that a credential was injected into this spawn: injection is per-command
    pub git_auth: bool,
    /// Whether the macOS keychain was reachable from inside the sandbox. Always false on other platforms.
    pub keychain_access: bool,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataPolicyResolvedKind,
    /// Controls whose value the enterprise-managed policy decided, as opposed to the user's own setting. Omitted when no control was managed.
    pub managed_controls: Option<Vec<SandboxManagedControl>>,
    /// Whether the resolved policy is fully active or degraded
    pub outcome: SandboxOutcome,
    /// Host platform the sandbox is running on
    pub platform: SandboxPlatform,
    /// Whether the policy came from built-in defaults or user configuration
    pub policy_source: SandboxPolicySource,
    /// Whether outbound traffic is unproxied, routed through a loopback proxy, or routed through an external proxy
    pub proxy_mode: SandboxProxyMode,
    /// Number of read-only path rules in the effective policy
    pub readonly_paths_count: i64,
    /// Number of read-write path rules in the effective policy
    pub readwrite_paths_count: i64,
    /// Internal tool-call ID, used only to correlate the decision with its owning span. Omitted when the decision is not attributable to a tool call.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataPolicyResolved { add_current_working_directory: "addCurrentWorkingDirectory", allow_bypass: "allowBypass", allow_local_network: "allowLocalNetwork", allow_outbound: "allowOutbound", allow_temp_storage: "allowTempStorage" if is_none, backend: "backend", control: "control", degradation_reason: "degradationReason" if is_none, denied_paths_count: "deniedPathsCount", effective_filesystem_policy: "effectiveFilesystemPolicy" if is_none, enforcement_point: "enforcementPoint", gh_auth: "ghAuth", git_auth: "gitAuth", keychain_access: "keychainAccess", kind: "kind", managed_controls: "managedControls" if is_none, outcome: "outcome", platform: "platform", policy_source: "policySource", proxy_mode: "proxyMode", readonly_paths_count: "readonlyPathsCount", readwrite_paths_count: "readwritePathsCount", tool_call_id: "toolCallId" if is_none });

#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataSpawnCompleted {
    /// Process-containment implementation backing the sandbox
    pub backend: SandboxBackend,
    /// Enforcement mechanism this decision describes
    pub control: SandboxControl,
    /// Why enforcement is weaker than configured, when it is
    pub degradation_reason: Option<SandboxDegradationReason>,
    /// Wall-clock time spent spawning the sandboxed process, in milliseconds
    pub duration_ms: f64,
    /// Runtime subsystem that applied the policy
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataSpawnCompletedKind,
    /// Whether the sandboxed process launched under the named backend. Not the exit status of the command that ran inside it.
    pub outcome: SandboxOutcome,
    /// Host platform the sandbox is running on
    pub platform: SandboxPlatform,
    /// Internal tool-call ID, used only to correlate the decision with its owning span. Omitted when the decision is not attributable to a tool call.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataSpawnCompleted { backend: "backend", control: "control", degradation_reason: "degradationReason" if is_none, duration_ms: "durationMs", enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", tool_call_id: "toolCallId" if is_none });

#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataEnforcementState {
    /// Runtime observation backing the state. Omitted for `inactive`, which has nothing to attest.
    pub attestation: Option<SandboxAttestation>,
    /// Containment backend that engaged. `unsupported` for any state other than `engaged`, since no backend is known to have run.
    pub backend: SandboxBackend,
    /// Command the enforcement governed. Populated only when content capture is enabled, and only for shell commands; MCP, LSP, and search command lines are runtime plumbing.
    pub command: Option<String>,
    /// Enforcement mechanism this decision describes
    pub control: SandboxControl,
    /// Runtime subsystem whose enforcement this describes
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataEnforcementStateKind,
    /// Observed enforcement state: `engaged`, `inactive`, or `failed`. Derived from runtime evidence, never from the configured posture or the compile-time target platform.
    pub outcome: SandboxOutcome,
    /// Host platform the sandbox is running on
    pub platform: SandboxPlatform,
    /// Internal tool-call ID, used only to correlate the decision with its owning span. Omitted when the decision is not attributable to a tool call.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataEnforcementState { attestation: "attestation" if is_none, backend: "backend", command: "command" if is_none, control: "control", enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", tool_call_id: "toolCallId" if is_none });

/// An enforcement check refused a specific access. Emitted per refusal with no deduplication, including when policy permits the caller to bypass the denial. Carries no backend: the built-in checks that produce this run in-process against the effective policy.
#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataAccessDenied {
    /// Runtime observation backing the denial.
    pub attestation: SandboxAttestation,
    /// Command whose execution the denial arose from. Populated only when content capture is enabled.
    pub command: Option<String>,
    /// How strong the evidence behind this denial is. Lets an analysis separate denials the sandbox recorded from ones inferred from output text, which otherwise look identical.
    pub confidence: Option<SandboxDenialConfidence>,
    /// Sandbox control the denial belongs to. Follows from `denialClass`.
    pub control: SandboxControl,
    /// Bounded class of the refused access.
    pub denial_class: SandboxDenialClass,
    /// Resource the check refused, when identified and content capture is enabled.
    pub denied_resource: Option<String>,
    /// Runtime subsystem that performed the check
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataAccessDeniedKind,
    /// Always `denied`.
    pub outcome: SandboxOutcome,
    /// Host operating-system family
    pub platform: SandboxPlatform,
    /// Executable image associated with the captured denial, normalized to a basename. Populated only when content capture is enabled.
    pub process_name: Option<String>,
    /// Tool call the denial belongs to, for span correlation only. Never exported as a telemetry attribute or metric dimension.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataAccessDenied { attestation: "attestation", command: "command" if is_none, confidence: "confidence" if is_none, control: "control", denial_class: "denialClass", denied_resource: "deniedResource" if is_none, enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", process_name: "processName" if is_none, tool_call_id: "toolCallId" if is_none });

/// Permissive learning mode (record and allow) recorded an access that the enforced policy would have refused, and allowed it. Emitted once per distinct recorded access of a record-and-allow run, bounded per command. The only per-access record of such a run: nothing was refused, so no `access_denied` is raised for it.
#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataAccessRecorded {
    /// Command that made the access. Populated only when content capture is enabled.
    pub command: Option<String>,
    /// Sandbox control the access belongs to. Follows from `denialClass`.
    pub control: SandboxControl,
    /// Bounded class of the access the enforced policy would have refused.
    pub denial_class: SandboxDenialClass,
    /// Resource the enforced policy would have refused. Populated only when content capture is enabled.
    pub denied_resource: Option<String>,
    /// Runtime subsystem that ran the command
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataAccessRecordedKind,
    /// Always `allowed`.
    pub outcome: SandboxOutcome,
    /// Why the run recorded and allowed instead of enforcing.
    pub permissive_source: SandboxPermissiveSource,
    /// Host operating-system family
    pub platform: SandboxPlatform,
    /// Tool call the access belongs to, for span correlation only. Never exported as a telemetry attribute or metric dimension.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataAccessRecorded { command: "command" if is_none, control: "control", denial_class: "denialClass", denied_resource: "deniedResource" if is_none, enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", permissive_source: "permissiveSource", platform: "platform", tool_call_id: "toolCallId" if is_none });

/// A request to run outside the process sandbox was resolved. This is what makes an `inactive` `enforcement_state` readable: without it, a command that ran unsandboxed because a person approved a bypass looks identical to one that ran unsandboxed because the session never had a sandbox. Reported only when a sandbox was in force, since bypassing a disabled sandbox bypasses nothing. Carries neither backend nor attestation: the verdict comes from the runtime's own permission flow or the local escalation prompt, not from a containment backend and not from the built-in policy check, so `source` is what records where it came from.
#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataBypassDecided {
    /// Command the verdict governs. Populated only when content capture is enabled.
    pub command: Option<String>,
    /// How strong the evidence behind `denialClass` was. Present exactly when `denialClass` is, so a verdict that relaxed the sandbox on a guess is distinguishable from one that relaxed it on a recorded refusal. Named to match `access_denied`, which reports the same pair.
    pub confidence: Option<SandboxDenialConfidence>,
    /// Always `bypass`.
    pub control: SandboxControl,
    /// Bounded class of the access whose refusal raised this escalation. Omitted for a pre-execution bypass, such as a detached command that cannot be sandboxed and therefore resolves no denial.
    pub denial_class: Option<SandboxDenialClass>,
    /// Resource whose refusal raised this escalation. Populated only when content capture is enabled.
    pub denied_resource: Option<String>,
    /// Runtime subsystem the bypass applies to
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataBypassDecidedKind,
    /// Whether the bypass was granted: `approved` or `declined`. `declined` also covers the cases where nobody answered, since the sandboxed denial stands either way.
    pub outcome: SandboxOutcome,
    /// Host operating-system family
    pub platform: SandboxPlatform,
    /// Executable image associated with the denial that raised this escalation, normalized to a basename. Populated only when content capture is enabled.
    pub process_name: Option<String>,
    /// Where the request originated. Orthogonal to `outcome`.
    pub source: SandboxBypassSource,
    /// Tool call the decision belongs to, for span correlation only. Never exported as a telemetry attribute or metric dimension.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataBypassDecided { command: "command" if is_none, confidence: "confidence" if is_none, control: "control", denial_class: "denialClass" if is_none, denied_resource: "deniedResource" if is_none, enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", process_name: "processName" if is_none, source: "source", tool_call_id: "toolCallId" if is_none });

/// A permissive retry was resolved. Distinct from `bypass_decided` because this rung never requests a run outside the process sandbox: it relaxes the process container for one run while the sandbox, and with it the network policy, stays attached. Reported for both outcomes, so an escalation the user declined still leaves evidence that the runtime asked and that the sandboxed denial stood.
#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataPermissiveRetryDecided {
    /// Command the verdict governs. Populated only when content capture is enabled.
    pub command: Option<String>,
    /// How strong the evidence behind `denialClass` was. Present exactly when `denialClass` is.
    pub confidence: Option<SandboxDenialConfidence>,
    /// Always `process`: the process container is what this rung relaxes, and the network control is deliberately untouched.
    pub control: SandboxControl,
    /// Bounded class of the access whose refusal raised this escalation.
    pub denial_class: Option<SandboxDenialClass>,
    /// Resource whose refusal raised this escalation. Populated only when content capture is enabled.
    pub denied_resource: Option<String>,
    /// Runtime subsystem the retry applies to
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataPermissiveRetryDecidedKind,
    /// Whether the permissive retry was granted: `approved` or `declined`. `declined` also covers the cases where nobody answered, since the sandboxed denial stands either way.
    pub outcome: SandboxOutcome,
    /// Host operating-system family
    pub platform: SandboxPlatform,
    /// Executable image associated with the denial that raised this escalation, normalized to a basename. Populated only when content capture is enabled.
    pub process_name: Option<String>,
    /// Where the request originated. Orthogonal to `outcome`.
    pub source: SandboxBypassSource,
    /// Tool call the decision belongs to, for span correlation only. Never exported as a telemetry attribute or metric dimension.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataPermissiveRetryDecided { command: "command" if is_none, confidence: "confidence" if is_none, control: "control", denial_class: "denialClass" if is_none, denied_resource: "deniedResource" if is_none, enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", process_name: "processName" if is_none, source: "source", tool_call_id: "toolCallId" if is_none });

/// An approved permissive retry finished. `succeeded` means the retry exited successfully without another correlated sandbox denial; `failed` means it failed or remained blocked and may therefore be followed by a full bypass.
#[derive(Debug, Clone, Default)]
pub struct SandboxDecisionDataPermissiveRetryCompleted {
    /// Command the retry executed. Populated only when content capture is enabled.
    pub command: Option<String>,
    /// How strong the evidence behind `denialClass` was.
    pub confidence: Option<SandboxDenialConfidence>,
    /// Always `process`: the retry changes process-container enforcement while leaving network policy attached.
    pub control: SandboxControl,
    /// Bounded class of the access whose refusal raised the permissive retry.
    pub denial_class: Option<SandboxDenialClass>,
    /// Resource whose refusal raised the retry. Populated only when content capture is enabled.
    pub denied_resource: Option<String>,
    /// Runtime subsystem that ran the retry
    pub enforcement_point: SandboxEnforcementPoint,
    /// Sandbox decision variant discriminator.
    pub kind: SandboxDecisionDataPermissiveRetryCompletedKind,
    /// Whether the permissive retry completed successfully: `succeeded` or `failed`.
    pub outcome: SandboxOutcome,
    /// Host operating-system family
    pub platform: SandboxPlatform,
    /// Executable image associated with the denial that raised the retry, normalized to a basename. Populated only when content capture is enabled.
    pub process_name: Option<String>,
    /// Tool call the completion belongs to, for span correlation only. Never exported as a telemetry attribute or metric dimension.
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(SandboxDecisionDataPermissiveRetryCompleted { command: "command" if is_none, confidence: "confidence" if is_none, control: "control", denial_class: "denialClass" if is_none, denied_resource: "deniedResource" if is_none, enforcement_point: "enforcementPoint", kind: "kind", outcome: "outcome", platform: "platform", process_name: "processName" if is_none, tool_call_id: "toolCallId" if is_none });

/// Session event "subagent.started". Sub-agent startup details including parent tool call and agent information
#[derive(Debug, Clone, Default)]
pub struct SubagentStartedData {
    /// Description of what the sub-agent does
    pub agent_description: String,
    /// Human-readable display name of the sub-agent
    pub agent_display_name: String,
    /// Internal name of the sub-agent
    pub agent_name: String,
    /// Type of the sub-agent selected at spawn time.
    pub agent_type: Option<String>,
    /// Whether the sub-agent runs synchronously or in the background.
    pub execution_mode: Option<String>,
    /// Interaction ID inherited from the user action that started this sub-agent
    pub interaction_id: Option<String>,
    /// Model the sub-agent will run with, when known at start.
    pub model: Option<String>,
    /// Authority or runtime mechanism responsible for sub-agent model selection, when known at start.
    pub model_selection_source: Option<SubagentModelSelectionSource>,
    /// Task-registry ID of the spawning sub-agent. Absent when the root session spawned this child.
    pub parent_id: Option<String>,
    /// Whether this sub-agent can be resumed. Currently always false.
    pub resumable: Option<bool>,
    /// Where the model input for this sub-agent came from. Present when the task planner resolved the launch (the task tool and workflow agents); absent for sub-agents created through other runtime paths.
    pub task_model_source: Option<SubagentTaskModelSource>,
    /// Tool call ID of the parent tool invocation that spawned this sub-agent
    pub tool_call_id: String,
    /// Root id of the workflow run that spawned this sub-agent, when it was spawned by one.
    pub workflow_run_id: Option<String>,
}
crate::record_serde::wire_record!(SubagentStartedData { agent_description: "agentDescription", agent_display_name: "agentDisplayName", agent_name: "agentName", agent_type: "agentType" if is_none, execution_mode: "executionMode" if is_none, interaction_id: "interactionId" if is_none, model: "model" if is_none, model_selection_source: "modelSelectionSource" if is_none, parent_id: "parentId" if is_none, resumable: "resumable" if is_none, task_model_source: "taskModelSource" if is_none, tool_call_id: "toolCallId", workflow_run_id: "workflowRunId" if is_none });

/// Session event "subagent.configured". Resolved runtime configuration for a configured sub-agent
#[derive(Debug, Clone, Default)]
pub struct SubagentConfiguredData {
    /// Resolved context tier, when configured for the model
    pub context_tier: Option<String>,
    /// Resolved model the sub-agent will run with
    pub model: String,
    /// Whether the sub-agent accepts follow-up turns
    pub multi_turn: bool,
    /// Resolved reasoning effort, when configured for the model
    pub reasoning_effort: Option<String>,
}
crate::record_serde::wire_record!(SubagentConfiguredData { context_tier: "contextTier" if is_none, model: "model", multi_turn: "multiTurn", reasoning_effort: "reasoningEffort" if is_none });

/// Session event "subagent.completed". Sub-agent completion details for successful execution
#[derive(Debug, Clone, Default)]
pub struct SubagentCompletedData {
    /// Human-readable display name of the sub-agent
    pub agent_display_name: String,
    /// Internal name of the sub-agent
    pub agent_name: String,
    /// Whether the sub-agent was torn down by cancellation - its own abort, or an ancestor being killed - instead of finishing its work. Cancellation is not a failure, so the run still reports completion; this distinguishes a torn-down sub-agent from one that ran to the end.
    pub cancelled: Option<bool>,
    /// Whether the first model actually dispatched matched the user's configured preference
    pub configured_model_matches_actual: Option<bool>,
    /// Concrete model the user configured for this sub-agent via `/subagents`, when present
    pub configured_model_preference: Option<String>,
    /// Wall-clock duration of the sub-agent execution in milliseconds
    pub duration_ms: Option<i64>,
    /// Whether the explicit task-call model matched the user's configured preference
    pub explicit_model_matches_preference: Option<bool>,
    /// Explicit model supplied by the parent agent on the task call, when present
    pub explicit_model_override: Option<String>,
    /// First model for which the sub-agent started an inference request, when one was dispatched
    pub first_dispatched_model: Option<String>,
    /// Interaction ID of the user action that produced this sub-agent turn
    pub interaction_id: Option<String>,
    /// Model used by the sub-agent
    pub model: Option<String>,
    /// Why an explicit task-call model did not become the effective model
    pub model_override_reason: Option<String>,
    /// Authority or runtime mechanism responsible for sub-agent model selection
    pub model_selection_source: Option<SubagentModelSelectionSource>,
    /// Tool call ID of the parent tool invocation that spawned this sub-agent
    pub tool_call_id: String,
    /// Total tokens (input + output) consumed by the sub-agent
    pub total_tokens: Option<i64>,
    /// Total number of tool calls made by the sub-agent
    pub total_tool_calls: Option<i64>,
}
crate::record_serde::wire_record!(SubagentCompletedData { agent_display_name: "agentDisplayName", agent_name: "agentName", cancelled: "cancelled" if is_none, configured_model_matches_actual: "configuredModelMatchesActual" if is_none, configured_model_preference: "configuredModelPreference" if is_none, duration_ms: "durationMs" if is_none, explicit_model_matches_preference: "explicitModelMatchesPreference" if is_none, explicit_model_override: "explicitModelOverride" if is_none, first_dispatched_model: "firstDispatchedModel" if is_none, interaction_id: "interactionId" if is_none, model: "model" if is_none, model_override_reason: "modelOverrideReason" if is_none, model_selection_source: "modelSelectionSource" if is_none, tool_call_id: "toolCallId", total_tokens: "totalTokens" if is_none, total_tool_calls: "totalToolCalls" if is_none });

/// Session event "subagent.failed". Sub-agent failure details including error message and agent information
#[derive(Debug, Clone, Default)]
pub struct SubagentFailedData {
    /// Human-readable display name of the sub-agent
    pub agent_display_name: String,
    /// Internal name of the sub-agent
    pub agent_name: String,
    /// Whether the first model actually dispatched matched the user's configured preference
    pub configured_model_matches_actual: Option<bool>,
    /// Concrete model the user configured for this sub-agent via `/subagents`, when present
    pub configured_model_preference: Option<String>,
    /// Wall-clock duration of the sub-agent execution in milliseconds
    pub duration_ms: Option<i64>,
    /// Error message describing why the sub-agent failed
    pub error: String,
    /// Whether the explicit task-call model matched the user's configured preference
    pub explicit_model_matches_preference: Option<bool>,
    /// Explicit model supplied by the parent agent on the task call, when present
    pub explicit_model_override: Option<String>,
    /// First model for which the sub-agent started an inference request, when one was dispatched
    pub first_dispatched_model: Option<String>,
    /// Interaction ID of the user action that produced this sub-agent turn
    pub interaction_id: Option<String>,
    /// Model selected for the sub-agent, when known
    pub model: Option<String>,
    /// Why an explicit task-call model did not become the effective model
    pub model_override_reason: Option<String>,
    /// Authority or runtime mechanism responsible for sub-agent model selection
    pub model_selection_source: Option<SubagentModelSelectionSource>,
    /// Tool call ID of the parent tool invocation that spawned this sub-agent
    pub tool_call_id: String,
    /// Total tokens (input + output) consumed before the sub-agent failed
    pub total_tokens: Option<i64>,
    /// Total number of tool calls made before the sub-agent failed
    pub total_tool_calls: Option<i64>,
}
crate::record_serde::wire_record!(SubagentFailedData { agent_display_name: "agentDisplayName", agent_name: "agentName", configured_model_matches_actual: "configuredModelMatchesActual" if is_none, configured_model_preference: "configuredModelPreference" if is_none, duration_ms: "durationMs" if is_none, error: "error", explicit_model_matches_preference: "explicitModelMatchesPreference" if is_none, explicit_model_override: "explicitModelOverride" if is_none, first_dispatched_model: "firstDispatchedModel" if is_none, interaction_id: "interactionId" if is_none, model: "model" if is_none, model_override_reason: "modelOverrideReason" if is_none, model_selection_source: "modelSelectionSource" if is_none, tool_call_id: "toolCallId", total_tokens: "totalTokens" if is_none, total_tool_calls: "totalToolCalls" if is_none });

/// Session event "subagent.selected". Custom agent selection details including name and available tools
#[derive(Debug, Clone, Default)]
pub struct SubagentSelectedData {
    /// Human-readable display name of the selected custom agent
    pub agent_display_name: String,
    /// Internal name of the selected custom agent
    pub agent_name: String,
    /// List of tool names available to this agent, or null for all tools
    pub tools: Option<Vec<String>>,
}
crate::record_serde::wire_record!(SubagentSelectedData {
    agent_display_name: "agentDisplayName",
    agent_name: "agentName",
    tools: "tools"
});

/// Session event "subagent.deselected". Empty payload; the event signals that the custom agent was deselected, returning to the default agent
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SubagentDeselectedData {}

/// Session event "hook.start". Hook invocation start details including type and input data
#[derive(Debug, Clone, Default)]
pub struct HookStartData {
    /// Unique identifier for this hook invocation
    pub hook_invocation_id: String,
    /// Type of hook being invoked (e.g., "preToolUse", "postToolUse", "sessionStart")
    pub hook_type: String,
    /// Input data passed to the hook. For postToolUse hooks the retained copy served by session.eventLog.read (and by a resumed session) drops the tool result's inline `contents`/`uiResource`/`skillInvocation` and replaces duplicated text result fields with a `[copilot:elided ...]` marker; the live subscription stream still delivers the full value. Canonical tool output remains in the adjacent tool.execution_complete event, while an invoked skill's authoritative body remains in its skill invocation event.
    pub input: Option<serde_json::Value>,
    /// Tool call ID of the parent tool invocation when this event originates from a sub-agent
    pub parent_tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(HookStartData { hook_invocation_id: "hookInvocationId", hook_type: "hookType", input: "input" if is_none, parent_tool_call_id: "parentToolCallId" if is_none });

/// Error details when the hook failed
#[derive(Debug, Clone, Default)]
pub struct HookEndError {
    /// Human-readable error message
    pub message: String,
    /// Source label of the hook that errored (e.g. the plugin it was loaded from), when known
    pub source: Option<String>,
    /// Error stack trace, when available
    pub stack: Option<String>,
}
crate::record_serde::wire_record!(HookEndError { message: "message", source: "source" if is_none, stack: "stack" if is_none });

/// Session event "hook.end". Hook invocation completion details including output, success status, and error information
#[derive(Debug, Clone, Default)]
pub struct HookEndData {
    /// Error details when the hook failed
    pub error: Option<HookEndError>,
    /// Identifier matching the corresponding hook.start event
    pub hook_invocation_id: String,
    /// Type of hook that was invoked (e.g., "preToolUse", "postToolUse", "sessionStart")
    pub hook_type: String,
    /// Output data produced by the hook. Durable and resumed postToolUse receipts may omit messages owned by a successful skill invocation and replace an unchanged skill sessionLog copy with an elision marker; hook-modified or re-sourced values are preserved, and the authoritative body remains in the skill invocation event.
    pub output: Option<serde_json::Value>,
    /// Tool call ID of the parent tool invocation when this event originates from a sub-agent
    pub parent_tool_call_id: Option<String>,
    /// Whether the hook completed successfully
    pub success: bool,
}
crate::record_serde::wire_record!(HookEndData { error: "error" if is_none, hook_invocation_id: "hookInvocationId", hook_type: "hookType", output: "output" if is_none, parent_tool_call_id: "parentToolCallId" if is_none, success: "success" });

/// Session event "hook.progress". Ephemeral progress update from a running hook process
#[derive(Debug, Clone, Default)]
pub struct HookProgressData {
    /// Human-readable progress message from the hook process
    pub message: String,
    /// When true, this status message replaces the previous temporary one instead of accumulating
    pub temporary: Option<bool>,
}
crate::record_serde::wire_record!(HookProgressData { message: "message", temporary: "temporary" if is_none });

/// Session event "session.binary_asset". Canonical bytes for a content-addressed binary asset shared by reference across events
#[derive(Debug, Clone, Default)]
pub struct SessionBinaryAssetData {
    /// Content-addressed id for this binary asset (e.g. "sha256:...").
    pub asset_id: String,
    /// Decoded byte length of the binary asset
    pub byte_length: i64,
    /// Base64-encoded binary data
    pub data: String,
    /// Human-readable description of the binary data
    pub description: Option<String>,
    /// Optional metadata from the producing tool.
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// MIME type of the binary asset
    pub mime_type: String,
    /// Binary asset type discriminator. Use "image" for images and "resource" otherwise.
    pub r#type: BinaryAssetType,
}
crate::record_serde::wire_record!(SessionBinaryAssetData { asset_id: "assetId", byte_length: "byteLength", data: "data", description: "description" if is_none, metadata: "metadata" if is_none, mime_type: "mimeType", r#type: "type" });

/// One persisted structured system-message block and its cache intent
#[derive(Debug, Clone, Default)]
pub struct SystemMessageContentBlock {
    /// Explicit prompt-cache intent. True places a breakpoint after this block, false suppresses one, and absence preserves the provider's legacy default.
    pub cache_breakpoint: Option<bool>,
    /// Text content for this system-message block.
    pub content: String,
    /// Diagnostic classification indicating whether the block is stable across equivalent sessions.
    pub is_static: Option<bool>,
}
crate::record_serde::wire_record!(SystemMessageContentBlock { cache_breakpoint: "cacheBreakpoint" if is_none, content: "content", is_static: "isStatic" if is_none });

/// Metadata about the prompt template and its construction
#[derive(Debug, Clone, Default)]
pub struct SystemMessageMetadata {
    /// Version identifier of the prompt template or structured prompt layout used
    pub prompt_version: Option<String>,
    /// Template variables used when constructing the prompt
    pub variables: Option<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(SystemMessageMetadata { prompt_version: "promptVersion" if is_none, variables: "variables" if is_none });

/// Session event "system.message". System/developer instruction content with role and optional template metadata
#[derive(Debug, Clone, Default)]
pub struct SystemMessageData {
    /// The system or developer prompt text sent as model input
    pub content: String,
    /// Optional ordered structured blocks corresponding to content, retained for prompt-cache layout restoration.
    pub content_blocks: Option<Vec<SystemMessageContentBlock>>,
    /// Logical interaction identifier for the model run receiving this prompt
    pub interaction_id: Option<String>,
    /// Metadata about the prompt template and its construction
    pub metadata: Option<SystemMessageMetadata>,
    /// Optional name identifier for the message source
    pub name: Option<String>,
    /// Message role: "system" for system prompts, "developer" for developer-injected instructions
    pub role: SystemMessageRole,
}
crate::record_serde::wire_record!(SystemMessageData { content: "content", content_blocks: "contentBlocks" if is_none, interaction_id: "interactionId" if is_none, metadata: "metadata" if is_none, name: "name" if is_none, role: "role" });

/// Session event "instruction.update". An authoritative developer-instruction update. Event order defines instruction order. Persistent updates are durable and restore across replay, resume, rewind, fork, and compaction; turn-scoped updates must be emitted with ephemeral=true and are never restored.
#[derive(Debug, Clone, Default)]
pub struct InstructionUpdateData {
    /// Authority responsible for the update.
    pub authority: InstructionUpdateAuthority,
    /// Earliest legal insertion boundary.
    pub boundary: InstructionUpdateBoundary,
    /// Authoritative instruction text. Empty or whitespace-only content is rejected.
    pub content: String,
    /// Deterministic behavior when the active provider cannot represent the update.
    pub fallback: InstructionUpdateFallback,
    /// Whether the update persists beyond the current turn.
    pub lifetime: InstructionUpdateLifetime,
    /// Optional stable source name for diagnostics and future host-authored updates.
    pub name: Option<String>,
    /// Whether this update was recorded after the conversation's initial user message.
    #[doc(hidden)]
    pub(crate) post_initial: Option<bool>,
}
crate::record_serde::wire_record!(InstructionUpdateData { authority: "authority", boundary: "boundary", content: "content", fallback: "fallback", lifetime: "lifetime", name: "name" if is_none, post_initial: "postInitial" if is_none });

/// Session event "system.notification". System-generated notification for runtime events like background task completion
#[derive(Debug, Clone, Default)]
pub struct SystemNotificationData {
    /// IDs of accepted assistant messages whose output this notification consumed, referring to assistant.message.data.messageId across the agent tree. Captured from the delivered snapshot, not the sender's later state. An empty list means no model-produced dependency; absence means provenance is unavailable.
    pub causal_dependencies: Option<Vec<String>>,
    /// The notification text, typically wrapped in `<system_notification>` XML tags
    pub content: String,
    /// Structured metadata identifying what triggered this notification
    pub kind: serde_json::Value,
    /// Provider reasoning settings anchored before this model-facing message for cache-stable replay; the historical responsesReasoning name is retained for compatibility
    pub responses_reasoning: Option<ResponsesReasoning>,
}
crate::record_serde::wire_record!(SystemNotificationData { causal_dependencies: "causalDependencies" if is_none, content: "content", kind: "kind", responses_reasoning: "responsesReasoning" if is_none });

/// No trusted Assisted Permissions assessment is available, including for historical events.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistedApprovalAssessmentUnavailable {
    /// Assisted Permissions assessment outcome discriminator.
    pub status: AssistedApprovalAssessmentUnavailableStatus,
}
crate::record_serde::wire_record!(AssistedApprovalAssessmentUnavailable { status: "status" });

/// The request did not reach the Assisted Permissions judge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistedApprovalAssessmentNotRun {
    /// Machine-readable reason. Current runtimes emit `inactive` or `not-reached`; consumers must tolerate future strings.
    pub reason: String,
    /// Assisted Permissions assessment outcome discriminator.
    pub status: AssistedApprovalAssessmentNotRunStatus,
}
crate::record_serde::wire_record!(AssistedApprovalAssessmentNotRun {
    reason: "reason",
    status: "status"
});

/// A deterministic runtime or policy gate prevented automatic approval without consulting the judge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistedApprovalAssessmentExcluded {
    /// Machine-readable exclusion reason. Current runtimes emit `managed-approval-required`, `sandbox-bypass`, `extension-permission-access`, `action-too-long`, `credential-minting`, `credential-exposure`, `authorization-history-incomplete`, `path-not-authorized`, or `content-excluded`; consumers must tolerate future strings.
    pub reason: String,
    /// Assisted Permissions assessment outcome discriminator.
    pub status: AssistedApprovalAssessmentExcludedStatus,
}
crate::record_serde::wire_record!(AssistedApprovalAssessmentExcluded {
    reason: "reason",
    status: "status"
});

/// The Assisted Permissions judge returned a usable verdict.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistedApprovalAssessmentCompleted {
    /// Judge-authored human-readable rationale. A bare ALLOW or DENY verdict legitimately leaves this absent.
    pub rationale: Option<String>,
    /// Judge verdict for this request.
    pub recommendation: AssistedApprovalCompletedRecommendation,
    /// Assisted Permissions assessment outcome discriminator.
    pub status: AssistedApprovalAssessmentCompletedStatus,
}
crate::record_serde::wire_record!(AssistedApprovalAssessmentCompleted { rationale: "rationale" if is_none, recommendation: "recommendation", status: "status" });

/// The judge path produced no usable verdict.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AssistedApprovalAssessmentFailed {
    /// Runtime-authored explanation of the failure when the runtime knows more than `reason`, such as the model error the judge call returned or how to make a judge model available. Absent when `reason` says everything known.
    pub message: Option<String>,
    /// Machine-readable failure reason. Current runtimes emit `timeout`, `abort`, `empty_response`, `model_error`, `parse_error`, or `context_too_large`; consumers must tolerate future strings.
    pub reason: String,
    /// Assisted Permissions assessment outcome discriminator.
    pub status: AssistedApprovalAssessmentFailedStatus,
}
crate::record_serde::wire_record!(AssistedApprovalAssessmentFailed { message: "message" if is_none, reason: "reason", status: "status" });

/// A parsed command identifier in a shell permission request, including whether it is read-only.
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestShellCommand {
    /// Command identifier (e.g., executable name)
    pub identifier: String,
    /// Whether this command is read-only (no side effects)
    pub read_only: bool,
}
crate::record_serde::wire_record!(PermissionRequestShellCommand {
    identifier: "identifier",
    read_only: "readOnly"
});

/// A parsed shell command segment used for argument-aware managed policy matching.
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestShellCommandSegment {
    /// Full text of this command segment, including arguments
    pub full_command_text: String,
    /// Command identifier (e.g., executable name)
    pub identifier: String,
}
crate::record_serde::wire_record!(PermissionRequestShellCommandSegment {
    full_command_text: "fullCommandText",
    identifier: "identifier"
});

/// A URL that may be accessed by a command in a shell permission request.
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestShellPossibleUrl {
    /// URL that may be accessed by the command
    pub url: String,
}
crate::record_serde::wire_record!(PermissionRequestShellPossibleUrl { url: "url" });

/// A sandbox filesystem policy edit that would let a blocked operation run inside the sandbox instead of outside it. Offered only on a sandbox escalation request whose denial adding this path lifts, and only when managed policy permits the grant. A host accepts it with session.sandbox.grantPathForRequest, which adds the path to the session's sandbox policy and re-runs the operation sandboxed. A host that persists sandbox settings must save the optional policy spelling when present; otherwise it must escape every literal `$` in the concrete path as `$$` before saving that path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionSandboxPathGrant {
    /// Which access the grant confers, and so which policy list the path is added to
    pub access: PermissionSandboxPathGrantAccess,
    /// The path the sandbox refused, present only when it differs from path. That happens when a write under a read-only folder moves the folder to the read-write paths, when a path that does not exist yet is granted through its nearest existing folder, because the OS sandbox cannot grant a path before it exists, and when either is spelled through a symlink, because a grant covers its path as written, so path is then the resolved location. Hosts should then name path in the offer, since the denial names this one.
    pub denied_path: Option<String>,
    /// Absolute path to add to the sandbox filesystem policy
    pub path: String,
    /// Policy spelling to persist for the grant. This can differ from path when a managed allowlist names the same location through an environment expression. Hosts that persist sandbox settings must save this value when present. When it is absent, they must escape every literal `$` in path as `$$` before saving path so the policy grammar does not interpret it as an environment reference.
    pub persisted_path: Option<String>,
    /// readonlyPaths entries the grant removes, exactly as written in the policy, because a read-only entry for the same location would otherwise keep the path read-only. A host that persists the path must remove these entries from its stored readonlyPaths too.
    pub removed_readonly_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PermissionSandboxPathGrant { access: "access", denied_path: "deniedPath" if is_none, path: "path", persisted_path: "persistedPath" if is_none, removed_readonly_paths: "removedReadonlyPaths" if is_none });

/// Shell command permission request
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionRequestShell {
    /// Whether the UI can offer session-wide approval for this command pattern
    pub can_offer_session_approval: bool,
    /// Parsed command identifiers found in the command text
    pub commands: Vec<PermissionRequestShellCommand>,
    /// Parsed command segments, including arguments, used for managed policy matching
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command_segments: Option<Vec<PermissionRequestShellCommandSegment>>,
    /// The complete shell command text to be executed
    pub full_command_text: String,
    /// Whether the command includes a file write redirection (e.g., > or >>)
    pub has_write_file_redirection: bool,
    /// Human-readable description of what the command intends to do
    pub intention: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestShellKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    #[serde(skip_serializing_if = "Option::is_none")]
    pub managed_approval_required: Option<bool>,
    /// File paths that may be read or written by the command
    pub possible_paths: Vec<String>,
    /// URLs that may be accessed by the command
    pub possible_urls: Vec<PermissionRequestShellPossibleUrl>,
    /// True when the tool is asking to run this command outside the sandbox, either because the command detaches and cannot be sandboxed at all, or because a sandboxed run looked blocked (host opted in via sandbox.allowBypass). The model cannot ask for this; only the tool raises it. This is a request, not a grant: the command runs unsandboxed only if the user approves this permission request. Hosts should highlight the elevated risk in the approval UI.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_sandbox_bypass: Option<bool>,
    /// What the tool tells the user about the bypass on offer: which policy rule blocked the call, or why it cannot be sandboxed. Only meaningful when requestSandboxBypass is true.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_sandbox_bypass_reason: Option<String>,
    /// True when the requested escalation is a permissive retry rather than a full bypass: the command re-runs inside the sandbox with its file and process restrictions recording instead of blocking, while the network policy stays enforced. Always accompanied by requestSandboxBypass, so hosts that do not recognize this field still treat the request as the escalation it is. Hosts that do recognize it must not describe the command as running outside the sandbox, which would overstate the privilege being granted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_sandbox_permissive: Option<bool>,
    /// Runtime-resolved canonical object each possiblePaths entry names, keyed by the requested spelling, used for authorization identity checks. Internal and experimental; clients should continue to display possiblePaths.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_paths: Option<HashMap<String, String>>,
    /// Runtime-resolved canonical working directory the command runs in, used for authorization identity checks. Internal and experimental; clients should not display it.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_working_directory: Option<String>,
    /// Sandbox policy edit that would let the command run inside the sandbox. Only present when requestSandboxBypass is true.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_path_grant: Option<PermissionSandboxPathGrant>,
    /// Tool call ID that triggered this permission request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
    /// Optional warning message about risks of running this command
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,
}

/// A named, complete UTF-8 text snapshot for a pending file operation.
#[derive(Debug, Clone, Default)]
pub struct PermissionWriteFileContent {
    /// Complete file contents, including an empty string for an empty file.
    pub content: String,
    /// Absolute path in the session filesystem namespace.
    pub path: String,
}
crate::record_serde::wire_record!(PermissionWriteFileContent {
    content: "content",
    path: "path"
});

/// Complete text snapshots for one proposed file change. At least one side is present. Different before and after paths describe a move.
#[derive(Debug, Clone, Default)]
pub struct PermissionWriteFileEdit {
    /// Complete file content the operation plans to write. Absent only when the file is being deleted.
    pub after: Option<PermissionWriteFileContent>,
    /// File content read while planning the operation. Absent only when the file is being created.
    pub before: Option<PermissionWriteFileContent>,
}
crate::record_serde::wire_record!(PermissionWriteFileEdit { after: "after" if is_none, before: "before" if is_none });

/// File write permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestWrite {
    /// Whether the UI can offer session-wide approval for file write operations
    pub can_offer_session_approval: bool,
    /// Unified diff showing the proposed changes
    pub diff: String,
    /// Complete before/after text previews captured while planning this request. Omitted when a complete preview is unavailable, including pre-read sandbox requests and non-UTF-8 files. Missing sides mean creation or deletion, not unavailable content.
    pub file_edits: Option<Vec<PermissionWriteFileEdit>>,
    /// Path of the file being written to
    pub file_name: String,
    /// Human-readable description of the intended file change
    pub intention: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestWriteKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Complete new file contents for newly created files
    pub new_file_contents: Option<String>,
    /// True when a built-in file tool (apply_patch / str_replace_editor) asked to write a path the sandbox filesystem policy would block, and the host opted in via sandbox.allowBypass. This is a request, not a grant: the write happens unsandboxed only if the user approves this permission request. Hosts should highlight the elevated risk in the approval UI.
    pub request_sandbox_bypass: Option<bool>,
    /// Justification for the sandbox-bypass request. Only meaningful when requestSandboxBypass is true.
    pub request_sandbox_bypass_reason: Option<String>,
    /// Runtime-resolved canonical path used for authorization identity checks. Internal and experimental; clients should continue to display fileName.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub resolved_path: Option<String>,
    /// Sandbox policy edit that would let the write run inside the sandbox. Only present when requestSandboxBypass is true.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub sandbox_path_grant: Option<PermissionSandboxPathGrant>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestWrite { can_offer_session_approval: "canOfferSessionApproval", diff: "diff", file_edits: "fileEdits" if is_none, file_name: "fileName", intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, new_file_contents: "newFileContents" if is_none, request_sandbox_bypass: "requestSandboxBypass" if is_none, request_sandbox_bypass_reason: "requestSandboxBypassReason" if is_none, resolved_path: "resolvedPath" if is_none, sandbox_path_grant: "sandboxPathGrant" if is_none, tool_call_id: "toolCallId" if is_none });

/// File or directory read permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestRead {
    /// Human-readable description of why the file is being read
    pub intention: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestReadKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Path of the file or directory being read
    pub path: String,
    /// True when the tool is asking to re-run this search outside the sandbox, after a sandboxed run looked blocked (host opted in via sandbox.allowBypass). The model cannot ask for this; only the tool raises it. This is a request, not a grant: the search runs unsandboxed only if the user approves this permission request. Hosts should highlight the elevated risk in the approval UI.
    pub request_sandbox_bypass: Option<bool>,
    /// What the tool tells the user about the bypass on offer: which policy rule blocked the call, or why it cannot be sandboxed. Only meaningful when requestSandboxBypass is true.
    pub request_sandbox_bypass_reason: Option<String>,
    /// Runtime-resolved canonical path used for authorization identity checks. Internal and experimental; clients should continue to display path.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub resolved_path: Option<String>,
    /// Sandbox policy edit that would let the read run inside the sandbox. Only present when requestSandboxBypass is true.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub sandbox_path_grant: Option<PermissionSandboxPathGrant>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestRead { intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, path: "path", request_sandbox_bypass: "requestSandboxBypass" if is_none, request_sandbox_bypass_reason: "requestSandboxBypassReason" if is_none, resolved_path: "resolvedPath" if is_none, sandbox_path_grant: "sandboxPathGrant" if is_none, tool_call_id: "toolCallId" if is_none });

/// MCP tool invocation permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestMcp {
    /// Arguments to pass to the MCP tool
    pub args: Option<serde_json::Value>,
    /// Permission kind discriminator
    pub kind: PermissionRequestMcpKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Advisory runtime permission recommendation. The SDK host remains responsible for deciding the request and may reject it.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub permission_recommendation: Option<PermissionRecommendation>,
    /// Whether this MCP tool is read-only (no side effects)
    pub read_only: bool,
    /// Display label for a Connector-owned MCP server, from the trusted Connector catalog; absent for other servers. `serverName` stays the identity for approvals and rules.
    pub server_display_name: Option<String>,
    /// Name of the MCP server providing the tool
    pub server_name: String,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Internal name of the MCP tool
    pub tool_name: String,
    /// Human-readable title of the MCP tool
    pub tool_title: String,
}
crate::record_serde::wire_record!(PermissionRequestMcp { args: "args" if is_none, kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, permission_recommendation: "permissionRecommendation" if is_none, read_only: "readOnly", server_display_name: "serverDisplayName" if is_none, server_name: "serverName", tool_call_id: "toolCallId" if is_none, tool_name: "toolName", tool_title: "toolTitle" });

/// URL access permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestUrl {
    /// Human-readable description of why the URL is being accessed
    pub intention: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestUrlKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Immediately preceding URL when this request is for a redirect target
    pub redirected_from: Option<String>,
    /// True when the tool is asking to run this URL fetch outside the sandbox, after the network policy denied the approved URL or the sandbox proxy could not reach it (host opted in via sandbox.allowBypass). The model cannot ask for this; only the tool raises it. This is a request, not a grant: the fetch runs only if the user approves this permission request. Hosts should highlight the elevated risk in the approval UI.
    pub request_sandbox_bypass: Option<bool>,
    /// What the tool tells the user about the bypass on offer: which policy rule blocked the call, or why it cannot be sandboxed. Only meaningful when requestSandboxBypass is true.
    pub request_sandbox_bypass_reason: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// URL to be fetched
    pub url: String,
}
crate::record_serde::wire_record!(PermissionRequestUrl { intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, redirected_from: "redirectedFrom" if is_none, request_sandbox_bypass: "requestSandboxBypass" if is_none, request_sandbox_bypass_reason: "requestSandboxBypassReason" if is_none, tool_call_id: "toolCallId" if is_none, url: "url" });

/// Bounded runtime attribution, independent of free-text rationale. Telemetry revalidates this vocabulary before standard collection.
#[derive(Debug, Clone, Default)]
pub struct PermissionApprovalEvaluation {
    /// Stage that produced this attribution.
    pub evaluation_stage: PermissionApprovalEvaluationEvaluationStage,
    /// Whether the request invoked the judge interface. A cached recommendation retains the original attempt fact. Omitted means unknown, including inherited outcomes.
    pub judge_attempted: Option<bool>,
    /// Status of the local judge interface, not proof of a model network call.
    pub judge_status: PermissionApprovalEvaluationJudgeStatus,
    /// Machine-readable runtime gate reason, never a command, path or human rationale.
    pub reason_code: PermissionApprovalEvaluationReasonCode,
}
crate::record_serde::wire_record!(PermissionApprovalEvaluation { evaluation_stage: "evaluationStage", judge_attempted: "judgeAttempted" if is_none, judge_status: "judgeStatus", reason_code: "reasonCode" });

/// Assisted-approval judge information attached to a permission request. Present only in assisted mode; its absence means the judge did not evaluate the request. The `recommendation` conveys the judge's disposition for this request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionAssistedApproval {
    /// Zero-based indexes into the shell request's `commandSegments` that a `requireApproval` recommendation identified as needing approval, validated by the runtime against that request. Absent when the recommendation did not attribute approval to specific segments; the whole command then needs approval.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub approval_required_command_segments: Option<Vec<i64>>,
    /// Runtime reason and judge-call metadata. Absent on older events; missing metadata means unknown, not that the judge was skipped.
    pub evaluation: Option<PermissionApprovalEvaluation>,
    /// Runtime-generated correlation ID for the original judge invocation. Cached recommendations retain it. Absent when review was skipped or the event predates correlation support; never part of the judge prompt.
    pub evaluation_id: Option<String>,
    /// Classified cause of an `error` recommendation. Absent for every other recommendation.
    pub failure_reason: Option<AssistedApprovalJudgeFailureReason>,
    /// Model id that produced the recommendation, when the judge was consulted and reported one. Absent for `excluded` (the judge was not consulted) and for failures that occurred before a model was selected.
    pub model: Option<String>,
    /// Human-readable reason for the judge's recommendation, when available.
    pub reason: Option<String>,
    /// The assisted-approval safety judge's outcome for this request.
    pub recommendation: AssistedApprovalRecommendation,
}
crate::record_serde::wire_record!(PermissionAssistedApproval { approval_required_command_segments: "approvalRequiredCommandSegments" if is_none, evaluation: "evaluation" if is_none, evaluation_id: "evaluationId" if is_none, failure_reason: "failureReason" if is_none, model: "model" if is_none, reason: "reason" if is_none, recommendation: "recommendation" });

/// Memory operation permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestMemory {
    /// Whether this is a store or vote memory operation
    pub action: Option<PermissionRequestMemoryAction>,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Source references for the stored fact (store only)
    pub citations: Option<String>,
    /// Vote direction (vote only)
    pub direction: Option<PermissionRequestMemoryDirection>,
    /// The fact being stored or voted on
    pub fact: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestMemoryKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Reason for the vote (vote only)
    pub reason: Option<String>,
    /// Repository name with owner associated with the stored memory (store only)
    pub repo_nwo: Option<String>,
    /// Scope of the stored memory (store only)
    pub scope: Option<PermissionRequestMemoryScope>,
    /// Topic or subject of the memory (store only)
    pub subject: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestMemory { action: "action" if is_none, assisted_approval: "assistedApproval" if is_none, citations: "citations" if is_none, direction: "direction" if is_none, fact: "fact", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, reason: "reason" if is_none, repo_nwo: "repoNwo" if is_none, scope: "scope" if is_none, subject: "subject" if is_none, tool_call_id: "toolCallId" if is_none });

/// Custom tool invocation permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestCustomTool {
    /// Arguments to pass to the custom tool
    pub args: Option<serde_json::Value>,
    /// Permission kind discriminator
    pub kind: PermissionRequestCustomToolKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Whether the tool declared that permission may be skipped unless a deny rule matches
    pub skip_permission: Option<bool>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Description of what the custom tool does
    pub tool_description: String,
    /// Name of the custom tool
    pub tool_name: String,
}
crate::record_serde::wire_record!(PermissionRequestCustomTool { args: "args" if is_none, kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, skip_permission: "skipPermission" if is_none, tool_call_id: "toolCallId" if is_none, tool_description: "toolDescription", tool_name: "toolName" });

/// Hook confirmation permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestHook {
    /// Optional message from the hook explaining why confirmation is needed
    pub hook_message: Option<String>,
    /// Permission kind discriminator
    pub kind: PermissionRequestHookKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Arguments of the tool call being gated
    pub tool_args: Option<serde_json::Value>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Name of the tool the hook is gating
    pub tool_name: String,
}
crate::record_serde::wire_record!(PermissionRequestHook { hook_message: "hookMessage" if is_none, kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, tool_args: "toolArgs" if is_none, tool_call_id: "toolCallId" if is_none, tool_name: "toolName" });

/// Extension management permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestExtensionManagement {
    /// Name of the extension being managed
    pub extension_name: Option<String>,
    /// Permission kind discriminator
    pub kind: PermissionRequestExtensionManagementKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// The extension management operation (scaffold, reload)
    pub operation: String,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestExtensionManagement { extension_name: "extensionName" if is_none, kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, operation: "operation", tool_call_id: "toolCallId" if is_none });

/// A declared phase shown in a workflow permission prompt.
#[derive(Debug, Clone, Default)]
pub struct WorkflowPermissionPhase {
    /// Optional phase detail
    pub detail: Option<String>,
    /// Phase title
    pub title: String,
}
crate::record_serde::wire_record!(WorkflowPermissionPhase { detail: "detail" if is_none, title: "title" });

/// Workflow run or authoring permission request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestWorkflow {
    /// Canonical key used for scoped workflow approvals
    pub approval_key: String,
    /// Whether this workflow is eligible for persistent approval
    pub can_persist_approval: bool,
    /// Workflow-declared AI-credit limit before any run/resume caller override is applied.
    pub declared_max_ai_credits: Option<f64>,
    /// Workflow-declared concurrent-subagent limit before any run/resume caller override is applied.
    pub declared_max_concurrent_subagents: Option<i64>,
    /// Workflow-declared total-subagent limit before any run/resume caller override is applied.
    pub declared_max_total_subagents: Option<i64>,
    /// Workflow-declared active-time limit in seconds before any run/resume caller override is applied.
    pub declared_timeout_seconds: Option<f64>,
    /// Workflow description
    pub description: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestWorkflowKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Effective AI-credit limit; omitted means unlimited
    pub max_ai_credits: Option<f64>,
    /// Effective concurrent-subagent limit; omitted means unlimited
    pub max_concurrent_subagents: Option<i64>,
    /// Effective total-subagent limit; omitted means unlimited
    pub max_total_subagents: Option<i64>,
    /// Workflow name
    pub name: String,
    /// Workflow operation, either run or author
    pub operation: WorkflowPermissionOperation,
    /// Declared workflow phases
    pub phases: Vec<WorkflowPermissionPhase>,
    /// Effective active-time limit in seconds; omitted means unlimited
    pub timeout_seconds: Option<f64>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestWorkflow { approval_key: "approvalKey", can_persist_approval: "canPersistApproval", declared_max_ai_credits: "declaredMaxAiCredits" if is_none, declared_max_concurrent_subagents: "declaredMaxConcurrentSubagents" if is_none, declared_max_total_subagents: "declaredMaxTotalSubagents" if is_none, declared_timeout_seconds: "declaredTimeoutSeconds" if is_none, description: "description", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, max_ai_credits: "maxAiCredits" if is_none, max_concurrent_subagents: "maxConcurrentSubagents" if is_none, max_total_subagents: "maxTotalSubagents" if is_none, name: "name", operation: "operation", phases: "phases", timeout_seconds: "timeoutSeconds" if is_none, tool_call_id: "toolCallId" if is_none });

/// Extension permission access request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestExtensionPermissionAccess {
    /// Capabilities the extension is requesting
    pub capabilities: Vec<String>,
    /// Name of the extension requesting permission access
    pub extension_name: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestExtensionPermissionAccessKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestExtensionPermissionAccess { capabilities: "capabilities", extension_name: "extensionName", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, tool_call_id: "toolCallId" if is_none });

/// Extension sensitive environment variable access request
#[derive(Debug, Clone, Default)]
pub struct PermissionRequestExtensionEnvAccess {
    /// Names of the sensitive environment variables the extension is requesting. Values never appear here.
    pub environment_variables: Vec<String>,
    /// Name of the extension requesting environment variable access
    pub extension_name: String,
    /// Permission kind discriminator
    pub kind: PermissionRequestExtensionEnvAccessKind,
    /// When true, managed policy requires an explicit user decision and automatic approval must be bypassed.
    pub managed_approval_required: Option<bool>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionRequestExtensionEnvAccess { environment_variables: "environmentVariables", extension_name: "extensionName", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, tool_call_id: "toolCallId" if is_none });

/// Shell command permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestCommands {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Whether the UI can offer session-wide approval for this command pattern
    pub can_offer_session_approval: bool,
    /// Command identifiers covered by this approval prompt
    pub command_identifiers: Vec<String>,
    /// The complete shell command text to be executed
    pub full_command_text: String,
    /// Human-readable description of what the command intends to do
    pub intention: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestCommandsKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// True when the shell command is requesting sandbox escalation. This is a request, not a grant.
    pub request_sandbox_bypass: Option<bool>,
    /// Reason for the sandbox escalation request.
    pub request_sandbox_bypass_reason: Option<String>,
    /// True when the escalation is a permissive retry that keeps the sandbox and network policy attached while recording file and process accesses instead of blocking them.
    pub request_sandbox_permissive: Option<bool>,
    /// Sandbox policy edit that would let the command run inside the sandbox. Only present when requestSandboxBypass is true.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub sandbox_path_grant: Option<PermissionSandboxPathGrant>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Optional warning message about risks of running this command
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestCommands { assisted_approval: "assistedApproval" if is_none, can_offer_session_approval: "canOfferSessionApproval", command_identifiers: "commandIdentifiers", full_command_text: "fullCommandText", intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, request_sandbox_bypass: "requestSandboxBypass" if is_none, request_sandbox_bypass_reason: "requestSandboxBypassReason" if is_none, request_sandbox_permissive: "requestSandboxPermissive" if is_none, sandbox_path_grant: "sandboxPathGrant" if is_none, tool_call_id: "toolCallId" if is_none, warning: "warning" if is_none });

/// File write permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestWrite {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Whether the UI can offer session-wide approval for file write operations
    pub can_offer_session_approval: bool,
    /// Unified diff showing the proposed changes
    pub diff: String,
    /// Complete before/after text previews captured while planning this request. Omitted when a complete preview is unavailable, including pre-read sandbox requests and non-UTF-8 files. Missing sides mean creation or deletion, not unavailable content.
    pub file_edits: Option<Vec<PermissionWriteFileEdit>>,
    /// Path of the file being written to
    pub file_name: String,
    /// Human-readable description of the intended file change
    pub intention: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestWriteKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Complete new file contents for newly created files
    pub new_file_contents: Option<String>,
    /// Runtime-resolved canonical path used for authorization identity checks. Internal and experimental; clients should continue to display fileName.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub resolved_path: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestWrite { assisted_approval: "assistedApproval" if is_none, can_offer_session_approval: "canOfferSessionApproval", diff: "diff", file_edits: "fileEdits" if is_none, file_name: "fileName", intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, new_file_contents: "newFileContents" if is_none, resolved_path: "resolvedPath" if is_none, tool_call_id: "toolCallId" if is_none });

/// File read permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestRead {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Human-readable description of why the file is being read
    pub intention: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestReadKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Path of the file or directory being read
    pub path: String,
    /// Runtime-resolved canonical path used for authorization identity checks. Internal and experimental; clients should continue to display path.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub resolved_path: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestRead { assisted_approval: "assistedApproval" if is_none, intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, path: "path", resolved_path: "resolvedPath" if is_none, tool_call_id: "toolCallId" if is_none });

/// MCP tool invocation permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestMcp {
    /// Arguments to pass to the MCP tool
    pub args: Option<serde_json::Value>,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Whether the host may offer a server-wide "approve all tools from this server" blanket. Absent is treated as true; the runtime sends false when managed policy disables bypass-permissions mode, which forbids the server-wide escalation while still allowing per-tool approval.
    pub can_offer_server_wide_approval: Option<bool>,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestMcpKind,
    /// Advisory runtime permission recommendation. The host remains responsible for deciding the request and may reject it.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub permission_recommendation: Option<PermissionRecommendation>,
    /// Display label for a Connector-owned MCP server, from the trusted Connector catalog; absent for other servers. `serverName` stays the identity for approvals and rules.
    pub server_display_name: Option<String>,
    /// Name of the MCP server providing the tool
    pub server_name: String,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Internal name of the MCP tool
    pub tool_name: String,
    /// Human-readable title of the MCP tool
    pub tool_title: String,
}
crate::record_serde::wire_record!(PermissionPromptRequestMcp { args: "args" if is_none, assisted_approval: "assistedApproval" if is_none, can_offer_server_wide_approval: "canOfferServerWideApproval" if is_none, kind: "kind", permission_recommendation: "permissionRecommendation" if is_none, server_display_name: "serverDisplayName" if is_none, server_name: "serverName", tool_call_id: "toolCallId" if is_none, tool_name: "toolName", tool_title: "toolTitle" });

/// URL access permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestUrl {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Human-readable description of why the URL is being accessed
    pub intention: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestUrlKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Wildcard pattern covering the registrable domain of the URL and its subdomains, for example https://*.example.com for https://api.example.com/x. The runtime computes it from the Public Suffix List, keeping the URL's scheme and non-default port, so every host offers the same scope. Absent when no parent-domain approval can be offered safely (IP addresses, private-network names, public suffixes) and for sandbox-bypass requests. Hosts may offer it as a broader alternative to approving the exact origin for the running session and, when the user chooses it, return it as the domain of an approve-for-session response. It is session-only: the runtime matches it by registrable domain, so a subdomain the Public Suffix List lists as a separate owner (for example a bucket under s3.amazonaws.com) is not covered, and the runtime never persists it. Do not offer it as a permanent approval; an approve-permanently response carrying this exact pattern is kept for the session only and writes nothing to settings. The pattern also matches the registrable domain itself.
    pub parent_domain_pattern: Option<String>,
    /// Immediately preceding URL when this prompt is for a redirect target
    pub redirected_from: Option<String>,
    /// True when the tool is asking to run this URL fetch outside the sandbox, after the network policy denied the approved URL or the sandbox proxy could not reach it (host opted in via sandbox.allowBypass). The model cannot ask for this; only the tool raises it. This is a request, not a grant: the fetch runs only if the user approves this permission request. Hosts should highlight the elevated risk in the approval UI.
    pub request_sandbox_bypass: Option<bool>,
    /// What the tool tells the user about the bypass on offer: which policy rule blocked the call, or why it cannot be sandboxed. Only meaningful when requestSandboxBypass is true.
    pub request_sandbox_bypass_reason: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// URL to be fetched
    pub url: String,
}
crate::record_serde::wire_record!(PermissionPromptRequestUrl { assisted_approval: "assistedApproval" if is_none, intention: "intention", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, parent_domain_pattern: "parentDomainPattern" if is_none, redirected_from: "redirectedFrom" if is_none, request_sandbox_bypass: "requestSandboxBypass" if is_none, request_sandbox_bypass_reason: "requestSandboxBypassReason" if is_none, tool_call_id: "toolCallId" if is_none, url: "url" });

/// Memory operation permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestMemory {
    /// Whether this is a store or vote memory operation
    pub action: Option<PermissionRequestMemoryAction>,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Source references for the stored fact (store only)
    pub citations: Option<String>,
    /// Vote direction (vote only)
    pub direction: Option<PermissionRequestMemoryDirection>,
    /// The fact being stored or voted on
    pub fact: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestMemoryKind,
    /// Reason for the vote (vote only)
    pub reason: Option<String>,
    /// Topic or subject of the memory (store only)
    pub subject: Option<String>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestMemory { action: "action" if is_none, assisted_approval: "assistedApproval" if is_none, citations: "citations" if is_none, direction: "direction" if is_none, fact: "fact", kind: "kind", reason: "reason" if is_none, subject: "subject" if is_none, tool_call_id: "toolCallId" if is_none });

/// Custom tool invocation permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestCustomTool {
    /// Arguments to pass to the custom tool
    pub args: Option<serde_json::Value>,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestCustomToolKind,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Description of what the custom tool does
    pub tool_description: String,
    /// Name of the custom tool
    pub tool_name: String,
}
crate::record_serde::wire_record!(PermissionPromptRequestCustomTool { args: "args" if is_none, assisted_approval: "assistedApproval" if is_none, kind: "kind", tool_call_id: "toolCallId" if is_none, tool_description: "toolDescription", tool_name: "toolName" });

/// Path access permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestPath {
    /// Underlying permission kind that needs path approval
    pub access_kind: PermissionPromptRequestPathAccessKind,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestPathKind,
    /// File paths that require explicit approval
    pub paths: Vec<String>,
    /// Canonical directory candidates that can be granted for file-tool read access in this logical session. Present only for read path prompts.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub read_only_directories: Option<Vec<String>>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestPath { access_kind: "accessKind", assisted_approval: "assistedApproval" if is_none, kind: "kind", paths: "paths", read_only_directories: "readOnlyDirectories" if is_none, tool_call_id: "toolCallId" if is_none });

/// Hook confirmation permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestHook {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Optional message from the hook explaining why confirmation is needed
    pub hook_message: Option<String>,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestHookKind,
    /// Arguments of the tool call being gated
    pub tool_args: Option<serde_json::Value>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
    /// Name of the tool the hook is gating
    pub tool_name: String,
}
crate::record_serde::wire_record!(PermissionPromptRequestHook { assisted_approval: "assistedApproval" if is_none, hook_message: "hookMessage" if is_none, kind: "kind", tool_args: "toolArgs" if is_none, tool_call_id: "toolCallId" if is_none, tool_name: "toolName" });

/// Extension management permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestExtensionManagement {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Name of the extension being managed
    pub extension_name: Option<String>,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestExtensionManagementKind,
    /// The extension management operation (scaffold, reload)
    pub operation: String,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestExtensionManagement { assisted_approval: "assistedApproval" if is_none, extension_name: "extensionName" if is_none, kind: "kind", operation: "operation", tool_call_id: "toolCallId" if is_none });

/// Workflow run or authoring permission prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestWorkflow {
    /// Canonical key used for scoped workflow approvals
    pub approval_key: String,
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Whether this workflow is eligible for persistent approval
    pub can_persist_approval: bool,
    /// Workflow-declared AI-credit limit before any run/resume caller override is applied.
    pub declared_max_ai_credits: Option<f64>,
    /// Workflow-declared concurrent-subagent limit before any run/resume caller override is applied.
    pub declared_max_concurrent_subagents: Option<i64>,
    /// Workflow-declared total-subagent limit before any run/resume caller override is applied.
    pub declared_max_total_subagents: Option<i64>,
    /// Workflow-declared active-time limit in seconds before any run/resume caller override is applied.
    pub declared_timeout_seconds: Option<f64>,
    /// Workflow description
    pub description: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestWorkflowKind,
    /// Whether managed policy requires a human response and forbids host auto-approval
    pub managed_approval_required: Option<bool>,
    /// Effective AI-credit limit; omitted means unlimited
    pub max_ai_credits: Option<f64>,
    /// Effective concurrent-subagent limit; omitted means unlimited
    pub max_concurrent_subagents: Option<i64>,
    /// Effective total-subagent limit; omitted means unlimited
    pub max_total_subagents: Option<i64>,
    /// Workflow name
    pub name: String,
    /// Workflow operation, either run or author
    pub operation: WorkflowPermissionOperation,
    /// Declared workflow phases
    pub phases: Vec<WorkflowPermissionPhase>,
    /// Effective active-time limit in seconds; omitted means unlimited
    pub timeout_seconds: Option<f64>,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestWorkflow { approval_key: "approvalKey", assisted_approval: "assistedApproval" if is_none, can_persist_approval: "canPersistApproval", declared_max_ai_credits: "declaredMaxAiCredits" if is_none, declared_max_concurrent_subagents: "declaredMaxConcurrentSubagents" if is_none, declared_max_total_subagents: "declaredMaxTotalSubagents" if is_none, declared_timeout_seconds: "declaredTimeoutSeconds" if is_none, description: "description", kind: "kind", managed_approval_required: "managedApprovalRequired" if is_none, max_ai_credits: "maxAiCredits" if is_none, max_concurrent_subagents: "maxConcurrentSubagents" if is_none, max_total_subagents: "maxTotalSubagents" if is_none, name: "name", operation: "operation", phases: "phases", timeout_seconds: "timeoutSeconds" if is_none, tool_call_id: "toolCallId" if is_none });

/// Extension permission access prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestExtensionPermissionAccess {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Capabilities the extension is requesting
    pub capabilities: Vec<String>,
    /// Name of the extension requesting permission access
    pub extension_name: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestExtensionPermissionAccessKind,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestExtensionPermissionAccess { assisted_approval: "assistedApproval" if is_none, capabilities: "capabilities", extension_name: "extensionName", kind: "kind", tool_call_id: "toolCallId" if is_none });

/// Extension sensitive environment variable access prompt
#[derive(Debug, Clone, Default)]
pub struct PermissionPromptRequestExtensionEnvAccess {
    /// Assisted-approval judge information for this request; present only in assisted mode.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub assisted_approval: Option<PermissionAssistedApproval>,
    /// Names of the sensitive environment variables the extension is requesting. Values never appear here.
    pub environment_variables: Vec<String>,
    /// Name of the extension requesting environment variable access
    pub extension_name: String,
    /// Prompt kind discriminator
    pub kind: PermissionPromptRequestExtensionEnvAccessKind,
    /// Tool call ID that triggered this permission request
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(PermissionPromptRequestExtensionEnvAccess { assisted_approval: "assistedApproval" if is_none, environment_variables: "environmentVariables", extension_name: "extensionName", kind: "kind", tool_call_id: "toolCallId" if is_none });

/// Session event "permission.requested". Permission request notification requiring client approval with request details
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionRequestedData {
    /// Agent mode captured from the owning turn when permission evaluation began.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_mode: Option<SessionMode>,
    /// Canonical runtime-owned Assisted Permissions assessment. New runtimes emit this for every permission request; absence means the event predates this field. Consumers must use `permission.completed` for the final permission result.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assisted_approval_assessment: Option<AssistedApprovalAssessment>,
    /// Experimental HydraFusion attribution of the staged phase whose tool call raised this request. Absent outside a staged phase.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fusion: Option<FusionAttribution>,
    /// Delegated agent that owns permission recovery when the owner session multiplexes remote agents. Absent otherwise.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner_agent_id: Option<String>,
    /// Execution session that owns permission recovery for this request. Absent on historical events.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner_session_id: Option<String>,
    /// Permission mode captured when evaluation began. Absent on historical events.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub permission_mode: Option<PermissionMode>,
    /// Details of the permission being requested
    pub permission_request: PermissionRequest,
    /// Derived user-facing permission prompt details for UI consumers
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt_request: Option<PermissionPromptRequest>,
    /// Permission-recovery episode that authorized this request to surface for interactive attention
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recovery_episode_id: Option<String>,
    /// Unique identifier for this permission request; used to respond via session.respondToPermission()
    pub request_id: RequestId,
    /// When true, this permission was already resolved by a permissionRequest hook and requires no client action
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_by_hook: Option<bool>,
    /// Neutral risk metadata supplied by the tool host. Consumers may display this value but must not use it to bypass the permission decision.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub risk_assessment: Option<serde_json::Value>,
}

/// Permission response variant indicating the request was approved without persisting an approval rule.
#[derive(Debug, Clone, Default)]
pub struct PermissionApproved {
    /// The permission request was approved
    pub kind: PermissionApprovedKind,
    /// Whether a managed approval policy already handled this request
    pub managed_approval_handled: Option<bool>,
}
crate::record_serde::wire_record!(PermissionApproved { kind: "kind", managed_approval_handled: "managedApprovalHandled" if is_none });

/// Permission response variant that approves a request and records file-tool read authority for specific directories in this logical session.
#[derive(Debug, Clone, Default)]
pub struct PermissionApprovedReadOnlyForSession {
    /// Canonical directories covered by the session read-only grant
    pub directories: Vec<String>,
    /// Approved with read-only directory authority for this session
    pub kind: PermissionApprovedReadOnlyForSessionKind,
}
crate::record_serde::wire_record!(PermissionApprovedReadOnlyForSession {
    directories: "directories",
    kind: "kind"
});

/// Session-scoped tool-approval rule for specific shell command identifiers.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalCommands {
    /// Command identifiers approved by the user
    pub command_identifiers: Vec<String>,
    /// Command approval kind
    pub kind: UserToolSessionApprovalCommandsKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalCommands {
    command_identifiers: "commandIdentifiers",
    kind: "kind"
});

/// Session-scoped tool-approval rule for read-only filesystem operations.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalRead {
    /// Read approval kind
    pub kind: UserToolSessionApprovalReadKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalRead { kind: "kind" });

/// Session-scoped tool-approval rule for filesystem write operations.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalWrite {
    /// Write approval kind
    pub kind: UserToolSessionApprovalWriteKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalWrite { kind: "kind" });

/// Session-scoped tool-approval rule for an MCP server tool, or all tools on the server when `toolName` is null.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalMcp {
    /// MCP tool approval kind
    pub kind: UserToolSessionApprovalMcpKind,
    /// MCP server name
    pub server_name: String,
    /// Optional MCP tool name, or null for all tools on the server
    pub tool_name: Option<String>,
}
crate::record_serde::wire_record!(UserToolSessionApprovalMcp {
    kind: "kind",
    server_name: "serverName",
    tool_name: "toolName"
});

/// Session-scoped tool-approval rule for writes to long-term memory.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalMemory {
    /// Memory approval kind
    pub kind: UserToolSessionApprovalMemoryKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalMemory { kind: "kind" });

/// Session-scoped tool-approval rule for a custom tool, keyed by tool name.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalCustomTool {
    /// Custom tool approval kind
    pub kind: UserToolSessionApprovalCustomToolKind,
    /// Custom tool name
    pub tool_name: String,
}
crate::record_serde::wire_record!(UserToolSessionApprovalCustomTool {
    kind: "kind",
    tool_name: "toolName"
});

/// Session-scoped tool-approval rule for extension-management operations, optionally narrowed by operation.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalExtensionManagement {
    /// Extension management approval kind
    pub kind: UserToolSessionApprovalExtensionManagementKind,
    /// Optional operation identifier
    pub operation: Option<String>,
}
crate::record_serde::wire_record!(UserToolSessionApprovalExtensionManagement { kind: "kind", operation: "operation" if is_none });

/// Session-scoped workflow approval, optionally narrowed by approval key.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalWorkflow {
    /// Optional workflow operation name or canonical approval key
    pub approval_key: Option<String>,
    /// Workflow approval kind
    pub kind: UserToolSessionApprovalWorkflowKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalWorkflow { approval_key: "approvalKey" if is_none, kind: "kind" });

/// Session-scoped tool-approval rule for an extension's permission-gated capability access, keyed by extension name.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalExtensionPermissionAccess {
    /// Extension name
    pub extension_name: String,
    /// Extension permission access approval kind
    pub kind: UserToolSessionApprovalExtensionPermissionAccessKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalExtensionPermissionAccess {
    extension_name: "extensionName",
    kind: "kind"
});

/// Session-scoped tool-approval rule for an extension's access to sensitive environment variables, keyed by extension name and the exact set of variable names.
#[derive(Debug, Clone, Default)]
pub struct UserToolSessionApprovalExtensionEnvAccess {
    /// Names of the sensitive environment variables this approval covers. Values are never persisted.
    pub environment_variables: Vec<String>,
    /// Extension name
    pub extension_name: String,
    /// Extension environment access approval kind
    pub kind: UserToolSessionApprovalExtensionEnvAccessKind,
}
crate::record_serde::wire_record!(UserToolSessionApprovalExtensionEnvAccess {
    environment_variables: "environmentVariables",
    extension_name: "extensionName",
    kind: "kind"
});

/// Permission response variant that approves a request and remembers the provided approval for the rest of the session.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionApprovedForSession {
    /// The approval to add as a session-scoped rule
    pub approval: UserToolSessionApproval,
    /// Approved and remembered for the rest of the session
    pub kind: PermissionApprovedForSessionKind,
    /// Whether a managed approval policy already handled this request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub managed_approval_handled: Option<bool>,
}

/// Permission response variant that approves a request and persists the provided approval to a project location key.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionApprovedForLocation {
    /// The approval to persist for this location
    pub approval: UserToolSessionApproval,
    /// Approved and persisted for this project location
    pub kind: PermissionApprovedForLocationKind,
    /// The location key (git root or cwd) to persist the approval to
    pub location_key: String,
    /// Whether a managed approval policy already handled this request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub managed_approval_handled: Option<bool>,
}

/// Permission response variant indicating the request was cancelled before use, with an optional reason.
#[derive(Debug, Clone, Default)]
pub struct PermissionCancelled {
    /// The permission request was cancelled before a response was used
    pub kind: PermissionCancelledKind,
    /// Optional explanation of why the request was cancelled
    pub reason: Option<String>,
}
crate::record_serde::wire_record!(PermissionCancelled { kind: "kind", reason: "reason" if is_none });

/// A permission approval or denial rule matched against a tool request, identified by a rule kind with an optional argument value.
#[derive(Debug, Clone, Default)]
pub struct PermissionRule {
    /// Argument value matched against the request, or null when the rule kind has no argument (e.g. 'read', 'write', 'memory').
    pub argument: Option<String>,
    /// The rule kind, such as Shell or GitHubMCP
    pub kind: String,
}
crate::record_serde::wire_record!(PermissionRule {
    argument: "argument",
    kind: "kind"
});

/// Permission response variant denied because matching approval rules explicitly blocked the request.
#[derive(Debug, Clone, Default)]
pub struct PermissionDeniedByRules {
    /// Denied because approval rules explicitly blocked it
    pub kind: PermissionDeniedByRulesKind,
    /// Rules that denied the request
    pub rules: Vec<PermissionRule>,
}
crate::record_serde::wire_record!(PermissionDeniedByRules {
    kind: "kind",
    rules: "rules"
});

/// Permission response variant denied because no approval rule matched and user confirmation was unavailable.
#[derive(Debug, Clone, Default)]
pub struct PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUser {
    /// Denied because no approval rule matched and user confirmation was unavailable
    pub kind: PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUserKind,
}
crate::record_serde::wire_record!(PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUser {
    kind: "kind"
});

/// Permission response variant for a rejection given in answer to a permission prompt, with optional feedback and force-reject flag. A person or an automated responder, such as the Assisted Permissions judge or a host policy, may have given it; the completion's `decisionSource` says which.
#[derive(Debug, Clone, Default)]
pub struct PermissionDeniedInteractivelyByUser {
    /// Optional feedback explaining the denial, from whoever gave it
    pub feedback: Option<String>,
    /// Whether to force-reject the current agent turn
    pub force_reject: Option<bool>,
    /// Rejected in answer to a permission prompt. Despite the name, this does not mean a person decided; read the completion's `decisionSource`
    pub kind: PermissionDeniedInteractivelyByUserKind,
}
crate::record_serde::wire_record!(PermissionDeniedInteractivelyByUser { feedback: "feedback" if is_none, force_reject: "forceReject" if is_none, kind: "kind" });

/// Permission response variant denying a path under content exclusion policy, with the path and message.
#[derive(Debug, Clone, Default)]
pub struct PermissionDeniedByContentExclusionPolicy {
    /// Denied by the organization's content exclusion policy
    pub kind: PermissionDeniedByContentExclusionPolicyKind,
    /// Human-readable explanation of why the path was excluded
    pub message: String,
    /// File path that triggered the exclusion
    pub path: String,
}
crate::record_serde::wire_record!(PermissionDeniedByContentExclusionPolicy {
    kind: "kind",
    message: "message",
    path: "path"
});

/// Permission response variant denied by a permission-request hook, with optional message and interrupt flag.
#[derive(Debug, Clone, Default)]
pub struct PermissionDeniedByPermissionRequestHook {
    /// Whether to interrupt the current agent turn
    pub interrupt: Option<bool>,
    /// Denied by a permission request hook registered by an extension or plugin
    pub kind: PermissionDeniedByPermissionRequestHookKind,
    /// Optional message from the hook explaining the denial
    pub message: Option<String>,
}
crate::record_serde::wire_record!(PermissionDeniedByPermissionRequestHook { interrupt: "interrupt" if is_none, kind: "kind", message: "message" if is_none });

/// Session event "permission.completed". Permission request completion notification signaling UI dismissal
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionCompletedData {
    /// Atomic structured blocked outcome when this permission response ended an Autopilot recovery episode unsuccessfully
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blocker: Option<TaskBlocker>,
    /// Who decided this permission request. Absent on completions recorded before this field existed, which consumers must treat as "not a human decision" rather than assuming one. Authorization records are minted only for `human_response`; an assisted-approval verdict, a host policy, an unattended fallback, and a hook resolution all produce the same `result` a person does, so this is the only field that distinguishes them.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decision_source: Option<PermissionDecisionSource>,
    /// Permission-recovery episode settled by this response, when the request was escalated by Autopilot
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recovery_episode_id: Option<String>,
    /// Request ID of the resolved permission request; clients should dismiss any UI for this request
    pub request_id: RequestId,
    /// The result of the permission request
    pub result: PermissionResult,
    /// Optional tool call ID associated with this permission prompt; clients may use it to correlate UI created from tool-scoped prompts
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
}

/// Session event "permission.carriedForward". Historical decode-only receipt from the retired Assisted Permissions authorization extractor. Current runtimes ignore it for permission decisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionCarriedForwardData {
    /// Always `authorization_carry_forward`. Stated explicitly so a consumer reading this event cannot mistake it for a human, host-policy, or assisted-approval decision.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub decision_source: PermissionDecisionSource,
    /// Identity of the prior authorization record that contained the proposal.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub record_id: String,
    /// Authorization edge minted for this admission. Not a prompt id: no prompt was raised, so no client should expect a request with this id.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub request_id: RequestId,
    /// Tool call this admission authorizes. Its execution receipts the prior grant, which is how a single-effect approval is spent rather than carried forward again.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub tool_call_id: String,
}
crate::record_serde::wire_record!(PermissionCarriedForwardData {
    decision_source: "decisionSource",
    record_id: "recordId",
    request_id: "requestId",
    tool_call_id: "toolCallId"
});

/// Session event "permission.messageAuthorization". Historical decode-only claim from the retired Assisted Permissions authorization extractor. Current runtimes preserve the payload but do not establish authority from it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionMessageAuthorizationData {
    /// The kind of effect authorized, as an action-class identifier.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub action_class: String,
    /// Whether the claim granted or denied authority.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub polarity: PermissionMessageAuthorizationPolarity,
    /// Deterministic identity of the record, derived from the turn and span offsets so re-extracting the same span mints nothing new.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub record_id: String,
    /// End byte offset of the authorizing span within the turn.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub span_end: i64,
    /// Start byte offset of the authorizing span within the turn.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub span_start: i64,
    /// Concrete named targets that appear verbatim inside the span.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub target_members: Option<Vec<String>>,
    /// The task the permission is scoped to, when the human named one.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub task: Option<String>,
    /// The human turn the quoted span was read from.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub turn_index: i64,
    /// The trusted version discriminator, when one exists. Exact shell-command grants carry the byte-identical commands grounded in the human span; world-derived classes carry a file object, remote tip, or runner only when that state was captured safely. An opaque object mirroring the runtime's adjacently-tagged resolution.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub world: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(PermissionMessageAuthorizationData { action_class: "actionClass", polarity: "polarity", record_id: "recordId", span_end: "spanEnd", span_start: "spanStart", target_members: "targetMembers" if is_none, task: "task" if is_none, turn_index: "turnIndex", world: "world" if is_none });

/// Session event "permission.messageAuthorizationRead". Historical decode-only extractor progress marker. Current runtimes do not run or resume extraction from it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionMessageAuthorizationReadData {
    /// Whether this read activates ongoing message-backed extraction. False for a contextual-assent-only pass while auto-approval is off, so unrelated future messages remain outside extraction.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub activates_extraction: Option<bool>,
    /// The human turn that was read by the proposer.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub turn_index: i64,
}
crate::record_serde::wire_record!(PermissionMessageAuthorizationReadData { activates_extraction: "activatesExtraction" if is_none, turn_index: "turnIndex" });

/// Session event "permission.messageAuthorizationDegraded". Historical decode-only degradation marker from the retired extractor. Current runtimes ignore it for permission decisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionMessageAuthorizationDegradedData {
    /// The human turn that could not be represented safely.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub turn_index: i64,
}
crate::record_serde::wire_record!(PermissionMessageAuthorizationDegradedData {
    turn_index: "turnIndex"
});

/// Session event "permission.assentDetected". Historical decode-only contextual-assent marker. Current runtimes do not project it into the conversation or permission flow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionAssentDetectedData {
    /// Permission request the likely assent may refer to. The runtime derives this from the preceding durable blocker; the human message and extraction model do not choose it.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub request_id: RequestId,
    /// Human turn whose text triggered the deterministic assent recognizer.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub turn_index: i64,
}
crate::record_serde::wire_record!(PermissionAssentDetectedData {
    request_id: "requestId",
    turn_index: "turnIndex"
});

/// Session event "permission.contextualAuthorization". Historical decode-only contextual authorization claim. Current runtimes preserve the payload but do not establish authority from it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionContextualAuthorizationData {
    /// Whether the contextual human span granted or denied authority.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub polarity: PermissionMessageAuthorizationPolarity,
    /// Deterministic identity of the contextual message grant.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub record_id: String,
    /// Original blocked permission request selected by deterministic event ordering, never by the extraction model.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub request_id: RequestId,
    /// End byte offset of the contextual decision span within the turn.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub span_end: i64,
    /// Start byte offset of the contextual decision span within the turn.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub span_start: i64,
    /// Human turn containing the contextual decision.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub turn_index: i64,
}
crate::record_serde::wire_record!(PermissionContextualAuthorizationData {
    polarity: "polarity",
    record_id: "recordId",
    request_id: "requestId",
    span_end: "spanEnd",
    span_start: "spanStart",
    turn_index: "turnIndex"
});

/// Session event "user_input.requested". User input request notification with question and optional predefined choices
#[derive(Debug, Clone, Default)]
pub struct UserInputRequestedData {
    /// Whether the user can provide a free-form text response in addition to predefined choices
    pub allow_freeform: Option<bool>,
    /// Predefined choices for the user to select from, if applicable
    pub choices: Option<Vec<String>>,
    /// The question or prompt to present to the user
    pub question: String,
    /// Unique identifier for this input request; used to respond via session.respondToUserInput()
    pub request_id: RequestId,
    /// The LLM-assigned tool call ID that triggered this request; used by remote UIs to correlate responses
    pub tool_call_id: Option<String>,
}
crate::record_serde::wire_record!(UserInputRequestedData { allow_freeform: "allowFreeform" if is_none, choices: "choices" if is_none, question: "question", request_id: "requestId", tool_call_id: "toolCallId" if is_none });

/// Session event "user_input.completed". User input request completion with the user's response
#[derive(Debug, Clone, Default)]
pub struct UserInputCompletedData {
    /// The user's answer to the input request
    pub answer: Option<String>,
    /// Request ID of the resolved user input request; clients should dismiss any UI for this request
    pub request_id: RequestId,
    /// Whether the answer was typed as free-form text rather than selected from choices
    pub was_freeform: Option<bool>,
}
crate::record_serde::wire_record!(UserInputCompletedData { answer: "answer" if is_none, request_id: "requestId", was_freeform: "wasFreeform" if is_none });

/// JSON Schema describing the form fields to present to the user (form mode only)
#[derive(Debug, Clone, Default)]
pub struct ElicitationRequestedSchema {
    /// Form field definitions, keyed by field name
    pub properties: HashMap<String, serde_json::Value>,
    /// List of required field names
    pub required: Option<Vec<String>>,
    /// Schema type indicator (always 'object')
    pub r#type: ElicitationRequestedSchemaType,
}
crate::record_serde::wire_record!(ElicitationRequestedSchema { properties: "properties", required: "required" if is_none, r#type: "type" });

/// Session event "elicitation.requested". Elicitation request; may be form-based (structured input) or URL-based (browser redirect)
#[derive(Debug, Clone, Default)]
pub struct ElicitationRequestedData {
    /// The source that initiated the request (MCP server name, or absent for agent-initiated)
    pub elicitation_source: Option<String>,
    /// Display label for a Connector-owned MCP server that initiated the request, from the trusted Connector catalog; absent for other sources. `elicitationSource` stays the identity.
    pub elicitation_source_display_name: Option<String>,
    /// Message describing what information is needed from the user
    pub message: String,
    /// Elicitation mode; "form" for structured input, "url" for browser-based. Defaults to "form" when absent.
    pub mode: Option<ElicitationRequestedMode>,
    /// JSON Schema describing the form fields to present to the user (form mode only)
    pub requested_schema: Option<ElicitationRequestedSchema>,
    /// Unique identifier for this elicitation request; used to respond via session.respondToElicitation()
    pub request_id: RequestId,
    /// Tool call ID from the LLM completion; used to correlate with CompletionChunk.toolCall.id for remote UIs
    pub tool_call_id: Option<String>,
    /// URL to open in the user's browser (url mode only)
    pub url: Option<String>,
}
crate::record_serde::wire_record!(ElicitationRequestedData { elicitation_source: "elicitationSource" if is_none, elicitation_source_display_name: "elicitationSourceDisplayName" if is_none, message: "message", mode: "mode" if is_none, requested_schema: "requestedSchema" if is_none, request_id: "requestId", tool_call_id: "toolCallId" if is_none, url: "url" if is_none });

/// Session event "elicitation.completed". Elicitation request completion with the user's response
#[derive(Debug, Clone, Default)]
pub struct ElicitationCompletedData {
    /// The user action: "accept" (submitted form), "decline" (explicitly refused), or "cancel" (dismissed)
    pub action: Option<ElicitationCompletedAction>,
    /// The submitted form data when action is 'accept'; keys match the requested schema fields
    pub content: Option<HashMap<String, serde_json::Value>>,
    /// Request ID of the resolved elicitation request; clients should dismiss any UI for this request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(ElicitationCompletedData { action: "action" if is_none, content: "content" if is_none, request_id: "requestId" });

/// Session event "sampling.requested". Sampling request from an MCP server; contains the server name and a requestId for correlation
#[derive(Debug, Clone, Default)]
pub struct SamplingRequestedData {
    /// The JSON-RPC request ID from the MCP protocol
    pub mcp_request_id: serde_json::Value,
    /// Unique identifier for this sampling request; used to respond via session.respondToSampling()
    pub request_id: RequestId,
    /// Display label for a Connector-owned MCP server, from the trusted Connector catalog; absent for other servers. `serverName` stays the identity for request matching.
    pub server_display_name: Option<String>,
    /// Name of the MCP server that initiated the sampling request
    pub server_name: String,
}
crate::record_serde::wire_record!(SamplingRequestedData { mcp_request_id: "mcpRequestId", request_id: "requestId", server_display_name: "serverDisplayName" if is_none, server_name: "serverName" });

/// Session event "sampling.completed". Sampling request completion notification signaling UI dismissal
#[derive(Debug, Clone, Default)]
pub struct SamplingCompletedData {
    /// Request ID of the resolved sampling request; clients should dismiss any UI for this request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(SamplingCompletedData {
    request_id: "requestId"
});

/// Single HTTP header entry as a name/value pair.
#[derive(Debug, Clone, Default)]
pub struct HeaderEntry {
    /// HTTP response header name as observed by the runtime.
    pub name: String,
    /// HTTP response header value as observed by the runtime.
    pub value: String,
}
crate::record_serde::wire_record!(HeaderEntry {
    name: "name",
    value: "value"
});

/// Raw HTTP response details from the OAuth auth challenge, as observed by the runtime.
#[derive(Debug, Clone, Default)]
pub struct McpOauthHttpResponse {
    /// Complete UTF-8 response body for host-specific challenge handling, including an empty string for an empty body. Omitted when the complete body is not valid UTF-8; body read failures fail the HTTP operation rather than exposing a partial response.
    pub body: Option<String>,
    /// HTTP response headers as observed by the runtime. Order and casing are transport-dependent, and duplicate header names may appear multiple times.
    pub headers: Vec<HeaderEntry>,
    /// HTTP status code returned with the auth challenge.
    pub status_code: i32,
}
crate::record_serde::wire_record!(McpOauthHttpResponse { body: "body" if is_none, headers: "headers", status_code: "statusCode" });

/// Static OAuth client configuration, if the server specifies one
#[derive(Debug, Clone, Default)]
pub struct McpOauthRequiredStaticClientConfig {
    /// OAuth client ID for the server
    pub client_id: String,
    /// Optional OAuth client secret for confidential static clients, when the runtime can resolve one
    pub client_secret: Option<String>,
    /// Optional non-default OAuth grant type. When set to 'client_credentials', the OAuth flow runs headlessly using the client_id + keychain-stored secret (no browser, no callback server).
    pub grant_type: Option<McpOauthRequiredStaticClientConfigGrantType>,
    /// Whether this is a public OAuth client
    pub public_client: Option<bool>,
    /// Configured OAuth scope string used when the server challenge omits scope or provides an empty scope
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(McpOauthRequiredStaticClientConfig { client_id: "clientId", client_secret: "clientSecret" if is_none, grant_type: "grantType" if is_none, public_client: "publicClient" if is_none, scope: "scope" if is_none });

/// OAuth WWW-Authenticate parameters parsed from an MCP auth challenge
#[derive(Debug, Clone, Default)]
pub struct McpOauthWWWAuthenticateParams {
    /// OAuth error from the WWW-Authenticate error parameter, if present
    pub error: Option<String>,
    /// Protected resource metadata URL from the WWW-Authenticate resource_metadata parameter, if present
    pub resource_metadata_url: Option<String>,
    /// Requested OAuth scopes from the WWW-Authenticate scope parameter, if present
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(McpOauthWWWAuthenticateParams { error: "error" if is_none, resource_metadata_url: "resourceMetadataUrl" if is_none, scope: "scope" if is_none });

/// Session event "mcp.oauth_required". OAuth authentication request for an MCP server
#[derive(Debug, Clone, Default)]
pub struct McpOauthRequiredData {
    /// Raw HTTP response details from the OAuth auth challenge, as observed by the runtime. Header order and casing are transport-dependent, and duplicate header names may appear multiple times.
    pub http_response: Option<McpOauthHttpResponse>,
    /// Why the runtime is requesting host-provided OAuth credentials.
    pub reason: McpOauthRequestReason,
    /// Unique identifier for this OAuth request; used to respond via session.mcp.oauth.handlePendingRequest
    pub request_id: RequestId,
    /// Raw OAuth protected-resource metadata document fetched for the MCP server, if available
    pub resource_metadata: Option<String>,
    /// Whether this Refresh request accepts kind runtimeRefresh to continue noninteractive grant renewal within its original authorization reservation. Missing or false means unsupported; the host must retain its legacy response behavior.
    pub runtime_refresh_supported: Option<bool>,
    /// Display name of the MCP server that requires OAuth
    pub server_name: String,
    /// URL of the MCP server that requires OAuth
    pub server_url: String,
    /// Static OAuth client configuration, if the server specifies one
    pub static_client_config: Option<McpOauthRequiredStaticClientConfig>,
    /// OAuth WWW-Authenticate parameters parsed from the auth challenge, if available
    pub www_authenticate_params: Option<McpOauthWWWAuthenticateParams>,
}
crate::record_serde::wire_record!(McpOauthRequiredData { http_response: "httpResponse" if is_none, reason: "reason", request_id: "requestId", resource_metadata: "resourceMetadata" if is_none, runtime_refresh_supported: "runtimeRefreshSupported" if is_none, server_name: "serverName", server_url: "serverUrl", static_client_config: "staticClientConfig" if is_none, www_authenticate_params: "wwwAuthenticateParams" if is_none });

/// Session event "mcp.oauth_completed". MCP OAuth request completion notification
#[derive(Debug, Clone, Default)]
pub struct McpOauthCompletedData {
    /// How the pending OAuth request was completed. An accepted runtimeRefresh handoff completes once at its authorization terminal: token when recovery succeeded, cancelled when it was cancelled or failed; see mcp.oauth_recovery_completed for the exact outcome.
    pub outcome: McpOauthCompletionOutcome,
    /// Request ID of the resolved OAuth request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(McpOauthCompletedData {
    outcome: "outcome",
    request_id: "requestId"
});

/// Session event "mcp.oauth_recovery_completed". Terminal outcome of a runtime-owned MCP OAuth recovery accepted through a runtimeRefresh handoff
#[derive(Debug, Clone, Default)]
pub struct McpOauthRecoveryCompletedData {
    /// Terminal outcome of the original authorization reservation, including reconnect
    pub outcome: McpOauthRecoveryCompletedOutcome,
    /// Request ID of the MCP OAuth request whose runtimeRefresh handoff was accepted
    pub request_id: RequestId,
    /// Name of the MCP server whose authorization recovery completed
    pub server_name: String,
}
crate::record_serde::wire_record!(McpOauthRecoveryCompletedData {
    outcome: "outcome",
    request_id: "requestId",
    server_name: "serverName"
});

/// Session event "mcp.headers_refresh_required". Dynamic headers refresh request for a remote MCP server
#[derive(Debug, Clone, Default)]
pub struct McpHeadersRefreshRequiredData {
    /// Why dynamic headers are being requested.
    pub reason: McpHeadersRefreshRequiredReason,
    /// Unique identifier for this headers refresh request; used to respond via session.mcp.headers.handlePendingHeadersRefreshRequest()
    pub request_id: RequestId,
    /// Display name of the remote MCP server requesting headers
    pub server_name: String,
    /// URL of the remote MCP server requesting headers
    pub server_url: String,
}
crate::record_serde::wire_record!(McpHeadersRefreshRequiredData {
    reason: "reason",
    request_id: "requestId",
    server_name: "serverName",
    server_url: "serverUrl"
});

/// Session event "mcp.headers_refresh_completed". MCP headers refresh request completion notification
#[derive(Debug, Clone, Default)]
pub struct McpHeadersRefreshCompletedData {
    /// How the pending MCP headers refresh request resolved.
    pub outcome: McpHeadersRefreshCompletedOutcome,
    /// Request ID of the resolved headers refresh request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(McpHeadersRefreshCompletedData {
    outcome: "outcome",
    request_id: "requestId"
});

/// Session event "session.custom_notification". Opaque custom notification data. Consumers may branch on source and name, but payload semantics are source-defined.
#[derive(Debug, Clone, Default)]
pub struct SessionCustomNotificationData {
    /// Source-defined custom notification name
    pub name: String,
    /// Source-defined JSON payload for the custom notification
    pub payload: serde_json::Value,
    /// Namespace for the custom notification producer
    pub source: String,
    /// Optional source-defined string identifiers describing the payload subject
    pub subject: Option<HashMap<String, String>>,
    /// Optional source-defined payload schema version
    pub version: Option<i64>,
}
crate::record_serde::wire_record!(SessionCustomNotificationData { name: "name", payload: "payload", source: "source", subject: "subject" if is_none, version: "version" if is_none });

/// Session event "ui.ephemeral_query". Ordered output and terminal state for a transient query that does not modify conversation history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UiEphemeralQueryData {
    /// Full response text, present for the `completed` phase.
    pub answer: Option<String>,
    /// Ordered text delta, present for the `chunk` phase.
    pub chunk: Option<String>,
    /// Model or transport failure message, present for the `failed` phase.
    pub error: Option<String>,
    /// Current query lifecycle phase.
    pub phase: UIEphemeralQueryPhase,
    /// Runtime-minted query identifier.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(UiEphemeralQueryData { answer: "answer" if is_none, chunk: "chunk" if is_none, error: "error" if is_none, phase: "phase", request_id: "requestId" });

/// Session event "external_tool.requested". External tool invocation request for client-side tool execution
#[derive(Debug, Clone, Default)]
pub struct ExternalToolRequestedData {
    /// Arguments to pass to the external tool
    pub arguments: Option<serde_json::Value>,
    /// Stable provider identity captured with an extension-owned tool definition; hosts use it to route the request to the same provider that was offered to the model
    pub provider_id: Option<String>,
    /// Unique identifier for this request; used to respond via session.respondToExternalTool()
    pub request_id: RequestId,
    /// Session ID that this external tool request belongs to
    pub session_id: SessionId,
    /// Tool call ID assigned to this external tool invocation
    pub tool_call_id: String,
    /// Name of the external tool to invoke
    pub tool_name: String,
    /// W3C Trace Context traceparent header for the execute_tool span
    pub traceparent: Option<String>,
    /// W3C Trace Context tracestate header for the execute_tool span
    pub tracestate: Option<String>,
    /// Active session working directory, when known.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(ExternalToolRequestedData { arguments: "arguments" if is_none, provider_id: "providerId" if is_none, request_id: "requestId", session_id: "sessionId", tool_call_id: "toolCallId", tool_name: "toolName", traceparent: "traceparent" if is_none, tracestate: "tracestate" if is_none, working_directory: "workingDirectory" if is_none });

/// Session event "external_tool.completed". External tool completion notification signaling UI dismissal
#[derive(Debug, Clone, Default)]
pub struct ExternalToolCompletedData {
    /// Request ID of the resolved external tool request; clients should dismiss any UI for this request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(ExternalToolCompletedData {
    request_id: "requestId"
});

/// Session event "command.queued". Queued slash command dispatch request for client execution
#[derive(Debug, Clone, Default)]
pub struct CommandQueuedData {
    /// The slash command text to be executed (e.g., /help, /clear)
    pub command: String,
    /// Unique identifier for this request; used to respond via session.respondToQueuedCommand()
    pub request_id: RequestId,
    /// Whether a schedule dispatched this command. Consumers must reject commands marked as not schedulable before executing a scheduled request. Absent for ordinary queued commands.
    pub scheduled: Option<bool>,
}
crate::record_serde::wire_record!(CommandQueuedData { command: "command", request_id: "requestId", scheduled: "scheduled" if is_none });

/// Session event "command.execute". Registered command dispatch request routed to the owning client
#[derive(Debug, Clone, Default)]
pub struct CommandExecuteData {
    /// Raw argument string after the command name
    pub args: String,
    /// The full command text (e.g., /deploy production)
    pub command: String,
    /// Command name without leading /
    pub command_name: String,
    /// Unique identifier; used to respond via session.commands.handlePendingCommand()
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(CommandExecuteData {
    args: "args",
    command: "command",
    command_name: "commandName",
    request_id: "requestId"
});

/// Session event "command.completed". Queued command completion notification signaling UI dismissal
#[derive(Debug, Clone, Default)]
pub struct CommandCompletedData {
    /// Request ID of the resolved command request; clients should dismiss any UI for this request
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(CommandCompletedData {
    request_id: "requestId"
});

/// Session event "auto_mode_switch.requested". Auto mode switch request notification requiring user approval
#[derive(Debug, Clone, Default)]
pub struct AutoModeSwitchRequestedData {
    /// The rate limit error code that triggered this request
    pub error_code: Option<String>,
    /// Unique identifier for this request; used to respond via session.respondToAutoModeSwitch()
    pub request_id: RequestId,
    /// Seconds until the rate limit resets, when known. Lets clients render a humanized reset time alongside the prompt.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(AutoModeSwitchRequestedData { error_code: "errorCode" if is_none, request_id: "requestId", retry_after_seconds: "retryAfterSeconds" if is_none });

/// Session event "auto_mode_switch.completed". Auto mode switch completion notification
#[derive(Debug, Clone, Default)]
pub struct AutoModeSwitchCompletedData {
    /// Request ID of the resolved request; clients should dismiss any UI for this request
    pub request_id: RequestId,
    /// The user's auto-mode-switch choice
    pub response: AutoModeSwitchResponse,
}
crate::record_serde::wire_record!(AutoModeSwitchCompletedData {
    request_id: "requestId",
    response: "response"
});

/// Session event "auto_mode_recovery_consent.requested". Recovery consent for a still-active request. Respond using session.ui.handlePendingAutoModeSwitch. The offer has no timer. Entering Autopilot before recovery admission withdraws it. Dismiss it on auto_mode_recovery_consent.completed or on session resume, rebuild, or disconnect; resume can invalidate it without a completed event.
#[derive(Debug, Clone, Default)]
pub struct AutoModeRecoveryConsentRequestedData {
    /// Whether this offer permits remembering consent with yes_always. Currently true only for per-model rate limits.
    pub allow_always: bool,
    /// Eligible upstream failure code: model_degraded, model_overloaded, or user_model_rate_limited.
    pub error_code: String,
    /// Original logical interaction identity, when available. Do not infer it from transcript order.
    pub interaction_id: Option<String>,
    /// Explicit model selected for the failed request before Auto recovery.
    pub original_model: String,
    /// Initiating user-message identity, when available, distinct from later steering messages.
    pub originating_message_id: Option<String>,
    /// One-use offer or recovery episode identifier bound to the originating live session. It cannot be carried across resume or used to replay a settled request.
    pub request_id: RequestId,
    /// Rate-limit reset information when supplied by the provider. This is not a consent deadline.
    pub retry_after_seconds: Option<f64>,
}
crate::record_serde::wire_record!(AutoModeRecoveryConsentRequestedData { allow_always: "allowAlways", error_code: "errorCode", interaction_id: "interactionId" if is_none, original_model: "originalModel", originating_message_id: "originatingMessageId" if is_none, request_id: "requestId", retry_after_seconds: "retryAfterSeconds" if is_none });

/// Session event "auto_mode_recovery_consent.completed". Recovery consent offer closed; clients must dismiss its UI. Completion does not imply consent or recovery success. Events belong to the originating live session; resume can invalidate an offer without this event.
#[derive(Debug, Clone, Default)]
pub struct AutoModeRecoveryConsentCompletedData {
    /// Settlement reason, distinguishing user acceptance/decline and saved-preference acceptance from cancellation, handler loss, supersession, and emergency disablement. mode_changed withdraws an unanswered offer when the session enters Autopilot before recovery admission.
    pub reason: String,
    /// Identifier of the settled offer; clients must dismiss the matching consent UI.
    pub request_id: RequestId,
    /// User response when one was supplied. Absent when the runtime settled the offer without a user answer.
    pub response: Option<AutoModeSwitchResponse>,
}
crate::record_serde::wire_record!(AutoModeRecoveryConsentCompletedData { reason: "reason", request_id: "requestId", response: "response" if is_none });

/// Session event "auto_mode_recovery.outcome". Authoritative final outcome of an active-request Auto recovery episode, not a retry-start notification or manual response regeneration. Normal response/error and session completion signals still settle the logical request.
#[derive(Debug, Clone, Default)]
pub struct AutoModeRecoveryOutcomeData {
    /// Whether a recovered Auto inference was allowed to proceed, reported on the final outcome rather than as a live start notification. True does not imply success. False when consent or preparation ended without a retry.
    pub auto_mode_recovery_admitted: bool,
    /// Original upstream failure code that qualified the request for recovery.
    pub error_code: String,
    /// Explicit model selected for the original failed request.
    pub original_model: String,
    /// Recovery episode identifier, also used by its consent offer when one was displayed.
    pub request_id: RequestId,
    /// Final recovery disposition, such as succeeded, failed, declined, cancelled, superseded, or unavailable. mode_changed means entering Autopilot stopped recovery before admission, including after consent was accepted.
    pub status: String,
}
crate::record_serde::wire_record!(AutoModeRecoveryOutcomeData {
    auto_mode_recovery_admitted: "autoModeRecoveryAdmitted",
    error_code: "errorCode",
    original_model: "originalModel",
    request_id: "requestId",
    status: "status"
});

/// Session event "session_limits_exhausted.requested". Session limit exhaustion notification requiring user action.
#[derive(Debug, Clone, Default)]
pub struct SessionLimitsExhaustedRequestedData {
    /// Configured max AI Credits for the current accounting window.
    pub max_ai_credits: f64,
    /// Unique identifier for this request; used to respond via session.ui.handlePendingSessionLimitsExhausted().
    pub request_id: RequestId,
    /// AI Credits already consumed in the current accounting window.
    pub used_ai_credits: f64,
}
crate::record_serde::wire_record!(SessionLimitsExhaustedRequestedData {
    max_ai_credits: "maxAiCredits",
    request_id: "requestId",
    used_ai_credits: "usedAiCredits"
});

/// The user's selected action for an exhausted session limit.
#[derive(Debug, Clone, Default)]
pub struct SessionLimitsExhaustedResponse {
    /// Action selected by the user.
    pub action: SessionLimitsExhaustedResponseAction,
    /// AI Credits to add to the current max when action is 'add'.
    pub additional_ai_credits: Option<f64>,
    /// New absolute max AI Credits when action is 'set'.
    pub max_ai_credits: Option<f64>,
}
crate::record_serde::wire_record!(SessionLimitsExhaustedResponse { action: "action", additional_ai_credits: "additionalAiCredits" if is_none, max_ai_credits: "maxAiCredits" if is_none });

/// Session event "session_limits_exhausted.completed". Session limit exhaustion prompt completion notification.
#[derive(Debug, Clone, Default)]
pub struct SessionLimitsExhaustedCompletedData {
    /// Request ID of the resolved request; clients should dismiss any UI for this request.
    pub request_id: RequestId,
    /// The user's selected session-limit action.
    pub response: SessionLimitsExhaustedResponse,
}
crate::record_serde::wire_record!(SessionLimitsExhaustedCompletedData {
    request_id: "requestId",
    response: "response"
});

/// Session event "session.auto_mode_resolved". Auto or Fusion Single resolution: the concrete model the session settled on, with an optional explanation. Published when routing selects a model that differs from the one last published in the current selection; selecting a model, switching Auto tiers, or returning to Auto starts a new selection. Does not change the user's selected model or tier. Lets SDK clients render the chosen model and its selection notice. The core selection fields (chosenModel/reasoningBucket/categoryScores) are stable; the routing-analytics fields (predictedLabel/confidence/candidateModels) mirror the upstream intent service and may evolve, hence the event's experimental stability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionAutoModeResolvedData {
    /// The effective Auto routing preference at publication, including managed defaults. Recorded so resume can distinguish an unchanged preference from a tier switch. Absent when no preference is known or in older session histories.
    pub auto_tier: Option<AutoTier>,
    /// Models offered to the router for this resolution
    pub available_models: Option<Vec<String>>,
    /// Ordered candidate model list the router returned, when not a fallback
    pub candidate_models: Option<Vec<String>>,
    /// Per-category classifier scores (0-1) behind the bucket: the granular HYDRA capability scores (reasoning, code_gen, debugging, tool_use), or the binary needs_reasoning/no_reasoning scores when HYDRA didn't run. Lets clients show a breakdown rather than just the bucket.
    pub category_scores: Option<HashMap<String, f64>>,
    /// The concrete model the session will use after any intent refinement
    pub chosen_model: String,
    /// The chosen model's score shortfall relative to the top candidate
    pub chosen_shortfall: Option<f64>,
    /// Classifier confidence for the predicted label, when available
    pub confidence: Option<f64>,
    /// End-to-end client wait time for the router request in milliseconds
    pub end_to_end_latency_ms: Option<f64>,
    /// Whether the router fell back to the standard Auto selection
    pub fallback: Option<bool>,
    /// Server-provided reason for falling back, when available
    pub fallback_reason: Option<String>,
    /// Whether the routed prompt contained an image
    pub has_image: Option<bool>,
    /// The predicted classifier label (e.g. `needs_reasoning`), when available
    pub predicted_label: Option<String>,
    /// Coarse request-difficulty bucket, for explaining why a model was chosen ("picked X because this looks like high-reasoning work")
    pub reasoning_bucket: Option<AutoModeResolvedReasoningBucket>,
    /// Server-reported router processing time in milliseconds
    pub router_latency_ms: Option<f64>,
    /// The routing method the server applied, when Auto Intent ran
    pub routing_method: Option<String>,
    /// Short human-readable selection notice. Auto forwards the routing service's explanation when supplied; Fusion Single supplies a neutral announcement of the selected model, without inventing a routing rationale. Otherwise omitted when no explanation is available. The text is display-only: several distinct routing categories share identical wording, so it cannot be used to recover the category or keyed on programmatically.
    pub selection_reason: Option<String>,
    /// Whether a sticky model choice overrode the router result
    pub sticky_override: Option<bool>,
}
crate::record_serde::wire_record!(SessionAutoModeResolvedData { auto_tier: "autoTier" if is_none, available_models: "availableModels" if is_none, candidate_models: "candidateModels" if is_none, category_scores: "categoryScores" if is_none, chosen_model: "chosenModel", chosen_shortfall: "chosenShortfall" if is_none, confidence: "confidence" if is_none, end_to_end_latency_ms: "endToEndLatencyMs" if is_none, fallback: "fallback" if is_none, fallback_reason: "fallbackReason" if is_none, has_image: "hasImage" if is_none, predicted_label: "predictedLabel" if is_none, reasoning_bucket: "reasoningBucket" if is_none, router_latency_ms: "routerLatencyMs" if is_none, routing_method: "routingMethod" if is_none, selection_reason: "selectionReason" if is_none, sticky_override: "stickyOverride" if is_none });

/// Session event "session.managed_plugin_progress". Experimental transient presentation-neutral progress for organization-required plugin preparation. Clients should localize the phase copy and display plugin specs without translating them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionManagedPluginProgressData {
    /// Current preparation phase.
    pub phase: ManagedPluginProgressPhase,
    /// Ordered plugin install specs affected by this phase. Empty while managed settings initialize and when preparation completes.
    pub plugin_specs: Vec<String>,
}
crate::record_serde::wire_record!(SessionManagedPluginProgressData {
    phase: "phase",
    plugin_specs: "pluginSpecs"
});

/// Reusable managed permission evaluation context. Treat permissions as runtime-owned policy data and retain it verbatim; it includes source-aware composition metadata. This is a snapshot, not a capability or approval token; only use contexts obtained from trusted policy sources.
#[derive(Debug, Clone, Default)]
pub struct ManagedPermissionsContext {
    /// Policy could not be determined; evaluation must deny every operation.
    pub fail_closed: bool,
    /// Runtime-owned composed permissions object. Absent when no permission policy is configured.
    pub permissions: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(ManagedPermissionsContext { fail_closed: "failClosed", permissions: "permissions" if is_none });

/// Policy Helper failure and recovery features implemented by this runtime. Capability presence, not absence of an optional failure, determines whether a client may rely on these contracts.
#[derive(Debug, Clone, Default)]
pub struct PolicyHelperCapabilities {
    /// The runtime supports session.managedSettings.recheckPolicyHelper for a fresh scoped helper evaluation.
    pub recheck: bool,
    /// The runtime emits PolicyHelperFailure on shared managed-settings snapshots and applicable structured operation errors.
    pub structured_failure: bool,
}
crate::record_serde::wire_record!(PolicyHelperCapabilities {
    recheck: "recheck",
    structured_failure: "structuredFailure"
});

/// One bounded field-specific finding reported by a Policy Helper. The path is a helper-defined logical configuration path, not necessarily a filesystem path.
#[derive(Debug, Clone, Default)]
pub struct PolicyHelperFailureDetail {
    /// Bounded helper-authored plain-text explanation for this finding.
    pub message: String,
    /// Optional helper-defined logical configuration path.
    pub path: Option<String>,
}
crate::record_serde::wire_record!(PolicyHelperFailureDetail { message: "message", path: "path" if is_none });

/// An explicit bounded absolute HTTP(S) destination supplied by a Policy Helper. The runtime validates but never fetches, previews, resolves redirects for, or opens it; clients must sanitize it again before navigation.
#[derive(Debug, Clone, Default)]
pub struct PolicyHelperHelpLink {
    /// Optional bounded helper-authored plain-text label. Clients may use localized fallback text when absent.
    pub label: Option<String>,
    /// Validated absolute HTTP(S) URL.
    pub url: String,
}
crate::record_serde::wire_record!(PolicyHelperHelpLink { label: "label" if is_none, url: "url" });

/// The latest unresolved Policy Helper failure for this policy context. Helper-authored text is bounded display-intended content but remains untrusted; render it as plain text, keep diagnostics secondary, and do not automatically log, persist, export, execute, or inject it into model context.
#[derive(Debug, Clone, Default)]
pub struct PolicyHelperFailure {
    /// Managed authority that registered the helper, when known. Known values are device and server; clients must tolerate future values.
    pub authority: Option<String>,
    /// Stable runtime classification. Known values are helper-reported, registration-undetermined, working-directory, resolve, resolve-task, spawn, timeout, exit-status, output-too-large, envelope, schema, and internal. This is not a closed set; clients must tolerate unknown values.
    pub category: String,
    /// Stable bounded helper-defined identifier for a valid structured helper error. Absent for runtime-detected failures and not globally unique.
    pub code: Option<String>,
    /// Optional bounded field-specific findings supplied by the helper.
    pub details: Option<Vec<PolicyHelperFailureDetail>>,
    /// Optional bounded untrusted secondary troubleshooting text, such as trimmed stderr. Do not use it as primary UI copy.
    pub diagnostic: Option<String>,
    /// Effective runtime behavior: ignore continues with remaining or retained policy, while failClosed activates restrictive safeguards. This is runtime-owned, not helper-authored.
    pub failure_mode: String,
    /// Optional explicit validated additional-information link. URLs embedded in other text are not links.
    pub help_link: Option<PolicyHelperHelpLink>,
    /// Bounded plain-text primary explanation. Runtime-authored for runtime failures and helper-authored for a valid structured helper error.
    pub message: String,
    /// True when an earlier successful helper-derived layer still contributes to this effective snapshot. This does not promise that every relaxation from the old layer was retained.
    pub previous_policy_retained: bool,
    /// Invocation reason. Known values are sessionStart and pluginActivation; clients must tolerate future values.
    pub reason: String,
    /// Optional bounded helper-authored plain-text recovery guidance. It is explanatory and must not be executed automatically.
    pub remediation: Option<String>,
    /// Runtime-calculated presentation severity: warning for ignore and error for failClosed. Error severity does not imply that session creation failed.
    pub severity: String,
}
crate::record_serde::wire_record!(PolicyHelperFailure { authority: "authority" if is_none, category: "category", code: "code" if is_none, details: "details" if is_none, diagnostic: "diagnostic" if is_none, failure_mode: "failureMode", help_link: "helpLink" if is_none, message: "message", previous_policy_retained: "previousPolicyRetained", reason: "reason", remediation: "remediation" if is_none, severity: "severity" });

/// Session event "session.managed_settings_resolved". Effective enterprise managed settings applied to the session and their contributing channels. Emitted whenever managed policy is applied or reapplied, including session start, resume, and account switch. This ephemeral live snapshot is delivered to subscribers but not persisted to the session event log; initial resolution occurs before session.start.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionManagedSettingsResolvedData {
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
crate::record_serde::wire_record!(SessionManagedSettingsResolvedData { bypass_permissions_disabled: "bypassPermissionsDisabled", client_managed: "clientManaged" if is_none, device_managed: "deviceManaged", fail_closed: "failClosed", managed_keys: "managedKeys", permissions_allow_intersected: "permissionsAllowIntersected" if is_none, permissions_context: "permissionsContext" if is_none, policy_helper_capabilities: "policyHelperCapabilities" if is_none, policy_helper_failure: "policyHelperFailure" if is_none, policy_helper_managed: "policyHelperManaged" if is_none, sandbox_enabled_by_undetermined_policy: "sandboxEnabledByUndeterminedPolicy" if is_none, server_managed: "serverManaged", settings: "settings" if is_none, source: "source" });

/// Session event "session.managed_settings_enforced". Runtime enforcement of enterprise managed settings: fires when the session blocks or caps a runtime action because enterprise policy governs it, so SDK clients can explain *why* an action was governed. Unlike `session.managed_settings_resolved` (which reports *what* is managed), this reports a concrete governed action — e.g. a user or host tried to turn on bypass permissions or Assisted Permissions while the corresponding policy disables it. Emitted live (not persisted to the session event log) on user/host-initiated attempts only, never for silent policy application. Marked experimental while the managed-settings surface stabilizes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionManagedSettingsEnforcedData {
    /// The category of runtime action that managed policy governed.
    pub action: ManagedSettingsEnforcedAction,
    /// For a `bypass_permissions_blocked` action, which permission-mode or escalation primitive was refused. Absent for actions without a specific primitive.
    pub escalation: Option<ManagedSettingsEnforcedEscalation>,
    /// Whether the enforcement was forced by fail-closed handling (managed policy could not be determined) rather than an explicit managed setting. When true, `setting` still names the restriction that was applied.
    pub fail_closed: bool,
    /// A human-readable explanation of why the action was governed, suitable for surfacing to the user.
    pub message: String,
    /// The managed setting key responsible for the enforcement (for example `permissions.disableBypassPermissionsMode` or `permissions.disableAssistedPermissionsMode`).
    pub setting: String,
}
crate::record_serde::wire_record!(SessionManagedSettingsEnforcedData { action: "action", escalation: "escalation" if is_none, fail_closed: "failClosed", message: "message", setting: "setting" });

/// A single slash command available in the session, as listed by the `commands.changed` event.
#[derive(Debug, Clone, Default)]
pub struct CommandsChangedCommand {
    /// Optional human-readable command description.
    pub description: Option<String>,
    /// Slash command name without the leading slash.
    pub name: String,
}
crate::record_serde::wire_record!(CommandsChangedCommand { description: "description" if is_none, name: "name" });

/// Session event "commands.changed". Session command list change notification, for SDK command registration or built-in command availability; `session.commands.list` returns the full list
#[derive(Debug, Clone, Default)]
pub struct CommandsChangedData {
    /// Current list of registered SDK commands
    pub commands: Vec<CommandsChangedCommand>,
}
crate::record_serde::wire_record!(CommandsChangedData {
    commands: "commands"
});

/// UI capability changes
#[derive(Debug, Clone, Default)]
pub struct CapabilitiesChangedUI {
    /// Whether a recovery-aware Auto-switch responder is registered. This does not imply current eligibility or override model policy.
    pub auto_mode_recovery: Option<bool>,
    /// Whether canvas rendering is now supported
    pub canvases: Option<bool>,
    /// Whether elicitation is now supported
    pub elicitation: Option<bool>,
    /// Whether MCP Apps (SEP-1865) UI passthrough is now supported
    pub mcp_apps: Option<bool>,
}
crate::record_serde::wire_record!(CapabilitiesChangedUI { auto_mode_recovery: "autoModeRecovery" if is_none, canvases: "canvases" if is_none, elicitation: "elicitation" if is_none, mcp_apps: "mcpApps" if is_none });

/// Session event "capabilities.changed". Session capability change notification
#[derive(Debug, Clone, Default)]
pub struct CapabilitiesChangedData {
    /// UI capability changes
    pub ui: Option<CapabilitiesChangedUI>,
}
crate::record_serde::wire_record!(CapabilitiesChangedData { ui: "ui" if is_none });

/// Session event "exit_plan_mode.requested". Plan approval request with plan content and available user actions
#[derive(Debug, Clone, Default)]
pub struct ExitPlanModeRequestedData {
    /// Available actions the user can take
    pub actions: Vec<ExitPlanModeAction>,
    /// Model the session had selected when the plan was authored, when one is known
    pub model: Option<String>,
    /// Full content of the plan file
    pub plan_content: String,
    /// Recommended action to preselect for the user
    pub recommended_action: ExitPlanModeAction,
    /// Unique identifier for this request; used to respond via session.respondToExitPlanMode()
    pub request_id: RequestId,
    /// Summary of the plan that was created
    pub summary: String,
}
crate::record_serde::wire_record!(ExitPlanModeRequestedData { actions: "actions", model: "model" if is_none, plan_content: "planContent", recommended_action: "recommendedAction", request_id: "requestId", summary: "summary" });

/// Session event "exit_plan_mode.completed". Plan mode exit completion with the user's approval decision and optional feedback
#[derive(Debug, Clone, Default)]
pub struct ExitPlanModeCompletedData {
    /// Whether the plan was approved by the user
    pub approved: Option<bool>,
    /// Whether edits should be auto-approved without confirmation
    pub auto_approve_edits: Option<bool>,
    /// Free-form feedback from the user if they requested changes to the plan
    pub feedback: Option<String>,
    /// Request ID of the resolved exit plan mode request; clients should dismiss any UI for this request
    pub request_id: RequestId,
    /// Action selected by the user
    pub selected_action: Option<ExitPlanModeAction>,
}
crate::record_serde::wire_record!(ExitPlanModeCompletedData { approved: "approved" if is_none, auto_approve_edits: "autoApproveEdits" if is_none, feedback: "feedback" if is_none, request_id: "requestId", selected_action: "selectedAction" if is_none });

#[derive(Debug, Clone, Default)]
pub struct HumanResponseRecordedResponseAskUser {
    /// How the person refused the question; absent for an accepted answer.
    pub action: Option<HumanResponseAskUserAction>,
    /// Exact answer content accepted from the user; empty when the person refused the question.
    pub content: HashMap<String, serde_json::Value>,
    /// Exact question displayed to the user.
    pub message: String,
    /// Exact response schema displayed to the user.
    pub requested_schema: ElicitationRequestedSchema,
    /// Runtime-owned response kind discriminator.
    pub response_kind: HumanResponseRecordedResponseAskUserResponseKind,
}
crate::record_serde::wire_record!(HumanResponseRecordedResponseAskUser { action: "action" if is_none, content: "content", message: "message", requested_schema: "requestedSchema", response_kind: "responseKind" });

#[derive(Debug, Clone, Default)]
pub struct HumanResponseRecordedResponseUserInput {
    /// Whether the displayed request allowed a free-form answer.
    pub allow_freeform: Option<bool>,
    /// Exact selected or free-form answer submitted by the user; empty when the person dismissed the question.
    pub answer: String,
    /// Exact choices displayed to the user, when the request offered choices.
    pub choices: Option<Vec<String>>,
    /// True when the person dismissed the question instead of answering it.
    pub dismissed: Option<bool>,
    /// Exact question displayed to the user.
    pub question: String,
    /// Runtime-owned response kind discriminator.
    pub response_kind: HumanResponseRecordedResponseUserInputResponseKind,
    /// Whether the answer was typed as free-form text rather than selected from the displayed choices.
    pub was_freeform: bool,
}
crate::record_serde::wire_record!(HumanResponseRecordedResponseUserInput { allow_freeform: "allowFreeform" if is_none, answer: "answer", choices: "choices" if is_none, dismissed: "dismissed" if is_none, question: "question", response_kind: "responseKind", was_freeform: "wasFreeform" });

#[derive(Debug, Clone, Default)]
pub struct HumanResponseRecordedResponseExitPlanMode {
    /// Actions offered by the plan review UI.
    pub actions: Vec<ExitPlanModeAction>,
    /// Whether the user approved the reviewed plan.
    pub approved: bool,
    /// Whether the selected response requested edit auto-approval.
    pub auto_approve_edits: Option<bool>,
    /// Exact feedback submitted with the plan decision, when present.
    pub feedback: Option<String>,
    /// Exact full plan content available from the review UI.
    pub plan_content: String,
    /// Action the plan review UI recommended.
    pub recommended_action: ExitPlanModeAction,
    /// Runtime-owned response kind discriminator.
    pub response_kind: HumanResponseRecordedResponseExitPlanModeResponseKind,
    /// Action the runtime applied for an approved plan: the user's selection, or the default an approval without one applies.
    pub selected_action: Option<ExitPlanModeAction>,
    /// Exact plan summary displayed to the user.
    pub summary: String,
}
crate::record_serde::wire_record!(HumanResponseRecordedResponseExitPlanMode { actions: "actions", approved: "approved", auto_approve_edits: "autoApproveEdits" if is_none, feedback: "feedback" if is_none, plan_content: "planContent", recommended_action: "recommendedAction", response_kind: "responseKind", selected_action: "selectedAction" if is_none, summary: "summary" });

/// Session event "human_response.recorded". Durable request-correlated evidence for a typed response to a runtime-owned question or plan review.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HumanResponseRecordedData {
    /// Controlled actor provenance established at response ingress.
    pub actor: HumanResponseActor,
    /// Request ID of the runtime-owned question or plan review.
    pub request_id: RequestId,
    /// Typed request and response payload.
    pub response: HumanResponseRecordedResponse,
    /// Tool call ID that opened the request, when present.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
}

/// Session event "session.tools_updated". Payload of `session.tools_updated` identifying the model whose resolved tools were updated.
#[derive(Debug, Clone, Default)]
pub struct SessionToolsUpdatedData {
    /// Identifier of the model the resolved tools apply to.
    pub model: String,
}
crate::record_serde::wire_record!(SessionToolsUpdatedData { model: "model" });

/// Session event "session.background_tasks_changed". Empty payload for `session.background_tasks_changed`, indicating background task state changed.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionBackgroundTasksChangedData {}

/// Session event "workflow.run_updated". Ephemeral invalidation signal for a changed workflow run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunUpdatedData {
    /// Monotonic revision now available for the run.
    pub revision: i64,
    /// Workflow run identifier.
    pub run_id: String,
}
crate::record_serde::wire_record!(WorkflowRunUpdatedData {
    revision: "revision",
    run_id: "runId"
});

/// Session event "workflow.run_started". Ephemeral signal that a workflow run attempt began executing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunStartedData {
    /// Attempt number this start committed; a resumed run increments it.
    pub attempt: i64,
    /// Identifier of the workflow run that started.
    pub run_id: String,
    /// Name of the workflow this run executes. Low cardinality by construction.
    pub workflow_name: String,
}
crate::record_serde::wire_record!(WorkflowRunStartedData {
    attempt: "attempt",
    run_id: "runId",
    workflow_name: "workflowName"
});

/// Session event "workflow.run_settled". Ephemeral signal that a workflow run reached a terminal status.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct WorkflowRunSettledData {
    /// AI credits this run consumed, in nano-AIU.
    pub consumed_nano_aiu: i64,
    /// Subagents this run consumed against its limits.
    pub consumed_subagents: i64,
    /// Active milliseconds accumulated across every attempt of this run.
    pub elapsed_ms: i64,
    /// Typed failure class recorded on the run, when it failed with one (e.g. `workflow_limit_reached`).
    pub failure_type: Option<String>,
    /// Identifier of the workflow run that settled.
    pub run_id: String,
    /// Terminal status the run committed.
    pub status: WorkflowRunSettledStatus,
}
crate::record_serde::wire_record!(WorkflowRunSettledData { consumed_nano_aiu: "consumedNanoAiu", consumed_subagents: "consumedSubagents", elapsed_ms: "elapsedMs", failure_type: "failureType" if is_none, run_id: "runId", status: "status" });

/// Public MCP skill provenance. The server name and resource URI address session.mcp.resources.read without invoking the skill; they do not grant permission or identify a durable server registration.
#[derive(Debug, Clone, Default)]
pub struct McpSkillOrigin {
    /// Name of the MCP server hosting the skill, matching serverName in session.mcp.resources.read.
    pub server_name: String,
    /// Exact URI of the skill's SKILL.md resource, matching uri in session.mcp.resources.read.
    pub uri: String,
}
crate::record_serde::wire_record!(McpSkillOrigin {
    server_name: "serverName",
    uri: "uri"
});

/// A single resolved skill in `session.skills_loaded`, including source, invocability, enabled state, path, and argument hint.
#[derive(Debug, Clone, Default)]
pub struct SkillsLoadedSkill {
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
    /// Absolute path to the skill file, if available
    pub path: Option<String>,
    /// Source location type (e.g., project, personal-copilot, plugin, builtin, remote, sdk)
    pub source: SkillSource,
    /// Whether the skill can be invoked by the user as a slash command
    pub user_invocable: bool,
}
crate::record_serde::wire_record!(SkillsLoadedSkill { argument_hint: "argumentHint" if is_none, command_name: "commandName" if is_none, declared_name: "declaredName" if is_none, description: "description", enabled: "enabled", mcp_origin: "mcpOrigin" if is_none, name: "name", path: "path" if is_none, source: "source", user_invocable: "userInvocable" });

/// Session event "session.skills_loaded". Payload of `session.skills_loaded` listing resolved skill metadata.
#[derive(Debug, Clone, Default)]
pub struct SessionSkillsLoadedData {
    /// Array of resolved skill metadata
    pub skills: Vec<SkillsLoadedSkill>,
}
crate::record_serde::wire_record!(SessionSkillsLoadedData { skills: "skills" });

/// A single loaded custom agent in `session.custom_agents_updated`, with identity, source, tools, invocability, and authored model configuration.
#[derive(Debug, Clone, Default)]
pub struct CustomAgentsUpdatedAgent {
    /// Description of what the agent does
    pub description: String,
    /// Whether model-driven invocation is disabled for this agent.
    pub disable_model_invocation: Option<bool>,
    /// Human-readable display name
    pub display_name: String,
    /// Unique identifier for the agent
    pub id: String,
    /// Model override for this agent, if set
    pub model: Option<String>,
    /// Whether authored models are preferences or required constraints
    pub model_policy: Option<AgentModelPolicy>,
    /// Authored model ids in priority order, if configured
    pub models: Option<Vec<String>>,
    /// Internal name of the agent
    pub name: String,
    /// Source location: user, project, inherited, remote, or plugin
    pub source: String,
    /// List of tool names available to this agent, or null when all tools are available
    pub tools: Option<Vec<String>>,
    /// Whether the agent can be selected by the user
    pub user_invocable: bool,
}
crate::record_serde::wire_record!(CustomAgentsUpdatedAgent { description: "description", disable_model_invocation: "disableModelInvocation" if is_none, display_name: "displayName", id: "id", model: "model" if is_none, model_policy: "modelPolicy" if is_none, models: "models" if is_none, name: "name", source: "source", tools: "tools", user_invocable: "userInvocable" });

/// Session event "session.custom_agents_updated". Payload of `session.custom_agents_updated` with loaded custom agents plus non-fatal warnings and fatal errors.
#[derive(Debug, Clone, Default)]
pub struct SessionCustomAgentsUpdatedData {
    /// Array of loaded custom agent metadata
    pub agents: Vec<CustomAgentsUpdatedAgent>,
    /// Fatal errors from agent loading
    pub errors: Vec<String>,
    /// Non-fatal warnings from agent loading
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(SessionCustomAgentsUpdatedData {
    agents: "agents",
    errors: "errors",
    warnings: "warnings"
});

/// Server-advertised metadata learned through modern discovery or legacy initialization.
#[derive(Debug, Clone, Default)]
pub struct McpServerMetadata {
    /// Non-empty natural-language guidance for using the server, or null when the server omitted instructions or advertised an empty string.
    pub instructions: Option<String>,
}
crate::record_serde::wire_record!(McpServerMetadata {
    instructions: "instructions"
});

/// A single MCP server status summary in `session.mcp_servers_loaded`, including name, status, source, transport, and plugin metadata.
#[derive(Debug, Clone, Default)]
pub struct McpServersLoadedServer {
    /// Human-readable display name supplied by a managed server catalog.
    pub display_name: Option<String>,
    /// Error message if the server failed to connect
    pub error: Option<String>,
    /// Server name (config key)
    pub name: String,
    /// Name of the plugin that supplied the effective MCP server config, only when source is plugin
    pub plugin_name: Option<String>,
    /// Version of the plugin that supplied the effective MCP server config, only when source is plugin
    pub plugin_version: Option<String>,
    /// Server-advertised metadata for a connected server. Omitted when no live connection metadata is available, including while pending or when failed, disabled, stopped, or not configured.
    pub server_metadata: Option<McpServerMetadata>,
    /// Configuration source: user, workspace, plugin, builtin, or managed
    pub source: Option<McpServerSource>,
    /// Connection status: connected, failed, needs-auth, pending, disabled, stopped, or not_configured
    pub status: McpServerStatus,
    /// Transport mechanism: stdio, http, sse (deprecated), or memory (in-process MCP server)
    pub transport: Option<McpServerTransport>,
}
crate::record_serde::wire_record!(McpServersLoadedServer { display_name: "displayName" if is_none, error: "error" if is_none, name: "name", plugin_name: "pluginName" if is_none, plugin_version: "pluginVersion" if is_none, server_metadata: "serverMetadata" if is_none, source: "source" if is_none, status: "status", transport: "transport" if is_none });

/// Session event "session.mcp_servers_loaded". Payload of `session.mcp_servers_loaded` listing MCP server status summaries.
#[derive(Debug, Clone, Default)]
pub struct SessionMcpServersLoadedData {
    /// Array of MCP server status summaries
    pub servers: Vec<McpServersLoadedServer>,
}
crate::record_serde::wire_record!(SessionMcpServersLoadedData { servers: "servers" });

/// Session event "session.mcp_server_status_changed". Payload of `session.mcp_server_status_changed` for one MCP server's status and optional failure error.
#[derive(Debug, Clone, Default)]
pub struct SessionMcpServerStatusChangedData {
    /// Runtime configuration provenance for a connected or failed server, or unknown when unavailable. Additional string values may be introduced.
    pub config_source: Option<String>,
    /// Error message if the server entered a failed state
    pub error: Option<String>,
    /// Runtime-produced classification for the final failed connection; unclassified means no classification was supplied. Additional string values may be introduced.
    pub error_classification: Option<String>,
    /// Name of the MCP server whose status changed
    pub server_name: String,
    /// Connection status: connected, failed, needs-auth, pending, disabled, stopped, or not_configured
    pub status: McpServerStatus,
}
crate::record_serde::wire_record!(SessionMcpServerStatusChangedData { config_source: "configSource" if is_none, error: "error" if is_none, error_classification: "errorClassification" if is_none, server_name: "serverName", status: "status" });

/// Session event "session.mcp_server_tools_loaded". Payload of `session.mcp_server_tools_loaded`, emitted once per connection when an MCP server's tools first become available from tools/list or a cached snapshot. A connected server is still loading tools if this event has not fired since the server last left connected; it can precede connected when the tools come from a cached snapshot.
#[derive(Debug, Clone, Default)]
pub struct SessionMcpServerToolsLoadedData {
    /// Name of the MCP server whose tools loaded
    pub server_name: String,
    /// Number of tools the server listed
    pub tool_count: i64,
}
crate::record_serde::wire_record!(SessionMcpServerToolsLoadedData {
    server_name: "serverName",
    tool_count: "toolCount"
});

/// Session event "session.mcp_server_removed". Payload of `session.mcp_server_removed` identifying an MCP server the graph no longer runs.
#[derive(Debug, Clone, Default)]
pub struct SessionMcpServerRemovedData {
    /// Name of the MCP server that was removed from the graph
    pub server_name: String,
}
crate::record_serde::wire_record!(SessionMcpServerRemovedData {
    server_name: "serverName"
});

/// Session event "session.mcp_server_needs_reconnect". Payload of `session.mcp_server_needs_reconnect` identifying an MCP server whose connection must be re-established.
#[derive(Debug, Clone, Default)]
pub struct SessionMcpServerNeedsReconnectData {
    /// Name of the MCP server that needs to reconnect
    pub server_name: String,
}
crate::record_serde::wire_record!(SessionMcpServerNeedsReconnectData {
    server_name: "serverName"
});

/// Session event "mcp.tools.list_changed". Payload identifying the MCP server associated with a list change.
#[derive(Debug, Clone, Default)]
pub struct McpToolsListChangedData {
    /// Name of the MCP server whose list changed
    pub server_name: String,
}
crate::record_serde::wire_record!(McpToolsListChangedData {
    server_name: "serverName"
});

/// Session event "mcp.resources.list_changed". Payload identifying the MCP server associated with a list change.
#[derive(Debug, Clone, Default)]
pub struct McpResourcesListChangedData {
    /// Name of the MCP server whose list changed
    pub server_name: String,
}
crate::record_serde::wire_record!(McpResourcesListChangedData {
    server_name: "serverName"
});

/// Session event "mcp.prompts.list_changed". Payload identifying the MCP server associated with a list change.
#[derive(Debug, Clone, Default)]
pub struct McpPromptsListChangedData {
    /// Name of the MCP server whose list changed
    pub server_name: String,
}
crate::record_serde::wire_record!(McpPromptsListChangedData {
    server_name: "serverName"
});

/// A single extension discovered by `session.extensions_loaded`, including qualified ID, source, and current status.
#[derive(Debug, Clone, Default)]
pub struct ExtensionsLoadedExtension {
    /// Source-qualified extension ID (e.g., 'project:my-ext', 'user:auth-helper', 'plugin:my-plugin:my-ext')
    pub id: String,
    /// Extension name (directory name)
    pub name: String,
    /// Discovery source
    pub source: ExtensionsLoadedExtensionSource,
    /// Current status: running, dormant, disabled, failed, or starting
    pub status: ExtensionsLoadedExtensionStatus,
}
crate::record_serde::wire_record!(ExtensionsLoadedExtension {
    id: "id",
    name: "name",
    source: "source",
    status: "status"
});

/// Session event "session.extensions_loaded". Payload of `session.extensions_loaded` listing discovered extensions and their statuses.
#[derive(Debug, Clone, Default)]
pub struct SessionExtensionsLoadedData {
    /// Array of discovered extensions and their status
    pub extensions: Vec<ExtensionsLoadedExtension>,
}
crate::record_serde::wire_record!(SessionExtensionsLoadedData {
    extensions: "extensions"
});

/// Session event "session.canvas.opened". Payload of `session.canvas.opened` with canvas instance and provider IDs plus optional icon, title, status, URL, and input.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasOpenedData {
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
crate::record_serde::wire_record!(SessionCanvasOpenedData { canvas_id: "canvasId", extension_id: "extensionId", extension_name: "extensionName" if is_none, icon: "icon" if is_none, input: "input" if is_none, instance_id: "instanceId", status: "status" if is_none, title: "title" if is_none, url: "url" if is_none });

/// A single action within a canvas declaration, with its name, optional description, and optional input schema.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasRegistryChangedCanvasAction {
    /// Action description
    pub description: Option<String>,
    /// JSON Schema for action input
    pub input_schema: Option<serde_json::Value>,
    /// Action name
    pub name: String,
}
crate::record_serde::wire_record!(CanvasRegistryChangedCanvasAction { description: "description" if is_none, input_schema: "inputSchema" if is_none, name: "name" });

/// A single canvas declaration in `session.canvas.registry_changed`, including provider IDs, display metadata, input schema, and actions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasRegistryChangedCanvas {
    /// Actions the agent or host may invoke
    pub actions: Option<Vec<CanvasRegistryChangedCanvasAction>>,
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
crate::record_serde::wire_record!(CanvasRegistryChangedCanvas { actions: "actions" if is_none, canvas_id: "canvasId", description: "description", display_name: "displayName", extension_id: "extensionId", extension_name: "extensionName" if is_none, icon: "icon" if is_none, input_schema: "inputSchema" if is_none });

/// Session event "session.canvas.registry_changed". Payload of `session.canvas.registry_changed` listing the canvas declarations currently available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasRegistryChangedData {
    /// Canvas declarations currently available
    pub canvases: Vec<CanvasRegistryChangedCanvas>,
}
crate::record_serde::wire_record!(SessionCanvasRegistryChangedData {
    canvases: "canvases"
});

/// Session event "session.canvas.closed". Payload of `session.canvas.closed` with the closed canvas instance ID, provider ID, and canvas ID.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasClosedData {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Stable caller-supplied identifier of the canvas instance that was closed
    pub instance_id: String,
}
crate::record_serde::wire_record!(SessionCanvasClosedData {
    canvas_id: "canvasId",
    extension_id: "extensionId",
    instance_id: "instanceId"
});

/// Session event "session.canvas.unavailable". Transient signal that an open canvas instance's provider has dropped (for example the extension is reloading mid-session). The host should keep the panel mounted and surface a reconnecting affordance rather than tearing it down; a subsequent `session.canvas.opened` for the same instanceId clears the affordance once the provider reconnects with a fresh url. Ephemeral and never persisted, so it is never replayed on cold resume.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasUnavailableData {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Stable caller-supplied identifier of the canvas instance whose provider became unavailable
    pub instance_id: String,
}
crate::record_serde::wire_record!(SessionCanvasUnavailableData {
    canvas_id: "canvasId",
    extension_id: "extensionId",
    instance_id: "instanceId"
});

/// Session event "session.canvas.recorded". Durable record that a canvas instance is open, used to restore open canvases on cold session resume. Intentionally omits the transient url and availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasRecordedData {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Input supplied when the instance was opened
    pub input: Option<serde_json::Value>,
    /// Stable caller-supplied canvas instance identifier
    pub instance_id: String,
    /// Rendered title
    pub title: Option<String>,
}
crate::record_serde::wire_record!(SessionCanvasRecordedData { canvas_id: "canvasId", extension_id: "extensionId", input: "input" if is_none, instance_id: "instanceId", title: "title" if is_none });

/// Session event "session.canvas.removed". Durable record that a canvas instance was closed, superseding a prior instance_recorded during resume replay.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCanvasRemovedData {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier
    pub extension_id: String,
    /// Stable caller-supplied identifier of the canvas instance that was closed
    pub instance_id: String,
}
crate::record_serde::wire_record!(SessionCanvasRemovedData {
    canvas_id: "canvasId",
    extension_id: "extensionId",
    instance_id: "instanceId"
});

/// Session event "session.extensions.attachments_pushed". Payload of `session.extensions.attachments_pushed` with extension-contributed attachments for the next send.
#[derive(Debug, Clone, Default)]
pub struct SessionExtensionsAttachmentsPushedData {
    /// Attachments contributed by an extension; the host should surface these as composer pills and forward them via the next session.send call.
    pub attachments: Vec<serde_json::Value>,
}
crate::record_serde::wire_record!(SessionExtensionsAttachmentsPushedData {
    attachments: "attachments"
});

/// Set when the underlying tools/call threw an error before returning a CallToolResult
#[derive(Debug, Clone, Default)]
pub struct McpAppToolCallCompleteError {
    /// Human-readable error message
    pub message: String,
}
crate::record_serde::wire_record!(McpAppToolCallCompleteError { message: "message" });

/// MCP App tool `_meta.ui` resource URI and SEP-1865 visibility captured with an `mcp_app.tool_call_complete` result.
#[derive(Debug, Clone, Default)]
pub struct McpAppToolCallCompleteToolMetaUI {
    /// `ui://` URI declared by the tool's `_meta.ui.resourceUri`
    pub resource_uri: Option<String>,
    /// Tool visibility per SEP-1865 (typically a subset of `["model","app"]`)
    pub visibility: Option<Vec<String>>,
}
crate::record_serde::wire_record!(McpAppToolCallCompleteToolMetaUI { resource_uri: "resourceUri" if is_none, visibility: "visibility" if is_none });

/// The tool's `_meta.ui` block at the time of the call, so consumers can decide whether to forward the result to the model without re-listing tools.
#[derive(Debug, Clone, Default)]
pub struct McpAppToolCallCompleteToolMeta {
    /// MCP App tool `_meta.ui` resource URI and SEP-1865 visibility captured with an `mcp_app.tool_call_complete` result.
    pub ui: Option<McpAppToolCallCompleteToolMetaUI>,
}
crate::record_serde::wire_record!(McpAppToolCallCompleteToolMeta { ui: "ui" if is_none });

/// Session event "mcp_app.tool_call_complete". MCP App view called a tool on a connected MCP server (SEP-1865)
#[derive(Debug, Clone, Default)]
pub struct McpAppToolCallCompleteData {
    /// Arguments passed to the tool by the app view, if any
    pub arguments: Option<HashMap<String, serde_json::Value>>,
    /// Wall-clock duration of the underlying tools/call in milliseconds
    pub duration_ms: f64,
    /// Set when the underlying tools/call threw an error before returning a CallToolResult
    pub error: Option<McpAppToolCallCompleteError>,
    /// Standard MCP CallToolResult returned by the server. Present whether or not the call set isError.
    pub result: Option<HashMap<String, serde_json::Value>>,
    /// Name of the MCP server hosting the tool
    pub server_name: String,
    /// True when the call completed without throwing AND the MCP CallToolResult did not set isError
    pub success: bool,
    /// The tool's `_meta.ui` block at the time of the call, so consumers can decide whether to forward the result to the model without re-listing tools.
    pub tool_meta: Option<McpAppToolCallCompleteToolMeta>,
    /// MCP tool name that was invoked
    pub tool_name: String,
}
crate::record_serde::wire_record!(McpAppToolCallCompleteData { arguments: "arguments" if is_none, duration_ms: "durationMs", error: "error" if is_none, result: "result" if is_none, server_name: "serverName", success: "success", tool_meta: "toolMeta" if is_none, tool_name: "toolName" });

/// Session event "session.indexed_search". Transient indexed-search status and diagnostics from the live runtime service. Never persisted or used to infer activation from session history.
pub type SessionIndexedSearchData = IndexedSearchData;

/// Extensible routing preference for the virtual `auto` model. New identifiers must be advertised and enabled by the provider. `fast` is an integrator-only latency preset.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum AutoTier {
    /// Optimize for efficiency.
    Efficiency,
    /// Balance efficiency and intelligence.
    Balance,
    /// Optimize for intelligence.
    Intelligence,
    /// Integrator-only preset that optimizes for latency.
    Fast,
    /// Unknown variant for forward compatibility.
    #[default]
    Unknown,
    /// Provider-advertised identifier, retained verbatim.
    Custom(String),
}
impl AutoTier {
    /// Returns the routing identifier without losing unknown values.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Efficiency => "efficiency",
            Self::Balance => "balance",
            Self::Intelligence => "intelligence",
            Self::Fast => "fast",
            Self::Unknown => "Unknown",
            Self::Custom(value) => value,
        }
    }
}
impl Serialize for AutoTier {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}
impl<'de> Deserialize<'de> for AutoTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        Ok(match value.as_str() {
            "efficiency" => Self::Efficiency,
            "balance" => Self::Balance,
            "intelligence" => Self::Intelligence,
            "fast" => Self::Fast,
            _ => Self::Custom(value),
        })
    }
}

/// Hosting platform type of the repository (github or ado)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkingDirectoryContextHostType {
    /// Repository is hosted on GitHub.
    #[serde(rename = "github")]
    GitHub,
    /// Repository is hosted on Azure DevOps.
    #[serde(rename = "ado")]
    Ado,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkingDirectoryContextHostType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkingDirectoryContextHostType",
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

/// Allowed values for the `ContextTier` enumeration.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ContextTier {
    /// Default context tier with standard context window size.
    #[serde(rename = "default")]
    Default,
    /// Extended context tier with a larger context window.
    #[serde(rename = "long_context")]
    LongContext,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ContextTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ContextTier",
            &["default", "long_context", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Default,
            1 => Self::LongContext,
            _ => Self::Unknown,
        })
    }
}

/// Reasoning summary mode used for model calls, if applicable (e.g. "none", "concise", "detailed")
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ReasoningSummary {
    /// Do not request reasoning summaries from the model.
    #[serde(rename = "none")]
    None,
    /// Request a concise summary of the model's reasoning.
    #[serde(rename = "concise")]
    Concise,
    /// Request a detailed summary of the model's reasoning.
    #[serde(rename = "detailed")]
    Detailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ReasoningSummary {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ReasoningSummary",
            &["none", "concise", "detailed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::Concise,
            2 => Self::Detailed,
            _ => Self::Unknown,
        })
    }
}

/// Output verbosity level used for supported model calls (e.g. "low", "medium", "high")
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum Verbosity {
    /// A terse response was requested.
    #[serde(rename = "low")]
    Low,
    /// A medium amount of response detail was requested.
    #[serde(rename = "medium")]
    Medium,
    /// A more detailed response was requested.
    #[serde(rename = "high")]
    High,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for Verbosity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "Verbosity",
            &["low", "medium", "high", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Low,
            1 => Self::Medium,
            2 => Self::High,
            _ => Self::Unknown,
        })
    }
}

/// Data loss prevention (DLP) block reported by the model provider's exact error code. Resending the same conversation is blocked again; rewind to the user.message whose messageId equals the error's originatingMessageId.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DlpBlockKind {
    /// The provider's DLP policy blocked the prompt (provider code `prompt_dlp_blocked`).
    #[serde(rename = "prompt_dlp_blocked")]
    PromptDlpBlocked,
    /// The provider blocked restricted content included inline in the request (provider code `inline_restricted`).
    #[serde(rename = "inline_restricted")]
    InlineRestricted,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DlpBlockKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DlpBlockKind",
            &["prompt_dlp_blocked", "inline_restricted", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::PromptDlpBlocked,
            1 => Self::InlineRestricted,
            _ => Self::Unknown,
        })
    }
}

/// What the user must do to recover from a failure, named as an action rather than as one client's affordance. The runtime cannot know which affordance a client offers — a slash command, a settings pane, a link — so the accompanying message stays host-agnostic and each client renders its own copy from this value. Absent when the runtime knows of no action the user can take.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum RemediationAction {
    /// Authenticate again with the Copilot backend. The current credential is absent, expired, or rejected.
    #[serde(rename = "sign_in")]
    SignIn,
    /// Authenticate as a different account. The current account exists but lacks access to the requested resource.
    #[serde(rename = "switch_account")]
    SwitchAccount,
    /// Inspect which account is currently authenticated before deciding what to change.
    #[serde(rename = "show_account")]
    ShowAccount,
    /// Review or widen the sandbox policy. The blocked path or host is named by the accompanying message or by the tool result the action arrived with.
    #[serde(rename = "review_sandbox_policy")]
    ReviewSandboxPolicy,
    /// Permit outbound network access in the sandbox policy.
    #[serde(rename = "allow_sandbox_outbound")]
    AllowSandboxOutbound,
    /// Discard conversation history back through the blocked request before continuing.
    #[serde(rename = "rewind_conversation")]
    RewindConversation,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for RemediationAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "RemediationAction",
            &[
                "sign_in",
                "switch_account",
                "show_account",
                "review_sandbox_policy",
                "allow_sandbox_outbound",
                "rewind_conversation",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::SignIn,
            1 => Self::SwitchAccount,
            2 => Self::ShowAccount,
            3 => Self::ReviewSandboxPolicy,
            4 => Self::AllowSandboxOutbound,
            5 => Self::RewindConversation,
            _ => Self::Unknown,
        })
    }
}

/// The session mode the agent is operating in
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SessionMode {
    /// The agent is responding interactively to the user.
    #[serde(rename = "interactive")]
    Interactive,
    /// The agent is preparing a plan before making changes.
    #[serde(rename = "plan")]
    Plan,
    /// The agent is working autonomously toward task completion.
    #[serde(rename = "autopilot")]
    Autopilot,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SessionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SessionMode",
            &["interactive", "plan", "autopilot", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Interactive,
            1 => Self::Plan,
            2 => Self::Autopilot,
            _ => Self::Unknown,
        })
    }
}

/// Who created the schedule: `user` (an explicit user action such as `/every` or `/after`) or `model` (the agent via the `manage_schedule` tool). Gates whether a scheduled skill that opted out of model invocation may fire: only user-created schedules may.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ScheduleOrigin {
    /// The schedule was created by an explicit user action, such as `/every` or `/after`.
    #[serde(rename = "user")]
    User,
    /// The schedule was created by the agent via the `manage_schedule` tool.
    #[serde(rename = "model")]
    Model,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ScheduleOrigin {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ScheduleOrigin",
            &["user", "model", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Model,
            _ => Self::Unknown,
        })
    }
}

/// The type of operation performed on the autopilot objective state file
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutopilotObjectiveChangedOperation {
    /// Autopilot objective state file was created for a new objective.
    #[serde(rename = "create")]
    Create,
    /// Autopilot objective state file was updated for an existing objective.
    #[serde(rename = "update")]
    Update,
    /// Autopilot objective state file was deleted or cleared.
    #[serde(rename = "delete")]
    Delete,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutopilotObjectiveChangedOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutopilotObjectiveChangedOperation",
            &["create", "update", "delete", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Create,
            1 => Self::Update,
            2 => Self::Delete,
            _ => Self::Unknown,
        })
    }
}

/// Current autopilot objective status, if one exists
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutopilotObjectiveChangedStatus {
    /// Objective is active and can drive autopilot continuations.
    #[serde(rename = "active")]
    Active,
    /// Objective is paused and will not drive autopilot continuations.
    #[serde(rename = "paused")]
    Paused,
    /// Legacy objective state indicating the previous continuation cap was reached.
    #[serde(rename = "cap_reached")]
    CapReached,
    /// Objective was completed by the agent.
    #[serde(rename = "completed")]
    Completed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutopilotObjectiveChangedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutopilotObjectiveChangedStatus",
            &["active", "paused", "cap_reached", "completed", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Active,
            1 => Self::Paused,
            2 => Self::CapReached,
            3 => Self::Completed,
            _ => Self::Unknown,
        })
    }
}

/// Indexed-search event variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchDataStatusKind {
    #[serde(rename = "status")]
    #[default]
    Status,
}
impl<'de> Deserialize<'de> for IndexedSearchDataStatusKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchDataStatusKind",
            &["status"],
            None,
        )?;
        Ok(Self::Status)
    }
}

/// Live indexed-search state for this session activation, never inferred from persisted history.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchState {
    /// Indexed search is not active for this session.
    #[serde(rename = "disabled")]
    Disabled,
    /// Indexed-search startup is in progress.
    #[serde(rename = "starting")]
    Starting,
    /// The indexed-search server started successfully; its index may still be warming.
    #[serde(rename = "enabled")]
    Enabled,
    /// The indexed-search server and its index are ready.
    #[serde(rename = "ready")]
    Ready,
    /// Indexed-search startup or the active server failed.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for IndexedSearchState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchState",
            &[
                "disabled", "starting", "enabled", "ready", "failed", "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Disabled,
            1 => Self::Starting,
            2 => Self::Enabled,
            3 => Self::Ready,
            4 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Configuration, policy, or workspace condition that disabled indexed search.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchDisabledReason {
    /// Indexed search was explicitly disabled by the environment.
    #[serde(rename = "use_tgrep_false")]
    UseTgrepFalse,
    /// Search uses the external ripgrep binary instead of bundled search.
    #[serde(rename = "use_builtin_ripgrep_false")]
    UseBuiltinRipgrepFalse,
    /// Organization policy disables indexed search.
    #[serde(rename = "organization")]
    Organization,
    /// Authentication has not resolved organization policy.
    #[serde(rename = "organization_policy_auth_pending")]
    OrganizationPolicyAuthPending,
    /// Organization policy could not be determined.
    #[serde(rename = "organization_policy_unknown")]
    OrganizationPolicyUnknown,
    /// The workspace uses a virtualized or network filesystem.
    #[serde(rename = "virtual_filesystem")]
    VirtualFilesystem,
    /// The workspace is inside a Windows cloud-sync root.
    #[serde(rename = "cloud_sync_root")]
    CloudSyncRoot,
    /// The Windows cloud-sync safety check failed.
    #[serde(rename = "cloud_sync_detection_failed")]
    CloudSyncDetectionFailed,
    /// The workspace is not available on the runtime's local filesystem.
    #[serde(rename = "workspace_not_local")]
    WorkspaceNotLocal,
    /// Another process holds the machine-wide indexed-search server slot.
    #[serde(rename = "server_limit")]
    ServerLimit,
    /// The workspace is a filesystem or network-share root.
    #[serde(rename = "filesystem_root")]
    FilesystemRoot,
    /// The workspace is the user's home directory.
    #[serde(rename = "home_directory")]
    HomeDirectory,
    /// The user's home directory could not be determined.
    #[serde(rename = "home_directory_detection_failed")]
    HomeDirectoryDetectionFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for IndexedSearchDisabledReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchDisabledReason",
            &[
                "use_tgrep_false",
                "use_builtin_ripgrep_false",
                "organization",
                "organization_policy_auth_pending",
                "organization_policy_unknown",
                "virtual_filesystem",
                "cloud_sync_root",
                "cloud_sync_detection_failed",
                "workspace_not_local",
                "server_limit",
                "filesystem_root",
                "home_directory",
                "home_directory_detection_failed",
                "Unknown",
            ],
            Some(13),
        )?;
        Ok(match index {
            0 => Self::UseTgrepFalse,
            1 => Self::UseBuiltinRipgrepFalse,
            2 => Self::Organization,
            3 => Self::OrganizationPolicyAuthPending,
            4 => Self::OrganizationPolicyUnknown,
            5 => Self::VirtualFilesystem,
            6 => Self::CloudSyncRoot,
            7 => Self::CloudSyncDetectionFailed,
            8 => Self::WorkspaceNotLocal,
            9 => Self::ServerLimit,
            10 => Self::FilesystemRoot,
            11 => Self::HomeDirectory,
            12 => Self::HomeDirectoryDetectionFailed,
            _ => Self::Unknown,
        })
    }
}

/// Indexed-search event variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchDataStartupKind {
    #[serde(rename = "startup")]
    #[default]
    Startup,
}
impl<'de> Deserialize<'de> for IndexedSearchDataStartupKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchDataStartupKind",
            &["startup"],
            None,
        )?;
        Ok(Self::Startup)
    }
}

/// Result of an indexed-search startup attempt.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchOutcome {
    /// A new indexed-search server was started.
    #[serde(rename = "started")]
    Started,
    /// The repository has too few files for automatic indexing.
    #[serde(rename = "skipped_below_threshold")]
    SkippedBelowThreshold,
    /// No Git repository was found and indexing was not forced.
    #[serde(rename = "skipped_no_gitroot")]
    SkippedNoGitroot,
    /// Configuration, policy, or workspace safety disabled indexing.
    #[serde(rename = "skipped_disabled")]
    SkippedDisabled,
    /// An existing indexed-search server was reused.
    #[serde(rename = "reused_existing")]
    ReusedExisting,
    /// The startup attempt failed.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for IndexedSearchOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchOutcome",
            &[
                "started",
                "skipped_below_threshold",
                "skipped_no_gitroot",
                "skipped_disabled",
                "reused_existing",
                "failed",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Started,
            1 => Self::SkippedBelowThreshold,
            2 => Self::SkippedNoGitroot,
            3 => Self::SkippedDisabled,
            4 => Self::ReusedExisting,
            5 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Category of an indexed-search server failure.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchErrorType {
    /// The indexed-search server could not be spawned.
    #[serde(rename = "spawn_error")]
    SpawnError,
    /// The indexed-search server exited unexpectedly.
    #[serde(rename = "unexpected_exit")]
    UnexpectedExit,
    /// The indexed-search server was terminated by a signal.
    #[serde(rename = "killed_by_signal")]
    KilledBySignal,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for IndexedSearchErrorType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchErrorType",
            &[
                "spawn_error",
                "unexpected_exit",
                "killed_by_signal",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::SpawnError,
            1 => Self::UnexpectedExit,
            2 => Self::KilledBySignal,
            _ => Self::Unknown,
        })
    }
}

/// Indexed-search event variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchDataServerErrorKind {
    #[serde(rename = "server_error")]
    #[default]
    ServerError,
}
impl<'de> Deserialize<'de> for IndexedSearchDataServerErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchDataServerErrorKind",
            &["server_error"],
            None,
        )?;
        Ok(Self::ServerError)
    }
}

/// Indexed-search event variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchDataIncrementalKind {
    #[serde(rename = "incremental")]
    #[default]
    Incremental,
}
impl<'de> Deserialize<'de> for IndexedSearchDataIncrementalKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchDataIncrementalKind",
            &["incremental"],
            None,
        )?;
        Ok(Self::Incremental)
    }
}

/// Phase of an incremental indexed-search update.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum IndexedSearchIncrementalPhase {
    /// A workspace scan found changes to index.
    #[serde(rename = "changes_detected")]
    ChangesDetected,
    /// The incremental index update completed.
    #[serde(rename = "updated")]
    Updated,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for IndexedSearchIncrementalPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "IndexedSearchIncrementalPhase",
            &["changes_detected", "updated", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::ChangesDetected,
            1 => Self::Updated,
            _ => Self::Unknown,
        })
    }
}

/// Transient indexed-search status and diagnostics from the live runtime service. Never persisted or used to infer activation from session history.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IndexedSearchData {
    Status(IndexedSearchDataStatus),
    Startup(IndexedSearchDataStartup),
    ServerError(IndexedSearchDataServerError),
    Incremental(IndexedSearchDataIncremental),
}

/// Origin of an effective session model change.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelChangeSource {
    /// The user selected a model directly with `/model <id>`.
    #[serde(rename = "model_command")]
    ModelCommand,
    /// The user selected the model with `/settings`.
    #[serde(rename = "settings_command")]
    SettingsCommand,
    /// The user selected the model with the `/config` alias.
    #[serde(rename = "config_command")]
    ConfigCommand,
    /// The user selected the model in the model picker, including the picker opened by bare `/model`.
    #[serde(rename = "model_picker")]
    ModelPicker,
    /// Organization-managed settings selected the model.
    #[serde(rename = "managed_settings")]
    ManagedSettings,
    /// Repository settings selected the model.
    #[serde(rename = "repo_settings")]
    RepoSettings,
    /// Startup model resolution selected the model.
    #[serde(rename = "startup")]
    Startup,
    /// Selecting an agent selected its configured model.
    #[serde(rename = "agent")]
    Agent,
    /// Entering, leaving, or reconfiguring plan mode selected the model.
    #[serde(rename = "plan_mode")]
    PlanMode,
    /// The runtime selected the model automatically, such as rate-limit recovery or refusal fallback.
    #[serde(rename = "automatic")]
    Automatic,
    /// The user selected the promoted model from the changeboarding card or its keyboard shortcut.
    #[serde(rename = "changeboarding_shortcut")]
    ChangeboardingShortcut,
    /// An SDK or RPC caller selected the model.
    #[serde(rename = "sdk")]
    Sdk,
    /// The user accepted a CAPI-issued Auto tier recommendation.
    #[serde(rename = "auto_tier_recommendation")]
    AutoTierRecommendation,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelChangeSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelChangeSource",
            &[
                "model_command",
                "settings_command",
                "config_command",
                "model_picker",
                "managed_settings",
                "repo_settings",
                "startup",
                "agent",
                "plan_mode",
                "automatic",
                "changeboarding_shortcut",
                "sdk",
                "auto_tier_recommendation",
                "Unknown",
            ],
            Some(13),
        )?;
        Ok(match index {
            0 => Self::ModelCommand,
            1 => Self::SettingsCommand,
            2 => Self::ConfigCommand,
            3 => Self::ModelPicker,
            4 => Self::ManagedSettings,
            5 => Self::RepoSettings,
            6 => Self::Startup,
            7 => Self::Agent,
            8 => Self::PlanMode,
            9 => Self::Automatic,
            10 => Self::ChangeboardingShortcut,
            11 => Self::Sdk,
            12 => Self::AutoTierRecommendation,
            _ => Self::Unknown,
        })
    }
}

/// Why the session no longer has an explicitly selected model.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelDeselectedReason {
    /// A host-managed provider snapshot no longer publishes the selected model.
    #[serde(rename = "provider_withdrawn")]
    ProviderWithdrawn,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelDeselectedReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelDeselectedReason",
            &["provider_withdrawn", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::ProviderWithdrawn,
            _ => Self::Unknown,
        })
    }
}

/// Enabled Auto preferences that Copilot API can recommend.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum RecommendedAutoTier {
    /// Optimize for efficiency.
    Efficiency,
    /// Balance efficiency and intelligence.
    Balance,
    /// Optimize for intelligence.
    Intelligence,
    /// Unknown variant for forward compatibility.
    #[default]
    Unknown,
    /// Provider-advertised identifier, retained verbatim.
    Custom(String),
}
impl RecommendedAutoTier {
    /// Returns the routing identifier without losing unknown values.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Efficiency => "efficiency",
            Self::Balance => "balance",
            Self::Intelligence => "intelligence",
            Self::Unknown => "Unknown",
            Self::Custom(value) => value,
        }
    }
}
impl Serialize for RecommendedAutoTier {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}
impl<'de> Deserialize<'de> for RecommendedAutoTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        Ok(match value.as_str() {
            "efficiency" => Self::Efficiency,
            "balance" => Self::Balance,
            "intelligence" => Self::Intelligence,
            _ => Self::Custom(value),
        })
    }
}

/// Terminal reason an Auto preference activation failed.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutoTierSwitchFailureReason {
    /// The candidate model was rejected by model policy.
    #[serde(rename = "policy_rejected")]
    PolicyRejected,
    /// The Auto routing request failed or returned an unusable response.
    #[serde(rename = "request_failed")]
    RequestFailed,
    /// The runtime could not prepare the Auto routing request.
    #[serde(rename = "setup_failed")]
    SetupFailed,
    /// The provider does not support Auto routing.
    #[serde(rename = "unsupported")]
    Unsupported,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutoTierSwitchFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutoTierSwitchFailureReason",
            &[
                "policy_rejected",
                "request_failed",
                "setup_failed",
                "unsupported",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::PolicyRejected,
            1 => Self::RequestFailed,
            2 => Self::SetupFailed,
            3 => Self::Unsupported,
            _ => Self::Unknown,
        })
    }
}

/// Permission mode for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionMode {
    /// Permission requests follow the normal approval flow.
    #[serde(rename = "manual")]
    Manual,
    /// Permission requests include an LLM safety recommendation; clients may automatically approve requests judged acceptable.
    #[serde(rename = "assisted")]
    Assisted,
    /// Tool, path, and URL permission requests are automatically approved.
    #[serde(rename = "allow-all")]
    AllowAll,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionMode",
            &["manual", "assisted", "allow-all", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Manual,
            1 => Self::Assisted,
            2 => Self::AllowAll,
            _ => Self::Unknown,
        })
    }
}

/// The type of operation performed on the plan file
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PlanChangedOperation {
    /// The plan file was created.
    #[serde(rename = "create")]
    Create,
    /// The plan file was updated.
    #[serde(rename = "update")]
    Update,
    /// The plan file was deleted.
    #[serde(rename = "delete")]
    Delete,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PlanChangedOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PlanChangedOperation",
            &["create", "update", "delete", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Create,
            1 => Self::Update,
            2 => Self::Delete,
            _ => Self::Unknown,
        })
    }
}

/// Whether the file was newly created or updated
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkspaceFileChangedOperation {
    /// The workspace file was created.
    #[serde(rename = "create")]
    Create,
    /// The workspace file was updated.
    #[serde(rename = "update")]
    Update,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkspaceFileChangedOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkspaceFileChangedOperation",
            &["create", "update", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Create,
            1 => Self::Update,
            _ => Self::Unknown,
        })
    }
}

/// Origin type of the session being handed off
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HandoffSourceType {
    /// The handoff originated from a remote session.
    #[serde(rename = "remote")]
    Remote,
    /// The handoff originated from a local session.
    #[serde(rename = "local")]
    Local,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HandoffSourceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HandoffSourceType",
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

/// Whether the accumulated numeric AI-credit subtotal covers the observed calls.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AiCreditsStatus {
    /// Every observed call supplied an AI-credit amount, including an explicit zero.
    #[serde(rename = "complete")]
    Complete,
    /// Some calls supplied amounts and some did not; the numeric value is only a reported subtotal.
    #[serde(rename = "partial")]
    Partial,
    /// No AI-credit amount was reported for the observed calls. Numeric zero is not a zero-cost claim.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AiCreditsStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AiCreditsStatus",
            &["complete", "partial", "unavailable", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Complete,
            1 => Self::Partial,
            2 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// The neutral kind of a model provider — the model analog of `AccountKind`. A model provider is the live, entitled source a model came from; central code never branches on this beyond a single dispatch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelProviderKind {
    /// GitHub Copilot / CAPI models, spawned by a github-resolving account that holds a Copilot seat.
    #[serde(rename = "copilot")]
    Copilot,
    /// Microsoft 365 Copilot (Loki) inference models, spawned by a resolvable Entra-derived Loki account.
    #[serde(rename = "loki")]
    Loki,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelProviderKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelProviderKind",
            &["copilot", "loki", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Copilot,
            1 => Self::Loki,
            _ => Self::Unknown,
        })
    }
}

/// Whether the session ended normally ("routine") or due to a crash/fatal error ("error")
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ShutdownType {
    /// The session ended normally.
    #[serde(rename = "routine")]
    Routine,
    /// The session ended because of a crash or fatal error.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ShutdownType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ShutdownType",
            &["routine", "error", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Routine,
            1 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Access policy reported by the quota service.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaAccessState {
    /// Access is allowed.
    #[serde(rename = "allowed")]
    Allowed,
    /// Access is denied.
    #[serde(rename = "denied")]
    Denied,
    /// Quota is not required for this access.
    #[serde(rename = "not_required")]
    NotRequired,
    /// The service did not supply a recognized access state.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Client-only compatibility placeholder when acquisitionStatus is unavailable or failed; not an observed service verdict.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaAccessState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaAccessState",
            &[
                "allowed",
                "denied",
                "not_required",
                "unknown",
                "unavailable",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Allowed,
            1 => Self::Denied,
            2 => Self::NotRequired,
            3 => Self::UnknownValue,
            4 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Acquisition state, independent of the service's business access/capacity verdict.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaAcquisitionStatus {
    /// A service quota observation was received.
    #[serde(rename = "succeeded")]
    Succeeded,
    /// No service snapshot is available for this caller or surface.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Acquisition failed; inspect httpStatus/acquisitionError without inferring a business verdict.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaAcquisitionStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaAcquisitionStatus",
            &["succeeded", "unavailable", "failed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Succeeded,
            1 => Self::Unavailable,
            2 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Capacity is independent of whether a numeric balance was supplied.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaCapacityState {
    /// Capacity is available.
    #[serde(rename = "available")]
    Available,
    /// The service explicitly reports exhaustion.
    #[serde(rename = "exhausted")]
    Exhausted,
    /// The reported entitlement is unlimited.
    #[serde(rename = "unlimited")]
    Unlimited,
    /// No billable quota is required.
    #[serde(rename = "not_required")]
    NotRequired,
    /// The service reports that capacity is not applicable.
    #[serde(rename = "not_applicable")]
    NotApplicable,
    /// No recognized capacity state was supplied.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Client-only compatibility placeholder when acquisitionStatus is unavailable or failed; not an observed service verdict.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaCapacityState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaCapacityState",
            &[
                "available",
                "exhausted",
                "unlimited",
                "not_required",
                "not_applicable",
                "unknown",
                "unavailable",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Available,
            1 => Self::Exhausted,
            2 => Self::Unlimited,
            3 => Self::NotRequired,
            4 => Self::NotApplicable,
            5 => Self::UnknownValue,
            6 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Identity scope of a monthly usage reading.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderMonthlyUsageScope {
    /// Consumption of the authenticated user within the parent observation's service and returned cycle.
    #[serde(rename = "user")]
    User,
    /// The service did not establish a recognized usage scope.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderMonthlyUsageScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderMonthlyUsageScope",
            &["user", "unknown", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Availability of a service's monthly consumption reading.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderMonthlyUsageState {
    /// The service reported a usable consumption reading, including an explicit zero.
    #[serde(rename = "available")]
    Available,
    /// The service found no usage policy; no consumption amount is known.
    #[serde(rename = "no_policy")]
    NoPolicy,
    /// The read failed or its data could not be used; consumption is unknown.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// The service reported an unrecognized usage state.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderMonthlyUsageState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderMonthlyUsageState",
            &[
                "available",
                "no_policy",
                "unavailable",
                "unknown",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Available,
            1 => Self::NoPolicy,
            2 => Self::Unavailable,
            3 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Units explicitly reported by a quota provider.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaUnit {
    /// AI credits, not tokens or currency.
    #[serde(rename = "ai_credits")]
    AiCredits,
    /// Requests charged against an entitlement.
    #[serde(rename = "requests")]
    Requests,
    /// Model tokens.
    #[serde(rename = "tokens")]
    Tokens,
    /// The observation does not establish a quantity unit.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaUnit {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaUnit",
            &["ai_credits", "requests", "tokens", "unknown", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::AiCredits,
            1 => Self::Requests,
            2 => Self::Tokens,
            3 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Timing and purpose of a provider quota observation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaObservationKind {
    /// An explicit account read; consumption may still be subject to service aggregation delay.
    #[serde(rename = "account_snapshot")]
    AccountSnapshot,
    /// A verdict attached before an inference response. May be cached; never a post-charge balance.
    #[serde(rename = "admission_state")]
    AdmissionState,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaObservationKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaObservationKind",
            &["account_snapshot", "admission_state", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AccountSnapshot,
            1 => Self::AdmissionState,
            _ => Self::Unknown,
        })
    }
}

/// Interpretation permitted for independently reported quantities.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderQuotaQuantityKind {
    /// Quantities form an authoritative available-of-entitled budget suitable for percentage presentation.
    #[serde(rename = "authoritative_budget")]
    AuthoritativeBudget,
    /// Raw advisory quantities; do not infer permission, usage percentage, reset cadence, or post-charge balance.
    #[serde(rename = "advisory_balance")]
    AdvisoryBalance,
    /// No quantity measurement, as with admission-only response headers.
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderQuotaQuantityKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderQuotaQuantityKind",
            &[
                "authoritative_budget",
                "advisory_balance",
                "none",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::AuthoritativeBudget,
            1 => Self::AdvisoryBalance,
            2 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// How the provider resolved the sensitivity label for one model request
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SensitivityLabelOutcome {
    /// The request inherited the sensitivity label carried in `label`.
    #[serde(rename = "inherited")]
    Inherited,
    /// The request's sources carried no sensitivity labels.
    #[serde(rename = "no_source_labels")]
    NoSourceLabels,
    /// The request's sources were labeled, but no label applies to the request.
    #[serde(rename = "no_applicable_label")]
    NoApplicableLabel,
    /// The provider could not determine the label; `failure` may describe why.
    #[serde(rename = "indeterminate")]
    Indeterminate,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SensitivityLabelOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SensitivityLabelOutcome",
            &[
                "inherited",
                "no_source_labels",
                "no_applicable_label",
                "indeterminate",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Inherited,
            1 => Self::NoSourceLabels,
            2 => Self::NoApplicableLabel,
            3 => Self::Indeterminate,
            _ => Self::Unknown,
        })
    }
}

/// What initiated a conversation compaction
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CompactionTrigger {
    /// Background compaction started automatically because context utilization crossed the background threshold.
    #[serde(rename = "threshold")]
    Threshold,
    /// Compaction forced by a context-limit model response (e.g. HTTP 413) before retrying the request.
    #[serde(rename = "context_limit_retry")]
    ContextLimitRetry,
    /// User-requested compaction, e.g. the /compact command or the history.compact API.
    #[serde(rename = "manual")]
    Manual,
    /// Emergency compaction triggered by high process memory usage.
    #[serde(rename = "memory_pressure")]
    MemoryPressure,
    /// Compaction requested while switching to a model with a smaller context window, including a HydraFusion turn's routed root model.
    #[serde(rename = "model_switch")]
    ModelSwitch,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CompactionTrigger {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CompactionTrigger",
            &[
                "threshold",
                "context_limit_retry",
                "manual",
                "memory_pressure",
                "model_switch",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Threshold,
            1 => Self::ContextLimitRetry,
            2 => Self::Manual,
            3 => Self::MemoryPressure,
            4 => Self::ModelSwitch,
            _ => Self::Unknown,
        })
    }
}

/// Controlled reason for a permission-recovery episode transition
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryReason {
    /// An action required permission that Assisted Permissions could not grant.
    #[serde(rename = "permission_required")]
    PermissionRequired,
    /// The agent repeated an equivalent permission request instead of making progress.
    #[serde(rename = "repeated_attempt")]
    RepeatedAttempt,
    /// The bounded number of distinct permission attempts was exhausted.
    #[serde(rename = "attempts_exhausted")]
    AttemptsExhausted,
    /// A responder approved the escalated permission request.
    #[serde(rename = "permission_approved")]
    PermissionApproved,
    /// A responder denied the escalated permission request.
    #[serde(rename = "permission_denied")]
    PermissionDenied,
    /// The response-capable client became unavailable while escalation was pending.
    #[serde(rename = "responder_unavailable")]
    ResponderUnavailable,
    /// A later tool call succeeded without requiring the blocked permission.
    #[serde(rename = "equivalent_alternative_succeeded")]
    EquivalentAlternativeSucceeded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryReason",
            &[
                "permission_required",
                "repeated_attempt",
                "attempts_exhausted",
                "permission_approved",
                "permission_denied",
                "responder_unavailable",
                "equivalent_alternative_succeeded",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::PermissionRequired,
            1 => Self::RepeatedAttempt,
            2 => Self::AttemptsExhausted,
            3 => Self::PermissionApproved,
            4 => Self::PermissionDenied,
            5 => Self::ResponderUnavailable,
            6 => Self::EquivalentAlternativeSucceeded,
            _ => Self::Unknown,
        })
    }
}

/// Category of structured task blocker
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TaskBlockerKind {
    /// Autopilot permission recovery requires intervention or has no safe autonomous path.
    #[serde(rename = "permission_recovery")]
    PermissionRecovery,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for TaskBlockerKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "TaskBlockerKind",
            &["permission_recovery", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::PermissionRecovery,
            _ => Self::Unknown,
        })
    }
}

/// Runtime handling applied to a recovery attempt
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryAttemptDisposition {
    /// The request was denied without prompting so the agent could try an alternative.
    #[serde(rename = "deferred")]
    Deferred,
    /// The request was surfaced to an interactive responder.
    #[serde(rename = "prompted")]
    Prompted,
    /// The interactive responder approved the request.
    #[serde(rename = "approved")]
    Approved,
    /// The interactive responder denied the request or became unavailable.
    #[serde(rename = "denied")]
    Denied,
    /// The request exhausted unattended recovery and produced a blocked outcome.
    #[serde(rename = "blocked")]
    Blocked,
    /// A tool call succeeded as an equivalent alternative.
    #[serde(rename = "succeeded")]
    Succeeded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryAttemptDisposition {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryAttemptDisposition",
            &[
                "deferred",
                "prompted",
                "approved",
                "denied",
                "blocked",
                "succeeded",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Deferred,
            1 => Self::Prompted,
            2 => Self::Approved,
            3 => Self::Denied,
            4 => Self::Blocked,
            5 => Self::Succeeded,
            _ => Self::Unknown,
        })
    }
}

/// Controlled reason for an individual attempt disposition
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryAttemptReason {
    /// The attempt required permission that Assisted Permissions could not grant.
    #[serde(rename = "permission_required")]
    PermissionRequired,
    /// The request repeated an earlier attempt.
    #[serde(rename = "repeated_attempt")]
    RepeatedAttempt,
    /// The request exceeded the bounded number of distinct attempts.
    #[serde(rename = "attempts_exhausted")]
    AttemptsExhausted,
    /// The interactive responder approved the request.
    #[serde(rename = "permission_approved")]
    PermissionApproved,
    /// The interactive responder denied the request.
    #[serde(rename = "permission_denied")]
    PermissionDenied,
    /// The interactive responder became unavailable.
    #[serde(rename = "responder_unavailable")]
    ResponderUnavailable,
    /// The tool call succeeded without the blocked permission.
    #[serde(rename = "equivalent_alternative_succeeded")]
    EquivalentAlternativeSucceeded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryAttemptReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryAttemptReason",
            &[
                "permission_required",
                "repeated_attempt",
                "attempts_exhausted",
                "permission_approved",
                "permission_denied",
                "responder_unavailable",
                "equivalent_alternative_succeeded",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::PermissionRequired,
            1 => Self::RepeatedAttempt,
            2 => Self::AttemptsExhausted,
            3 => Self::PermissionApproved,
            4 => Self::PermissionDenied,
            5 => Self::ResponderUnavailable,
            6 => Self::EquivalentAlternativeSucceeded,
            _ => Self::Unknown,
        })
    }
}

/// Relationship of an attempt to earlier permission requests
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryAttemptRelation {
    /// The first denied permission request in the episode.
    #[serde(rename = "initial")]
    Initial,
    /// A request equivalent to an earlier attempt.
    #[serde(rename = "retry")]
    Retry,
    /// A distinct request or a successful alternative tool call.
    #[serde(rename = "alternative")]
    Alternative,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryAttemptRelation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryAttemptRelation",
            &["initial", "retry", "alternative", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Initial,
            1 => Self::Retry,
            2 => Self::Alternative,
            _ => Self::Unknown,
        })
    }
}

/// Action selected when autonomous recovery cannot continue
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryOnBlocked {
    /// Surface the existing permission prompt to a response-capable client.
    #[serde(rename = "ask")]
    Ask,
    /// Return a structured unsuccessful blocked outcome because no responder is available.
    #[serde(rename = "fail")]
    Fail,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryOnBlocked {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryOnBlocked",
            &["ask", "fail", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Ask,
            1 => Self::Fail,
            _ => Self::Unknown,
        })
    }
}

/// Lifecycle state of a permission-recovery episode
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecoveryStatus {
    /// Autopilot may try a bounded equivalent alternative.
    #[serde(rename = "recovering")]
    Recovering,
    /// An interactive permission response is required.
    #[serde(rename = "awaiting_approval")]
    AwaitingApproval,
    /// The episode ended through approval or a successful equivalent alternative.
    #[serde(rename = "resolved")]
    Resolved,
    /// No autonomous path remains and the task requires intervention.
    #[serde(rename = "blocked")]
    Blocked,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecoveryStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecoveryStatus",
            &[
                "recovering",
                "awaiting_approval",
                "resolved",
                "blocked",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Recovering,
            1 => Self::AwaitingApproval,
            2 => Self::Resolved,
            3 => Self::Blocked,
            _ => Self::Unknown,
        })
    }
}

/// Controlled reason that a task is blocked by permission recovery
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TaskBlockerReason {
    /// A required action is waiting on or could not obtain permission.
    #[serde(rename = "permission_required")]
    PermissionRequired,
    /// The agent repeated an equivalent denied permission request.
    #[serde(rename = "repeated_attempt")]
    RepeatedAttempt,
    /// The bounded autonomous attempts for one permission-recovery episode were exhausted.
    #[serde(rename = "attempts_exhausted")]
    AttemptsExhausted,
    /// Historical compatibility value for a recovery resolved by permission approval; new task blockers do not emit it.
    #[serde(rename = "permission_approved")]
    PermissionApproved,
    /// A responder denied the escalated permission request.
    #[serde(rename = "permission_denied")]
    PermissionDenied,
    /// No response-capable client remained available for the permission request.
    #[serde(rename = "responder_unavailable")]
    ResponderUnavailable,
    /// Historical compatibility value for a recovery resolved by an equivalent permitted alternative; new task blockers do not emit it.
    #[serde(rename = "equivalent_alternative_succeeded")]
    EquivalentAlternativeSucceeded,
    /// The epoch-wide consecutive-block or total-denial limit was reached.
    #[serde(rename = "permission_recovery_run_limit")]
    PermissionRecoveryRunLimit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for TaskBlockerReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "TaskBlockerReason",
            &[
                "permission_required",
                "repeated_attempt",
                "attempts_exhausted",
                "permission_approved",
                "permission_denied",
                "responder_unavailable",
                "equivalent_alternative_succeeded",
                "permission_recovery_run_limit",
                "Unknown",
            ],
            Some(8),
        )?;
        Ok(match index {
            0 => Self::PermissionRequired,
            1 => Self::RepeatedAttempt,
            2 => Self::AttemptsExhausted,
            3 => Self::PermissionApproved,
            4 => Self::PermissionDenied,
            5 => Self::ResponderUnavailable,
            6 => Self::EquivalentAlternativeSucceeded,
            7 => Self::PermissionRecoveryRunLimit,
            _ => Self::Unknown,
        })
    }
}

/// Semantic result of evaluating a task completion request
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TaskCompletionOutcome {
    /// The completion request was accepted and the objective is complete.
    #[serde(rename = "completed")]
    Completed,
    /// The completion request was rejected because more work or validation remains.
    #[serde(rename = "continue")]
    Continue,
    /// Completion cannot proceed without intervention; the active objective is paused when one is identified.
    #[serde(rename = "blocked")]
    Blocked,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for TaskCompletionOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "TaskCompletionOutcome",
            &["completed", "continue", "blocked", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Completed,
            1 => Self::Continue,
            2 => Self::Blocked,
            _ => Self::Unknown,
        })
    }
}

/// Structured terminal status from a tool completion event.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CompletionReceiptToolStatus {
    /// The tool completed successfully.
    #[serde(rename = "success")]
    Success,
    /// The tool failed without a more specific structured status.
    #[serde(rename = "failure")]
    Failure,
    /// The tool exceeded its time budget.
    #[serde(rename = "timeout")]
    Timeout,
    /// The user rejected the tool call.
    #[serde(rename = "rejected")]
    Rejected,
    /// The permissions service denied the tool call.
    #[serde(rename = "denied")]
    Denied,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CompletionReceiptToolStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CompletionReceiptToolStatus",
            &[
                "success", "failure", "timeout", "rejected", "denied", "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Failure,
            2 => Self::Timeout,
            3 => Self::Rejected,
            4 => Self::Denied,
            _ => Self::Unknown,
        })
    }
}

/// Runtime reason the completion decision was accepted.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CompletionReceiptStopReason {
    /// The model reached a natural terminal response.
    #[serde(rename = "natural")]
    Natural,
    /// A terminal tool ended the interaction.
    #[serde(rename = "terminal_tool")]
    TerminalTool,
    /// The configured agentStop continuation limit was reached.
    #[serde(rename = "agent_stop_block_limit")]
    AgentStopBlockLimit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CompletionReceiptStopReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CompletionReceiptStopReason",
            &[
                "natural",
                "terminal_tool",
                "agent_stop_block_limit",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Natural,
            1 => Self::TerminalTool,
            2 => Self::AgentStopBlockLimit,
            _ => Self::Unknown,
        })
    }
}

/// Kind of turn for which HydraFusion routing is running.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionTurnKind {
    /// A user-message turn.
    #[serde(rename = "user")]
    User,
    /// A conversation-compaction turn.
    #[serde(rename = "compaction")]
    Compaction,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionTurnKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionTurnKind",
            &["user", "compaction", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Compaction,
            _ => Self::Unknown,
        })
    }
}

/// Server-recommended routing behavior for a later HydraFusion turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionFollowUpAction {
    /// Reuse the durable primary model without routing.
    #[serde(rename = "reuse_primary")]
    ReusePrimary,
    /// Request a new routing decision.
    #[serde(rename = "reroute")]
    Reroute,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionFollowUpAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionFollowUpAction",
            &["reuse_primary", "reroute", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::ReusePrimary,
            1 => Self::Reroute,
            _ => Self::Unknown,
        })
    }
}

/// Validated HydraFusion execution pattern.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionPattern {
    /// Run one primary solver phase.
    #[serde(rename = "single")]
    Single,
    /// Run a primary phase, a judge, and an optional repair.
    #[serde(rename = "cascade")]
    Cascade,
    /// Run a primary draft, a read-only critique, and a revision.
    #[serde(rename = "critique")]
    Critique,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionPattern {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionPattern",
            &["single", "cascade", "critique", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Single,
            1 => Self::Cascade,
            2 => Self::Critique,
            _ => Self::Unknown,
        })
    }
}

/// HydraFusion phase kind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionPhaseKind {
    /// Primary solver phase.
    #[serde(rename = "primary")]
    Primary,
    /// Read-only cascade judge phase.
    #[serde(rename = "judge")]
    Judge,
    /// Cascade repair phase.
    #[serde(rename = "repair")]
    Repair,
    /// Initial critique-pattern draft phase.
    #[serde(rename = "draft")]
    Draft,
    /// Read-only critique phase.
    #[serde(rename = "critic")]
    Critic,
    /// Critique-pattern revision phase.
    #[serde(rename = "revision")]
    Revision,
    /// Follow-up phase continuing from the resolved model.
    #[serde(rename = "follow_up")]
    FollowUp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionPhaseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionPhaseKind",
            &[
                "primary",
                "judge",
                "repair",
                "draft",
                "critic",
                "revision",
                "follow_up",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Primary,
            1 => Self::Judge,
            2 => Self::Repair,
            3 => Self::Draft,
            4 => Self::Critic,
            5 => Self::Revision,
            6 => Self::FollowUp,
            _ => Self::Unknown,
        })
    }
}

/// Conversation scope in which a HydraFusion phase executes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionConversationScope {
    /// Canonical root conversation history.
    #[serde(rename = "root")]
    Root,
    /// Isolated read-only review history that does not enter the root conversation.
    #[serde(rename = "review")]
    Review,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionConversationScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionConversationScope",
            &["root", "review", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Root,
            1 => Self::Review,
            _ => Self::Unknown,
        })
    }
}

/// The agent mode that was active when this message was sent
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserMessageAgentMode {
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
impl<'de> Deserialize<'de> for UserMessageAgentMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UserMessageAgentMode",
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

/// How this user message was delivered to the agentic loop, relative to whether the loop was already running. This is the timing axis only; the message's origin (human vs. system/command/schedule/skill/etc.) is carried separately by `source`. A system-injected message has a delivery too — e.g. a background-task notification waking an idle agent is `idle`, the same mechanism as a human starting a fresh turn.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserMessageDelivery {
    /// Delivered while the loop was idle; starts its own run immediately (a human's fresh turn, or a system notification waking an idle agent).
    #[serde(rename = "idle")]
    Idle,
    /// Injected into the current in-flight run while the agent was busy (immediate mode).
    #[serde(rename = "steering")]
    Steering,
    /// Enqueued while the agent was busy; processed as its own run afterward.
    #[serde(rename = "queued")]
    Queued,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UserMessageDelivery {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UserMessageDelivery",
            &["idle", "steering", "queued", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Idle,
            1 => Self::Steering,
            2 => Self::Queued,
            _ => Self::Unknown,
        })
    }
}

/// What the agent was doing when the user interrupted it.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentInterruptedActivity {
    /// A request to the model was open.
    #[serde(rename = "model_call")]
    ModelCall,
    /// The turn was sleeping between retry attempts.
    #[serde(rename = "retry_backoff")]
    RetryBackoff,
    /// One or more tools were executing.
    #[serde(rename = "tool_call")]
    ToolCall,
    /// Background sub-agents were running while the main loop was idle.
    #[serde(rename = "background_agent")]
    BackgroundAgent,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentInterruptedActivity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentInterruptedActivity",
            &[
                "model_call",
                "retry_backoff",
                "tool_call",
                "background_agent",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::ModelCall,
            1 => Self::RetryBackoff,
            2 => Self::ToolCall,
            3 => Self::BackgroundAgent,
            _ => Self::Unknown,
        })
    }
}

/// Where the interruption landed relative to the first streamed token.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentInterruptedCancelPhase {
    /// No output had been produced when the request was cancelled.
    #[serde(rename = "pre_first_token")]
    PreFirstToken,
    /// The response was already streaming when the request was cancelled.
    #[serde(rename = "mid_stream")]
    MidStream,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentInterruptedCancelPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentInterruptedCancelPhase",
            &["pre_first_token", "mid_stream", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::PreFirstToken,
            1 => Self::MidStream,
            _ => Self::Unknown,
        })
    }
}

/// Transport used for a failed model call
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFailureTransport {
    /// HTTP transport, including SSE streams.
    #[serde(rename = "http")]
    Http,
    /// WebSocket transport.
    #[serde(rename = "websocket")]
    Websocket,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFailureTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFailureTransport",
            &["http", "websocket", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websocket,
            _ => Self::Unknown,
        })
    }
}

/// Content-safe activity observed while a HydraFusion phase is running.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionPhaseActivityKind {
    /// The provider produced additional private output bytes.
    #[serde(rename = "model_output")]
    ModelOutput,
    /// A tool began executing inside the phase.
    #[serde(rename = "tool_started")]
    ToolStarted,
    /// A tool finished executing inside the phase.
    #[serde(rename = "tool_completed")]
    ToolCompleted,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionPhaseActivityKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionPhaseActivityKind",
            &["model_output", "tool_started", "tool_completed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::ModelOutput,
            1 => Self::ToolStarted,
            2 => Self::ToolCompleted,
            _ => Self::Unknown,
        })
    }
}

/// How a durable phase checkpoint contributes its exact message to canonical root history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum FusionProjectionMode {
    /// Append the exact root message immediately.
    #[serde(rename = "append")]
    Append,
    /// Hold a terminal message outside canonical history until the final commit selects it.
    #[serde(rename = "staged")]
    Staged,
    /// Do not project the checkpoint into root history.
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionProjectionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionProjectionMode",
            &["append", "staged", "none", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Append,
            1 => Self::Staged,
            2 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// Durable outcome status of a HydraFusion phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FusionPhaseStatus {
    /// The phase completed successfully.
    #[serde(rename = "succeeded")]
    Succeeded,
    /// The phase failed.
    #[serde(rename = "failed")]
    Failed,
    /// The phase was cancelled.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FusionPhaseStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FusionPhaseStatus",
            &["succeeded", "failed", "cancelled", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Succeeded,
            1 => Self::Failed,
            2 => Self::Cancelled,
            _ => Self::Unknown,
        })
    }
}

/// Tool call type: "function" for standard tool calls, "custom" for grammar-based tool calls. Defaults to "function" when absent.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistantMessageToolRequestType {
    /// Standard function-style tool call.
    #[serde(rename = "function")]
    Function,
    /// Custom grammar-based tool call.
    #[serde(rename = "custom")]
    Custom,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistantMessageToolRequestType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistantMessageToolRequestType",
            &["function", "custom", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Function,
            1 => Self::Custom,
            _ => Self::Unknown,
        })
    }
}

/// The system that produced a citation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CitationProvider {
    /// Citation produced by an Anthropic (Claude) model response.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Citation produced by an OpenAI model response.
    #[serde(rename = "openai")]
    Openai,
    /// Citation synthesized client-side by the runtime from tool output.
    #[serde(rename = "client")]
    Client,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CitationProvider {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CitationProvider",
            &["anthropic", "openai", "client", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Anthropic,
            1 => Self::Openai,
            2 => Self::Client,
            _ => Self::Unknown,
        })
    }
}

/// Hosted program caller type
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistantMessageToolRequestCallerType {
    #[serde(rename = "program")]
    Program,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistantMessageToolRequestCallerType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistantMessageToolRequestCallerType",
            &["program", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Program,
            _ => Self::Unknown,
        })
    }
}

/// API endpoint used for this model call, matching CAPI supported_endpoints vocabulary
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistantUsageApiEndpoint {
    /// Chat Completions API endpoint.
    #[serde(rename = "/chat/completions")]
    ChatCompletions,
    /// Anthropic Messages API endpoint.
    #[serde(rename = "/v1/messages")]
    V1Messages,
    /// Responses API endpoint.
    #[serde(rename = "/responses")]
    Responses,
    /// WebSocket Responses API endpoint.
    #[serde(rename = "ws:/responses")]
    WsResponses,
    /// Gemini Interactions API endpoint.
    #[serde(rename = "/interactions")]
    Interactions,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistantUsageApiEndpoint {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistantUsageApiEndpoint",
            &[
                "/chat/completions",
                "/v1/messages",
                "/responses",
                "ws:/responses",
                "/interactions",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::ChatCompletions,
            1 => Self::V1Messages,
            2 => Self::Responses,
            3 => Self::WsResponses,
            4 => Self::Interactions,
            _ => Self::Unknown,
        })
    }
}

/// How the runtime fell back to an uncompressed request body after a compressed body was rejected
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallRequestBodyCompressionFallback {
    /// A compressed body was rejected with HTTP 400 or 415 and this outcome comes from the single uncompressed resend.
    #[serde(rename = "retried")]
    Retried,
    /// Request body compression was already turned off for the session by an earlier rejection, so the body was sent uncompressed.
    #[serde(rename = "disabled")]
    Disabled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallRequestBodyCompressionFallback {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallRequestBodyCompressionFallback",
            &["retried", "disabled", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Retried,
            1 => Self::Disabled,
            _ => Self::Unknown,
        })
    }
}

/// Content-Encoding applied to the request body sent on the wire
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallRequestBodyEncoding {
    /// The request body was sent uncompressed (including every WebSocket request).
    #[serde(rename = "identity")]
    Identity,
    /// The request body was sent gzip-compressed.
    #[serde(rename = "gzip")]
    Gzip,
    /// The request body was sent zstd-compressed.
    #[serde(rename = "zstd")]
    Zstd,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallRequestBodyEncoding {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallRequestBodyEncoding",
            &["identity", "gzip", "zstd", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Identity,
            1 => Self::Gzip,
            2 => Self::Zstd,
            _ => Self::Unknown,
        })
    }
}

/// Transport used for a successful model call
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistantUsageTransport {
    /// HTTP transport, including SSE streams.
    #[serde(rename = "http")]
    Http,
    /// WebSocket transport.
    #[serde(rename = "websocket")]
    Websocket,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistantUsageTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistantUsageTransport",
            &["http", "websocket", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websocket,
            _ => Self::Unknown,
        })
    }
}

/// Normalized cause of the WebSocket failure that triggered the HTTP fallback
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallWebSocketFallbackErrorKind {
    /// DNS resolution of the WebSocket host failed.
    #[serde(rename = "dns")]
    Dns,
    /// The TLS handshake or a TLS record failed.
    #[serde(rename = "tls")]
    Tls,
    /// A WebSocket connect or read timed out.
    #[serde(rename = "timeout")]
    Timeout,
    /// The peer refused the TCP connection.
    #[serde(rename = "connection_refused")]
    ConnectionRefused,
    /// The connection was reset, aborted, or ended mid-stream without a close.
    #[serde(rename = "connection_reset")]
    ConnectionReset,
    /// The peer closed the WebSocket.
    #[serde(rename = "closed_by_peer")]
    ClosedByPeer,
    /// The runtime closed the WebSocket itself.
    #[serde(rename = "closed_locally")]
    ClosedLocally,
    /// The socket was already closed before it was used.
    #[serde(rename = "not_connected")]
    NotConnected,
    /// The upgrade or request was rejected with an HTTP status.
    #[serde(rename = "http_status")]
    HttpStatus,
    /// A WebSocket protocol violation or an undecodable frame.
    #[serde(rename = "protocol")]
    Protocol,
    /// The WebSocket request could not be built.
    #[serde(rename = "configuration")]
    Configuration,
    /// A failure that maps onto none of the other kinds.
    #[serde(rename = "other")]
    Other,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallWebSocketFallbackErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallWebSocketFallbackErrorKind",
            &[
                "dns",
                "tls",
                "timeout",
                "connection_refused",
                "connection_reset",
                "closed_by_peer",
                "closed_locally",
                "not_connected",
                "http_status",
                "protocol",
                "configuration",
                "other",
                "Unknown",
            ],
            Some(12),
        )?;
        Ok(match index {
            0 => Self::Dns,
            1 => Self::Tls,
            2 => Self::Timeout,
            3 => Self::ConnectionRefused,
            4 => Self::ConnectionReset,
            5 => Self::ClosedByPeer,
            6 => Self::ClosedLocally,
            7 => Self::NotConnected,
            8 => Self::HttpStatus,
            9 => Self::Protocol,
            10 => Self::Configuration,
            11 => Self::Other,
            _ => Self::Unknown,
        })
    }
}

/// Why a WebSocket-capable model call was carried by the HTTP fallback
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallWebSocketFallbackReason {
    /// The WebSocket connection could not be established.
    #[serde(rename = "connect_failed")]
    ConnectFailed,
    /// No usable WebSocket connection was available for the request.
    #[serde(rename = "connection_unavailable")]
    ConnectionUnavailable,
    /// Sending the request over the WebSocket failed.
    #[serde(rename = "send_failed")]
    SendFailed,
    /// The WebSocket returned a retryable API error.
    #[serde(rename = "api_error")]
    ApiError,
    /// The WebSocket transport failed before any output reached the consumer.
    #[serde(rename = "transport_failed")]
    TransportFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallWebSocketFallbackReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallWebSocketFallbackReason",
            &[
                "connect_failed",
                "connection_unavailable",
                "send_failed",
                "api_error",
                "transport_failed",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::ConnectFailed,
            1 => Self::ConnectionUnavailable,
            2 => Self::SendFailed,
            3 => Self::ApiError,
            4 => Self::TransportFailed,
            _ => Self::Unknown,
        })
    }
}

/// For HTTP 400 failures only: whether the response carried a structured CAPI error envelope (structured_error, a deterministic validation failure) or no error body (bodyless, the transient gateway/proxy signature). Absent for non-400 failures.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFailureBadRequestKind {
    /// The 400 response carried no error body (transient gateway/proxy signature).
    #[serde(rename = "bodyless")]
    Bodyless,
    /// The 400 response carried a structured CAPI error envelope (deterministic validation failure).
    #[serde(rename = "structured_error")]
    StructuredError,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFailureBadRequestKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFailureBadRequestKind",
            &["bodyless", "structured_error", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Bodyless,
            1 => Self::StructuredError,
            _ => Self::Unknown,
        })
    }
}

/// Boundary that produced a model call failure
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFailureKind {
    /// The provider returned an API error response.
    #[serde(rename = "api")]
    Api,
    /// The request transport failed before a usable API response completed.
    #[serde(rename = "transport")]
    Transport,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFailureKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFailureKind",
            &["api", "transport", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Api,
            1 => Self::Transport,
            _ => Self::Unknown,
        })
    }
}

/// Bounded product-state failure classified before telemetry projection.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum ModelCallProductStateError {
    /// A Responses input item belongs to a different transport connection.
    #[serde(rename = "input_item_connection_mismatch")]
    InputItemConnectionMismatch,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallProductStateError {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallProductStateError",
            &["input_item_connection_mismatch", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::InputItemConnectionMismatch,
            _ => Self::Unknown,
        })
    }
}

/// Where the failed model call originated
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFailureSource {
    /// Model call from the top-level agent.
    #[serde(rename = "top_level")]
    TopLevel,
    /// Model call from a sub-agent.
    #[serde(rename = "subagent")]
    Subagent,
    /// Model call from MCP sampling.
    #[serde(rename = "mcp_sampling")]
    McpSampling,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFailureSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFailureSource",
            &["top_level", "subagent", "mcp_sampling", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::TopLevel,
            1 => Self::Subagent,
            2 => Self::McpSampling,
            _ => Self::Unknown,
        })
    }
}

/// Final bounded result of one logical model operation after its internal retry loop settles
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFinalResult {
    /// The final attempt succeeded.
    #[serde(rename = "success")]
    Success,
    /// The final attempt failed with HTTP 400.
    #[serde(rename = "http_400")]
    Http400,
    /// The final attempt failed with HTTP 413.
    #[serde(rename = "http_413")]
    Http413,
    /// The final attempt failed with HTTP 429.
    #[serde(rename = "http_429")]
    Http429,
    /// The final attempt failed with another HTTP 4xx status.
    #[serde(rename = "http_4xx")]
    Http4xx,
    /// The final attempt failed with an HTTP 5xx status.
    #[serde(rename = "http_5xx")]
    Http5xx,
    /// The final attempt failed in the request transport.
    #[serde(rename = "transport_error")]
    TransportError,
    /// The final attempt failed without another bounded classification.
    #[serde(rename = "other_error")]
    OtherError,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFinalResult {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFinalResult",
            &[
                "success",
                "http_400",
                "http_413",
                "http_429",
                "http_4xx",
                "http_5xx",
                "transport_error",
                "other_error",
                "Unknown",
            ],
            Some(8),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Http400,
            2 => Self::Http413,
            3 => Self::Http429,
            4 => Self::Http4xx,
            5 => Self::Http5xx,
            6 => Self::TransportError,
            7 => Self::OtherError,
            _ => Self::Unknown,
        })
    }
}

/// Final outcome of one logical model dispatch after response acceptance processing
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ModelCallFinishedOutcome {
    /// The provider response was accepted for continued agent processing.
    #[serde(rename = "success")]
    Success,
    /// The dispatch ended with a provider or transport error.
    #[serde(rename = "error")]
    Error,
    /// The dispatch was cancelled before an accepted response was produced.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// The provider response was rejected during post-response acceptance processing.
    #[serde(rename = "rejected")]
    Rejected,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ModelCallFinishedOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ModelCallFinishedOutcome",
            &["success", "error", "cancelled", "rejected", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Error,
            2 => Self::Cancelled,
            3 => Self::Rejected,
            _ => Self::Unknown,
        })
    }
}

/// Finite reason code describing why the current turn was aborted
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AbortReason {
    /// The local user requested the abort, for example by pressing Ctrl+C in the CLI.
    #[serde(rename = "user_initiated")]
    UserInitiated,
    /// A remote command requested the abort.
    #[serde(rename = "remote_command")]
    RemoteCommand,
    /// An MCP server delivered a user.abort notification.
    #[serde(rename = "user_abort")]
    UserAbort,
    /// Autopilot stopped the run because the active objective reached its user-set --max-ai-credits limit.
    #[serde(rename = "autopilot_credit_limit")]
    AutopilotCreditLimit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AbortReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AbortReason",
            &[
                "user_initiated",
                "remote_command",
                "user_abort",
                "autopilot_credit_limit",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::UserInitiated,
            1 => Self::RemoteCommand,
            2 => Self::UserAbort,
            3 => Self::AutopilotCreditLimit,
            _ => Self::Unknown,
        })
    }
}

/// Configuration source: user, workspace, plugin, builtin, managed, or account
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerSource {
    /// Server configured in the user's global MCP configuration.
    #[serde(rename = "user")]
    User,
    /// Server configured by the current workspace.
    #[serde(rename = "workspace")]
    Workspace,
    /// Server contributed by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Server bundled with the runtime.
    #[serde(rename = "builtin")]
    Builtin,
    /// Server supplied by a trusted host-managed catalog.
    #[serde(rename = "managed")]
    Managed,
    /// Server contributed by a signed-in account; enablement and organization policy still apply.
    #[serde(rename = "account")]
    Account,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerSource",
            &[
                "user",
                "workspace",
                "plugin",
                "builtin",
                "managed",
                "account",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Workspace,
            2 => Self::Plugin,
            3 => Self::Builtin,
            4 => Self::Managed,
            5 => Self::Account,
            _ => Self::Unknown,
        })
    }
}

/// Transport mechanism: stdio, http, sse (deprecated), or memory (in-process MCP server)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerTransport {
    /// Server communicates over stdio with a local child process.
    #[serde(rename = "stdio")]
    Stdio,
    /// Server communicates over streamable HTTP.
    #[serde(rename = "http")]
    Http,
    /// Server communicates over Server-Sent Events (deprecated).
    #[serde(rename = "sse")]
    Sse,
    /// Server is backed by an in-memory runtime implementation.
    #[serde(rename = "memory")]
    Memory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerTransport",
            &["stdio", "http", "sse", "memory", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Stdio,
            1 => Self::Http,
            2 => Self::Sse,
            3 => Self::Memory,
            _ => Self::Unknown,
        })
    }
}

/// Allowed values for the `ToolExecutionStartToolDescriptionMetaUIVisibility` enumeration.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionStartToolDescriptionMetaUIVisibility {
    /// Tool is callable by the model (LLM tool surface)
    #[serde(rename = "model")]
    Model,
    /// Tool is callable by the MCP App view (iframe) via session.mcp.apps.callTool
    #[serde(rename = "app")]
    App,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ToolExecutionStartToolDescriptionMetaUIVisibility {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionStartToolDescriptionMetaUIVisibility",
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

/// Shell output source. Terminal output has no separate stdout/stderr attribution.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolShellOutputStream {
    /// Output from the shell command's standard output stream. This is the default when stream is omitted.
    #[serde(rename = "stdout")]
    Stdout,
    /// Output from the shell command's standard error stream.
    #[serde(rename = "stderr")]
    Stderr,
    /// Inherently merged output that cannot be attributed separately to stdout or stderr.
    #[serde(rename = "terminal")]
    Terminal,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ToolShellOutputStream {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolShellOutputStream",
            &["stdout", "stderr", "terminal", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Stdout,
            1 => Self::Stderr,
            2 => Self::Terminal,
            _ => Self::Unknown,
        })
    }
}

/// Kind of file mutation committed by a built-in editing tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(from = "String", into = "String")]
pub enum ToolExecutionCompleteFileEditKind {
    /// A file was created.
    Create,
    /// A file was written by an edit operation.
    Edit,
    /// A file was deleted.
    Delete,
    /// An unrecognized operation kind, retaining its wire value.
    Unknown(String),
}
impl Default for ToolExecutionCompleteFileEditKind {
    fn default() -> Self {
        Self::Unknown("unknown".to_owned())
    }
}
impl From<String> for ToolExecutionCompleteFileEditKind {
    fn from(value: String) -> Self {
        match value.as_str() {
            "create" => Self::Create,
            "edit" => Self::Edit,
            "delete" => Self::Delete,
            _ => Self::Unknown(value),
        }
    }
}
impl From<ToolExecutionCompleteFileEditKind> for String {
    fn from(value: ToolExecutionCompleteFileEditKind) -> Self {
        match value {
            ToolExecutionCompleteFileEditKind::Create => "create".to_owned(),
            ToolExecutionCompleteFileEditKind::Edit => "edit".to_owned(),
            ToolExecutionCompleteFileEditKind::Delete => "delete".to_owned(),
            ToolExecutionCompleteFileEditKind::Unknown(value) => value,
        }
    }
}

/// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PersistedBinaryImageType {
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
impl<'de> Deserialize<'de> for PersistedBinaryImageType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PersistedBinaryImageType",
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

/// Why the binary data is absent: it exceeded the inline size limit, its asset was unavailable, or its image format was unsupported
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum OmittedBinaryOmittedReason {
    /// Bytes exceeded the session's inline size limit.
    #[serde(rename = "too_large")]
    TooLarge,
    /// The referenced binary asset could not be found (e.g. a truncated log).
    #[serde(rename = "asset_unavailable")]
    AssetUnavailable,
    /// The image format or MIME label is not accepted by the model.
    #[serde(rename = "unsupported_format")]
    UnsupportedFormat,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for OmittedBinaryOmittedReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "OmittedBinaryOmittedReason",
            &[
                "too_large",
                "asset_unavailable",
                "unsupported_format",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::TooLarge,
            1 => Self::AssetUnavailable,
            2 => Self::UnsupportedFormat,
            _ => Self::Unknown,
        })
    }
}

/// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum OmittedBinaryType {
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
impl<'de> Deserialize<'de> for OmittedBinaryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "OmittedBinaryType",
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

/// Binary result type discriminator. Use "image" for images and "resource" for other binary data.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum BinaryAssetReferenceType {
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
impl<'de> Deserialize<'de> for BinaryAssetReferenceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "BinaryAssetReferenceType",
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

/// A model-facing binary result as persisted: full inline data, a size-omitted marker, or a deduplicated asset reference
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PersistedBinaryResult {
    PersistedBinaryImage(PersistedBinaryImage),
    OmittedBinaryResult(OmittedBinaryResult),
    BinaryAssetReference(BinaryAssetReference),
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentTextType {
    #[serde(rename = "text")]
    #[default]
    Text,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentTextType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentTextType",
            &["text"],
            None,
        )?;
        Ok(Self::Text)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentShellExitType {
    #[serde(rename = "shell_exit")]
    #[default]
    ShellExit,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentShellExitType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentShellExitType",
            &["shell_exit"],
            None,
        )?;
        Ok(Self::ShellExit)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentImageType {
    #[serde(rename = "image")]
    #[default]
    Image,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentImageType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentImageType",
            &["image"],
            None,
        )?;
        Ok(Self::Image)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentAudioType {
    #[serde(rename = "audio")]
    #[default]
    Audio,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentAudioType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentAudioType",
            &["audio"],
            None,
        )?;
        Ok(Self::Audio)
    }
}

/// Theme variant this icon is intended for
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentResourceLinkIconTheme {
    /// Icon intended for light themes.
    #[serde(rename = "light")]
    Light,
    /// Icon intended for dark themes.
    #[serde(rename = "dark")]
    Dark,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentResourceLinkIconTheme {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentResourceLinkIconTheme",
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

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentResourceLinkType {
    #[serde(rename = "resource_link")]
    #[default]
    ResourceLink,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentResourceLinkType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentResourceLinkType",
            &["resource_link"],
            None,
        )?;
        Ok(Self::ResourceLink)
    }
}

/// The embedded resource contents, either text or base64-encoded binary
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ToolExecutionCompleteContentResourceDetails {
    EmbeddedTextResourceContents(EmbeddedTextResourceContents),
    EmbeddedBlobResourceContents(EmbeddedBlobResourceContents),
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteContentResourceType {
    #[serde(rename = "resource")]
    #[default]
    Resource,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteContentResourceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteContentResourceType",
            &["resource"],
            None,
        )?;
        Ok(Self::Resource)
    }
}

/// A content block within a tool result, which may be text, terminal output, image, audio, or a resource
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ToolExecutionCompleteContent {
    Text(ToolExecutionCompleteContentText),
    ShellExit(ToolExecutionCompleteContentShellExit),
    Image(ToolExecutionCompleteContentImage),
    Audio(ToolExecutionCompleteContentAudio),
    ResourceLink(ToolExecutionCompleteContentResourceLink),
    Resource(ToolExecutionCompleteContentResource),
}

/// Allowed values for the `ToolExecutionCompleteToolDescriptionMetaUIVisibility` enumeration.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ToolExecutionCompleteToolDescriptionMetaUIVisibility {
    /// Tool is callable by the model (LLM tool surface)
    #[serde(rename = "model")]
    Model,
    /// Tool is callable by the MCP App view (iframe) via session.mcp.apps.callTool
    #[serde(rename = "app")]
    App,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ToolExecutionCompleteToolDescriptionMetaUIVisibility {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ToolExecutionCompleteToolDescriptionMetaUIVisibility",
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

/// What triggered the skill invocation: `user-invoked` (explicit user action, such as via a slash command or UI affordance), `agent-invoked` (agent requested the skill), or `context-load` (loaded as part of another context, such as preloading skills configured on a custom agent or subagent)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInvokedTrigger {
    /// Skill invocation requested explicitly by the user, such as via a slash command or UI affordance.
    #[serde(rename = "user-invoked")]
    UserInvoked,
    /// Skill invocation requested by the agent.
    #[serde(rename = "agent-invoked")]
    AgentInvoked,
    /// Skill content loaded as part of another context, such as a configured custom agent or subagent.
    #[serde(rename = "context-load")]
    ContextLoad,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInvokedTrigger {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInvokedTrigger",
            &["user-invoked", "agent-invoked", "context-load", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::UserInvoked,
            1 => Self::AgentInvoked,
            2 => Self::ContextLoad,
            _ => Self::Unknown,
        })
    }
}

/// Process-containment backend selected for the host platform
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxBackend {
    /// Apple Seatbelt process sandbox.
    #[serde(rename = "seatbelt")]
    Seatbelt,
    /// Linux Bubblewrap process sandbox.
    #[serde(rename = "bubblewrap")]
    Bubblewrap,
    /// Windows ProcessContainer sandbox.
    #[serde(rename = "process_container")]
    ProcessContainer,
    /// No supported process-containment backend is available.
    #[serde(rename = "unsupported")]
    Unsupported,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxBackend {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxBackend",
            &[
                "seatbelt",
                "bubblewrap",
                "process_container",
                "unsupported",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Seatbelt,
            1 => Self::Bubblewrap,
            2 => Self::ProcessContainer,
            3 => Self::Unsupported,
            _ => Self::Unknown,
        })
    }
}

/// Customer-controllable sandbox governance area
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxControl {
    /// Process containment and sandbox spawn behavior.
    #[serde(rename = "process")]
    Process,
    /// Filesystem read, write, and deny policy.
    #[serde(rename = "filesystem")]
    Filesystem,
    /// Outbound and local-network access policy.
    #[serde(rename = "network")]
    Network,
    /// Selection of the sandbox or built-in enforcement route.
    #[serde(rename = "routing")]
    Routing,
    /// Decisions to run outside the process sandbox, whether requested by the model or resolved by a person.
    #[serde(rename = "bypass")]
    Bypass,
    /// Credential and keychain capability injection.
    #[serde(rename = "credentials")]
    Credentials,
    /// Host-platform and backend support behavior.
    #[serde(rename = "platform")]
    Platform,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxControl {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxControl",
            &[
                "process",
                "filesystem",
                "network",
                "routing",
                "bypass",
                "credentials",
                "platform",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Process,
            1 => Self::Filesystem,
            2 => Self::Network,
            3 => Self::Routing,
            4 => Self::Bypass,
            5 => Self::Credentials,
            6 => Self::Platform,
            _ => Self::Unknown,
        })
    }
}

/// Finite reason why sandbox enforcement is weaker than configured
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDegradationReason {
    /// The selected backend cannot enforce per-path deny rules.
    #[serde(rename = "denied_paths_unsupported")]
    DeniedPathsUnsupported,
    /// The host platform has no supported process-containment backend.
    #[serde(rename = "unsupported_platform")]
    UnsupportedPlatform,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxDegradationReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDegradationReason",
            &[
                "denied_paths_unsupported",
                "unsupported_platform",
                "Unknown",
            ],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::DeniedPathsUnsupported,
            1 => Self::UnsupportedPlatform,
            _ => Self::Unknown,
        })
    }
}

/// Runtime boundary that enforced or routed a sandbox decision
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxEnforcementPoint {
    /// Shell command process containment.
    #[serde(rename = "shell")]
    Shell,
    /// Built-in filesystem policy enforcement.
    #[serde(rename = "builtin_filesystem")]
    BuiltinFilesystem,
    /// Search-tool sandbox or policy enforcement.
    #[serde(rename = "search")]
    Search,
    /// Web-fetch network policy enforcement.
    #[serde(rename = "web_fetch")]
    WebFetch,
    /// Model Context Protocol server routing.
    #[serde(rename = "mcp")]
    Mcp,
    /// Language server process routing.
    #[serde(rename = "lsp")]
    Lsp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxEnforcementPoint {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxEnforcementPoint",
            &[
                "shell",
                "builtin_filesystem",
                "search",
                "web_fetch",
                "mcp",
                "lsp",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Shell,
            1 => Self::BuiltinFilesystem,
            2 => Self::Search,
            3 => Self::WebFetch,
            4 => Self::Mcp,
            5 => Self::Lsp,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataPolicyResolvedKind {
    #[serde(rename = "policy_resolved")]
    #[default]
    PolicyResolved,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataPolicyResolvedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataPolicyResolvedKind",
            &["policy_resolved"],
            None,
        )?;
        Ok(Self::PolicyResolved)
    }
}

/// Sandbox control whose value the enterprise-managed policy decided
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxManagedControl {
    /// The managed policy forced the sandbox on.
    #[serde(rename = "enabled")]
    Enabled,
    /// The managed policy forbade per-command sandbox bypass.
    #[serde(rename = "allow_bypass")]
    AllowBypass,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxManagedControl {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxManagedControl",
            &["enabled", "allow_bypass", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Enabled,
            1 => Self::AllowBypass,
            _ => Self::Unknown,
        })
    }
}

/// Finite result of a sandbox decision. Each `SandboxDecisionData` variant uses a disjoint subset: `policy_resolved` is `resolved | degraded`, `spawn_completed` and `permissive_retry_completed` are `succeeded | failed`, `enforcement_state` is `engaged | inactive | failed`, `access_denied` is `denied`, `access_recorded` is `allowed`, and escalation decisions are `approved | declined`.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxOutcome {
    /// The sandbox policy resolved successfully. Describes configuration only and makes no claim that a backend engaged.
    #[serde(rename = "resolved")]
    Resolved,
    /// No sandbox governed the workload.
    #[serde(rename = "inactive")]
    Inactive,
    /// A runtime-owned containment backend accepted the workload. Evidence of engagement, not of verified containment for the workload's lifetime.
    #[serde(rename = "engaged")]
    Engaged,
    /// The sandbox operation completed successfully.
    #[serde(rename = "succeeded")]
    Succeeded,
    /// The sandbox operation failed.
    #[serde(rename = "failed")]
    Failed,
    /// The sandbox is active with one or more controls weakened by platform limitations or an explicitly selected relaxed mode.
    #[serde(rename = "degraded")]
    Degraded,
    /// An enforcement check refused the requested access.
    #[serde(rename = "denied")]
    Denied,
    /// A request to run outside the process sandbox was granted.
    #[serde(rename = "approved")]
    Approved,
    /// A request to run outside the process sandbox was not granted.
    #[serde(rename = "declined")]
    Declined,
    /// Permissive learning mode recorded an access the enforced policy would have refused, and allowed it.
    #[serde(rename = "allowed")]
    Allowed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxOutcome",
            &[
                "resolved",
                "inactive",
                "engaged",
                "succeeded",
                "failed",
                "degraded",
                "denied",
                "approved",
                "declined",
                "allowed",
                "Unknown",
            ],
            Some(10),
        )?;
        Ok(match index {
            0 => Self::Resolved,
            1 => Self::Inactive,
            2 => Self::Engaged,
            3 => Self::Succeeded,
            4 => Self::Failed,
            5 => Self::Degraded,
            6 => Self::Denied,
            7 => Self::Approved,
            8 => Self::Declined,
            9 => Self::Allowed,
            _ => Self::Unknown,
        })
    }
}

/// Host operating-system family used for sandbox enforcement
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxPlatform {
    /// Apple macOS host.
    #[serde(rename = "macos")]
    Macos,
    /// Linux host.
    #[serde(rename = "linux")]
    Linux,
    /// Microsoft Windows host.
    #[serde(rename = "windows")]
    Windows,
    /// Host platform outside the explicitly supported families.
    #[serde(rename = "other")]
    Other,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxPlatform {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxPlatform",
            &["macos", "linux", "windows", "other", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Macos,
            1 => Self::Linux,
            2 => Self::Windows,
            3 => Self::Other,
            _ => Self::Unknown,
        })
    }
}

/// Origin of the effective sandbox policy
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxPolicySource {
    /// Runtime default sandbox policy.
    #[serde(rename = "default_policy")]
    DefaultPolicy,
    /// User-configured sandbox policy merged with runtime-required grants.
    #[serde(rename = "user_policy")]
    UserPolicy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxPolicySource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxPolicySource",
            &["default_policy", "user_policy", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::DefaultPolicy,
            1 => Self::UserPolicy,
            _ => Self::Unknown,
        })
    }
}

/// Bounded classification of effective sandbox proxy routing
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxProxyMode {
    /// No sandbox proxy is configured.
    #[serde(rename = "none")]
    None,
    /// Traffic routes through a loopback proxy.
    #[serde(rename = "loopback")]
    Loopback,
    /// Traffic routes through a non-loopback proxy endpoint.
    #[serde(rename = "external")]
    External,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxProxyMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxProxyMode",
            &["none", "loopback", "external", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::Loopback,
            2 => Self::External,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataSpawnCompletedKind {
    #[serde(rename = "spawn_completed")]
    #[default]
    SpawnCompleted,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataSpawnCompletedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataSpawnCompletedKind",
            &["spawn_completed"],
            None,
        )?;
        Ok(Self::SpawnCompleted)
    }
}

/// Runtime observation backing an enforcement-state or denial claim. Absent on `enforcement_state` when no observation backs the state.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxAttestation {
    /// A containment backend accepted and applied the spawn request.
    #[serde(rename = "spawn_succeeded")]
    SpawnSucceeded,
    /// The spawn was refused for lack of a usable containment backend.
    #[serde(rename = "unsupported")]
    Unsupported,
    /// A runtime-owned policy check ran and returned a verdict. Attests the check, not that the caller honoured it.
    #[serde(rename = "builtin_policy_checked")]
    BuiltinPolicyChecked,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxAttestation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxAttestation",
            &[
                "spawn_succeeded",
                "unsupported",
                "builtin_policy_checked",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::SpawnSucceeded,
            1 => Self::Unsupported,
            2 => Self::BuiltinPolicyChecked,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataEnforcementStateKind {
    #[serde(rename = "enforcement_state")]
    #[default]
    EnforcementState,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataEnforcementStateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataEnforcementStateKind",
            &["enforcement_state"],
            None,
        )?;
        Ok(Self::EnforcementState)
    }
}

/// How strong the evidence behind a denial is. A denial the sandbox itself recorded is a fact; one inferred from a command's output text is a judgement, and an analysis that cannot tell them apart will treat a false positive as enforcement.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDenialConfidence {
    /// The sandbox's own denial capture recorded the refused access. The strongest evidence available: the kernel observed it, not the runtime.
    #[serde(rename = "captured")]
    Captured,
    /// The command named a path that the effective policy independently denies. No capture confirmed it, but the policy did.
    #[serde(rename = "policy_corroborated")]
    PolicyCorroborated,
    /// The failure carried a fingerprint the sandbox itself emits, so the sandbox is known to have refused something even though the resource was not confirmed.
    #[serde(rename = "sandbox_reported")]
    SandboxReported,
    /// Classified from the command's own output text alone. The weakest evidence: a command that merely prints sandbox-like wording reaches this level.
    #[serde(rename = "output_classified")]
    OutputClassified,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxDenialConfidence {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDenialConfidence",
            &[
                "captured",
                "policy_corroborated",
                "sandbox_reported",
                "output_classified",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Captured,
            1 => Self::PolicyCorroborated,
            2 => Self::SandboxReported,
            3 => Self::OutputClassified,
            _ => Self::Unknown,
        })
    }
}

/// Bounded class of access an enforcement check refused. Raw resources, commands, and process names accompany it only when content capture is enabled; diagnostic text and matched rules are never exported.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDenialClass {
    /// A read was refused because the effective policy does not grant it.
    #[serde(rename = "filesystem_read")]
    FilesystemRead,
    /// A write was refused. Distinct from `filesystem_read` because a read-only grant denies writes to a path it otherwise permits.
    #[serde(rename = "filesystem_write")]
    FilesystemWrite,
    /// A process could not start because the sandbox refused a required process-scoped resource. Currently emitted for the Windows MSYS `BaseNamedObjects` fork failure confirmed by learning-mode capture.
    #[serde(rename = "process_startup")]
    ProcessStartup,
    /// Windows registry access was refused and correlated with capture evidence.
    #[serde(rename = "registry_access")]
    RegistryAccess,
    /// Windows ALPC or RPC access was refused and correlated with capture evidence.
    #[serde(rename = "ipc_access")]
    IpcAccess,
    /// Access to another Windows process was refused and correlated with capture evidence.
    #[serde(rename = "process_access")]
    ProcessAccess,
    /// Windows job-object access was refused and correlated with capture evidence.
    #[serde(rename = "job_access")]
    JobAccess,
    /// Windows UI-handle access was refused and correlated with capture evidence.
    #[serde(rename = "ui_access")]
    UiAccess,
    /// Windows service-control-manager access was refused and correlated with capture evidence.
    #[serde(rename = "service_access")]
    ServiceAccess,
    /// An outbound connection was refused by `network.allowOutbound`.
    #[serde(rename = "network_outbound")]
    NetworkOutbound,
    /// A connection to a local or loopback destination was refused by `network.allowLocalNetwork`.
    #[serde(rename = "network_local")]
    NetworkLocal,
    /// A destination was refused by the sandbox host allow/deny rules.
    #[serde(rename = "network_host")]
    NetworkHost,
    /// The sandbox's denial capture recorded a refusal its record does not attribute to a more specific control: an unclassified resource (registry, COM, section object) or an AppContainer capability with no network meaning. Deliberately generic — the capture proves the denial happened, and naming a narrower class than the record supports would be a guess.
    #[serde(rename = "other_access")]
    OtherAccess,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxDenialClass {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDenialClass",
            &[
                "filesystem_read",
                "filesystem_write",
                "process_startup",
                "registry_access",
                "ipc_access",
                "process_access",
                "job_access",
                "ui_access",
                "service_access",
                "network_outbound",
                "network_local",
                "network_host",
                "other_access",
                "Unknown",
            ],
            Some(13),
        )?;
        Ok(match index {
            0 => Self::FilesystemRead,
            1 => Self::FilesystemWrite,
            2 => Self::ProcessStartup,
            3 => Self::RegistryAccess,
            4 => Self::IpcAccess,
            5 => Self::ProcessAccess,
            6 => Self::JobAccess,
            7 => Self::UiAccess,
            8 => Self::ServiceAccess,
            9 => Self::NetworkOutbound,
            10 => Self::NetworkLocal,
            11 => Self::NetworkHost,
            12 => Self::OtherAccess,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataAccessDeniedKind {
    #[serde(rename = "access_denied")]
    #[default]
    AccessDenied,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataAccessDeniedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataAccessDeniedKind",
            &["access_denied"],
            None,
        )?;
        Ok(Self::AccessDenied)
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataAccessRecordedKind {
    #[serde(rename = "access_recorded")]
    #[default]
    AccessRecorded,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataAccessRecordedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataAccessRecordedKind",
            &["access_recorded"],
            None,
        )?;
        Ok(Self::AccessRecorded)
    }
}

/// Why a sandboxed run recorded and allowed its process-container access checks instead of enforcing them.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxPermissiveSource {
    /// A person approved the permissive retry for this command after a sandboxed attempt was blocked.
    #[serde(rename = "approved_retry")]
    ApprovedRetry,
    /// Device-managed policy (`sandbox.learningMode: "allow"`) starts sandboxed shell commands in permissive learning mode.
    #[serde(rename = "policy")]
    Policy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxPermissiveSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxPermissiveSource",
            &["approved_retry", "policy", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::ApprovedRetry,
            1 => Self::Policy,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataBypassDecidedKind {
    #[serde(rename = "bypass_decided")]
    #[default]
    BypassDecided,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataBypassDecidedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataBypassDecidedKind",
            &["bypass_decided"],
            None,
        )?;
        Ok(Self::BypassDecided)
    }
}

/// Where a request to run outside the process sandbox originated. Orthogonal to the outcome: the same verdict means a different thing depending on where the request came from.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxBypassSource {
    /// No longer produced. The model could once ask for a bypass in the tool call itself; no tool exposes that parameter now. Retained so historical events still deserialize.
    #[serde(rename = "model_requested")]
    ModelRequested,
    /// The runtime raised the prompt itself — either after a sandboxed attempt looked blocked, or before a run the sandbox cannot enforce at all, such as a detached command — and a person answered it. In the second case nothing had executed when the decision was made.
    #[serde(rename = "user_prompted")]
    UserPrompted,
    /// The runtime raised the prompt itself — after a sandboxed attempt looked blocked, or before a run it cannot enforce — but the permission flow produced no confirmed human answer. This includes unavailable or unreadable prompts, cancellation, and automated rule, hook, or content-exclusion denials.
    #[serde(rename = "prompt_unavailable")]
    PromptUnavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxBypassSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxBypassSource",
            &[
                "model_requested",
                "user_prompted",
                "prompt_unavailable",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::ModelRequested,
            1 => Self::UserPrompted,
            2 => Self::PromptUnavailable,
            _ => Self::Unknown,
        })
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataPermissiveRetryDecidedKind {
    #[serde(rename = "permissive_retry_decided")]
    #[default]
    PermissiveRetryDecided,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataPermissiveRetryDecidedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataPermissiveRetryDecidedKind",
            &["permissive_retry_decided"],
            None,
        )?;
        Ok(Self::PermissiveRetryDecided)
    }
}

/// Sandbox decision variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxDecisionDataPermissiveRetryCompletedKind {
    #[serde(rename = "permissive_retry_completed")]
    #[default]
    PermissiveRetryCompleted,
}
impl<'de> Deserialize<'de> for SandboxDecisionDataPermissiveRetryCompletedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxDecisionDataPermissiveRetryCompletedKind",
            &["permissive_retry_completed"],
            None,
        )?;
        Ok(Self::PermissiveRetryCompleted)
    }
}

/// Payload of `sandbox.decision`, a bounded governance record of what the process sandbox was configured to do and whether it took effect. Discriminated by `kind`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SandboxDecisionData {
    PolicyResolved(SandboxDecisionDataPolicyResolved),
    SpawnCompleted(SandboxDecisionDataSpawnCompleted),
    EnforcementState(SandboxDecisionDataEnforcementState),
    AccessDenied(SandboxDecisionDataAccessDenied),
    AccessRecorded(SandboxDecisionDataAccessRecorded),
    BypassDecided(SandboxDecisionDataBypassDecided),
    PermissiveRetryDecided(SandboxDecisionDataPermissiveRetryDecided),
    PermissiveRetryCompleted(SandboxDecisionDataPermissiveRetryCompleted),
}

/// Authority or runtime mechanism responsible for sub-agent model selection.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SubagentModelSelectionSource {
    /// Explicit model supplied by the parent agent on the task call and selected for dispatch.
    #[serde(rename = "explicit_override")]
    ExplicitOverride,
    /// Required model policy configured for the sub-agent.
    #[serde(rename = "configured_required")]
    ConfiguredRequired,
    /// Non-required model preference configured for the sub-agent.
    #[serde(rename = "configured_preference")]
    ConfiguredPreference,
    /// Complementary-model default selected for the sub-agent.
    #[serde(rename = "complementary_default")]
    ComplementaryDefault,
    /// Model inherited from the parent session.
    #[serde(rename = "session_inheritance")]
    SessionInheritance,
    /// Default model declared by the agent definition.
    #[serde(rename = "agent_definition_default")]
    AgentDefinitionDefault,
    /// Runtime policy, Auto mode, or an experiment selected the model.
    #[serde(rename = "runtime_policy")]
    RuntimePolicy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SubagentModelSelectionSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SubagentModelSelectionSource",
            &[
                "explicit_override",
                "configured_required",
                "configured_preference",
                "complementary_default",
                "session_inheritance",
                "agent_definition_default",
                "runtime_policy",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::ExplicitOverride,
            1 => Self::ConfiguredRequired,
            2 => Self::ConfiguredPreference,
            3 => Self::ComplementaryDefault,
            4 => Self::SessionInheritance,
            5 => Self::AgentDefinitionDefault,
            6 => Self::RuntimePolicy,
            _ => Self::Unknown,
        })
    }
}

/// Where the model input for a task-tool sub-agent came from.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SubagentTaskModelSource {
    /// The spawning agent supplied the task tool's model argument.
    #[serde(rename = "task_argument")]
    TaskArgument,
    /// The task omitted a model and the per-sub-agent settings entry supplied a concrete one.
    #[serde(rename = "subagent_configuration")]
    SubagentConfiguration,
    /// The task omitted a model and the user-defined custom agent's definition supplied one.
    #[serde(rename = "custom_agent_definition")]
    CustomAgentDefinition,
    /// Neither the task call, the per-sub-agent settings entry, nor a custom agent definition supplied a model.
    #[serde(rename = "unset")]
    Unset,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SubagentTaskModelSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SubagentTaskModelSource",
            &[
                "task_argument",
                "subagent_configuration",
                "custom_agent_definition",
                "unset",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::TaskArgument,
            1 => Self::SubagentConfiguration,
            2 => Self::CustomAgentDefinition,
            3 => Self::Unset,
            _ => Self::Unknown,
        })
    }
}

/// Binary asset type discriminator. Use "image" for images and "resource" otherwise.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum BinaryAssetType {
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
impl<'de> Deserialize<'de> for BinaryAssetType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "BinaryAssetType",
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

/// Message role: "system" for system prompts, "developer" for developer-injected instructions
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SystemMessageRole {
    /// System prompt message.
    #[serde(rename = "system")]
    System,
    /// Developer instruction message.
    #[serde(rename = "developer")]
    Developer,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SystemMessageRole {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SystemMessageRole",
            &["system", "developer", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::System,
            1 => Self::Developer,
            _ => Self::Unknown,
        })
    }
}

/// Authority responsible for an instruction update
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionUpdateAuthority {
    /// The runtime generated the instruction.
    #[serde(rename = "runtime")]
    Runtime,
    /// The SDK host supplied the instruction.
    #[serde(rename = "host")]
    Host,
    /// Managed policy supplied the instruction.
    #[serde(rename = "managedPolicy")]
    ManagedPolicy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionUpdateAuthority {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionUpdateAuthority",
            &["runtime", "host", "managedPolicy", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Runtime,
            1 => Self::Host,
            2 => Self::ManagedPolicy,
            _ => Self::Unknown,
        })
    }
}

/// Earliest legal insertion boundary for an instruction update
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionUpdateBoundary {
    /// Insert after the current tool-result run and before the next model request.
    #[serde(rename = "afterToolResults")]
    AfterToolResults,
    /// Insert immediately before the next model request at a provider-legal boundary.
    #[serde(rename = "nextModelRequest")]
    NextModelRequest,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionUpdateBoundary {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionUpdateBoundary",
            &["afterToolResults", "nextModelRequest", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AfterToolResults,
            1 => Self::NextModelRequest,
            _ => Self::Unknown,
        })
    }
}

/// Deterministic behavior when the active provider cannot represent the requested update
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionUpdateFallback {
    /// Rebuild the leading system prompt rather than silently dropping the update.
    #[serde(rename = "rebuildSystemPrompt")]
    RebuildSystemPrompt,
    /// Reject the update without changing authoritative instructions.
    #[serde(rename = "reject")]
    Reject,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionUpdateFallback {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionUpdateFallback",
            &["rebuildSystemPrompt", "reject", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::RebuildSystemPrompt,
            1 => Self::Reject,
            _ => Self::Unknown,
        })
    }
}

/// How long an instruction update remains authoritative
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionUpdateLifetime {
    /// Persists in event order across replay, resume, rewind, fork, and compaction.
    #[serde(rename = "persistent")]
    Persistent,
    /// Applies only to the current turn and is not restored from durable history.
    #[serde(rename = "turn")]
    Turn,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionUpdateLifetime {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionUpdateLifetime",
            &["persistent", "turn", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Persistent,
            1 => Self::Turn,
            _ => Self::Unknown,
        })
    }
}

/// Assisted Permissions assessment outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalAssessmentUnavailableStatus {
    #[serde(rename = "unavailable")]
    #[default]
    Unavailable,
}
impl<'de> Deserialize<'de> for AssistedApprovalAssessmentUnavailableStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalAssessmentUnavailableStatus",
            &["unavailable"],
            None,
        )?;
        Ok(Self::Unavailable)
    }
}

/// Assisted Permissions assessment outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalAssessmentNotRunStatus {
    #[serde(rename = "not-run")]
    #[default]
    NotRun,
}
impl<'de> Deserialize<'de> for AssistedApprovalAssessmentNotRunStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalAssessmentNotRunStatus",
            &["not-run"],
            None,
        )?;
        Ok(Self::NotRun)
    }
}

/// Assisted Permissions assessment outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalAssessmentExcludedStatus {
    #[serde(rename = "excluded")]
    #[default]
    Excluded,
}
impl<'de> Deserialize<'de> for AssistedApprovalAssessmentExcludedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalAssessmentExcludedStatus",
            &["excluded"],
            None,
        )?;
        Ok(Self::Excluded)
    }
}

/// Usable verdict returned by the Assisted Permissions judge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalCompletedRecommendation {
    /// The judge recommends automatic approval.
    #[serde(rename = "approve")]
    Approve,
    /// The judge requires an explicit permission decision.
    #[serde(rename = "requireApproval")]
    RequireApproval,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistedApprovalCompletedRecommendation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalCompletedRecommendation",
            &["approve", "requireApproval", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Approve,
            1 => Self::RequireApproval,
            _ => Self::Unknown,
        })
    }
}

/// Assisted Permissions assessment outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalAssessmentCompletedStatus {
    #[serde(rename = "completed")]
    #[default]
    Completed,
}
impl<'de> Deserialize<'de> for AssistedApprovalAssessmentCompletedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalAssessmentCompletedStatus",
            &["completed"],
            None,
        )?;
        Ok(Self::Completed)
    }
}

/// Assisted Permissions assessment outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalAssessmentFailedStatus {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for AssistedApprovalAssessmentFailedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalAssessmentFailedStatus",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// Canonical runtime-owned Assisted Permissions assessment for one permission request. This assessment is advisory; `permission.completed` remains the authoritative final permission decision.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AssistedApprovalAssessment {
    Unavailable(AssistedApprovalAssessmentUnavailable),
    NotRun(AssistedApprovalAssessmentNotRun),
    Excluded(AssistedApprovalAssessmentExcluded),
    Completed(AssistedApprovalAssessmentCompleted),
    Failed(AssistedApprovalAssessmentFailed),
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestShellKind {
    #[serde(rename = "shell")]
    #[default]
    Shell,
}
impl<'de> Deserialize<'de> for PermissionRequestShellKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestShellKind",
            &["shell"],
            None,
        )?;
        Ok(Self::Shell)
    }
}

/// Access a sandbox path grant confers
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionSandboxPathGrantAccess {
    /// Read access: the path is added to readonlyPaths.
    #[serde(rename = "read")]
    Read,
    /// Read and write access: the path is added to readwritePaths.
    #[serde(rename = "readWrite")]
    ReadWrite,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionSandboxPathGrantAccess {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionSandboxPathGrantAccess",
            &["read", "readWrite", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Read,
            1 => Self::ReadWrite,
            _ => Self::Unknown,
        })
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestWriteKind {
    #[serde(rename = "write")]
    #[default]
    Write,
}
impl<'de> Deserialize<'de> for PermissionRequestWriteKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestWriteKind",
            &["write"],
            None,
        )?;
        Ok(Self::Write)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestReadKind {
    #[serde(rename = "read")]
    #[default]
    Read,
}
impl<'de> Deserialize<'de> for PermissionRequestReadKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestReadKind",
            &["read"],
            None,
        )?;
        Ok(Self::Read)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestMcpKind {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for PermissionRequestMcpKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestMcpKind",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// Advisory recommendation the runtime attaches to a permission request whose origin it can vouch for by construction. Unlike the auto-approval judge this does not depend on auto mode and does not evaluate what the tool call does; its absence simply means the runtime has no opinion and the request follows the host's normal approval flow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRecommendation {
    /// The runtime vouches for the request's origin and recommends approving it without prompting. The host still owns the decision and may deny it; deny rules, managed policy, and the auto-approval safety judge all outrank this recommendation.
    #[serde(rename = "approve")]
    Approve,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRecommendation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRecommendation",
            &["approve", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Approve,
            _ => Self::Unknown,
        })
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestUrlKind {
    #[serde(rename = "url")]
    #[default]
    Url,
}
impl<'de> Deserialize<'de> for PermissionRequestUrlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestUrlKind",
            &["url"],
            None,
        )?;
        Ok(Self::Url)
    }
}

/// Whether this is a store or vote memory operation
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestMemoryAction {
    /// Store a new memory.
    #[serde(rename = "store")]
    Store,
    /// Vote on an existing memory.
    #[serde(rename = "vote")]
    Vote,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRequestMemoryAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestMemoryAction",
            &["store", "vote", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Store,
            1 => Self::Vote,
            _ => Self::Unknown,
        })
    }
}

/// Stage that produced this attribution.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovalEvaluationEvaluationStage {
    /// The attribution stage is unknown.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// The request resolved before assisted-approval evaluation.
    #[serde(rename = "not_reached")]
    NotReached,
    /// A runtime gate skipped the judge.
    #[serde(rename = "pre_judge")]
    PreJudge,
    /// The judge interface produced the evaluation.
    #[serde(rename = "judge")]
    Judge,
    /// A cached recommendation or another request's outcome was reused.
    #[serde(rename = "reuse")]
    Reuse,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionApprovalEvaluationEvaluationStage {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovalEvaluationEvaluationStage",
            &[
                "unknown",
                "not_reached",
                "pre_judge",
                "judge",
                "reuse",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::UnknownValue,
            1 => Self::NotReached,
            2 => Self::PreJudge,
            3 => Self::Judge,
            4 => Self::Reuse,
            _ => Self::Unknown,
        })
    }
}

/// Status of the local judge interface, not proof of a model network call.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovalEvaluationJudgeStatus {
    /// No authoritative attribution is available.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// This evaluation did not invoke the judge interface.
    #[serde(rename = "not_called")]
    NotCalled,
    /// The judge interface returned a usable verdict.
    #[serde(rename = "completed")]
    Completed,
    /// The judge interface returned an error.
    #[serde(rename = "failed")]
    Failed,
    /// This evaluation reused a cached recommendation.
    #[serde(rename = "cached")]
    Cached,
    /// This request inherited another decision without local judge attribution.
    #[serde(rename = "inherited")]
    Inherited,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionApprovalEvaluationJudgeStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovalEvaluationJudgeStatus",
            &[
                "unknown",
                "not_called",
                "completed",
                "failed",
                "cached",
                "inherited",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::UnknownValue,
            1 => Self::NotCalled,
            2 => Self::Completed,
            3 => Self::Failed,
            4 => Self::Cached,
            5 => Self::Inherited,
            _ => Self::Unknown,
        })
    }
}

/// Machine-readable runtime gate reason, never a command, path or human rationale.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovalEvaluationReasonCode {
    /// Attribution is missing or outside the supported vocabulary.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// The request resolved before assisted-approval evaluation.
    #[serde(rename = "not-reached")]
    NotReached,
    /// Assisted approval was inactive for this request.
    #[serde(rename = "inactive")]
    Inactive,
    /// The judge was skipped because authorization extraction could not safely establish a complete recent history.
    #[serde(rename = "authorization-history-incomplete")]
    AuthorizationHistoryIncomplete,
    /// Managed policy required a human decision.
    #[serde(rename = "managed-approval-required")]
    ManagedApprovalRequired,
    /// An extension capability grant required explicit user consent.
    #[serde(rename = "extension-permission-access")]
    ExtensionPermissionAccess,
    /// The request asked to bypass sandbox restrictions.
    #[serde(rename = "sandbox-bypass")]
    SandboxBypass,
    /// An action field exceeded the judge input limit.
    #[serde(rename = "action-too-long")]
    ActionTooLong,
    /// The action would grant login or privilege to whoever holds a key, which always requires explicit approval.
    #[serde(rename = "credential-minting")]
    CredentialMinting,
    /// An automation started the turn, and the command would use a live credential from a credential store, which requires explicit approval.
    #[serde(rename = "credential-exposure")]
    CredentialExposure,
    /// The script path was not authorized for inspection.
    #[serde(rename = "path-not-authorized")]
    PathNotAuthorized,
    /// A code source was excluded from review by content exclusion policy.
    #[serde(rename = "content-excluded")]
    ContentExcluded,
    /// The script working directory was invalid.
    #[serde(rename = "invalid-working-directory")]
    InvalidWorkingDirectory,
    /// The script snapshot could not be read.
    #[serde(rename = "unreadable")]
    Unreadable,
    /// The script path was not a regular file.
    #[serde(rename = "not-regular-file")]
    NotRegularFile,
    /// The script snapshot exceeded the size limit.
    #[serde(rename = "too-large")]
    TooLarge,
    /// The script snapshot was not UTF-8.
    #[serde(rename = "non-utf8")]
    NonUtf8,
    /// The script interpreter could not be inspected.
    #[serde(rename = "interpreter-unavailable")]
    InterpreterUnavailable,
    /// The interpreter snapshot exceeded the size limit.
    #[serde(rename = "interpreter-too-large")]
    InterpreterTooLarge,
    /// The shell environment could not be reviewed.
    #[serde(rename = "shell-environment-unreviewable")]
    ShellEnvironmentUnreviewable,
    /// A script path could not be represented for review.
    #[serde(rename = "unrepresentable-path")]
    UnrepresentablePath,
    /// An interpreter wrapped a script that could not be reviewed.
    #[serde(rename = "interpreter-wrapped-script")]
    InterpreterWrappedScript,
    /// The script invocation could not be reviewed.
    #[serde(rename = "unreviewable-script-invocation")]
    UnreviewableScriptInvocation,
    /// The script argument binding could not be reviewed.
    #[serde(rename = "argument-binding-unreviewable")]
    ArgumentBindingUnreviewable,
    /// The shell command could not be analyzed for execution evidence.
    #[serde(rename = "unsupported-command-shape")]
    UnsupportedCommandShape,
    /// The shell command used a code source that cannot be bound for review.
    #[serde(rename = "unsupported-source")]
    UnsupportedSource,
    /// The shell command used a code source computed at run time.
    #[serde(rename = "dynamic-source")]
    DynamicSource,
    /// The shell command referenced more code sources than can be reviewed.
    #[serde(rename = "too-many-sources")]
    TooManySources,
    /// A code-bearing executable could not be inspected.
    #[serde(rename = "executable-unavailable")]
    ExecutableUnavailable,
    /// A code-bearing executable exceeded the binding size limit.
    #[serde(rename = "executable-too-large")]
    ExecutableTooLarge,
    /// The script review metadata was malformed.
    #[serde(rename = "malformed-script-action-review")]
    MalformedScriptActionReview,
    /// The script snapshot manifest was malformed.
    #[serde(rename = "malformed-script-action-manifest")]
    MalformedScriptActionManifest,
    /// Script review was unavailable.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// The judge interface returned a usable verdict.
    #[serde(rename = "judge-verdict")]
    JudgeVerdict,
    /// The judge interface returned an error.
    #[serde(rename = "judge-error")]
    JudgeError,
    /// The fully assembled judge request exceeded the selected model's input capacity after reserving output headroom.
    #[serde(rename = "judge-context-too-large")]
    JudgeContextTooLarge,
    /// The request inherited an outcome from another decision.
    #[serde(rename = "inherited")]
    Inherited,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionApprovalEvaluationReasonCode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovalEvaluationReasonCode",
            &[
                "unknown",
                "not-reached",
                "inactive",
                "authorization-history-incomplete",
                "managed-approval-required",
                "extension-permission-access",
                "sandbox-bypass",
                "action-too-long",
                "credential-minting",
                "credential-exposure",
                "path-not-authorized",
                "content-excluded",
                "invalid-working-directory",
                "unreadable",
                "not-regular-file",
                "too-large",
                "non-utf8",
                "interpreter-unavailable",
                "interpreter-too-large",
                "shell-environment-unreviewable",
                "unrepresentable-path",
                "interpreter-wrapped-script",
                "unreviewable-script-invocation",
                "argument-binding-unreviewable",
                "unsupported-command-shape",
                "unsupported-source",
                "dynamic-source",
                "too-many-sources",
                "executable-unavailable",
                "executable-too-large",
                "malformed-script-action-review",
                "malformed-script-action-manifest",
                "unavailable",
                "judge-verdict",
                "judge-error",
                "judge-context-too-large",
                "inherited",
                "Unknown",
            ],
            Some(37),
        )?;
        Ok(match index {
            0 => Self::UnknownValue,
            1 => Self::NotReached,
            2 => Self::Inactive,
            3 => Self::AuthorizationHistoryIncomplete,
            4 => Self::ManagedApprovalRequired,
            5 => Self::ExtensionPermissionAccess,
            6 => Self::SandboxBypass,
            7 => Self::ActionTooLong,
            8 => Self::CredentialMinting,
            9 => Self::CredentialExposure,
            10 => Self::PathNotAuthorized,
            11 => Self::ContentExcluded,
            12 => Self::InvalidWorkingDirectory,
            13 => Self::Unreadable,
            14 => Self::NotRegularFile,
            15 => Self::TooLarge,
            16 => Self::NonUtf8,
            17 => Self::InterpreterUnavailable,
            18 => Self::InterpreterTooLarge,
            19 => Self::ShellEnvironmentUnreviewable,
            20 => Self::UnrepresentablePath,
            21 => Self::InterpreterWrappedScript,
            22 => Self::UnreviewableScriptInvocation,
            23 => Self::ArgumentBindingUnreviewable,
            24 => Self::UnsupportedCommandShape,
            25 => Self::UnsupportedSource,
            26 => Self::DynamicSource,
            27 => Self::TooManySources,
            28 => Self::ExecutableUnavailable,
            29 => Self::ExecutableTooLarge,
            30 => Self::MalformedScriptActionReview,
            31 => Self::MalformedScriptActionManifest,
            32 => Self::Unavailable,
            33 => Self::JudgeVerdict,
            34 => Self::JudgeError,
            35 => Self::JudgeContextTooLarge,
            36 => Self::Inherited,
            _ => Self::Unknown,
        })
    }
}

/// Why the assisted-approval judge produced no usable recommendation. Present only alongside an `error` recommendation, where the human-readable reason is a fixed string and therefore cannot distinguish these cases. Intended to make a judge failure reportable by a consumer that has no access to the host's logs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalJudgeFailureReason {
    /// The judge model call exceeded its deadline.
    #[serde(rename = "timeout")]
    Timeout,
    /// The judge model call was cancelled before it returned.
    #[serde(rename = "abort")]
    Abort,
    /// The judge model call completed but returned no content.
    #[serde(rename = "empty_response")]
    EmptyResponse,
    /// The judge model call failed (for example a transport, authentication, or rate-limit error).
    #[serde(rename = "model_error")]
    ModelError,
    /// The judge model replied, but the reply carried no ALLOW/DENY verdict.
    #[serde(rename = "parse_error")]
    ParseError,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistedApprovalJudgeFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalJudgeFailureReason",
            &[
                "timeout",
                "abort",
                "empty_response",
                "model_error",
                "parse_error",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Timeout,
            1 => Self::Abort,
            2 => Self::EmptyResponse,
            3 => Self::ModelError,
            4 => Self::ParseError,
            _ => Self::Unknown,
        })
    }
}

/// Outcome of the assisted-approval safety judge for a permission request. Present only in assisted mode; its absence means the judge did not evaluate the request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AssistedApprovalRecommendation {
    /// The judge evaluated the request and recommends automatically approving it.
    #[serde(rename = "approve")]
    Approve,
    /// The judge evaluated the request and does not recommend automatically approving it; explicit approval is required. Whether that means prompting, denying, or something else is the consumer's decision.
    #[serde(rename = "requireApproval")]
    RequireApproval,
    /// Assisted mode is enabled, but this request category is never automatically approvable (for example, sandbox-bypass requests), so the judge was not consulted.
    #[serde(rename = "excluded")]
    Excluded,
    /// The judge was consulted but did not return a usable recommendation, so the request requires explicit approval.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AssistedApprovalRecommendation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AssistedApprovalRecommendation",
            &["approve", "requireApproval", "excluded", "error", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Approve,
            1 => Self::RequireApproval,
            2 => Self::Excluded,
            3 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Vote direction (vote only)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestMemoryDirection {
    /// Vote that the memory is useful or accurate.
    #[serde(rename = "upvote")]
    Upvote,
    /// Vote that the memory is incorrect or outdated.
    #[serde(rename = "downvote")]
    Downvote,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRequestMemoryDirection {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestMemoryDirection",
            &["upvote", "downvote", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Upvote,
            1 => Self::Downvote,
            _ => Self::Unknown,
        })
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestMemoryKind {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for PermissionRequestMemoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestMemoryKind",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

/// Scope of a stored memory.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestMemoryScope {
    /// Store the memory for the current repository.
    #[serde(rename = "repository")]
    Repository,
    /// Store the memory for the current user.
    #[serde(rename = "user")]
    User,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionRequestMemoryScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestMemoryScope",
            &["repository", "user", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Repository,
            1 => Self::User,
            _ => Self::Unknown,
        })
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestCustomToolKind {
    #[serde(rename = "custom-tool")]
    #[default]
    CustomTool,
}
impl<'de> Deserialize<'de> for PermissionRequestCustomToolKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestCustomToolKind",
            &["custom-tool"],
            None,
        )?;
        Ok(Self::CustomTool)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestHookKind {
    #[serde(rename = "hook")]
    #[default]
    Hook,
}
impl<'de> Deserialize<'de> for PermissionRequestHookKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestHookKind",
            &["hook"],
            None,
        )?;
        Ok(Self::Hook)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestExtensionManagementKind {
    #[serde(rename = "extension-management")]
    #[default]
    ExtensionManagement,
}
impl<'de> Deserialize<'de> for PermissionRequestExtensionManagementKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestExtensionManagementKind",
            &["extension-management"],
            None,
        )?;
        Ok(Self::ExtensionManagement)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestWorkflowKind {
    #[serde(rename = "workflow")]
    #[default]
    Workflow,
}
impl<'de> Deserialize<'de> for PermissionRequestWorkflowKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestWorkflowKind",
            &["workflow"],
            None,
        )?;
        Ok(Self::Workflow)
    }
}

/// Operation gated by a workflow permission request.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowPermissionOperation {
    /// Running a registered workflow, which spends subagents, active time, and AI credits under the approved limits.
    #[serde(rename = "run")]
    Run,
    /// Authoring a workflow, which writes JavaScript into a session-scoped extension and loads it.
    #[serde(rename = "author")]
    Author,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowPermissionOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowPermissionOperation",
            &["run", "author", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Run,
            1 => Self::Author,
            _ => Self::Unknown,
        })
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestExtensionPermissionAccessKind {
    #[serde(rename = "extension-permission-access")]
    #[default]
    ExtensionPermissionAccess,
}
impl<'de> Deserialize<'de> for PermissionRequestExtensionPermissionAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestExtensionPermissionAccessKind",
            &["extension-permission-access"],
            None,
        )?;
        Ok(Self::ExtensionPermissionAccess)
    }
}

/// Permission kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionRequestExtensionEnvAccessKind {
    #[serde(rename = "extension-env-access")]
    #[default]
    ExtensionEnvAccess,
}
impl<'de> Deserialize<'de> for PermissionRequestExtensionEnvAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionRequestExtensionEnvAccessKind",
            &["extension-env-access"],
            None,
        )?;
        Ok(Self::ExtensionEnvAccess)
    }
}

/// Details of the permission being requested
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PermissionRequest {
    Shell(PermissionRequestShell),
    Write(PermissionRequestWrite),
    Read(PermissionRequestRead),
    Mcp(PermissionRequestMcp),
    Url(PermissionRequestUrl),
    Memory(PermissionRequestMemory),
    CustomTool(PermissionRequestCustomTool),
    Hook(PermissionRequestHook),
    ExtensionManagement(PermissionRequestExtensionManagement),
    Workflow(PermissionRequestWorkflow),
    ExtensionPermissionAccess(PermissionRequestExtensionPermissionAccess),
    ExtensionEnvAccess(PermissionRequestExtensionEnvAccess),
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestCommandsKind {
    #[serde(rename = "commands")]
    #[default]
    Commands,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestCommandsKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestCommandsKind",
            &["commands"],
            None,
        )?;
        Ok(Self::Commands)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestWriteKind {
    #[serde(rename = "write")]
    #[default]
    Write,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestWriteKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestWriteKind",
            &["write"],
            None,
        )?;
        Ok(Self::Write)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestReadKind {
    #[serde(rename = "read")]
    #[default]
    Read,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestReadKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestReadKind",
            &["read"],
            None,
        )?;
        Ok(Self::Read)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestMcpKind {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestMcpKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestMcpKind",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestUrlKind {
    #[serde(rename = "url")]
    #[default]
    Url,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestUrlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestUrlKind",
            &["url"],
            None,
        )?;
        Ok(Self::Url)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestMemoryKind {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestMemoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestMemoryKind",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestCustomToolKind {
    #[serde(rename = "custom-tool")]
    #[default]
    CustomTool,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestCustomToolKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestCustomToolKind",
            &["custom-tool"],
            None,
        )?;
        Ok(Self::CustomTool)
    }
}

/// Underlying permission kind that needs path approval
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestPathAccessKind {
    /// Read access to a filesystem path.
    #[serde(rename = "read")]
    Read,
    /// Shell command access involving a filesystem path.
    #[serde(rename = "shell")]
    Shell,
    /// Write access to a filesystem path.
    #[serde(rename = "write")]
    Write,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestPathAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestPathAccessKind",
            &["read", "shell", "write", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Read,
            1 => Self::Shell,
            2 => Self::Write,
            _ => Self::Unknown,
        })
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestPathKind {
    #[serde(rename = "path")]
    #[default]
    Path,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestPathKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestPathKind",
            &["path"],
            None,
        )?;
        Ok(Self::Path)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestHookKind {
    #[serde(rename = "hook")]
    #[default]
    Hook,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestHookKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestHookKind",
            &["hook"],
            None,
        )?;
        Ok(Self::Hook)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestExtensionManagementKind {
    #[serde(rename = "extension-management")]
    #[default]
    ExtensionManagement,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestExtensionManagementKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestExtensionManagementKind",
            &["extension-management"],
            None,
        )?;
        Ok(Self::ExtensionManagement)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestWorkflowKind {
    #[serde(rename = "workflow")]
    #[default]
    Workflow,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestWorkflowKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestWorkflowKind",
            &["workflow"],
            None,
        )?;
        Ok(Self::Workflow)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestExtensionPermissionAccessKind {
    #[serde(rename = "extension-permission-access")]
    #[default]
    ExtensionPermissionAccess,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestExtensionPermissionAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestExtensionPermissionAccessKind",
            &["extension-permission-access"],
            None,
        )?;
        Ok(Self::ExtensionPermissionAccess)
    }
}

/// Prompt kind discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionPromptRequestExtensionEnvAccessKind {
    #[serde(rename = "extension-env-access")]
    #[default]
    ExtensionEnvAccess,
}
impl<'de> Deserialize<'de> for PermissionPromptRequestExtensionEnvAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionPromptRequestExtensionEnvAccessKind",
            &["extension-env-access"],
            None,
        )?;
        Ok(Self::ExtensionEnvAccess)
    }
}

/// Derived user-facing permission prompt details for UI consumers
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PermissionPromptRequest {
    Commands(PermissionPromptRequestCommands),
    Write(PermissionPromptRequestWrite),
    Read(PermissionPromptRequestRead),
    Mcp(PermissionPromptRequestMcp),
    Url(PermissionPromptRequestUrl),
    Memory(PermissionPromptRequestMemory),
    CustomTool(PermissionPromptRequestCustomTool),
    Path(PermissionPromptRequestPath),
    Hook(PermissionPromptRequestHook),
    ExtensionManagement(PermissionPromptRequestExtensionManagement),
    Workflow(PermissionPromptRequestWorkflow),
    ExtensionPermissionAccess(PermissionPromptRequestExtensionPermissionAccess),
    ExtensionEnvAccess(PermissionPromptRequestExtensionEnvAccess),
}

/// Controlled reason or actor responsible for a permission response.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDecisionSource {
    /// The response followed the assisted-approval judge recommendation.
    #[serde(rename = "assisted_approval")]
    AssistedApproval,
    /// A human supplied the response through an interactive prompt.
    #[serde(rename = "human_response")]
    HumanResponse,
    /// The host applied a standing policy or override rather than a judge recommendation or human decision.
    #[serde(rename = "host_policy")]
    HostPolicy,
    /// The host denied the request because no interactive user response was available.
    #[serde(rename = "unattended_fallback")]
    UnattendedFallback,
    /// Historical compatibility value for sessions created while authorization carry-forward was executable. Current runtimes do not produce this source.
    #[serde(rename = "authorization_carry_forward")]
    AuthorizationCarryForward,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionDecisionSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDecisionSource",
            &[
                "assisted_approval",
                "human_response",
                "host_policy",
                "unattended_fallback",
                "authorization_carry_forward",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::AssistedApproval,
            1 => Self::HumanResponse,
            2 => Self::HostPolicy,
            3 => Self::UnattendedFallback,
            4 => Self::AuthorizationCarryForward,
            _ => Self::Unknown,
        })
    }
}

/// The permission request was approved
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovedKind {
    #[serde(rename = "approved")]
    #[default]
    Approved,
}
impl<'de> Deserialize<'de> for PermissionApprovedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovedKind",
            &["approved"],
            None,
        )?;
        Ok(Self::Approved)
    }
}

/// Approved with read-only directory authority for this session
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovedReadOnlyForSessionKind {
    #[serde(rename = "approved-read-only-for-session")]
    #[default]
    ApprovedReadOnlyForSession,
}
impl<'de> Deserialize<'de> for PermissionApprovedReadOnlyForSessionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovedReadOnlyForSessionKind",
            &["approved-read-only-for-session"],
            None,
        )?;
        Ok(Self::ApprovedReadOnlyForSession)
    }
}

/// Command approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalCommandsKind {
    #[serde(rename = "commands")]
    #[default]
    Commands,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalCommandsKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalCommandsKind",
            &["commands"],
            None,
        )?;
        Ok(Self::Commands)
    }
}

/// Read approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalReadKind {
    #[serde(rename = "read")]
    #[default]
    Read,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalReadKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalReadKind",
            &["read"],
            None,
        )?;
        Ok(Self::Read)
    }
}

/// Write approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalWriteKind {
    #[serde(rename = "write")]
    #[default]
    Write,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalWriteKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalWriteKind",
            &["write"],
            None,
        )?;
        Ok(Self::Write)
    }
}

/// MCP tool approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalMcpKind {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalMcpKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalMcpKind",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// Memory approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalMemoryKind {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalMemoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalMemoryKind",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

/// Custom tool approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalCustomToolKind {
    #[serde(rename = "custom-tool")]
    #[default]
    CustomTool,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalCustomToolKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalCustomToolKind",
            &["custom-tool"],
            None,
        )?;
        Ok(Self::CustomTool)
    }
}

/// Extension management approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalExtensionManagementKind {
    #[serde(rename = "extension-management")]
    #[default]
    ExtensionManagement,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalExtensionManagementKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalExtensionManagementKind",
            &["extension-management"],
            None,
        )?;
        Ok(Self::ExtensionManagement)
    }
}

/// Workflow approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalWorkflowKind {
    #[serde(rename = "workflow")]
    #[default]
    Workflow,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalWorkflowKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalWorkflowKind",
            &["workflow"],
            None,
        )?;
        Ok(Self::Workflow)
    }
}

/// Extension permission access approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalExtensionPermissionAccessKind {
    #[serde(rename = "extension-permission-access")]
    #[default]
    ExtensionPermissionAccess,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalExtensionPermissionAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalExtensionPermissionAccessKind",
            &["extension-permission-access"],
            None,
        )?;
        Ok(Self::ExtensionPermissionAccess)
    }
}

/// Extension environment access approval kind
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UserToolSessionApprovalExtensionEnvAccessKind {
    #[serde(rename = "extension-env-access")]
    #[default]
    ExtensionEnvAccess,
}
impl<'de> Deserialize<'de> for UserToolSessionApprovalExtensionEnvAccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UserToolSessionApprovalExtensionEnvAccessKind",
            &["extension-env-access"],
            None,
        )?;
        Ok(Self::ExtensionEnvAccess)
    }
}

/// The approval to add as a session-scoped rule
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum UserToolSessionApproval {
    Commands(UserToolSessionApprovalCommands),
    Read(UserToolSessionApprovalRead),
    Write(UserToolSessionApprovalWrite),
    Mcp(UserToolSessionApprovalMcp),
    Memory(UserToolSessionApprovalMemory),
    CustomTool(UserToolSessionApprovalCustomTool),
    ExtensionManagement(UserToolSessionApprovalExtensionManagement),
    Workflow(UserToolSessionApprovalWorkflow),
    ExtensionPermissionAccess(UserToolSessionApprovalExtensionPermissionAccess),
    ExtensionEnvAccess(UserToolSessionApprovalExtensionEnvAccess),
}

/// Approved and remembered for the rest of the session
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovedForSessionKind {
    #[serde(rename = "approved-for-session")]
    #[default]
    ApprovedForSession,
}
impl<'de> Deserialize<'de> for PermissionApprovedForSessionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovedForSessionKind",
            &["approved-for-session"],
            None,
        )?;
        Ok(Self::ApprovedForSession)
    }
}

/// Approved and persisted for this project location
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionApprovedForLocationKind {
    #[serde(rename = "approved-for-location")]
    #[default]
    ApprovedForLocation,
}
impl<'de> Deserialize<'de> for PermissionApprovedForLocationKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionApprovedForLocationKind",
            &["approved-for-location"],
            None,
        )?;
        Ok(Self::ApprovedForLocation)
    }
}

/// The permission request was cancelled before a response was used
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for PermissionCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Denied because approval rules explicitly blocked it
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDeniedByRulesKind {
    #[serde(rename = "denied-by-rules")]
    #[default]
    DeniedByRules,
}
impl<'de> Deserialize<'de> for PermissionDeniedByRulesKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDeniedByRulesKind",
            &["denied-by-rules"],
            None,
        )?;
        Ok(Self::DeniedByRules)
    }
}

/// Denied because no approval rule matched and user confirmation was unavailable
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUserKind {
    #[serde(rename = "denied-no-approval-rule-and-could-not-request-from-user")]
    #[default]
    DeniedNoApprovalRuleAndCouldNotRequestFromUser,
}
impl<'de> Deserialize<'de> for PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUserKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUserKind",
            &["denied-no-approval-rule-and-could-not-request-from-user"],
            None,
        )?;
        Ok(Self::DeniedNoApprovalRuleAndCouldNotRequestFromUser)
    }
}

/// Rejected in answer to a permission prompt. Despite the name, this does not mean a person decided; read the completion's `decisionSource`
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDeniedInteractivelyByUserKind {
    #[serde(rename = "denied-interactively-by-user")]
    #[default]
    DeniedInteractivelyByUser,
}
impl<'de> Deserialize<'de> for PermissionDeniedInteractivelyByUserKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDeniedInteractivelyByUserKind",
            &["denied-interactively-by-user"],
            None,
        )?;
        Ok(Self::DeniedInteractivelyByUser)
    }
}

/// Denied by the organization's content exclusion policy
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDeniedByContentExclusionPolicyKind {
    #[serde(rename = "denied-by-content-exclusion-policy")]
    #[default]
    DeniedByContentExclusionPolicy,
}
impl<'de> Deserialize<'de> for PermissionDeniedByContentExclusionPolicyKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDeniedByContentExclusionPolicyKind",
            &["denied-by-content-exclusion-policy"],
            None,
        )?;
        Ok(Self::DeniedByContentExclusionPolicy)
    }
}

/// Denied by a permission request hook registered by an extension or plugin
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDeniedByPermissionRequestHookKind {
    #[serde(rename = "denied-by-permission-request-hook")]
    #[default]
    DeniedByPermissionRequestHook,
}
impl<'de> Deserialize<'de> for PermissionDeniedByPermissionRequestHookKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDeniedByPermissionRequestHookKind",
            &["denied-by-permission-request-hook"],
            None,
        )?;
        Ok(Self::DeniedByPermissionRequestHook)
    }
}

/// The result of the permission request
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PermissionResult {
    Approved(PermissionApproved),
    ApprovedReadOnlyForSession(PermissionApprovedReadOnlyForSession),
    ApprovedForSession(PermissionApprovedForSession),
    ApprovedForLocation(PermissionApprovedForLocation),
    Cancelled(PermissionCancelled),
    DeniedByRules(PermissionDeniedByRules),
    DeniedNoApprovalRuleAndCouldNotRequestFromUser(
        PermissionDeniedNoApprovalRuleAndCouldNotRequestFromUser,
    ),
    DeniedInteractivelyByUser(PermissionDeniedInteractivelyByUser),
    DeniedByContentExclusionPolicy(PermissionDeniedByContentExclusionPolicy),
    DeniedByPermissionRequestHook(PermissionDeniedByPermissionRequestHook),
}

/// Direction stored in a historical extractor claim. Current runtimes do not apply it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionMessageAuthorizationPolarity {
    /// Historical claim recorded as a grant.
    #[serde(rename = "grant")]
    Grant,
    /// Historical claim recorded as a denial.
    #[serde(rename = "denial")]
    Denial,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionMessageAuthorizationPolarity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionMessageAuthorizationPolarity",
            &["grant", "denial", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Grant,
            1 => Self::Denial,
            _ => Self::Unknown,
        })
    }
}

/// Elicitation mode; "form" for structured input, "url" for browser-based. Defaults to "form" when absent.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ElicitationRequestedMode {
    /// Structured form-based elicitation.
    #[serde(rename = "form")]
    Form,
    /// Browser URL-based elicitation.
    #[serde(rename = "url")]
    Url,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ElicitationRequestedMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ElicitationRequestedMode",
            &["form", "url", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Form,
            1 => Self::Url,
            _ => Self::Unknown,
        })
    }
}

/// Schema type indicator (always 'object')
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ElicitationRequestedSchemaType {
    #[serde(rename = "object")]
    #[default]
    Object,
}
impl<'de> Deserialize<'de> for ElicitationRequestedSchemaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ElicitationRequestedSchemaType",
            &["object"],
            None,
        )?;
        Ok(Self::Object)
    }
}

/// The user action: "accept" (submitted form), "decline" (explicitly refused), or "cancel" (dismissed)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ElicitationCompletedAction {
    /// The user submitted the requested form.
    #[serde(rename = "accept")]
    Accept,
    /// The user explicitly declined the request.
    #[serde(rename = "decline")]
    Decline,
    /// The user dismissed the request.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ElicitationCompletedAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ElicitationCompletedAction",
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

/// Reason the runtime is requesting host-provided MCP OAuth credentials
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthRequestReason {
    /// Initial credentials are required before connecting to the MCP server.
    #[serde(rename = "initial")]
    Initial,
    /// The current host-provided credential was rejected and a replacement is requested.
    #[serde(rename = "refresh")]
    Refresh,
    /// The server requires a new host authorization flow before continuing.
    #[serde(rename = "reauth")]
    Reauth,
    /// The server requires a credential with additional scope or audience.
    #[serde(rename = "upscope")]
    Upscope,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthRequestReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthRequestReason",
            &["initial", "refresh", "reauth", "upscope", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Initial,
            1 => Self::Refresh,
            2 => Self::Reauth,
            3 => Self::Upscope,
            _ => Self::Unknown,
        })
    }
}

/// Optional non-default OAuth grant type. When set to 'client_credentials', the OAuth flow runs headlessly using the client_id + keychain-stored secret (no browser, no callback server).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthRequiredStaticClientConfigGrantType {
    #[serde(rename = "client_credentials")]
    #[default]
    ClientCredentials,
}
impl<'de> Deserialize<'de> for McpOauthRequiredStaticClientConfigGrantType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthRequiredStaticClientConfigGrantType",
            &["client_credentials"],
            None,
        )?;
        Ok(Self::ClientCredentials)
    }
}

/// How the pending MCP OAuth request was completed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthCompletionOutcome {
    /// The request completed with a token-backed OAuth provider.
    #[serde(rename = "token")]
    Token,
    /// The request completed without an OAuth provider.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthCompletionOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthCompletionOutcome",
            &["token", "cancelled", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Token,
            1 => Self::Cancelled,
            _ => Self::Unknown,
        })
    }
}

/// Terminal outcome of the original authorization reservation, including reconnect
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthRecoveryCompletedOutcome {
    /// Credentials were renewed and the server reconnected.
    #[serde(rename = "succeeded")]
    Succeeded,
    /// The recovery was cancelled before it completed.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Credential renewal or reconnect failed, or the server was dropped.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthRecoveryCompletedOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthRecoveryCompletedOutcome",
            &["succeeded", "cancelled", "failed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Succeeded,
            1 => Self::Cancelled,
            2 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// Why dynamic headers are being requested.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpHeadersRefreshRequiredReason {
    /// The transport is making its first dynamic header request for this server.
    #[serde(rename = "startup")]
    Startup,
    /// The previously cached dynamic headers expired.
    #[serde(rename = "ttl-expired")]
    TtlExpired,
    /// The server returned 401 and stale dynamic headers were invalidated.
    #[serde(rename = "auth-failed")]
    AuthFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpHeadersRefreshRequiredReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpHeadersRefreshRequiredReason",
            &["startup", "ttl-expired", "auth-failed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Startup,
            1 => Self::TtlExpired,
            2 => Self::AuthFailed,
            _ => Self::Unknown,
        })
    }
}

/// How the pending MCP headers refresh request resolved.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpHeadersRefreshCompletedOutcome {
    /// The host supplied dynamic headers.
    #[serde(rename = "headers")]
    Headers,
    /// The host responded with no dynamic headers.
    #[serde(rename = "none")]
    None,
    /// The host credential broker rejected or failed the refresh.
    #[serde(rename = "error")]
    Error,
    /// No response arrived within the bounded window.
    #[serde(rename = "timeout")]
    Timeout,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpHeadersRefreshCompletedOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpHeadersRefreshCompletedOutcome",
            &["headers", "none", "error", "timeout", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Headers,
            1 => Self::None,
            2 => Self::Error,
            3 => Self::Timeout,
            _ => Self::Unknown,
        })
    }
}

/// Lifecycle phase for a Rust-owned ephemeral query stream.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIEphemeralQueryPhase {
    /// The ephemeral query stream has begun.
    #[serde(rename = "started")]
    Started,
    /// A partial result chunk was produced by the stream.
    #[serde(rename = "chunk")]
    Chunk,
    /// The ephemeral query stream finished successfully.
    #[serde(rename = "completed")]
    Completed,
    /// The ephemeral query stream ended with an error.
    #[serde(rename = "failed")]
    Failed,
    /// The ephemeral query stream was cancelled before completing.
    #[serde(rename = "aborted")]
    Aborted,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIEphemeralQueryPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIEphemeralQueryPhase",
            &[
                "started",
                "chunk",
                "completed",
                "failed",
                "aborted",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Started,
            1 => Self::Chunk,
            2 => Self::Completed,
            3 => Self::Failed,
            4 => Self::Aborted,
            _ => Self::Unknown,
        })
    }
}

/// The user's auto-mode-switch choice
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutoModeSwitchResponse {
    /// Switch models for this request.
    #[serde(rename = "yes")]
    Yes,
    /// Switch models now and keep using the replacement automatically.
    #[serde(rename = "yes_always")]
    YesAlways,
    /// Do not switch models.
    #[serde(rename = "no")]
    No,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutoModeSwitchResponse {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutoModeSwitchResponse",
            &["yes", "yes_always", "no", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Yes,
            1 => Self::YesAlways,
            2 => Self::No,
            _ => Self::Unknown,
        })
    }
}

/// User action selected for an exhausted session limit.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SessionLimitsExhaustedResponseAction {
    /// Increase the current max by an exact AI Credits amount.
    #[serde(rename = "add")]
    Add,
    /// Set a new absolute max AI Credits value.
    #[serde(rename = "set")]
    Set,
    /// Remove the current session limit.
    #[serde(rename = "unset")]
    Unset,
    /// Leave the limit unchanged and cancel the blocked model request.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SessionLimitsExhaustedResponseAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SessionLimitsExhaustedResponseAction",
            &["add", "set", "unset", "cancel", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Add,
            1 => Self::Set,
            2 => Self::Unset,
            3 => Self::Cancel,
            _ => Self::Unknown,
        })
    }
}

/// Coarse request-difficulty bucket for UX explainability
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AutoModeResolvedReasoningBucket {
    /// The request looks low-reasoning; a lighter model is appropriate.
    #[serde(rename = "low")]
    Low,
    /// The request needs a moderate amount of reasoning.
    #[serde(rename = "medium")]
    Medium,
    /// The request looks high-reasoning; a stronger model is appropriate.
    #[serde(rename = "high")]
    High,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AutoModeResolvedReasoningBucket {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AutoModeResolvedReasoningBucket",
            &["low", "medium", "high", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Low,
            1 => Self::Medium,
            2 => Self::High,
            _ => Self::Unknown,
        })
    }
}

/// Current phase of organization-required plugin preparation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ManagedPluginProgressPhase {
    /// Managed settings are still resolving before plugin preparation can be planned.
    #[serde(rename = "initializing")]
    Initializing,
    /// Required plugin content is being installed.
    #[serde(rename = "installing")]
    Installing,
    /// Installed required plugin content is being updated.
    #[serde(rename = "updating")]
    Updating,
    /// Managed plugin preparation has completed.
    #[serde(rename = "complete")]
    Complete,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ManagedPluginProgressPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ManagedPluginProgressPhase",
            &[
                "initializing",
                "installing",
                "updating",
                "complete",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Initializing,
            1 => Self::Installing,
            2 => Self::Updating,
            3 => Self::Complete,
            _ => Self::Unknown,
        })
    }
}

/// Summary of which managed-settings channels contributed to the effective session policy. Use the per-channel booleans for exact provenance.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ManagedSettingsResolvedSource {
    /// Only the server/account channel contributed.
    #[serde(rename = "server")]
    Server,
    /// Only the device MDM/plist/registry/file channel contributed.
    #[serde(rename = "device")]
    Device,
    /// Only session-local SDK-host injection contributed.
    #[serde(rename = "client")]
    Client,
    /// A policy helper registered by device or server policy contributed. Device registration takes priority when present.
    #[serde(rename = "policyHelper")]
    PolicyHelper,
    /// More than one channel contributed. Ordinary keys resolve device over server over policy helper per key, while permissions compose restrictively across all present layers.
    #[serde(rename = "mixed")]
    Mixed,
    /// No managed policy is in force (no channel contributed).
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ManagedSettingsResolvedSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ManagedSettingsResolvedSource",
            &[
                "server",
                "device",
                "client",
                "policyHelper",
                "mixed",
                "none",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Server,
            1 => Self::Device,
            2 => Self::Client,
            3 => Self::PolicyHelper,
            4 => Self::Mixed,
            5 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// The category of runtime action that enterprise managed settings governed (blocked or capped)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ManagedSettingsEnforcedAction {
    /// An attempt to enter a permission mode governed by managed policy was refused or capped. The `setting` and `escalation` fields identify whether this was bypass permissions or Assisted Permissions.
    #[serde(rename = "bypass_permissions_blocked")]
    BypassPermissionsBlocked,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ManagedSettingsEnforcedAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ManagedSettingsEnforcedAction",
            &["bypass_permissions_blocked", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::BypassPermissionsBlocked,
            _ => Self::Unknown,
        })
    }
}

/// For a `bypass_permissions_blocked` action, which permission-mode or escalation primitive was refused
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ManagedSettingsEnforcedEscalation {
    /// Full allow-all permissions — automatically approving tools, paths, and URLs.
    #[serde(rename = "allow_all")]
    AllowAll,
    /// Automatic approval of all tool permission requests.
    #[serde(rename = "approve_all")]
    ApproveAll,
    /// Assisted Permissions — uses an LLM review to reduce prompts, distinct from allow-all and not a hard security boundary.
    #[serde(rename = "assisted_approval")]
    AssistedApproval,
    /// Unrestricted filesystem access outside the session's allowed directories.
    #[serde(rename = "unrestricted_paths")]
    UnrestrictedPaths,
    /// Unrestricted URL fetch access.
    #[serde(rename = "unrestricted_urls")]
    UnrestrictedUrls,
    /// A server-wide MCP "Always Allow" (or `--allow-tool <server>`) blanket that would auto-approve every tool from an MCP server. Capped to per-tool approval; each tool still prompts.
    #[serde(rename = "server_wide_mcp_approval")]
    ServerWideMcpApproval,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ManagedSettingsEnforcedEscalation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ManagedSettingsEnforcedEscalation",
            &[
                "allow_all",
                "approve_all",
                "assisted_approval",
                "unrestricted_paths",
                "unrestricted_urls",
                "server_wide_mcp_approval",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::AllowAll,
            1 => Self::ApproveAll,
            2 => Self::AssistedApproval,
            3 => Self::UnrestrictedPaths,
            4 => Self::UnrestrictedUrls,
            5 => Self::ServerWideMcpApproval,
            _ => Self::Unknown,
        })
    }
}

/// Exit plan mode action
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExitPlanModeAction {
    /// Exit plan mode without starting implementation.
    #[serde(rename = "exit_only")]
    ExitOnly,
    /// Exit plan mode and continue in interactive mode.
    #[serde(rename = "interactive")]
    Interactive,
    /// Exit plan mode and continue autonomously.
    #[serde(rename = "autopilot")]
    Autopilot,
    /// Exit plan mode and continue with parallel autonomous workers.
    #[serde(rename = "autopilot_fleet")]
    AutopilotFleet,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExitPlanModeAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExitPlanModeAction",
            &[
                "exit_only",
                "interactive",
                "autopilot",
                "autopilot_fleet",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::ExitOnly,
            1 => Self::Interactive,
            2 => Self::Autopilot,
            3 => Self::AutopilotFleet,
            _ => Self::Unknown,
        })
    }
}

/// Controlled provenance for a typed runtime response. Only `human_response`, minted by a trusted direct-interaction ingress, is human authorization evidence.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HumanResponseActor {
    /// A built-in trusted client submitted the response after direct human interaction.
    #[serde(rename = "human_response")]
    HumanResponse,
    /// A host or SDK automation submitted the response without direct human interaction.
    #[serde(rename = "host_automation")]
    HostAutomation,
    /// The response came through a legacy or otherwise unattributed ingress.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HumanResponseActor {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HumanResponseActor",
            &["human_response", "host_automation", "unknown", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::HumanResponse,
            1 => Self::HostAutomation,
            2 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// How the person refused a built-in `ask_user` question instead of answering it.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HumanResponseAskUserAction {
    /// The person explicitly declined the question.
    #[serde(rename = "decline")]
    Decline,
    /// The person dismissed the question.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HumanResponseAskUserAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HumanResponseAskUserAction",
            &["decline", "cancel", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Decline,
            1 => Self::Cancel,
            _ => Self::Unknown,
        })
    }
}

/// Runtime-owned response kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HumanResponseRecordedResponseAskUserResponseKind {
    #[serde(rename = "ask_user")]
    #[default]
    AskUser,
}
impl<'de> Deserialize<'de> for HumanResponseRecordedResponseAskUserResponseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "HumanResponseRecordedResponseAskUserResponseKind",
            &["ask_user"],
            None,
        )?;
        Ok(Self::AskUser)
    }
}

/// Runtime-owned response kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HumanResponseRecordedResponseUserInputResponseKind {
    #[serde(rename = "user_input")]
    #[default]
    UserInput,
}
impl<'de> Deserialize<'de> for HumanResponseRecordedResponseUserInputResponseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "HumanResponseRecordedResponseUserInputResponseKind",
            &["user_input"],
            None,
        )?;
        Ok(Self::UserInput)
    }
}

/// Runtime-owned response kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HumanResponseRecordedResponseExitPlanModeResponseKind {
    #[serde(rename = "exit_plan_mode")]
    #[default]
    ExitPlanMode,
}
impl<'de> Deserialize<'de> for HumanResponseRecordedResponseExitPlanModeResponseKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "HumanResponseRecordedResponseExitPlanModeResponseKind",
            &["exit_plan_mode"],
            None,
        )?;
        Ok(Self::ExitPlanMode)
    }
}

/// Exact runtime-owned question or reviewed plan paired with the typed response that settled it. A refusal records the question without an answer and never grants authority.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum HumanResponseRecordedResponse {
    AskUser(HumanResponseRecordedResponseAskUser),
    UserInput(HumanResponseRecordedResponseUserInput),
    ExitPlanMode(HumanResponseRecordedResponseExitPlanMode),
}

/// Terminal status a workflow run committed. A settled run is never `pending` or `running`, so those two members of the run-status domain are deliberately absent.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum WorkflowRunSettledStatus {
    /// The workflow body resolved and its result was committed.
    #[serde(rename = "completed")]
    Completed,
    /// The run was stopped by a limit, an approval refusal or another policy decision.
    #[serde(rename = "halted")]
    Halted,
    /// The attempt paused intentionally while preserving resumable run state.
    #[serde(rename = "paused")]
    Paused,
    /// The run was cancelled by its caller or by session disposal.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// The run failed, with `failureType` carrying the class when it has one.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for WorkflowRunSettledStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "WorkflowRunSettledStatus",
            &[
                "completed",
                "halted",
                "paused",
                "cancelled",
                "error",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Completed,
            1 => Self::Halted,
            2 => Self::Paused,
            3 => Self::Cancelled,
            4 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Source location type (e.g., project, personal-copilot, plugin, builtin, sdk, mcp)
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillSource {
    /// Skill defined in the current project's skill directories.
    #[serde(rename = "project")]
    Project,
    /// Skill discovered from a parent directory in the current workspace tree.
    #[serde(rename = "inherited")]
    Inherited,
    /// Skill defined in the user's Copilot skill directory.
    #[serde(rename = "personal-copilot")]
    PersonalCopilot,
    /// Skill defined in the user's personal agents skill directory.
    #[serde(rename = "personal-agents")]
    PersonalAgents,
    /// Skill provided by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Skill loaded from a configured custom skill directory.
    #[serde(rename = "custom")]
    Custom,
    /// Skill bundled with the runtime.
    #[serde(rename = "builtin")]
    Builtin,
    /// Pathless skill supplied lazily by an SDK skill provider.
    #[serde(rename = "sdk")]
    Sdk,
    /// Pathless skill discovered from an MCP server through the skills extension.
    #[serde(rename = "mcp")]
    Mcp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillSource",
            &[
                "project",
                "inherited",
                "personal-copilot",
                "personal-agents",
                "plugin",
                "custom",
                "builtin",
                "sdk",
                "mcp",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::Project,
            1 => Self::Inherited,
            2 => Self::PersonalCopilot,
            3 => Self::PersonalAgents,
            4 => Self::Plugin,
            5 => Self::Custom,
            6 => Self::Builtin,
            7 => Self::Sdk,
            8 => Self::Mcp,
            _ => Self::Unknown,
        })
    }
}

/// Whether configured models are advisory preferences or required constraints
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentModelPolicy {
    /// Treat the authored models as advisory preferences that callers may override.
    #[serde(rename = "preferred")]
    Preferred,
    /// Require subagent execution to use one of the authored models.
    #[serde(rename = "required")]
    Required,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentModelPolicy {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentModelPolicy",
            &["preferred", "required", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Preferred,
            1 => Self::Required,
            _ => Self::Unknown,
        })
    }
}

/// Connection status: connected, failed, needs-auth, pending, disabled, stopped, or not_configured
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerStatus {
    /// The server is connected and available.
    #[serde(rename = "connected")]
    Connected,
    /// The server failed to connect or initialize.
    #[serde(rename = "failed")]
    Failed,
    /// The server requires authentication before it can connect.
    #[serde(rename = "needs-auth")]
    NeedsAuth,
    /// The server connection is still being established.
    #[serde(rename = "pending")]
    Pending,
    /// The server is configured but disabled.
    #[serde(rename = "disabled")]
    Disabled,
    /// The server is not running: it may not have started yet, may have been explicitly stopped, or may be quarantined by restrictive managed policy. It can be restarted on demand when policy permits.
    #[serde(rename = "stopped")]
    Stopped,
    /// The server is not configured for this session.
    #[serde(rename = "not_configured")]
    NotConfigured,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerStatus",
            &[
                "connected",
                "failed",
                "needs-auth",
                "pending",
                "disabled",
                "stopped",
                "not_configured",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Connected,
            1 => Self::Failed,
            2 => Self::NeedsAuth,
            3 => Self::Pending,
            4 => Self::Disabled,
            5 => Self::Stopped,
            6 => Self::NotConfigured,
            _ => Self::Unknown,
        })
    }
}

/// Discovery source
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExtensionsLoadedExtensionSource {
    /// Extension discovered from the current project.
    #[serde(rename = "project")]
    Project,
    /// Extension discovered from the user's extension directory.
    #[serde(rename = "user")]
    User,
    /// Extension contributed by an installed plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Extension discovered from the current session's state directory.
    #[serde(rename = "session")]
    Session,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExtensionsLoadedExtensionSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExtensionsLoadedExtensionSource",
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
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExtensionsLoadedExtensionStatus {
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
impl<'de> Deserialize<'de> for ExtensionsLoadedExtensionStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExtensionsLoadedExtensionStatus",
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
