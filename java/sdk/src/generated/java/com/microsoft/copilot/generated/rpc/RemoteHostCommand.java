/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import javax.annotation.processing.Generated;

/**
 * A native relay operation, not an arbitrary remote JSON-RPC method.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "type", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = RemoteHostCommandSelectSession.class, name = "selectSession"),
    @JsonSubTypes.Type(value = RemoteHostCommandClientId.class, name = "clientId"),
    @JsonSubTypes.Type(value = RemoteHostCommandPrepareAttachments.class, name = "prepareAttachments"),
    @JsonSubTypes.Type(value = RemoteHostCommandStartTurn.class, name = "startTurn"),
    @JsonSubTypes.Type(value = RemoteHostCommandCancelTurn.class, name = "cancelTurn"),
    @JsonSubTypes.Type(value = RemoteHostCommandConfirmToolCall.class, name = "confirmToolCall"),
    @JsonSubTypes.Type(value = RemoteHostCommandConfirmToolResult.class, name = "confirmToolResult"),
    @JsonSubTypes.Type(value = RemoteHostCommandCompleteToolCall.class, name = "completeToolCall"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetMode.class, name = "setMode"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetAllowedModels.class, name = "setAllowedModels"),
    @JsonSubTypes.Type(value = RemoteHostCommandGetSessionModels.class, name = "getSessionModels"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetApprovalMode.class, name = "setApprovalMode"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetSessionApproveAll.class, name = "setSessionApproveAll"),
    @JsonSubTypes.Type(value = RemoteHostCommandCompleteExitPlanInput.class, name = "completeExitPlanInput"),
    @JsonSubTypes.Type(value = RemoteHostCommandCompleteUserInput.class, name = "completeUserInput"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetModel.class, name = "setModel"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetDraftText.class, name = "setDraftText"),
    @JsonSubTypes.Type(value = RemoteHostCommandPublishViewState.class, name = "publishViewState"),
    @JsonSubTypes.Type(value = RemoteHostCommandSelectAgent.class, name = "selectAgent"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetPendingMessage.class, name = "setPendingMessage"),
    @JsonSubTypes.Type(value = RemoteHostCommandRemovePendingMessage.class, name = "removePendingMessage"),
    @JsonSubTypes.Type(value = RemoteHostCommandSessionDiff.class, name = "sessionDiff"),
    @JsonSubTypes.Type(value = RemoteHostCommandAnnounceActiveClient.class, name = "announceActiveClient"),
    @JsonSubTypes.Type(value = RemoteHostCommandRemoveActiveClient.class, name = "removeActiveClient"),
    @JsonSubTypes.Type(value = RemoteHostCommandSetTitle.class, name = "setTitle"),
    @JsonSubTypes.Type(value = RemoteHostCommandGetAutopilotObjectiveState.class, name = "getAutopilotObjectiveState"),
    @JsonSubTypes.Type(value = RemoteHostCommandDisableSandboxForSession.class, name = "disableSandboxForSession"),
    @JsonSubTypes.Type(value = RemoteHostCommandGetPlan.class, name = "getPlan"),
    @JsonSubTypes.Type(value = RemoteHostCommandListCheckpoints.class, name = "listCheckpoints"),
    @JsonSubTypes.Type(value = RemoteHostCommandReadCheckpoint.class, name = "readCheckpoint"),
    @JsonSubTypes.Type(value = RemoteHostCommandListWorkspaceFiles.class, name = "listWorkspaceFiles"),
    @JsonSubTypes.Type(value = RemoteHostCommandCompletions.class, name = "completions"),
    @JsonSubTypes.Type(value = RemoteHostCommandBindUserRequestedShell.class, name = "bindUserRequestedShell"),
    @JsonSubTypes.Type(value = RemoteHostCommandAuthorizeMcpResource.class, name = "authorizeMcpResource")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
abstract class RemoteHostCommand {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the type discriminator
     */
    public abstract String getType();
}
