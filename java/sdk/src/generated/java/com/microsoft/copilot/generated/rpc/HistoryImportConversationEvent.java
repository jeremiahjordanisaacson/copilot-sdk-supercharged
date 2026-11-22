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
 * Conversation records that can enter a session without model or tool execution.
 *
 * @since 1.0.0
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY, property = "type", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = HistoryImportConversationEventUserMessage.class, name = "user.message"),
    @JsonSubTypes.Type(value = HistoryImportConversationEventAssistantMessage.class, name = "assistant.message"),
    @JsonSubTypes.Type(value = HistoryImportConversationEventToolExecutionStart.class, name = "tool.execution_start"),
    @JsonSubTypes.Type(value = HistoryImportConversationEventToolExecutionComplete.class, name = "tool.execution_complete")
})
@JsonIgnoreProperties(ignoreUnknown = true)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public abstract class HistoryImportConversationEvent {

    /**
     * Returns the discriminator value for this variant.
     *
     * @return the type discriminator
     */
    public abstract String getType();
}
