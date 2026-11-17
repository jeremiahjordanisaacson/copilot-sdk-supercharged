/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionInteractionRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionInteractionRequestOptions {

    @JsonProperty("askUserVariant")
    private AskUserVariant askUserVariant;

    @JsonProperty("requestUserInput")
    private Boolean requestUserInput;

    @JsonProperty("requestShellAuthentication")
    private Boolean requestShellAuthentication;

    @JsonProperty("commands")
    private List<CommandWireDefinition> commands;

    @JsonProperty("requestElicitation")
    private Boolean requestElicitation;

    @JsonProperty("requestExitPlanMode")
    private Boolean requestExitPlanMode;

    @JsonProperty("requestAutoModeSwitch")
    private Boolean requestAutoModeSwitch;

    @JsonProperty("requestAutoModeRecovery")
    private Boolean requestAutoModeRecovery;

    /** Gets the ask-user variant. @return the ask-user variant */
    public AskUserVariant getAskUserVariant() {
        return askUserVariant;
    }

    /** Sets the ask-user variant. @param askUserVariant the ask-user variant */
    public SessionInteractionRequestOptions setAskUserVariant(AskUserVariant askUserVariant) {
        this.askUserVariant = askUserVariant;
        return this;
    }

    /** Gets request user input flag. @return the flag */
    public Boolean getRequestUserInput() {
        return requestUserInput;
    }

    /** Sets request user input flag. @param requestUserInput the flag */
    public SessionInteractionRequestOptions setRequestUserInput(boolean requestUserInput) {
        this.requestUserInput = requestUserInput;
        return this;
    }

    /** Gets request shell authentication flag. @return the flag */
    public Boolean getRequestShellAuthentication() {
        return requestShellAuthentication;
    }

    /**
     * Sets request shell authentication flag. @param requestShellAuthentication the
     * flag
     */
    public SessionInteractionRequestOptions setRequestShellAuthentication(boolean requestShellAuthentication) {
        this.requestShellAuthentication = requestShellAuthentication;
        return this;
    }

    /** Gets the commands wire definitions. @return the commands */
    public List<CommandWireDefinition> getCommands() {
        return commands == null ? null : Collections.unmodifiableList(commands);
    }

    /** Sets the commands wire definitions. @param commands the commands */
    public SessionInteractionRequestOptions setCommands(List<CommandWireDefinition> commands) {
        this.commands = commands;
        return this;
    }

    /** Gets the requestElicitation flag. @return the flag */
    public Boolean getRequestElicitation() {
        return requestElicitation;
    }

    /** Sets the requestElicitation flag. @param requestElicitation the flag */
    public SessionInteractionRequestOptions setRequestElicitation(boolean requestElicitation) {
        this.requestElicitation = requestElicitation;
        return this;
    }

    /** Gets the requestExitPlanMode flag. @return the flag */
    public Boolean getRequestExitPlanMode() {
        return requestExitPlanMode;
    }

    /** Sets the requestExitPlanMode flag. @param requestExitPlanMode the flag */
    public SessionInteractionRequestOptions setRequestExitPlanMode(Boolean requestExitPlanMode) {
        this.requestExitPlanMode = requestExitPlanMode;
        return this;
    }

    /** Gets the requestAutoModeSwitch flag. @return the flag */
    public Boolean getRequestAutoModeSwitch() {
        return requestAutoModeSwitch;
    }

    /**
     * Sets the requestAutoModeSwitch flag. @param requestAutoModeSwitch the flag
     */
    public SessionInteractionRequestOptions setRequestAutoModeSwitch(Boolean requestAutoModeSwitch) {
        this.requestAutoModeSwitch = requestAutoModeSwitch;
        return this;
    }

    /** Gets the recovery enrollment flag. @return the flag, or {@code null} */
    public Boolean getRequestAutoModeRecovery() {
        return requestAutoModeRecovery;
    }

    /** Sets recovery enrollment. @param requestAutoModeRecovery the flag */
    public SessionInteractionRequestOptions setRequestAutoModeRecovery(Boolean requestAutoModeRecovery) {
        this.requestAutoModeRecovery = requestAutoModeRecovery;
        return this;
    }

    /**
     * Clears the requestUserInput setting, reverting to the default behavior.
     */
    public void clearRequestUserInput() {
        this.requestUserInput = null;
    }

    /**
     * Clears the requestShellAuthentication setting, reverting to the default
     * behavior.
     */
    public void clearRequestShellAuthentication() {
        this.requestShellAuthentication = null;
    }

    /**
     * Clears the requestElicitation setting, reverting to the default behavior.
     */
    public void clearRequestElicitation() {
        this.requestElicitation = null;
    }
}
