/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
/** Grouped SessionInteraction configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionInteractionOptions {

    private AskUserVariant askUserVariant;
    private UserInputHandler onUserInputRequest;
    private ShellAuthenticationHandler onShellAuthenticationRequest;
    private List<CommandDefinition> commands;
    private ElicitationHandler onElicitationRequest;
    private ExitPlanModeHandler onExitPlanModeRequest;
    private AutoModeSwitchHandler onAutoModeSwitchRequest;
    private boolean autoModeRecovery;

    /**
     * Gets the experience used by the built-in {@code ask_user} tool.
     *
     * @return the ask-user variant, or {@code null} to use the legacy experience
     */
    public AskUserVariant getAskUserVariant() {
        return askUserVariant;
    }

    /**
     * Sets the model-facing shape of the built-in {@code ask_user} tool when the
     * session is resumed by a new client.
     * <p>
     * When unset, the option is omitted and the legacy shape is used. Set an
     * elicitation handler when selecting {@link AskUserVariant#ELICITATION}.
     *
     * @param askUserVariant
     *            the ask-user variant
     * @return this config instance for method chaining
     */
    public SessionInteractionOptions setAskUserVariant(AskUserVariant askUserVariant) {
        this.askUserVariant = askUserVariant;
        return this;
    }

    /**
     * Gets the user input request handler.
     *
     * @return the user input handler
     */
    @JsonIgnore
    public UserInputHandler getOnUserInputRequest() {
        return onUserInputRequest;
    }

    /**
     * Sets a handler for user input requests from the agent.
     *
     * @param onUserInputRequest
     *            the user input handler
     * @return this config for method chaining
     * @see UserInputHandler
     */
    public SessionInteractionOptions setOnUserInputRequest(UserInputHandler onUserInputRequest) {
        this.onUserInputRequest = onUserInputRequest;
        return this;
    }

    /**
     * Gets the shell authentication request handler.
     *
     * @return the shell authentication handler
     */
    @JsonIgnore
    public ShellAuthenticationHandler getOnShellAuthenticationRequest() {
        return onShellAuthenticationRequest;
    }

    /**
     * Sets a handler for passwords and other answers that shell commands run by the
     * agent ask for.
     * <p>
     * When provided, {@code sudo -A}, {@code ssh}, and {@code git} prompts reach
     * this handler instead of failing for lack of a terminal. Return
     * {@link ShellAuthenticationResponse#cancel()} when nobody can answer.
     *
     * @param onShellAuthenticationRequest
     *            the shell authentication handler
     * @return this config instance for method chaining
     * @see ShellAuthenticationHandler
     */
    public SessionInteractionOptions setOnShellAuthenticationRequest(
            ShellAuthenticationHandler onShellAuthenticationRequest) {
        this.onShellAuthenticationRequest = onShellAuthenticationRequest;
        return this;
    }

    /**
     * Gets the slash commands registered for this session.
     *
     * @return the list of command definitions, or {@code null}
     */
    public List<CommandDefinition> getCommands() {
        return commands == null ? null : Collections.unmodifiableList(commands);
    }

    /**
     * Sets slash commands registered for this session.
     * <p>
     * When the CLI has a TUI, each command appears as {@code /name} for the user to
     * invoke. The handler is called when the user executes the command.
     * <p>
     * An empty list replaces the commands previously registered for this session
     * with none; {@code null} leaves them unchanged.
     *
     * @param commands
     *            the list of command definitions
     * @return this config for method chaining
     * @see CommandDefinition
     */
    public SessionInteractionOptions setCommands(List<CommandDefinition> commands) {
        this.commands = commands;
        return this;
    }

    /**
     * Gets the elicitation request handler.
     *
     * @return the elicitation handler, or {@code null}
     */
    @JsonIgnore
    public ElicitationHandler getOnElicitationRequest() {
        return onElicitationRequest;
    }

    /**
     * Sets a handler for elicitation requests from the server or MCP tools.
     * <p>
     * When provided, the server will route elicitation requests to this handler and
     * report elicitation as a supported capability.
     *
     * @param onElicitationRequest
     *            the elicitation handler
     * @return this config for method chaining
     * @see ElicitationHandler
     */
    public SessionInteractionOptions setOnElicitationRequest(ElicitationHandler onElicitationRequest) {
        this.onElicitationRequest = onElicitationRequest;
        return this;
    }

    /**
     * Gets the exit-plan-mode request handler.
     *
     * @return the exit-plan-mode handler, or {@code null}
     * @since 1.0.8
     */
    @JsonIgnore
    public ExitPlanModeHandler getOnExitPlanModeRequest() {
        return onExitPlanModeRequest;
    }

    /**
     * Sets a handler for exit-plan-mode requests from the server.
     * <p>
     * When provided, the server will route {@code exitPlanMode.request} callbacks
     * to this handler.
     *
     * @param onExitPlanModeRequest
     *            the exit-plan-mode handler
     * @return this config for method chaining
     * @see ExitPlanModeHandler
     * @since 1.0.8
     */
    public SessionInteractionOptions setOnExitPlanModeRequest(ExitPlanModeHandler onExitPlanModeRequest) {
        this.onExitPlanModeRequest = onExitPlanModeRequest;
        return this;
    }

    /**
     * Gets the auto-mode-switch request handler.
     *
     * @return the auto-mode-switch handler, or {@code null}
     * @since 1.0.8
     */
    @JsonIgnore
    public AutoModeSwitchHandler getOnAutoModeSwitchRequest() {
        return onAutoModeSwitchRequest;
    }

    /**
     * Sets a handler for auto-mode-switch requests from the server.
     * <p>
     * When provided, the server will route {@code autoModeSwitch.request} callbacks
     * to this handler.
     *
     * @param onAutoModeSwitchRequest
     *            the auto-mode-switch handler
     * @return this config for method chaining
     * @see AutoModeSwitchHandler
     * @since 1.0.8
     */
    public SessionInteractionOptions setOnAutoModeSwitchRequest(AutoModeSwitchHandler onAutoModeSwitchRequest) {
        this.onAutoModeSwitchRequest = onAutoModeSwitchRequest;
        return this;
    }

    /**
     * Returns whether retained-request Auto recovery is explicitly enabled on
     * resume.
     *
     * @return {@code false} by default
     */
    public boolean isAutoModeRecovery() {
        return autoModeRecovery;
    }

    /**
     * Re-enrolls the resumed session in runtime-owned Auto recovery. Requires an
     * Auto-mode-switch handler and a positive runtime acknowledgment.
     *
     * @param autoModeRecovery
     *            whether to opt in
     * @return this instance for method chaining
     * @see SessionInteractionOptions#setAutoModeRecovery(boolean)
     */
    public SessionInteractionOptions setAutoModeRecovery(boolean autoModeRecovery) {
        this.autoModeRecovery = autoModeRecovery;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionInteractionOptions clone() {
        SessionInteractionOptions copy = new SessionInteractionOptions();
        copy.askUserVariant = this.askUserVariant;
        copy.onUserInputRequest = this.onUserInputRequest;
        copy.onShellAuthenticationRequest = this.onShellAuthenticationRequest;
        copy.commands = this.commands != null ? new ArrayList<>(this.commands) : null;
        copy.onElicitationRequest = this.onElicitationRequest;
        copy.onExitPlanModeRequest = this.onExitPlanModeRequest;
        copy.onAutoModeSwitchRequest = this.onAutoModeSwitchRequest;
        copy.autoModeRecovery = this.autoModeRecovery;
        return copy;
    }

}
