/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code accounts} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionAccountsApi {

    private static final com.fasterxml.jackson.databind.ObjectMapper MAPPER = RpcMapper.INSTANCE;

    private final RpcCaller caller;
    private final String sessionId;

    /** API methods for the {@code accounts.login} sub-namespace. */
    public final SessionAccountsLoginApi login;

    /** @param caller the RPC transport function */
    SessionAccountsApi(RpcCaller caller, String sessionId) {
        this.caller = caller;
        this.sessionId = sessionId;
        this.login = new SessionAccountsLoginApi(caller, sessionId);
    }

    /**
     * Lists signed-in accounts, including derived accounts. The active entry identifies the session's selected account.
     * <p>
     * Invokes the method with no params, applying the runtime defaults.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<List<Account>> list() {
        return list(null);
    }

    /**
     * Lists signed-in accounts, including derived accounts. The active entry identifies the session's selected account.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<List<Account>> list(SessionAccountsListParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = params == null ? MAPPER.createObjectNode() : MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.accounts.list", _p, RpcMapper.INSTANCE.getTypeFactory().constructCollectionType(List.class, Account.class));
    }

    /**
     * Gets the selected account and its credential-free identity metadata, including resolved Copilot user information when available.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionAccountsGetActiveResult> getActive() {
        return caller.invoke("session.accounts.getActive", java.util.Map.of("sessionId", this.sessionId), SessionAccountsGetActiveResult.class);
    }

    /**
     * Gets validation errors from the most recent authentication attempt, not errors for a specific account.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<List<AuthValidationError>> lastErrors() {
        return caller.invoke("session.accounts.lastErrors", java.util.Map.of("sessionId", this.sessionId), RpcMapper.INSTANCE.getTypeFactory().constructCollectionType(List.class, AuthValidationError.class));
    }

    /**
     * Switches the session's selected authentication to an available account.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> switchActive(SessionAccountsSwitchActiveParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.accounts.switchActive", _p, Void.class);
    }

    /**
     * Logs out the specified account, or the active account when selectionId is omitted.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Boolean> logout(SessionAccountsLogoutParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.accounts.logout", _p, Boolean.class);
    }

    /**
     * Installs and persists a GitHub authentication token for the given host and login. This credential shape is specific to GitHub, not a general token setter for every authentication provider.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> setCredentials(SessionAccountsSetCredentialsParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.accounts.setCredentials", _p, Void.class);
    }

}
