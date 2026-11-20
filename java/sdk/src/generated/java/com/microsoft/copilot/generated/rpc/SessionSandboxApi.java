/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code sandbox} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionSandboxApi {

    private static final com.fasterxml.jackson.databind.ObjectMapper MAPPER = RpcMapper.INSTANCE;

    private final RpcCaller caller;
    private final String sessionId;

    /** @param caller the RPC transport function */
    SessionSandboxApi(RpcCaller caller, String sessionId) {
        this.caller = caller;
        this.sessionId = sessionId;
    }

    /**
     * Returns whether managed policy requires sandbox enforcement and whether an enforcement failure has permanently blocked the session.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxGetEnforcementStatusResult> getEnforcementStatus() {
        return caller.invoke("session.sandbox.getEnforcementStatus", java.util.Map.of("sessionId", this.sessionId), SessionSandboxGetEnforcementStatusResult.class);
    }

    /**
     * Reads this runtime's cached enforcement latch without contacting a remote owner or probing a sandbox backend. Hosts use this to answer owner-status callbacks without re-entering the remote status request.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SandboxEnforcementStatus> getCachedEnforcementStatus() {
        return caller.invoke("session.sandbox.getCachedEnforcementStatus", java.util.Map.of("sessionId", this.sessionId), SandboxEnforcementStatus.class);
    }

    /**
     * Reads authoritative effective sandbox policy, source attribution and temporary choices for one live session.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxGetStateResult> getState() {
        return caller.invoke("session.sandbox.getState", java.util.Map.of("sessionId", this.sessionId), SessionSandboxGetStateResult.class);
    }

    /**
     * Registers the live session's configured sandbox credentials with the runtime's process secret filter. Resolves credentials inside the runtime and returns no secret values. Local hosts call this after sandbox option updates so later logs redact the current proxy credentials.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> registerProxySecrets() {
        return caller.invoke("session.sandbox.registerProxySecrets", java.util.Map.of("sessionId", this.sessionId), Void.class);
    }

    /**
     * Clears temporary sandbox choices for one session and adopts the supplied preference under the current managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxResetSessionChoicesResult> resetSessionChoices(SessionSandboxResetSessionChoicesParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.sandbox.resetSessionChoices", _p, SessionSandboxResetSessionChoicesResult.class);
    }

    /**
     * Invokes a closed sandbox operation through the session's original authenticated attachment or AHP owner. Requires a trusted in-process host and a matching runtime-owned session binding. Never falls back to the local presentation session.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SandboxInvokeOwnedResult> invokeOwned(SessionSandboxInvokeOwnedParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.sandbox.invokeOwned", _p, SandboxInvokeOwnedResult.class);
    }

    /**
     * Lists possible secret-bearing environment variables available to new sandboxed shells that are not already covered by credential masking. Uses local name rules and known-provider host suggestions, never returns secret values, and changes no settings or network permissions. Returns no suggestions while the session is not sandboxed.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxGetCredentialSuggestionsResult> getCredentialSuggestions() {
        return caller.invoke("session.sandbox.getCredentialSuggestions", java.util.Map.of("sessionId", this.sessionId), SessionSandboxGetCredentialSuggestionsResult.class);
    }

    /**
     * Disables sandboxing for the remainder of the current session and approves the referenced pending sandbox-bypass permission request. The request is rejected unless the exact request is still pending and the effective sandbox policy permits bypass.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxDisableForSessionResult> disableForSession(SessionSandboxDisableForSessionParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.sandbox.disableForSession", _p, SessionSandboxDisableForSessionResult.class);
    }

    /**
     * Adds the path offered by a pending sandbox escalation permission request's sandboxPathGrant to the session's sandbox policy and approves the request, so the blocked operation re-runs inside the sandbox rather than outside it. The request is rejected unless the exact request is still pending, carries a sandboxPathGrant, and the grant still takes effect under the current managed policy. Does not persist the path; hosts that store sandbox settings save it themselves.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxGrantPathForRequestResult> grantPathForRequest(SessionSandboxGrantPathForRequestParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.sandbox.grantPathForRequest", _p, SessionSandboxGrantPathForRequestResult.class);
    }

    /**
     * Checks fresh sandbox admission against the session's existing enforcement owner.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> ensureEnforcementAvailable() {
        return caller.invoke("session.sandbox.ensureEnforcementAvailable", java.util.Map.of("sessionId", this.sessionId), Void.class);
    }

    /**
     * Resolves endpoint-specific egress under the session sandbox and retains the original proxy lease.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionSandboxGetRemoteMcpEgressResult> getRemoteMcpEgress(SessionSandboxGetRemoteMcpEgressParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.sandbox.getRemoteMcpEgress", _p, SessionSandboxGetRemoteMcpEgressResult.class);
    }

}
