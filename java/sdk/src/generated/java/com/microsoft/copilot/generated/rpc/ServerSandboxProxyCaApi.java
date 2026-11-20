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
 * API methods for the {@code sandbox.proxyCa} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerSandboxProxyCaApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerSandboxProxyCaApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Atomically replaces a stale stored certificate authority only if OS trust does not include it. Never trusts the replacement. Returns false when no certificate authority exists, it is current, or another process already trusted it. A host can use this after a user declines an update without granting consent to trust a replacement.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Boolean> rotateIfUntrusted(SandboxProxyCaRotateIfUntrustedParams params) {
        return caller.invoke("sandbox.proxyCa.rotateIfUntrusted", params, Boolean.class);
    }

    /**
     * Removes the stored certificate authority for the selected Copilot configuration directory from OS trust. Keeps the stored certificate authority. Can show an OS authentication prompt. The sandboxConfig field does not affect removal.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaRemoveForConfigDirResult> removeForConfigDir(SandboxProxyCaRemoveForConfigDirParams params) {
        return caller.invoke("sandbox.proxyCa.removeForConfigDir", params, SandboxProxyCaRemoveForConfigDirResult.class);
    }

    /**
     * Reports whether the user declined the ordinary certificate-authority installation offer in this explicit host context. A stale certificate authority still needs a new consent decision.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Boolean> getDeclined(SandboxProxyCaGetDeclinedParams params) {
        return caller.invoke("sandbox.proxyCa.getDeclined", params, Boolean.class);
    }

    /**
     * Remembers a user's decline of the ordinary certificate-authority installation offer in this explicit host context. Changes no certificate or OS trust. Hosts must not call this when an offer was never shown or when the user declines an update to a stale certificate authority.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> decline(SandboxProxyCaDeclineParams params) {
        return caller.invoke("sandbox.proxyCa.decline", params, Void.class);
    }

    /**
     * Reports whether this runtime process can offer installing the sandbox credential-proxy certificate authority into OS trust.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Boolean> checkInstallationSupport() {
        return caller.invoke("sandbox.proxyCa.checkInstallationSupport", java.util.Map.of(), Boolean.class);
    }

    /**
     * Reports whether this sandbox configuration uses the credential proxy and can need certificate-authority setup.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Boolean> checkRelevance(SandboxProxyCaCheckRelevanceParams params) {
        return caller.invoke("sandbox.proxyCa.checkRelevance", params, Boolean.class);
    }

    /**
     * Reports whether the persistent certificate authority of the sandbox credential proxy exists, whether OS trust includes it, and whether it must be rotated. Changes nothing.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaGetStatusResult> getStatus(SandboxProxyCaGetStatusParams params) {
        return caller.invoke("sandbox.proxyCa.getStatus", params, SandboxProxyCaGetStatusResult.class);
    }

    /**
     * Creates the persistent certificate authority of the sandbox credential proxy if none is stored, without changing OS trust, and returns the path of its public certificate. Keeps an existing certificate authority, even one that must be rotated. Fails where OS trust is unsupported. Trust it with sandbox.proxyCa.trust: the CLI trusts only the hosts in the saved user settings, so it refuses a certificate authority that also covers hosts from sandboxConfig.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaCreateResult> create(SandboxProxyCaCreateParams params) {
        return caller.invoke("sandbox.proxyCa.create", params, SandboxProxyCaCreateResult.class);
    }

    /**
     * Replaces the persistent certificate authority of the sandbox credential proxy with a new one for the current credential hosts. If OS trust included the old one, removes it and trusts the new one, which can show an OS authentication prompt. Running sandboxed tools keep the old certificate authority until they restart.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaRotateResult> rotate(SandboxProxyCaRotateParams params) {
        return caller.invoke("sandbox.proxyCa.rotate", params, SandboxProxyCaRotateResult.class);
    }

    /**
     * Adds the persistent certificate authority of the sandbox credential proxy to OS trust, so sandboxed clients that read only OS trust accept the proxy. Call create first. Refuses a certificate authority that is not constrained to the current credential hosts. Can show an OS authentication prompt.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaTrustResult> trust(SandboxProxyCaTrustParams params) {
        return caller.invoke("sandbox.proxyCa.trust", params, SandboxProxyCaTrustResult.class);
    }

    /**
     * Reports whether the selected Copilot profile's sandbox credential-proxy certificate authority exists, whether OS trust includes it, and whether it needs rotation. Changes nothing.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaGetStatusForConfigDirResult> getStatusForConfigDir(SandboxProxyCaGetStatusForConfigDirParams params) {
        return caller.invoke("sandbox.proxyCa.getStatusForConfigDir", params, SandboxProxyCaGetStatusForConfigDirResult.class);
    }

    /**
     * Creates the selected Copilot profile's sandbox credential-proxy certificate authority if none exists. Returns its public certificate path without changes to OS trust. Keeps an existing certificate authority, even when it needs rotation. Fails where OS trust is unsupported. Use sandbox.proxyCa.trustForConfigDir to trust it.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaCreateForConfigDirResult> createForConfigDir(SandboxProxyCaCreateForConfigDirParams params) {
        return caller.invoke("sandbox.proxyCa.createForConfigDir", params, SandboxProxyCaCreateForConfigDirResult.class);
    }

    /**
     * Replaces the selected Copilot profile's sandbox credential-proxy certificate authority for its current credential hosts. If OS trust included the old certificate authority, removes it and trusts the replacement. Can show an OS authentication prompt. Active sandboxed tools keep the old certificate authority until they restart.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaRotateForConfigDirResult> rotateForConfigDir(SandboxProxyCaRotateForConfigDirParams params) {
        return caller.invoke("sandbox.proxyCa.rotateForConfigDir", params, SandboxProxyCaRotateForConfigDirResult.class);
    }

    /**
     * Adds the selected Copilot profile's sandbox credential-proxy certificate authority to OS trust. Call createForConfigDir first. Refuses a certificate authority that is not constrained to the current credential hosts. Can show an OS authentication prompt.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaTrustForConfigDirResult> trustForConfigDir(SandboxProxyCaTrustForConfigDirParams params) {
        return caller.invoke("sandbox.proxyCa.trustForConfigDir", params, SandboxProxyCaTrustForConfigDirResult.class);
    }

    /**
     * Validates and adds a supplied credential-proxy CA certificate to OS trust. Device-management hosts can distribute a certificate file and explicitly permit its credential hosts. Includes the selected profile's saved credential hosts. Refuses unsafe certificate constraints before any OS trust change. Without a file, uses the selected profile's stored CA. Can show an OS authentication prompt.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaTrustCertificateResult> trustCertificate(SandboxProxyCaTrustCertificateParams params) {
        return caller.invoke("sandbox.proxyCa.trustCertificate", params, SandboxProxyCaTrustCertificateResult.class);
    }

    /**
     * Removes the persistent certificate authority of the sandbox credential proxy from OS trust. Keeps the stored certificate authority. Can show an OS authentication prompt. Sandboxed clients that read only OS trust then reject the proxy; clients that read the per-process certificate bundle continue to work.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SandboxProxyCaRemoveResult> remove() {
        return caller.invoke("sandbox.proxyCa.remove", java.util.Map.of(), SandboxProxyCaRemoveResult.class);
    }

}
