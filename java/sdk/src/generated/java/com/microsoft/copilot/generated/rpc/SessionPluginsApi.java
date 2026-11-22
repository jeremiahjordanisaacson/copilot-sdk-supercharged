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
 * API methods for the {@code plugins} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionPluginsApi {

    private static final com.fasterxml.jackson.databind.ObjectMapper MAPPER = RpcMapper.INSTANCE;

    private final RpcCaller caller;
    private final String sessionId;

    /** API methods for the {@code plugins.marketplaces} sub-namespace. */
    public final SessionPluginsMarketplacesApi marketplaces;

    /** @param caller the RPC transport function */
    SessionPluginsApi(RpcCaller caller, String sessionId) {
        this.caller = caller;
        this.sessionId = sessionId;
        this.marketplaces = new SessionPluginsMarketplacesApi(caller, sessionId);
    }

    /**
     * Inspects extension contributions for a changed plugin under the selected session's activation policy and installation root.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SessionPluginInspectExtensionContributionsResult> inspectExtensionContributions(SessionPluginsInspectExtensionContributionsParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.inspectExtensionContributions", _p, SessionPluginInspectExtensionContributionsResult.class);
    }

    /**
     * Lists globally installed, live, built-in, and enterprise-managed desired plugins using the live session's authoritative account, working directory, and retained managed policy.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsListResult> list() {
        return caller.invoke("session.plugins.list", java.util.Map.of("sessionId", this.sessionId), SessionPluginsListResult.class);
    }

    /**
     * Installs a plugin using the live session's authoritative account, working directory, and retained managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsInstallResult> install(SessionPluginsInstallParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.install", _p, SessionPluginsInstallResult.class);
    }

    /**
     * Uninstalls a plugin when permitted by the live session's retained managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> uninstall(SessionPluginsUninstallParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.uninstall", _p, Void.class);
    }

    /**
     * Updates an installed plugin using the live session's authoritative account, working directory, and retained managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsUpdateResult> update(SessionPluginsUpdateParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.update", _p, SessionPluginsUpdateResult.class);
    }

    /**
     * Enables installed plugins when permitted by the live session's retained managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> enable(SessionPluginsEnableParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.enable", _p, Void.class);
    }

    /**
     * Disables installed plugins when permitted by the live session's retained managed policy.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> disable(SessionPluginsDisableParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.disable", _p, Void.class);
    }

    /**
     * Reads Team and managed soft plugin defaults for the live session. Does not install plugins or persist user preferences.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsGetDefaultsResult> getDefaults() {
        return caller.invoke("session.plugins.getDefaults", java.util.Map.of("sessionId", this.sessionId), SessionPluginsGetDefaultsResult.class);
    }

    /**
     * Compares an installed plugin version with its cached marketplace catalog. Never fetches or refreshes the checkout.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsCheckUpdateResult> checkUpdate(SessionPluginsCheckUpdateParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.checkUpdate", _p, SessionPluginsCheckUpdateResult.class);
    }

    /**
     * Reloads the session's plugin set, refreshing MCP servers, custom agents, hooks, and skills cache so SDK-driven changes via `server.plugins.*` take effect immediately.
     * <p>
     * Invokes the method with no params, applying the runtime defaults.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> reload() {
        return reload(null);
    }

    /**
     * Reloads the session's plugin set, refreshing MCP servers, custom agents, hooks, and skills cache so SDK-driven changes via `server.plugins.*` take effect immediately.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> reload(SessionPluginsReloadParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = params == null ? MAPPER.createObjectNode() : MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.reload", _p, Void.class);
    }

    /**
     * Reconciles canonical workspace consent and plugin policy for the original embedding host, then returns effective activation.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<PluginsResolveActivationResult> resolveActivation() {
        return caller.invoke("session.plugins.resolveActivation", java.util.Map.of("sessionId", this.sessionId), PluginsResolveActivationResult.class);
    }

    /**
     * Installs or updates the plugins the session's managed settings require now, without waiting for the next automatic attempt after a failure, then reloads plugins. Reports progress and failures with the same `session.info` and `session.warning` events as automatic preparation. Fails when the runtime does not prepare managed plugins for the session.
     * <p>
     * Invokes the method with no params, applying the runtime defaults.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsRetryManagedResult> retryManaged() {
        return retryManaged(null);
    }

    /**
     * Installs or updates the plugins the session's managed settings require now, without waiting for the next automatic attempt after a failure, then reloads plugins. Reports progress and failures with the same `session.info` and `session.warning` events as automatic preparation. Fails when the runtime does not prepare managed plugins for the session.
     * <p>
     * Note: the {@code sessionId} field in the params record is overridden
     * by the session-scoped wrapper; any value provided is ignored.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SessionPluginsRetryManagedResult> retryManaged(SessionPluginsRetryManagedParams params) {
        com.fasterxml.jackson.databind.node.ObjectNode _p = params == null ? MAPPER.createObjectNode() : MAPPER.valueToTree(params);
        _p.put("sessionId", this.sessionId);
        return caller.invoke("session.plugins.retryManaged", _p, SessionPluginsRetryManagedResult.class);
    }

    /**
     * Inspects available and inactive plugins with live, external, and trusted bundled provenance under the bound session's configuration and policy.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SessionPluginInventoryResult> inspectInventory() {
        return caller.invoke("session.plugins.inspectInventory", java.util.Map.of("sessionId", this.sessionId), SessionPluginInventoryResult.class);
    }

}
