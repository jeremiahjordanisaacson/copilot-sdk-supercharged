/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.SessionGitHubOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.junit.jupiter.api.Test;

import com.microsoft.copilot.rpc.GitHubTokenProviderResult;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;

class GitHubTokenProviderRegistryTest {

    @Test
    void staticTokenAndProviderAreMutuallyExclusive() {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var create = new CreateSessionOptions()
                    .setGitHub(new SessionGitHubOptions().setToken("static").setTokenProvider(
                            args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled())))
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var createError = assertThrows(CompletionException.class, () -> client.createSession(create).join());
            assertInstanceOf(IllegalArgumentException.class, createError.getCause());

            var resume = new ResumeSessionOptions()
                    .setGitHub(new SessionGitHubOptions().setToken("static").setTokenProvider(
                            args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled())))
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var resumeError = assertThrows(CompletionException.class,
                    () -> client.resumeSession("session", resume).join());
            assertInstanceOf(IllegalArgumentException.class, resumeError.getCause());
        }
    }

    @Test
    void registrationRollbackAndResumeReplacementAreIsolated() {
        var registry = new GitHubTokenProviderRegistry();
        var first = registry.register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));
        var second = registry
                .register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));

        assertNotNull(registry.get(first.id()));
        assertNotNull(registry.get(second.id()));
        first.claim("session-1");
        second.claim("session-1");
        assertNull(registry.get(first.id()));
        assertNotNull(registry.get(second.id()));

        first.close();
        assertNotNull(registry.get(second.id()));
        second.close();
        assertNull(registry.get(second.id()));

        var retired = registry
                .register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));
        retired.claim("session-2");
        registry.retire("session-2");
        assertNull(registry.get(retired.id()));

        var failed = registry
                .register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));
        failed.close();
        assertNull(registry.get(failed.id()));
    }
}
