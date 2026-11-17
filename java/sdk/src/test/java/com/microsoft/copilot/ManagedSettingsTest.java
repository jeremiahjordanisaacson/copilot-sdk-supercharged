/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionGitHubOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionPolicyOptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.rpc.DisableBypassPermissionsModes;
import com.microsoft.copilot.rpc.ManagedSettings;
import com.microsoft.copilot.rpc.ManagedSettingsPermissions;
import com.microsoft.copilot.rpc.PermissionRequestResult;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.jupiter.api.Test;

class ManagedSettingsTest {
    @Test
    void forwardsManagedSettingsOnCreateAndResume() throws Exception {
        var permissions = new ManagedSettingsPermissions()
                .setDisableBypassPermissionsMode(DisableBypassPermissionsModes.DISABLE)
                .setDisableAssistedPermissionsMode(true).setDeny(List.of("Shell(rm *)"))
                .setAsk(List.of("Domain(publish.example)")).setAllow(List.of("Read(**)"))
                .setLimitTo(List.of("Domain(github.com)"));
        var managedSettings = new ManagedSettings().setPermissions(permissions);

        var create = SessionRequestBuilder
                .buildCreateRequest(
                        new CreateSessionOptions()
                                .setGitHub(new SessionGitHubOptions().setFetchManagedSettings(true)
                                        .setInteractiveUser(true))
                                .setPolicy(new SessionPolicyOptions()
                                        .setExcludedManagedSettings(List.of("sandbox", "allowedMcpServers"))
                                        .setEnforceManagedModelDefaults(true).setManagedSettings(managedSettings)),
                        "managed-create");
        var resume = SessionRequestBuilder.buildResumeRequest("managed-resume",
                new ResumeSessionOptions()
                        .setGitHub(new SessionGitHubOptions().setFetchManagedSettings(true).setInteractiveUser(true))
                        .setPolicy(new SessionPolicyOptions().setEnforceManagedModelDefaults(true)
                                .setExcludedManagedSettings(List.of("deniedMcpServers"))
                                .setManagedSettings(managedSettings)));

        assertEquals(managedSettings, create.getPolicy().getManagedSettings());
        assertEquals(managedSettings, resume.getPolicy().getManagedSettings());
        assertEquals(List.of("sandbox", "allowedMcpServers"), create.getPolicy().getExcludedManagedSettings());
        assertEquals(List.of("deniedMcpServers"), resume.getPolicy().getExcludedManagedSettings());
        assertTrue(create.getPolicy().getEnforceManagedModelDefaults());
        assertTrue(resume.getPolicy().getEnforceManagedModelDefaults());
        assertEquals(Boolean.TRUE, create.getGitHub().getInteractiveUser());
        assertEquals(Boolean.TRUE, resume.getGitHub().getInteractiveUser());
        var json = new ObjectMapper().writeValueAsString(create);
        assertTrue(json.contains("\"fetchManagedSettings\":true"));
        assertTrue(json.contains("\"excludedManagedSettings\":[\"sandbox\",\"allowedMcpServers\"]"));
        assertTrue(json.contains("\"enforceManagedModelDefaults\":true"));
        assertTrue(json.contains("\"interactiveUser\":true"));
        assertTrue(json.contains("\"managedSettings\":{\"permissions\""));
        assertTrue(json.contains("\"disableBypassPermissionsMode\":\"disable\""));
        assertTrue(json.contains("\"limitTo\":[\"Domain(github.com)\"]"));
        assertTrue(json.contains("\"disableAssistedPermissionsMode\":true"));
    }

    @Test
    void omitsInteractiveUserWhenUnset() throws Exception {
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "default-create");
        var resume = SessionRequestBuilder.buildResumeRequest("default-resume", new ResumeSessionOptions());
        var mapper = new ObjectMapper();

        assertFalse(mapper.writeValueAsString(create).contains("interactiveUser"));
        assertFalse(mapper.writeValueAsString(resume).contains("interactiveUser"));
    }

    @Test
    void acceptsFutureBypassPermissionsModes() throws Exception {
        var permissions = new ManagedSettingsPermissions().setDisableBypassPermissionsMode("future-fail-closed-mode");
        var json = new ObjectMapper().writeValueAsString(permissions);

        assertTrue(json.contains("\"disableBypassPermissionsMode\":\"future-fail-closed-mode\""));
    }

    @Test
    void preservesExplicitEmptyPermissionArrays() throws Exception {
        // Security-critical: a present empty allow list admits nothing, while an
        // absent (null) list imposes no such restriction. Jackson NON_NULL must
        // emit an explicit empty array as `[]` and omit null fields, so the two
        // remain distinguishable on the wire.
        var permissions = new ManagedSettingsPermissions().setDeny(List.of()).setAsk(List.of()).setAllow(List.of())
                .setLimitTo(List.of());
        var managedSettings = new ManagedSettings().setPermissions(permissions);
        var create = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setPolicy(new SessionPolicyOptions().setManagedSettings(managedSettings)),
                "managed-empty");

        var json = new ObjectMapper().writeValueAsString(create);
        assertTrue(json.contains("\"deny\":[]"), json);
        assertTrue(json.contains("\"ask\":[]"), json);
        assertTrue(json.contains("\"allow\":[]"), json);
        assertTrue(json.contains("\"limitTo\":[]"), json);
    }

    @Test
    void distinguishesExplicitEmptyAllowFromAbsentAllow() throws Exception {
        // Present empty allow admits nothing; the null deny/ask must be omitted.
        var permissions = new ManagedSettingsPermissions().setAllow(List.of());
        var managedSettings = new ManagedSettings().setPermissions(permissions);
        var create = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setPolicy(new SessionPolicyOptions().setManagedSettings(managedSettings)),
                "managed-mixed");

        var json = new ObjectMapper().writeValueAsString(create);
        assertTrue(json.contains("\"allow\":[]"), json);
        assertFalse(json.contains("\"deny\""), json);
        assertFalse(json.contains("\"ask\""), json);
    }

    @Test
    void directInjectionEnablesManagedSafeguards() throws Exception {
        var session = new CopilotSession("session-1", null);
        var settings = new ManagedSettings().setPermissions(new ManagedSettingsPermissions());
        var managedSettingsEnabled = new AtomicBoolean();
        var config = new CreateSessionOptions().setPolicy(new SessionPolicyOptions().setManagedSettings(settings))
                .setPermissions(new SessionPermissionOptions().setOnRequest((request, invocation) -> {
                    managedSettingsEnabled.set(invocation.isManagedSettingsEnabled());
                    return CompletableFuture.completedFuture(PermissionRequestResult.noResult());
                }));

        SessionRequestBuilder.configureSession(session, config);
        session.handlePermissionRequest(new ObjectMapper().readTree("{\"kind\":\"read\"}")).get();

        assertTrue(managedSettingsEnabled.get());
    }
}
