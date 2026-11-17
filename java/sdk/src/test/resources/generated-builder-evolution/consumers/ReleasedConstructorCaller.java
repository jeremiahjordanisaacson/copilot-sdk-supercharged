/* Copyright (c) Microsoft Corporation. All rights reserved. */
package fixture;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.CommandQueuedEvent.CommandQueuedEventData;
import com.microsoft.copilot.generated.rpc.MarketplaceInfo;
import com.microsoft.copilot.generated.rpc.SandboxConfigUserPolicyFilesystem;
import com.microsoft.copilot.generated.rpc.LocalSessionMetadataValue;
import com.microsoft.copilot.generated.rpc.SessionAccountsSetParams;
import com.microsoft.copilot.generated.rpc.SkillsInvokedSkill;
import com.microsoft.copilot.generated.rpc.TaskBlocker;
import com.microsoft.copilot.generated.rpc.TaskBlockerKind;
import com.microsoft.copilot.generated.rpc.TaskBlockerReason;
import com.microsoft.copilot.generated.rpc.AuthWriteSwitchActive;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryData;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryReason;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryStatus;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryOnBlocked;
import java.util.List;

public final class ReleasedConstructorCaller {
    public static void main(String[] args) throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        var skill = SkillsInvokedSkill.builder().name("example").path("/example").content("content")
                .allowedTools(List.of("tool")).disableModelInvocation(false).invokedAtTurn(5L).build();
        var blocker = TaskBlocker.builder().kind(TaskBlockerKind.PERMISSION_RECOVERY)
                .reason(TaskBlockerReason.PERMISSION_REQUIRED).resumable(true)
                .permissionRecovery(PermissionRecoveryData.builder().episodeId("episode")
                        .status(PermissionRecoveryStatus.BLOCKED).onBlocked(PermissionRecoveryOnBlocked.ASK)
                        .reason(PermissionRecoveryReason.PERMISSION_REQUIRED).maxAttempts(1L).attempts(List.of()).build())
                .build();
        var filesystem = SandboxConfigUserPolicyFilesystem.builder().readwritePaths(List.of("/write"))
                .readonlyPaths(List.of("/read")).deniedPaths(List.of("/deny")).build();
        var metadata = LocalSessionMetadataValue.builder().sessionId("session").startTime("start")
                .modifiedTime("modified").summary("summary").name("name").clientName("client").isRemote(false)
                .isDetached(true).mcTaskId("task").build();
        var account = SessionAccountsSetParams.builder().sessionId("session")
                .command(AuthWriteSwitchActive.builder().selectionId("selected").build()).build();
        System.out.println(mapper.writeValueAsString(skill));
        System.out.println(mapper.writeValueAsString(blocker));
        System.out.println(mapper.writeValueAsString(filesystem));
        System.out.println(mapper.writeValueAsString(metadata));
        System.out.println(mapper.writeValueAsString(account));
        System.out.println(mapper.writeValueAsString(CommandQueuedEventData.builder().requestId("legacy-request").command("/help").build()));
        System.out.println(mapper.writeValueAsString(MarketplaceInfo.builder().name("market").source("source")
                .isDefault(false).managed(true).available(false).build()));
    }
}
