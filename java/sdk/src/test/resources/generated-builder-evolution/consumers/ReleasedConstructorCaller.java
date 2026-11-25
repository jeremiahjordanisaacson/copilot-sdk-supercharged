/* Copyright (c) Microsoft Corporation. All rights reserved. */
package fixture;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.microsoft.copilot.generated.CommandQueuedEvent.CommandQueuedEventData;
import com.microsoft.copilot.generated.UserInputCompletedEvent.UserInputCompletedEventData;
import com.microsoft.copilot.generated.UserInputRequestedEvent.UserInputRequestedEventData;
import com.microsoft.copilot.generated.ElicitationCompletedEvent.ElicitationCompletedEventData;
import com.microsoft.copilot.generated.ElicitationCompletedAction;
import com.microsoft.copilot.generated.ElicitationRequestedEvent.ElicitationRequestedEventData;
import com.microsoft.copilot.generated.rpc.SessionAccountsLoginBeginResult;
import com.microsoft.copilot.generated.rpc.MarketplaceInfo;
import com.microsoft.copilot.generated.rpc.SandboxConfigUserPolicyFilesystem;
import com.microsoft.copilot.generated.rpc.LocalSessionMetadataValue;
import com.microsoft.copilot.generated.rpc.MetadataSnapshotCurrentMode;
import com.microsoft.copilot.generated.rpc.SessionMetadataSnapshotResult;
import com.microsoft.copilot.generated.rpc.SkillsInvokedSkill;
import com.microsoft.copilot.generated.rpc.TaskBlocker;
import com.microsoft.copilot.generated.rpc.TaskBlockerKind;
import com.microsoft.copilot.generated.rpc.TaskBlockerReason;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryData;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryReason;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryStatus;
import com.microsoft.copilot.generated.rpc.PermissionRecoveryOnBlocked;
import com.microsoft.copilot.generated.rpc.UIUserInputResponse;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;

public final class ReleasedConstructorCaller {
    public static void main(String[] args) throws Exception {
        ObjectMapper mapper = new ObjectMapper().registerModule(new JavaTimeModule())
                .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
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
        var login = SessionAccountsLoginBeginResult.builder().flowId("legacy-flow")
                .step(Map.of("kind", "awaiting")).build();
        Object step = login.step();
        System.out.println(mapper.writeValueAsString(skill));
        System.out.println(mapper.writeValueAsString(blocker));
        System.out.println(mapper.writeValueAsString(filesystem));
        System.out.println(mapper.writeValueAsString(metadata));
        System.out.println(mapper.writeValueAsString(CommandQueuedEventData.builder().requestId("legacy-request").command("/help").build()));
        System.out.println(mapper.writeValueAsString(MarketplaceInfo.builder().name("market").source("source")
                .isDefault(false).managed(true).available(false).build()));
        System.out.println(mapper.writeValueAsString(
                Map.of("flowId", login.flowId(), "step", step)));
        System.out.println(mapper.writeValueAsString(SessionMetadataSnapshotResult.builder()
                .sessionId("snapshot-session").startTime(OffsetDateTime.parse("2026-01-02T03:04:05Z"))
                .modifiedTime(OffsetDateTime.parse("2026-01-02T04:05:06Z"))
                .isRemote(false).alreadyInUse(true).workspacePath("/workspace")
                .initialName("snapshot-name").clientName("snapshot-client").summary("snapshot-summary")
                .workingDirectory("/work").currentMode(MetadataSnapshotCurrentMode.PLAN)
                .selectedModel("snapshot-model").sessionLimits(null).workspace(null).build()));
        System.out.println(mapper.writeValueAsString(new UIUserInputResponse("yes", true)));
        System.out.println(mapper.writeValueAsString(
                new UserInputCompletedEventData("input", "yes", true)));
        System.out.println(mapper.writeValueAsString(
                new UserInputRequestedEventData("input", "Question?", List.of("yes"), true, "tool")));
        System.out.println(mapper.writeValueAsString(
                new ElicitationCompletedEventData("input", ElicitationCompletedAction.ACCEPT, Map.of("screenshot", ""))));
        System.out.println(mapper.writeValueAsString(
                new ElicitationRequestedEventData("input", "tool", null, null, "Question?", null, null, null)));
        System.out.println(mapper.writeValueAsString(new com.microsoft.copilot.generated.rpc.SlashCommandInfo(
                "legacy-command", List.of("legacy"), "Legacy command",
                com.microsoft.copilot.generated.rpc.SlashCommandKind.CLIENT,
                com.microsoft.copilot.generated.rpc.SlashCommandInput.builder().hint("argument").build(),
                true, false, true)));
    }
}
