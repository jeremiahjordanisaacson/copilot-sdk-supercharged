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
 * API methods for the {@code git} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerGitApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerGitApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Reads the checked-out branch name, returning an empty string on detached HEAD.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitStringResult> branchName(GitBranchNameParams params) {
        return caller.invoke("git.branchName", params, GitStringResult.class);
    }

    /**
     * Lists configured remote names.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitRemotesResult> remotes(GitRemotesParams params) {
        return caller.invoke("git.remotes", params, GitRemotesResult.class);
    }

    /**
     * Reads a named remote's URL, defaulting to origin.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitStringResult> remoteUrl(GitRemoteUrlParams params) {
        return caller.invoke("git.remoteUrl", params, GitStringResult.class);
    }

    /**
     * Counts staged, unstaged, and untracked changes without submodule changes.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitChangeCountsResult> changeCounts(GitChangeCountsParams params) {
        return caller.invoke("git.changeCounts", params, GitChangeCountsResult.class);
    }

    /**
     * Checks whether a local branch exists.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitBooleanResult> branchExists(GitBranchExistsParams params) {
        return caller.invoke("git.branchExists", params, GitBooleanResult.class);
    }

    /**
     * Stages all checkout changes and creates a commit, with normal Git hooks enabled.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitCommitAllResult> commitAll(GitCommitAllParams params) {
        return caller.invoke("git.commitAll", params, GitCommitAllResult.class);
    }

    /**
     * Reads file content from HEAD with a five-megabyte output limit.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitStringResult> readHeadFile(GitReadHeadFileParams params) {
        return caller.invoke("git.readHeadFile", params, GitStringResult.class);
    }

    /**
     * Creates or reuses a named worktree without changing the source checkout.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitPreparedWorktree> createWorktree(GitCreateWorktreeParams params) {
        return caller.invoke("git.createWorktree", params, GitPreparedWorktree.class);
    }

    /**
     * Checks both branch and target-directory collisions using the worktree path template.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitBooleanResult> worktreeNameAvailable(GitWorktreeNameAvailableParams params) {
        return caller.invoke("git.worktreeNameAvailable", params, GitBooleanResult.class);
    }

    /**
     * Discovers a repository root, optionally resolving linked worktrees to the main checkout.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitRootResult> findRoot(GitFindRootParams params) {
        return caller.invoke("git.findRoot", params, GitRootResult.class);
    }

    /**
     * Discovers a root without masking Git access or policy errors; a non-repository returns null.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitOptionalStringResult> findRootChecked(GitFindRootCheckedParams params) {
        return caller.invoke("git.findRootChecked", params, GitOptionalStringResult.class);
    }

    /**
     * Reads the branch identity, including detached@ followed by the short commit hash on detached HEAD.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitOptionalStringResult> currentBranch(GitCurrentBranchParams params) {
        return caller.invoke("git.currentBranch", params, GitOptionalStringResult.class);
    }

    /**
     * Resolves the default branch of a repository remote.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitStringResult> defaultBranch(GitDefaultBranchParams params) {
        return caller.invoke("git.defaultBranch", params, GitStringResult.class);
    }

    /**
     * Classifies repository remotes as GitHub or Azure DevOps, with an explicit no-root fallback.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitRemoteHostDetection> detectRemoteHosts(GitDetectRemoteHostsParams params) {
        return caller.invoke("git.detectRemoteHosts", params, GitRemoteHostDetection.class);
    }

    /**
     * Reads the repository identifier and forge host for a checkout.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitRepoIdentifierResult> repositoryIdentifier(GitRepositoryIdentifierParams params) {
        return caller.invoke("git.repositoryIdentifier", params, GitRepoIdentifierResult.class);
    }

    /**
     * Reads checkout status, preserving failures unless clean fallback is explicitly requested.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitWorkingTreeStatus> workingTreeStatus(GitWorkingTreeStatusParams params) {
        return caller.invoke("git.workingTreeStatus", params, GitWorkingTreeStatus.class);
    }

    /**
     * Summarizes checkout changes with a caller-selected file limit.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitStringResult> worktreeChangeSummary(GitWorktreeChangeSummaryParams params) {
        return caller.invoke("git.worktreeChangeSummary", params, GitStringResult.class);
    }

    /**
     * Checks whether Git accepts a branch name.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitBooleanResult> isValidBranchName(GitIsValidBranchNameParams params) {
        return caller.invoke("git.isValidBranchName", params, GitBooleanResult.class);
    }

    /**
     * Creates a worktree and moves local changes, preserving stash recovery and submodule diagnostics.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitMoveWorktreeResult> moveWorktreeChanges(GitMoveWorktreeChangesParams params) {
        return caller.invoke("git.moveWorktreeChanges", params, GitMoveWorktreeResult.class);
    }

    /**
     * Creates a worktree with collision suffixes and default-branch fallback when requested.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitCreatedWorktree> createWorktreeFromBase(GitCreateWorktreeFromBaseParams params) {
        return caller.invoke("git.createWorktreeFromBase", params, GitCreatedWorktree.class);
    }

    /**
     * Creates or reuses a pull request worktree after fetching its ref.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitPreparedWorktree> createPullRequestWorktree(GitCreatePullRequestWorktreeParams params) {
        return caller.invoke("git.createPullRequestWorktree", params, GitPreparedWorktree.class);
    }

    /**
     * Creates or reuses an issue worktree.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitPreparedWorktree> createIssueWorktree(GitCreateIssueWorktreeParams params) {
        return caller.invoke("git.createIssueWorktree", params, GitPreparedWorktree.class);
    }

    /**
     * Collects bounded checkout diffs, optionally ignoring whitespace-only edits.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitDiffChangesResult> diffChanges(GitDiffChangesParams params) {
        return caller.invoke("git.diffChanges", params, GitDiffChangesResult.class);
    }

    /**
     * Carries persisted trust from a main repository root to a worktree. A trusted subdirectory never grants trust for the full repository.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> inheritWorktreeTrust(GitInheritWorktreeTrustParams params) {
        return caller.invoke("git.inheritWorktreeTrust", params, Void.class);
    }

    /**
     * Switches the checkout to an existing local branch.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> switchBranch(GitSwitchBranchParams params) {
        return caller.invoke("git.switchBranch", params, Void.class);
    }

    /**
     * Creates a branch at HEAD and pushes it with upstream tracking to the selected remote.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> createAndPushBranch(GitCreateAndPushBranchParams params) {
        return caller.invoke("git.createAndPushBranch", params, Void.class);
    }

    /**
     * Reads the remote that the branch checked out in a working tree tracks, as `branch.&lt;name>.remote` configures it. Reports `origin` rather than failing whenever there is no tracking configuration to read — on a detached HEAD, on a branch with no upstream, or when git itself fails — because a caller asking which remote to talk to needs an answer it can act on, not an error. Marked internal because it exists to carry a CLI call site off the napi boundary onto the SDK contract; it is migration plumbing, not a surface consumers are meant to depend on.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitCurrentBranchRemoteResult> currentBranchRemote(GitCurrentBranchRemoteParams params) {
        return caller.invoke("git.currentBranchRemote", params, GitCurrentBranchRemoteResult.class);
    }

    /**
     * Computes the unstaged diff of the entire Git working tree containing cwd, including untracked files. Uses the runtime's existing Git diff implementation without requiring a live model session. Repository-review SDK hosts and CLI frontends use this operation independently of the foreground session.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<WorkspaceDiffResult> diff(GitDiffParams params) {
        return caller.invoke("git.diff", params, WorkspaceDiffResult.class);
    }

    /**
     * Collects the repository context of a working directory in one call: working tree root, repository identifier and host, current branch, and the HEAD and base commits. Every repository field is omitted when the path is not inside a git working tree, and the requested path is echoed back as `cwd`. The answer is the same `SessionWorkingDirectoryContext` that `session.metadata.recordContextChange` accepts, so a caller polling for a context change can forward the result unchanged. Marked internal because it exists to carry a CLI call site off the napi boundary onto the SDK contract; it is migration plumbing, not a surface consumers are meant to depend on. It can become public once an SDK consumer needs to derive session context from a directory itself.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<MetadataGetWorkingDirectoryContextResult> workingDirectoryContext(GitWorkingDirectoryContextParams params) {
        return caller.invoke("git.workingDirectoryContext", params, MetadataGetWorkingDirectoryContextResult.class);
    }

    /**
     * Lists the GitHub repositories a working tree's remotes point at, one entry per distinct repository, so a caller can resolve a base and head repository without parsing remote URLs itself. When several remotes name the same repository, only the first is listed, and the entry keeps that remote name. Remotes pointing at no GitHub host are left out, so an empty list means the tree reaches GitHub through no remote. Failing to read the remotes is reported as an error rather than as an empty list, because the two mean different things to a caller. Marked internal because it exists to carry a CLI call site off the napi boundary onto the SDK contract; it is migration plumbing, not a surface consumers are meant to depend on.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<GitReposFromRemotesResult> reposFromRemotes(GitReposFromRemotesParams params) {
        return caller.invoke("git.reposFromRemotes", params, GitReposFromRemotesResult.class);
    }

}
