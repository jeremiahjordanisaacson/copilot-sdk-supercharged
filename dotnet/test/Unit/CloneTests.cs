/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Xunit;
using InstallationDecision = Microsoft.Copilot.Rpc.InstallationDecision;
using NetworkAuthenticateBasicResult = Microsoft.Copilot.Rpc.NetworkAuthenticateBasicResult;
using NetworkScope = Microsoft.Copilot.Rpc.NetworkScope;
using SandboxConfig = Microsoft.Copilot.Rpc.SandboxConfig;

namespace Microsoft.Copilot.Test.Unit;

public class CloneTests
{
#pragma warning disable MCPSDK001
    [Fact]
    public void CopilotClientOptions_Clone_CopiesAllProperties()
    {
        var extensionLaunchProvider = new TestExtensionLaunchProvider();
        InstallationConfirmationHandler installationConfirmationHandler =
            (_request, _context) => ValueTask.FromResult(InstallationDecision.Decline);
        var network = new NetworkConfig
        {
            Scope = NetworkScope.Runtime,
            ResolveProxy = (_request, _cancellationToken) => Task.FromResult<string?>(null),
            AuthenticateNegotiate = (_request, _cancellationToken) => Task.FromResult<string?>(null),
            AuthenticateBasic = (_request, _cancellationToken) => Task.FromResult<NetworkAuthenticateBasicResult?>(null),
        };
        var connection = RuntimeConnection.ForSpawnedTcp(8080, "tok", "/usr/bin/copilot", ["--verbose", "--debug"]);
        connection.WorkingDirectory = "/home/user";
        connection.Environment = new Dictionary<string, string> { ["KEY"] = "value" };
        connection.BaseDirectory = "/custom/copilot/home";
        connection.SessionIdleTimeoutSeconds = 600;
        var original = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Network = network,
            ClientInfo = new CopilotClientInfo
            {
                ApplicationName = "example-app",
                ApplicationVersion = "1.0.0",
                IntegrationName = "example-integration",
                IntegrationVersion = "2.0.0",
            },
            Runtime = connection,
            Observability = new ClientObservabilityOptions { LogLevel = CopilotLogLevel.Debug },
            GitHub = new ClientGitHubOptions { Token = "ghp_test", UseLoggedInUser = false },
            Plugins = new ClientPluginOptions { BuiltinDirectories = ["/plugins/core", "/plugins/github"] },
            Extensions = new ClientExtensionOptions { LaunchProvider = extensionLaunchProvider },
            Installations = new ClientInstallationOptions { ConfirmationHandler = installationConfirmationHandler }
        };

        var clone = original.Clone();

        Assert.Same(original.Runtime, clone.Runtime);
        Assert.Equal((original.Runtime as ChildProcessRuntimeConnection)?.WorkingDirectory, (clone.Runtime as ChildProcessRuntimeConnection)?.WorkingDirectory);
        Assert.Equal(original.Observability.LogLevel, clone.Observability.LogLevel);
        Assert.Equal((original.Runtime as ChildProcessRuntimeConnection)?.Environment, (clone.Runtime as ChildProcessRuntimeConnection)?.Environment);
        Assert.Equal(original.GitHub.Token, clone.GitHub.Token);
        Assert.Equal(original.GitHub.UseLoggedInUser, clone.GitHub.UseLoggedInUser);
        Assert.Equal((original.Runtime as OwnedRuntimeConnection)?.BaseDirectory, (clone.Runtime as OwnedRuntimeConnection)?.BaseDirectory);
        Assert.Equal(original.Plugins.BuiltinDirectories, clone.Plugins.BuiltinDirectories);
        Assert.NotSame(original.Plugins.BuiltinDirectories, clone.Plugins.BuiltinDirectories);
        Assert.Equal((original.Runtime as OwnedRuntimeConnection)?.SessionIdleTimeoutSeconds, (clone.Runtime as OwnedRuntimeConnection)?.SessionIdleTimeoutSeconds);
        Assert.Same(extensionLaunchProvider, clone.Extensions.LaunchProvider);
        Assert.Same(network, clone.Network);
        Assert.Same(installationConfirmationHandler, clone.Installations.ConfirmationHandler);
        Assert.Same(original.ClientInfo, clone.ClientInfo);
    }
#pragma warning restore MCPSDK001

    [Fact]
    public void CopilotClientOptions_Clone_ConnectionIsShared()
    {
        var connection = RuntimeConnection.ForSpawnedStdio();
        var original = new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = connection };

        var clone = original.Clone();

        Assert.Same(connection, clone.Runtime);
    }

    [Fact]
    public void CopilotClientOptions_Clone_EnvironmentIsShared()
    {
        var env = new Dictionary<string, string> { ["key"] = "value" };
        var original = new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForSpawnedStdio(environment: env) };

        var clone = original.Clone();

        Assert.Same((original.Runtime as ChildProcessRuntimeConnection)?.Environment, (clone.Runtime as ChildProcessRuntimeConnection)?.Environment);
    }

    private sealed class TestExtensionLaunchProvider : Microsoft.Copilot.Rpc.IExtensionLaunchProviderHandler
    {
        public Task<Microsoft.Copilot.Rpc.ExtensionLaunchProviderResolveResult> ResolveAsync(
            Microsoft.Copilot.Rpc.ExtensionLaunchProviderResolveRequest request,
            CancellationToken cancellationToken = default) =>
            Task.FromResult(new Microsoft.Copilot.Rpc.ExtensionLaunchProviderResolveResult());
    }

    private sealed class TestSkillProvider : ISkillProvider
    {
        public Task<IReadOnlyList<SkillProviderDescriptor>> ListSkillsAsync(CancellationToken cancellationToken) =>
            Task.FromResult<IReadOnlyList<SkillProviderDescriptor>>([]);

        public Task<string?> ReadSkillAsync(string name, CancellationToken cancellationToken) =>
            Task.FromResult<string?>(null);
    }

    [Fact]
    public void SessionConfig_Clone_CopiesAllProperties()
    {
        var original = new CreateSessionOptions
        {
            SessionId = "test-session",
            ClientName = "my-app",
            Model = "gpt-4",
            WorkingDirectory = "/workspace",
            Cloud = new CloudSessionOptions
            {
                Repository = new CloudSessionRepository
                {
                    Owner = "github",
                    Name = "copilot-sdk",
                    Branch = "main"
                }
            },
            Inference = new SessionInferenceOptions
            {
                ReasoningEffort = "high",
                ReasoningSummary = ReasoningSummary.Detailed,
                ContextTier = ContextTier.LongContext,
                EnableCitations = true
            },
            Interaction = new SessionInteractionOptions
            {
                AskUserVariant = AskUserVariant.Elicitation,
                OnExitPlanModeRequest = static (_, _) => Task.FromResult(new ExitPlanModeResult()),
                OnAutoModeSwitchRequest = static (_, _) => Task.FromResult(AutoModeSwitchResponse.No)
            },
            ConfigDirectory = "/config",
            Tools = new SessionToolOptions
            {
                AvailableTools = ["tool1", "tool2"],
                ExcludedTools = ["tool3"],
                LargeOutput = new LargeToolOutputConfig
                {
                    Enabled = true,
                    MaxSizeBytes = 2048,
                    OutputDirectory = "/tmp/out"
                }
            },
            Agents = new SessionAgentOptions
            {
                ExcludedBuiltInAgents = ["explore", "task"],
                Definitions = [new CustomAgentConfig
        {
            Name = "agent1",
            Model = "claude-haiku-4.5",
            ReasoningEffort = "high"
        }

        ],
                Selected = "agent1",
                Default = new DefaultAgentConfig
                {
                    ExcludedTools = ["hidden-tool"]
                }
            },
            Filesystem = new CreateSessionFilesystemOptions
            {
                AdditionalDirectories = ["/shared", "/generated"],
                TrackFileChanges = true
            },
            Events = new SessionEventOptions
            {
                Streaming = true,
                IncludeSubAgentStreamingEvents = false
            },
            GitHub = new SessionGitHubOptions
            {
                TelemetryEnabled = false,
                Capi = new CapiSessionOptions
                {
                    EnableWebSocketResponses = false
                }
            },
            Experiments = new SessionExperimentOptions
            {
                EnableExperimentalMode = true
            },
            Instructions = new CreateSessionInstructionOptions
            {
                OnDemandDiscovery = true,
                Directories = ["/instructions"]
            },
            Mcp = new SessionMcpOptions
            {
                Servers = new Dictionary<string, McpServerConfig>
                {
                    ["server1"] = new McpStdioServerConfig
                    {
                        Command = "echo"
                    }
                },
                OauthTokenStorage = McpOAuthTokenStorageMode.Persistent,
                DisabledServers = ["server1"]
            },
            Skills = new SessionSkillOptions
            {
                Directories = ["/skills"],
                DisabledSkills = ["skill1"]
            },
            Permissions = new SessionPermissionOptions
            {
                Sandbox = new SandboxConfig
                {
                    Enabled = true,
                    SandboxMcpServers = false
                }
            },
            Plugins = new SessionPluginOptions
            {
                Directories = ["/plugins"]
            },
            State = new SessionStateOptions
            {
                Memory = new MemoryConfiguration
                {
                    Enabled = true
                }
            },
            Execution = new SessionExecutionOptions
            {
                Limits = new SessionLimitsConfig
                {
                    MaxAiCredits = 42.5
                }
            }
        };

        var clone = original.Clone();

        Assert.Equal(original.SessionId, clone.SessionId);
        Assert.Equal(original.ClientName, clone.ClientName);
        Assert.Equal(original.Model, clone.Model);
        Assert.Equal(original.Inference.ReasoningEffort, clone.Inference.ReasoningEffort);
        Assert.Equal(original.Inference.ReasoningSummary, clone.Inference.ReasoningSummary);
        Assert.Equal(original.Inference.ContextTier, clone.Inference.ContextTier);
        Assert.Equal(original.Interaction.AskUserVariant, clone.Interaction.AskUserVariant);
        Assert.Equal(original.ConfigDirectory, clone.ConfigDirectory);
        Assert.Equal(original.Tools.AvailableTools, clone.Tools.AvailableTools);
        Assert.Equal(original.Tools.ExcludedTools, clone.Tools.ExcludedTools);
        Assert.Equal(original.Agents.ExcludedBuiltInAgents, clone.Agents.ExcludedBuiltInAgents);
        Assert.Equal(original.WorkingDirectory, clone.WorkingDirectory);
        Assert.Equal(original.Filesystem.AdditionalDirectories, clone.Filesystem.AdditionalDirectories);
        Assert.Equal(original.Events.Streaming, clone.Events.Streaming);
        Assert.Equal(original.Inference.EnableCitations, clone.Inference.EnableCitations);
        Assert.Equal(original.Filesystem.TrackFileChanges, clone.Filesystem.TrackFileChanges);
        Assert.Equal(original.GitHub.TelemetryEnabled, clone.GitHub.TelemetryEnabled);
        Assert.Equal(original.Experiments.EnableExperimentalMode, clone.Experiments.EnableExperimentalMode);
        Assert.Equal(original.Instructions.OnDemandDiscovery, clone.Instructions.OnDemandDiscovery);
        Assert.Equal(original.Events.IncludeSubAgentStreamingEvents, clone.Events.IncludeSubAgentStreamingEvents);
        Assert.Equal(original.Mcp.Servers.Count, clone.Mcp.Servers!.Count);
        Assert.Equal(original.Mcp.OauthTokenStorage, clone.Mcp.OauthTokenStorage);
        Assert.Equal(original.Agents.Definitions.Count, clone.Agents.Definitions!.Count);
        Assert.Equal(original.Agents.Definitions[0].Model, clone.Agents.Definitions[0].Model);
        Assert.Equal(original.Agents.Definitions[0].ReasoningEffort, clone.Agents.Definitions[0].ReasoningEffort);
        Assert.Equal(original.Agents.Selected, clone.Agents.Selected);
        Assert.Same(original.GitHub.Capi, clone.GitHub.Capi);
        Assert.Same(original.Cloud, clone.Cloud);
        Assert.Equal(original.Agents.Default!.ExcludedTools, clone.Agents.Default!.ExcludedTools);
        Assert.Equal(original.Skills.Directories, clone.Skills.Directories);
        Assert.Equal(original.Instructions.Directories, clone.Instructions.Directories);
        Assert.Equal(original.Skills.DisabledSkills, clone.Skills.DisabledSkills);
        Assert.Equal(original.Mcp.DisabledServers, clone.Mcp.DisabledServers);
        Assert.Same(original.Permissions.Sandbox, clone.Permissions.Sandbox);
        Assert.Equal(original.Plugins.Directories, clone.Plugins.Directories);
        Assert.Same(original.Tools.LargeOutput, clone.Tools.LargeOutput);
        Assert.Same(original.State.Memory, clone.State.Memory);
        Assert.Same(original.Execution.Limits, clone.Execution.Limits);
        Assert.Same(original.Interaction.OnExitPlanModeRequest, clone.Interaction.OnExitPlanModeRequest);
        Assert.Same(original.Interaction.OnAutoModeSwitchRequest, clone.Interaction.OnAutoModeSwitchRequest);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    [InlineData(null)]
    public void SessionConfig_Clone_PreservesRefreshCustomInstructions(bool? refresh)
    {
        var original = new CreateSessionOptions { Instructions = new CreateSessionInstructionOptions { RefreshCustomInstructions = refresh } };

        var clone = original.Clone();

        Assert.NotSame(original, clone);
        Assert.Equal(refresh, clone.Instructions.RefreshCustomInstructions);
    }

    [Fact]
    public void SessionConfig_RefreshCustomInstructions_IsCreateOnlyAndDefaultsToNull()
    {
        Assert.Null(new CreateSessionOptions().Instructions.RefreshCustomInstructions);
        Assert.Null(typeof(SessionOptionsBase).GetProperty(nameof(CreateSessionOptions.Instructions.RefreshCustomInstructions)));
        Assert.Null(typeof(ResumeSessionOptions).GetProperty(nameof(CreateSessionOptions.Instructions.RefreshCustomInstructions)));
    }

    [Fact]
    public void SessionConfig_Clone_CopiesSkillProvider()
    {
        var provider = new TestSkillProvider();
        var original = new CreateSessionOptions
        {
            Skills = new SessionSkillOptions
            {
                Provider = provider
            }
        };

        var clone = original.Clone();

        Assert.Same(provider, clone.Skills.Provider);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesSkillProvider()
    {
        var provider = new TestSkillProvider();
        var original = new ResumeSessionOptions
        {
            Skills = new SessionSkillOptions
            {
                Provider = provider
            }
        };

        var clone = original.Clone();

        Assert.Same(provider, clone.Skills.Provider);
    }

    [Fact]
    public void SessionConfig_Clone_CollectionsAreIndependent()
    {
        var original = new CreateSessionOptions
        {
            Tools = new SessionToolOptions
            {
                AvailableTools = ["tool1"],
                ExcludedTools = ["tool2"]
            },
            Agents = new SessionAgentOptions
            {
                ExcludedBuiltInAgents = ["explore"],
                Definitions = [new CustomAgentConfig
        {
            Name = "a1"
        }

        ]
            },
            Mcp = new SessionMcpOptions
            {
                Servers = new Dictionary<string, McpServerConfig>
                {
                    ["s1"] = new McpStdioServerConfig
                    {
                        Command = "echo"
                    }
                },
                DisabledServers = ["server1"]
            },
            Filesystem = new CreateSessionFilesystemOptions
            {
                AdditionalDirectories = ["/shared"]
            },
            Skills = new SessionSkillOptions
            {
                Directories = ["/skills"],
                DisabledSkills = ["skill1"]
            },
            Instructions = new CreateSessionInstructionOptions
            {
                Directories = ["/instructions"]
            }
        };

        var clone = original.Clone();

        // Mutate clone collections
        clone.Tools.AvailableTools!.Add("tool99");
        clone.Tools.ExcludedTools!.Add("tool99");
        clone.Agents.ExcludedBuiltInAgents!.Add("task");
        clone.Mcp.Servers!["s2"] = new McpStdioServerConfig { Command = "echo" };
        clone.Agents.Definitions!.Add(new CustomAgentConfig { Name = "a2" });
        clone.Filesystem.AdditionalDirectories!.Add("/generated");
        clone.Skills.Directories!.Add("/more");
        clone.Instructions.Directories!.Add("/more-instructions");
        clone.Skills.DisabledSkills!.Add("skill99");
        clone.Mcp.DisabledServers!.Add("server99");

        // Original is unaffected
        Assert.Single(original.Tools.AvailableTools!);
        Assert.Single(original.Tools.ExcludedTools!);
        Assert.Single(original.Agents.ExcludedBuiltInAgents!);
        Assert.Single(original.Mcp.Servers!);
        Assert.Single(original.Agents.Definitions!);
        Assert.Single(original.Filesystem.AdditionalDirectories!);
        Assert.Single(original.Skills.Directories!);
        Assert.Single(original.Instructions.Directories!);
        Assert.Single(original.Skills.DisabledSkills!);
        Assert.Single(original.Mcp.DisabledServers!);
    }

    [Fact]
    public void SessionConfig_Clone_PreservesMcpServersComparer()
    {
        var servers = new Dictionary<string, McpServerConfig>(StringComparer.OrdinalIgnoreCase) { ["server"] = new McpStdioServerConfig { Command = "echo" } };
        var original = new CreateSessionOptions
        {
            Mcp = new SessionMcpOptions
            {
                Servers = servers
            }
        };

        var clone = original.Clone();

        Assert.True(clone.Mcp.Servers!.ContainsKey("SERVER")); // case-insensitive lookup works
    }

    [Fact]
    public void OperationOptions_Clone_AcceptsSharedConfigurationGroups()
    {
        var create = new CreateSessionOptions();
        SessionOptionsBase sharedCreate = create;
        sharedCreate.Filesystem = new SessionFilesystemOptions { AdditionalDirectories = ["/shared"] };
        sharedCreate.Instructions = new SessionInstructionOptions { OrganizationText = "Shared instructions" };
        var createClone = create.Clone();
        createClone.Filesystem.TrustWorkingDirectory = WorkingDirectoryTrust.Session;
        createClone.Instructions.RefreshCustomInstructions = true;
        createClone.Filesystem.AdditionalDirectories!.Add("/clone");
        Assert.Equal(["/shared"], create.Filesystem.AdditionalDirectories);
        Assert.Equal("Shared instructions", createClone.Instructions.OrganizationText);
        Assert.Null(create.Instructions.RefreshCustomInstructions);

        var resume = new ResumeSessionOptions();
        SessionOptionsBase sharedResume = resume;
        sharedResume.Canvases = new SessionCanvasOptions { RequestRenderer = true };
        var resumeClone = resume.Clone();
        resumeClone.Canvases.Open = [new Microsoft.Copilot.Rpc.OpenCanvasInstance
        {
            CanvasId = "editor", ExtensionId = "test", InstanceId = "document"
        }];
        Assert.True(resumeClone.Canvases.RequestRenderer);
        Assert.Null(resume.Canvases.Open);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CollectionsAreIndependent()
    {
        var original = new ResumeSessionOptions
        {
            Tools = new SessionToolOptions
            {
                AvailableTools = ["tool1"],
                ExcludedTools = ["tool2"]
            },
            Agents = new SessionAgentOptions
            {
                ExcludedBuiltInAgents = ["explore"],
                Definitions = [new CustomAgentConfig
        {
            Name = "a1"
        }

        ]
            },
            Mcp = new SessionMcpOptions
            {
                Servers = new Dictionary<string, McpServerConfig>
                {
                    ["s1"] = new McpStdioServerConfig
                    {
                        Command = "echo"
                    }
                },
                DisabledServers = ["server1"]
            },
            Filesystem = new SessionFilesystemOptions
            {
                AdditionalDirectories = ["/shared"]
            },
            Skills = new SessionSkillOptions
            {
                Directories = ["/skills"],
                DisabledSkills = ["skill1"]
            },
            Instructions = new SessionInstructionOptions
            {
                Directories = ["/instructions"]
            },
            Canvases = new ResumeSessionCanvasOptions
            {
                Open = [new Microsoft.Copilot.Rpc.OpenCanvasInstance
                {
                    CanvasId = "editor", ExtensionId = "test", InstanceId = "document"
                }]
            }
        };

        var clone = original.Clone();

        // Mutate clone collections
        clone.Tools.AvailableTools!.Add("tool99");
        clone.Tools.ExcludedTools!.Add("tool99");
        clone.Agents.ExcludedBuiltInAgents!.Add("task");
        clone.Mcp.Servers!["s2"] = new McpStdioServerConfig { Command = "echo" };
        clone.Agents.Definitions!.Add(new CustomAgentConfig { Name = "a2" });
        clone.Filesystem.AdditionalDirectories!.Add("/generated");
        clone.Skills.Directories!.Add("/more");
        clone.Instructions.Directories!.Add("/more-instructions");
        clone.Skills.DisabledSkills!.Add("skill99");
        clone.Mcp.DisabledServers!.Add("server99");
        clone.Canvases.Open!.Clear();

        // Original is unaffected
        Assert.Single(original.Tools.AvailableTools!);
        Assert.Single(original.Tools.ExcludedTools!);
        Assert.Single(original.Agents.ExcludedBuiltInAgents!);
        Assert.Single(original.Mcp.Servers!);
        Assert.Single(original.Agents.Definitions!);
        Assert.Single(original.Filesystem.AdditionalDirectories!);
        Assert.Single(original.Skills.Directories!);
        Assert.Single(original.Instructions.Directories!);
        Assert.Single(original.Skills.DisabledSkills!);
        Assert.Single(original.Mcp.DisabledServers!);
        Assert.Single(original.Canvases.Open!);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesMcpServersComparer()
    {
        var servers = new Dictionary<string, McpServerConfig>(StringComparer.OrdinalIgnoreCase) { ["server"] = new McpStdioServerConfig { Command = "echo" } };
        var original = new ResumeSessionOptions
        {
            Mcp = new SessionMcpOptions
            {
                Servers = servers
            }
        };

        var clone = original.Clone();

        Assert.True(clone.Mcp.Servers!.ContainsKey("SERVER"));
    }

    public static TheoryData<MessageSource?> MessageSources => new()
    {
        null, MessageSource.User, MessageSource.System, MessageSource.Agent("Reviewer-7")
    };

    [Theory]
    [MemberData(nameof(MessageSources))]
    public void MessageOptions_Clone_CopiesAllProperties(MessageSource? source)
    {
        var original = new MessageOptions
        {
            Prompt = "Hello",
            Attachments = [new AttachmentFile { Path = "/test.txt", DisplayName = "test.txt" }],
            Mode = "immediate",
            AgentMode = AgentMode.Plan,
            Source = source,
            DisplayPrompt = "Display text",
            ClientTurnId = "host-turn-1",
            RequestHeaders = new Dictionary<string, string> { ["X-Test"] = "original" },
        };

        var clone = original.Clone();

        Assert.Equal(original.Prompt, clone.Prompt);
        Assert.Equal(original.Mode, clone.Mode);
        Assert.Equal(original.AgentMode, clone.AgentMode);
        Assert.Equal(original.Source, clone.Source);
        Assert.Equal(original.DisplayPrompt, clone.DisplayPrompt);
        Assert.Equal(original.ClientTurnId, clone.ClientTurnId);
        Assert.Single(clone.Attachments!);
        Assert.NotSame(original.Attachments, clone.Attachments);
        Assert.Equal(original.RequestHeaders, clone.RequestHeaders);
        Assert.NotSame(original.RequestHeaders, clone.RequestHeaders);

        clone.Source = source == MessageSource.System ? MessageSource.User : MessageSource.System;
        clone.RequestHeaders!["X-Test"] = "changed";
        Assert.Equal(source, original.Source);
        Assert.Equal("original", original.RequestHeaders["X-Test"]);
    }

    [Fact]
    public void MessageOptions_Clone_AttachmentsAreIndependent()
    {
        var original = new MessageOptions
        {
            Attachments = [new AttachmentFile { Path = "/test.txt", DisplayName = "test.txt" }],
        };

        var clone = original.Clone();

        clone.Attachments!.Add(new AttachmentFile { Path = "/other.txt", DisplayName = "other.txt" });

        Assert.Single(original.Attachments!);
    }

    [Fact]
    public void Clone_WithNullCollections_ReturnsNullCollections()
    {
        var original = new CreateSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.Tools.AvailableTools);
        Assert.Null(clone.Tools.ExcludedTools);
        Assert.Null(clone.Agents.ExcludedBuiltInAgents);
        Assert.Null(clone.Mcp.Servers);
        Assert.Null(clone.Agents.Definitions);
        Assert.Null(clone.Skills.Directories);
        Assert.Null(clone.Instructions.Directories);
        Assert.Null(clone.Skills.DisabledSkills);
        Assert.Null(clone.Mcp.DisabledServers);
        Assert.Null(clone.Permissions.Sandbox);
        Assert.Null(clone.Tools.Definitions);
        Assert.Null(clone.Agents.Default);
        Assert.True(clone.Events.IncludeSubAgentStreamingEvents);
    }

    [Fact]
    public void SessionConfig_Clone_CopiesAgentProperty()
    {
        var original = new CreateSessionOptions
        {
            Agents = new SessionAgentOptions
            {
                Selected = "test-agent",
                Definitions = [new CustomAgentConfig
        {
            Name = "test-agent",
            Prompt = "You are a test agent."
        }

        ]
            }
        };

        var clone = original.Clone();

        Assert.Equal("test-agent", clone.Agents.Selected);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesAgentProperty()
    {
        var original = new ResumeSessionOptions
        {
            Agents = new SessionAgentOptions
            {
                Selected = "test-agent",
                Definitions = [new CustomAgentConfig
        {
            Name = "test-agent",
            Prompt = "You are a test agent."
        }

        ]
            }
        };

        var clone = original.Clone();

        Assert.Equal("test-agent", clone.Agents.Selected);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesModeSwitchHandlers()
    {
        var original = new ResumeSessionOptions
        {
            Interaction = new SessionInteractionOptions
            {
                OnExitPlanModeRequest = static (_, _) => Task.FromResult(new ExitPlanModeResult()),
                OnAutoModeSwitchRequest = static (_, _) => Task.FromResult(AutoModeSwitchResponse.No)
            }
        };

        var clone = original.Clone();

        Assert.Same(original.Interaction.OnExitPlanModeRequest, clone.Interaction.OnExitPlanModeRequest);
        Assert.Same(original.Interaction.OnAutoModeSwitchRequest, clone.Interaction.OnAutoModeSwitchRequest);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesIncludeSubAgentStreamingEvents()
    {
        var original = new ResumeSessionOptions
        {
            Events = new SessionEventOptions
            {
                IncludeSubAgentStreamingEvents = false
            }
        };

        var clone = original.Clone();

        Assert.False(clone.Events.IncludeSubAgentStreamingEvents);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesIncludeSubAgentStreamingEventsDefault()
    {
        var original = new ResumeSessionOptions();

        var clone = original.Clone();

        Assert.True(clone.Events.IncludeSubAgentStreamingEvents);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesEnableSessionTelemetry()
    {
        var original = new ResumeSessionOptions
        {
            GitHub = new SessionGitHubOptions
            {
                TelemetryEnabled = false
            }
        };

        var clone = original.Clone();

        Assert.False(clone.GitHub.TelemetryEnabled);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesEnableExperimentalMode()
    {
        var original = new ResumeSessionOptions
        {
            Experiments = new SessionExperimentOptions
            {
                EnableExperimentalMode = true
            }
        };

        var clone = original.Clone();

        Assert.True(clone.Experiments.EnableExperimentalMode);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesContinuePendingWork()
    {
        var original = new ResumeSessionOptions
        {
            ContinuePendingWork = true
        };

        var clone = original.Clone();

        Assert.True(clone.ContinuePendingWork);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesReasoningSummary()
    {
        var original = new ResumeSessionOptions
        {
            Inference = new SessionInferenceOptions
            {
                ReasoningSummary = ReasoningSummary.None
            }
        };

        var clone = original.Clone();

        Assert.Equal(original.Inference.ReasoningSummary, clone.Inference.ReasoningSummary);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesContextTier()
    {
        var original = new ResumeSessionOptions
        {
            Inference = new SessionInferenceOptions
            {
                ContextTier = ContextTier.LongContext
            }
        };

        var clone = original.Clone();

        Assert.Equal(original.Inference.ContextTier, clone.Inference.ContextTier);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesPluginDirectoriesAndLargeOutput()
    {
        var largeOutput = new LargeToolOutputConfig { Enabled = false, MaxSizeBytes = 4096, OutputDirectory = "/tmp/resume" };
        var original = new ResumeSessionOptions
        {
            Plugins = new SessionPluginOptions
            {
                Directories = ["/resume/plugins"]
            },
            Tools = new SessionToolOptions
            {
                LargeOutput = largeOutput
            },
            State = new SessionStateOptions
            {
                Memory = new MemoryConfiguration
                {
                    Enabled = true
                }
            }
        };

        var clone = original.Clone();

        Assert.Equal(original.Plugins.Directories, clone.Plugins.Directories);
        Assert.Same(original.Tools.LargeOutput, clone.Tools.LargeOutput);
        Assert.Same(original.State.Memory, clone.State.Memory);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesContinuePendingWorkDefault()
    {
        var original = new ResumeSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.ContinuePendingWork);
    }

    [Fact]
    public void SessionConfig_Clone_PreservesEnableSessionTelemetryDefault()
    {
        var original = new CreateSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.GitHub.TelemetryEnabled);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesEnableSessionTelemetryDefault()
    {
        var original = new ResumeSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.GitHub.TelemetryEnabled);
    }

    [Fact]
    public void SessionConfig_Clone_PreservesEnableExperimentalModeDefault()
    {
        var original = new CreateSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.Experiments.EnableExperimentalMode);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesEnableExperimentalModeDefault()
    {
        var original = new ResumeSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.Experiments.EnableExperimentalMode);
    }

    [Fact]
    public void SessionConfig_Clone_CopiesEnableOnDemandInstructionDiscovery()
    {
        var original = new CreateSessionOptions
        {
            Instructions = new CreateSessionInstructionOptions { OnDemandDiscovery = false }
        };

        var clone = original.Clone();

        Assert.False(clone.Instructions.OnDemandDiscovery);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesEnableOnDemandInstructionDiscovery()
    {
        var original = new ResumeSessionOptions
        {
            Instructions = new SessionInstructionOptions
            {
                OnDemandDiscovery = true
            }
        };

        var clone = original.Clone();

        Assert.True(clone.Instructions.OnDemandDiscovery);
    }

    [Fact]
    public void SessionConfig_Clone_PreservesEnableOnDemandInstructionDiscoveryDefault()
    {
        var original = new CreateSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.Instructions.OnDemandDiscovery);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_PreservesEnableOnDemandInstructionDiscoveryDefault()
    {
        var original = new ResumeSessionOptions();

        var clone = original.Clone();

        Assert.Null(clone.Instructions.OnDemandDiscovery);
    }

    [Fact]
    public void SessionConfig_Clone_CopiesMcpOAuthTokenStorage()
    {
        var original = new CreateSessionOptions
        {
            Mcp = new SessionMcpOptions
            {
                OauthTokenStorage = McpOAuthTokenStorageMode.Persistent
            }
        };

        var clone = original.Clone();

        Assert.Equal(McpOAuthTokenStorageMode.Persistent, clone.Mcp.OauthTokenStorage);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesMcpOAuthTokenStorage()
    {
        var original = new ResumeSessionOptions
        {
            Mcp = new SessionMcpOptions
            {
                OauthTokenStorage = McpOAuthTokenStorageMode.Persistent
            }
        };

        var clone = original.Clone();

        Assert.Equal(McpOAuthTokenStorageMode.Persistent, clone.Mcp.OauthTokenStorage);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesSandboxConfig()
    {
        var original = new ResumeSessionOptions
        {
            Permissions = new SessionPermissionOptions
            {
                Sandbox = new SandboxConfig
                {
                    Enabled = false,
                    AddCurrentWorkingDirectory = false
                }
            }
        };

        var clone = original.Clone();

        Assert.Same(original.Permissions.Sandbox, clone.Permissions.Sandbox);
        Assert.Null(new ResumeSessionOptions().Clone().Permissions.Sandbox);
    }

    [Fact]
    public void SessionConfig_Clone_CopiesCapiOptions()
    {
        var original = new CreateSessionOptions
        {
            GitHub = new SessionGitHubOptions
            {
                Capi = new CapiSessionOptions
                {
                    EnableWebSocketResponses = false
                }
            }
        };

        var clone = original.Clone();

        Assert.Same(original.GitHub.Capi, clone.GitHub.Capi);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesCapiOptions()
    {
        var original = new ResumeSessionOptions
        {
            GitHub = new SessionGitHubOptions
            {
                Capi = new CapiSessionOptions
                {
                    EnableWebSocketResponses = false
                }
            }
        };

        var clone = original.Clone();

        Assert.Same(original.GitHub.Capi, clone.GitHub.Capi);
    }
}
