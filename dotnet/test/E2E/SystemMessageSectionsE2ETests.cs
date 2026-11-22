/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Rpc;
using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class SystemMessageSectionsE2ETests(E2ETestFixture fixture, ITestOutputHelper output) : E2ETestBase(fixture, "system_message_sections", output)
{
    [Fact]
    public async Task Should_Use_Replaced_Identity_Section_In_Response()
    {
        var session = await CreateSessionAsync(new CreateSessionOptions
        { SystemMessage = new SystemMessageConfig { Mode = SystemMessageMode.Customize, Sections = new Dictionary<SystemMessageSection, SectionOverride> { [SystemMessageSection.Identity] = new SectionOverride { Action = SectionOverrideAction.Replace, Content = "You are a helpful gardening assistant called Botanica. You only answer questions about plants and gardening." } } }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var response = await TestHelper.SendAndGetFinalAssistantMessageAsync(session, new MessageOptions { Prompt = "Who are you?" });

        Assert.NotNull(response);
        var content = response.Data.Content.ToLowerInvariant();
        Assert.True(
            content.Contains("botanica") || content.Contains("garden") || content.Contains("plant"),
            $"Expected response to reflect the replaced identity section, but got: {response.Data.Content}");
    }

    [Fact]
    public async Task Should_Use_Replaced_Preamble_Section_In_Response()
    {
        var session = await CreateSessionAsync(new CreateSessionOptions
        { SystemMessage = new SystemMessageConfig { Mode = SystemMessageMode.Customize, Sections = new Dictionary<SystemMessageSection, SectionOverride> { [SystemMessageSection.Preamble] = new SectionOverride { Action = SectionOverrideAction.Replace, Content = "You are a helpful gardening assistant called Botanica. You only answer questions about plants and gardening." } } }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var response = await TestHelper.SendAndGetFinalAssistantMessageAsync(session, new MessageOptions { Prompt = "Who are you?" });

        Assert.NotNull(response);
        var content = response.Data.Content.ToLowerInvariant();
        Assert.True(
            content.Contains("botanica") || content.Contains("garden") || content.Contains("plant"),
            $"Expected response to reflect the replaced preamble section, but got: {response.Data.Content}");
    }
}
