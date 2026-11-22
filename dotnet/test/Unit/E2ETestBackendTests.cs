/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Test.Harness;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public class E2ETestBackendTests
{
    [Theory]
    [InlineData(null, "capi")]
    [InlineData("", "capi")]
    [InlineData("capi", "capi")]
    [InlineData("ANTHROPIC-MESSAGES", "anthropic-messages")]
    [InlineData("openai-responses", "openai-responses")]
    [InlineData("openai-completions", "openai-completions")]
    public void ParsesBackend(string? value, string expected)
        => Assert.Equal(expected, E2ETestBackendConfiguration.Parse(value).ToWireName());

    [Fact]
    public void RejectsUnknownBackend()
        => Assert.Throws<ArgumentOutOfRangeException>(
            () => E2ETestBackendConfiguration.Parse("unknown"));

    [Theory]
    [InlineData("anthropic-messages", "anthropic", null, "claude-sonnet-5")]
    [InlineData("openai-responses", "openai", "responses", "gpt-4.1")]
    [InlineData("openai-completions", "openai", "completions", "gpt-4.1")]
    public void AppliesProvider(
        string backendValue,
        string expectedType,
        string? expectedWireApi,
        string expectedModel)
    {
        var backend = E2ETestBackendConfiguration.Parse(backendValue);
        var config = new CreateSessionOptions();
        backend.ApplyProvider(config, "http://localhost:1234");

        Assert.Equal(expectedModel, config.Model);
        Assert.Equal("http://localhost:1234", config.Inference.Providers![0].BaseUrl);
        Assert.Equal(expectedType, config.Inference.Providers![0].Type);
        Assert.Equal(expectedWireApi, config.Inference.Providers![0].WireApi);
        Assert.Equal(expectedModel, config.Inference.Models![0].ModelId);
        Assert.Equal(expectedModel, config.Inference.Models![0].WireModel);
        Assert.False(string.IsNullOrEmpty(config.Inference.Providers![0].BearerToken));
    }

    [Fact]
    public void PreservesExplicitModel()
    {
        var config = new CreateSessionOptions { Model = "test-model" };
        E2ETestBackend.OpenAIResponses.ApplyProvider(config, "http://localhost:1234");

        Assert.Equal("test-model", config.Model);
        Assert.Equal("test-model", config.Inference.Models![0].ModelId);
        Assert.Equal("test-model", config.Inference.Models![0].WireModel);
    }

    [Fact]
    public void PreservesExplicitProvider()
    {
        var provider = new ProviderConfig
        {
            Type = "custom",
            ModelId = "provider-model",
        };
        var config = new CreateSessionOptions
        {
            GitHub = new SessionGitHubOptions { Capi = new CapiSessionOptions { Enabled = false } },
            Model = "session-model",
            Inference = ByokTestOptions.Inference(provider, new SessionInferenceOptions { })
        };

        E2ETestBackend.OpenAIResponses.ApplyProvider(config, "http://localhost:1234");

        Assert.Equal("session-model", config.Model);
        Assert.Equal("custom", config.Inference.Providers![0].Type);
        Assert.Equal("provider-model", config.Inference.Models![0].ModelId);
    }
}
