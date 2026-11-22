/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Copilot.Rpc;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    private static ToolResultObject CreateToolResultObject(string shape) => shape switch
    {
        "text only" => new ToolResultObject { TextResultForLlm = "redacted" },
        "image without description" => new ToolResultObject
        {
            TextResultForLlm = "redacted",
            BinaryResultsForLlm = [new ToolBinaryResult { Data = "AAAA", MimeType = "image/png", Type = ToolBinaryResultType.Image }],
        },
        "every member set" => new ToolResultObject
        {
            TextResultForLlm = "redacted",
            BinaryResultsForLlm =
            [
                new ToolBinaryResult { Data = "AAAA", MimeType = "image/png", Type = ToolBinaryResultType.Image, Description = "chart" },
            ],
            ResultType = "failure",
            Error = "failed",
            SessionLog = "log",
            ToolTelemetry = new Dictionary<string, object> { ["source"] = "test" },
            SkipLargeOutputProcessing = true,
            ToolReferences = ["view"],
        },
        _ => throw new ArgumentOutOfRangeException(nameof(shape)),
    };

    private static string ExpectedToolResultJson(string shape) => shape switch
    {
        "text only" => """{"textResultForLlm":"redacted","resultType":"success"}""",
        "image without description" =>
            """{"textResultForLlm":"redacted","binaryResultsForLlm":[{"data":"AAAA","mimeType":"image/png","type":"image"}],"resultType":"success"}""",
        "every member set" =>
            """{"textResultForLlm":"redacted","binaryResultsForLlm":[{"data":"AAAA","mimeType":"image/png","type":"image","description":"chart"}],"resultType":"failure","error":"failed","sessionLog":"log","toolTelemetry":{"source":"test"},"skipLargeOutputProcessing":true,"toolReferences":["view"]}""",
        _ => throw new ArgumentOutOfRangeException(nameof(shape)),
    };

    private static async Task<JsonElement> SendToolResultObjectAsync(string path, ToolResultObject value)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        switch (path)
        {
            case "postToolUse modifiedResult":
                {
                    await using var session = await client.CreateSessionAsync(new CreateSessionOptions
                    {
                        Hooks = new SessionHookOptions
                        {
                            Handlers = new SessionHooks
                            {
                                OnPostToolUse = (_, _) => Task.FromResult<PostToolUseHookOutput?>(new PostToolUseHookOutput { ModifiedResult = value }),
                            },
                        },
                    });
                    var result = await server.SendRequestAsync("hooks.invoke", new Dictionary<string, object?>
                    {
                        ["sessionId"] = session.SessionId,
                        ["hookType"] = "postToolUse",
                        ["input"] = new Dictionary<string, object?>
                        {
                            ["sessionId"] = session.SessionId,
                            ["timestamp"] = 1_730_000_000_000L,
                            ["cwd"] = "workdir",
                            ["toolName"] = "view",
                            ["toolArgs"] = new Dictionary<string, object?>(),
                            ["toolResult"] = new Dictionary<string, object?> { ["resultType"] = "success", ["textResultForLlm"] = "original" },
                        },
                    }).WaitAsync(TimeSpan.FromSeconds(5));
                    return result.GetProperty("output").GetProperty("modifiedResult").Clone();
                }
            case "tool handler result":
                {
                    await using var session = await client.CreateSessionAsync(new CreateSessionOptions { Tools = new SessionToolOptions { Definitions = [new ToolResultFunction(value)] } });
                    await server.SendSessionEventAsync(session.SessionId, "external_tool.requested", new()
                    {
                        ["requestId"] = "result-request",
                        ["sessionId"] = session.SessionId,
                        ["toolCallId"] = "result-call",
                        ["toolName"] = "record_note",
                        ["arguments"] = new Dictionary<string, object?>(),
                    });
                    var request = await WaitForRequestAsync(server, "session.tools.handlePendingToolCall");
                    return request.Params.GetProperty("result").Clone();
                }
            case "canvas action result":
                {
                    server.ResponseFactory = _ => new Dictionary<string, object?>();
                    await using var session = await client.CreateSessionAsync(new CreateSessionOptions { Canvases = new SessionCanvasOptions { Handler = new ActionResultCanvasHandler(value) } });
                    var result = await server.SendRequestAsync("canvas.action.invoke", new Dictionary<string, object?>
                    {
                        ["sessionId"] = session.SessionId,
                        ["extensionId"] = "test-extension",
                        ["canvasId"] = "test-canvas",
                        ["instanceId"] = "test-instance",
                        ["actionName"] = "capture",
                    }).WaitAsync(TimeSpan.FromSeconds(5));
                    return result.Clone();
                }
            default:
                throw new ArgumentOutOfRangeException(nameof(path));
        }
    }

    [Theory]
    [InlineData("postToolUse modifiedResult", "text only")]
    [InlineData("postToolUse modifiedResult", "image without description")]
    [InlineData("postToolUse modifiedResult", "every member set")]
    [InlineData("tool handler result", "text only")]
    [InlineData("tool handler result", "image without description")]
    [InlineData("tool handler result", "every member set")]
    [InlineData("canvas action result", "text only")]
    [InlineData("canvas action result", "image without description")]
    [InlineData("canvas action result", "every member set")]
    public async Task Tool_Result_Object_Sends_Only_The_Members_The_Caller_Set(string path, string shape)
    {
        var sent = await SendToolResultObjectAsync(path, CreateToolResultObject(shape));

        Assert.True(
            JsonNode.DeepEquals(JsonNode.Parse(ExpectedToolResultJson(shape)), JsonNode.Parse(sent.GetRawText())),
            $"sent {sent.GetRawText()}");
    }

    [Theory]
    [InlineData("tool handler result", null)]
    [InlineData("tool handler result", false)]
    [InlineData("tool handler result", true)]
    [InlineData("postToolUse modifiedResult", null)]
    [InlineData("postToolUse modifiedResult", false)]
    [InlineData("postToolUse modifiedResult", true)]
    [InlineData("canvas action result", null)]
    [InlineData("canvas action result", false)]
    [InlineData("canvas action result", true)]
    public async Task Tool_Result_Object_Preserves_Large_Output_Policy(string path, bool? skip)
    {
        var sent = await SendToolResultObjectAsync(path, new ToolResultObject
        {
            TextResultForLlm = "large output",
            SkipLargeOutputProcessing = skip,
        });

        if (skip is bool value)
        {
            Assert.Equal(value, sent.GetProperty("skipLargeOutputProcessing").GetBoolean());
        }
        else
        {
            Assert.False(sent.TryGetProperty("skipLargeOutputProcessing", out _));
        }
    }

    private sealed class ActionResultCanvasHandler(object? result) : CanvasHandlerBase
    {
        public override Task<CanvasProviderOpenResult> OnOpenAsync(
            CanvasProviderOpenRequest context, CancellationToken cancellationToken) =>
            Task.FromResult(new CanvasProviderOpenResult { Status = "ready" });

        public override Task<object?> OnActionAsync(
            CanvasProviderInvokeActionRequest context, CancellationToken cancellationToken) =>
            Task.FromResult(result);
    }
}
#endif
