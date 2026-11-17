/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System;
using System.IO;
using System.Reflection;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Copilot;
using Microsoft.Copilot.Rpc;
using Microsoft.Extensions.Logging;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public class CanvasTests
{
    private static JsonSerializerOptions GetSerializerOptions()
    {
        var prop = typeof(CopilotClient).GetProperty(
            "SerializerOptionsForMessageFormatter",
            BindingFlags.NonPublic | BindingFlags.Static);
        var options = (JsonSerializerOptions?)prop?.GetValue(null);
        Assert.NotNull(options);
        return options!;
    }

    private static CopilotSession CreateSession()
    {
        var options = GetSerializerOptions();
        var rpcType = typeof(CopilotClient).Assembly.GetType("Microsoft.Copilot.JsonRpc");
        Assert.NotNull(rpcType);

        var inputStream = new MemoryStream();
        var outputStream = new MemoryStream();
        object? rpc;
        try
        {
            rpc = Activator.CreateInstance(
                rpcType!,
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
                binder: null,
                args: [inputStream, outputStream, options, null],
                culture: null);
            Assert.NotNull(rpc);
        }
        catch
        {
            inputStream.Dispose();
            outputStream.Dispose();
            throw;
        }

        var logger = new TestLogger();
        var ctor = typeof(CopilotSession).GetConstructor(
            BindingFlags.Instance | BindingFlags.NonPublic,
            binder: null,
            types: [typeof(string), rpcType!, typeof(ILogger), typeof(CopilotClient), typeof(string)],
            modifiers: null);
        Assert.NotNull(ctor);
        try
        {
            return (CopilotSession)ctor!.Invoke(["session-1", rpc, logger, new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent }), null]);
        }
        catch
        {
            inputStream.Dispose();
            outputStream.Dispose();
            throw;
        }
    }

    private sealed class TestLogger : ILogger
    {
        public IDisposable? BeginScope<TState>(TState state) where TState : notnull => null;

        public bool IsEnabled(LogLevel logLevel) => false;

        public void Log<TState>(
            LogLevel logLevel,
            EventId eventId,
            TState state,
            Exception? exception,
            Func<TState, Exception?, string> formatter)
        {
        }
    }

    private static void DispatchEvent(CopilotSession session, SessionEvent evt)
    {
        var method = typeof(CopilotSession).GetMethod(
            "DispatchEvent",
            BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(method);
        method!.Invoke(session, [evt]);
    }

    [Fact]
    public void CanvasDeclaration_Serializes_CamelCase_SkippingNulls()
    {
        var options = GetSerializerOptions();
        var decl = new CanvasDeclaration
        {
            Id = "report",
            DisplayName = "Quarterly Report",
            Description = "Renders the latest report",
        };

        var json = JsonSerializer.Serialize(decl, options);
        using var doc = JsonDocument.Parse(json);
        var root = doc.RootElement;

        Assert.Equal("report", root.GetProperty("id").GetString());
        Assert.Equal("Quarterly Report", root.GetProperty("displayName").GetString());
        Assert.Equal("Renders the latest report", root.GetProperty("description").GetString());
        Assert.False(root.TryGetProperty("inputSchema", out _));
        Assert.False(root.TryGetProperty("actions", out _));
    }

    [Fact]
    public void CanvasProviderOpenResult_Roundtrips_WithCamelCaseFields()
    {
        var options = GetSerializerOptions();
        var response = new CanvasProviderOpenResult
        {
            Url = "https://example.com/c/1",
            Title = "Demo",
            Status = "ready"
        };

        var json = JsonSerializer.Serialize(response, options);
        var parsed = JsonSerializer.Deserialize<CanvasProviderOpenResult>(json, options);

        Assert.NotNull(parsed);
        Assert.Equal("https://example.com/c/1", parsed!.Url);
        Assert.Equal("Demo", parsed.Title);
        Assert.Equal("ready", parsed.Status);
    }

    [Fact]
    public void SessionCanvasOpenedEvent_UpdatesOpenCanvasSnapshots()
    {
        var session = CreateSession();

        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "", ExtensionId = "project:counter", InstanceId = "missing-canvas-id", } });
        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "counter", ExtensionId = "project:counter", ExtensionName = "Counter Provider", InstanceId = "counter-1", Title = "Counter", Icon = "beaker", Status = "ready", Url = "https://example.test/counter", Input = JsonDocument.Parse("""{"seed":1}""").RootElement.Clone(), } });
        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "logs", ExtensionId = "project:logs", InstanceId = "logs-1", Title = "Logs", } });

        Assert.Collection(
            session.OpenCanvases,
            canvas => Assert.Equal("counter-1", canvas.InstanceId),
            canvas => Assert.Equal("logs-1", canvas.InstanceId));

        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "counter", ExtensionId = "project:counter", ExtensionName = "Counter Provider", InstanceId = "counter-1", Title = "Counter Updated", Icon = "beaker-filled", Status = "reconnected", Url = "https://example.test/counter-updated", Input = JsonDocument.Parse("""{"seed":2}""").RootElement.Clone(), } });

        Assert.Collection(
            session.OpenCanvases,
            canvas =>
            {
                Assert.Equal("counter-1", canvas.InstanceId);
                Assert.Equal("Counter Updated", canvas.Title);
                Assert.Equal("beaker-filled", canvas.Icon);
                Assert.Equal("reconnected", canvas.Status);
                Assert.Equal("https://example.test/counter-updated", canvas.Url);
                Assert.Equal(2, canvas.Input!.Value.GetProperty("seed").GetInt32());
            },
            canvas => Assert.Equal("logs-1", canvas.InstanceId));
    }

    [Fact]
    public void SessionCanvasClosedEvent_RemovesOpenCanvasSnapshots()
    {
        var session = CreateSession();

        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "counter", ExtensionId = "project:counter", InstanceId = "counter-1", Title = "Counter", } });
        DispatchEvent(session, new SessionCanvasOpenedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasOpenedData { CanvasId = "logs", ExtensionId = "project:logs", InstanceId = "logs-1", Title = "Logs", } });

        Assert.Collection(
            session.OpenCanvases,
            canvas => Assert.Equal("counter-1", canvas.InstanceId),
            canvas => Assert.Equal("logs-1", canvas.InstanceId));

        // Closing one instance removes it; the other remains.
        DispatchEvent(session, new SessionCanvasClosedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasClosedData { CanvasId = "counter", ExtensionId = "project:counter", InstanceId = "counter-1", } });

        Assert.Collection(
            session.OpenCanvases,
            canvas => Assert.Equal("logs-1", canvas.InstanceId));

        // Closing an absent instance is a no-op (idempotent).
        DispatchEvent(session, new SessionCanvasClosedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasClosedData { CanvasId = "counter", ExtensionId = "project:counter", InstanceId = "counter-1", } });

        // A closed event with an empty instance id leaves the snapshot intact.
        DispatchEvent(session, new SessionCanvasClosedEvent
        { Id = Guid.NewGuid(), Timestamp = DateTimeOffset.UtcNow, Data = new SessionCanvasClosedData { CanvasId = "logs", ExtensionId = "project:logs", InstanceId = "", } });

        Assert.Collection(
            session.OpenCanvases,
            canvas => Assert.Equal("logs-1", canvas.InstanceId));
    }

    [Fact]
    public void ExtensionInfo_Serializes_SourceAndName()
    {
        var options = GetSerializerOptions();
        var info = new ExtensionInfo { Source = "github-app", Name = "demo" };
        var json = JsonSerializer.Serialize(info, options);
        using var doc = JsonDocument.Parse(json);
        Assert.Equal("github-app", doc.RootElement.GetProperty("source").GetString());
        Assert.Equal("demo", doc.RootElement.GetProperty("name").GetString());
    }

    [Fact]
    public void CanvasProviderIdentity_Serializes_IdAndName()
    {
        var options = GetSerializerOptions();
        var identity = new CanvasProviderIdentity { Id = "app:builtin:window-1", Name = "Built-in" };
        var json = JsonSerializer.Serialize(identity, options);
        using var doc = JsonDocument.Parse(json);
        Assert.Equal("app:builtin:window-1", doc.RootElement.GetProperty("id").GetString());
        Assert.Equal("Built-in", doc.RootElement.GetProperty("name").GetString());
    }

    [Fact]
    public void CanvasProviderIdentity_OmitsNullName()
    {
        var options = GetSerializerOptions();
        var identity = new CanvasProviderIdentity { Id = "app:builtin:window-1" };
        var json = JsonSerializer.Serialize(identity, options);
        using var doc = JsonDocument.Parse(json);
        Assert.Equal("app:builtin:window-1", doc.RootElement.GetProperty("id").GetString());
        Assert.False(doc.RootElement.TryGetProperty("name", out _));
    }

    [Fact]
    public async Task CanvasHandlerBase_DefaultOnClose_Completes()
    {
        var handler = new TestHandler();
        await handler.OnCloseAsync(new CanvasProviderCloseRequest(), CancellationToken.None);
    }

    [Fact]
    public async Task CanvasHandlerBase_DefaultOnAction_ThrowsNoHandlerCanvasException()
    {
        var handler = new TestHandler();
        var ex = await Assert.ThrowsAsync<CanvasException>(
            () => handler.OnActionAsync(new CanvasProviderInvokeActionRequest(), CancellationToken.None));
        Assert.Equal("canvas_action_no_handler", ex.Code);
    }

    [Fact]
    public void CanvasException_NoHandler_HasExpectedCode()
    {
        var err = CanvasException.NoHandler();
        Assert.Equal("canvas_action_no_handler", err.Code);
        Assert.False(string.IsNullOrEmpty(err.Message));
    }

    [Fact]
    public void SessionConfig_Clone_CopiesCanvasFields()
    {
        var handler = new TestHandler();
        var declaration = new CanvasDeclaration { Id = "c1", DisplayName = "C", Description = "d" };
        var config = new CreateSessionOptions
        {
            Canvases = new SessionCanvasOptions
            {
                Definitions = new[]
        {
            declaration
        },
                RequestRenderer = true,
                Provider = new CanvasProviderIdentity
                {
                    Id = "app:builtin:window-1",
                    Name = "Built-in"
                },
                Handler = handler
            },
            Extensions = new SessionExtensionOptions
            {
                Enabled = true,
                Identity = new ExtensionInfo
                {
                    Source = "github-app",
                    Name = "demo"
                }
            }
        };

        var clone = config.Clone();

        Assert.NotNull(clone.Canvases.Definitions);
        Assert.Single(clone.Canvases.Definitions!);
        Assert.Equal("c1", clone.Canvases.Definitions![0].Id);
        Assert.True(clone.Canvases.RequestRenderer);
        Assert.True(clone.Extensions.Enabled);
        Assert.NotNull(clone.Extensions.Identity);
        Assert.Equal("github-app", clone.Extensions.Identity!.Source);
        Assert.NotNull(clone.Canvases.Provider);
        Assert.Equal("app:builtin:window-1", clone.Canvases.Provider!.Id);
        Assert.Same(handler, clone.Canvases.Handler);

        // Mutating the clone's list does not affect the original.
        clone.Canvases.Definitions!.Add(new CanvasDeclaration { Id = "c2", DisplayName = "C2", Description = "d2" });
        Assert.Single(config.Canvases.Definitions!);
    }

    [Fact]
    public void ResumeSessionConfig_Clone_CopiesCanvasFields()
    {
        var handler = new TestHandler();
        var config = new ResumeSessionOptions
        {
            Canvases = new ResumeSessionCanvasOptions
            {
                Definitions = new[]
        {
            new CanvasDeclaration
            {
                Id = "c1",
                DisplayName = "C",
                Description = "d"
            }
        },
                RequestRenderer = true,
                Provider = new CanvasProviderIdentity
                {
                    Id = "app:builtin:window-2"
                },
                Handler = handler
            },
            Extensions = new SessionExtensionOptions
            {
                Identity = new ExtensionInfo
                {
                    Source = "s",
                    Name = "n"
                }
            }
        };

        var clone = config.Clone();

        Assert.NotNull(clone.Canvases.Definitions);
        Assert.Single(clone.Canvases.Definitions!);
        Assert.True(clone.Canvases.RequestRenderer);
        Assert.NotNull(clone.Extensions.Identity);
        Assert.NotNull(clone.Canvases.Provider);
        Assert.Equal("app:builtin:window-2", clone.Canvases.Provider!.Id);
        Assert.Same(handler, clone.Canvases.Handler);
    }

    private sealed class TestHandler : CanvasHandlerBase
    {
        public override Task<CanvasProviderOpenResult> OnOpenAsync(CanvasProviderOpenRequest context, CancellationToken cancellationToken)
            => Task.FromResult(new CanvasProviderOpenResult { Url = "https://example.com" });
    }
}
