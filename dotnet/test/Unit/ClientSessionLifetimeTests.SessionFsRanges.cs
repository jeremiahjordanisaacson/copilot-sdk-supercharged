// Copyright (c) Microsoft Corporation. All rights reserved.

#if NET
using System.Text;
using System.Text.Json;
using Microsoft.Copilot.Rpc;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task SessionFs_Returns_Standard_Base64_On_The_Wire(bool range)
    {
        byte[] bytes = [0, 255, 254, 1];
        byte[] file = [9, .. bytes, 10];
        SessionFsProvider provider = range
            ? new RangeOnlySessionFsProvider((_, offset, length, _) => Task.FromResult(new SessionFsByteRange
            {
                Content = file.AsMemory((int)Math.Min(offset, file.Length), (int)(length ?? 0)),
                ByteLength = file.Length,
            }))
            : new BinarySessionFsProvider(_ => Task.FromResult(bytes));

        await using var server = await StartSessionFsServerAsync();
        await using var client = CreateSessionFsClient(server);
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions
        {
            State = new SessionStateOptions { CreateSessionFsProvider = _ => provider },
        });

        var result = await ReadSessionFsAsync(server, session.SessionId, range, offset: 1, length: 4);
        var content = result.GetProperty("content");
        Assert.Equal("AP/+AQ==", content.GetString());
        Assert.Equal(bytes, content.GetBytesFromBase64());
        Assert.False(result.TryGetProperty("error", out _));
        if (range)
        {
            Assert.Equal(file.Length, result.GetProperty("byteLength").GetInt64());
            var eof = await ReadSessionFsAsync(server, session.SessionId, range, offset: 100, length: 0);
            Assert.Equal(string.Empty, eof.GetProperty("content").GetString());
            Assert.Equal(file.Length, eof.GetProperty("byteLength").GetInt64());
            var invalid = await ReadSessionFsAsync(server, session.SessionId, range, offset: -1, length: 4);
            Assert.Equal(string.Empty, invalid.GetProperty("content").GetString());
            Assert.Equal(0L, invalid.GetProperty("byteLength").GetInt64());
            Assert.Contains("safe integers", invalid.GetProperty("error").GetProperty("message").GetString());
        }
        else
        {
            Assert.False(result.TryGetProperty("byteLength", out _));
        }
    }

    [Fact]
    public async Task SessionFs_Leaves_Range_Result_Validation_To_The_Runtime()
    {
        var provider = new RangeOnlySessionFsProvider((_, _, _, _) =>
            Task.FromResult(new SessionFsByteRange { Content = new byte[] { 0xff }, ByteLength = 5 }));
        await using var server = await StartSessionFsServerAsync();
        await using var client = CreateSessionFsClient(server);
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions
        {
            State = new SessionStateOptions { CreateSessionFsProvider = _ => provider },
        });

        var result = await ReadSessionFsAsync(server, session.SessionId, range: true, offset: 1, length: 2);
        Assert.False(result.TryGetProperty("error", out _));
        Assert.Equal("/w==", result.GetProperty("content").GetString());
        Assert.Equal(5L, result.GetProperty("byteLength").GetInt64());
    }

    [Fact]
    public async Task SessionFs_Preserves_Reimplemented_Handler_Interfaces_On_The_Wire()
    {
        await using var server = await StartSessionFsServerAsync();
        await using var client = CreateSessionFsClient(server);
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions
        {
            State = new SessionStateOptions { CreateSessionFsProvider = _ => new CustomByteReadHandler() },
        });

        var binary = await ReadSessionFsAsync(server, session.SessionId, range: false);
        Assert.Equal("AA==", binary.GetProperty("content").GetString());
        var range = await ReadSessionFsAsync(server, session.SessionId, range: true, offset: 0, length: 1);
        Assert.Equal("/w==", range.GetProperty("content").GetString());
        Assert.Equal(17L, range.GetProperty("byteLength").GetInt64());
    }

    private static async Task<FakeCopilotServer> StartSessionFsServerAsync()
    {
        var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = request => request.Method == "sessionFs.setProvider"
            ? new Dictionary<string, object?> { ["success"] = true }
            : throw new InvalidOperationException($"Unexpected RPC method '{request.Method}'.");
        return server;
    }

    private static CopilotClient CreateSessionFsClient(FakeCopilotServer server) => new(new CopilotClientOptions
    {
        Mode = CopilotClientMode.CodingAgent,
        Runtime = RuntimeConnection.ForTcp(server.Host, server.Port),
        SessionFs = new SessionFsConfig
        {
            InitialWorkingDirectory = "/",
            SessionStatePath = "/session-state",
            Conventions = SessionFsSetProviderConventions.Posix,
        },
    });

    private static Task<JsonElement> ReadSessionFsAsync(
        FakeCopilotServer server, string sessionId, bool range, long offset = 0, long? length = null)
    {
        var parameters = new Dictionary<string, object?> { ["sessionId"] = sessionId, ["path"] = "/file" };
        if (range)
        {
            parameters["offset"] = offset;
            parameters["length"] = length;
        }
        return server.SendRequestAsync(range ? "sessionFs.readFileRange" : "sessionFs.readFileBytes", parameters)
            .WaitAsync(TimeSpan.FromSeconds(5));
    }

    private sealed class CustomByteReadHandler : TextOnlySessionFsProvider, ISessionFsHandler, ISessionFsReadFileRangeHandler
    {
        Task<SessionFsReadFileBytesResult> ISessionFsHandler.ReadFileBytesAsync(SessionFsReadFileBytesRequest request, CancellationToken cancellationToken) =>
            Task.FromResult(new SessionFsReadFileBytesResult { Content = "AA==" });

        Task<SessionFsReadFileRangeResult> ISessionFsReadFileRangeHandler.ReadFileRangeAsync(SessionFsReadFileRangeRequest request, CancellationToken cancellationToken) =>
            Task.FromResult(new SessionFsReadFileRangeResult { Content = "/w==", ByteLength = 17 });
    }

    [Theory]
    [InlineData(2L, 2L)]
    [InlineData(4L, null)]
    [InlineData(3L, 0L)]
    [InlineData(7L, 20L)]
    [InlineData(9L, null)]
    [InlineData(100L, 2L)]
    public async Task RangeOnlySessionFsProvider_PreservesBytePositionsAndEof(long offset, long? length)
    {
        var data = Encoding.UTF8.GetBytes("a☃🚀z");
        var available = Math.Max(0, data.Length - offset);
        var expected = data.AsMemory((int)Math.Min(offset, data.Length), (int)Math.Min(length ?? available, available));
        var calls = 0;
        var provider = new RangeOnlySessionFsProvider((path, start, count, _) =>
        {
            Assert.Equal("/file", path);
            Assert.Equal(offset, start);
            Assert.Equal(length, count);
            calls++;
            return Task.FromResult(new SessionFsByteRange { Content = expected, ByteLength = data.Length });
        });
        Assert.False((object)provider is ISessionFsBinaryProvider);
        var result = await ((ISessionFsReadFileRangeHandler)provider).ReadFileRangeAsync(
            new SessionFsReadFileRangeRequest { SessionId = "session", Path = "/file", Offset = offset, Length = length });
        Assert.Null(result.Error);
        Assert.Equal(data.Length, result.ByteLength);
        Assert.Equal(expected.ToArray(), Convert.FromBase64String(result.Content));
        Assert.Equal(1, calls);
    }

    [Theory]
    [InlineData(-1L)]
    [InlineData(9_007_199_254_740_992L)]
    public async Task RangeSessionFsProvider_RejectsInvalidRequestsBeforeInvokingProvider(long position)
    {
        var provider = new RangeOnlySessionFsProvider((_, _, _, _) => throw new InvalidOperationException("must not invoke"));
        var handler = (ISessionFsReadFileRangeHandler)provider;
        foreach (var request in new[]
        {
            new SessionFsReadFileRangeRequest { SessionId = "session", Path = "/file", Offset = position },
            new SessionFsReadFileRangeRequest { SessionId = "session", Path = "/file", Offset = 0, Length = position },
        })
        {
            var result = await handler.ReadFileRangeAsync(request);
            Assert.NotNull(result.Error);
            Assert.Contains("safe integers", result.Error.Message);
        }
    }

    [Fact]
    public async Task RangeSessionFsProvider_RejectsMalformedResponsesAndPreservesCancellation()
    {
        var provider = new RangeOnlySessionFsProvider((_, _, _, _) =>
            Task.FromResult(new SessionFsByteRange { Content = new byte[1], ByteLength = 5 }));
        var request = new SessionFsReadFileRangeRequest { SessionId = "session", Path = "/file", Offset = 1, Length = 2 };
        var result = await ((ISessionFsReadFileRangeHandler)provider).ReadFileRangeAsync(request);
        Assert.NotNull(result.Error);
        Assert.Contains("incorrect byte range length", result.Error.Message);
        var legacy = new TextOnlySessionFsProvider();
        result = await ((ISessionFsReadFileRangeHandler)legacy).ReadFileRangeAsync(request);
        Assert.NotNull(result.Error);
        Assert.Contains("not supported", result.Error.Message);
        provider = new RangeOnlySessionFsProvider((_, _, _, token) => Task.FromCanceled<SessionFsByteRange>(token));
        await Assert.ThrowsAnyAsync<OperationCanceledException>(() =>
            ((ISessionFsReadFileRangeHandler)provider).ReadFileRangeAsync(request, new CancellationToken(true)));
    }

    private sealed class RangeOnlySessionFsProvider(
        Func<string, long, long?, CancellationToken, Task<SessionFsByteRange>> read)
        : TextOnlySessionFsProvider, ISessionFsReadFileRangeProvider
    {
        public Task<SessionFsByteRange> ReadFileRangeAsync(string path, long offset, long? length, CancellationToken cancellationToken) =>
            read(path, offset, length, cancellationToken);
    }
}
#endif
