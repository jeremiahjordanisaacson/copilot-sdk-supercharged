' Copyright (c) Microsoft Corporation. All rights reserved.
'
' BasicExample.vb - Demonstrates core usage of the Visual Basic Copilot SDK.
'
' Prerequisites:
'   - Copilot CLI installed and on PATH, or set COPILOT_CLI_PATH
'   - Authenticated via `gh auth login` or a GITHUB_TOKEN env var

Imports System.Threading
Imports GitHub.Copilot.SDK

Module BasicExample

    Sub Main(args As String())
        MainAsync().GetAwaiter().GetResult()
    End Sub

    Private Async Function MainAsync() As Task
        ' 1. Create a client with default options (stdio transport, auto-start)
        Using client As New CopilotClient(New CopilotClientOptions With {
            .LogLevel = "info"
        })
            ' 2. Start the CLI server
            Console.WriteLine("Starting Copilot client...")
            Await client.StartAsync()
            Console.WriteLine("Client started.")

            ' 3. Create a session with permission handling and a custom tool
            Dim weatherTool As New DefineTool With {
                .Name = "get_weather",
                .Description = "Get current weather for a city",
                .ParametersSchema = New Dictionary(Of String, Object) From {
                    {"type", "object"},
                    {"properties", New Dictionary(Of String, Object) From {
                        {"city", New Dictionary(Of String, Object) From {
                            {"type", "string"},
                            {"description", "City name"}
                        }}
                    }},
                    {"required", New String() {"city"}}
                },
                .Handler = Async Function(input)
                               Return "72F and sunny"
                           End Function
            }

            Dim session = Await client.CreateSessionAsync(New SessionConfig With {
                .Model = "gpt-4",
                .OnPermissionRequest = PermissionHandlers.ApproveAll(),
                .Tools = New List(Of DefineTool) From {weatherTool}
            })

            Console.WriteLine($"Session created: {session.SessionId}")

            ' 4. Subscribe to events
            Dim done As New TaskCompletionSource(Of Boolean)()

            Dim subscription = session.On(Sub(evt)
                Select Case True
                    Case TypeOf evt Is AssistantMessageDeltaEvent
                        ' Streaming delta
                        Dim delta = DirectCast(evt, AssistantMessageDeltaEvent)
                        Console.Write(delta.Data?.Delta)

                    Case TypeOf evt Is AssistantMessageEvent
                        ' Final complete message
                        Dim msg = DirectCast(evt, AssistantMessageEvent)
                        Console.WriteLine()
                        Console.WriteLine($"[Assistant]: {msg.Data?.Content}")

                    Case TypeOf evt Is SessionIdleEvent
                        Console.WriteLine("[Session idle]")
                        done.TrySetResult(True)

                    Case TypeOf evt Is SessionErrorEvent
                        Dim errEvt = DirectCast(evt, SessionErrorEvent)
                        Console.WriteLine($"[Error]: {errEvt.Data?.Message}")
                        done.TrySetResult(False)

                    Case TypeOf evt Is ToolCallEvent
                        Dim toolEvt = DirectCast(evt, ToolCallEvent)
                        Console.WriteLine($"[Tool call]: {toolEvt.Data?.ToolName}")
                End Select
            End Sub)

            ' 5. Send a message and wait for completion
            Console.WriteLine("Sending message...")
            Await session.SendAsync(New MessageOptions With {
                .Prompt = "What is the weather in Seattle?"
            })

            ' Wait for the session to become idle
            Await done.Task

            ' 6. Retrieve metadata
            Dim metadata = Await client.GetSessionMetadataAsync(session.SessionId)
            Console.WriteLine($"Session model: {metadata.Model}")

            ' 7. Cleanup
            subscription.Dispose()
            session.Dispose()
            Await client.StopAsync()
            Console.WriteLine("Done.")
        End Using
    End Function

End Module
