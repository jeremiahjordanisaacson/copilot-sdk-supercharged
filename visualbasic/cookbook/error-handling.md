# Error Handling

Recipes for robust error handling with the VB.NET Copilot SDK.

## Handling Session Errors

```vb
session.On(Sub(evt)
    If TypeOf evt Is SessionErrorEvent Then
        Dim errEvt = DirectCast(evt, SessionErrorEvent)
        Console.WriteLine($"Error [{errEvt.Data?.Code}]: {errEvt.Data?.Message}")
    End If
End Sub)
```

## Timeouts with SendAndWaitAsync

```vb
Try
    Dim response = Await session.SendAndWaitAsync(
        New MessageOptions With { .Prompt = "Complex task..." },
        timeout:=TimeSpan.FromSeconds(120)
    )

    If response IsNot Nothing Then
        Console.WriteLine(response.Data?.Content)
    Else
        Console.WriteLine("No response received")
    End If
Catch ex As TimeoutException
    Console.WriteLine($"Timed out: {ex.Message}")
    Await session.AbortAsync()
Catch ex As OperationCanceledException
    Console.WriteLine("Cancelled by user")
End Try
```

## Retry Pattern

```vb
Private Async Function SendWithRetryAsync(
    session As CopilotSession,
    prompt As String,
    Optional maxRetries As Integer = 3
) As Task(Of AssistantMessageEvent)

    For attempt = 1 To maxRetries
        Try
            Return Await session.SendAndWaitAsync(
                New MessageOptions With { .Prompt = prompt },
                timeout:=TimeSpan.FromSeconds(60)
            )
        Catch ex As TimeoutException When attempt < maxRetries
            Console.WriteLine($"Attempt {attempt} timed out, retrying...")
            Await session.AbortAsync()
            Await Task.Delay(1000 * attempt) ' exponential backoff
        Catch ex As InvalidOperationException When attempt < maxRetries
            Console.WriteLine($"Attempt {attempt} failed: {ex.Message}, retrying...")
            Await Task.Delay(1000 * attempt)
        End Try
    Next

    Throw New Exception("All retry attempts exhausted")
End Function
```

## Graceful Shutdown

```vb
Private Async Function GracefulShutdownAsync(client As CopilotClient) As Task
    Try
        Await client.StopAsync()
    Catch ex As AggregateException
        For Each inner In ex.InnerExceptions
            Console.WriteLine($"Shutdown warning: {inner.Message}")
        Next
    Finally
        client.Dispose()
    End Try
End Function
```

## Handling JSON-RPC Errors

```vb
Try
    Dim response = Await client.PingAsync()
    Console.WriteLine($"Server time: {response.ServerTime}")
Catch ex As JsonRpcException
    Console.WriteLine($"RPC error: {ex.Message}")
Catch ex As InvalidOperationException
    Console.WriteLine($"Client error: {ex.Message}")
End Try
```
