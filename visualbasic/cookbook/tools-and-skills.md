# Tools and Skills

Recipes for registering custom tools and enabling skills.

## Defining a Tool

```vb
Imports System.Text.Json

Dim readFileTool As New DefineTool With {
    .Name = "read_file",
    .Description = "Read the contents of a file",
    .ParametersSchema = New Dictionary(Of String, Object) From {
        {"type", "object"},
        {"properties", New Dictionary(Of String, Object) From {
            {"path", New Dictionary(Of String, Object) From {
                {"type", "string"},
                {"description", "File path to read"}
            }}
        }},
        {"required", New String() {"path"}}
    },
    .Handler = Async Function(input)
                   Dim el = DirectCast(input, JsonElement)
                   Dim path = el.GetProperty("path").GetString()
                   If File.Exists(path) Then
                       Return File.ReadAllText(path)
                   Else
                       Return $"File not found: {path}"
                   End If
               End Function
}
```

## Registering Tools at Session Creation

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Model = "gpt-4",
    .OnPermissionRequest = PermissionHandlers.ApproveAll(),
    .Tools = New List(Of DefineTool) From {readFileTool, writeFileTool}
})
```

## Registering Tools After Creation

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})

session.RegisterTool(readFileTool)
session.RegisterTool(writeFileTool)
```

## Tool with Multiple Parameters

```vb
Dim searchTool As New DefineTool With {
    .Name = "search_database",
    .Description = "Search a database table",
    .ParametersSchema = New Dictionary(Of String, Object) From {
        {"type", "object"},
        {"properties", New Dictionary(Of String, Object) From {
            {"table", New Dictionary(Of String, Object) From {
                {"type", "string"},
                {"description", "Table name"}
            }},
            {"query", New Dictionary(Of String, Object) From {
                {"type", "string"},
                {"description", "Search query"}
            }},
            {"limit", New Dictionary(Of String, Object) From {
                {"type", "integer"},
                {"description", "Max results to return"}
            }}
        }},
        {"required", New String() {"table", "query"}}
    },
    .Handler = Async Function(input)
                   Dim el = DirectCast(input, JsonElement)
                   Dim table = el.GetProperty("table").GetString()
                   Dim query = el.GetProperty("query").GetString()
                   Dim limit = 10
                   Dim limitProp As JsonElement = Nothing
                   If el.TryGetProperty("limit", limitProp) Then
                       limit = limitProp.GetInt32()
                   End If
                   Return $"Found {limit} results in {table} for '{query}'"
               End Function
}
```

## Enabling Skills

Skills are named capabilities that expand what the assistant can do:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Model = "gpt-4",
    .OnPermissionRequest = PermissionHandlers.ApproveAll(),
    .Skills = New List(Of String) From {
        "code-review",
        "testing",
        "documentation"
    }
})
```

## Tool Call Events

Monitor when the assistant calls tools:

```vb
session.On(Of ToolCallEvent)(Sub(evt)
    Console.WriteLine($"Tool called: {evt.Data?.ToolName}")
    Console.WriteLine($"  Call ID: {evt.Data?.ToolCallId}")
End Sub)
```

## Tool Error Handling

Tool handlers can return error information:

```vb
Dim safeTool As New DefineTool With {
    .Name = "safe_operation",
    .Description = "An operation that might fail",
    .Handler = Async Function(input)
                   Try
                       ' ... perform operation ...
                       Return "success: operation completed"
                   Catch ex As Exception
                       Return $"error: {ex.Message}"
                   End Try
               End Function
}
```
