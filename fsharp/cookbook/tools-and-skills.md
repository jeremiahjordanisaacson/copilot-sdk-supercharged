# Tools and Skills

Register tools that the Copilot assistant can invoke during a session.

## Simple Tool (No Parameters)

```fsharp
open CopilotSDK.Supercharged.FSharp

let greetTool =
    DefineTool.createSimple "greet" "Say hello to the user" (fun () -> async {
        return "Hello from the F# SDK!"
    })

session.RegisterTool(greetTool)
```

## Tool with JSON Arguments

```fsharp
open System.Text.Json

let searchTool =
    DefineTool.create
        "search_files"
        "Search for files matching a pattern"
        None
        (fun invocation -> async {
            let query =
                invocation.Arguments
                |> Option.bind (fun a ->
                    match a.TryGetProperty("query") with
                    | true, q -> Some (q.GetString())
                    | _ -> None)
                |> Option.defaultValue "*"
            let results = System.IO.Directory.GetFiles(".", query)
            return ToolResultObject.success (String.concat "\n" results) :> obj
        })

session.RegisterTool(searchTool)
```

## Typed Tool with Deserialization

```fsharp
type CalculatorArgs = { Expression: string }

let calcTool =
    DefineTool.createTyped<CalculatorArgs, ToolResultObject>
        "calculate"
        "Evaluate a math expression"
        None
        (fun args -> async {
            // In a real app, use a proper evaluator
            return ToolResultObject.success (sprintf "Result of '%s' = 42" args.Expression)
        })

session.RegisterTool(calcTool)
```

## Pipe-Friendly Builder

```fsharp
let myTool =
    DefineTool.define "lookup" "Look up a value in the database"
    |> DefineTool.withHandler (fun inv -> async {
        return ToolResultObject.success "found: 42" :> obj
    })
    |> DefineTool.build

session.RegisterTool(myTool)
```

## Registering Multiple Tools

```fsharp
let tools = [greetTool; searchTool; calcTool; myTool]
session.RegisterTools(tools)
```

## Error Handling in Tools

```fsharp
let riskyTool =
    DefineTool.create "risky_operation" "Might fail" None
        (fun _ -> async {
            try
                // Do risky work
                return ToolResultObject.success "it worked" :> obj
            with ex ->
                return ToolResultObject.failure ex.Message :> obj
        })
```
