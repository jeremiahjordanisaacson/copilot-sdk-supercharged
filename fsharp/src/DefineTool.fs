namespace CopilotSDK.Supercharged.FSharp

open System
open System.Text.Json

/// Helper module for defining tools with F# idioms.
///
/// Example:
///   let weatherTool =
///       DefineTool.create
///           "get_weather"
///           "Get current weather for a city"
///           (Some parameterSchema)
///           (fun invocation -> async {
///               let city =
///                   invocation.Arguments
///                   |> Option.bind (fun a ->
///                       match a.TryGetProperty("city") with
///                       | true, c -> Some (c.GetString())
///                       | _ -> None)
///                   |> Option.defaultValue "Unknown"
///               return ToolResultObject.success (sprintf "Weather in %s: Sunny, 72F" city) :> obj
///           })
[<RequireQualifiedAccess>]
module DefineTool =

    /// Create a RegisteredTool from a name, description, optional JSON schema, and handler.
    let create
        (name: string)
        (description: string)
        (parameters: ToolParameterSchema option)
        (handler: ToolHandler) : RegisteredTool =
        { Tool =
            { Name = name
              Description = description
              Parameters = parameters }
          Handler = handler }

    /// Create a RegisteredTool using a typed argument extractor.
    /// The extractor deserializes the arguments JSON element into a typed value,
    /// and the handler receives that typed value.
    let createTyped<'TArgs, 'TResult>
        (name: string)
        (description: string)
        (parameters: ToolParameterSchema option)
        (handler: 'TArgs -> Async<'TResult>) : RegisteredTool =
        let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let wrappedHandler (invocation: ToolInvocation) = async {
            let args =
                match invocation.Arguments with
                | Some elem ->
                    JsonSerializer.Deserialize<'TArgs>(elem.GetRawText(), opts)
                | None ->
                    Unchecked.defaultof<'TArgs>
            let! result = handler args
            return result :> obj
        }
        create name description parameters wrappedHandler

    /// Create a simple tool with no parameters.
    let createSimple
        (name: string)
        (description: string)
        (handler: unit -> Async<string>) : RegisteredTool =
        let wrappedHandler (_invocation: ToolInvocation) = async {
            let! result = handler ()
            return ToolResultObject.success result :> obj
        }
        create name description None wrappedHandler

    /// Pipe-friendly tool builder: start with a name and description, then
    /// compose using |> withParameters |> withHandler.
    type ToolBuilder =
        { Name: string
          Description: string
          Parameters: ToolParameterSchema option
          Handler: ToolHandler option }

    let define (name: string) (description: string) : ToolBuilder =
        { Name = name
          Description = description
          Parameters = None
          Handler = None }

    let withParameters (schema: ToolParameterSchema) (builder: ToolBuilder) : ToolBuilder =
        { builder with Parameters = Some schema }

    let withHandler (handler: ToolHandler) (builder: ToolBuilder) : ToolBuilder =
        { builder with Handler = Some handler }

    let build (builder: ToolBuilder) : RegisteredTool =
        match builder.Handler with
        | Some handler -> create builder.Name builder.Description builder.Parameters handler
        | None -> failwith "Tool handler is required. Call withHandler before build."
