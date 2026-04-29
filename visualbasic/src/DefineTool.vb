' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Text.Json

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' Defines a custom tool that can be registered with a session so the
    ''' assistant can invoke it during a conversation.
    ''' </summary>
    ''' <example>
    ''' <code>
    ''' Dim weatherTool As New DefineTool With {
    '''     .Name = "get_weather",
    '''     .Description = "Get current weather for a city",
    '''     .ParametersSchema = New Dictionary(Of String, Object) From {
    '''         {"type", "object"},
    '''         {"properties", New Dictionary(Of String, Object) From {
    '''             {"city", New Dictionary(Of String, Object) From {
    '''                 {"type", "string"},
    '''                 {"description", "City name"}
    '''             }}
    '''         }},
    '''         {"required", New String() {"city"}}
    '''     },
    '''     .Handler = Async Function(input)
    '''                    Dim el = DirectCast(input, JsonElement)
    '''                    Dim city = el.GetProperty("city").GetString()
    '''                    Return $"Weather in {city}: 72F, sunny"
    '''                End Function
    ''' }
    ''' </code>
    ''' </example>
    Public Class DefineTool

        ''' <summary>Unique name of the tool (e.g. "get_weather").</summary>
        Public Property Name As String

        ''' <summary>Human-readable description shown to the model.</summary>
        Public Property Description As String

        ''' <summary>
        ''' JSON Schema describing the tool's input parameters.
        ''' Typically a Dictionary(Of String, Object) representing a JSON Schema object.
        ''' </summary>
        Public Property ParametersSchema As Object

        ''' <summary>
        ''' Async handler invoked when the assistant calls this tool.
        ''' The input parameter is the deserialized arguments (usually a <see cref="JsonElement"/>).
        ''' Return a string result for the model.
        ''' </summary>
        Public Property Handler As Func(Of Object, Task(Of String))

        ''' <summary>
        ''' Creates an empty tool definition. Set properties before registering.
        ''' </summary>
        Public Sub New()
        End Sub

        ''' <summary>
        ''' Creates a fully specified tool definition.
        ''' </summary>
        Public Sub New(name As String, description As String, parametersSchema As Object, handler As Func(Of Object, Task(Of String)))
            Me.Name = name
            Me.Description = description
            Me.ParametersSchema = parametersSchema
            Me.Handler = handler
        End Sub

    End Class

End Namespace
