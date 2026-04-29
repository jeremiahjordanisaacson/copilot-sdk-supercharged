' Copyright (c) Microsoft Corporation. All rights reserved.

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' Provides the SDK protocol version constant.
    ''' This must match the version expected by the copilot-agent-runtime server.
    ''' </summary>
    Public Module SdkProtocolVersion

        ''' <summary>
        ''' The SDK protocol version string.
        ''' </summary>
        Public Const Version As String = "2"

        ''' <summary>
        ''' Gets the protocol version as an integer.
        ''' </summary>
        Public Function GetVersion() As Integer
            Return Integer.Parse(Version)
        End Function

    End Module

End Namespace
