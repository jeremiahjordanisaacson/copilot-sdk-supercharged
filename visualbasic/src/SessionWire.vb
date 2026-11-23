' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Text.Json

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' Translates the flat <c>session.create</c> / <c>session.resume</c> payload built by the client
    ''' into the grouped SDK protocol 4 payload. Options keep their flat names on the Visual Basic
    ''' side; only the wire shape changes.
    ''' </summary>
    Public Module SessionWire

        ''' <summary>Keys that stay at the top level of the grouped payload.</summary>
        Private ReadOnly RootKeys As New HashSet(Of String) From {
            "sessionId", "model", "name", "clientName", "workingDirectory", "configDirectory",
            "enableConfigDiscovery", "systemMessage", "diagnostics", "lifecycleDiagnostics", "cloud",
            "traceparent", "tracestate", "suppressResumeEvent", "continuePendingWork",
            "allowTranscriptRecovery", "requestedEnvironmentVariables"
        }

        ''' <summary>
        ''' Feature groups. A flat key with one of these names and a dictionary value is an
        ''' already-grouped object whose members win over flat keys.
        ''' </summary>
        Private ReadOnly GroupNames As New HashSet(Of String) From {
            "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
            "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
            "canvases", "extensions", "experiments", "policy"
        }

        ''' <summary>Flat key to (group, member).</summary>
        Private ReadOnly GroupedKeys As New Dictionary(Of String, String()) From {
            {"reasoningEffort", {"inference", "reasoningEffort"}},
            {"reasoningSummary", {"inference", "reasoningSummary"}},
            {"contextTier", {"inference", "contextTier"}},
            {"allowMultipleToolCalls", {"inference", "allowMultipleToolCalls"}},
            {"enableCitations", {"inference", "enableCitations"}},
            {"modelCapabilities", {"inference", "capabilities"}},
            {"verbosity", {"inference", "verbosity"}},
            {"providers", {"inference", "providers"}},
            {"models", {"inference", "models"}},
            {"defaultProvider", {"inference", "defaultProvider"}},
            {"tools", {"tools", "definitions"}},
            {"availableTools", {"tools", "availableTools"}},
            {"excludedTools", {"tools", "excludedTools"}},
            {"toolSearch", {"tools", "search"}},
            {"largeOutput", {"tools", "largeOutput"}},
            {"imageGeneration", {"tools", "imageGeneration"}},
            {"manageScheduleEnabled", {"tools", "manageScheduleEnabled"}},
            {"webSearch", {"tools", "webSearch"}},
            {"shell", {"tools", "shell"}},
            {"maxInlineBinaryBytes", {"tools", "maxInlineBinaryBytes"}},
            {"requestPermission", {"permissions", "request"}},
            {"sandboxConfig", {"permissions", "sandbox"}},
            {"sandbox", {"permissions", "sandbox"}},
            {"permissionMode", {"permissions", "mode"}},
            {"sandboxConfigSource", {"permissions", "sandboxSource"}},
            {"commands", {"interaction", "commands"}},
            {"askUserVariant", {"interaction", "askUserVariant"}},
            {"requestUserInput", {"interaction", "requestUserInput"}},
            {"requestElicitation", {"interaction", "requestElicitation"}},
            {"requestShellAuthentication", {"interaction", "requestShellAuthentication"}},
            {"requestExitPlanMode", {"interaction", "requestExitPlanMode"}},
            {"requestAutoModeSwitch", {"interaction", "requestAutoModeSwitch"}},
            {"requestAutoModeRecovery", {"interaction", "requestAutoModeRecovery"}},
            {"streaming", {"events", "streaming"}},
            {"includeSubAgentStreamingEvents", {"events", "includeSubAgentStreamingEvents"}},
            {"customAgents", {"agents", "definitions"}},
            {"defaultAgent", {"agents", "default"}},
            {"agent", {"agents", "selected"}},
            {"includedBuiltinAgents", {"agents", "includedBuiltinAgents"}},
            {"excludedBuiltinAgents", {"agents", "excludedBuiltinAgents"}},
            {"customAgentsLocalOnly", {"agents", "localDiscoveryOnly"}},
            {"agentDirectories", {"agents", "directories"}},
            {"skillDirectories", {"skills", "directories"}},
            {"enableSkills", {"skills", "enabled"}},
            {"includedBuiltinSkills", {"skills", "includedBuiltinSkills"}},
            {"disabledSkills", {"skills", "disabledSkills"}},
            {"hasSkillProvider", {"skills", "hasProvider"}},
            {"ignoredSkillsLocations", {"skills", "ignoredLocations"}},
            {"mcpServers", {"mcp", "servers"}},
            {"disabledMcpServers", {"mcp", "disabledServers"}},
            {"requestMcpApps", {"mcp", "enableApps"}},
            {"enableMcpApps", {"mcp", "enableApps"}},
            {"mcpOAuthTokenStorage", {"mcp", "oauthTokenStorage"}},
            {"authClientIdMetadataUrl", {"mcp", "authClientIdMetadataUrl"}},
            {"envValueMode", {"mcp", "envValueMode"}},
            {"managedMcpServers", {"mcp", "managedServers"}},
            {"allowAllMcpServerInstructions", {"mcp", "allowAllServerInstructions"}},
            {"gitHubToken", {"github", "token"}},
            {"gitHubTokenProviderRegistrationId", {"github", "tokenProviderRegistrationId"}},
            {"githubMcpToolConfig", {"github", "mcp"}},
            {"capi", {"github", "capi"}},
            {"enableSessionTelemetry", {"github", "telemetryEnabled"}},
            {"enableManagedSettings", {"github", "fetchManagedSettings"}},
            {"interactiveUser", {"github", "interactiveUser"}},
            {"enableGitHubTelemetryForwarding", {"github", "telemetryForwarding"}},
            {"authInfo", {"github", "authInfo"}},
            {"copilotUrl", {"github", "copilotUrl"}},
            {"additionalDirectories", {"filesystem", "additionalDirectories"}},
            {"enableFileChangeTracking", {"filesystem", "trackFileChanges"}},
            {"trustWorkingDirectory", {"filesystem", "trustWorkingDirectory"}},
            {"sessionLimits", {"execution", "limits"}},
            {"autopilot", {"execution", "autopilot"}},
            {"autopilotContinuation", {"execution", "autopilotContinuation"}},
            {"instructionDirectories", {"instructions", "directories"}},
            {"organizationCustomInstructions", {"instructions", "organizationText"}},
            {"enableOnDemandInstructionDiscovery", {"instructions", "onDemandDiscovery"}},
            {"skipCustomInstructions", {"instructions", "skipCustomInstructions"}},
            {"refreshCustomInstructions", {"instructions", "refreshCustomInstructions"}},
            {"hooks", {"hooks", "hasHandlers"}},
            {"hookDirectories", {"hooks", "directories"}},
            {"enableFileHooks", {"hooks", "enableRepositoryHooks"}},
            {"coauthorEnabled", {"git", "coauthorEnabled"}},
            {"enableHostGitOperations", {"git", "enableHostGitOperations"}},
            {"infiniteSessions", {"state", "infiniteSessions"}},
            {"memory", {"state", "memory"}},
            {"hasMemoryStore", {"state", "hasMemoryStore"}},
            {"enableSessionStore", {"state", "enableCrossSessionStore"}},
            {"skipEmbeddingRetrieval", {"state", "skipEmbeddingRetrieval"}},
            {"embeddingCacheStorage", {"state", "embeddingCacheStorage"}},
            {"pluginDirectories", {"plugins", "directories"}},
            {"installedPlugins", {"plugins", "installed"}},
            {"canvases", {"canvases", "definitions"}},
            {"canvasProvider", {"canvases", "provider"}},
            {"requestCanvasRenderer", {"canvases", "requestRenderer"}},
            {"openCanvases", {"canvases", "open"}},
            {"requestExtensions", {"extensions", "enabled"}},
            {"extensionSdkPath", {"extensions", "sdkPath"}},
            {"extensionInfo", {"extensions", "identity"}},
            {"enableExperimentalMode", {"experiments", "enableExperimentalMode"}},
            {"isExperimentalMode", {"experiments", "enableExperimentalMode"}},
            {"featureFlags", {"experiments", "flags"}},
            {"expAssignments", {"experiments", "assignments"}},
            {"featureFlagConfiguration", {"experiments", "configuration"}},
            {"allowedModels", {"policy", "allowedModels"}},
            {"managedSettings", {"policy", "managedSettings"}},
            {"excludedManagedSettings", {"policy", "excludedManagedSettings"}},
            {"enforceManagedModelDefaults", {"policy", "enforceManagedModelDefaults"}}
        }

        ''' <summary>Group members accepted only by session.create (dropped on resume).</summary>
        Private ReadOnly CreateOnlyMembers As New HashSet(Of String) From {
            "filesystem.trustWorkingDirectory", "instructions.refreshCustomInstructions"
        }

        ''' <summary>Group members accepted only by session.resume (dropped on create).</summary>
        Private ReadOnly ResumeOnlyMembers As New HashSet(Of String) From {"canvases.open"}

        ''' <summary>Members accepted in each group: exactly the "group.member" paths of the table above.</summary>
        Private ReadOnly AllowedMembers As HashSet(Of String) =
            New HashSet(Of String)(GroupedKeys.Values.Select(Function(path) path(0) & "." & path(1)))

        ''' <summary>Legacy <c>provider</c> fields that describe the model rather than the connection.</summary>
        Private ReadOnly ProviderModelFields As New HashSet(Of String) From {
            "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens", "capabilities"
        }

        ''' <summary>
        ''' Converts a flat <c>session.create</c> (isResume = False) or <c>session.resume</c>
        ''' (isResume = True) payload into the grouped SDK protocol 4 payload. ROOT keys stay top
        ''' level, known flat keys move to <c>group.member</c>, unknown keys stay at the root, null
        ''' values are skipped, empty groups are not emitted, and members of already-grouped
        ''' dictionaries win over flat keys.
        ''' </summary>
        Public Function GroupSessionParams(flat As IDictionary(Of String, Object), isResume As Boolean) As Dictionary(Of String, Object)
            Dim result As New Dictionary(Of String, Object)
            Dim groups As New Dictionary(Of String, Dictionary(Of String, Object))
            Dim order As New List(Of String)
            Dim legacyProvider As IDictionary(Of String, Object) = Nothing

            For Each kvp In flat
                Dim key = kvp.Key
                Dim value = kvp.Value
                If IsNullValue(value) Then Continue For

                If RootKeys.Contains(key) Then
                    result(key) = value
                ElseIf key = "configDir" Then
                    If Not result.ContainsKey("configDirectory") Then result("configDirectory") = value
                ElseIf key = "disableResume" Then
                    If isResume AndAlso Not result.ContainsKey("suppressResumeEvent") Then result("suppressResumeEvent") = value
                ElseIf key = "provider" Then
                    legacyProvider = TryCast(value, IDictionary(Of String, Object))
                ElseIf GroupNames.Contains(key) AndAlso TypeOf value Is IDictionary(Of String, Object) Then
                    Dim target = GetGroup(groups, order, key)
                    For Each member In DirectCast(value, IDictionary(Of String, Object))
                        If MemberAllowed(isResume, key, member.Key) Then target(member.Key) = member.Value
                    Next
                Else
                    Dim path As String() = Nothing
                    If GroupedKeys.TryGetValue(key, path) Then
                        If MemberAllowed(isResume, path(0), path(1)) Then
                            SetIfAbsent(GetGroup(groups, order, path(0)), path(1), value)
                            If key = "mcpServers" Then SetIfAbsent(GetGroup(groups, order, "mcp"), "envValueMode", "direct")
                        End If
                    Else
                        result(key) = value
                    End If
                End If
            Next

            If legacyProvider IsNot Nothing Then
                TranslateLegacyProvider(legacyProvider, result, groups, order)
            End If

            For Each name In order
                Dim members = groups(name)
                If members.Count > 0 Then result(name) = members
            Next
            Return result
        End Function

        ''' <summary>
        ''' Applies the <see cref="ClientMode.Empty"/> defaults to a grouped payload. Values the caller
        ''' already set (non-null) are kept.
        ''' </summary>
        Public Function ApplyEmptyModeDefaults(grouped As Dictionary(Of String, Object)) As Dictionary(Of String, Object)
            ApplyDefault(grouped, "experiments", "enableExperimentalMode", False)
            ApplyDefault(grouped, "github", "telemetryEnabled", False)
            ApplyDefault(grouped, "mcp", "oauthTokenStorage", "in-memory")
            ApplyDefault(grouped, "state", "skipEmbeddingRetrieval", True)
            ApplyDefault(grouped, "state", "embeddingCacheStorage", "in-memory")
            ApplyDefault(grouped, "state", "enableCrossSessionStore", False)
            ApplyDefault(grouped, "state", "memory", New Dictionary(Of String, Object) From {{"enabled", False}})
            ApplyDefault(grouped, "instructions", "onDemandDiscovery", False)
            ApplyDefault(grouped, "hooks", "enableRepositoryHooks", False)
            ApplyDefault(grouped, "git", "enableHostGitOperations", False)
            Return grouped
        End Function

        ' ----- helpers -----

        ''' <summary>
        ''' The runtime rejects unknown group members, so only table members are sent, honoring the
        ''' create-only and resume-only members.
        ''' </summary>
        Private Function MemberAllowed(isResume As Boolean, groupName As String, member As String) As Boolean
            Dim memberPath = groupName & "." & member
            If Not AllowedMembers.Contains(memberPath) Then Return False
            If isResume AndAlso CreateOnlyMembers.Contains(memberPath) Then Return False
            If Not isResume AndAlso ResumeOnlyMembers.Contains(memberPath) Then Return False
            Return True
        End Function

        Private Function IsNullValue(value As Object) As Boolean
            If value Is Nothing Then Return True
            If TypeOf value Is JsonElement Then
                Dim element = DirectCast(value, JsonElement)
                Return element.ValueKind = JsonValueKind.Null OrElse element.ValueKind = JsonValueKind.Undefined
            End If
            Return False
        End Function

        Private Function GetGroup(groups As Dictionary(Of String, Dictionary(Of String, Object)),
                                  order As List(Of String),
                                  name As String) As Dictionary(Of String, Object)
            Dim existing As Dictionary(Of String, Object) = Nothing
            If groups.TryGetValue(name, existing) Then Return existing
            Dim created As New Dictionary(Of String, Object)
            groups(name) = created
            order.Add(name)
            Return created
        End Function

        Private Sub SetIfAbsent(target As Dictionary(Of String, Object), member As String, value As Object)
            If Not target.ContainsKey(member) Then target(member) = value
        End Sub

        Private Sub ApplyDefault(grouped As Dictionary(Of String, Object), groupName As String, member As String, value As Object)
            Dim existing As Object = Nothing
            Dim target As IDictionary(Of String, Object) = Nothing
            If grouped.TryGetValue(groupName, existing) Then target = TryCast(existing, IDictionary(Of String, Object))
            If target Is Nothing Then
                target = New Dictionary(Of String, Object)
                grouped(groupName) = target
            End If
            Dim current As Object = Nothing
            If target.TryGetValue(member, current) AndAlso Not IsNullValue(current) Then Return
            target(member) = value
        End Sub

        ''' <summary>
        ''' Translates the legacy single BYOK <c>provider</c> into a named provider called "default"
        ''' (with <c>github.capi.enabled = false</c>, as v1 BYOK behaved). Explicit named providers,
        ''' a default provider, or <c>capi.enabled</c> win over the legacy option.
        ''' </summary>
        Private Sub TranslateLegacyProvider(provider As IDictionary(Of String, Object),
                                            result As Dictionary(Of String, Object),
                                            groups As Dictionary(Of String, Dictionary(Of String, Object)),
                                            order As List(Of String))
            Dim inference = GetGroup(groups, order, "inference")
            Dim github = GetGroup(groups, order, "github")
            Dim existingCapi As Object = Nothing
            Dim capi As IDictionary(Of String, Object) = Nothing
            If github.TryGetValue("capi", existingCapi) Then capi = TryCast(existingCapi, IDictionary(Of String, Object))
            Dim capiEnabledSet = capi IsNot Nothing AndAlso capi.ContainsKey("enabled")
            If inference.ContainsKey("providers") OrElse inference.ContainsKey("defaultProvider") OrElse capiEnabledSet Then Return

            Dim connection As New Dictionary(Of String, Object) From {{"name", "default"}}
            Dim modelFields As New Dictionary(Of String, Object)
            For Each field In provider
                If IsNullValue(field.Value) Then Continue For
                If ProviderModelFields.Contains(field.Key) Then
                    modelFields(field.Key) = field.Value
                Else
                    connection(field.Key) = field.Value
                End If
            Next

            inference("providers") = New List(Of Object) From {connection}
            inference("defaultProvider") = "default"
            If capi Is Nothing Then
                capi = New Dictionary(Of String, Object)
                github("capi") = capi
            End If
            capi("enabled") = False

            Dim model As Object = Nothing
            If modelFields.Count > 0 AndAlso result.TryGetValue("model", model) AndAlso
               TypeOf model Is String AndAlso Not String.IsNullOrEmpty(DirectCast(model, String)) Then
                Dim entry As New Dictionary(Of String, Object) From {{"provider", "default"}, {"id", model}}
                For Each field In modelFields
                    entry(field.Key) = field.Value
                Next
                inference("models") = New List(Of Object) From {entry}
            End If
        End Sub

    End Module

End Namespace
