(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Translates the flat [session.create] / [session.resume] payload built by
    the client into the grouped SDK protocol 4 payload. Options keep their
    flat names on the OCaml side; only the wire shape changes. *)

(* Keys that stay at the top level of the grouped payload. *)
let root_keys =
  [ "sessionId"; "model"; "name"; "clientName"; "workingDirectory"
  ; "configDirectory"; "enableConfigDiscovery"; "systemMessage"; "diagnostics"
  ; "lifecycleDiagnostics"; "cloud"; "traceparent"; "tracestate"
  ; "suppressResumeEvent"; "continuePendingWork"; "allowTranscriptRecovery"
  ; "requestedEnvironmentVariables"
  ]

(* Feature groups. A flat key with one of these names and an object value is
   an already-grouped object whose members win over flat keys. *)
let group_names =
  [ "inference"; "tools"; "permissions"; "interaction"; "events"; "agents"
  ; "skills"; "mcp"; "github"; "filesystem"; "execution"; "instructions"
  ; "hooks"; "git"; "state"; "plugins"; "canvases"; "extensions"
  ; "experiments"; "policy"
  ]

(* Flat key -> (group, member). *)
let grouped_keys : (string * (string * string)) list =
  [ ("reasoningEffort", ("inference", "reasoningEffort"))
  ; ("reasoningSummary", ("inference", "reasoningSummary"))
  ; ("contextTier", ("inference", "contextTier"))
  ; ("allowMultipleToolCalls", ("inference", "allowMultipleToolCalls"))
  ; ("enableCitations", ("inference", "enableCitations"))
  ; ("modelCapabilities", ("inference", "capabilities"))
  ; ("verbosity", ("inference", "verbosity"))
  ; ("providers", ("inference", "providers"))
  ; ("models", ("inference", "models"))
  ; ("defaultProvider", ("inference", "defaultProvider"))
  ; ("tools", ("tools", "definitions"))
  ; ("availableTools", ("tools", "availableTools"))
  ; ("excludedTools", ("tools", "excludedTools"))
  ; ("toolSearch", ("tools", "search"))
  ; ("largeOutput", ("tools", "largeOutput"))
  ; ("imageGeneration", ("tools", "imageGeneration"))
  ; ("manageScheduleEnabled", ("tools", "manageScheduleEnabled"))
  ; ("webSearch", ("tools", "webSearch"))
  ; ("shell", ("tools", "shell"))
  ; ("maxInlineBinaryBytes", ("tools", "maxInlineBinaryBytes"))
  ; ("requestPermission", ("permissions", "request"))
  ; ("sandboxConfig", ("permissions", "sandbox"))
  ; ("sandbox", ("permissions", "sandbox"))
  ; ("permissionMode", ("permissions", "mode"))
  ; ("sandboxConfigSource", ("permissions", "sandboxSource"))
  ; ("commands", ("interaction", "commands"))
  ; ("askUserVariant", ("interaction", "askUserVariant"))
  ; ("requestUserInput", ("interaction", "requestUserInput"))
  ; ("requestElicitation", ("interaction", "requestElicitation"))
  ; ("requestShellAuthentication", ("interaction", "requestShellAuthentication"))
  ; ("requestExitPlanMode", ("interaction", "requestExitPlanMode"))
  ; ("requestAutoModeSwitch", ("interaction", "requestAutoModeSwitch"))
  ; ("requestAutoModeRecovery", ("interaction", "requestAutoModeRecovery"))
  ; ("streaming", ("events", "streaming"))
  ; ("includeSubAgentStreamingEvents", ("events", "includeSubAgentStreamingEvents"))
  ; ("customAgents", ("agents", "definitions"))
  ; ("defaultAgent", ("agents", "default"))
  ; ("agent", ("agents", "selected"))
  ; ("includedBuiltinAgents", ("agents", "includedBuiltinAgents"))
  ; ("excludedBuiltinAgents", ("agents", "excludedBuiltinAgents"))
  ; ("customAgentsLocalOnly", ("agents", "localDiscoveryOnly"))
  ; ("agentDirectories", ("agents", "directories"))
  ; ("skillDirectories", ("skills", "directories"))
  ; ("enableSkills", ("skills", "enabled"))
  ; ("includedBuiltinSkills", ("skills", "includedBuiltinSkills"))
  ; ("disabledSkills", ("skills", "disabledSkills"))
  ; ("hasSkillProvider", ("skills", "hasProvider"))
  ; ("ignoredSkillsLocations", ("skills", "ignoredLocations"))
  ; ("mcpServers", ("mcp", "servers"))
  ; ("disabledMcpServers", ("mcp", "disabledServers"))
  ; ("requestMcpApps", ("mcp", "enableApps"))
  ; ("enableMcpApps", ("mcp", "enableApps"))
  ; ("mcpOAuthTokenStorage", ("mcp", "oauthTokenStorage"))
  ; ("authClientIdMetadataUrl", ("mcp", "authClientIdMetadataUrl"))
  ; ("envValueMode", ("mcp", "envValueMode"))
  ; ("managedMcpServers", ("mcp", "managedServers"))
  ; ("allowAllMcpServerInstructions", ("mcp", "allowAllServerInstructions"))
  ; ("gitHubToken", ("github", "token"))
  ; ("gitHubTokenProviderRegistrationId", ("github", "tokenProviderRegistrationId"))
  ; ("githubMcpToolConfig", ("github", "mcp"))
  ; ("capi", ("github", "capi"))
  ; ("enableSessionTelemetry", ("github", "telemetryEnabled"))
  ; ("enableManagedSettings", ("github", "fetchManagedSettings"))
  ; ("interactiveUser", ("github", "interactiveUser"))
  ; ("enableGitHubTelemetryForwarding", ("github", "telemetryForwarding"))
  ; ("authInfo", ("github", "authInfo"))
  ; ("copilotUrl", ("github", "copilotUrl"))
  ; ("additionalDirectories", ("filesystem", "additionalDirectories"))
  ; ("enableFileChangeTracking", ("filesystem", "trackFileChanges"))
  ; ("trustWorkingDirectory", ("filesystem", "trustWorkingDirectory"))
  ; ("sessionLimits", ("execution", "limits"))
  ; ("autopilot", ("execution", "autopilot"))
  ; ("autopilotContinuation", ("execution", "autopilotContinuation"))
  ; ("instructionDirectories", ("instructions", "directories"))
  ; ("organizationCustomInstructions", ("instructions", "organizationText"))
  ; ("enableOnDemandInstructionDiscovery", ("instructions", "onDemandDiscovery"))
  ; ("skipCustomInstructions", ("instructions", "skipCustomInstructions"))
  ; ("refreshCustomInstructions", ("instructions", "refreshCustomInstructions"))
  ; ("hooks", ("hooks", "hasHandlers"))
  ; ("hookDirectories", ("hooks", "directories"))
  ; ("enableFileHooks", ("hooks", "enableRepositoryHooks"))
  ; ("coauthorEnabled", ("git", "coauthorEnabled"))
  ; ("enableHostGitOperations", ("git", "enableHostGitOperations"))
  ; ("infiniteSessions", ("state", "infiniteSessions"))
  ; ("memory", ("state", "memory"))
  ; ("hasMemoryStore", ("state", "hasMemoryStore"))
  ; ("enableSessionStore", ("state", "enableCrossSessionStore"))
  ; ("skipEmbeddingRetrieval", ("state", "skipEmbeddingRetrieval"))
  ; ("embeddingCacheStorage", ("state", "embeddingCacheStorage"))
  ; ("pluginDirectories", ("plugins", "directories"))
  ; ("installedPlugins", ("plugins", "installed"))
  ; ("canvases", ("canvases", "definitions"))
  ; ("canvasProvider", ("canvases", "provider"))
  ; ("requestCanvasRenderer", ("canvases", "requestRenderer"))
  ; ("openCanvases", ("canvases", "open"))
  ; ("requestExtensions", ("extensions", "enabled"))
  ; ("extensionSdkPath", ("extensions", "sdkPath"))
  ; ("extensionInfo", ("extensions", "identity"))
  ; ("enableExperimentalMode", ("experiments", "enableExperimentalMode"))
  ; ("isExperimentalMode", ("experiments", "enableExperimentalMode"))
  ; ("featureFlags", ("experiments", "flags"))
  ; ("expAssignments", ("experiments", "assignments"))
  ; ("featureFlagConfiguration", ("experiments", "configuration"))
  ; ("allowedModels", ("policy", "allowedModels"))
  ; ("managedSettings", ("policy", "managedSettings"))
  ; ("excludedManagedSettings", ("policy", "excludedManagedSettings"))
  ; ("enforceManagedModelDefaults", ("policy", "enforceManagedModelDefaults"))
  ]

(* Group members accepted only by session.create (dropped on resume). *)
let create_only_members =
  [ ("filesystem", "trustWorkingDirectory"); ("instructions", "refreshCustomInstructions") ]

(* Group members accepted only by session.resume (dropped on create). *)
let resume_only_members = [ ("canvases", "open") ]

(* Legacy [provider] fields that describe the model, not the connection. *)
let provider_model_fields =
  [ "modelId"; "wireModel"; "maxPromptTokens"; "maxContextWindowTokens"
  ; "maxOutputTokens"; "capabilities"
  ]

(* The runtime rejects unknown group members, so only members named in
   [grouped_keys] are sent, honoring the create-only and resume-only members. *)
let member_allowed ~resume (group_name : string) (member : string) : bool =
  List.exists (fun (_, target) -> target = (group_name, member)) grouped_keys
  && not (resume && List.mem (group_name, member) create_only_members)
  && not ((not resume) && List.mem (group_name, member) resume_only_members)

(** [group_session_params ~resume flat] converts a flat [session.create]
    ([~resume:false]) or [session.resume] ([~resume:true]) payload into the
    grouped SDK protocol 4 payload. ROOT keys stay top level, known flat keys
    move to [group.member], unknown keys stay at the root, null values are
    skipped, empty groups are not emitted, and members of already-grouped
    objects win over flat keys. The legacy single [provider] becomes a named
    provider called "default". *)
let group_session_params ~resume (flat : Yojson.Safe.t) : Yojson.Safe.t =
  match flat with
  | `Assoc fields ->
    let root : (string * Yojson.Safe.t) list ref = ref [] in
    let groups : (string, (string * Yojson.Safe.t) list ref) Hashtbl.t =
      Hashtbl.create 16
    in
    let order : string list ref = ref [] in
    let group name =
      match Hashtbl.find_opt groups name with
      | Some members -> members
      | None ->
        let members = ref [] in
        Hashtbl.replace groups name members;
        order := name :: !order;
        members
    in
    let set_member ~overwrite group_name member value =
      let members = group group_name in
      if not (List.mem_assoc member !members) then
        members := (member, value) :: !members
      else if overwrite then
        members := (member, value) :: List.remove_assoc member !members
    in
    let set_root key value = root := (key, value) :: List.remove_assoc key !root in
    let legacy_provider = ref None in
    let place (key, value) =
      if value = `Null then ()
      else if List.mem key root_keys then set_root key value
      else if key = "configDir" then begin
        if not (List.mem_assoc "configDirectory" !root) then
          set_root "configDirectory" value
      end
      else if key = "disableResume" then begin
        if resume && not (List.mem_assoc "suppressResumeEvent" !root) then
          set_root "suppressResumeEvent" value
      end
      else if key = "provider" then begin
        match value with
        | `Assoc provider_fields -> legacy_provider := Some provider_fields
        | _ -> ()
      end
      else begin
        match value with
        | `Assoc members when List.mem key group_names ->
          List.iter
            (fun (member, member_value) ->
              if member_allowed ~resume key member then
                set_member ~overwrite:true key member member_value)
            members
        | _ ->
          (match List.assoc_opt key grouped_keys with
           | Some (group_name, member) ->
             if member_allowed ~resume group_name member then begin
               set_member ~overwrite:false group_name member value;
               if key = "mcpServers" then
                 set_member ~overwrite:false "mcp" "envValueMode" (`String "direct")
             end
           | None -> set_root key value)
      end
    in
    List.iter place fields;
    (match !legacy_provider with
     | None -> ()
     | Some provider_fields ->
       let inference = group "inference" in
       let github = group "github" in
       let existing_capi =
         match List.assoc_opt "capi" !github with
         | Some (`Assoc capi) -> capi
         | _ -> []
       in
       let explicit =
         List.mem_assoc "providers" !inference
         || List.mem_assoc "defaultProvider" !inference
         || List.mem_assoc "enabled" existing_capi
       in
       if not explicit then begin
         let present = List.filter (fun (_, v) -> v <> `Null) provider_fields in
         let model_fields, connection =
           List.partition (fun (k, _) -> List.mem k provider_model_fields) present
         in
         set_member ~overwrite:true "inference" "providers"
           (`List [ `Assoc (("name", `String "default") :: connection) ]);
         set_member ~overwrite:true "inference" "defaultProvider" (`String "default");
         set_member ~overwrite:true "github" "capi"
           (`Assoc (List.remove_assoc "enabled" existing_capi @ [ ("enabled", `Bool false) ]));
         match List.assoc_opt "model" !root, model_fields with
         | Some (`String model), _ :: _ when model <> "" ->
           set_member ~overwrite:true "inference" "models"
             (`List
               [ `Assoc
                   (("provider", `String "default") :: ("id", `String model) :: model_fields)
               ])
         | _ -> ()
       end);
    let group_fields =
      List.filter_map
        (fun name ->
          match !(Hashtbl.find groups name) with
          | [] -> None
          | members -> Some (name, `Assoc (List.rev members)))
        (List.rev !order)
    in
    let root_fields =
      List.filter (fun (key, _) -> not (List.mem_assoc key group_fields)) (List.rev !root)
    in
    `Assoc (root_fields @ group_fields)
  | other -> other

(* Defaults applied under the caller's values for the "empty" client mode. *)
let empty_mode_defaults : (string * string * Yojson.Safe.t) list =
  [ ("experiments", "enableExperimentalMode", `Bool false)
  ; ("github", "telemetryEnabled", `Bool false)
  ; ("mcp", "oauthTokenStorage", `String "in-memory")
  ; ("state", "skipEmbeddingRetrieval", `Bool true)
  ; ("state", "embeddingCacheStorage", `String "in-memory")
  ; ("state", "enableCrossSessionStore", `Bool false)
  ; ("state", "memory", `Assoc [ ("enabled", `Bool false) ])
  ; ("instructions", "onDemandDiscovery", `Bool false)
  ; ("hooks", "enableRepositoryHooks", `Bool false)
  ; ("git", "enableHostGitOperations", `Bool false)
  ]

(** [apply_empty_mode_defaults grouped] adds the "empty" client-mode defaults
    to a grouped payload, keeping every value the caller already set. *)
let apply_empty_mode_defaults (grouped : Yojson.Safe.t) : Yojson.Safe.t =
  match grouped with
  | `Assoc fields ->
    let apply fields (group_name, member, value) =
      let members =
        match List.assoc_opt group_name fields with
        | Some (`Assoc members) -> members
        | _ -> []
      in
      match List.assoc_opt member members with
      | Some existing when existing <> `Null -> fields
      | _ ->
        let updated = `Assoc (List.remove_assoc member members @ [ (member, value) ]) in
        if List.mem_assoc group_name fields then
          List.map (fun (key, v) -> if key = group_name then (key, updated) else (key, v)) fields
        else fields @ [ (group_name, updated) ]
    in
    `Assoc (List.fold_left apply fields empty_mode_defaults)
  | other -> other
