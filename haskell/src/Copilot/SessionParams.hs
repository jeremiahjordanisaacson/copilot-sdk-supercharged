---------------------------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
---------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- | SDK protocol 4 session payload translation.
--
-- "Copilot.Client" builds a flat @session.create@ \/ @session.resume@ payload
-- from 'Copilot.Types.SessionConfig'. Protocol 4 servers expect most options
-- inside named groups (@inference@, @tools@, @permissions@, ...) and reject
-- the flat keys, so 'groupSessionParams' converts the payload right before it
-- is sent. Callers keep using the same configuration fields.
module Copilot.SessionParams
  ( groupSessionParams
  , applyEmptyModeDefaults
  ) where

import           Data.Aeson        (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as KM
import           Data.List         (foldl')
import qualified Data.Map.Strict   as Map
import           Data.Text         (Text)
import qualified Data.Text         as T

-- | Keys that stay at the top level of the payload.
sessionRootKeys :: [Text]
sessionRootKeys =
  [ "sessionId", "model", "name", "clientName", "workingDirectory"
  , "configDirectory", "enableConfigDiscovery", "systemMessage", "diagnostics"
  , "lifecycleDiagnostics", "cloud", "traceparent", "tracestate"
  , "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery"
  , "requestedEnvironmentVariables"
  ]

-- | Protocol 4 option groups.
sessionGroupNames :: [Text]
sessionGroupNames =
  [ "inference", "tools", "permissions", "interaction", "events", "agents"
  , "skills", "mcp", "github", "filesystem", "execution", "instructions"
  , "hooks", "git", "state", "plugins", "canvases", "extensions"
  , "experiments", "policy"
  ]

createOnlyGroupedKeys, resumeOnlyGroupedKeys :: [Text]
createOnlyGroupedKeys = ["trustWorkingDirectory", "refreshCustomInstructions"]
resumeOnlyGroupedKeys = ["openCanvases"]

-- | Legacy @provider@ members that describe the model, not the connection.
legacyProviderModelFields :: [Text]
legacyProviderModelFields =
  [ "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens"
  , "maxOutputTokens", "capabilities"
  ]

-- | Flat wire key to (group, member).
sessionGroupedKeys :: Map.Map Text (Text, Text)
sessionGroupedKeys = Map.fromList
  [ ("reasoningEffort", ("inference", "reasoningEffort"))
  , ("reasoningSummary", ("inference", "reasoningSummary"))
  , ("contextTier", ("inference", "contextTier"))
  , ("allowMultipleToolCalls", ("inference", "allowMultipleToolCalls"))
  , ("enableCitations", ("inference", "enableCitations"))
  , ("modelCapabilities", ("inference", "capabilities"))
  , ("verbosity", ("inference", "verbosity"))
  , ("providers", ("inference", "providers"))
  , ("models", ("inference", "models"))
  , ("defaultProvider", ("inference", "defaultProvider"))
  , ("tools", ("tools", "definitions"))
  , ("availableTools", ("tools", "availableTools"))
  , ("excludedTools", ("tools", "excludedTools"))
  , ("toolSearch", ("tools", "search"))
  , ("largeOutput", ("tools", "largeOutput"))
  , ("imageGeneration", ("tools", "imageGeneration"))
  , ("manageScheduleEnabled", ("tools", "manageScheduleEnabled"))
  , ("webSearch", ("tools", "webSearch"))
  , ("shell", ("tools", "shell"))
  , ("maxInlineBinaryBytes", ("tools", "maxInlineBinaryBytes"))
  , ("requestPermission", ("permissions", "request"))
  , ("sandboxConfig", ("permissions", "sandbox"))
  , ("sandbox", ("permissions", "sandbox"))
  , ("permissionMode", ("permissions", "mode"))
  , ("sandboxConfigSource", ("permissions", "sandboxSource"))
  , ("commands", ("interaction", "commands"))
  , ("askUserVariant", ("interaction", "askUserVariant"))
  , ("requestUserInput", ("interaction", "requestUserInput"))
  , ("requestElicitation", ("interaction", "requestElicitation"))
  , ("requestShellAuthentication", ("interaction", "requestShellAuthentication"))
  , ("requestExitPlanMode", ("interaction", "requestExitPlanMode"))
  , ("requestAutoModeSwitch", ("interaction", "requestAutoModeSwitch"))
  , ("requestAutoModeRecovery", ("interaction", "requestAutoModeRecovery"))
  , ("streaming", ("events", "streaming"))
  , ("includeSubAgentStreamingEvents", ("events", "includeSubAgentStreamingEvents"))
  , ("customAgents", ("agents", "definitions"))
  , ("defaultAgent", ("agents", "default"))
  , ("agent", ("agents", "selected"))
  , ("includedBuiltinAgents", ("agents", "includedBuiltinAgents"))
  , ("excludedBuiltinAgents", ("agents", "excludedBuiltinAgents"))
  , ("customAgentsLocalOnly", ("agents", "localDiscoveryOnly"))
  , ("agentDirectories", ("agents", "directories"))
  , ("skillDirectories", ("skills", "directories"))
  , ("enableSkills", ("skills", "enabled"))
  , ("includedBuiltinSkills", ("skills", "includedBuiltinSkills"))
  , ("disabledSkills", ("skills", "disabledSkills"))
  , ("hasSkillProvider", ("skills", "hasProvider"))
  , ("ignoredSkillsLocations", ("skills", "ignoredLocations"))
  , ("mcpServers", ("mcp", "servers"))
  , ("disabledMcpServers", ("mcp", "disabledServers"))
  , ("requestMcpApps", ("mcp", "enableApps"))
  , ("enableMcpApps", ("mcp", "enableApps"))
  , ("mcpOAuthTokenStorage", ("mcp", "oauthTokenStorage"))
  , ("authClientIdMetadataUrl", ("mcp", "authClientIdMetadataUrl"))
  , ("envValueMode", ("mcp", "envValueMode"))
  , ("managedMcpServers", ("mcp", "managedServers"))
  , ("allowAllMcpServerInstructions", ("mcp", "allowAllServerInstructions"))
  , ("gitHubToken", ("github", "token"))
  , ("gitHubTokenProviderRegistrationId", ("github", "tokenProviderRegistrationId"))
  , ("githubMcpToolConfig", ("github", "mcp"))
  , ("capi", ("github", "capi"))
  , ("enableSessionTelemetry", ("github", "telemetryEnabled"))
  , ("enableManagedSettings", ("github", "fetchManagedSettings"))
  , ("interactiveUser", ("github", "interactiveUser"))
  , ("enableGitHubTelemetryForwarding", ("github", "telemetryForwarding"))
  , ("authInfo", ("github", "authInfo"))
  , ("copilotUrl", ("github", "copilotUrl"))
  , ("additionalDirectories", ("filesystem", "additionalDirectories"))
  , ("enableFileChangeTracking", ("filesystem", "trackFileChanges"))
  , ("trustWorkingDirectory", ("filesystem", "trustWorkingDirectory"))
  , ("sessionLimits", ("execution", "limits"))
  , ("autopilot", ("execution", "autopilot"))
  , ("autopilotContinuation", ("execution", "autopilotContinuation"))
  , ("instructionDirectories", ("instructions", "directories"))
  , ("organizationCustomInstructions", ("instructions", "organizationText"))
  , ("enableOnDemandInstructionDiscovery", ("instructions", "onDemandDiscovery"))
  , ("skipCustomInstructions", ("instructions", "skipCustomInstructions"))
  , ("refreshCustomInstructions", ("instructions", "refreshCustomInstructions"))
  , ("hooks", ("hooks", "hasHandlers"))
  , ("hookDirectories", ("hooks", "directories"))
  , ("enableFileHooks", ("hooks", "enableRepositoryHooks"))
  , ("coauthorEnabled", ("git", "coauthorEnabled"))
  , ("enableHostGitOperations", ("git", "enableHostGitOperations"))
  , ("infiniteSessions", ("state", "infiniteSessions"))
  , ("memory", ("state", "memory"))
  , ("hasMemoryStore", ("state", "hasMemoryStore"))
  , ("enableSessionStore", ("state", "enableCrossSessionStore"))
  , ("skipEmbeddingRetrieval", ("state", "skipEmbeddingRetrieval"))
  , ("embeddingCacheStorage", ("state", "embeddingCacheStorage"))
  , ("pluginDirectories", ("plugins", "directories"))
  , ("installedPlugins", ("plugins", "installed"))
  , ("canvases", ("canvases", "definitions"))
  , ("canvasProvider", ("canvases", "provider"))
  , ("requestCanvasRenderer", ("canvases", "requestRenderer"))
  , ("openCanvases", ("canvases", "open"))
  , ("requestExtensions", ("extensions", "enabled"))
  , ("extensionSdkPath", ("extensions", "sdkPath"))
  , ("extensionInfo", ("extensions", "identity"))
  , ("enableExperimentalMode", ("experiments", "enableExperimentalMode"))
  , ("isExperimentalMode", ("experiments", "enableExperimentalMode"))
  , ("featureFlags", ("experiments", "flags"))
  , ("expAssignments", ("experiments", "assignments"))
  , ("featureFlagConfiguration", ("experiments", "configuration"))
  , ("allowedModels", ("policy", "allowedModels"))
  , ("managedSettings", ("policy", "managedSettings"))
  , ("excludedManagedSettings", ("policy", "excludedManagedSettings"))
  , ("enforceManagedModelDefaults", ("policy", "enforceManagedModelDefaults"))
  ]

-- | Option groups being built: group name to its members.
type Groups = Map.Map Text (KM.KeyMap Value)

-- | Set @group.member@; later writes win.
putMember :: Text -> Text -> Value -> Groups -> Groups
putMember group member value =
  Map.insertWith KM.union group (KM.singleton (K.fromText member) value)

-- | Whether @group.member@ is set.
hasMember :: Text -> Text -> Groups -> Bool
hasMember group member groups =
  maybe False (KM.member (K.fromText member)) (Map.lookup group groups)

-- | Accumulator of the first pass: root members, groups, explicit grouped
-- objects, the legacy provider, and the legacy WebSocket flag.
data Acc = Acc
  { accRoot      :: KM.KeyMap Value
  , accGroups    :: Groups
  , accExplicit  :: [(Text, KM.KeyMap Value)]
  , accProvider  :: Maybe (KM.KeyMap Value)
  , accWebSocket :: Maybe Value
  }

-- | Convert a flat @session.create@ \/ @session.resume@ payload (first
-- argument: whether it is a resume) into the grouped SDK protocol 4 payload.
--
-- Root keys stay at the top level, known flat keys move to their group
-- member, and unknown keys stay at the root unchanged. Null values are
-- skipped (except an explicit null @defaultProvider@ on resume, which clears
-- it) and empty groups are not emitted. Members of an already-grouped object
-- win over the equivalent flat keys. Create-only members are dropped on
-- resume and resume-only members on create. A legacy single @provider@
-- becomes the named @default@ provider unless explicit providers are given,
-- and the legacy @enableWebSocketResponses@ flag moves into @github.capi@.
groupSessionParams :: Bool -> Value -> Value
groupSessionParams isResume (Object flat) =
  let firstPass = foldl' step (Acc KM.empty Map.empty [] Nothing Nothing) (KM.toList flat)

      step acc (k, v)
        | v == Null && not (isResume && key == "defaultProvider") = acc
        | key `elem` sessionGroupNames, Object members <- v =
            acc { accExplicit = (key, members) : accExplicit acc }
        | key == "configDir" =
            if KM.member "configDirectory" flat then acc
            else acc { accRoot = KM.insert "configDirectory" v (accRoot acc) }
        | key == "disableResume" =
            if KM.member "suppressResumeEvent" flat then acc
            else acc { accRoot = KM.insert "suppressResumeEvent" v (accRoot acc) }
        | key == "provider" = case v of
            Object p -> acc { accProvider = Just p }
            _        -> acc
        | key == "enableWebSocketResponses" = acc { accWebSocket = Just v }
        | key `elem` sessionRootKeys = acc { accRoot = KM.insert k v (accRoot acc) }
        | isResume && key `elem` createOnlyGroupedKeys = acc
        | not isResume && key `elem` resumeOnlyGroupedKeys = acc
        | otherwise = case Map.lookup key sessionGroupedKeys of
            Just (group, member) -> acc { accGroups = putMember group member v (accGroups acc) }
            Nothing              -> acc { accRoot = KM.insert k v (accRoot acc) }
        where key = K.toText k

      mergeExplicit groups (group, members) = foldl' (mergeMember group) groups (KM.toList members)
      mergeMember group groups (mk, mv)
        | mv == Null && not (isResume && group == "inference" && K.toText mk == "defaultProvider") = groups
        | otherwise = putMember group (K.toText mk) mv groups

      groupsWithExplicit = foldl' mergeExplicit (accGroups firstPass) (reverse (accExplicit firstPass))

      groupsWithProvider = case accProvider firstPass of
        Just provider
          | not (hasMember "inference" "providers" groupsWithExplicit)
          , not (hasMember "inference" "defaultProvider" groupsWithExplicit)
          , not (hasMember "github" "capi" groupsWithExplicit) ->
              translateLegacyProvider (KM.lookup "model" flat) provider groupsWithExplicit
        _ -> groupsWithExplicit

      groupsWithWebSocket = case accWebSocket firstPass of
        Nothing -> groupsWithProvider
        Just flag ->
          let capi = case Map.lookup "github" groupsWithProvider >>= KM.lookup "capi" of
                Just (Object c) -> c
                _               -> KM.empty
          in if KM.member "enableWebSocketResponses" capi
               then groupsWithProvider
               else putMember "github" "capi"
                      (Object (KM.insert "enableWebSocketResponses" flag capi)) groupsWithProvider

      groupsFinal
        | hasMember "mcp" "servers" groupsWithWebSocket
        , not (hasMember "mcp" "envValueMode" groupsWithWebSocket) =
            putMember "mcp" "envValueMode" (String "direct") groupsWithWebSocket
        | otherwise = groupsWithWebSocket

      addGroup root (group, members)
        | KM.null members = root
        | otherwise = KM.insert (K.fromText group) (Object members) root
  in Object (foldl' addGroup (accRoot firstPass) (Map.toList groupsFinal))
groupSessionParams _ other = other

-- | v1 BYOK: a single provider becomes the named @default@ connection with
-- Copilot models disabled, plus a model definition for model-specific fields
-- when the session model is known.
translateLegacyProvider :: Maybe Value -> KM.KeyMap Value -> Groups -> Groups
translateLegacyProvider mModel provider groups =
  let fields = [ (K.toText k, v) | (k, v) <- KM.toList provider, v /= Null, K.toText k /= "name" ]
      connection = object $ ("name" .= ("default" :: Text))
        : [ K.fromText k .= v | (k, v) <- fields, k `notElem` legacyProviderModelFields ]
      modelFields = [ (k, v) | (k, v) <- fields, k `elem` legacyProviderModelFields ]
      withConnection =
          putMember "github" "capi" (object [ "enabled" .= False ])
        $ putMember "inference" "defaultProvider" (String "default")
        $ putMember "inference" "providers" (toJSON [connection]) groups
  in case mModel of
       Just (String model)
         | not (null modelFields)
         , not (T.null model)
         , not (hasMember "inference" "models" withConnection) ->
             let entry = object $ [ "provider" .= ("default" :: Text), "id" .= model ]
                   ++ [ K.fromText k .= v | (k, v) <- modelFields ]
             in putMember "inference" "models" (toJSON [entry]) withConnection
       _ -> withConnection

-- | Fill the conservative @empty@ client mode defaults into a grouped payload
-- without overriding values the caller supplied.
applyEmptyModeDefaults :: Value -> Value
applyEmptyModeDefaults (Object payload) = Object (foldl' setDefault payload defaults)
  where
    defaults :: [(Text, Text, Value)]
    defaults =
      [ ("experiments", "enableExperimentalMode", Bool False)
      , ("github", "telemetryEnabled", Bool False)
      , ("mcp", "oauthTokenStorage", String "in-memory")
      , ("state", "skipEmbeddingRetrieval", Bool True)
      , ("state", "embeddingCacheStorage", String "in-memory")
      , ("state", "enableCrossSessionStore", Bool False)
      , ("state", "memory", object [ "enabled" .= False ])
      , ("instructions", "onDemandDiscovery", Bool False)
      , ("hooks", "enableRepositoryHooks", Bool False)
      , ("git", "enableHostGitOperations", Bool False)
      ]
    setDefault acc (group, member, value) =
      let groupKey  = K.fromText group
          memberKey = K.fromText member
          members   = case KM.lookup groupKey acc of
            Just (Object m) -> m
            _               -> KM.empty
      in if KM.member memberKey members
           then acc
           else KM.insert groupKey (Object (KM.insert memberKey value members)) acc
applyEmptyModeDefaults other = other
