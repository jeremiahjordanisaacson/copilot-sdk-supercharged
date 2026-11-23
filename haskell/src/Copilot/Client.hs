---------------------------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
---------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The Copilot Client module.
--
-- Manages the connection to the Copilot CLI server. Spawns the CLI process,
-- communicates via JSON-RPC 2.0 over stdio, and provides session management.
module Copilot.Client
  ( -- * Client
    CopilotClient
  , newCopilotClient
  , startClient
  , stopClient
  , forceStopClient

    -- * Errors
  , CopilotClientError (..)
  , policyHelperFailure

    -- * Session management
  , createSession
  , resumeSession
  , deleteSession
  , listSessions
  , getLastSessionId
  , getSessionMetadata
  , getForegroundSessionId
  , setForegroundSessionId
  , setSessionFsProvider

    -- * Queries
  , ping
  , getStatus
  , getAuthStatus
  , listModels
  , getClientState

    -- * Managed settings
  , clearManagedSettingsCache

    -- * Lifecycle events
  , onLifecycleEvent
  ) where

import           Control.Concurrent        (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Exception         (SomeException, catch, try, throwIO, Exception, fromException)
import           Control.Monad             (when, void, forM_)
import           Data.Aeson                (Value (..), object, (.=), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (parseMaybe)
import           Data.IORef
import           Data.List                 (isSuffixOf)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromMaybe, isJust, isNothing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.UUID                 as UUID
import           Data.UUID.V4              (nextRandom)
import           System.Exit               (ExitCode)
import           System.Process            (CreateProcess (..), StdStream (..), ProcessHandle,
                                            createProcess, proc, terminateProcess,
                                            waitForProcess)

import           Copilot.JsonRpc           (JsonRpcClient, JsonRpcError (..),
                                            newJsonRpcClient,
                                            sendRequest, setRequestHandler)
import qualified Copilot.JsonRpc           as JR
import           Copilot.SdkProtocolVersion (sdkProtocolVersion)
import           Copilot.Session           (CopilotSession, newCopilotSession, dispatchSessionEvent,
                                            registerSessionTools, registerSessionPermissionHandler,
                                            registerSessionUserInputHandler, registerSessionHooks,
                                            getSessionToolHandler, handleSessionPermissionRequest,
                                            handleSessionUserInputRequest, handleSessionHooksInvoke,
                                            registerExitPlanModeHandler, handleExitPlanModeRequest,
                                            registerTraceContextProvider,
                                            registerSessionShellAuthenticationHandler,
                                            handleSessionShellAuthenticationRequest,
                                            registerSessionStoreMemoryHandler,
                                            handleSessionStoreMemory,
                                            addSessionCloseCallback, runSessionCloseCallbacks,
                                            destroySession)
import           Copilot.SessionParams     (applyEmptyModeDefaults, groupSessionParams)
import           Copilot.Types

-- ============================================================================
-- Client Errors
-- ============================================================================

-- | Errors raised by the client.
data CopilotClientError
  = ClientNotConnected
  | ProtocolVersionMismatch Int (Maybe Int)
  | SessionNotFound Text
  | ServerError Text
  | ClientError Text
  | RequestFailed Text JsonRpcError
    -- ^ The named request (for example @session.create@) failed with the
    -- server's JSON-RPC error; see 'policyHelperFailure'
  deriving (Show)

instance Exception CopilotClientError

-- | The Policy Helper failure carried in the error data of a failed
-- @session.create@ or @session.resume@ ('RequestFailed'), if any.
policyHelperFailure :: CopilotClientError -> Maybe PolicyHelperFailure
policyHelperFailure (RequestFailed _ err) = policyHelperFailureFromErrorData (jreData err)
policyHelperFailure _                     = Nothing

-- ============================================================================
-- Client State
-- ============================================================================

-- | The Copilot CLI client.
data CopilotClient = CopilotClient
  { ccOptions           :: !CopilotClientOptions
  , ccState             :: !(IORef ConnectionState)
  , ccProcess           :: !(IORef (Maybe ProcessHandle))
  , ccJsonRpc           :: !(IORef (Maybe JsonRpcClient))
  , ccSessions          :: !(MVar (Map.Map Text CopilotSession))
  , ccModelsCache       :: !(MVar (Maybe [ModelInfo]))
  , ccLifecycleHandlers :: !(IORef [SessionLifecycleHandler])
  , ccTokenProviders    :: !(MVar (Map.Map Text GitHubTokenProvider))  -- ^ By registration ID
  }

-- | Create a new 'CopilotClient' with the given options.
newCopilotClient :: CopilotClientOptions -> IO CopilotClient
newCopilotClient opts = do
  stateRef     <- newIORef Disconnected
  processRef   <- newIORef Nothing
  jsonRpcRef   <- newIORef Nothing
  sessionsVar  <- newMVar Map.empty
  modelsCacheVar <- newMVar Nothing
  handlersRef  <- newIORef []
  tokenProvidersVar <- newMVar Map.empty
  pure CopilotClient
    { ccOptions           = opts
    , ccState             = stateRef
    , ccProcess           = processRef
    , ccJsonRpc           = jsonRpcRef
    , ccSessions          = sessionsVar
    , ccModelsCache       = modelsCacheVar
    , ccLifecycleHandlers = handlersRef
    , ccTokenProviders    = tokenProvidersVar
    }

-- | Get the current connection state.
getClientState :: CopilotClient -> IO ConnectionState
getClientState = readIORef . ccState

-- ============================================================================
-- Start / Stop
-- ============================================================================

-- | Start the CLI server and establish a connection.
--
-- Spawns the Copilot CLI process with @--headless --no-auto-update --log-level info --stdio@
-- (a @.js@ CLI path runs under @node@), creates a JSON-RPC client over the
-- process stdio pipes, and verifies the protocol version with the @connect@
-- handshake (falling back to @ping@ on servers without it). When network
-- callbacks are configured they are registered with @network.setProvider@.
startClient :: CopilotClient -> IO ()
startClient client = do
  st <- readIORef (ccState client)
  when (st == Connected) $ pure ()

  writeIORef (ccState client) Connecting

  -- Determine CLI path; JavaScript entry points (npm-loader.js, index.js) run under node
  let cliPath = case ccoCliPath (ccOptions client) of
        Just p  -> p
        Nothing -> "copilot"
      (cliCommand, cliPrefixArgs)
        | ".js" `isSuffixOf` cliPath = ("node", [cliPath])
        | otherwise                  = (cliPath, [])

  -- Build args
  let baseArgs = ccoCliArgs (ccOptions client)
               ++ [ "--headless"
                  , "--no-auto-update"
                  , "--log-level"
                  , T.unpack (ccoLogLevel (ccOptions client))
                  , "--stdio"
                  ]

  -- Auth flags
  let authArgs = case ccoGithubToken (ccOptions client) of
        Just _ -> ["--auth-token-env", "COPILOT_SDK_AUTH_TOKEN"]
        Nothing -> []

  let loginArgs = case ccoUseLoggedInUser (ccOptions client) of
        Just False -> ["--no-auto-login"]
        Nothing | isJust (ccoGithubToken (ccOptions client)) -> ["--no-auto-login"]
        _ -> []

  let allArgs = baseArgs ++ authArgs ++ loginArgs

  -- Build environment
  let envSetup = case (ccoEnv (ccOptions client), ccoGithubToken (ccOptions client)) of
        (Just env, Just tok) -> Just $ ("COPILOT_SDK_AUTH_TOKEN", T.unpack tok) : env
        (Just env, Nothing)  -> Just env
        (Nothing, Just tok)  -> Just [("COPILOT_SDK_AUTH_TOKEN", T.unpack tok)]
        (Nothing, Nothing)   -> Nothing

  let cpSpec = (proc cliCommand (cliPrefixArgs ++ allArgs))
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , cwd     = ccoCwd (ccOptions client)
        , env     = envSetup
        }

  result <- try $ createProcess cpSpec
  case result of
    Left (e :: SomeException) -> do
      writeIORef (ccState client) ConnectionError
      throwIO $ ClientError $ "Failed to start CLI server: " <> T.pack (show e)
    Right (Just hIn, Just hOut, Just hErr, ph) -> do
      writeIORef (ccProcess client) (Just ph)

      -- Create JSON-RPC client
      rpcClient <- newJsonRpcClient hIn hOut
      writeIORef (ccJsonRpc client) (Just rpcClient)

      -- Setup handlers
      setupHandlers client rpcClient

      -- Start the reader
      JR.startClient rpcClient

      -- Handshake, verify the protocol version, and register network callbacks
      verifyResult <- try $ do
        verifyProtocolVersion client rpcClient
        registerNetworkProvider client rpcClient
      case verifyResult of
        Left (e :: SomeException) -> do
          writeIORef (ccState client) ConnectionError
          case fromException e of
            Just (clientErr :: CopilotClientError) -> throwIO clientErr
            Nothing -> throwIO $ ClientError $ T.pack (show e)
        Right () -> do
          writeIORef (ccState client) Connected

    _ -> do
      writeIORef (ccState client) ConnectionError
      throwIO $ ClientError "Failed to create process pipes"

-- | Stop the client gracefully: destroy sessions, close connection, kill process.
stopClient :: CopilotClient -> IO [Text]
stopClient client = do
  errors <- newIORef ([] :: [Text])

  -- Destroy all sessions
  sessions <- readMVar (ccSessions client)
  forM_ (Map.toList sessions) $ \(sid, session) -> do
    result <- try $ destroySession session
    case result of
      Left (e :: SomeException) ->
        modifyIORef errors (("Failed to destroy session " <> sid <> ": " <> T.pack (show e)) :)
      Right () -> pure ()

  modifyMVar_ (ccSessions client) $ \_ -> pure Map.empty
  modifyMVar_ (ccTokenProviders client) $ \_ -> pure Map.empty

  -- Stop JSON-RPC client
  mRpc <- readIORef (ccJsonRpc client)
  case mRpc of
    Just rpc -> do
      result <- try $ JR.stopClient rpc
      case result of
        Left (e :: SomeException) ->
          modifyIORef errors (("Failed to stop JSON-RPC client: " <> T.pack (show e)) :)
        Right () -> pure ()
    Nothing -> pure ()
  writeIORef (ccJsonRpc client) Nothing

  -- Clear models cache
  modifyMVar_ (ccModelsCache client) $ \_ -> pure Nothing

  -- Kill CLI process
  mPh <- readIORef (ccProcess client)
  case mPh of
    Just ph -> do
      result <- try $ terminateProcess ph
      case result of
        Left (e :: SomeException) ->
          modifyIORef errors (("Failed to kill CLI process: " <> T.pack (show e)) :)
        Right () -> do
          _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
          pure ()
    Nothing -> pure ()
  writeIORef (ccProcess client) Nothing

  writeIORef (ccState client) Disconnected
  readIORef errors

-- | Forcefully stop the client without graceful cleanup.
forceStopClient :: CopilotClient -> IO ()
forceStopClient client = do
  modifyMVar_ (ccSessions client) $ \_ -> pure Map.empty
  modifyMVar_ (ccTokenProviders client) $ \_ -> pure Map.empty

  mRpc <- readIORef (ccJsonRpc client)
  case mRpc of
    Just rpc -> void (try $ JR.stopClient rpc :: IO (Either SomeException ()))
    Nothing  -> pure ()
  writeIORef (ccJsonRpc client) Nothing

  modifyMVar_ (ccModelsCache client) $ \_ -> pure Nothing

  mPh <- readIORef (ccProcess client)
  case mPh of
    Just ph -> void (try $ terminateProcess ph :: IO (Either SomeException ()))
    Nothing -> pure ()
  writeIORef (ccProcess client) Nothing

  writeIORef (ccState client) Disconnected

-- ============================================================================
-- Session Management
-- ============================================================================

-- | Create a new conversation session.
--
-- The options are sent in the grouped SDK protocol 4 layout (see
-- 'groupSessionParams'). In 'ClientModeEmpty', 'scAvailableTools' is
-- required and conservative defaults apply beneath the caller's options.
createSession :: CopilotClient -> SessionConfig -> IO CopilotSession
createSession client config = do
  rpc <- ensureConnected client
  let mode = ccoMode (ccOptions client)
  requireAvailableTools mode (scAvailableTools config)
  requireSingleTokenSource (scGitHubToken config) (scGitHubTokenProvider config)

  -- Register the GitHub token provider before the request so the runtime can
  -- call back while the session is being created.
  mRegistration <- registerTokenProvider client (scGitHubTokenProvider config)

  -- Build the create request
  let toolsJson = case scTools config of
        [] -> Nothing
        ts -> Just $ map toolToJson ts

  let flatParams = object $ filter ((/= Null) . snd)
        [ "model"              .= scModel config
        , "sessionId"          .= scSessionId config
        , "reasoningEffort"    .= scReasoningEffort config
        , "configDir"          .= scConfigDir config
        , "tools"              .= toolsJson
        , "systemMessage"      .= scSystemMessage config
        , "availableTools"     .= scAvailableTools config
        , "excludedTools"      .= scExcludedTools config
        , "provider"           .= scProvider config
        , "requestPermission"  .= (isJust (scOnPermissionRequest config))
        , "requestUserInput"   .= (isJust (scOnUserInputRequest config))
        , "hooks"              .= hasHooks (scHooks config)
        , "workingDirectory"   .= scWorkingDirectory config
        , "streaming"          .= scStreaming config
        , "includeSubAgentStreamingEvents" .= scIncludeSubAgentStreamingEvents config
        , "mcpServers"         .= scMcpServers config
        , "customAgents"       .= scCustomAgents config
        , "skillDirectories"   .= scSkillDirectories config
        , "disabledSkills"     .= scDisabledSkills config
        , "infiniteSessions"   .= scInfiniteSessions config
        , "modelCapabilities"  .= scModelCapabilities config
        , "enableConfigDiscovery" .= scEnableConfigDiscovery config
        , "gitHubToken"        .= scGitHubToken config
        , "instructionDirectories" .= scInstructionDirectories config
        -- Upstream-sync session options (parity with @github/copilot-sdk)
        , "enableCitations"          .= scEnableCitations config
        , "excludedBuiltinAgents"    .= scExcludedBuiltinAgents config
        , "sessionLimits"            .= scSessionLimits config
        , "memory"                   .= scMemory config
        , "otlpProtocol"             .= scOtlpProtocol config
        , "enableWebSocketResponses" .= scEnableWebSocketResponses config
        , "expAssignments"           .= scExpAssignments config
        , "mcpAuthHandler"           .= (if isJust (scOnMcpAuthRequest config) then Just True else Nothing)
        -- Upstream-sync 2026-08 session options (parity with @github/copilot-sdk)
        , "rewindEnabled"            .= scRewindEnabled config
        , "additionalDirectories"    .= scAdditionalDirectories config
        , "disabledMcpServers"       .= scDisabledMcpServers config
        , "githubMcpToolConfig"      .= scGithubMcpToolConfig config
        , "canvasProvider"           .= (canvasProviderToWire <$> scCanvasProvider config)
        , "customAgentsLocalOnly"    .= scCustomAgentsLocalOnly config
        , "toolSearch"               .= (toolSearchToWire <$> scToolSearch config)
        , "enableExperimentalMode"   .= scExperimentalMode config
        , "contentExclusion"         .= scContentExclusion config
        -- v2.6.0 / v3.0.0 session options (SDK protocol 4)
        , "askUserVariant"           .= scAskUserVariant config
        , "gitHubTokenProviderRegistrationId" .= mRegistration
        , "sandboxConfig"            .= scSandbox config
        , "featureFlags"             .= scFeatureFlags config
        , "allowedModels"            .= scAllowedModels config
        , "authClientIdMetadataUrl"  .= scAuthClientIdMetadataUrl config
        , "agentDirectories"         .= scAgentDirectories config
        , "hookDirectories"          .= scHookDirectories config
        , "trustWorkingDirectory"    .= scTrustWorkingDirectory config
        , "imageGeneration"          .= scImageGeneration config
        , "autopilot"                .= scAutopilot config
        , "providers"                .= scProviders config
        , "models"                   .= scModels config
        , "defaultProvider"          .= scDefaultProvider config
        , "capi"                     .= scCapi config
        , "requestShellAuthentication" .= whenSet (scOnShellAuthenticationRequest config)
        , "hasMemoryStore"           .= whenSet (scOnStoreMemory config)
        ]
      reqParams = finalizeSessionParams mode False flatParams

  result <- sendRequest rpc "session.create" reqParams
  case result of
    Left err -> do
      unregisterTokenProvider client mRegistration
      throwIO $ RequestFailed "session.create" err
    Right val -> do
      let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") val :: Maybe Text
          mWorkspacePath = parseMaybe (withObject "" $ \o -> o .:? "workspacePath") val :: Maybe (Maybe Text)
      case mSessionId of
        Nothing -> do
          unregisterTokenProvider client mRegistration
          throwIO $ ServerError "No sessionId in create response"
        Just sid -> do
          let wsp = case mWorkspacePath of
                      Just wp -> wp
                      Nothing -> Nothing
          session <- newCopilotSession sid rpc wsp
          registerSessionTools session (scTools config)
          case scOnPermissionRequest config of
            Just h  -> registerSessionPermissionHandler session h
            Nothing -> pure ()
          case scOnUserInputRequest config of
            Just h  -> registerSessionUserInputHandler session h
            Nothing -> pure ()
          case scHooks config of
            Just h  -> registerSessionHooks session h
            Nothing -> pure ()
          case scOnShellAuthenticationRequest config of
            Just h  -> registerSessionShellAuthenticationHandler session h
            Nothing -> pure ()
          case scOnStoreMemory config of
            Just h  -> registerSessionStoreMemoryHandler session h
            Nothing -> pure ()
          case ccoOnGetTraceContext (ccOptions client) of
            Just p  -> registerTraceContextProvider session p
            Nothing -> pure ()
          forM_ mRegistration $ \regId ->
            addSessionCloseCallback session (unregisterTokenProvider client (Just regId))
          modifyMVar_ (ccSessions client) $ \m -> pure (Map.insert sid session m)
          pure session

-- | Resume an existing session.
--
-- Like 'createSession', the options are sent in the grouped SDK protocol 4
-- layout; 'rscDisableResume' becomes @suppressResumeEvent@ and
-- 'rscClearDefaultProvider' sends an explicit null default provider.
resumeSession :: CopilotClient -> Text -> ResumeSessionConfig -> IO CopilotSession
resumeSession client sessionId config = do
  rpc <- ensureConnected client
  let mode = ccoMode (ccOptions client)
  requireAvailableTools mode (rscAvailableTools config)
  requireSingleTokenSource (rscGitHubToken config) (rscGitHubTokenProvider config)

  mRegistration <- registerTokenProvider client (rscGitHubTokenProvider config)

  let toolsJson = case rscTools config of
        [] -> Nothing
        ts -> Just $ map toolToJson ts

  let flatPairs = filter ((/= Null) . snd)
        [ "sessionId"          .= sessionId
        , "model"              .= rscModel config
        , "reasoningEffort"    .= rscReasoningEffort config
        , "tools"              .= toolsJson
        , "systemMessage"      .= rscSystemMessage config
        , "availableTools"     .= rscAvailableTools config
        , "excludedTools"      .= rscExcludedTools config
        , "provider"           .= rscProvider config
        , "requestPermission"  .= (isJust (rscOnPermissionRequest config))
        , "requestUserInput"   .= (isJust (rscOnUserInputRequest config))
        , "hooks"              .= hasHooks (rscHooks config)
        , "workingDirectory"   .= rscWorkingDirectory config
        , "configDir"          .= rscConfigDir config
        , "streaming"          .= rscStreaming config
        , "includeSubAgentStreamingEvents" .= rscIncludeSubAgentStreamingEvents config
        , "mcpServers"         .= rscMcpServers config
        , "customAgents"       .= rscCustomAgents config
        , "skillDirectories"   .= rscSkillDirectories config
        , "disabledSkills"     .= rscDisabledSkills config
        , "infiniteSessions"   .= rscInfiniteSessions config
        , "modelCapabilities"  .= rscModelCapabilities config
        , "enableConfigDiscovery" .= rscEnableConfigDiscovery config
        , "gitHubToken"        .= rscGitHubToken config
        , "instructionDirectories" .= rscInstructionDirectories config
        , "disableResume"      .= rscDisableResume config
        -- v2.6.0 / v3.0.0 resume options (SDK protocol 4)
        , "allowTranscriptRecovery"  .= rscAllowTranscriptRecovery config
        , "askUserVariant"           .= rscAskUserVariant config
        , "gitHubTokenProviderRegistrationId" .= mRegistration
        , "sandboxConfig"            .= rscSandbox config
        , "featureFlags"             .= rscFeatureFlags config
        , "allowedModels"            .= rscAllowedModels config
        , "authClientIdMetadataUrl"  .= rscAuthClientIdMetadataUrl config
        , "agentDirectories"         .= rscAgentDirectories config
        , "hookDirectories"          .= rscHookDirectories config
        , "imageGeneration"          .= rscImageGeneration config
        , "autopilot"                .= rscAutopilot config
        , "providers"                .= rscProviders config
        , "models"                   .= rscModels config
        , "defaultProvider"          .= rscDefaultProvider config
        , "capi"                     .= rscCapi config
        , "requestShellAuthentication" .= whenSet (rscOnShellAuthenticationRequest config)
        , "hasMemoryStore"           .= whenSet (rscOnStoreMemory config)
        ]
      -- An explicit null default provider clears the one stored with the session.
      clearDefault = [ "defaultProvider" .= Null
                     | rscClearDefaultProvider config && isNothing (rscDefaultProvider config) ]
      reqParams = finalizeSessionParams mode True (object (flatPairs ++ clearDefault))

  result <- sendRequest rpc "session.resume" reqParams
  case result of
    Left err -> do
      unregisterTokenProvider client mRegistration
      throwIO $ RequestFailed "session.resume" err
    Right val -> do
      let mSid = parseMaybe (withObject "" $ \o -> o .: "sessionId") val :: Maybe Text
          mWsp = parseMaybe (withObject "" $ \o -> o .:? "workspacePath") val :: Maybe (Maybe Text)
      case mSid of
        Nothing -> do
          unregisterTokenProvider client mRegistration
          throwIO $ ServerError "No sessionId in resume response"
        Just sid -> do
          let wsp = case mWsp of
                      Just wp -> wp
                      Nothing -> Nothing
          session <- newCopilotSession sid rpc wsp
          registerSessionTools session (rscTools config)
          case rscOnPermissionRequest config of
            Just h  -> registerSessionPermissionHandler session h
            Nothing -> pure ()
          case rscOnUserInputRequest config of
            Just h  -> registerSessionUserInputHandler session h
            Nothing -> pure ()
          case rscHooks config of
            Just h  -> registerSessionHooks session h
            Nothing -> pure ()
          case rscOnShellAuthenticationRequest config of
            Just h  -> registerSessionShellAuthenticationHandler session h
            Nothing -> pure ()
          case rscOnStoreMemory config of
            Just h  -> registerSessionStoreMemoryHandler session h
            Nothing -> pure ()
          case ccoOnGetTraceContext (ccOptions client) of
            Just p  -> registerTraceContextProvider session p
            Nothing -> pure ()
          forM_ mRegistration $ \regId ->
            addSessionCloseCallback session (unregisterTokenProvider client (Just regId))
          modifyMVar_ (ccSessions client) $ \m -> pure (Map.insert sid session m)
          pure session

-- | Delete a session permanently.
deleteSession :: CopilotClient -> Text -> IO ()
deleteSession client sessionId = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.delete" (object [ "sessionId" .= sessionId ])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> do
      let mSuccess = parseMaybe (withObject "" $ \o -> o .: "success") val :: Maybe Bool
          mError = parseMaybe (withObject "" $ \o -> o .:? "error") val :: Maybe (Maybe Text)
      case mSuccess of
        Just True -> do
          mSession <- Map.lookup sessionId <$> readMVar (ccSessions client)
          modifyMVar_ (ccSessions client) $ \m -> pure (Map.delete sessionId m)
          forM_ mSession runSessionCloseCallbacks
        _ -> do
          let errMsg = case mError of
                Just (Just e) -> e
                _             -> "Unknown error"
          throwIO $ ServerError $ "Failed to delete session " <> sessionId <> ": " <> errMsg

-- | List all available sessions.
listSessions :: CopilotClient -> IO [SessionMetadata]
listSessions client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.list" (object [])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> do
      let mSessions = parseMaybe (withObject "" $ \o -> o .: "sessions") val :: Maybe [SessionMetadata]
      case mSessions of
        Nothing -> pure []
        Just ss -> pure ss

-- | Get the ID of the most recently updated session.
getLastSessionId :: CopilotClient -> IO (Maybe Text)
getLastSessionId client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.getLastId" (object [])
  case result of
    Left _ -> pure Nothing
    Right val -> pure $ parseMaybe (withObject "" $ \o -> o .: "sessionId") val

-- | Get metadata for a session by ID.
getSessionMetadata :: CopilotClient -> Text -> IO (Maybe Value)
getSessionMetadata client sessionId = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.getMetadata" (object [ "sessionId" .= sessionId ])
  case result of
    Left _ -> pure Nothing
    Right val -> pure (Just val)

-- | Get the foreground session ID.
getForegroundSessionId :: CopilotClient -> IO (Maybe Text)
getForegroundSessionId client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.getForeground" (object [])
  case result of
    Left _ -> pure Nothing
    Right val -> pure $ parseMaybe (withObject "" $ \o -> o .: "sessionId") val

-- | Set the foreground session ID.
setForegroundSessionId :: CopilotClient -> Text -> IO ()
setForegroundSessionId client sessionId = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "session.setForeground" (object [ "sessionId" .= sessionId ])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> do
      let mSuccess = parseMaybe (withObject "" $ \o -> o .: "success") val :: Maybe Bool
          mError = parseMaybe (withObject "" $ \o -> o .:? "error") val :: Maybe (Maybe Text)
      case mSuccess of
        Just True -> pure ()
        _ -> do
          let errMsg = case mError of
                Just (Just e) -> e
                _             -> "Unknown error"
          throwIO $ ServerError $ "Failed to set foreground session: " <> errMsg

-- | Set the session filesystem provider configuration.
setSessionFsProvider :: CopilotClient -> SessionFsConfig -> IO ()
setSessionFsProvider client config = do
  rpc <- ensureConnected client
  let params = Aeson.toJSON config
  result <- sendRequest rpc "sessionFs.setProvider" params
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right _  -> pure ()

-- ============================================================================
-- Queries
-- ============================================================================

-- | Send a ping to verify connectivity.
ping :: CopilotClient -> Maybe Text -> IO PingResponse
ping client mMessage = do
  rpc <- ensureConnected client
  let params = object $ maybe [] (\m -> ["message" .= m]) mMessage
  result <- sendRequest rpc "ping" params
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> case Aeson.fromJSON val of
      Aeson.Success pr -> pure pr
      Aeson.Error e    -> throwIO $ ServerError $ T.pack e

-- | Get CLI status.
getStatus :: CopilotClient -> IO GetStatusResponse
getStatus client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "status.get" (object [])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> case Aeson.fromJSON val of
      Aeson.Success r -> pure r
      Aeson.Error e   -> throwIO $ ServerError $ T.pack e

-- | Get authentication status.
getAuthStatus :: CopilotClient -> IO GetAuthStatusResponse
getAuthStatus client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "auth.getStatus" (object [])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right val -> case Aeson.fromJSON val of
      Aeson.Success r -> pure r
      Aeson.Error e   -> throwIO $ ServerError $ T.pack e

-- | List available models (cached after first call).
listModels :: CopilotClient -> IO [ModelInfo]
listModels client = do
  rpc <- ensureConnected client
  modifyMVar_ (ccModelsCache client) $ \mCache ->
    case mCache of
      Just models -> pure (Just models)
      Nothing -> do
        result <- sendRequest rpc "models.list" (object [])
        case result of
          Left err -> throwIO $ ServerError (jreMessage err)
          Right val -> do
            let mModels = parseMaybe (withObject "" $ \o -> o .: "models") val :: Maybe [ModelInfo]
            case mModels of
              Just models -> pure (Just models)
              Nothing     -> pure (Just [])
  cached <- readMVar (ccModelsCache client)
  pure $ maybe [] id cached

-- | Clear the runtime's cached managed settings (@managedSettings.clearCache@)
-- so they are fetched again.
clearManagedSettingsCache :: CopilotClient -> IO ()
clearManagedSettingsCache client = do
  rpc <- ensureConnected client
  result <- sendRequest rpc "managedSettings.clearCache" (object [])
  case result of
    Left err -> throwIO $ ServerError (jreMessage err)
    Right _  -> pure ()

-- | Subscribe to session lifecycle events.
-- Returns an unsubscribe action.
--
-- Note: Haskell functions cannot be compared by equality. The returned
-- unsubscribe action removes the handler by dropping the head of the list
-- that was prepended. For robust unsubscription, only call unsubscribe once.
onLifecycleEvent :: CopilotClient -> SessionLifecycleHandler -> IO (IO ())
onLifecycleEvent client handler = do
  -- Use an IORef as a unique "alive" flag for this subscription
  aliveRef <- newIORef True
  let wrappedHandler evt = do
        alive <- readIORef aliveRef
        when alive $ handler evt
  modifyIORef (ccLifecycleHandlers client) (wrappedHandler :)
  pure $ writeIORef aliveRef False

-- ============================================================================
-- Internal
-- ============================================================================

hasHooks :: Maybe SessionHooks -> Bool
hasHooks Nothing = False
hasHooks (Just sh) = isJust (shOnPreToolUse sh)
                  || isJust (shOnPostToolUse sh)
                  || isJust (shOnPreMcpToolCall sh)
                  || isJust (shOnUserPromptSubmitted sh)
                  || isJust (shOnSessionStart sh)
                  || isJust (shOnSessionEnd sh)
                  || isJust (shOnErrorOccurred sh)
                  || isJust (shOnUserPromptTransformed sh)
                  || isJust (shOnSubagentStart sh)
                  || isJust (shOnSubagentStop sh)

-- | Convert a Tool to its JSON wire representation (without handler).
toolToJson :: Tool -> Value
toolToJson t = object $ filter ((/= Null) . snd)
  [ "name"        .= toolName t
  , "description" .= toolDescription t
  , "parameters"  .= toolParameters t
  , "idempotent"  .= toolIdempotent t
  ]

-- | @Just True@ when a callback is set, used for capability flags.
whenSet :: Maybe a -> Maybe Bool
whenSet m = if isJust m then Just True else Nothing

-- | Canvas provider identity: a bare string is the provider @id@.
canvasProviderToWire :: Value -> Value
canvasProviderToWire (String providerId) = object [ "id" .= providerId ]
canvasProviderToWire v                   = v

-- | Tool search configuration: a bare boolean toggles @enabled@.
toolSearchToWire :: Value -> Value
toolSearchToWire (Bool enabled) = object [ "enabled" .= enabled ]
toolSearchToWire v              = v

-- | Group a flat create\/resume payload for SDK protocol 4 and, in
-- 'ClientModeEmpty', fill in the empty-mode defaults.
finalizeSessionParams :: ClientMode -> Bool -> Value -> Value
finalizeSessionParams mode isResume flat =
  let grouped = groupSessionParams isResume flat
  in if mode == ClientModeEmpty then applyEmptyModeDefaults grouped else grouped

-- | 'ClientModeEmpty' requires an explicit tool allowlist on every session.
requireAvailableTools :: ClientMode -> Maybe [Text] -> IO ()
requireAvailableTools ClientModeEmpty Nothing =
  throwIO $ ClientError "Client mode \"empty\" requires availableTools on every session create and resume"
requireAvailableTools _ _ = pure ()

-- | A static GitHub token and a GitHub token provider are mutually exclusive.
requireSingleTokenSource :: Maybe Text -> Maybe GitHubTokenProvider -> IO ()
requireSingleTokenSource (Just _) (Just _) =
  throwIO $ ClientError "gitHubToken and gitHubTokenProvider are mutually exclusive"
requireSingleTokenSource _ _ = pure ()

-- | Register a GitHub token provider under a fresh registration ID.
registerTokenProvider :: CopilotClient -> Maybe GitHubTokenProvider -> IO (Maybe Text)
registerTokenProvider _ Nothing = pure Nothing
registerTokenProvider client (Just provider) = do
  registrationId <- UUID.toText <$> nextRandom
  modifyMVar_ (ccTokenProviders client) $ \m -> pure (Map.insert registrationId provider m)
  pure (Just registrationId)

-- | Drop a GitHub token provider registration.
unregisterTokenProvider :: CopilotClient -> Maybe Text -> IO ()
unregisterTokenProvider _ Nothing = pure ()
unregisterTokenProvider client (Just registrationId) =
  modifyMVar_ (ccTokenProviders client) $ \m -> pure (Map.delete registrationId m)

-- | Ensure the client is connected, auto-starting if configured.
ensureConnected :: CopilotClient -> IO JsonRpcClient
ensureConnected client = do
  mRpc <- readIORef (ccJsonRpc client)
  case mRpc of
    Just rpc -> pure rpc
    Nothing
      | ccoAutoStart (ccOptions client) -> do
          Copilot.Client.startClient client
          mRpc' <- readIORef (ccJsonRpc client)
          case mRpc' of
            Just rpc -> pure rpc
            Nothing  -> throwIO ClientNotConnected
      | otherwise -> throwIO ClientNotConnected

-- | Send the @connect@ handshake (with 'ccoClientInfo', when set) and verify
-- the server's protocol version. Falls back to @ping@ on servers that do not
-- implement @connect@.
verifyProtocolVersion :: CopilotClient -> JsonRpcClient -> IO ()
verifyProtocolVersion client rpc = do
  let connectParams = object $ catMaybes
        [ ("clientInfo" .=) <$> (ccoClientInfo (ccOptions client) >>= clientInfoToWire) ]
  connectResult <- sendRequest rpc "connect" connectParams
  response <- case connectResult of
    Right val -> pure val
    Left err
      | jreCode err == (-32601) || jreMessage err == "Unhandled method connect" -> do
          pingResult <- sendRequest rpc "ping" (object [])
          case pingResult of
            Right val   -> pure val
            Left pingErr -> throwIO $ ServerError (jreMessage pingErr)
      | otherwise -> throwIO $ RequestFailed "connect" err
  let serverVersion = parseMaybe (withObject "" $ \o -> o .:? "protocolVersion") response :: Maybe (Maybe Int)
  case maybe Nothing id serverVersion of
    Nothing -> throwIO $ ClientError $ protocolMismatchMessage Nothing
    Just sv
      | sv /= sdkProtocolVersion -> throwIO $ ClientError $ protocolMismatchMessage (Just sv)
      | otherwise -> pure ()

-- | The protocol version mismatch error text.
protocolMismatchMessage :: Maybe Int -> Text
protocolMismatchMessage mServer =
  "SDK protocol version mismatch: SDK supports version " <> T.pack (show sdkProtocolVersion)
    <> ", but server " <> maybe "does not report a protocol version"
                               (\v -> "reports version " <> T.pack (show v)) mServer
    <> ". Please update your SDK or server to ensure compatibility."

-- | Register the configured network callbacks with @network.setProvider@.
-- Each registered callback is marked with an empty object.
registerNetworkProvider :: CopilotClient -> JsonRpcClient -> IO ()
registerNetworkProvider client rpc =
  case ccoNetwork (ccOptions client) of
    Nothing -> pure ()
    Just net -> do
      let marker present name = if present then Just (name .= object []) else Nothing
          callbacks = catMaybes
            [ marker (isJust (noResolveProxy net)) "resolveProxy"
            , marker (isJust (noAuthenticateNegotiate net)) "authenticateNegotiate"
            , marker (isJust (noAuthenticateBasic net)) "authenticateBasic"
            ]
          params = object $ catMaybes [ ("scope" .=) <$> noScope net ] ++ callbacks
      when (not (null callbacks)) $ do
        result <- sendRequest rpc "network.setProvider" params
        case result of
          Left err -> throwIO $ RequestFailed "network.setProvider" err
          Right _  -> pure ()

-- | Set up JSON-RPC handlers for session events, tool calls, permissions, etc.
setupHandlers :: CopilotClient -> JsonRpcClient -> IO ()
setupHandlers client rpc = do
  -- session.event notification
  setRequestHandler rpc "session.event" $ \params -> do
    handleSessionEventNotification client params
    pure (Right Null)

  -- session.lifecycle notification
  setRequestHandler rpc "session.lifecycle" $ \params -> do
    handleLifecycleNotification client params
    pure (Right Null)

  -- tool.call request
  setRequestHandler rpc "tool.call" $ \params -> do
    handleToolCallReq client params

  -- permission.request
  setRequestHandler rpc "permission.request" $ \params -> do
    handlePermissionReq client params

  -- userInput.request
  setRequestHandler rpc "userInput.request" $ \params -> do
    handleUserInputReq client params

  -- hooks.invoke
  setRequestHandler rpc "hooks.invoke" $ \params -> do
    handleHooksInvokeReq client params

  -- exitPlanMode.request
  setRequestHandler rpc "exitPlanMode.request" $ \params -> do
    handleExitPlanModeReq client params

  -- gitHubToken.getToken (GitHub token providers)
  setRequestHandler rpc "gitHubToken.getToken" $ \params -> do
    handleGitHubTokenReq client params

  -- shellAuthentication.request / shellAuthentication.cancel
  setRequestHandler rpc "shellAuthentication.request" $ \params -> do
    handleShellAuthenticationReq client params
  -- A pending answer cannot be withdrawn once its callback runs; the runtime
  -- ignores answers to cancelled requests.
  setRequestHandler rpc "shellAuthentication.cancel" $ \_ ->
    pure (Right (object []))

  -- memoryStore.store
  setRequestHandler rpc "memoryStore.store" $ \params -> do
    handleMemoryStoreReq client params

  -- network.* callbacks
  setRequestHandler rpc "network.resolveProxy" $ \params -> do
    handleResolveProxyReq client params
  setRequestHandler rpc "network.authenticateNegotiate" $ \params -> do
    handleAuthenticateNegotiateReq client params
  setRequestHandler rpc "network.authenticateBasic" $ \params -> do
    handleAuthenticateBasicReq client params

-- | Dispatch a session.event notification to the appropriate session.
handleSessionEventNotification :: CopilotClient -> Value -> IO ()
handleSessionEventNotification client params = do
  let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") params :: Maybe Text
      mEvent     = parseMaybe (withObject "" $ \o -> o .: "event") params :: Maybe SessionEvent
  case (mSessionId, mEvent) of
    (Just sid, Just evt) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup sid sessions of
        Just session -> dispatchSessionEvent session evt
        Nothing      -> pure ()
    _ -> pure ()

-- | Dispatch a session.lifecycle notification.
handleLifecycleNotification :: CopilotClient -> Value -> IO ()
handleLifecycleNotification client params = do
  case Aeson.fromJSON params of
    Aeson.Success (evt :: SessionLifecycleEvent) -> do
      handlers <- readIORef (ccLifecycleHandlers client)
      mapM_ (\h -> catch (h evt) (\(_ :: SomeException) -> pure ())) handlers
    Aeson.Error _ -> pure ()

-- | Handle a tool.call request from the server.
handleToolCallReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleToolCallReq client params = do
  case Aeson.fromJSON params of
    Aeson.Error e -> pure $ Left $ JsonRpcError (-32602) (T.pack e) Nothing
    Aeson.Success (tcr :: ToolCallRequest) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup (tcrSessionId tcr) sessions of
        Nothing -> pure $ Left $ JsonRpcError (-32602) ("Unknown session: " <> tcrSessionId tcr) Nothing
        Just session -> do
          mHandler <- getSessionToolHandler session (tcrToolName tcr)
          case mHandler of
            Nothing -> do
              let result = ToolResultStruct $ ToolResultObject
                    { troTextResultForLlm    = "Tool '" <> tcrToolName tcr <> "' is not supported by this client instance."
                    , troBinaryResultsForLlm = Nothing
                    , troResultType          = Failure
                    , troError               = Just $ "tool '" <> tcrToolName tcr <> "' not supported"
                    , troSessionLog          = Nothing
                    , troToolTelemetry       = Just Map.empty
                    }
              pure $ Right $ Aeson.toJSON $ ToolCallResponse result
            Just handler -> do
              let invocation = ToolInvocation
                    { tiSessionId  = tcrSessionId tcr
                    , tiToolCallId = tcrToolCallId tcr
                    , tiToolName   = tcrToolName tcr
                    , tiArguments  = tcrArguments tcr
                    }
              result <- try $ handler (tcrArguments tcr) invocation
              case result of
                Right tr -> pure $ Right $ Aeson.toJSON $ ToolCallResponse tr
                Left (e :: SomeException) -> do
                  let failResult = ToolResultStruct $ ToolResultObject
                        { troTextResultForLlm    = "Invoking this tool produced an error. Detailed information is not available."
                        , troBinaryResultsForLlm = Nothing
                        , troResultType          = Failure
                        , troError               = Just (T.pack $ show e)
                        , troSessionLog          = Nothing
                        , troToolTelemetry       = Just Map.empty
                        }
                  pure $ Right $ Aeson.toJSON $ ToolCallResponse failResult

-- | Handle a permission.request from the server.
handlePermissionReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handlePermissionReq client params = do
  let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") params :: Maybe Text
      mPermReq   = parseMaybe (withObject "" $ \o -> o .: "permissionRequest") params
  case (mSessionId, mPermReq) of
    (Just sid, Just permReq) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup sid sessions of
        Nothing -> pure $ Left $ JsonRpcError (-32602) ("Session not found: " <> sid) Nothing
        Just session -> do
          result <- try $ handleSessionPermissionRequest session permReq sid
          case result of
            Right pr -> pure $ Right $ object [ "result" .= pr ]
            Left (_ :: SomeException) ->
              pure $ Right $ object
                [ "result" .= object [ "kind" .= ("denied-no-approval-rule-and-could-not-request-from-user" :: Text) ] ]
    _ -> pure $ Left $ JsonRpcError (-32602) "Invalid permission request payload" Nothing

-- | Handle a userInput.request from the server.
handleUserInputReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleUserInputReq client params = do
  let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") params :: Maybe Text
      mQuestion  = parseMaybe (withObject "" $ \o -> o .: "question") params :: Maybe Text
      mChoices   = parseMaybe (withObject "" $ \o -> o .:? "choices") params :: Maybe (Maybe [Text])
      mAllow     = parseMaybe (withObject "" $ \o -> o .:? "allowFreeform") params :: Maybe (Maybe Bool)
  case (mSessionId, mQuestion) of
    (Just sid, Just question) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup sid sessions of
        Nothing -> pure $ Left $ JsonRpcError (-32602) ("Session not found: " <> sid) Nothing
        Just session -> do
          let req = UserInputRequest
                { uirQuestion      = question
                , uirChoices       = case mChoices of Just c -> c; Nothing -> Nothing
                , uirAllowFreeform = case mAllow of Just a -> a; Nothing -> Nothing
                }
          result <- try $ handleSessionUserInputRequest session req sid
          case result of
            Right resp -> pure $ Right $ Aeson.toJSON resp
            Left (e :: SomeException) ->
              pure $ Left $ JsonRpcError (-32603) (T.pack $ show e) Nothing
    _ -> pure $ Left $ JsonRpcError (-32602) "Invalid user input request payload" Nothing

-- | Handle a hooks.invoke from the server.
handleHooksInvokeReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleHooksInvokeReq client params = do
  let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") params :: Maybe Text
      mHookType  = parseMaybe (withObject "" $ \o -> o .: "hookType") params :: Maybe Text
      mInput     = parseMaybe (withObject "" $ \o -> o .: "input") params :: Maybe Value
  case (mSessionId, mHookType) of
    (Just sid, Just hookType) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup sid sessions of
        Nothing -> pure $ Left $ JsonRpcError (-32602) ("Session not found: " <> sid) Nothing
        Just session -> do
          let input = case mInput of
                        Just i  -> i
                        Nothing -> Null
          result <- try $ handleSessionHooksInvoke session hookType input sid
          case result of
            Right mOutput -> case mOutput of
              Just output -> pure $ Right $ object [ "output" .= output ]
              Nothing     -> pure $ Right $ object []
            Left (e :: SomeException) ->
              pure $ Left $ JsonRpcError (-32603) (T.pack $ show e) Nothing
    _ -> pure $ Left $ JsonRpcError (-32602) "Invalid hooks invoke payload" Nothing

-- | Handle an exitPlanMode.request from the server.
handleExitPlanModeReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleExitPlanModeReq client params = do
  let mSessionId = parseMaybe (withObject "" $ \o -> o .: "sessionId") params :: Maybe Text
  case mSessionId of
    Nothing -> pure $ Right $ object [ "result" .= object [ "approved" .= True ] ]
    Just sid -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup sid sessions of
        Nothing -> pure $ Right $ object [ "result" .= object [ "approved" .= True ] ]
        Just session -> do
          result <- try $ handleExitPlanModeRequest session params
          case result of
            Right resp -> pure $ Right $ object [ "result" .= resp ]
            Left (_ :: SomeException) ->
              pure $ Right $ object [ "result" .= object [ "approved" .= True ] ]

-- | Handle a @gitHubToken.getToken@ request by calling the registered provider.
handleGitHubTokenReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleGitHubTokenReq client params = do
  let mArgs = parseMaybe (withObject "gitHubToken.getToken" $ \o ->
        GitHubTokenProviderArgs
          <$> o .:  "registrationId"
          <*> o .:? "host" .!= ""
          <*> o .:? "sessionId"
          <*> o .:? "reason" .!= "") params
  case mArgs of
    Nothing -> pure $ Left $ JsonRpcError (-32602) "Invalid gitHubToken.getToken payload" Nothing
    Just args -> do
      providers <- readMVar (ccTokenProviders client)
      case Map.lookup (gtpaRegistrationId args) providers of
        Nothing -> pure $ Left $ JsonRpcError (-32602)
          ("Unknown GitHub token provider registration: " <> gtpaRegistrationId args) Nothing
        Just provider -> do
          result <- try (provider args)
          case result of
            Right answer -> pure $ Right $ Aeson.toJSON answer
            Left (e :: SomeException) ->
              pure $ Left $ JsonRpcError (-32603) ("GitHub token provider failed: " <> T.pack (show e)) Nothing

-- | Handle a @shellAuthentication.request@; cancels when the session has no
-- handler or the handler fails.
handleShellAuthenticationReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleShellAuthenticationReq client params =
  case Aeson.fromJSON params of
    Aeson.Error e -> pure $ Left $ JsonRpcError (-32602) (T.pack e) Nothing
    Aeson.Success (req :: ShellAuthenticationRequest) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup (sarSessionId req) sessions of
        Nothing -> pure $ Right $ Aeson.toJSON ShellAuthenticationCancel
        Just session -> do
          result <- try $ handleSessionShellAuthenticationRequest session req
          case result of
            Right answer -> pure $ Right $ Aeson.toJSON answer
            Left (_ :: SomeException) -> pure $ Right $ Aeson.toJSON ShellAuthenticationCancel

-- | Handle a @memoryStore.store@ request; replies null once stored.
handleMemoryStoreReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleMemoryStoreReq client params =
  case Aeson.fromJSON params of
    Aeson.Error e -> pure $ Left $ JsonRpcError (-32602) (T.pack e) Nothing
    Aeson.Success (memory :: StoredMemory) -> do
      sessions <- readMVar (ccSessions client)
      case Map.lookup (stmSessionId memory) sessions of
        Nothing -> pure $ Left $ JsonRpcError (-32602) "Memory store owner is unavailable" Nothing
        Just session -> do
          result <- try $ handleSessionStoreMemory session memory
          case result of
            Right () -> pure (Right Null)
            Left (e :: SomeException) -> pure $ Left $ JsonRpcError (-32603) (T.pack $ show e) Nothing

-- | Handle @network.resolveProxy@ with the 'noResolveProxy' callback.
handleResolveProxyReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleResolveProxyReq client params =
  case ccoNetwork (ccOptions client) >>= noResolveProxy of
    Nothing -> pure $ Left $ JsonRpcError (-32601) "No resolveProxy callback registered" Nothing
    Just callback -> do
      let url = fromMaybe "" (parseMaybe (withObject "" $ \o -> o .: "url") params)
      mProxy <- callback url
      pure $ Right $ object $ catMaybes [ ("proxy" .=) <$> mProxy ]

-- | Handle @network.authenticateNegotiate@ with the 'noAuthenticateNegotiate'
-- callback. The token is never logged.
handleAuthenticateNegotiateReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleAuthenticateNegotiateReq client params =
  case ccoNetwork (ccOptions client) >>= noAuthenticateNegotiate of
    Nothing -> pure $ Left $ JsonRpcError (-32601) "No authenticateNegotiate callback registered" Nothing
    Just callback -> do
      let spn = fromMaybe "" (parseMaybe (withObject "" $ \o -> o .: "spn") params)
      mToken <- callback spn
      pure $ Right $ object $ catMaybes [ ("token" .=) <$> mToken ]

-- | Handle @network.authenticateBasic@ with the 'noAuthenticateBasic'
-- callback. Credentials are never logged.
handleAuthenticateBasicReq :: CopilotClient -> Value -> IO (Either JsonRpcError Value)
handleAuthenticateBasicReq client params =
  case ccoNetwork (ccOptions client) >>= noAuthenticateBasic of
    Nothing -> pure $ Left $ JsonRpcError (-32601) "No authenticateBasic callback registered" Nothing
    Just callback -> do
      let mReq = parseMaybe (withObject "network.authenticateBasic" $ \o ->
            NetworkBasicAuthRequest
              <$> o .:? "proxy" .!= ""
              <*> o .:? "realm"
              <*> o .:? "previousRejected" .!= False) params
      case mReq of
        Nothing -> pure $ Left $ JsonRpcError (-32602) "Invalid network.authenticateBasic payload" Nothing
        Just req -> do
          mCredentials <- callback req
          pure $ Right $ object $ case mCredentials of
            Just credentials ->
              [ "username" .= nbcUsername credentials
              , "password" .= nbcPassword credentials
              ]
            Nothing -> []
