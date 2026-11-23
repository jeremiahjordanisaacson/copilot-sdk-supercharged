---------------------------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
---------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Copilot Session management.
--
-- A 'CopilotSession' represents a single conversation session with the
-- Copilot CLI. It supports sending messages, subscribing to events,
-- registering tools, and handling permission\/user-input\/hook requests.
module Copilot.Session
  ( -- * Session type
    CopilotSession
  , newCopilotSession
  , sessionId
  , workspacePath

    -- * Sending messages
  , send
  , sendAndWait

    -- * Session controls
  , switchAutoTier
  , detachSession
  , recheckPolicyHelper
  , setPermissionMode

    -- * Event subscription
  , onSessionEvent
  , dispatchSessionEvent

    -- * Tool registration (internal)
  , registerSessionTools
  , getSessionToolHandler

    -- * Permission / user-input / hooks handlers (internal)
  , registerSessionPermissionHandler
  , registerSessionUserInputHandler
  , registerSessionHooks
  , handleSessionPermissionRequest
  , handleSessionUserInputRequest
  , handleSessionHooksInvoke

    -- * Shell authentication / memory store handlers (internal)
  , registerSessionShellAuthenticationHandler
  , handleSessionShellAuthenticationRequest
  , registerSessionStoreMemoryHandler
  , handleSessionStoreMemory

    -- * Exit plan mode (internal)
  , registerExitPlanModeHandler
  , handleExitPlanModeRequest

    -- * Trace context (internal)
  , registerTraceContextProvider
  , injectTraceContext

    -- * Release callbacks (internal)
  , addSessionCloseCallback
  , runSessionCloseCallbacks

    -- * Lifecycle
  , destroySession
  , abortSession
  , getMessages
  , getMetadata
  ) where

import           Control.Concurrent        (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar)
import           Control.Exception         (SomeException, catch, try, throwIO, Exception)
import           Control.Monad             (forM_, void, when)
import           Data.Aeson                (Value (..), object, (.=), (.:), (.:?), withObject)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Key            (fromText)
import qualified Data.Aeson.KeyMap         as KM
import           Data.Aeson.Types          (parseMaybe)
import           Data.IORef
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.Timeout            (timeout)

import           Copilot.JsonRpc           (JsonRpcClient, JsonRpcError (..),
                                            sendRequest)
import           Copilot.Types

-- ============================================================================
-- Session Error
-- ============================================================================

data SessionError
  = SessionRpcError Text
  | SessionTimeout Int
  | SessionErrorEvent Text (Maybe Text)
  deriving (Show)

instance Exception SessionError

-- ============================================================================
-- Session Type
-- ============================================================================

-- | A single conversation session with the Copilot CLI.
data CopilotSession = CopilotSession
  { csSessionId         :: !Text
  , csWorkspacePath     :: !(Maybe Text)
  , csJsonRpc           :: !JsonRpcClient
  , csEventHandlers     :: !(IORef [SessionEventHandler])
  , csToolHandlers      :: !(IORef (Map.Map Text ToolHandler))
  , csPermissionHandler :: !(IORef (Maybe PermissionHandler))
  , csUserInputHandler  :: !(IORef (Maybe UserInputHandler))
  , csHooks             :: !(IORef (Maybe SessionHooks))
  , csExitPlanModeHandler :: !(IORef (Maybe ExitPlanModeHandler))
  , csTraceContextProvider :: !(IORef (Maybe TraceContextProvider))
  , csShellAuthHandler  :: !(IORef (Maybe ShellAuthenticationHandler))
  , csStoreMemoryHandler :: !(IORef (Maybe StoreMemoryHandler))
  , csCloseCallbacks    :: !(IORef [IO ()])
  }

-- | Create a new session.
newCopilotSession :: Text -> JsonRpcClient -> Maybe Text -> IO CopilotSession
newCopilotSession sid rpc wsp = do
  handlers   <- newIORef []
  toolH      <- newIORef Map.empty
  permH      <- newIORef Nothing
  userInputH <- newIORef Nothing
  hooksRef   <- newIORef Nothing
  exitPlanH  <- newIORef Nothing
  traceCtxP  <- newIORef Nothing
  shellAuthH <- newIORef Nothing
  memoryH    <- newIORef Nothing
  closeCbs   <- newIORef []
  pure CopilotSession
    { csSessionId         = sid
    , csWorkspacePath     = wsp
    , csJsonRpc           = rpc
    , csEventHandlers     = handlers
    , csToolHandlers      = toolH
    , csPermissionHandler = permH
    , csUserInputHandler  = userInputH
    , csHooks             = hooksRef
    , csExitPlanModeHandler = exitPlanH
    , csTraceContextProvider = traceCtxP
    , csShellAuthHandler  = shellAuthH
    , csStoreMemoryHandler = memoryH
    , csCloseCallbacks    = closeCbs
    }

-- | Get the session ID.
sessionId :: CopilotSession -> Text
sessionId = csSessionId

-- | Get the workspace path (when infinite sessions are enabled).
workspacePath :: CopilotSession -> Maybe Text
workspacePath = csWorkspacePath

-- ============================================================================
-- Sending Messages
-- ============================================================================

-- | Send a message to the session.
-- Returns the message ID.
--
-- 'moSource' and 'moClientTurnId' are sent as @source@ and @clientTurnId@;
-- 'moResponseSchema' requests structured output (see
-- 'structuredResponseFormat'). The legacy 'moResponseFormat' is not sent.
send :: CopilotSession -> MessageOptions -> IO Text
send session opts = do
  let reqParams = object $ filter ((/= Null) . snd)
        [ "sessionId"      .= csSessionId session
        , "prompt"         .= moPrompt opts
        , "attachments"    .= moAttachments opts
        , "mode"           .= moMode opts
        , "agentMode"      .= moAgentMode opts
        , "displayPrompt"  .= moDisplayPrompt opts
        , "requestHeaders" .= moRequestHeaders opts
        , "imageOptions"   .= moImageOptions opts
        , "source"         .= moSource opts
        , "clientTurnId"   .= moClientTurnId opts
        , "responseFormat" .= (structuredResponseFormat <$> moResponseSchema opts)
        ]
  result <- sendRequest (csJsonRpc session) "session.send" reqParams
  case result of
    Left err -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> do
      let mMsgId = parseMaybe (withObject "" $ \o -> o .: "messageId") val :: Maybe Text
      case mMsgId of
        Just mid -> pure mid
        Nothing  -> throwIO $ SessionRpcError "No messageId in send response"

-- | Send a message and wait for the session to become idle.
--
-- Returns the last @assistant.message@ event received, or 'Nothing' if none
-- was emitted before the session went idle.
--
-- The timeout is in microseconds (default: 60 seconds = 60000000).
sendAndWait :: CopilotSession -> MessageOptions -> Maybe Int -> IO (Maybe SessionEvent)
sendAndWait session opts mTimeout = do
  let timeoutUs = maybe 60000000 id mTimeout

  idleMVar <- newEmptyMVar :: IO (MVar ())
  errorMVar <- newEmptyMVar :: IO (MVar SessionError)
  lastMsgRef <- newIORef (Nothing :: Maybe SessionEvent)

  -- Subscribe to events BEFORE sending to avoid race conditions
  let handler evt = do
        case seType evt of
          "assistant.message" -> writeIORef lastMsgRef (Just evt)
          "session.idle"      -> void $ tryPutMVar idleMVar ()
          "session.error"     -> do
            let mMsg = parseMaybe (withObject "" $ \o -> o .: "message") (seData evt) :: Maybe Text
                mStack = parseMaybe (withObject "" $ \o -> o .:? "stack") (seData evt) :: Maybe (Maybe Text)
            void $ tryPutMVar errorMVar (SessionErrorEvent (maybe "session error" id mMsg) (maybe Nothing id mStack))
          _ -> pure ()

  unsubscribe <- onSessionEvent session handler

  -- Send the message
  sendResult <- try $ send session opts
  case sendResult of
    Left (e :: SomeException) -> do
      unsubscribe
      throwIO $ SessionRpcError (T.pack $ show e)
    Right _ -> pure ()

  -- Wait for idle, error, or timeout
  result <- timeout timeoutUs $ do
    -- Race between idle and error
    let waitLoop = do
          -- Check if idle already signaled
          mIdle <- tryTakeMVar idleMVar
          case mIdle of
            Just () -> pure Nothing  -- No error
            Nothing -> do
              mErr <- tryTakeMVar errorMVar
              case mErr of
                Just err -> pure (Just err)
                Nothing  -> do
                  -- Small sleep then retry via STM or polling
                  -- Use a simple approach: block on idleMVar with a short timeout
                  mIdle2 <- timeout 10000 (takeMVar idleMVar)  -- 10ms poll
                  case mIdle2 of
                    Just () -> pure Nothing
                    Nothing -> waitLoop
    waitLoop

  unsubscribe

  case result of
    Nothing -> throwIO $ SessionTimeout (timeoutUs `div` 1000)
    Just (Just err) -> throwIO err
    Just Nothing -> readIORef lastMsgRef

-- ============================================================================
-- Event Subscription
-- ============================================================================

-- | Subscribe to session events.
-- Returns an IO action that unsubscribes when called.
onSessionEvent :: CopilotSession -> SessionEventHandler -> IO (IO ())
onSessionEvent session handler = do
  -- Use an IORef as a unique "alive" flag for this subscription
  aliveRef <- newIORef True
  let wrappedHandler evt = do
        alive <- readIORef aliveRef
        when alive $ handler evt
  modifyIORef (csEventHandlers session) (wrappedHandler :)
  pure $ writeIORef aliveRef False

-- | Dispatch an event to all registered handlers. (Internal)
dispatchSessionEvent :: CopilotSession -> SessionEvent -> IO ()
dispatchSessionEvent session evt = do
  handlers <- readIORef (csEventHandlers session)
  forM_ handlers $ \h ->
    catch (h evt) (\(_ :: SomeException) -> pure ())

-- ============================================================================
-- Tool Registration
-- ============================================================================

-- | Register tools for this session. (Internal)
registerSessionTools :: CopilotSession -> [Tool] -> IO ()
registerSessionTools session tools = do
  let m = Map.fromList [ (toolName t, toolHandler t) | t <- tools ]
  writeIORef (csToolHandlers session) m

-- | Get a tool handler by name. (Internal)
getSessionToolHandler :: CopilotSession -> Text -> IO (Maybe ToolHandler)
getSessionToolHandler session name = do
  handlers <- readIORef (csToolHandlers session)
  pure $ Map.lookup name handlers

-- ============================================================================
-- Permission Handler
-- ============================================================================

-- | Register a permission handler. (Internal)
registerSessionPermissionHandler :: CopilotSession -> PermissionHandler -> IO ()
registerSessionPermissionHandler session handler =
  writeIORef (csPermissionHandler session) (Just handler)

-- | Handle a permission request. (Internal)
handleSessionPermissionRequest :: CopilotSession -> Value -> Text -> IO PermissionRequestResult
handleSessionPermissionRequest session reqVal sid = do
  mHandler <- readIORef (csPermissionHandler session)
  case mHandler of
    Nothing -> pure $ PermissionRequestResult "denied-no-approval-rule-and-could-not-request-from-user" Nothing
    Just handler -> do
      case Aeson.fromJSON reqVal of
        Aeson.Success permReq -> handler permReq sid
        Aeson.Error _ ->
          -- Try to at least get the kind
          let mKind = parseMaybe (withObject "" $ \o -> o .: "kind") reqVal :: Maybe Text
          in case mKind of
            Just kind -> handler (PermissionRequest kind Nothing Map.empty) sid
            Nothing   -> pure $ PermissionRequestResult "denied-no-approval-rule-and-could-not-request-from-user" Nothing

-- ============================================================================
-- User Input Handler
-- ============================================================================

-- | Register a user input handler. (Internal)
registerSessionUserInputHandler :: CopilotSession -> UserInputHandler -> IO ()
registerSessionUserInputHandler session handler =
  writeIORef (csUserInputHandler session) (Just handler)

-- | Handle a user input request. (Internal)
handleSessionUserInputRequest :: CopilotSession -> UserInputRequest -> Text -> IO UserInputResponse
handleSessionUserInputRequest session req sid = do
  mHandler <- readIORef (csUserInputHandler session)
  case mHandler of
    Nothing -> throwIO $ SessionRpcError "No user input handler registered"
    Just handler -> handler req sid

-- ============================================================================
-- Hooks
-- ============================================================================

-- | Register hooks. (Internal)
registerSessionHooks :: CopilotSession -> SessionHooks -> IO ()
registerSessionHooks session hooks =
  writeIORef (csHooks session) (Just hooks)

-- | Handle a hooks invocation. (Internal)
handleSessionHooksInvoke :: CopilotSession -> Text -> Value -> Text -> IO (Maybe Value)
handleSessionHooksInvoke session hookType input sid = do
  mHooks <- readIORef (csHooks session)
  case mHooks of
    Nothing -> pure Nothing
    Just hooks -> do
      let inv = HookInvocation { hiSessionId = sid }
      case hookType of
        "preToolUse" -> case shOnPreToolUse hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "postToolUse" -> case shOnPostToolUse hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "preMcpToolCall" -> case shOnPreMcpToolCall hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "userPromptSubmitted" -> case shOnUserPromptSubmitted hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "userPromptTransformed" -> case shOnUserPromptTransformed hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "sessionStart" -> case shOnSessionStart hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "sessionEnd" -> case shOnSessionEnd hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "errorOccurred" -> case shOnErrorOccurred hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "subagentStart" -> case shOnSubagentStart hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        "subagentStop" -> case shOnSubagentStop hooks of
          Nothing -> pure Nothing
          Just handler -> do
            case Aeson.fromJSON input of
              Aeson.Success inp -> do
                result <- handler inp inv
                pure $ fmap Aeson.toJSON result
              Aeson.Error _ -> pure Nothing

        _ -> pure Nothing

-- ============================================================================
-- Shell Authentication / Memory Store
-- ============================================================================

-- | Register a shell authentication handler. (Internal)
registerSessionShellAuthenticationHandler :: CopilotSession -> ShellAuthenticationHandler -> IO ()
registerSessionShellAuthenticationHandler session handler =
  writeIORef (csShellAuthHandler session) (Just handler)

-- | Answer a shell authentication request; cancels when no handler is
-- registered. (Internal)
handleSessionShellAuthenticationRequest :: CopilotSession -> ShellAuthenticationRequest -> IO ShellAuthenticationResponse
handleSessionShellAuthenticationRequest session req = do
  mHandler <- readIORef (csShellAuthHandler session)
  case mHandler of
    Nothing      -> pure ShellAuthenticationCancel
    Just handler -> handler req

-- | Register a store-memory handler. (Internal)
registerSessionStoreMemoryHandler :: CopilotSession -> StoreMemoryHandler -> IO ()
registerSessionStoreMemoryHandler session handler =
  writeIORef (csStoreMemoryHandler session) (Just handler)

-- | Hand a memory the agent asked to keep to the session's handler. (Internal)
handleSessionStoreMemory :: CopilotSession -> StoredMemory -> IO ()
handleSessionStoreMemory session memory = do
  mHandler <- readIORef (csStoreMemoryHandler session)
  case mHandler of
    Nothing      -> throwIO $ SessionRpcError "No memory store handler registered"
    Just handler -> handler memory

-- ============================================================================
-- Exit Plan Mode
-- ============================================================================

-- | Register an exit plan mode handler. (Internal)
registerExitPlanModeHandler :: CopilotSession -> ExitPlanModeHandler -> IO ()
registerExitPlanModeHandler session handler =
  writeIORef (csExitPlanModeHandler session) (Just handler)

-- | Handle an exit plan mode request. (Internal)
handleExitPlanModeRequest :: CopilotSession -> Value -> IO ExitPlanModeResponse
handleExitPlanModeRequest session reqVal = do
  mHandler <- readIORef (csExitPlanModeHandler session)
  case mHandler of
    Nothing -> pure $ ExitPlanModeResponse True
    Just handler -> do
      case Aeson.fromJSON reqVal of
        Aeson.Success req -> handler req
        Aeson.Error _ ->
          let mSid = parseMaybe (withObject "" $ \o -> o .: "sessionId") reqVal :: Maybe Text
          in case mSid of
            Just sid -> handler (ExitPlanModeRequest sid)
            Nothing  -> pure $ ExitPlanModeResponse True

-- ============================================================================
-- Trace Context
-- ============================================================================

-- | Register a trace context provider. (Internal)
registerTraceContextProvider :: CopilotSession -> TraceContextProvider -> IO ()
registerTraceContextProvider session provider =
  writeIORef (csTraceContextProvider session) (Just provider)

-- | Inject trace context into request params. (Internal)
injectTraceContext :: CopilotSession -> Value -> IO Value
injectTraceContext session params = do
  mProvider <- readIORef (csTraceContextProvider session)
  case mProvider of
    Nothing -> pure params
    Just provider -> do
      ctx <- provider `catch` (\(_ :: SomeException) -> pure $ TraceContext Nothing Nothing)
      let addField key mVal obj = case mVal of
            Nothing -> obj
            Just v  -> case obj of
              Object hm -> Object (KM.insert (fromText key) (String v) hm)
              _         -> obj
      pure $ addField "traceparent" (tcTraceparent ctx) $ addField "tracestate" (tcTracestate ctx) params

-- ============================================================================
-- Lifecycle
-- ============================================================================

-- | Release this session: detach from it (@session.detach@, falling back to
-- @session.destroy@ on runtimes without detach) and drop its handlers.
-- Session state on disk is kept, so the session can be resumed later.
destroySession :: CopilotSession -> IO ()
destroySession = detachSession

-- | Detach from this session (@session.detach@) and release its handlers and
-- registrations (for example a GitHub token provider). Session state on disk
-- is kept, so the session can be resumed later with
-- 'Copilot.Client.resumeSession'.
detachSession :: CopilotSession -> IO ()
detachSession session = do
  let params = object [ "sessionId" .= csSessionId session ]
  detached <- sendRequest (csJsonRpc session) "session.detach" params
  result <- case detached of
    Left err | jreCode err == (-32601) ->
      sendRequest (csJsonRpc session) "session.destroy" params
    other -> pure other
  case result of
    Left err -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> do
      let mSuccess = parseMaybe (withObject "" $ \o -> o .:? "success") val :: Maybe (Maybe Bool)
          mError   = parseMaybe (withObject "" $ \o -> o .:? "error") val :: Maybe (Maybe Text)
      case mSuccess of
        Just (Just False) ->
          throwIO $ SessionRpcError $ "Failed to detach session " <> csSessionId session
            <> ": " <> maybe "Unknown error" id (maybe Nothing id mError)
        _ -> do
          writeIORef (csEventHandlers session) []
          writeIORef (csToolHandlers session) Map.empty
          writeIORef (csPermissionHandler session) Nothing
          writeIORef (csUserInputHandler session) Nothing
          writeIORef (csHooks session) Nothing
          writeIORef (csExitPlanModeHandler session) Nothing
          writeIORef (csTraceContextProvider session) Nothing
          writeIORef (csShellAuthHandler session) Nothing
          writeIORef (csStoreMemoryHandler session) Nothing
          runSessionCloseCallbacks session

-- | Register an action to run once when the session is released. (Internal)
addSessionCloseCallback :: CopilotSession -> IO () -> IO ()
addSessionCloseCallback session callback =
  modifyIORef (csCloseCallbacks session) (callback :)

-- | Run and clear the session's release actions. (Internal)
runSessionCloseCallbacks :: CopilotSession -> IO ()
runSessionCloseCallbacks session = do
  callbacks <- atomicModifyIORef' (csCloseCallbacks session) (\cbs -> ([], cbs))
  forM_ callbacks $ \cb -> cb `catch` (\(_ :: SomeException) -> pure ())

-- | Change the Auto routing preference without changing the model
-- (@session.model.switchAutoTier@, experimental). Pass a tier such as
-- 'autoTierFast', 'autoTierEfficiency', 'autoTierBalance', or
-- 'autoTierIntelligence'; 'Nothing' returns to the provider's default
-- routing. Returns the runtime's acknowledgement.
switchAutoTier :: CopilotSession -> Maybe Text -> IO Value
switchAutoTier session tier = do
  let params = object
        [ "sessionId" .= csSessionId session
        , "autoTier"  .= tier
        ]
  result <- sendRequest (csJsonRpc session) "session.model.switchAutoTier" params
  case result of
    Left err  -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> pure val

-- | Re-run the managed-settings policy helper for this session
-- (@session.managedSettings.recheckPolicyHelper@).
recheckPolicyHelper :: CopilotSession -> IO Value
recheckPolicyHelper session = do
  let params = object [ "sessionId" .= csSessionId session ]
  result <- sendRequest (csJsonRpc session) "session.managedSettings.recheckPolicyHelper" params
  case result of
    Left err  -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> pure val

-- | Set the session permission mode (@session.permissions.setMode@). When the
-- runtime refuses the mode, 'pmrReason' explains why (for example
-- 'permissionModeRefusalManagedPolicy').
setPermissionMode :: CopilotSession -> Text -> IO PermissionModeResult
setPermissionMode session mode = do
  let params = object
        [ "sessionId" .= csSessionId session
        , "mode"      .= mode
        ]
  result <- sendRequest (csJsonRpc session) "session.permissions.setMode" params
  case result of
    Left err  -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> case Aeson.fromJSON val of
      Aeson.Success r -> pure r
      Aeson.Error e   -> throwIO $ SessionRpcError (T.pack e)

-- | Abort the currently processing message.
abortSession :: CopilotSession -> IO ()
abortSession session = do
  let params = object [ "sessionId" .= csSessionId session ]
  result <- sendRequest (csJsonRpc session) "session.abort" params
  case result of
    Left err -> throwIO $ SessionRpcError (jreMessage err)
    Right _  -> pure ()

-- | Retrieve metadata for this session.
getMetadata :: CopilotSession -> IO Value
getMetadata session = do
  let params = object [ "sessionId" .= csSessionId session ]
  result <- sendRequest (csJsonRpc session) "session.getMetadata" params
  case result of
    Left err -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> pure val

-- | Retrieve all events/messages from the session history.
getMessages :: CopilotSession -> IO [SessionEvent]
getMessages session = do
  let params = object [ "sessionId" .= csSessionId session ]
  result <- sendRequest (csJsonRpc session) "session.getMessages" params
  case result of
    Left err -> throwIO $ SessionRpcError (jreMessage err)
    Right val -> do
      let mEvents = parseMaybe (withObject "" $ \o -> o .: "events") val :: Maybe [SessionEvent]
      pure $ maybe [] id mEvents

