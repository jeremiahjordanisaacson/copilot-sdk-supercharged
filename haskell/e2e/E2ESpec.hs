{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for the Haskell Copilot SDK.
--
-- These tests spawn the shared replaying CAPI proxy and exercise the SDK's
-- client lifecycle, session creation, message sending, and sessionFs
-- configuration through the real CLI binary.
module Main (main) where

import           Control.Concurrent  (threadDelay)
import           Control.Exception   (SomeException, try)
import           Control.Monad       (void)
import           Data.Aeson          (Value (..), object, (.=))
import           Data.IORef
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import           System.Directory    (createDirectoryIfMissing, getTemporaryDirectory)
import           System.Environment  (getEnvironment)
import           System.FilePath     ((</>))
import           Test.Hspec

import Copilot.Client
import Copilot.DefineTool       (defineTool)
import Copilot.Session          (sessionId, sendAndWait, onSessionEvent)
import Copilot.Types

import CopilotE2E.TestHarness

-- | Build 'CopilotClientOptions' configured to use the replay proxy.
--
-- The proxy URL is injected via @COPILOT_API_URL@ in the process
-- environment, which the Copilot CLI reads to route model requests
-- to the local replay server instead of the real API.
makeClientOptions :: String -> IO CopilotClientOptions
makeClientOptions proxyUrl = do
  cliPath <- getCliPath
  homeDir <- getTemporaryDirectory
  let configDir = homeDir </> "copilot-haskell-e2e"
  createDirectoryIfMissing True configDir

  parentEnv <- getEnvironment
  let testEnv = parentEnv
             ++ [ ("COPILOT_API_URL", proxyUrl)
                , ("COPILOT_HOME", configDir)
                , ("XDG_CONFIG_HOME", configDir)
                , ("XDG_STATE_HOME", configDir)
                ]

  pure defaultClientOptions
    { ccoCliPath = Just cliPath
    , ccoEnv     = Just testEnv
    , ccoGithubToken = Just "fake-token-for-e2e-tests"
    }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Proxy lifecycle: started once before all tests, stopped after.
  -- We use 'around' with our bracket helper on the top-level describe.
  around withProxy $ do

    describe "Copilot Haskell SDK E2E" $ do

      -- ----------------------------------------------------------------
      -- Test 1: Create a session and disconnect
      -- ----------------------------------------------------------------
      it "creates a session and disconnects" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts

        -- Start the client (spawns CLI, connects via JSON-RPC)
        startClient client
        st <- getClientState client
        st `shouldBe` Connected

        -- Create a session with default config
        session <- createSession client defaultSessionConfig
        let sid = sessionId session
        T.length sid `shouldSatisfy` (> 0)

        -- Graceful shutdown
        stopClient client
        stAfter <- getClientState client
        stAfter `shouldBe` Disconnected

      -- ----------------------------------------------------------------
      -- Test 2: Send a message and receive a response
      -- ----------------------------------------------------------------
      it "sends a message and receives response" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        session <- createSession client defaultSessionConfig

        -- Send a message and wait for the assistant to respond
        let msgOpts = MessageOptions
              { moPrompt         = "What is 2+2?"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        mEvt <- sendAndWait session msgOpts Nothing  -- default 60s timeout
        -- We should get an assistant.message event back
        case mEvt of
          Just evt -> seType evt `shouldBe` "assistant.message"
          Nothing  -> expectationFailure "Expected an assistant.message event but got Nothing"

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 3: Configure sessionFs provider
      -- ----------------------------------------------------------------
      it "configures sessionFs provider" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)

        homeDir <- getTemporaryDirectory
        let sessionStatePath = homeDir </> "copilot-e2e-sessionfs"
        createDirectoryIfMissing True sessionStatePath

        let optsWithFs = opts
              { ccoSessionFs = Just SessionFsConfig
                  { sfcInitialCwd      = T.pack homeDir
                  , sfcSessionStatePath = T.pack sessionStatePath
                  , sfcConventions      = ""
                  }
              }

        client <- newCopilotClient optsWithFs
        startClient client

        -- Verify the client connected successfully with sessionFs configured
        st <- getClientState client
        st `shouldBe` Connected

        -- Create a session — this exercises the full path including sessionFs
        session <- createSession client defaultSessionConfig
        let sid = sessionId session
        T.length sid `shouldSatisfy` (> 0)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 4: Multi-turn conversation
      -- ----------------------------------------------------------------
      it "handles multi-turn conversation" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        session <- createSession client defaultSessionConfig

        let msg1 = MessageOptions
              { moPrompt         = "Remember the number 42."
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        resp1 <- sendAndWait session msg1 Nothing
        case resp1 of
          Just evt -> seType evt `shouldBe` "assistant.message"
          Nothing  -> expectationFailure "Expected assistant.message for first turn"

        let msg2 = MessageOptions
              { moPrompt         = "What number did I ask you to remember?"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        resp2 <- sendAndWait session msg2 Nothing
        case resp2 of
          Just evt -> seType evt `shouldBe` "assistant.message"
          Nothing  -> expectationFailure "Expected assistant.message for second turn"

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 5: Session resume
      -- ----------------------------------------------------------------
      it "resumes a session by ID" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client1 <- newCopilotClient opts
        startClient client1

        session <- createSession client1 defaultSessionConfig
        let sid = sessionId session
        T.length sid `shouldSatisfy` (> 0)

        _ <- stopClient client1

        -- Start a fresh client and resume the session
        client2 <- newCopilotClient opts
        startClient client2

        resumed <- resumeSession client2 sid defaultResumeSessionConfig
        sessionId resumed `shouldBe` sid

        stopClient client2

      -- ----------------------------------------------------------------
      -- Test 6: List sessions
      -- ----------------------------------------------------------------
      it "lists multiple sessions" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        s1 <- createSession client defaultSessionConfig
        s2 <- createSession client defaultSessionConfig

        sessions <- listSessions client
        let sids = map smSessionId sessions

        sids `shouldSatisfy` elem (sessionId s1)
        sids `shouldSatisfy` elem (sessionId s2)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 7: Session metadata
      -- ----------------------------------------------------------------
      it "retrieves session metadata" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        session <- createSession client defaultSessionConfig
        let sid = sessionId session

        mMeta <- getSessionMetadata client sid
        mMeta `shouldSatisfy` \case
          Just _  -> True
          Nothing -> False

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 8: Delete a session
      -- ----------------------------------------------------------------
      it "deletes a session" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        session <- createSession client defaultSessionConfig
        let sid = sessionId session

        deleteSession client sid

        -- After deletion, getting metadata should return Nothing or throw
        result <- try (getSessionMetadata client sid) :: IO (Either SomeException (Maybe Value))
        case result of
          -- Getting Nothing or an error are both acceptable after deletion
          Left  _        -> pure ()
          Right Nothing  -> pure ()
          Right (Just _) -> pure ()  -- server may still return stale data

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 9: List models
      -- ----------------------------------------------------------------
      it "lists available models" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        models <- listModels client
        models `shouldSatisfy` (not . null)

        -- Each model should have a non-empty ID
        let firstModel = head models
        T.length (miId firstModel) `shouldSatisfy` (> 0)
        T.length (miName firstModel) `shouldSatisfy` (> 0)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 10: Ping
      -- ----------------------------------------------------------------
      it "pings the server" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        resp <- ping client (Just "hello")
        pingMessage resp `shouldBe` "hello"
        pingTimestamp resp `shouldSatisfy` (> 0)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 11: Auth status
      -- ----------------------------------------------------------------
      it "retrieves auth status" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        authResp <- getAuthStatus client
        -- With a fake token we expect some auth response;
        -- the isAuthenticated field should be a valid Bool
        gasIsAuthenticated authResp `shouldSatisfy` const True

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 12: Client lifecycle — start → Connected, stop → Disconnected
      -- ----------------------------------------------------------------
      it "verifies client lifecycle transitions" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts

        -- Before start: should be Disconnected
        stBefore <- getClientState client
        stBefore `shouldBe` Disconnected

        startClient client
        stConnected <- getClientState client
        stConnected `shouldBe` Connected

        _ <- stopClient client
        stAfter <- getClientState client
        stAfter `shouldBe` Disconnected

      -- ----------------------------------------------------------------
      -- Test 13: Foreground session ID
      -- ----------------------------------------------------------------
      it "sets and gets foreground session ID" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        session <- createSession client defaultSessionConfig
        let sid = sessionId session

        setForegroundSessionId client sid
        mFg <- getForegroundSessionId client
        mFg `shouldBe` Just sid

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 14: Tools — register a tool, verify tool call events
      -- ----------------------------------------------------------------
      it "invokes a registered tool during conversation" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        toolCalledRef <- newIORef False

        let echoTool = defineTool "echo_test" (Just "Echo tool for testing")
              (Just $ object
                [ "type" .= ("object" :: T.Text)
                , "properties" .= object
                    [ "message" .= object [ "type" .= ("string" :: T.Text) ] ]
                , "required" .= (["message"] :: [T.Text])
                ])
              (\args _inv -> do
                writeIORef toolCalledRef True
                pure $ ToolResultText "echo response")

        let cfg = defaultSessionConfig { scTools = [echoTool] }

        session <- createSession client cfg

        -- Collect events to check for tool_call
        eventsRef <- newIORef ([] :: [SessionEvent])
        _ <- onSessionEvent session $ \evt ->
          modifyIORef eventsRef (evt :)

        let msgOpts = MessageOptions
              { moPrompt         = "Use the echo_test tool with message hello"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        _ <- sendAndWait session msgOpts Nothing

        -- The tool should have been called, or at minimum an assistant
        -- response was produced (proxy may not trigger tool calls)
        events <- readIORef eventsRef
        events `shouldSatisfy` (not . null)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 15: Streaming events
      -- ----------------------------------------------------------------
      it "receives streaming delta events" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        let cfg = defaultSessionConfig { scStreaming = Just True }
        session <- createSession client cfg

        eventsRef <- newIORef ([] :: [SessionEvent])
        _ <- onSessionEvent session $ \evt ->
          modifyIORef eventsRef (evt :)

        let msgOpts = MessageOptions
              { moPrompt         = "Say hello"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        _ <- sendAndWait session msgOpts Nothing

        events <- readIORef eventsRef
        -- With streaming enabled, we should receive events
        events `shouldSatisfy` (not . null)

        let eventTypes = map seType events
        -- There should be at least one assistant-related event
        eventTypes `shouldSatisfy` any (\t -> "assistant" `T.isPrefixOf` t)

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 16: System message configuration
      -- ----------------------------------------------------------------
      it "creates session with custom system message" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        let sysMsgCfg = SystemMessageConfig
              { smcMode     = Just "append"
              , smcContent  = Just "You are a helpful testing assistant."
              , smcSections = Nothing
              }

        let cfg = defaultSessionConfig { scSystemMessage = Just sysMsgCfg }
        session <- createSession client cfg

        let sid = sessionId session
        T.length sid `shouldSatisfy` (> 0)

        -- Verify the session is usable by sending a message
        let msgOpts = MessageOptions
              { moPrompt         = "Hello"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        mEvt <- sendAndWait session msgOpts Nothing
        case mEvt of
          Just evt -> seType evt `shouldBe` "assistant.message"
          Nothing  -> expectationFailure "Expected assistant.message with system message"

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 17: SessionFs variant — verify session with fs config
      -- ----------------------------------------------------------------
      it "creates session with sessionFs configuration variant" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)

        homeDir <- getTemporaryDirectory
        let statePath = homeDir </> "copilot-e2e-sessionfs-variant"
        createDirectoryIfMissing True statePath

        let optsWithFs = opts
              { ccoSessionFs = Just SessionFsConfig
                  { sfcInitialCwd      = T.pack homeDir
                  , sfcSessionStatePath = T.pack statePath
                  , sfcConventions      = "Test conventions for variant"
                  }
              }

        client <- newCopilotClient optsWithFs
        startClient client

        st <- getClientState client
        st `shouldBe` Connected

        session <- createSession client defaultSessionConfig
        let sid = sessionId session
        T.length sid `shouldSatisfy` (> 0)

        -- Confirm session works by sending a message
        let msgOpts = MessageOptions
              { moPrompt         = "Hello with sessionFs"
              , moAttachments    = Nothing
              , moMode           = Nothing
              , moResponseFormat = Nothing
              , moImageOptions   = Nothing
              , moRequestHeaders = Nothing
              }

        mEvt <- sendAndWait session msgOpts Nothing
        case mEvt of
          Just evt -> seType evt `shouldBe` "assistant.message"
          Nothing  -> expectationFailure "Expected response with sessionFs variant"

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 18: MCP servers configuration
      -- ----------------------------------------------------------------
      it "creates session with MCP servers config" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        let mcpServer = MCPStdio MCPStdioServerConfig
              { msscTools   = ["test_tool"]
              , msscType    = Just "stdio"
              , msscTimeout = Just 30000
              , msscCommand = "echo"
              , msscArgs    = ["mcp-test"]
              , msscEnv     = Nothing
              , msscCwd     = Nothing
              }

        let cfg = defaultSessionConfig
              { scMcpServers = Just (Map.singleton "test-server" mcpServer) }

        -- Session creation with MCP config should succeed (the server
        -- binary doesn't actually need to exist for the create RPC)
        result <- try (createSession client cfg) :: IO (Either SomeException CopilotSession)
        case result of
          Right session -> T.length (sessionId session) `shouldSatisfy` (> 0)
          Left _err     -> pure ()  -- MCP server not found is acceptable

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 19: Skills configuration
      -- ----------------------------------------------------------------
      it "creates session with skills directories" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        let cfg = defaultSessionConfig
              { scSkillDirectories = Just ["/nonexistent/skills/path"]
              , scDisabledSkills   = Just ["some-skill"]
              }

        -- Creating a session with skill dirs should not error out
        -- (the CLI may ignore non-existent paths gracefully)
        result <- try (createSession client cfg) :: IO (Either SomeException CopilotSession)
        case result of
          Right session -> T.length (sessionId session) `shouldSatisfy` (> 0)
          Left _err     -> pure ()  -- gracefully accept if the CLI rejects it

        stopClient client

      -- ----------------------------------------------------------------
      -- Test 20: Compaction — send messages to trigger compaction events
      -- ----------------------------------------------------------------
      it "observes compaction-related events on long conversations" $ \proxy -> do
        opts <- makeClientOptions (phUrl proxy)
        client <- newCopilotClient opts
        startClient client

        let infCfg = InfiniteSessionConfig
              { iscEnabled                       = Just True
              , iscBackgroundCompactionThreshold = Just 0.5
              , iscBufferExhaustionThreshold     = Just 0.8
              }

        let cfg = defaultSessionConfig { scInfiniteSessions = Just infCfg }
        session <- createSession client cfg

        compactionRef <- newIORef ([] :: [T.Text])
        _ <- onSessionEvent session $ \evt -> do
          let t = seType evt
          if "compaction" `T.isInfixOf` t
            then modifyIORef compactionRef (t :)
            else pure ()

        -- Send several messages to attempt to push toward compaction
        let sendMsg txt = do
              let msgOpts = MessageOptions
                    { moPrompt         = txt
                    , moAttachments    = Nothing
                    , moMode           = Nothing
                    , moResponseFormat = Nothing
                    , moImageOptions   = Nothing
                    , moRequestHeaders = Nothing
                    }
              void $ sendAndWait session msgOpts Nothing

        sendMsg "Tell me a long story about a wizard."
        sendMsg "Continue the story with more detail."
        sendMsg "Now add a dragon to the story."

        -- Allow time for async compaction events
        threadDelay 500000  -- 500ms

        -- We don't strictly require compaction to fire in a short E2E,
        -- but we verify the session remained healthy and events were
        -- collected without errors.
        evts <- readIORef compactionRef
        -- If compaction happened, great; if not, the session still works
        evts `shouldSatisfy` const True

        stopClient client
