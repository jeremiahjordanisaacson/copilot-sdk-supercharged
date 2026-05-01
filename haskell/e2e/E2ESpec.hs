{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for the Haskell Copilot SDK.
--
-- These tests spawn the shared replaying CAPI proxy and exercise the SDK's
-- client lifecycle, session creation, message sending, and sessionFs
-- configuration through the real CLI binary.
module Main (main) where

import qualified Data.Text     as T
import System.Directory        (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment      (getEnvironment)
import System.FilePath         ((</>))
import Test.Hspec

import Copilot.Client
import Copilot.Session         (sessionId, sendAndWait)
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
