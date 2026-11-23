{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test harness for Haskell E2E tests.
--
-- Spawns the shared replaying CAPI proxy from @test\/harness\/server.ts@ and
-- provides helpers to start\/stop it. The proxy URL is passed to the Copilot
-- CLI via the @COPILOT_API_URL@ environment variable so that all outbound
-- model requests are served from YAML snapshot files.
module CopilotE2E.TestHarness
  ( ProxyHandle (..)
  , startProxy
  , stopProxy
  , withProxy
  , getCliPath
  , configureProxy
  , snapshotsDir
  ) where

import Control.Exception    (SomeException, bracket, catch)
import Control.Monad        (filterM)
import Data.List            (isInfixOf, isPrefixOf)
import System.Directory     (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment   (lookupEnv)
import System.FilePath      ((</>))
import System.IO            (Handle, hGetLine, hClose, hIsEOF)
import System.Info          (os)
import System.Process       (CreateProcess (..), StdStream (..), ProcessHandle,
                             createProcess, shell, proc, terminateProcess,
                             waitForProcess)

-- | Handle to a running proxy process.
data ProxyHandle = ProxyHandle
  { phUrl     :: !String         -- ^ Proxy base URL, e.g. @http://localhost:3000@
  , phProcess :: !ProcessHandle  -- ^ OS process handle
  , phStdout  :: !Handle         -- ^ stdout pipe (kept open for reading)
  }

-- | Locate the Copilot CLI binary for E2E tests.
--
-- Checks @COPILOT_CLI_PATH@ first, then the CLI in the sibling @nodejs@
-- directory: @\@github\/copilot\/npm-loader.js@ (CLI 1.0.113 and newer, which
-- starts the native binary), then the legacy @index.js@ entry points in the
-- platform-specific package (e.g. @\@github\/copilot-linux-x64@) and in
-- @\@github\/copilot@. @.js@ paths are run with @node@ by the client; any
-- other path (such as @node_modules\/.bin\/copilot@) is executed directly.
getCliPath :: IO FilePath
getCliPath = do
  envPath <- lookupEnv "COPILOT_CLI_PATH"
  case envPath of
    Just p -> do
      exists <- doesFileExist p
      if exists then pure p
                else fallbackCliPath
    Nothing -> fallbackCliPath
  where
    fallbackCliPath = do
      cwd <- getCurrentDirectory
      -- cabal test runs from haskell/, so go up one level to repo root
      let githubModules = cwd </> ".." </> "nodejs" </> "node_modules" </> "@github"
          npmLoader     = githubModules </> "copilot" </> "npm-loader.js"
          legacyCli     = githubModules </> "copilot" </> "index.js"
      platformCli <- findPlatformCli githubModules
      let candidates = [npmLoader] ++ maybe [] pure platformCli ++ [legacyCli]
      found <- firstExistingFile candidates
      case found of
        Just p  -> pure p
        Nothing -> error $ "CLI not found for tests. Run 'npm install' in the nodejs directory.\n"
                        ++ "Tried: " ++ show candidates

    -- Locate node_modules/@github/copilot-<platform>/index.js, excluding the
    -- language-server package which also matches the copilot-* prefix.
    findPlatformCli githubModules = do
      dirExists <- doesDirectoryExist githubModules
      if not dirExists
        then pure Nothing
        else do
          entries <- listDirectory githubModules
          let platformDirs = filter (\d -> "copilot-" `isPrefixOf` d
                                            && not ("language-server" `isInfixOf` d)) entries
          existing <- filterM (\d -> doesFileExist (githubModules </> d </> "index.js")) platformDirs
          pure $ case existing of
            (d : _) -> Just (githubModules </> d </> "index.js")
            []      -> Nothing

    firstExistingFile []       = pure Nothing
    firstExistingFile (p : ps) = do
      e <- doesFileExist p
      if e then pure (Just p) else firstExistingFile ps

-- | Start the replaying CAPI proxy.
--
-- Spawns @npx tsx server.ts@ in the @test\/harness@ directory and reads
-- the first line of stdout to extract the @Listening: http://...@ URL.
--
-- On Windows the command is run through the shell so that @npx@ is found
-- via @PATH@.
startProxy :: IO ProxyHandle
startProxy = do
  cwd <- getCurrentDirectory
  let harnessDir  = cwd </> ".." </> "test" </> "harness"
      serverPath  = harnessDir </> "server.ts"

  let cpSpec = if isWindows
        then (shell $ "npx tsx " ++ show serverPath)
              { std_out = CreatePipe
              , std_err = Inherit
              , cwd     = Just harnessDir
              }
        else (proc "npx" ["tsx", serverPath])
              { std_out = CreatePipe
              , std_err = Inherit
              , cwd     = Just harnessDir
              }

  (_, mStdout, _, ph) <- createProcess cpSpec
  case mStdout of
    Nothing -> do
      terminateProcess ph
      error "Failed to capture proxy stdout"
    Just hOut -> do
      -- Read lines until we find the Listening URL
      url <- readListeningUrl hOut
      pure ProxyHandle
        { phUrl     = url
        , phProcess = ph
        , phStdout  = hOut
        }

-- | Read stdout lines until we find "Listening: http://..."
readListeningUrl :: Handle -> IO String
readListeningUrl h = do
  eof <- hIsEOF h
  if eof
    then error "Proxy exited before printing Listening URL"
    else do
      line <- hGetLine h
      case extractUrl line of
        Just url -> pure url
        Nothing  -> readListeningUrl h  -- skip non-matching lines
  where
    extractUrl :: String -> Maybe String
    extractUrl s =
      let prefix = "Listening: "
      in case break (== 'L') s of
           (_, rest)
             | prefix `isPrefixOf` rest ->
                 let url = drop (length prefix) rest
                 in Just (takeWhile (/= ' ') url)
           _ -> Nothing

-- | Stop the proxy process.
--
-- Sends @POST \/stop@ to trigger graceful shutdown, then terminates and
-- waits for the process.
stopProxy :: ProxyHandle -> IO ()
stopProxy ph = do
  -- Best-effort graceful stop via HTTP
  sendStopRequest (phUrl ph) `catch` ignoreException
  terminateProcess (phProcess ph)
  _ <- waitForProcess (phProcess ph)
  hClose (phStdout ph) `catch` ignoreException

-- | Ignore any exception (used for best-effort cleanup).
ignoreException :: SomeException -> IO ()
ignoreException _ = pure ()

-- | Send a POST to /stop on the proxy. We use a simple shell curl to avoid
-- pulling in an HTTP client dependency just for tests.
sendStopRequest :: String -> IO ()
sendStopRequest url = do
  let stopUrl = url ++ "/stop?skipWritingCache=true"
  let cpSpec = if isWindows
        then shell $ "curl -s -X POST " ++ show stopUrl
        else proc "curl" ["-s", "-X", "POST", stopUrl]
  (_, _, _, ph) <- createProcess cpSpec { std_out = CreatePipe, std_err = CreatePipe }
  _ <- waitForProcess ph
  pure ()

-- | Bracket helper: start proxy, run action, stop proxy.
withProxy :: (ProxyHandle -> IO a) -> IO a
withProxy = bracket startProxy stopProxy

-- | Detect Windows at runtime.
isWindows :: Bool
isWindows = "mingw" `isInfixOf` os || "windows" `isInfixOf` os

-- | The directory containing test snapshot YAML files.
snapshotsDir :: IO FilePath
snapshotsDir = do
  cwd <- getCurrentDirectory
  let repoRoot = cwd </> ".."
  pure (repoRoot </> "test" </> "snapshots")

-- | Configure the proxy with a specific snapshot file and working directory.
--
-- Uses curl to POST JSON to the proxy's /config endpoint. curl is run
-- directly (no shell) so the JSON body is passed intact on every platform;
-- 'show' renders each path as a JSON string (escaping backslashes and quotes).
configureProxy :: ProxyHandle -> FilePath -> FilePath -> IO ()
configureProxy proxy filePath wd = do
  let configUrl = phUrl proxy ++ "/config"
      body = "{\"filePath\":" ++ show filePath ++ ",\"workDir\":" ++ show wd ++ "}"
      cpSpec = proc "curl" ["-s", "-X", "POST", "-H", "Content-Type: application/json", "-d", body, configUrl]
  (_, _, _, ph) <- createProcess cpSpec { std_out = CreatePipe, std_err = CreatePipe }
  _ <- waitForProcess ph
  pure ()
