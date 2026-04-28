# Persisting Sessions - Haskell

Patterns for saving and resuming Copilot sessions across application restarts in Haskell.

## Basic Save and Load with Aeson

**Scenario:** Save session state to a JSON file and restore it on next run.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Copilot.SDK

saveSessionState :: CopilotSession -> FilePath -> IO ()
saveSessionState session path = do
  state <- getSessionState session
  BL.writeFile path (Aeson.encode state)
  putStrLn $ "Session saved to " ++ path

loadSessionState :: FilePath -> IO (Maybe SessionState)
loadSessionState path = do
  exists <- doesFileExist path
  if exists
    then Aeson.decode <$> BL.readFile path
    else return Nothing

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  Right _ <- sendAndWait session SendOptions
    { message = "Remember: my project is called Starlight."
    }

  saveSessionState session "session_state.json"
```

## Resuming a Saved Session

**Scenario:** Load a previously saved session and continue the conversation.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import Copilot.SDK

loadSessionState :: FilePath -> IO (Maybe SessionState)
loadSessionState path = do
  exists <- doesFileExist path
  if exists
    then Aeson.decode <$> BL.readFile path
    else return Nothing

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  let statePath = "session_state.json"

  savedState <- loadSessionState statePath

  session <- case savedState of
    Just state -> do
      putStrLn "Resuming saved session..."
      Right s <- createSession client SessionConfig
        { systemPrompt = "You are a helpful assistant."
        , sessionState = Just state
        }
      return s

    Nothing -> do
      putStrLn "Starting fresh session..."
      Right s <- createSession client SessionConfig
        { systemPrompt = "You are a helpful assistant."
        , sessionState = Nothing
        }
      return s

  Right response <- sendAndWait session SendOptions
    { message = "What is my project called?"
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Session Store Module

**Scenario:** Manage multiple named sessions with file-based persistence.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module SessionStore
  ( SessionStore
  , newStore
  , saveSession
  , loadSession
  , deleteSession
  , listSessions
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         removeFile, listDirectory)
import System.FilePath ((</>), takeBaseName)
import Copilot.SDK

data SessionStore = SessionStore { storeDir :: FilePath }

newStore :: FilePath -> IO SessionStore
newStore dir = do
  createDirectoryIfMissing True dir
  return SessionStore { storeDir = dir }

sessionPath :: SessionStore -> String -> FilePath
sessionPath store name = storeDir store </> (name ++ ".json")

saveSession :: SessionStore -> String -> CopilotSession -> IO ()
saveSession store name session = do
  state <- getSessionState session
  BL.writeFile (sessionPath store name) (Aeson.encode state)

loadSession :: SessionStore -> String -> IO (Maybe SessionState)
loadSession store name = do
  let path = sessionPath store name
  exists <- doesFileExist path
  if exists
    then Aeson.decode <$> BL.readFile path
    else return Nothing

deleteSession :: SessionStore -> String -> IO ()
deleteSession store name = do
  let path = sessionPath store name
  exists <- doesFileExist path
  if exists then removeFile path else return ()

listSessions :: SessionStore -> IO [String]
listSessions store = do
  files <- listDirectory (storeDir store)
  return [takeBaseName f | f <- files, ".json" `isSuffixOf` f]

-- Usage
main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  store <- newStore "./sessions"

  -- Create and save
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a project planner." }

  Right _ <- sendAndWait session SendOptions
    { message = "We are building a Haskell web framework." }

  saveSession store "web-framework" session

  -- Restore
  mState <- loadSession store "web-framework"
  case mState of
    Just state -> do
      Right restored <- createSession client SessionConfig
        { systemPrompt = "You are a project planner."
        , sessionState = Just state
        }
      Right resp <- sendAndWait restored SendOptions
        { message = "What are we building?" }
      putStrLn $ messageContent resp

    Nothing ->
      putStrLn "No saved session found."

  names <- listSessions store
  putStrLn $ "Saved sessions: " ++ show names
```

## Auto-Save with a Wrapper

**Scenario:** Automatically save session state after every message exchange.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Copilot.SDK

sendAndSave :: CopilotSession -> String -> FilePath -> IO (Either CopilotError CopilotResponse)
sendAndSave session msg savePath = do
  result <- sendAndWait session SendOptions { message = msg }
  case result of
    Right response -> do
      state <- getSessionState session
      BL.writeFile savePath (Aeson.encode state)
      return (Right response)
    Left err -> return (Left err)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant." }

  let savePath = "autosave.json"

  Right _ <- sendAndSave session "Hello!" savePath
  Right resp <- sendAndSave session "What did I just say?" savePath

  putStrLn $ "Response: " ++ messageContent resp
  putStrLn $ "Auto-saved to " ++ savePath
```

## Session Envelope with Metadata

**Scenario:** Store metadata (timestamps, description) alongside session state.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import Copilot.SDK

data SessionEnvelope = SessionEnvelope
  { envState       :: SessionState
  , envSavedAt     :: String
  , envDescription :: String
  , envTurnCount   :: Int
  } deriving (Generic, Show)

instance ToJSON SessionEnvelope
instance FromJSON SessionEnvelope

saveWithMetadata :: CopilotSession -> FilePath -> String -> Int -> IO ()
saveWithMetadata session path description turnCount = do
  state <- getSessionState session
  now <- getCurrentTime
  let envelope = SessionEnvelope
        { envState = state
        , envSavedAt = show now
        , envDescription = description
        , envTurnCount = turnCount
        }
  BL.writeFile path (encode envelope)

loadWithMetadata :: FilePath -> IO (Maybe SessionEnvelope)
loadWithMetadata path = decode <$> BL.readFile path

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant." }

  Right _ <- sendAndWait session SendOptions { message = "Hello!" }

  saveWithMetadata session "envelope.json" "Cookbook demo" 1

  mEnvelope <- loadWithMetadata "envelope.json"
  case mEnvelope of
    Just env -> do
      putStrLn $ "Saved at: " ++ envSavedAt env
      putStrLn $ "Description: " ++ envDescription env
      putStrLn $ "Turns: " ++ show (envTurnCount env)
    Nothing ->
      putStrLn "Failed to load envelope."
```

## Best Practices

1. **Use `aeson`** for JSON serialization/deserialization of session state.
2. **Use `bracket`** (via `withCopilotClient`) to ensure cleanup even when saving fails.
3. **Auto-save after every turn** in production to minimize data loss.
4. **Store metadata** in a wrapper type alongside session state for easier management.
5. **Derive `Generic`, `ToJSON`, and `FromJSON`** for envelope types to reduce boilerplate.
6. **Check `doesFileExist`** before loading to handle missing files gracefully.
7. **Use `createDirectoryIfMissing True`** to ensure the storage directory exists before writing.
