# Tools and Skills - Haskell

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Haskell.

## Defining a Simple Tool

**Scenario:** Expose a Haskell function as a tool that the model can call during a conversation.

```haskell
import Copilot.SDK
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  defineTool "get_current_time" "Returns the current UTC date and time" $ \_params -> do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" now

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant with access to tools."
    }

  Right response <- sendAndWait session SendOptions
    { message = "What is the current time?"
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured parameters and returns structured output.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Copilot.SDK

calculate :: Aeson.Value -> IO String
calculate params = do
  let obj = case params of
              Aeson.Object o -> o
              _ -> KM.empty

  let getNum key = case KM.lookup key obj of
        Just (Aeson.Number n) -> Just (realToFrac n :: Double)
        _ -> Nothing

  let getStr key = case KM.lookup key obj of
        Just (Aeson.String s) -> Just s
        _ -> Nothing

  let mResult = do
        a <- getNum "a"
        b <- getNum "b"
        op <- getStr "operation"
        case op of
          "add"      -> Just (a + b)
          "subtract" -> Just (a - b)
          "multiply" -> Just (a * b)
          "divide"   -> if b /= 0 then Just (a / b) else Nothing
          _          -> Nothing

  case mResult of
    Just r  -> return $ "{\"result\": " ++ show r ++ "}"
    Nothing -> return "{\"error\": \"Invalid parameters or division by zero\"}"

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  defineTool "calculate" "Performs arithmetic: add, subtract, multiply, divide" calculate

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a calculator. Use the calculate tool for math."
    }

  Right response <- sendAndWait session SendOptions
    { message = "What is 42 multiplied by 17?"
    }

  putStrLn $ "Response: " ++ messageContent response
```

## File System Tools

**Scenario:** Give the model tools to read files and list directory contents.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import System.Directory (listDirectory, doesFileExist)
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  defineTool "read_file" "Reads the contents of a file" $ \params -> do
    let path = case params of
          Aeson.Object obj -> case KM.lookup "path" obj of
            Just (Aeson.String s) -> s
            _ -> ""
          _ -> ""

    exists <- doesFileExist (show path)
    if exists
      then do
        content <- readFile (show path)
        let truncated = take 10000 content
        return $ "{\"path\": " ++ show path ++ ", \"content\": " ++ show truncated ++ "}"
      else
        return $ "{\"error\": \"File not found: " ++ show path ++ "\"}"

  defineTool "list_directory" "Lists files in a directory" $ \params -> do
    let dir = case params of
          Aeson.Object obj -> case KM.lookup "directory" obj of
            Just (Aeson.String s) -> s
            _ -> "."
          _ -> "."

    entries <- listDirectory (show dir)
    let filesJson = "[" ++ intercalate ", " (map show entries) ++ "]"
    return $ "{\"directory\": " ++ show dir ++ ", \"files\": " ++ filesJson ++ "}"

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a file explorer. Use your tools to navigate."
    }

  Right response <- sendAndWait session SendOptions
    { message = "List files in the current directory."
    }

  putStrLn $ messageContent response
```

## Key-Value Store Tools with IORef

**Scenario:** Create tools backed by mutable state for a key-value store.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  store <- newIORef (Map.empty :: Map.Map String String)

  defineTool "kv_set" "Stores a value under a key" $ \params -> do
    let obj = case params of { Aeson.Object o -> o; _ -> KM.empty }
    let key = case KM.lookup "key" obj of { Just (Aeson.String s) -> s; _ -> "" }
    let val = case KM.lookup "value" obj of { Just (Aeson.String s) -> s; _ -> "" }
    modifyIORef' store (Map.insert (show key) (show val))
    return $ "{\"status\": \"ok\", \"key\": " ++ show key ++ "}"

  defineTool "kv_get" "Retrieves a value by key" $ \params -> do
    let key = case params of
          Aeson.Object obj -> case KM.lookup "key" obj of
            Just (Aeson.String s) -> show s
            _ -> ""
          _ -> ""
    m <- readIORef store
    case Map.lookup key m of
      Just v  -> return $ "{\"key\": " ++ show key ++ ", \"value\": " ++ show v ++ "}"
      Nothing -> return $ "{\"error\": \"Key not found\"}"

  defineTool "kv_list" "Lists all keys in the store" $ \_params -> do
    m <- readIORef store
    let keys = Map.keys m
    let keysJson = "[" ++ intercalate ", " (map show keys) ++ "]"
    return $ "{\"keys\": " ++ keysJson ++ "}"

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a key-value store assistant." }

  Right response <- sendAndWait session SendOptions
    { message = "Store name=Alice and role=Developer, then list all keys."
    }

  putStrLn $ messageContent response
```

## Orchestrating Sub-Agents

**Scenario:** Use tool definitions to delegate work to specialized session sub-agents.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  -- Create specialized sub-agent sessions
  Right codeAgent <- createSession client SessionConfig
    { systemPrompt = "You generate Haskell code. Output only code." }

  Right reviewAgent <- createSession client SessionConfig
    { systemPrompt = "You review Haskell code for bugs. Be concise." }

  -- Define tools that delegate to sub-agents
  defineTool "generate_code" "Generates Haskell code from a description" $ \params -> do
    let request = extractString "request" params
    Right resp <- sendAndWait codeAgent SendOptions { message = request }
    return (messageContent resp)

  defineTool "review_code" "Reviews Haskell code for issues" $ \params -> do
    let code = extractString "code" params
    Right resp <- sendAndWait reviewAgent SendOptions
      { message = "Review this code:\n" ++ code }
    return (messageContent resp)

  -- Orchestrator session coordinates the sub-agents
  Right orchestrator <- createSession client SessionConfig
    { systemPrompt = "You orchestrate tasks. Use generate_code then review_code." }

  Right response <- sendAndWait orchestrator SendOptions
    { message = "Generate a safe head function for lists, then review it." }

  putStrLn $ messageContent response

-- Helper to extract a string from JSON params
extractString :: String -> Aeson.Value -> String
extractString key (Aeson.Object obj) =
  case KM.lookup (fromString key) obj of
    Just (Aeson.String s) -> unpack s
    _ -> ""
extractString _ _ = ""
```

## Best Practices

1. **Use `IORef`** for mutable state in tool handlers when running single-threaded.
2. **Use `aeson`** for JSON parameter parsing and response formatting.
3. **Handle missing parameters** gracefully by pattern matching on `Maybe` values.
4. **Limit output size** with `take` to stay within token limits.
5. **Use `bracket`** (via `withCopilotClient`) to ensure tools and sessions are cleaned up.
6. **Keep tool handlers pure where possible**, isolating IO to the minimum needed.
7. **Use sub-agent orchestration** to decompose complex tasks into specialized sessions.
