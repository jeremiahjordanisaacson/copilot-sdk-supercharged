# Multiple Sessions - Haskell

Patterns for managing multiple independent Copilot conversations simultaneously in Haskell.

## Basic Multi-Session Setup

**Scenario:** Run multiple independent conversations from a single client, each with its own system prompt.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right codeSession <- createSession client SessionConfig
    { systemPrompt = "You are an expert Haskell programmer. Provide concise code."
    }

  Right docsSession <- createSession client SessionConfig
    { systemPrompt = "You are a technical writer for Haskell libraries."
    }

  -- Each session maintains independent conversation history
  Right codeResp <- sendAndWait codeSession SendOptions
    { message = "Write a function to transpose a matrix using lists." }
  putStrLn $ "Code:\n" ++ messageContent codeResp

  Right docsResp <- sendAndWait docsSession SendOptions
    { message = "Write Haddock docs for a matrix transposition function." }
  putStrLn $ "Docs:\n" ++ messageContent docsResp
```

## Session Registry with IORef

**Scenario:** Manage a dynamic set of named sessions using a mutable map.

```haskell
import qualified Data.Map.Strict as Map
import Data.IORef
import Copilot.SDK

data SessionRegistry = SessionRegistry
  { registryClient   :: CopilotClient
  , registrySessions :: IORef (Map.Map String CopilotSession)
  }

newRegistry :: CopilotClient -> IO SessionRegistry
newRegistry client = do
  ref <- newIORef Map.empty
  return SessionRegistry
    { registryClient = client
    , registrySessions = ref
    }

getOrCreateSession :: SessionRegistry -> String -> SessionConfig -> IO (Either CopilotError CopilotSession)
getOrCreateSession reg name config = do
  sessions <- readIORef (registrySessions reg)
  case Map.lookup name sessions of
    Just session -> return (Right session)
    Nothing -> do
      result <- createSession (registryClient reg) config
      case result of
        Right session -> do
          modifyIORef' (registrySessions reg) (Map.insert name session)
          return (Right session)
        Left err -> return (Left err)

sendToSession :: SessionRegistry -> String -> String -> IO (Either CopilotError CopilotResponse)
sendToSession reg name msg = do
  sessions <- readIORef (registrySessions reg)
  case Map.lookup name sessions of
    Nothing      -> return (Left $ CopilotError "Session not found")
    Just session -> sendAndWait session SendOptions { message = msg }

removeSession :: SessionRegistry -> String -> IO ()
removeSession reg name =
  modifyIORef' (registrySessions reg) (Map.delete name)

listSessions :: SessionRegistry -> IO [String]
listSessions reg = do
  sessions <- readIORef (registrySessions reg)
  return (Map.keys sessions)

-- Usage
main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  reg <- newRegistry client

  _ <- getOrCreateSession reg "frontend" SessionConfig
    { systemPrompt = "You are a frontend expert." }

  _ <- getOrCreateSession reg "backend" SessionConfig
    { systemPrompt = "You are a backend expert." }

  Right resp <- sendToSession reg "frontend" "How do I use React hooks?"
  putStrLn $ messageContent resp

  names <- listSessions reg
  putStrLn $ "Active sessions: " ++ show names
```

## Concurrent Sessions with Async

**Scenario:** Send messages to multiple sessions concurrently and collect all results.

```haskell
import Control.Concurrent.Async (mapConcurrently)
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  let configs =
        [ ("concise",  "Give answers in under 50 words.")
        , ("detailed", "Give thorough, detailed explanations.")
        , ("creative", "Use analogies and metaphors.")
        ]

  -- Create sessions
  sessions <- mapM (\(name, prompt) -> do
    Right session <- createSession client SessionConfig { systemPrompt = prompt }
    return (name, session)
    ) configs

  let question = "What is a monad?"

  -- Send to all sessions concurrently
  results <- mapConcurrently (\(name, session) -> do
    result <- sendAndWait session SendOptions { message = question }
    case result of
      Right resp -> return (name, messageContent resp)
      Left err   -> return (name, "Error: " ++ show err)
    ) sessions

  -- Print all results
  mapM_ (\(name, msg) -> do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn msg
    putStrLn ""
    ) results
```

## Per-User Sessions with MVar

**Scenario:** Use an MVar for thread-safe per-user session management in a concurrent application.

```haskell
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Copilot.SDK

data UserSessions = UserSessions
  { usClient   :: CopilotClient
  , usSessions :: MVar (Map.Map String CopilotSession)
  }

newUserSessions :: CopilotClient -> IO UserSessions
newUserSessions client = do
  mvar <- newMVar Map.empty
  return UserSessions { usClient = client, usSessions = mvar }

userChat :: UserSessions -> String -> String -> IO (Either CopilotError CopilotResponse)
userChat us userId msg = do
  session <- modifyMVar (usSessions us) $ \sessions ->
    case Map.lookup userId sessions of
      Just s  -> return (sessions, s)
      Nothing -> do
        Right s <- createSession (usClient us) SessionConfig
          { systemPrompt = "You are a helpful assistant for user " ++ userId ++ "." }
        return (Map.insert userId s sessions, s)

  sendAndWait session SendOptions { message = msg }

endUserSession :: UserSessions -> String -> IO ()
endUserSession us userId =
  modifyMVar_ (usSessions us) (return . Map.delete userId)

activeUsers :: UserSessions -> IO [String]
activeUsers us = do
  sessions <- readMVar (usSessions us)
  return (Map.keys sessions)

-- Usage
main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  us <- newUserSessions client

  Right resp1 <- userChat us "alice" "How do I use lenses in Haskell?"
  putStrLn $ "Alice: " ++ messageContent resp1

  Right resp2 <- userChat us "bob" "How do I use parsec?"
  putStrLn $ "Bob: " ++ messageContent resp2

  users <- activeUsers us
  putStrLn $ "Active users: " ++ show users

  endUserSession us "alice"
```

## Round-Robin Across Sessions

**Scenario:** Distribute requests across a pool of sessions in round-robin order.

```haskell
import Data.IORef
import qualified Data.Vector as V
import Copilot.SDK

data RoundRobinPool = RoundRobinPool
  { poolSessions :: V.Vector CopilotSession
  , poolIndex    :: IORef Int
  }

newPool :: CopilotClient -> [SessionConfig] -> IO RoundRobinPool
newPool client configs = do
  sessions <- mapM (\cfg -> do
    Right s <- createSession client cfg
    return s
    ) configs
  ref <- newIORef 0
  return RoundRobinPool
    { poolSessions = V.fromList sessions
    , poolIndex = ref
    }

poolSend :: RoundRobinPool -> String -> IO (Either CopilotError CopilotResponse, Int)
poolSend pool msg = do
  idx <- atomicModifyIORef' (poolIndex pool) $ \i ->
    let next = (i + 1) `mod` V.length (poolSessions pool)
    in (next, i)

  let session = poolSessions pool V.! idx
  result <- sendAndWait session SendOptions { message = msg }
  return (result, idx)

-- Usage
main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  pool <- newPool client
    [ SessionConfig { systemPrompt = "Be concise." }
    , SessionConfig { systemPrompt = "Be detailed." }
    , SessionConfig { systemPrompt = "Be creative." }
    ]

  let questions = ["What is a functor?", "What is an applicative?", "What is a monad?"]

  mapM_ (\q -> do
    (Right resp, idx) <- poolSend pool q
    putStrLn $ "Session " ++ show idx ++ " answered '" ++ q ++ "':"
    putStrLn $ messageContent resp
    putStrLn ""
    ) questions
```

## Best Practices

1. **Use `IORef` for single-threaded** session registries and `MVar` for thread-safe concurrent access.
2. **Use `mapConcurrently`** from the `async` package to send messages to multiple sessions in parallel.
3. **Share a single client** across sessions to reuse the underlying connection.
4. **Use `bracket`** (via `withCopilotClient`) to guarantee cleanup on all exit paths.
5. **Keep sessions in a `Map`** for efficient lookup by name or user ID.
6. **Use `atomicModifyIORef'`** for thread-safe round-robin index updates.
7. **Remove sessions from the map** when they are no longer needed to free resources.
