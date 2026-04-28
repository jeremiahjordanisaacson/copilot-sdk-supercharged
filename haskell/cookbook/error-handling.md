# Error Handling - Haskell

Patterns for handling errors gracefully in the Copilot SDK for Haskell, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with Either

**Scenario:** Use the `Either` type to handle SDK calls that may fail.

```haskell
import Copilot.SDK (newCopilotClient, startClient, stopClient,
                     createSession, sendAndWait,
                     ClientOptions(..), SessionConfig(..), SendOptions(..))

main :: IO ()
main = do
  let opts = defaultClientOptions
  client <- newCopilotClient opts

  result <- startClient client
  case result of
    Left err -> putStrLn $ "Failed to start client: " ++ show err
    Right _  -> do
      sessionResult <- createSession client SessionConfig
        { systemPrompt = "You are a helpful assistant."
        }

      case sessionResult of
        Left err -> putStrLn $ "Failed to create session: " ++ show err
        Right session -> do
          response <- sendAndWait session SendOptions
            { message = "Hello!"
            }
          case response of
            Left err  -> putStrLn $ "Send failed: " ++ show err
            Right msg -> putStrLn $ "Response: " ++ messageContent msg

      stopClient client
```

## Using ExceptT for Monadic Error Handling

**Scenario:** Chain multiple SDK calls cleanly using the `ExceptT` monad transformer.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (ExceptT, runExceptT, liftEither, liftIO)
import Copilot.SDK

type CopilotAction a = ExceptT CopilotError IO a

liftCopilot :: IO (Either CopilotError a) -> CopilotAction a
liftCopilot action = do
  result <- liftIO action
  liftEither result

runConversation :: CopilotAction String
runConversation = do
  client <- liftCopilot $ newCopilotClient defaultClientOptions
  liftCopilot $ startClient client

  session <- liftCopilot $ createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  response <- liftCopilot $ sendAndWait session SendOptions
    { message = "Explain monads in simple terms."
    }

  liftIO $ stopClient client
  return (messageContent response)

main :: IO ()
main = do
  result <- runExceptT runConversation
  case result of
    Left err  -> putStrLn $ "Error: " ++ show err
    Right msg -> putStrLn $ "Response: " ++ msg
```

## Resource Cleanup with bracket

**Scenario:** Ensure the client is always stopped, even when exceptions occur.

```haskell
import Control.Exception (bracket, SomeException, catch)
import Copilot.SDK

withCopilotClient :: ClientOptions -> (CopilotClient -> IO a) -> IO a
withCopilotClient opts action =
  bracket
    -- Acquire: create and start client
    (do
      client <- newCopilotClient opts
      startClient client
      return client
    )
    -- Release: always stop client
    (\client -> do
      stopClient client
      putStrLn "Client stopped."
    )
    -- Use: run the action
    action

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  Right response <- sendAndWait session SendOptions
    { message = "Explain bracket in Haskell."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Retry with Configurable Attempts

**Scenario:** Retry transient failures with a configurable number of attempts and delay.

```haskell
import Control.Concurrent (threadDelay)
import Copilot.SDK

data RetryConfig = RetryConfig
  { maxAttempts  :: Int
  , delayMicros  :: Int  -- microseconds between retries
  }

defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { maxAttempts = 3
  , delayMicros = 2000000  -- 2 seconds
  }

retry :: RetryConfig -> IO (Either e a) -> IO (Either e a)
retry config action = go 1
  where
    go attempt = do
      result <- action
      case result of
        Right _ -> return result
        Left err
          | attempt >= maxAttempts config -> return (Left err)
          | otherwise -> do
              putStrLn $ "Attempt " ++ show attempt ++ "/" ++ show (maxAttempts config)
                ++ " failed. Retrying..."
              threadDelay (delayMicros config)
              go (attempt + 1)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  result <- retry defaultRetryConfig $
    sendAndWait session SendOptions
      { message = "Hello!"
      }

  case result of
    Right response -> putStrLn $ "Response: " ++ messageContent response
    Left err       -> putStrLn $ "All retries failed: " ++ show err
```

## Catching Specific Exception Types

**Scenario:** Use Haskell's exception system to catch and handle different error categories.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException, SomeException, try)
import Copilot.SDK

data ErrorCategory
  = TimeoutError String
  | ConnectionError String
  | AuthError String
  | UnknownError String
  deriving (Show)

classifyError :: CopilotError -> ErrorCategory
classifyError err
  | "timeout"    `isInfixOf` msg = TimeoutError msg
  | "connection" `isInfixOf` msg = ConnectionError msg
  | "refused"    `isInfixOf` msg = ConnectionError msg
  | "auth"       `isInfixOf` msg = AuthError msg
  | otherwise                    = UnknownError msg
  where
    msg = map toLower (show err)
    isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

handleError :: ErrorCategory -> IO ()
handleError (TimeoutError msg)    = putStrLn $ "Timeout: " ++ msg ++ ". Increase timeout."
handleError (ConnectionError msg) = putStrLn $ "Connection: " ++ msg ++ ". Is CLI running?"
handleError (AuthError msg)       = putStrLn $ "Auth: " ++ msg ++ ". Check credentials."
handleError (UnknownError msg)    = putStrLn $ "Unknown error: " ++ msg

main :: IO ()
main = do
  result <- try $ withCopilotClient defaultClientOptions $ \client -> do
    Right session <- createSession client SessionConfig
      { systemPrompt = "You are a helpful assistant." }
    Right response <- sendAndWait session SendOptions
      { message = "Hello!" }
    putStrLn $ messageContent response

  case result of
    Right ()                       -> return ()
    Left (err :: SomeException)    -> putStrLn $ "Caught exception: " ++ show err
```

## Best Practices

1. **Use `bracket`** for resource management to guarantee cleanup on all exit paths.
2. **Use `ExceptT`** for clean monadic error chaining across multiple SDK calls.
3. **Pattern match on `Either`** values rather than using partial functions like `fromRight`.
4. **Implement retry logic** with configurable attempts and delays for transient failures.
5. **Classify errors** into domain types for targeted recovery strategies.
6. **Wrap SDK actions in `try`** when you need to catch unexpected exceptions.
7. **Use `withCopilotClient`** as your standard entry point to ensure clients are always stopped.
