# Advanced v2.0 Features - Haskell

Recipes for v2.0 SDK features in Haskell: per-session auth, SessionFs, commands, system prompts, skills, config discovery, image generation, and more.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth instead of a global token.

```haskell
import Copilot.SDK
import System.Environment (getEnv)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  tokenA <- getEnv "GITHUB_TOKEN_USER_A"
  tokenB <- getEnv "GITHUB_TOKEN_USER_B"

  Right sessionA <- createSession client SessionConfig
    { githubToken = Just tokenA
    , systemPrompt = "You are a helpful assistant."
    }

  Right response <- sendAndWait sessionA SendOptions
    { message = "Summarize my recent pull requests."
    }

  putStrLn $ "Response: " ++ messageContent response

  -- Create a second session with a different user token
  Right sessionB <- createSession client SessionConfig
    { githubToken = Just tokenB
    , systemPrompt = "You are a code reviewer."
    }

  pure ()
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { sessionIdleTimeoutSeconds = Just 300  -- 5 minutes
    , systemPrompt = "You are a helpful assistant."
    }

  Right response <- sendAndWait session SendOptions
    { message = "Hello!"
    }

  putStrLn $ "Response: " ++ messageContent response

  -- Session automatically expires after 300s of inactivity.
  -- Sending a message after timeout returns Left SessionExpired.
```

## SessionFs (Session Filesystem)

**Scenario:** Configure a session filesystem provider with I/O operations for file-based context.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import Copilot.SDK.SessionFs
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

-- Define a SessionFs provider with 10 I/O operations
mySessionFsProvider :: SessionFsProvider
mySessionFsProvider = SessionFsProvider
  { readFile       = \path -> do
      exists <- doesFileExist path
      if exists
        then Right <$> BS.readFile path
        else pure $ Left "File not found"
  , writeFile      = \path content -> do
      BS.writeFile path content
      pure $ Right ()
  , listDirectory  = \path -> do
      exists <- doesDirectoryExist path
      if exists
        then Right <$> System.Directory.listDirectory path
        else pure $ Left "Directory not found"
  , createDirectory = \path -> do
      createDirectoryIfMissing True path
      pure $ Right ()
  , deleteFile     = \path -> do
      removeFile path
      pure $ Right ()
  , fileExists     = \path -> Right <$> doesFileExist path
  , getFileInfo    = \path -> do
      exists <- doesFileExist path
      if exists
        then do
          size <- getFileSize path
          modTime <- getModificationTime path
          pure $ Right FileInfo { fileSize = size, fileModified = modTime }
        else pure $ Left "File not found"
  , copyFile       = \src dst -> do
      System.Directory.copyFile src dst
      pure $ Right ()
  , moveFile       = \src dst -> do
      renamePath src dst
      pure $ Right ()
  , searchFiles    = \path pattern -> do
      allFiles <- listDirectoryRecursive path
      matches <- filterM (\f -> do
        content <- BS.readFile f
        pure $ pattern `BS.isInfixOf` content) allFiles
      pure $ Right matches
  }

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a file management assistant."
    , sessionFs = Just SessionFsConfig
        { provider = mySessionFsProvider
        , rootPath = "/workspace/project"
        , writable = True
        }
    }

  Right response <- sendAndWait session SendOptions
    { message = "List all files in the current project directory."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Commands and UI Elicitation

**Scenario:** Register commands and handle UI elicitation requests from the model.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import Data.IORef

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  let commands =
        [ Command
            { commandName = "deploy"
            , commandDescription = "Deploy the application to a target environment"
            , commandParameters =
                [ CommandParam "environment" "string" True
                , CommandParam "version" "string" False
                ]
            }
        , Command
            { commandName = "rollback"
            , commandDescription = "Roll back the most recent deployment"
            , commandParameters =
                [ CommandParam "environment" "string" True
                ]
            }
        ]

  -- Handle elicitation: the model asks the user for input
  let elicitationHandler request = do
        putStrLn $ "Model asks: " ++ elicitationMessage request
        mapM_ (\(i, opt) ->
          putStrLn $ "  " ++ show i ++ ") " ++ opt)
          (zip [1..] (elicitationOptions request))
        putStr "Your choice: "
        answer <- getLine
        pure $ ElicitationResponse { elicitationAnswer = answer }

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a deployment assistant."
    , sessionCommands = commands
    , elicitationHandler = Just elicitationHandler
    }

  Right response <- sendAndWait session SendOptions
    { message = "/deploy"
    }

  putStrLn $ "Response: " ++ messageContent response
```

## System Prompt Customization

**Scenario:** Use replace and customize modes with sections to control the system prompt.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  -- Replace mode: completely replace the default system prompt
  Right sessionReplace <- createSession client SessionConfig
    { systemPrompt = SystemPromptReplace
        "You are a Haskell programming expert. Only answer Haskell questions."
    }

  -- Customize mode: add sections before/after the default prompt
  Right sessionCustom <- createSession client SessionConfig
    { systemPrompt = SystemPromptCustomize
        [ PromptSection
            { sectionPosition = Before
            , sectionContent = "You are assisting a senior Haskell developer."
            }
        , PromptSection
            { sectionPosition = After
            , sectionContent = unlines
                [ "Always prefer pure functions over IO when possible."
                , "Use type-level programming for compile-time guarantees."
                , "Cite Haskell Wiki or GHC docs when relevant."
                ]
            }
        ]
    }

  Right response <- sendAndWait sessionCustom SendOptions
    { message = "How do I implement a type-safe state machine in Haskell?"
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Per-Agent Skills

**Scenario:** Configure skill directories and disable specific skills per agent.

```haskell
import Copilot.SDK
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  homeDir <- getHomeDirectory

  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a code assistant with limited skills."
    , skillDirectories =
        [ homeDir </> ".copilot" </> "skills"
        , "/project/.copilot-skills"
        ]
    , disabledSkills =
        [ "web-search"
        , "image-generation"
        ]
    }

  Right response <- sendAndWait session SendOptions
    { message = "Refactor this function to use the Reader monad."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Per-Agent Tool Visibility

**Scenario:** Hide specific tools from the model for a given session.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a read-only code reviewer."
    , excludedTools =
        [ "file_write"
        , "shell_execute"
        , "git_push"
        ]
    }

  Right response <- sendAndWait session SendOptions
    { message = "Review this module and suggest improvements."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Runtime Request Headers

**Scenario:** Attach custom headers to individual send requests for tracing or routing.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import qualified Data.Map.Strict as Map

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  Right response <- sendAndWait session SendOptions
    { message = "Explain monad transformers in Haskell."
    , requestHeaders = Map.fromList
        [ ("X-Request-Id", "req-abc-123")
        , ("X-Trace-Parent", "00-traceid-spanid-01")
        , ("X-Custom-Routing", "priority-queue")
        ]
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Model Capabilities Override

**Scenario:** Override model capabilities for a session, such as vision or function calling.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are an assistant with extended capabilities."
    , modelCapabilities = Just ModelCapabilities
        { vision = True
        , functionCalling = True
        , jsonOutput = True
        , maxTokens = 8192
        }
    }

  Right response <- sendAndWait session SendOptions
    { message = "Analyze this architecture diagram."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Config Discovery

**Scenario:** Enable automatic discovery of project-level configuration files.

```haskell
import Copilot.SDK

main :: IO ()
main = do
  let opts = defaultClientOptions
        { enableConfigDiscovery = True
        }

  withCopilotClient opts $ \client -> do
    -- The SDK automatically scans for:
    --   .copilot/config.hs
    --   .copilot/config.json
    --   .github/copilot-config.yml
    -- in the workspace and its parents.

    Right session <- createSession client SessionConfig
      { systemPrompt = "You are a helpful assistant."
      }

    Right response <- sendAndWait session SendOptions
      { message = "What configuration is active for this project?"
      }

    putStrLn $ "Response: " ++ messageContent response
```

## Sub-Agent Streaming Events

**Scenario:** Subscribe to streaming events from sub-agents during orchestration.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import qualified Data.Text.IO as TIO

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are an orchestrator that delegates to sub-agents."
    , includeSubAgentStreamingEvents = True
    }

  send session SendOptions
    { message = "Research Haskell type classes and write a tutorial."
    , streaming = True
    , onEvent = \event -> case eventType event of
        "assistant.message_delta" ->
          TIO.putStr (eventDelta event)

        "sub_agent.start" ->
          putStrLn $ "\n[Sub-agent started: " ++ eventAgentName event ++ "]"

        "sub_agent.message_delta" ->
          TIO.putStr (eventDelta event)

        "sub_agent.end" ->
          putStrLn $ "\n[Sub-agent finished: " ++ eventAgentName event ++ "]"

        _ -> pure ()
    }
```

## Session Metadata

**Scenario:** Retrieve metadata about an active session.

```haskell
import Copilot.SDK

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a helpful assistant."
    }

  _ <- sendAndWait session SendOptions { message = "Hello!" }

  Right metadata <- getSessionMetadata session

  putStrLn $ "Session ID:  " ++ sessionMetaId metadata
  putStrLn $ "Created at:  " ++ show (sessionMetaCreatedAt metadata)
  putStrLn $ "Turn count:  " ++ show (sessionMetaTurnCount metadata)
  putStrLn $ "Model:       " ++ sessionMetaModel metadata
  putStrLn $ "Token usage: " ++ show (sessionMetaTokenUsage metadata)
```

## MCP Server Configuration

**Scenario:** Configure MCP (Model Context Protocol) servers using stdio and HTTP transports.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import qualified Data.Map.Strict as Map
import System.Environment (getEnv)

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  mcpApiKey <- getEnv "MCP_API_KEY"

  -- Stdio transport: launch a local MCP server as a child process
  Right sessionStdio <- createSession client SessionConfig
    { systemPrompt = "You are an assistant with MCP tools."
    , mcpServers =
        [ McpServerStdio
            { mcpName = "local-tools"
            , mcpCommand = "node"
            , mcpArgs = ["./mcp-server/index.js"]
            , mcpEnv = Map.fromList [("MCP_LOG_LEVEL", "info")]
            }
        ]
    }

  -- HTTP transport: connect to a remote MCP server
  Right sessionHttp <- createSession client SessionConfig
    { systemPrompt = "You are an assistant with remote tools."
    , mcpServers =
        [ McpServerHttp
            { mcpName = "remote-tools"
            , mcpUrl = "https://mcp.example.com/v1"
            , mcpHeaders = Map.fromList
                [("Authorization", "Bearer " ++ mcpApiKey)]
            }
        ]
    }

  Right response <- sendAndWait sessionStdio SendOptions
    { message = "Use the local tools to analyze the project."
    }

  putStrLn $ "Response: " ++ messageContent response
```

## Image Generation

**Scenario:** Configure the response format for image generation tasks.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.SDK
import qualified Data.ByteString as BS
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Base64 as B64

main :: IO ()
main = withCopilotClient defaultClientOptions $ \client -> do
  Right session <- createSession client SessionConfig
    { systemPrompt = "You are a creative assistant that generates images."
    , modelCapabilities = Just ModelCapabilities
        { vision = True
        , imageGeneration = True
        }
    }

  Right response <- sendAndWait session SendOptions
    { message = "Generate an image of a Haskell lambda symbol in a cosmic nebula."
    , responseFormat = Just ResponseFormat
        { formatType = "image"
        , formatSize = "1024x1024"
        , formatQuality = "high"
        }
    }

  case responseImages response of
    imgs | not (null imgs) ->
      mapM_ (\(idx, img) -> do
        putStrLn $ "Image " ++ show idx ++ " URL: " ++ imageUrl img

        -- Save base64 image data if returned inline
        case imageBase64Data img of
          Just b64 -> do
            let path = "generated_" ++ show idx ++ ".png"
            BS.writeFile path (B64.decodeLenient b64)
            putStrLn $ "Image " ++ show idx ++ " saved to " ++ path
          Nothing -> pure ()
        ) (zip [1..] imgs)

    _ -> putStrLn $ "Response: " ++ messageContent response
```
