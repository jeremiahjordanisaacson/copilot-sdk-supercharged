# Copilot Supercharged Haskell SDK

A Haskell SDK for interacting with the GitHub Copilot CLI server via JSON-RPC 2.0 over stdio.

## Overview

This SDK enables Haskell applications to communicate with the Copilot CLI server, create and manage conversation sessions, register custom tools, handle permissions and user input, and subscribe to session events.

The SDK follows the same patterns as all other SDKs in this repository (see the [full list of 40 supported languages](../README.md#available-sdks)).

## Architecture

```
┌────────────────────┐         stdio (JSON-RPC 2.0)         ┌─────────────────┐
│   Haskell Client   │ ◄────── Content-Length framing ──────► │  Copilot CLI    │
│                    │                                       │  (--headless    │
│  CopilotClient     │  Requests:  session.create/send/etc   │   --stdio)      │
│  CopilotSession    │  Server→:  tool.call, permission.*   │                 │
│  JsonRpcClient     │  Notifs:   session.event             │                 │
└────────────────────┘                                       └─────────────────┘
```

### Modules

| Module | Description |
|--------|-------------|
| `Copilot.Client` | Main entry point. Spawns CLI process, manages sessions. |
| `Copilot.Session` | Session operations: send, sendAndWait, events, lifecycle. |
| `Copilot.JsonRpc` | JSON-RPC 2.0 client with Content-Length header framing. |
| `Copilot.Types` | All data types with Aeson JSON instances. |
| `Copilot.DefineTool` | Helper functions for defining tools. |
| `Copilot.SdkProtocolVersion` | Protocol version constant (must match server). |
| `Copilot.SessionParams` | Translates session options into the grouped SDK protocol 4 payload. |

## Installation

Add to your `cabal.project` or use as a local dependency:

```cabal
build-depends: copilot-sdk-supercharged
```

### Dependencies

- `aeson` - JSON serialization/deserialization
- `text` - Text type
- `bytestring` - Binary I/O for Content-Length framing
- `process` - Spawning the CLI subprocess
- `async` - Concurrent operations
- `stm` - Thread-safe state (TVar, TMVar)
- `uuid` - Generating JSON-RPC request IDs
- `containers` - Map for pending requests and handlers

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.Client
import Copilot.Session
import Copilot.Types

main :: IO ()
main = do
  -- Create and start client
  client <- newCopilotClient defaultClientOptions
  startClient client

  -- Create a session
  session <- createSession client defaultSessionConfig

  -- Subscribe to events
  unsubscribe <- onSessionEvent session $ \evt ->
    case seType evt of
      "assistant.message" -> putStrLn $ "Assistant: " ++ show (seData evt)
      _ -> pure ()

  -- Send a message and wait for response
  response <- sendAndWait session
    (defaultMessageOptions "What is 2+2?")
    Nothing  -- default 60s timeout

  -- Clean up
  unsubscribe
  destroySession session
  _ <- stopClient client
  pure ()
```

## What's New in v3.0.0

**SDK protocol 4.** The SDK now speaks SDK protocol 4 (`sdkProtocolVersion == 4`), which requires Copilot CLI 1.0.113 or newer. `startClient` sends the `connect` handshake (falling back to `ping` on servers without it) and rejects older CLIs with `SDK protocol version mismatch: SDK supports version 4, but server reports version N. Please update your SDK or server to ensure compatibility.`

**Existing options keep working.** Keep filling the same `SessionConfig` and `ResumeSessionConfig` fields. Right before `session.create` and `session.resume`, the client translates the flat options into the protocol 4 option groups (`inference`, `tools`, `permissions`, `events`, ...) with `Copilot.SessionParams.groupSessionParams`. A legacy `scProvider` becomes the named `default` provider (with Copilot models disabled), `scEnableWebSocketResponses` moves into `github.capi`, and `scExperimentalMode` is sent as `experiments.enableExperimentalMode`. Other behavior changes:

- `destroySession` now detaches with `session.detach`; session state stays on disk so the session can be resumed.
- `send` no longer sends the legacy `moResponseFormat`; use `moResponseSchema` for structured output.
- Failed `session.create` / `session.resume` requests throw `RequestFailed` (a `CopilotClientError`, now exported) carrying the server's `JsonRpcError`.
- `defaultMessageOptions` builds a `MessageOptions` with only the prompt set.

### New client options and methods

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Copilot.Client
import Copilot.Types

main :: IO ()
main = do
  client <- newCopilotClient defaultClientOptions
    { ccoClientInfo = Just defaultClientInfo
        { cciEditorName = Just "my-editor", cciEditorVersion = Just "1.2.0" }
    , ccoMode       = ClientModeCodingAgent  -- or ClientModeEmpty
    , ccoNetwork    = Just defaultNetworkOptions
        { noScope        = Just "connection"
        , noResolveProxy = Just $ \_url -> pure (Just "PROXY proxy.example.com:8080; DIRECT")
        , noAuthenticateBasic = Just $ \req ->
            pure $ if nbarPreviousRejected req
              then Nothing
              else Just (NetworkBasicCredentials "proxy-user" "proxy-password")
        }
    }
  startClient client
  clearManagedSettingsCache client  -- managedSettings.clearCache
  _ <- stopClient client
  pure ()
```

- **Client info handshake** (`ccoClientInfo`): sent as `clientInfo` on `connect`; empty fields are dropped and nothing is sent when every field is empty.
- **Client mode** (`ccoMode`): `ClientModeCodingAgent` (the default, wire name `coding-agent`) keeps today's behavior. `ClientModeEmpty` requires `scAvailableTools` / `rscAvailableTools` on every create and resume, and applies conservative defaults beneath your own options (no experimental mode, telemetry, repository hooks, host git operations, or cross-session memory; in-memory OAuth and embedding storage). `parseClientMode` accepts `copilot-cli` as an alias of `coding-agent`.
- **Network callbacks** (`ccoNetwork`): `noResolveProxy`, `noAuthenticateNegotiate`, and `noAuthenticateBasic` are registered with `network.setProvider` after connecting and answer `network.resolveProxy`, `network.authenticateNegotiate`, and `network.authenticateBasic`. Tokens and passwords are never logged.
- **Managed settings cache clear**: `clearManagedSettingsCache`.

### New session options

```haskell
import qualified Data.Map.Strict as Map

session <- createSession client defaultSessionConfig
  { scAskUserVariant          = Just askUserVariantElicitation
  , scSandbox                 = Just defaultSandboxConfig { sbcEnabled = True, sbcAllowBypass = Just True }
  , scFeatureFlags            = Just (Map.fromList [("my_feature", True)])
  , scAllowedModels           = Just ["gpt-4.1", "claude-sonnet-4.5"]
  , scAuthClientIdMetadataUrl = Just "https://example.com/oauth/client-metadata.json"
  , scModelCapabilities       = Just (modelCapabilitiesWithLimits
                                  defaultModelLimitsOverride { mloMaxOutputTokens = Just 4096 })
  , scAgentDirectories        = Just ["./agents"]
  , scHookDirectories         = Just ["./hooks"]
  , scTrustWorkingDirectory   = Just trustWorkingDirectorySession  -- create only
  , scImageGeneration         = Just defaultImageGenerationConfig
                                  { igcEnabled = Just True, igcSaveToDisk = Just True }
  , scAutopilot               = Just defaultAutopilotConfig
                                  { apcContinuationEnabled = Just True, apcMaxContinues = Just 5 }
  }
```

| Option | Sent as |
|--------|---------|
| `scAskUserVariant` (`askUserVariantLegacy`, `askUserVariantElicitation`) | `interaction.askUserVariant` |
| `scSandbox` (`sbcAllowBypass` lets the agent ask to run a command outside the sandbox) | `permissions.sandbox` |
| `scFeatureFlags` | `experiments.flags` |
| `scAllowedModels` | `policy.allowedModels` |
| `scAuthClientIdMetadataUrl` | `mcp.authClientIdMetadataUrl` |
| `scModelCapabilities` with `modelCapabilitiesWithLimits` | `inference.capabilities` (`limits.max_output_tokens`) |
| `scAgentDirectories`, `scHookDirectories` | `agents.directories`, `hooks.directories` |
| `scTrustWorkingDirectory` (`trustWorkingDirectorySession`, `trustWorkingDirectoryUser`) | `filesystem.trustWorkingDirectory` |
| `scImageGeneration` | `tools.imageGeneration` |
| `scAutopilot` | `execution.autopilot` |
| `rscAllowTranscriptRecovery` (resume only) | root `allowTranscriptRecovery` |

**GitHub token provider.** Supply GitHub tokens on demand instead of a static `scGitHubToken` (the two are mutually exclusive). The SDK registers the provider under a fresh registration ID, answers `gitHubToken.getToken`, and drops the registration when the session is released:

```haskell
import qualified Data.Text as T
import System.Environment (lookupEnv)

session <- createSession client defaultSessionConfig
  { scGitHubTokenProvider = Just $ \args -> do
      -- gtpaHost args and gtpaReason args describe the request
      mToken <- lookupEnv "MY_GITHUB_TOKEN"
      pure $ case mToken of
        Just token -> GitHubTokenGranted GitHubTokenGrant
          { gtgAccessToken = T.pack token, gtgTokenType = Nothing, gtgExpiresIn = 3600 }
        Nothing -> GitHubTokenCancelled
  }
```

**Shell authentication and memory store.** Answer `sudo`, `ssh`, or `git` prompts raised by shell commands (the answer is never shown to the model), and keep memories the agent asks to store:

```haskell
import qualified Data.Text.IO as TIO

session <- createSession client defaultSessionConfig
  { scOnShellAuthenticationRequest = Just $ \req ->
      pure $ if sarProgram req == "git" && sarInputKind req == "confirm"
        then ShellAuthenticationAccept "yes"
        else ShellAuthenticationCancel
  , scOnStoreMemory = Just $ \memory ->
      TIO.appendFile "memories.txt" (stmSubject memory <> ": " <> stmFact memory <> "\n")
  }
```

**Sub-agent lifecycle hooks** (`subagentStart` / `subagentStop` through `hooks.invoke`):

```haskell
session <- createSession client defaultSessionConfig
  { scHooks = Just defaultSessionHooks
      { shOnSubagentStart = Just $ \input _inv -> do
          putStrLn ("Sub-agent starting: " ++ T.unpack (ssaAgentName input))
          pure (Just (SubagentStartHookOutput (Just "Keep answers short.")))
      , shOnSubagentStop = Just $ \input _inv -> do
          putStrLn ("Sub-agent stopped: " ++ T.unpack (sstStopReason input))
          pure Nothing
      }
  }
```

**Named BYOK providers and API key commands.** Register several provider connections and models, then select a model with `provider/id`. `npcApiKeyCommand` (and `pcApiKeyCommand` on the legacy `ProviderConfig`) runs a command for a fresh API key before each request:

```haskell
session <- createSession client defaultSessionConfig
  { scProviders = Just
      [ (defaultNamedProviderConfig "local" "http://localhost:11434/v1")
          { npcType = Just "openai", npcModelProvider = Just modelProviderOllama }
      , (defaultNamedProviderConfig "openai" "https://api.openai.com/v1")
          { npcType = Just "openai", npcApiKeyCommand = Just "op read op://dev/openai/api-key" }
      ]
  , scModels          = Just [ defaultProviderModelConfig "local" "llama3.2" ]
  , scDefaultProvider = Just "local"
  , scModel           = Just "local/llama3.2"
  , scCapi            = Just defaultCapiConfig { capiEnabled = Just False }
  }

-- Legacy single provider with an API key command
legacy <- createSession client defaultSessionConfig
  { scProvider = Just (defaultProviderConfig "https://api.openai.com/v1")
      { pcType = Just "openai", pcApiKeyCommand = Just "op read op://dev/openai/api-key" }
  , scModel    = Just "gpt-4.1"
  }

-- On resume, rscClearDefaultProvider sends an explicit null default provider
resumed <- resumeSession client (sessionId session) defaultResumeSessionConfig
  { rscAllowTranscriptRecovery = Just True
  , rscClearDefaultProvider    = True
  }
```

**Idempotent tools.** Mark tools that are safe to retry with the same arguments:

```haskell
lookupTool :: Tool
lookupTool = (defineTool "lookup_order" (Just "Read-only order lookup") Nothing $ \_args _inv ->
                pure (ToolResultText "shipped"))
  { toolIdempotent = Just True }
```

**Message source, client turn ID, and structured output:**

```haskell
import Data.Aeson (object, (.=))
import Data.Text (Text)

let schema = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object [ "answer" .= object [ "type" .= ("integer" :: Text) ] ]
      , "required"   .= (["answer"] :: [Text])
      ]
mEvt <- sendAndWait session (defaultMessageOptions "What is 6 * 7?")
  { moSource         = Just messageSourceSystem  -- or messageSourceUser, messageSourceAgent "reviewer"
  , moClientTurnId   = Just "turn-42"
  , moResponseSchema = Just schema  -- responseFormat {type: "json_schema", ...}
  } Nothing
```

**Session controls:**

```haskell
import Control.Monad (when)

_ <- switchAutoTier session (Just autoTierFast)  -- session.model.switchAutoTier
_ <- recheckPolicyHelper session                 -- session.managedSettings.recheckPolicyHelper
result <- setPermissionMode session "allow-all"  -- session.permissions.setMode ("manual", "assisted", "allow-all")
when (pmrReason result == Just permissionModeRefusalManagedPolicy) $
  putStrLn "Blocked by managed policy"
detachSession session                            -- session.detach
```

**Shell output events and Policy Helper failures:**

```haskell
import Control.Exception (try)
import qualified Data.Aeson as Aeson

_ <- onSessionEvent session $ \evt ->
  when (seType evt == sessionEventToolShellOutput) $
    case Aeson.fromJSON (seData evt) of
      Aeson.Success out -> TIO.putStr (sodText out)  -- sodStream: stdout / stderr
      Aeson.Error _     -> pure ()

created <- try (createSession client defaultSessionConfig)
case created of
  Left err -> case policyHelperFailure err of
    Just failure -> TIO.putStrLn ("Blocked by policy: " <> phfMessage failure)
    Nothing      -> putStrLn ("Create failed: " ++ show err)
  Right _ -> pure ()
```

## Known Limitations

- **Runtime-supervised AHP host** (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- **In-process native runtime hosting** is not the default connection: the Haskell SDK keeps spawning the CLI over stdio. `ccoInProcess` is kept as-is.
- **Shell authentication cancellation**: `shellAuthentication.cancel` is acknowledged, but a handler that is already running is not interrupted; the runtime ignores answers to cancelled requests.

## Defining Custom Tools

Tools allow the Copilot assistant to call functions you define:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, (.=), object)
import GHC.Generics (Generic)
import Copilot.DefineTool
import Copilot.Types

data SearchArgs = SearchArgs { query :: Text }
  deriving (Show, Generic)

instance FromJSON SearchArgs

searchTool :: Tool
searchTool = defineSimpleTool
  "search"
  (Just "Search the web")
  (Just $ object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "query" .= object [ "type" .= ("string" :: Text) ] ]
    , "required" .= (["query"] :: [Text])
    ])
  (\(SearchArgs q) _inv -> pure $ "Results for: " <> q)

-- Use in session config:
-- createSession client defaultSessionConfig { scTools = [searchTool] }
```

For full control over the result type, use `defineTool` instead of `defineSimpleTool`:

```haskell
myTool :: Tool
myTool = defineTool "my_tool" (Just "Description") Nothing $ \args inv -> do
  -- args :: Value (raw JSON)
  -- inv :: ToolInvocation (session context)
  pure $ ToolResultStruct $ ToolResultObject
    { troTextResultForLlm    = "Tool executed successfully"
    , troBinaryResultsForLlm = Nothing
    , troResultType          = Success
    , troError               = Nothing
    , troSessionLog          = Nothing
    , troToolTelemetry       = Nothing
    }
```

## Permission Handling

Register a handler to control tool permissions:

```haskell
session <- createSession client defaultSessionConfig
  { scOnPermissionRequest = Just $ \req sessionId -> do
      putStrLn $ "Permission requested: " ++ show (prKind req)
      pure $ PermissionRequestResult "approved" Nothing
  }
```

## User Input Handling

Allow the assistant to ask the user questions:

```haskell
session <- createSession client defaultSessionConfig
  { scOnUserInputRequest = Just $ \req sessionId -> do
      putStrLn $ "Question: " ++ show (uirQuestion req)
      pure $ UserInputResponse "yes" True
  }
```

## Hooks

Intercept session lifecycle events:

```haskell
session <- createSession client defaultSessionConfig
  { scHooks = Just defaultSessionHooks
      { shOnPreToolUse = Just $ \input inv -> do
          putStrLn $ "About to use tool: " ++ show (ptuToolName input)
          pure $ Just PreToolUseHookOutput
            { ptuoPermissionDecision       = Just "allow"
            , ptuoPermissionDecisionReason = Nothing
            , ptuoModifiedArgs             = Nothing
            , ptuoAdditionalContext        = Nothing
            , ptuoSuppressOutput           = Nothing
            }
      }
  }
```

## Image Generation

Enable the image generation tool with `scImageGeneration`, and pass per-message
image options with `moImageOptions`:

```haskell
session <- createSession client defaultSessionConfig
  { scImageGeneration = Just defaultImageGenerationConfig
      { igcEnabled = Just True, igcSaveToDisk = Just True }
  }
response <- sendAndWait session (defaultMessageOptions "Generate a sunset over mountains")
    { moImageOptions = Just ImageOptions
        { imageOptionsSize    = Just "1024x1024"
        , imageOptionsQuality = Just "hd"
        , imageOptionsStyle   = Just "natural"
        }
    } (Just 60000000)
```

`moResponseFormat` is kept for source compatibility but is no longer sent with
SDK protocol 4.

## Event Handling

Subscribe to specific event types:

```haskell
unsubscribe <- onSessionEvent session $ \evt ->
  case seType evt of
    "session.start"           -> putStrLn "Session started"
    "assistant.message"       -> putStrLn "Got assistant message"
    "assistant.message_delta" -> putStrLn "Streaming delta..."
    "tool.execution_start"    -> putStrLn "Tool executing..."
    "tool.execution_complete" -> putStrLn "Tool done"
    "session.idle"            -> putStrLn "Session idle"
    "session.error"           -> putStrLn "Session error!"
    _                         -> pure ()
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```haskell
client <- newCopilotClient defaultOptions { sessionIdleTimeoutSeconds = Just 300 }
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```haskell
client <- newCopilotClient defaultOptions
    { sessionFs = Just SessionFsConfig
        { initialCwd = "/repo"
        , sessionStatePath = "/tmp/state"
        , conventions = Posix
        }
    }
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```haskell
meta <- getSessionMetadata client "session-123"
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```haskell
session <- createSession client SessionConfig
    { skillDirectories = ["./skills"]
    , disabledSkills = ["test-skill"]
    , includeSubAgentStreamingEvents = Just True
    }
```

- `skillDirectories` - directories to scan for skill definitions
- `disabledSkills` - skills to exclude from the session
- `includeSubAgentStreamingEvents` - receive streaming events from sub-agents

## Client Options

```haskell
let opts = defaultClientOptions
      { ccoCliPath    = Just "/path/to/copilot"
      , ccoLogLevel   = "debug"
      , ccoAutoStart  = True
      , ccoGithubToken = Just "ghp_xxx"
      }
client <- newCopilotClient opts
```

## Recent Features (v2.4–v2.5)

Options added in the v2.4 and v2.5 upstream syncs. These are optional
`SessionConfig` fields (accessors prefixed `sc`), `CopilotClientOptions` fields
(prefixed `cco`), or `MessageOptions` fields (prefixed `mo`).

**v2.5.0**

- Reasoning effort — `scReasoningEffort :: Maybe Text`
- Tool search (discover tools on demand) — `scToolSearch :: Maybe Value`
- Session rewind — `scRewindEnabled :: Maybe Bool`
- Additional directories — `scAdditionalDirectories :: Maybe [Text]`
- Disabled MCP servers — `scDisabledMcpServers :: Maybe [Text]`
- GitHub MCP tool config — `scGithubMcpToolConfig`
- Canvas provider — `scCanvasProvider`
- Custom agents local-only — `scCustomAgentsLocalOnly :: Maybe Bool`
- Experimental mode — `scExperimentalMode :: Maybe Bool`
- Content exclusion — `scContentExclusion :: Maybe Bool`
- User-prompt-transformed hook — `shOnUserPromptTransformed`
- Permission decision context — `ptuoDecisionContext`
- Agent-factory args schema — `cacArgsSchema`
- Built-in plugin directories — `ccoBuiltinPluginDirectories :: Maybe [Text]`
- In-process FFI transport — `ccoInProcess :: Maybe Bool`

**v2.4.0**

- BYOK bearer-token provider — `ccoBearerTokenProvider :: Maybe BearerTokenProvider`
- MCP OAuth token handler — `scOnMcpAuthRequest :: Maybe McpAuthHandler`
- HTTP request handler — `ccoRequestHandler :: Maybe CopilotRequestHandler`
- Session citations — `scEnableCitations :: Maybe Bool`
- Excluded built-in agents — `scExcludedBuiltinAgents :: Maybe [Text]`
- Session spending limits — `scSessionLimits :: Maybe SessionLimitsConfig` (`slcMaxAiCredits`)
- Session memory — `scMemory :: Maybe MemoryConfiguration`
- OTLP protocol — `scOtlpProtocol :: Maybe Text`
- WebSocket responses — `scEnableWebSocketResponses :: Maybe Bool`
- Experiment assignments — `scExpAssignments`
- Tool defer loading — `toolDeferAuto`, `toolDeferNever`
- System-message sections — `systemMessageSectionPreamble`, `systemMessageSectionPreserve`
- Post-tool-use / pre-MCP hooks — `shOnPostToolUse`, `shOnPreMcpToolCall`
- Message agent mode / display prompt — `moAgentMode`, `moDisplayPrompt`
- GitHub attachment variants — `attachmentGitHubCommit`, `attachmentGitHubRepository`

Reasoning effort, content exclusion, extra directories, and rewind:

```haskell
session <- createSession client defaultSessionConfig
  { scReasoningEffort       = Just "high"        -- e.g. "low" | "medium" | "high"
  , scContentExclusion      = Just True          -- honor content-exclusion rules
  , scAdditionalDirectories = Just ["../shared", "../docs"]
  , scRewindEnabled         = Just True          -- allow rewinding to an earlier checkpoint
  , scDisabledMcpServers    = Just ["playwright"]
  }
```

Tool search and experimental mode:

```haskell
import Data.Aeson (object, (.=))

session <- createSession client defaultSessionConfig
  { scToolSearch       = Just (object ["enabled" .= True])
  , scExperimentalMode = Just True
  }
```

Session spending limits and citations:

```haskell
session <- createSession client defaultSessionConfig
  { scSessionLimits   = Just SessionLimitsConfig { slcMaxAiCredits = Just 5.0 }
  , scEnableCitations = Just True
  , scOtlpProtocol    = Just "http/protobuf"
  }
```

BYOK bearer-token provider and in-process transport (client options):

```haskell
let opts = defaultClientOptions
      { ccoBearerTokenProvider      = Just $ \args ->
          pure "ghs_ephemeral_token"        -- scoped to (ptaSessionId args)
      , ccoBuiltinPluginDirectories = Just ["./plugins"]
      , ccoInProcess                = Just False
      }
client <- newCopilotClient opts
```

## JSON-RPC Protocol

The SDK communicates using JSON-RPC 2.0 with Content-Length header framing
(the same framing used by the Language Server Protocol):

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":"uuid","method":"session.create","params":{...}}
```

### Client-to-Server Methods

| Method | Description |
|--------|-------------|
| `connect` | Handshake (with optional `clientInfo`), returns protocol version |
| `ping` | Health check, returns protocol version (handshake fallback) |
| `status.get` | Get CLI version and protocol info |
| `auth.getStatus` | Get authentication status |
| `models.list` | List available models |
| `network.setProvider` | Register network callbacks (`ccoNetwork`) |
| `managedSettings.clearCache` | Clear cached managed settings |
| `session.create` | Create a new session (grouped protocol 4 options) |
| `session.resume` | Resume an existing session (grouped protocol 4 options) |
| `session.send` | Send a message to a session |
| `session.detach` | Detach from a session (`destroySession`, `detachSession`) |
| `session.delete` | Permanently delete a session |
| `session.list` | List all sessions |
| `session.getMessages` | Get session history |
| `session.abort` | Abort current processing |
| `session.getLastId` | Get the last session ID |
| `session.model.switchAutoTier` | Change the Auto routing tier |
| `session.managedSettings.recheckPolicyHelper` | Re-run the policy helper |
| `session.permissions.setMode` | Set the permission mode |

### Server-to-Client Requests

| Method | Description |
|--------|-------------|
| `tool.call` | Execute a registered tool |
| `permission.request` | Request permission for an operation |
| `userInput.request` | Request user input |
| `hooks.invoke` | Invoke a session hook (including `subagentStart` / `subagentStop`) |
| `exitPlanMode.request` | Ask to leave plan mode |
| `gitHubToken.getToken` | Ask a GitHub token provider for a token |
| `shellAuthentication.request` / `shellAuthentication.cancel` | Answer (or cancel) a shell authentication prompt |
| `memoryStore.store` | Store a memory |
| `network.resolveProxy`, `network.authenticateNegotiate`, `network.authenticateBasic` | Network callbacks |

### Server-to-Client Notifications

| Method | Description |
|--------|-------------|
| `session.event` | Session event (message, idle, error, etc.) |
| `session.lifecycle` | Session lifecycle event (created, deleted, etc.) |

## Building

```bash
cabal build
```

## Running the Example

```bash
cabal run copilot-basic-example
```

## Protocol Version

The SDK protocol version is `4` (`Copilot.SdkProtocolVersion.sdkProtocolVersion`),
which requires Copilot CLI 1.0.113 or newer. On startup the client sends the
`connect` handshake (falling back to `ping` on servers without it) and rejects
servers that report a different version.

## Thread Safety

- The JSON-RPC client uses STM (`TVar`, `TMVar`) for thread-safe pending request tracking
- A write lock (`TMVar`) ensures atomic message writing to the CLI stdin
- Session event handlers are called synchronously; handler exceptions are caught and ignored
- The async reader thread runs continuously in the background, dispatching messages
