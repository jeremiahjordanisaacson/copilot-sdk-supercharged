---------------------------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
---------------------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Type definitions for the Copilot SDK.
--
-- This module contains all data types, Aeson JSON instances, and type aliases
-- used across the SDK. It mirrors the types found in the Node.js, Go, and
-- other SDK implementations.
module Copilot.Types
  ( -- * Connection state
    ConnectionState (..)

    -- * Tool types
  , ToolResultType (..)
  , ToolBinaryResult (..)
  , ToolResultObject (..)
  , ToolResult (..)
  , ToolInvocation (..)
  , ToolHandler
  , Tool (..)
  , ToolCallRequest (..)
  , ToolCallResponse (..)

    -- * Session configuration
  , SystemPromptSection (..)
  , SectionOverrideAction (..)
  , SectionOverride (..)
  , SystemMessageConfig (..)
  , ProviderConfig (..)
  , defaultProviderConfig
  , AzureProviderOptions (..)
  , MCPStdioServerConfig (..)
  , MCPHTTPServerConfig (..)
  , MCPServerConfig (..)
  , CustomAgentConfig (..)
  , InfiniteSessionConfig (..)
  , CommandContext (..)
  , CommandHandler
  , CommandDefinition (..)
  , ElicitationContext (..)
  , ElicitationResult (..)
  , ElicitationHandler
  , SessionConfig (..)
  , defaultSessionConfig
  , ResumeSessionConfig (..)
  , defaultResumeSessionConfig

    -- * Upstream-sync feature types & constants
  , SessionLimitsConfig (..)
  , MemoryConfiguration (..)
  , ProviderTokenArgs (..)
  , BearerTokenProvider
  , McpAuthHandler
  , CopilotRequestHandler
  , toolDeferAuto
  , toolDeferNever
  , systemMessageSectionPreamble
  , systemMessageSectionIdentity
  , systemMessageSectionToolInstructions
  , systemMessageSectionPreserve
  , attachmentGitHubCommit
  , attachmentGitHubRelease
  , attachmentGitHubActionsJob
  , attachmentGitHubRepository
  , attachmentGitHubFileDiff
  , attachmentGitHubTreeComparison
  , attachmentGitHubUrl
  , attachmentGitHubFile
  , attachmentGitHubSnippet

    -- * v2.6.0 upstream-sync feature types & constants
  , CopilotClientInfo (..)
  , defaultClientInfo
  , clientInfoToWire
  , autoTierEfficiency
  , autoTierBalance
  , autoTierIntelligence
  , autoTierFast
  , askUserVariantLegacy
  , askUserVariantElicitation
  , trustWorkingDirectorySession
  , trustWorkingDirectoryUser
  , MessageSource
  , messageSourceUser
  , messageSourceSystem
  , messageSourceAgent
  , modelProviderOpenAI
  , modelProviderAnthropic
  , modelProviderAzureOpenAI
  , modelProviderOllama
  , modelProviderLmStudio
  , modelProviderFoundryLocal
  , modelProviderLlamaCpp
  , GitHubTokenProviderArgs (..)
  , GitHubTokenGrant (..)
  , GitHubTokenProviderResult (..)
  , GitHubTokenProvider
  , SandboxConfig (..)
  , defaultSandboxConfig
  , ImageGenerationConfig (..)
  , defaultImageGenerationConfig
  , AutopilotConfig (..)
  , defaultAutopilotConfig
  , ModelLimitsOverride (..)
  , defaultModelLimitsOverride
  , modelCapabilitiesWithLimits
  , structuredResponseFormat

    -- * v3.0.0 / SDK protocol 4 types & constants
  , ClientMode (..)
  , parseClientMode
  , NamedProviderConfig (..)
  , defaultNamedProviderConfig
  , ProviderModelConfig (..)
  , defaultProviderModelConfig
  , CapiConfig (..)
  , defaultCapiConfig
  , ShellAuthenticationRequest (..)
  , ShellAuthenticationResponse (..)
  , ShellAuthenticationHandler
  , StoredMemory (..)
  , StoreMemoryHandler
  , NetworkBasicAuthRequest (..)
  , NetworkBasicCredentials (..)
  , NetworkOptions (..)
  , defaultNetworkOptions
  , PolicyHelperFailure (..)
  , policyHelperFailureFromErrorData
  , permissionModeRefusalManagedPolicy
  , PermissionModeResult (..)
  , sessionEventToolShellOutput
  , ShellOutputData (..)

    -- * v3.1.0 MCP secret store & plugin extension approval types
  , McpSecretsSaveResult (..)
  , PermissionsExtensionApproval (..)
  , PermissionsExtensionApprovalsGrantResult (..)
  , PermissionsExtensionApprovalsListResult (..)
  , PermissionsExtensionApprovalsRevokeResult (..)

    -- * Response format & image types
  , ResponseFormat (..)
  , ImageOptions (..)
  , AssistantImageData (..)
  , ContentBlock (..)

    -- * Message options
  , Attachment (..)
  , MessageOptions (..)
  , defaultMessageOptions

    -- * Permission types
  , PermissionRequest (..)
  , PermissionRequestResult (..)
  , PermissionHandler

    -- * User input types
  , UserInputRequest (..)
  , UserInputResponse (..)
  , UserInputHandler
  , UserInputImageAttachmentConstraints (..)
  , UserInputImageAttachment (..)
  , defaultUserInputImageAttachment
  , UserInputImageResponse (..)
  , defaultUserInputImageResponse
  , UserInputImageHandler

    -- * Exit plan mode types
  , ExitPlanModeRequest (..)
  , ExitPlanModeResponse (..)
  , ExitPlanModeHandler

    -- * Trace context types
  , TraceContext (..)
  , TraceContextProvider

    -- * Hook types
  , HookInvocation (..)
  , PreToolUseHookInput (..)
  , PreToolUseHookOutput (..)
  , PostToolUseHookInput (..)
  , PostToolUseHookOutput (..)
  , UserPromptSubmittedHookInput (..)
  , UserPromptSubmittedHookOutput (..)
  , SessionStartHookInput (..)
  , SessionStartHookOutput (..)
  , SessionEndHookInput (..)
  , SessionEndHookOutput (..)
  , ErrorOccurredHookInput (..)
  , ErrorOccurredHookOutput (..)
  , SubagentStartHookInput (..)
  , SubagentStartHookOutput (..)
  , SubagentStopHookInput (..)
  , SubagentStopHookOutput (..)
  , SessionHooks (..)
  , defaultSessionHooks

    -- * Session event types
  , SessionEvent (..)
  , SessionEventHandler

    -- * Ping / Status / Auth
  , PingResponse (..)
  , GetStatusResponse (..)
  , GetAuthStatusResponse (..)

    -- * Model types
  , ModelSupports (..)
  , ModelLimits (..)
  , ModelVisionLimits (..)
  , ModelCapabilities (..)
  , ModelPolicy (..)
  , ModelBilling (..)
  , ModelInfo (..)

    -- * Session metadata & lifecycle
  , SessionMetadata (..)
  , SessionLifecycleEvent (..)
  , SessionLifecycleHandler

    -- * Session filesystem
  , SessionFsConfig (..)
  , SessionFsFileInfo (..)
  , SessionFsProvider (..)

    -- * Client options
  , CopilotClientOptions (..)
  , defaultClientOptions
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson         hiding (Success)
import           Data.Aeson.Types   (Parser, toJSONKeyText, FromJSONKeyFunction (..))
import qualified Data.Aeson         as Aeson
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)

-- ============================================================================
-- Connection State
-- ============================================================================

-- | Current connection state of the client.
data ConnectionState
  = Disconnected
  | Connecting
  | Connected
  | ConnectionError
  deriving (Show, Eq, Generic)

instance ToJSON ConnectionState where
  toJSON Disconnected    = String "disconnected"
  toJSON Connecting      = String "connecting"
  toJSON Connected       = String "connected"
  toJSON ConnectionError = String "error"

instance FromJSON ConnectionState where
  parseJSON = withText "ConnectionState" $ \case
    "disconnected" -> pure Disconnected
    "connecting"   -> pure Connecting
    "connected"    -> pure Connected
    "error"        -> pure ConnectionError
    other          -> fail $ "Unknown ConnectionState: " ++ show other

-- ============================================================================
-- Tool Result Types
-- ============================================================================

-- | Result type classification for tool execution.
data ToolResultType = Success | Failure | Rejected | Denied
  deriving (Show, Eq, Generic)

instance ToJSON ToolResultType where
  toJSON Success  = String "success"
  toJSON Failure  = String "failure"
  toJSON Rejected = String "rejected"
  toJSON Denied   = String "denied"

instance FromJSON ToolResultType where
  parseJSON = withText "ToolResultType" $ \case
    "success"  -> pure Success
    "failure"  -> pure Failure
    "rejected" -> pure Rejected
    "denied"   -> pure Denied
    other      -> fail $ "Unknown ToolResultType: " ++ show other

-- | Binary data returned by a tool.
data ToolBinaryResult = ToolBinaryResult
  { tbrData        :: !Text
  , tbrMimeType    :: !Text
  , tbrType        :: !Text
  , tbrDescription :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ToolBinaryResult where
  toJSON ToolBinaryResult{..} = object
    [ "data"        .= tbrData
    , "mimeType"    .= tbrMimeType
    , "type"        .= tbrType
    , "description" .= tbrDescription
    ]

instance FromJSON ToolBinaryResult where
  parseJSON = withObject "ToolBinaryResult" $ \o ->
    ToolBinaryResult
      <$> o .:  "data"
      <*> o .:  "mimeType"
      <*> o .:  "type"
      <*> o .:? "description"

-- | Structured tool result object.
data ToolResultObject = ToolResultObject
  { troTextResultForLlm    :: !Text
  , troBinaryResultsForLlm :: !(Maybe [ToolBinaryResult])
  , troResultType          :: !ToolResultType
  , troError               :: !(Maybe Text)
  , troSessionLog          :: !(Maybe Text)
  , troToolTelemetry       :: !(Maybe (Map.Map Text Value))
  } deriving (Show, Eq, Generic)

instance ToJSON ToolResultObject where
  toJSON ToolResultObject{..} = object
    [ "textResultForLlm"    .= troTextResultForLlm
    , "binaryResultsForLlm" .= troBinaryResultsForLlm
    , "resultType"          .= troResultType
    , "error"               .= troError
    , "sessionLog"          .= troSessionLog
    , "toolTelemetry"       .= troToolTelemetry
    ]

instance FromJSON ToolResultObject where
  parseJSON = withObject "ToolResultObject" $ \o ->
    ToolResultObject
      <$> o .:  "textResultForLlm"
      <*> o .:? "binaryResultsForLlm"
      <*> o .:  "resultType"
      <*> o .:? "error"
      <*> o .:? "sessionLog"
      <*> o .:? "toolTelemetry"

-- | Tool result: either a simple string or a structured object.
data ToolResult
  = ToolResultText   !Text
  | ToolResultStruct !ToolResultObject
  deriving (Show, Eq)

instance ToJSON ToolResult where
  toJSON (ToolResultText t)   = toJSON t
  toJSON (ToolResultStruct o) = toJSON o

instance FromJSON ToolResult where
  parseJSON v@(String _) = ToolResultText <$> parseJSON v
  parseJSON v@(Object _) = ToolResultStruct <$> parseJSON v
  parseJSON _            = fail "ToolResult must be a string or object"

-- | Context about a tool invocation.
data ToolInvocation = ToolInvocation
  { tiSessionId  :: !Text
  , tiToolCallId :: !Text
  , tiToolName   :: !Text
  , tiArguments  :: !Value
  } deriving (Show, Eq, Generic)

-- | A tool handler function.
-- Takes arguments (as a JSON 'Value') and the invocation context, returns an IO
-- action producing a 'ToolResult'.
type ToolHandler = Value -> ToolInvocation -> IO ToolResult

-- | Tool definition with name, description, JSON schema parameters, and handler.
data Tool = Tool
  { toolName        :: !Text
  , toolDescription :: !(Maybe Text)
  , toolParameters  :: !(Maybe Value)
  , toolHandler     :: !ToolHandler
  , toolIdempotent  :: !(Maybe Bool)  -- ^ Safe to retry with the same arguments (wire: idempotent)
  }

instance Show Tool where
  show t = "Tool { name = " ++ show (toolName t)
        ++ ", description = " ++ show (toolDescription t) ++ " }"

-- | Request payload from the server for a tool call.
data ToolCallRequest = ToolCallRequest
  { tcrSessionId  :: !Text
  , tcrToolCallId :: !Text
  , tcrToolName   :: !Text
  , tcrArguments  :: !Value
  } deriving (Show, Eq, Generic)

instance FromJSON ToolCallRequest where
  parseJSON = withObject "ToolCallRequest" $ \o ->
    ToolCallRequest
      <$> o .: "sessionId"
      <*> o .: "toolCallId"
      <*> o .: "toolName"
      <*> o .: "arguments"

instance ToJSON ToolCallRequest where
  toJSON ToolCallRequest{..} = object
    [ "sessionId"  .= tcrSessionId
    , "toolCallId" .= tcrToolCallId
    , "toolName"   .= tcrToolName
    , "arguments"  .= tcrArguments
    ]

-- | Response payload for a tool call.
data ToolCallResponse = ToolCallResponse
  { tcrResult :: !ToolResult
  } deriving (Show, Eq)

instance ToJSON ToolCallResponse where
  toJSON (ToolCallResponse r) = object [ "result" .= r ]

instance FromJSON ToolCallResponse where
  parseJSON = withObject "ToolCallResponse" $ \o ->
    ToolCallResponse <$> o .: "result"

-- ============================================================================
-- System Message Configuration
-- ============================================================================

-- | Known system prompt section identifiers for the "customize" mode.
data SystemPromptSection
  = SectionIdentity
  | SectionTone
  | SectionToolEfficiency
  | SectionEnvironmentContext
  | SectionCodeChangeRules
  | SectionGuidelines
  | SectionSafety
  | SectionToolInstructions
  | SectionCustomInstructions
  | SectionLastInstructions
  deriving (Show, Eq, Ord, Generic)

instance ToJSON SystemPromptSection where
  toJSON SectionIdentity           = String "identity"
  toJSON SectionTone               = String "tone"
  toJSON SectionToolEfficiency     = String "tool_efficiency"
  toJSON SectionEnvironmentContext = String "environment_context"
  toJSON SectionCodeChangeRules    = String "code_change_rules"
  toJSON SectionGuidelines         = String "guidelines"
  toJSON SectionSafety             = String "safety"
  toJSON SectionToolInstructions   = String "tool_instructions"
  toJSON SectionCustomInstructions = String "custom_instructions"
  toJSON SectionLastInstructions   = String "last_instructions"

instance FromJSON SystemPromptSection where
  parseJSON = withText "SystemPromptSection" $ \case
    "identity"             -> pure SectionIdentity
    "tone"                 -> pure SectionTone
    "tool_efficiency"      -> pure SectionToolEfficiency
    "environment_context"  -> pure SectionEnvironmentContext
    "code_change_rules"    -> pure SectionCodeChangeRules
    "guidelines"           -> pure SectionGuidelines
    "safety"               -> pure SectionSafety
    "tool_instructions"    -> pure SectionToolInstructions
    "custom_instructions"  -> pure SectionCustomInstructions
    "last_instructions"    -> pure SectionLastInstructions
    other                  -> fail $ "Unknown SystemPromptSection: " <> T.unpack other

instance ToJSONKey SystemPromptSection where
  toJSONKey = toJSONKeyText $ \case
    SectionIdentity           -> "identity"
    SectionTone               -> "tone"
    SectionToolEfficiency     -> "tool_efficiency"
    SectionEnvironmentContext -> "environment_context"
    SectionCodeChangeRules    -> "code_change_rules"
    SectionGuidelines         -> "guidelines"
    SectionSafety             -> "safety"
    SectionToolInstructions   -> "tool_instructions"
    SectionCustomInstructions -> "custom_instructions"
    SectionLastInstructions   -> "last_instructions"

instance FromJSONKey SystemPromptSection where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "identity"             -> pure SectionIdentity
    "tone"                 -> pure SectionTone
    "tool_efficiency"      -> pure SectionToolEfficiency
    "environment_context"  -> pure SectionEnvironmentContext
    "code_change_rules"    -> pure SectionCodeChangeRules
    "guidelines"           -> pure SectionGuidelines
    "safety"               -> pure SectionSafety
    "tool_instructions"    -> pure SectionToolInstructions
    "custom_instructions"  -> pure SectionCustomInstructions
    "last_instructions"    -> pure SectionLastInstructions
    other                  -> fail $ "Unknown SystemPromptSection: " <> T.unpack other

-- | Override action for a system prompt section.
data SectionOverrideAction
  = OverrideReplace
  | OverrideRemove
  | OverrideAppend
  | OverridePrepend
  deriving (Show, Eq, Generic)

instance ToJSON SectionOverrideAction where
  toJSON OverrideReplace = String "replace"
  toJSON OverrideRemove  = String "remove"
  toJSON OverrideAppend  = String "append"
  toJSON OverridePrepend = String "prepend"

instance FromJSON SectionOverrideAction where
  parseJSON = withText "SectionOverrideAction" $ \case
    "replace" -> pure OverrideReplace
    "remove"  -> pure OverrideRemove
    "append"  -> pure OverrideAppend
    "prepend" -> pure OverridePrepend
    other     -> fail $ "Unknown SectionOverrideAction: " <> T.unpack other

-- | Override operation for a single system prompt section.
data SectionOverride = SectionOverride
  { soAction  :: !SectionOverrideAction
  , soContent :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SectionOverride where
  toJSON SectionOverride{..} = object
    [ "action"  .= soAction
    , "content" .= soContent
    ]

instance FromJSON SectionOverride where
  parseJSON = withObject "SectionOverride" $ \o ->
    SectionOverride
      <$> o .:  "action"
      <*> o .:? "content"

-- | System message configuration for session creation.
-- Mode can be "append" (default), "replace", or "customize".
data SystemMessageConfig = SystemMessageConfig
  { smcMode     :: !(Maybe Text)  -- ^ "append", "replace", or "customize"
  , smcContent  :: !(Maybe Text)
  , smcSections :: !(Maybe (Map.Map SystemPromptSection SectionOverride))
  } deriving (Show, Eq, Generic)

instance ToJSON SystemMessageConfig where
  toJSON SystemMessageConfig{..} = object $ catMaybes
    [ ("mode"     .=) <$> smcMode
    , ("content"  .=) <$> smcContent
    , ("sections" .=) <$> smcSections
    ]

instance FromJSON SystemMessageConfig where
  parseJSON = withObject "SystemMessageConfig" $ \o ->
    SystemMessageConfig
      <$> o .:? "mode"
      <*> o .:? "content"
      <*> o .:? "sections"

-- ============================================================================
-- Provider Configuration
-- ============================================================================

-- | Azure-specific provider options.
data AzureProviderOptions = AzureProviderOptions
  { apoApiVersion :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AzureProviderOptions where
  toJSON AzureProviderOptions{..} = object $ catMaybes [ ("apiVersion" .=) <$> apoApiVersion ]

instance FromJSON AzureProviderOptions where
  parseJSON = withObject "AzureProviderOptions" $ \o ->
    AzureProviderOptions <$> o .:? "apiVersion"

-- | Legacy single BYOK provider. On the protocol 4 wire it becomes the named
-- @default@ provider (@inference.providers@) with Copilot models disabled;
-- model-specific fields become an @inference.models@ entry for the session
-- model (see 'Copilot.SessionParams.groupSessionParams').
data ProviderConfig = ProviderConfig
  { pcType        :: !(Maybe Text)  -- ^ "openai", "azure", or "anthropic"
  , pcWireApi     :: !(Maybe Text)  -- ^ "completions" or "responses"
  , pcBaseUrl     :: !Text
  , pcApiKey      :: !(Maybe Text)
  , pcBearerToken :: !(Maybe Text)
  , pcAzure       :: !(Maybe AzureProviderOptions)
  , pcApiKeyCommand          :: !(Maybe Text)  -- ^ Command run before each request for a fresh API key
  , pcModelProvider          :: !(Maybe Text)  -- ^ Product serving the model, e.g. 'modelProviderOllama'
  , pcTransport              :: !(Maybe Text)  -- ^ "http" or "websockets"
  , pcHeaders                :: !(Maybe (Map.Map Text Text))
  , pcModelId                :: !(Maybe Text)
  , pcWireModel              :: !(Maybe Text)
  , pcMaxPromptTokens        :: !(Maybe Int)
  , pcMaxContextWindowTokens :: !(Maybe Int)
  , pcMaxOutputTokens        :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | A provider with the given base URL and nothing else set.
defaultProviderConfig :: Text -> ProviderConfig
defaultProviderConfig baseUrl = ProviderConfig
  { pcType = Nothing, pcWireApi = Nothing, pcBaseUrl = baseUrl, pcApiKey = Nothing
  , pcBearerToken = Nothing, pcAzure = Nothing, pcApiKeyCommand = Nothing
  , pcModelProvider = Nothing, pcTransport = Nothing, pcHeaders = Nothing
  , pcModelId = Nothing, pcWireModel = Nothing, pcMaxPromptTokens = Nothing
  , pcMaxContextWindowTokens = Nothing, pcMaxOutputTokens = Nothing
  }

instance ToJSON ProviderConfig where
  toJSON ProviderConfig{..} = object $ catMaybes
    [ ("type" .=) <$> pcType
    , ("wireApi" .=) <$> pcWireApi
    , Just ("baseUrl" .= pcBaseUrl)
    , ("apiKey" .=) <$> pcApiKey
    , ("bearerToken" .=) <$> pcBearerToken
    , ("azure" .=) <$> pcAzure
    , ("apiKeyCommand" .=) <$> pcApiKeyCommand
    , ("modelProvider" .=) <$> pcModelProvider
    , ("transport" .=) <$> pcTransport
    , ("headers" .=) <$> pcHeaders
    , ("modelId" .=) <$> pcModelId
    , ("wireModel" .=) <$> pcWireModel
    , ("maxPromptTokens" .=) <$> pcMaxPromptTokens
    , ("maxContextWindowTokens" .=) <$> pcMaxContextWindowTokens
    , ("maxOutputTokens" .=) <$> pcMaxOutputTokens
    ]

instance FromJSON ProviderConfig where
  parseJSON = withObject "ProviderConfig" $ \o ->
    ProviderConfig
      <$> o .:? "type"
      <*> o .:? "wireApi"
      <*> o .:  "baseUrl"
      <*> o .:? "apiKey"
      <*> o .:? "bearerToken"
      <*> o .:? "azure"
      <*> o .:? "apiKeyCommand"
      <*> o .:? "modelProvider"
      <*> o .:? "transport"
      <*> o .:? "headers"
      <*> o .:? "modelId"
      <*> o .:? "wireModel"
      <*> o .:? "maxPromptTokens"
      <*> o .:? "maxContextWindowTokens"
      <*> o .:? "maxOutputTokens"

-- ============================================================================
-- MCP Server Configuration
-- ============================================================================

-- | Configuration for a local/stdio MCP server.
data MCPStdioServerConfig = MCPStdioServerConfig
  { msscTools   :: ![Text]
  , msscType    :: !(Maybe Text)   -- ^ "local" or "stdio"
  , msscTimeout :: !(Maybe Int)    -- ^ Tool call timeout in ms
  , msscCommand :: !Text
  , msscArgs    :: ![Text]
  , msscEnv     :: !(Maybe (Map.Map Text Text))
  , msscCwd     :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON MCPStdioServerConfig where
  toJSON MCPStdioServerConfig{..} = object $ catMaybes
    [ Just $ "tools"   .= msscTools
    , Just $ "command" .= msscCommand
    , Just $ "args"    .= msscArgs
    , ("type"    .=) <$> msscType
    , ("timeout" .=) <$> msscTimeout
    , ("env"     .=) <$> msscEnv
    , ("cwd"     .=) <$> msscCwd
    ]

instance FromJSON MCPStdioServerConfig where
  parseJSON = withObject "MCPStdioServerConfig" $ \o ->
    MCPStdioServerConfig
      <$> o .:  "tools"
      <*> o .:? "type"
      <*> o .:? "timeout"
      <*> o .:  "command"
      <*> o .:? "args" .!= []
      <*> o .:? "env"
      <*> o .:? "cwd"

-- | Configuration for a remote MCP server (HTTP or SSE).
data MCPHTTPServerConfig = MCPHTTPServerConfig
  { mhscTools   :: ![Text]
  , mhscType    :: !Text           -- ^ "http" or "sse"
  , mhscTimeout :: !(Maybe Int)    -- ^ Tool call timeout in ms
  , mhscUrl     :: !Text
  , mhscHeaders :: !(Maybe (Map.Map Text Text))
  } deriving (Show, Eq, Generic)

instance ToJSON MCPHTTPServerConfig where
  toJSON MCPHTTPServerConfig{..} = object $ catMaybes
    [ Just $ "tools"   .= mhscTools
    , Just $ "type"    .= mhscType
    , Just $ "url"     .= mhscUrl
    , ("timeout" .=) <$> mhscTimeout
    , ("headers" .=) <$> mhscHeaders
    ]

instance FromJSON MCPHTTPServerConfig where
  parseJSON = withObject "MCPHTTPServerConfig" $ \o ->
    MCPHTTPServerConfig
      <$> o .:  "tools"
      <*> o .:  "type"
      <*> o .:? "timeout"
      <*> o .:  "url"
      <*> o .:? "headers"

-- | Union type for MCP server configurations.
-- Deserialization checks for "url" to distinguish HTTP from Stdio variants.
data MCPServerConfig
  = MCPStdio MCPStdioServerConfig
  | MCPHTTP  MCPHTTPServerConfig
  deriving (Show, Eq, Generic)

instance ToJSON MCPServerConfig where
  toJSON (MCPStdio c) = toJSON c
  toJSON (MCPHTTP  c) = toJSON c

instance FromJSON MCPServerConfig where
  parseJSON v = (MCPHTTP <$> parseJSON v) <|> (MCPStdio <$> parseJSON v)

-- ============================================================================
-- Custom Agent Configuration
-- ============================================================================

-- | Configuration for a custom agent.
data CustomAgentConfig = CustomAgentConfig
  { cacName        :: !Text
  , cacDisplayName :: !(Maybe Text)
  , cacDescription :: !(Maybe Text)
  , cacTools       :: !(Maybe [Text])
  , cacPrompt      :: !Text
  , cacMcpServers  :: !(Maybe (Map.Map Text MCPServerConfig))
  , cacInfer       :: !(Maybe Bool)
  , cacSkills      :: !(Maybe [Text])  -- ^ List of skill names to preload
  , cacArgsSchema  :: !(Maybe Value)  -- ^ JSON schema for agent factory authoring arguments (argsSchema)
  } deriving (Show, Eq, Generic)

instance ToJSON CustomAgentConfig where
  toJSON CustomAgentConfig{..} = object $ catMaybes
    [ Just ("name"   .= cacName)
    , ("displayName" .=) <$> cacDisplayName
    , ("description" .=) <$> cacDescription
    , ("tools"       .=) <$> cacTools
    , Just ("prompt" .= cacPrompt)
    , ("mcpServers"  .=) <$> cacMcpServers
    , ("infer"       .=) <$> cacInfer
    , ("skills"      .=) <$> cacSkills
    , ("argsSchema"  .=) <$> cacArgsSchema
    ]

instance FromJSON CustomAgentConfig where
  parseJSON = withObject "CustomAgentConfig" $ \o ->
    CustomAgentConfig
      <$> o .:  "name"
      <*> o .:? "displayName"
      <*> o .:? "description"
      <*> o .:? "tools"
      <*> o .:  "prompt"
      <*> o .:? "mcpServers"
      <*> o .:? "infer"
      <*> o .:? "skills"
      <*> o .:? "argsSchema"

-- ============================================================================
-- Infinite Session Configuration
-- ============================================================================

-- | Configuration for infinite sessions with automatic context compaction.
data InfiniteSessionConfig = InfiniteSessionConfig
  { iscEnabled                        :: !(Maybe Bool)
  , iscBackgroundCompactionThreshold  :: !(Maybe Double)
  , iscBufferExhaustionThreshold      :: !(Maybe Double)
  } deriving (Show, Eq, Generic)

instance ToJSON InfiniteSessionConfig where
  toJSON InfiniteSessionConfig{..} = object $ catMaybes
    [ ("enabled"                       .=) <$> iscEnabled
    , ("backgroundCompactionThreshold" .=) <$> iscBackgroundCompactionThreshold
    , ("bufferExhaustionThreshold"     .=) <$> iscBufferExhaustionThreshold
    ]

instance FromJSON InfiniteSessionConfig where
  parseJSON = withObject "InfiniteSessionConfig" $ \o ->
    InfiniteSessionConfig
      <$> o .:? "enabled"
      <*> o .:? "backgroundCompactionThreshold"
      <*> o .:? "bufferExhaustionThreshold"

-- ============================================================================
-- Commands
-- ============================================================================

-- | Context for a slash-command invocation.
data CommandContext = CommandContext
  { ccSessionId   :: !Text
  , ccCommand     :: !Text
  , ccCommandName :: !Text
  , ccArgs        :: !Text
  } deriving (Show, Eq, Generic)

-- | Handler invoked when a registered slash-command is executed.
type CommandHandler = CommandContext -> IO ()

-- | Definition of a slash command registered with the session.
data CommandDefinition = CommandDefinition
  { cdName        :: !Text
  , cdDescription :: !(Maybe Text)
  , cdHandler     :: !CommandHandler
  }

instance Show CommandDefinition where
  show cd = "CommandDefinition { name = " ++ show (cdName cd) ++ " }"

-- ============================================================================
-- UI Elicitation
-- ============================================================================

-- | Context for an elicitation request from the server.
data ElicitationContext = ElicitationContext
  { ecSessionId        :: !Text
  , ecMessage          :: !Text
  , ecRequestedSchema  :: !(Maybe (Map.Map Text Value))
  , ecMode             :: !(Maybe Text)
  , ecElicitationSource :: !(Maybe Text)
  , ecUrl              :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | Result returned from an elicitation handler.
data ElicitationResult = ElicitationResult
  { erAction  :: !Text
  , erContent :: !(Maybe (Map.Map Text Value))
  } deriving (Show, Eq, Generic)

-- | Handler for elicitation requests from the server.
type ElicitationHandler = ElicitationContext -> IO ElicitationResult

-- ============================================================================
-- Session Configuration
-- ============================================================================

-- | Configuration for creating a new session.
data SessionConfig = SessionConfig
  { scSessionId                      :: !(Maybe Text)
  , scModel                          :: !(Maybe Text)
  , scReasoningEffort                :: !(Maybe Text)
  , scConfigDir                      :: !(Maybe Text)
  , scTools                          :: ![Tool]
  , scSystemMessage                  :: !(Maybe SystemMessageConfig)
  , scAvailableTools                 :: !(Maybe [Text])
  , scExcludedTools                  :: !(Maybe [Text])
  , scProvider                       :: !(Maybe ProviderConfig)
  , scOnPermissionRequest            :: !(Maybe PermissionHandler)
  , scOnUserInputRequest             :: !(Maybe UserInputHandler)
  , scHooks                          :: !(Maybe SessionHooks)
  , scWorkingDirectory               :: !(Maybe Text)
  , scStreaming                      :: !(Maybe Bool)
  , scIncludeSubAgentStreamingEvents :: !(Maybe Bool)  -- ^ Include sub-agent streaming events (default: true)
  , scMcpServers                     :: !(Maybe (Map.Map Text MCPServerConfig))
  , scCustomAgents                   :: !(Maybe [CustomAgentConfig])
  , scSkillDirectories               :: !(Maybe [Text])
  , scDisabledSkills                 :: !(Maybe [Text])
  , scInfiniteSessions               :: !(Maybe InfiniteSessionConfig)
  , scModelCapabilities              :: !(Maybe (Map.Map Text Value))  -- ^ Model capabilities overrides
  , scEnableConfigDiscovery          :: !(Maybe Bool)  -- ^ Auto-discover MCP server configs (default: false)
  , scGitHubToken                    :: !(Maybe Text)  -- ^ GitHub token for authentication. Overrides client-level token for this session only.
  , scCommands                       :: !(Maybe [CommandDefinition])  -- ^ Slash commands registered for this session
  , scOnElicitationRequest           :: !(Maybe ElicitationHandler)  -- ^ Handler for elicitation requests from the server
  , scInstructionDirectories         :: !(Maybe [Text])  -- ^ Directories to search for instruction files
  -- --- Upstream-sync session options (parity with @github/copilot-sdk) ---
  , scEnableCitations                :: !(Maybe Bool)  -- ^ Emit source citations for grounded responses
  , scExcludedBuiltinAgents          :: !(Maybe [Text])  -- ^ Built-in agents to exclude from this session
  , scSessionLimits                  :: !(Maybe SessionLimitsConfig)  -- ^ Per-session AI-credit spend limits
  , scMemory                         :: !(Maybe MemoryConfiguration)  -- ^ Opt-in persistent session memory
  , scOtlpProtocol                   :: !(Maybe Text)  -- ^ OTLP telemetry protocol ("grpc" or "http/protobuf")
  , scEnableWebSocketResponses       :: !(Maybe Bool)  -- ^ Stream model responses over a WebSocket transport
  , scExpAssignments                 :: !(Maybe (Map.Map Text Value))  -- ^ Experiment assignment overrides
  , scOnMcpAuthRequest               :: !(Maybe McpAuthHandler)  -- ^ MCP OAuth host token handler
  -- --- Upstream-sync 2026-08 session options (parity with @github/copilot-sdk) ---
  , scRewindEnabled                  :: !(Maybe Bool)  -- ^ Enable session rewind support
  , scAdditionalDirectories          :: !(Maybe [Text])  -- ^ Additional directories to include in the session
  , scDisabledMcpServers             :: !(Maybe [Text])  -- ^ MCP servers to disable for this session
  , scGithubMcpToolConfig            :: !(Maybe (Map.Map Text Value))  -- ^ GitHub MCP tool configuration
  , scCanvasProvider                 :: !(Maybe Value)  -- ^ Canvas provider configuration
  , scCustomAgentsLocalOnly          :: !(Maybe Bool)  -- ^ Restrict custom agents to local-only definitions
  , scToolSearch                     :: !(Maybe Value)  -- ^ Tool search configuration
  , scExperimentalMode               :: !(Maybe Bool)  -- ^ Enable experimental mode
  , scContentExclusion               :: !(Maybe Bool)  -- ^ Enable content exclusion
  -- --- v2.6.0 / v3.0.0 session options (SDK protocol 4) ---
  , scAskUserVariant                 :: !(Maybe Text)  -- ^ 'askUserVariantLegacy' or 'askUserVariantElicitation'
  , scGitHubTokenProvider            :: !(Maybe GitHubTokenProvider)  -- ^ Supplies GitHub tokens on demand; exclusive with 'scGitHubToken'
  , scSandbox                        :: !(Maybe SandboxConfig)  -- ^ Sandbox configuration (permissions.sandbox)
  , scFeatureFlags                   :: !(Maybe (Map.Map Text Bool))  -- ^ Runtime feature flag overrides (experiments.flags)
  , scAllowedModels                  :: !(Maybe [Text])  -- ^ Model allowlist (policy.allowedModels)
  , scAuthClientIdMetadataUrl        :: !(Maybe Text)  -- ^ OAuth client metadata URL for MCP authorization
  , scAgentDirectories               :: !(Maybe [Text])  -- ^ Extra directories to load custom agents from
  , scHookDirectories                :: !(Maybe [Text])  -- ^ Extra directories to load hooks from
  , scTrustWorkingDirectory          :: !(Maybe Text)  -- ^ 'trustWorkingDirectorySession' or 'trustWorkingDirectoryUser' (create only)
  , scImageGeneration                :: !(Maybe ImageGenerationConfig)  -- ^ Image generation (tools.imageGeneration)
  , scAutopilot                      :: !(Maybe AutopilotConfig)  -- ^ Autopilot continuation (execution.autopilot)
  , scProviders                      :: !(Maybe [NamedProviderConfig])  -- ^ Named BYOK providers (inference.providers)
  , scModels                         :: !(Maybe [ProviderModelConfig])  -- ^ BYOK models (inference.models)
  , scDefaultProvider                :: !(Maybe Text)  -- ^ Default named provider (inference.defaultProvider)
  , scCapi                           :: !(Maybe CapiConfig)  -- ^ Copilot API options (github.capi)
  , scOnShellAuthenticationRequest   :: !(Maybe ShellAuthenticationHandler)  -- ^ Answers sudo/ssh/git prompts
  , scOnStoreMemory                  :: !(Maybe StoreMemoryHandler)  -- ^ Stores memories the agent asks to keep
  -- --- v3.1.0 session options ---
  , scOnUserInputImageRequest        :: !(Maybe UserInputImageHandler)  -- ^ ask_user handler that can answer with images; takes precedence over 'scOnUserInputRequest'
  }

-- | Default (empty) session configuration.
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
  { scSessionId                      = Nothing
  , scModel                          = Nothing
  , scReasoningEffort                = Nothing
  , scConfigDir                      = Nothing
  , scTools                          = []
  , scSystemMessage                  = Nothing
  , scAvailableTools                 = Nothing
  , scExcludedTools                  = Nothing
  , scProvider                       = Nothing
  , scOnPermissionRequest            = Nothing
  , scOnUserInputRequest             = Nothing
  , scHooks                          = Nothing
  , scWorkingDirectory               = Nothing
  , scStreaming                      = Nothing
  , scIncludeSubAgentStreamingEvents = Nothing
  , scMcpServers                     = Nothing
  , scCustomAgents                   = Nothing
  , scSkillDirectories               = Nothing
  , scDisabledSkills                 = Nothing
  , scInfiniteSessions               = Nothing
  , scModelCapabilities              = Nothing
  , scEnableConfigDiscovery          = Nothing
  , scGitHubToken                    = Nothing
  , scCommands                       = Nothing
  , scOnElicitationRequest           = Nothing
  , scInstructionDirectories         = Nothing
  , scEnableCitations                = Nothing
  , scExcludedBuiltinAgents          = Nothing
  , scSessionLimits                  = Nothing
  , scMemory                         = Nothing
  , scOtlpProtocol                   = Nothing
  , scEnableWebSocketResponses       = Nothing
  , scExpAssignments                 = Nothing
  , scOnMcpAuthRequest               = Nothing
  , scRewindEnabled                  = Nothing
  , scAdditionalDirectories          = Nothing
  , scDisabledMcpServers             = Nothing
  , scGithubMcpToolConfig            = Nothing
  , scCanvasProvider                 = Nothing
  , scCustomAgentsLocalOnly          = Nothing
  , scToolSearch                     = Nothing
  , scExperimentalMode               = Nothing
  , scContentExclusion               = Nothing
  , scAskUserVariant                 = Nothing
  , scGitHubTokenProvider            = Nothing
  , scSandbox                        = Nothing
  , scFeatureFlags                   = Nothing
  , scAllowedModels                  = Nothing
  , scAuthClientIdMetadataUrl        = Nothing
  , scAgentDirectories               = Nothing
  , scHookDirectories                = Nothing
  , scTrustWorkingDirectory          = Nothing
  , scImageGeneration                = Nothing
  , scAutopilot                      = Nothing
  , scProviders                      = Nothing
  , scModels                         = Nothing
  , scDefaultProvider                = Nothing
  , scCapi                           = Nothing
  , scOnShellAuthenticationRequest   = Nothing
  , scOnStoreMemory                  = Nothing
  , scOnUserInputImageRequest        = Nothing
  }

-- | Configuration for resuming a session.
data ResumeSessionConfig = ResumeSessionConfig
  { rscModel                          :: !(Maybe Text)
  , rscReasoningEffort                :: !(Maybe Text)
  , rscTools                          :: ![Tool]
  , rscSystemMessage                  :: !(Maybe SystemMessageConfig)
  , rscAvailableTools                 :: !(Maybe [Text])
  , rscExcludedTools                  :: !(Maybe [Text])
  , rscProvider                       :: !(Maybe ProviderConfig)
  , rscOnPermissionRequest            :: !(Maybe PermissionHandler)
  , rscOnUserInputRequest             :: !(Maybe UserInputHandler)
  , rscHooks                          :: !(Maybe SessionHooks)
  , rscWorkingDirectory               :: !(Maybe Text)
  , rscConfigDir                      :: !(Maybe Text)
  , rscStreaming                      :: !(Maybe Bool)
  , rscIncludeSubAgentStreamingEvents :: !(Maybe Bool)  -- ^ Include sub-agent streaming events (default: true)
  , rscMcpServers                     :: !(Maybe (Map.Map Text MCPServerConfig))
  , rscCustomAgents                   :: !(Maybe [CustomAgentConfig])
  , rscSkillDirectories               :: !(Maybe [Text])
  , rscDisabledSkills                 :: !(Maybe [Text])
  , rscInfiniteSessions               :: !(Maybe InfiniteSessionConfig)
  , rscModelCapabilities              :: !(Maybe (Map.Map Text Value))  -- ^ Model capabilities overrides
  , rscEnableConfigDiscovery          :: !(Maybe Bool)  -- ^ Auto-discover MCP server configs (default: false)
  , rscGitHubToken                    :: !(Maybe Text)  -- ^ GitHub token for authentication. Overrides client-level token for this session only.
  , rscCommands                       :: !(Maybe [CommandDefinition])  -- ^ Slash commands registered for this session
  , rscOnElicitationRequest           :: !(Maybe ElicitationHandler)  -- ^ Handler for elicitation requests from the server
  , rscDisableResume                  :: !(Maybe Bool)  -- ^ Suppress the resume event (wire: suppressResumeEvent)
  , rscInstructionDirectories         :: !(Maybe [Text])  -- ^ Directories to search for instruction files
  -- --- v2.6.0 / v3.0.0 resume options (SDK protocol 4) ---
  , rscAllowTranscriptRecovery        :: !(Maybe Bool)  -- ^ Allow recovery from the transcript when state is missing
  , rscAskUserVariant                 :: !(Maybe Text)
  , rscGitHubTokenProvider            :: !(Maybe GitHubTokenProvider)
  , rscSandbox                        :: !(Maybe SandboxConfig)
  , rscFeatureFlags                   :: !(Maybe (Map.Map Text Bool))
  , rscAllowedModels                  :: !(Maybe [Text])
  , rscAuthClientIdMetadataUrl        :: !(Maybe Text)
  , rscAgentDirectories               :: !(Maybe [Text])
  , rscHookDirectories                :: !(Maybe [Text])
  , rscImageGeneration                :: !(Maybe ImageGenerationConfig)
  , rscAutopilot                      :: !(Maybe AutopilotConfig)
  , rscProviders                      :: !(Maybe [NamedProviderConfig])
  , rscModels                         :: !(Maybe [ProviderModelConfig])
  , rscDefaultProvider                :: !(Maybe Text)
  , rscClearDefaultProvider           :: !Bool  -- ^ Send an explicit null default provider to clear it
  , rscCapi                           :: !(Maybe CapiConfig)
  , rscOnShellAuthenticationRequest   :: !(Maybe ShellAuthenticationHandler)
  , rscOnStoreMemory                  :: !(Maybe StoreMemoryHandler)
  -- --- v3.1.0 resume options ---
  , rscOnUserInputImageRequest        :: !(Maybe UserInputImageHandler)  -- ^ ask_user handler that can answer with images; takes precedence over 'rscOnUserInputRequest'
  }

-- | Default (empty) resume session configuration.
defaultResumeSessionConfig :: ResumeSessionConfig
defaultResumeSessionConfig = ResumeSessionConfig
  { rscModel                          = Nothing
  , rscReasoningEffort                = Nothing
  , rscTools                          = []
  , rscSystemMessage                  = Nothing
  , rscAvailableTools                 = Nothing
  , rscExcludedTools                  = Nothing
  , rscProvider                       = Nothing
  , rscOnPermissionRequest            = Nothing
  , rscOnUserInputRequest             = Nothing
  , rscHooks                          = Nothing
  , rscWorkingDirectory               = Nothing
  , rscConfigDir                      = Nothing
  , rscStreaming                      = Nothing
  , rscIncludeSubAgentStreamingEvents = Nothing
  , rscMcpServers                     = Nothing
  , rscCustomAgents                   = Nothing
  , rscSkillDirectories               = Nothing
  , rscDisabledSkills                 = Nothing
  , rscInfiniteSessions               = Nothing
  , rscModelCapabilities              = Nothing
  , rscEnableConfigDiscovery          = Nothing
  , rscGitHubToken                    = Nothing
  , rscCommands                       = Nothing
  , rscOnElicitationRequest           = Nothing
  , rscDisableResume                  = Nothing
  , rscInstructionDirectories         = Nothing
  , rscAllowTranscriptRecovery        = Nothing
  , rscAskUserVariant                 = Nothing
  , rscGitHubTokenProvider            = Nothing
  , rscSandbox                        = Nothing
  , rscFeatureFlags                   = Nothing
  , rscAllowedModels                  = Nothing
  , rscAuthClientIdMetadataUrl        = Nothing
  , rscAgentDirectories               = Nothing
  , rscHookDirectories                = Nothing
  , rscImageGeneration                = Nothing
  , rscAutopilot                      = Nothing
  , rscProviders                      = Nothing
  , rscModels                         = Nothing
  , rscDefaultProvider                = Nothing
  , rscClearDefaultProvider           = False
  , rscCapi                           = Nothing
  , rscOnShellAuthenticationRequest   = Nothing
  , rscOnStoreMemory                  = Nothing
  , rscOnUserInputImageRequest        = Nothing
  }

-- ============================================================================
-- Upstream-sync Feature Types & Constants (parity with @github/copilot-sdk)
-- ============================================================================

-- | Per-session AI-credit budget; set 'slcMaxAiCredits' to cap spend.
newtype SessionLimitsConfig = SessionLimitsConfig
  { slcMaxAiCredits :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON SessionLimitsConfig where
  toJSON SessionLimitsConfig{..} = object $ filter ((/= Null) . snd)
    [ "maxAiCredits" .= slcMaxAiCredits ]

-- | Opt-in persistent session memory.
newtype MemoryConfiguration = MemoryConfiguration
  { mcEnabled :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON MemoryConfiguration where
  toJSON MemoryConfiguration{..} = object $ filter ((/= Null) . snd)
    [ "enabled" .= mcEnabled ]

-- | Arguments passed to a BYOK bearer-token provider (per-session scoping).
newtype ProviderTokenArgs = ProviderTokenArgs
  { ptaSessionId :: Text
  } deriving (Show, Eq, Generic)

-- | BYOK bearer-token provider: mints a fresh bearer token per session.
type BearerTokenProvider = ProviderTokenArgs -> IO Text

-- | MCP OAuth host token handler: supplies an OAuth token for an MCP auth request.
type McpAuthHandler = Value -> IO Value

-- | Intercepts outbound LLM inference HTTP/WebSocket requests. Receives the
-- request and a context value and returns the (possibly mutated) request.
type CopilotRequestHandler = Value -> Value -> IO Value

-- | Tool "defer" loading policy: lazy via search ('toolDeferAuto') or eager
-- pre-load ('toolDeferNever').
toolDeferAuto, toolDeferNever :: Text
toolDeferAuto  = "auto"
toolDeferNever = "never"

-- | System-message section identifiers. The @preamble@ section targets only the
-- identity preamble; @preserve@ protects an addressable section from removal.
systemMessageSectionPreamble, systemMessageSectionIdentity,
  systemMessageSectionToolInstructions, systemMessageSectionPreserve :: Text
systemMessageSectionPreamble         = "preamble"
systemMessageSectionIdentity         = "identity"
systemMessageSectionToolInstructions = "tool_instructions"
systemMessageSectionPreserve         = "preserve"

-- | GitHub-anchored attachment variants.
attachmentGitHubCommit, attachmentGitHubRelease, attachmentGitHubActionsJob,
  attachmentGitHubRepository, attachmentGitHubFileDiff, attachmentGitHubTreeComparison,
  attachmentGitHubUrl, attachmentGitHubFile, attachmentGitHubSnippet :: Text
attachmentGitHubCommit         = "GitHubCommit"
attachmentGitHubRelease        = "GitHubRelease"
attachmentGitHubActionsJob     = "GitHubActionsJob"
attachmentGitHubRepository     = "GitHubRepository"
attachmentGitHubFileDiff       = "GitHubFileDiff"
attachmentGitHubTreeComparison = "GitHubTreeComparison"
attachmentGitHubUrl            = "GitHubUrl"
attachmentGitHubFile           = "GitHubFile"
attachmentGitHubSnippet        = "GitHubSnippet"

-- ============================================================================
-- v2.6.0 Upstream-sync Feature Types & Constants
-- ============================================================================

-- | Identity of the application embedding the SDK, sent as @clientInfo@ on the
-- @connect@ handshake. Empty fields are dropped; nothing is sent when every
-- field is empty.
data CopilotClientInfo = CopilotClientInfo
  { cciEditorName       :: !(Maybe Text)
  , cciEditorVersion    :: !(Maybe Text)
  , cciExtensionName    :: !(Maybe Text)
  , cciExtensionVersion :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | Client info with every field unset.
defaultClientInfo :: CopilotClientInfo
defaultClientInfo = CopilotClientInfo Nothing Nothing Nothing Nothing

-- | Wire shape of 'CopilotClientInfo', or 'Nothing' when every field is empty.
clientInfoToWire :: CopilotClientInfo -> Maybe Value
clientInfoToWire CopilotClientInfo{..} =
  let nonEmpty (Just v) | not (T.null v) = Just v
      nonEmpty _ = Nothing
      fields = catMaybes
        [ ("editorName" .=)       <$> nonEmpty cciEditorName
        , ("editorVersion" .=)    <$> nonEmpty cciEditorVersion
        , ("extensionName" .=)    <$> nonEmpty cciExtensionName
        , ("extensionVersion" .=) <$> nonEmpty cciExtensionVersion
        ]
  in if null fields then Nothing else Just (object fields)

-- | Auto routing tiers for 'Copilot.Session.switchAutoTier'.
autoTierEfficiency, autoTierBalance, autoTierIntelligence, autoTierFast :: Text
autoTierEfficiency   = "efficiency"
autoTierBalance      = "balance"
autoTierIntelligence = "intelligence"
autoTierFast         = "fast"

-- | Ask-user tool variants ('scAskUserVariant').
askUserVariantLegacy, askUserVariantElicitation :: Text
askUserVariantLegacy      = "legacy"
askUserVariantElicitation = "elicitation"

-- | Working directory trust scopes ('scTrustWorkingDirectory').
trustWorkingDirectorySession, trustWorkingDirectoryUser :: Text
trustWorkingDirectorySession = "session"
trustWorkingDirectoryUser    = "user"

-- | Origin of a sent message ('moSource'): user, system, or an agent.
type MessageSource = Text

-- | Message sources.
messageSourceUser, messageSourceSystem :: MessageSource
messageSourceUser   = "user"
messageSourceSystem = "system"

-- | Source for a message sent on behalf of the agent with the given ID.
messageSourceAgent :: Text -> MessageSource
messageSourceAgent agentId = "agent-" <> agentId

-- | Products that can serve a BYOK provider's models ('pcModelProvider').
modelProviderOpenAI, modelProviderAnthropic, modelProviderAzureOpenAI,
  modelProviderOllama, modelProviderLmStudio, modelProviderFoundryLocal,
  modelProviderLlamaCpp :: Text
modelProviderOpenAI       = "openai"
modelProviderAnthropic    = "anthropic"
modelProviderAzureOpenAI  = "azure_openai"
modelProviderOllama       = "ollama"
modelProviderLmStudio     = "lm_studio"
modelProviderFoundryLocal = "foundry_local"
modelProviderLlamaCpp     = "llama_cpp"

-- | Arguments of a @gitHubToken.getToken@ request.
data GitHubTokenProviderArgs = GitHubTokenProviderArgs
  { gtpaRegistrationId :: !Text
  , gtpaHost           :: !Text
  , gtpaSessionId      :: !(Maybe Text)
  , gtpaReason         :: !Text
  } deriving (Show, Eq, Generic)

-- | A GitHub token granted by a 'GitHubTokenProvider'.
data GitHubTokenGrant = GitHubTokenGrant
  { gtgAccessToken :: !Text
  , gtgTokenType   :: !(Maybe Text)  -- ^ Defaults to bearer when omitted
  , gtgExpiresIn   :: !Int           -- ^ Remaining lifetime in seconds
  } deriving (Show, Eq, Generic)

-- | Answer of a 'GitHubTokenProvider'.
data GitHubTokenProviderResult
  = GitHubTokenGranted GitHubTokenGrant
  | GitHubTokenCancelled
  deriving (Show, Eq, Generic)

instance ToJSON GitHubTokenProviderResult where
  toJSON GitHubTokenCancelled = object [ "kind" .= ("cancelled" :: Text) ]
  toJSON (GitHubTokenGranted GitHubTokenGrant{..}) = object $ catMaybes
    [ Just ("kind" .= ("token" :: Text))
    , Just ("accessToken" .= gtgAccessToken)
    , Just ("expiresIn" .= gtgExpiresIn)
    , ("tokenType" .=) <$> gtgTokenType
    ]

-- | Supplies GitHub tokens on demand (@gitHubToken.getToken@). Mutually
-- exclusive with a static 'scGitHubToken'.
type GitHubTokenProvider = GitHubTokenProviderArgs -> IO GitHubTokenProviderResult

-- | Sandbox configuration (@permissions.sandbox@).
data SandboxConfig = SandboxConfig
  { sbcEnabled                    :: !Bool
  , sbcAllowBypass                :: !(Maybe Bool)  -- ^ Let the agent ask to run a command outside the sandbox
  , sbcAddCurrentWorkingDirectory :: !(Maybe Bool)
  , sbcSandboxMcpServers          :: !(Maybe Bool)
  , sbcUserPolicy                 :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

-- | Sandbox disabled, nothing else set.
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig False Nothing Nothing Nothing Nothing

instance ToJSON SandboxConfig where
  toJSON SandboxConfig{..} = object $ catMaybes
    [ Just ("enabled" .= sbcEnabled)
    , ("allowBypass" .=) <$> sbcAllowBypass
    , ("addCurrentWorkingDirectory" .=) <$> sbcAddCurrentWorkingDirectory
    , ("sandboxMcpServers" .=) <$> sbcSandboxMcpServers
    , ("userPolicy" .=) <$> sbcUserPolicy
    ]

-- | Image generation configuration (@tools.imageGeneration@).
data ImageGenerationConfig = ImageGenerationConfig
  { igcEnabled    :: !(Maybe Bool)
  , igcWatermark  :: !(Maybe Bool)
  , igcSaveToDisk :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)

-- | Image generation configuration with every field unset.
defaultImageGenerationConfig :: ImageGenerationConfig
defaultImageGenerationConfig = ImageGenerationConfig Nothing Nothing Nothing

instance ToJSON ImageGenerationConfig where
  toJSON ImageGenerationConfig{..} = object $ catMaybes
    [ ("enabled" .=) <$> igcEnabled
    , ("watermark" .=) <$> igcWatermark
    , ("saveToDisk" .=) <$> igcSaveToDisk
    ]

-- | Autopilot continuation options (@execution.autopilot@).
data AutopilotConfig = AutopilotConfig
  { apcContinuationEnabled :: !(Maybe Bool)
  , apcMaxContinues        :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | Autopilot options with every field unset.
defaultAutopilotConfig :: AutopilotConfig
defaultAutopilotConfig = AutopilotConfig Nothing Nothing

instance ToJSON AutopilotConfig where
  toJSON AutopilotConfig{..} = object $ catMaybes
    [ ("continuationEnabled" .=) <$> apcContinuationEnabled
    , ("maxContinues" .=) <$> apcMaxContinues
    ]

-- | Token limits for a model capabilities override. Wire keys are snake_case
-- (@limits.max_output_tokens@ and similar).
data ModelLimitsOverride = ModelLimitsOverride
  { mloMaxPromptTokens        :: !(Maybe Int)
  , mloMaxContextWindowTokens :: !(Maybe Int)
  , mloMaxOutputTokens        :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | Limits override with every field unset.
defaultModelLimitsOverride :: ModelLimitsOverride
defaultModelLimitsOverride = ModelLimitsOverride Nothing Nothing Nothing

instance ToJSON ModelLimitsOverride where
  toJSON ModelLimitsOverride{..} = object $ catMaybes
    [ ("max_prompt_tokens" .=) <$> mloMaxPromptTokens
    , ("max_context_window_tokens" .=) <$> mloMaxContextWindowTokens
    , ("max_output_tokens" .=) <$> mloMaxOutputTokens
    ]

-- | A 'scModelCapabilities' override carrying the given limits.
modelCapabilitiesWithLimits :: ModelLimitsOverride -> Map.Map Text Value
modelCapabilitiesWithLimits limits = Map.singleton "limits" (toJSON limits)

-- | The @session.send@ @responseFormat@ object requesting structured output
-- that matches a JSON Schema (see 'moResponseSchema').
structuredResponseFormat :: Value -> Value
structuredResponseFormat schema = object
  [ "type" .= ("json_schema" :: Text)
  , "jsonSchema" .= object
      [ "name"   .= ("response" :: Text)
      , "strict" .= True
      , "schema" .= schema
      ]
  ]

-- ============================================================================
-- v3.0.0 / SDK Protocol 4 Types & Constants
-- ============================================================================

-- | Client behavior profile ('ccoMode'). 'ClientModeCodingAgent' (the
-- default, wire name @coding-agent@) keeps the coding-agent behavior;
-- 'ClientModeEmpty' requires 'scAvailableTools' on every session and applies
-- conservative defaults beneath the caller's options.
data ClientMode = ClientModeCodingAgent | ClientModeEmpty
  deriving (Show, Eq, Generic)

-- | Parse a client mode name; @copilot-cli@ is an alias of @coding-agent@.
parseClientMode :: Text -> Maybe ClientMode
parseClientMode name = case name of
  ""             -> Just ClientModeCodingAgent
  "coding-agent" -> Just ClientModeCodingAgent
  "copilot-cli"  -> Just ClientModeCodingAgent
  "empty"        -> Just ClientModeEmpty
  _              -> Nothing

-- | A named BYOK provider connection (@inference.providers@).
data NamedProviderConfig = NamedProviderConfig
  { npcName          :: !Text
  , npcType          :: !(Maybe Text)
  , npcBaseUrl       :: !Text
  , npcApiKey        :: !(Maybe Text)
  , npcApiKeyCommand :: !(Maybe Text)  -- ^ Command run before each request for a fresh API key
  , npcBearerToken   :: !(Maybe Text)
  , npcWireApi       :: !(Maybe Text)
  , npcTransport     :: !(Maybe Text)
  , npcModelProvider :: !(Maybe Text)
  , npcAzure         :: !(Maybe AzureProviderOptions)
  , npcHeaders       :: !(Maybe (Map.Map Text Text))
  } deriving (Show, Eq, Generic)

-- | A provider with the given name and base URL and nothing else set.
defaultNamedProviderConfig :: Text -> Text -> NamedProviderConfig
defaultNamedProviderConfig name baseUrl =
  NamedProviderConfig name Nothing baseUrl Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON NamedProviderConfig where
  toJSON NamedProviderConfig{..} = object $ catMaybes
    [ Just ("name" .= npcName)
    , ("type" .=) <$> npcType
    , Just ("baseUrl" .= npcBaseUrl)
    , ("apiKey" .=) <$> npcApiKey
    , ("apiKeyCommand" .=) <$> npcApiKeyCommand
    , ("bearerToken" .=) <$> npcBearerToken
    , ("wireApi" .=) <$> npcWireApi
    , ("transport" .=) <$> npcTransport
    , ("modelProvider" .=) <$> npcModelProvider
    , ("azure" .=) <$> npcAzure
    , ("headers" .=) <$> npcHeaders
    ]

-- | A BYOK model served by a named provider (@inference.models@). Select it
-- with the model ID @provider/id@.
data ProviderModelConfig = ProviderModelConfig
  { pmcProvider               :: !Text
  , pmcId                     :: !Text
  , pmcModelId                :: !(Maybe Text)
  , pmcWireModel              :: !(Maybe Text)
  , pmcName                   :: !(Maybe Text)
  , pmcMaxPromptTokens        :: !(Maybe Int)
  , pmcMaxContextWindowTokens :: !(Maybe Int)
  , pmcMaxOutputTokens        :: !(Maybe Int)
  , pmcCapabilities           :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

-- | A model with the given provider and ID and nothing else set.
defaultProviderModelConfig :: Text -> Text -> ProviderModelConfig
defaultProviderModelConfig provider modelId =
  ProviderModelConfig provider modelId Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ProviderModelConfig where
  toJSON ProviderModelConfig{..} = object $ catMaybes
    [ Just ("provider" .= pmcProvider)
    , Just ("id" .= pmcId)
    , ("modelId" .=) <$> pmcModelId
    , ("wireModel" .=) <$> pmcWireModel
    , ("name" .=) <$> pmcName
    , ("maxPromptTokens" .=) <$> pmcMaxPromptTokens
    , ("maxContextWindowTokens" .=) <$> pmcMaxContextWindowTokens
    , ("maxOutputTokens" .=) <$> pmcMaxOutputTokens
    , ("capabilities" .=) <$> pmcCapabilities
    ]

-- | Copilot API (CAPI) options (@github.capi@).
data CapiConfig = CapiConfig
  { capiEnabled                  :: !(Maybe Bool)  -- ^ Enable or disable Copilot models
  , capiAutoTier                 :: !(Maybe Text)  -- ^ Auto routing preference (see 'autoTierBalance')
  , capiEnableWebSocketResponses :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)

-- | CAPI options with every field unset.
defaultCapiConfig :: CapiConfig
defaultCapiConfig = CapiConfig Nothing Nothing Nothing

instance ToJSON CapiConfig where
  toJSON CapiConfig{..} = object $ catMaybes
    [ ("enabled" .=) <$> capiEnabled
    , ("autoTier" .=) <$> capiAutoTier
    , ("enableWebSocketResponses" .=) <$> capiEnableWebSocketResponses
    ]

-- | A password or other answer that a shell command run by the agent asks for
-- (@shellAuthentication.request@). The answer is never shown to the model.
data ShellAuthenticationRequest = ShellAuthenticationRequest
  { sarSessionId :: !Text
  , sarRequestId :: !Text
  , sarProgram   :: !Text   -- ^ @sudo@, @ssh@, or @git@
  , sarPrompt    :: !Text
  , sarInputKind :: !Text   -- ^ @secret@, @text@, or @confirm@
  , sarCommand   :: !Text
  , sarCwd       :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON ShellAuthenticationRequest where
  parseJSON = withObject "ShellAuthenticationRequest" $ \o ->
    ShellAuthenticationRequest
      <$> o .:? "sessionId" .!= ""
      <*> o .:  "requestId"
      <*> o .:? "program" .!= ""
      <*> o .:? "prompt" .!= ""
      <*> o .:? "inputKind" .!= ""
      <*> o .:? "command" .!= ""
      <*> o .:? "cwd" .!= ""

-- | Answer to a 'ShellAuthenticationRequest'.
data ShellAuthenticationResponse
  = ShellAuthenticationAccept Text
  | ShellAuthenticationCancel
  deriving (Show, Eq, Generic)

instance ToJSON ShellAuthenticationResponse where
  toJSON (ShellAuthenticationAccept v) = object [ "action" .= ("accept" :: Text), "value" .= v ]
  toJSON ShellAuthenticationCancel     = object [ "action" .= ("cancel" :: Text) ]

-- | Answers shell authentication requests for a session.
type ShellAuthenticationHandler = ShellAuthenticationRequest -> IO ShellAuthenticationResponse

-- | A memory the agent asked to store (@memoryStore.store@).
data StoredMemory = StoredMemory
  { stmSessionId :: !Text
  , stmSubject   :: !Text  -- ^ Short topic the memory is about
  , stmFact      :: !Text  -- ^ The fact to remember
  , stmCitations :: !Text  -- ^ Sources supporting the fact
  , stmReason    :: !Text  -- ^ Why the agent considers the fact worth remembering
  } deriving (Show, Eq, Generic)

instance FromJSON StoredMemory where
  parseJSON = withObject "StoredMemory" $ \o ->
    StoredMemory
      <$> o .:? "sessionId" .!= ""
      <*> o .:? "subject" .!= ""
      <*> o .:? "fact" .!= ""
      <*> o .:? "citations" .!= ""
      <*> o .:? "reason" .!= ""

-- | Stores memories the agent asks to keep.
type StoreMemoryHandler = StoredMemory -> IO ()

-- | A proxy Basic authentication challenge (@network.authenticateBasic@).
data NetworkBasicAuthRequest = NetworkBasicAuthRequest
  { nbarProxy            :: !Text
  , nbarRealm            :: !(Maybe Text)
  , nbarPreviousRejected :: !Bool
  } deriving (Show, Eq, Generic)

-- | Proxy credentials. Never log them.
data NetworkBasicCredentials = NetworkBasicCredentials
  { nbcUsername :: !Text
  , nbcPassword :: !Text
  } deriving (Eq, Generic)

-- | Network callbacks registered with @network.setProvider@ after connecting.
data NetworkOptions = NetworkOptions
  { noScope                 :: !(Maybe Text)  -- ^ @connection@ or @runtime@
  , noResolveProxy          :: !(Maybe (Text -> IO (Maybe Text)))
    -- ^ URL to a PAC-style proxy list, or 'Nothing' for default routing
  , noAuthenticateNegotiate :: !(Maybe (Text -> IO (Maybe Text)))
    -- ^ Service principal to a base64 SPNEGO token
  , noAuthenticateBasic     :: !(Maybe (NetworkBasicAuthRequest -> IO (Maybe NetworkBasicCredentials)))
  }

-- | Network options with no callbacks.
defaultNetworkOptions :: NetworkOptions
defaultNetworkOptions = NetworkOptions Nothing Nothing Nothing Nothing

-- | A structured Policy Helper failure carried in RPC error data
-- (@error.data.policyHelperFailure@). Treat the text as untrusted.
data PolicyHelperFailure = PolicyHelperFailure
  { phfCategory               :: !Text
  , phfFailureMode            :: !Text
  , phfMessage                :: !Text
  , phfPreviousPolicyRetained :: !Bool
  , phfReason                 :: !Text
  , phfAuthority              :: !(Maybe Text)
  , phfCode                   :: !(Maybe Text)
  , phfDetails                :: !(Maybe Value)
  , phfDiagnostic             :: !(Maybe Text)
  , phfHelpLink               :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

instance FromJSON PolicyHelperFailure where
  parseJSON = withObject "PolicyHelperFailure" $ \o ->
    PolicyHelperFailure
      <$> o .:? "category" .!= ""
      <*> o .:? "failureMode" .!= ""
      <*> o .:? "message" .!= ""
      <*> o .:? "previousPolicyRetained" .!= False
      <*> o .:? "reason" .!= ""
      <*> o .:? "authority"
      <*> o .:? "code"
      <*> o .:? "details"
      <*> o .:? "diagnostic"
      <*> o .:? "helpLink"

-- | Extract the Policy Helper failure from JSON-RPC error data.
policyHelperFailureFromErrorData :: Maybe Value -> Maybe PolicyHelperFailure
policyHelperFailureFromErrorData (Just (Object o)) =
  case KM.lookup "policyHelperFailure" o of
    Just v  -> case fromJSON v of
      Aeson.Success f -> Just f
      Aeson.Error _   -> Nothing
    Nothing -> Nothing
policyHelperFailureFromErrorData _ = Nothing

-- | Known refusal reason of a 'PermissionModeResult'.
permissionModeRefusalManagedPolicy :: Text
permissionModeRefusalManagedPolicy = "managed_policy"

-- | Result of 'Copilot.Session.setPermissionMode'.
data PermissionModeResult = PermissionModeResult
  { pmrSuccess :: !Bool
  , pmrMode    :: !Text
  , pmrReason  :: !(Maybe Text)  -- ^ For example 'permissionModeRefusalManagedPolicy'
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionModeResult where
  parseJSON = withObject "PermissionModeResult" $ \o ->
    PermissionModeResult
      <$> o .:? "success" .!= False
      <*> o .:? "mode" .!= ""
      <*> o .:? "reason"

-- | Event type of live shell output chunks.
sessionEventToolShellOutput :: Text
sessionEventToolShellOutput = "tool.shell_output"

-- | Data of a @tool.shell_output@ event ('seData').
data ShellOutputData = ShellOutputData
  { sodToolCallId :: !Text
  , sodText       :: !Text
  , sodSequence   :: !Int
  , sodStream     :: !(Maybe Text)  -- ^ @stdout@, @stderr@, or omitted
  } deriving (Show, Eq, Generic)

instance FromJSON ShellOutputData where
  parseJSON = withObject "ShellOutputData" $ \o ->
    ShellOutputData
      <$> o .:? "toolCallId" .!= ""
      <*> o .:? "text" .!= ""
      <*> o .:? "sequence" .!= 0
      <*> o .:? "stream"

-- ============================================================================
-- v3.1.0 MCP Secret Store & Plugin Extension Approval Types
-- ============================================================================

-- | Result of 'Copilot.Client.mcpSecretsSave'.
newtype McpSecretsSaveResult = McpSecretsSaveResult
  { mssrUsedKeychain :: Bool
    -- ^ 'True' when the OS keychain stored the credential, 'False' when the
    -- runtime used its file backend
  } deriving (Show, Eq, Generic)

instance FromJSON McpSecretsSaveResult where
  parseJSON = withObject "McpSecretsSaveResult" $ \o ->
    McpSecretsSaveResult <$> o .:? "usedKeychain" .!= False

-- | A user-scoped approval for one installed plugin extension.
data PermissionsExtensionApproval = PermissionsExtensionApproval
  { peaExtensionName     :: !Text
    -- ^ Stable identity, for example @plugin:plugin-name:extension-name@
  , peaPluginName        :: !Text
  , peaCapabilities      :: ![Text]
    -- ^ Exact privileged capabilities the user approved
  , peaGrantedAt         :: !Text          -- ^ RFC 3339 timestamp
  , peaInstallationKey   :: !Text
    -- ^ Opaque; pass it back unchanged to 'Copilot.Client.extensionApprovalsRevoke'
  , peaInstallationLabel :: !(Maybe Text)  -- ^ Human-readable installation source
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionsExtensionApproval where
  parseJSON = withObject "PermissionsExtensionApproval" $ \o ->
    PermissionsExtensionApproval
      <$> o .:? "extensionName" .!= ""
      <*> o .:? "pluginName" .!= ""
      <*> o .:? "capabilities" .!= []
      <*> o .:? "grantedAt" .!= ""
      <*> o .:? "installationKey" .!= ""
      <*> o .:? "installationLabel"

-- | Result of 'Copilot.Client.extensionApprovalsGrant'.
data PermissionsExtensionApprovalsGrantResult = PermissionsExtensionApprovalsGrantResult
  { peagGranted  :: !Bool
    -- ^ Whether the pending request was eligible and its approval persisted
  , peagApproval :: !(Maybe PermissionsExtensionApproval)
    -- ^ The persisted approval when 'peagGranted' is 'True'
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionsExtensionApprovalsGrantResult where
  parseJSON = withObject "PermissionsExtensionApprovalsGrantResult" $ \o ->
    PermissionsExtensionApprovalsGrantResult
      <$> o .:? "granted" .!= False
      <*> o .:? "approval"

-- | Result of 'Copilot.Client.extensionApprovalsList'.
newtype PermissionsExtensionApprovalsListResult = PermissionsExtensionApprovalsListResult
  { pealApprovals :: [PermissionsExtensionApproval]
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionsExtensionApprovalsListResult where
  parseJSON = withObject "PermissionsExtensionApprovalsListResult" $ \o ->
    PermissionsExtensionApprovalsListResult <$> o .:? "approvals" .!= []

-- | Result of 'Copilot.Client.extensionApprovalsRevoke'.
newtype PermissionsExtensionApprovalsRevokeResult = PermissionsExtensionApprovalsRevokeResult
  { pearRevoked :: Bool  -- ^ Whether a matching approval existed and was removed
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionsExtensionApprovalsRevokeResult where
  parseJSON = withObject "PermissionsExtensionApprovalsRevokeResult" $ \o ->
    PermissionsExtensionApprovalsRevokeResult <$> o .:? "revoked" .!= False

-- ============================================================================
-- Response Format & Image Types
-- ============================================================================

-- | Response format for message responses.
data ResponseFormat = ResponseFormatText | ResponseFormatImage | ResponseFormatJsonObject
  deriving (Show, Eq)

instance ToJSON ResponseFormat where
  toJSON ResponseFormatText       = String "text"
  toJSON ResponseFormatImage      = String "image"
  toJSON ResponseFormatJsonObject = String "json_object"

instance FromJSON ResponseFormat where
  parseJSON = withText "ResponseFormat" $ \case
    "text"        -> pure ResponseFormatText
    "image"       -> pure ResponseFormatImage
    "json_object" -> pure ResponseFormatJsonObject
    other         -> fail $ "Unknown response format: " <> T.unpack other

-- | Options for image generation.
data ImageOptions = ImageOptions
  { imageOptionsSize    :: Maybe Text
  , imageOptionsQuality :: Maybe Text
  , imageOptionsStyle   :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON ImageOptions where
  toJSON ImageOptions{..} = object $ catMaybes
    [ ("size" .=) <$> imageOptionsSize
    , ("quality" .=) <$> imageOptionsQuality
    , ("style" .=) <$> imageOptionsStyle
    ]

instance FromJSON ImageOptions where
  parseJSON = withObject "ImageOptions" $ \o -> ImageOptions
    <$> o .:? "size"
    <*> o .:? "quality"
    <*> o .:? "style"

-- | Image data from an assistant image response.
data AssistantImageData = AssistantImageData
  { assistantImageFormat        :: Text
  , assistantImageBase64        :: Text
  , assistantImageUrl           :: Maybe Text
  , assistantImageRevisedPrompt :: Maybe Text
  , assistantImageWidth         :: Int
  , assistantImageHeight        :: Int
  } deriving (Show, Eq)

instance ToJSON AssistantImageData where
  toJSON AssistantImageData{..} = object
    [ "format"        .= assistantImageFormat
    , "base64"        .= assistantImageBase64
    , "url"           .= assistantImageUrl
    , "revisedPrompt" .= assistantImageRevisedPrompt
    , "width"         .= assistantImageWidth
    , "height"        .= assistantImageHeight
    ]

instance FromJSON AssistantImageData where
  parseJSON = withObject "AssistantImageData" $ \o -> AssistantImageData
    <$> o .: "format"
    <*> o .: "base64"
    <*> o .:? "url"
    <*> o .:? "revisedPrompt"
    <*> o .: "width"
    <*> o .: "height"

-- | A content block in a mixed text+image response.
data ContentBlock
  = TextBlock Text
  | ImageBlock AssistantImageData
  deriving (Show, Eq)

instance ToJSON ContentBlock where
  toJSON (TextBlock t)  = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ImageBlock i) = object ["type" .= ("image" :: Text), "image" .= i]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text"  -> TextBlock <$> o .: "text"
      "image" -> ImageBlock <$> o .: "image"
      _       -> fail $ "Unknown content block type: " <> T.unpack typ

-- ============================================================================
-- Message Options & Attachments
-- ============================================================================

-- | Attachment to a message (file, directory, or selection).
data Attachment = Attachment
  { attType        :: !Text
  , attPath        :: !(Maybe Text)
  , attFilePath    :: !(Maybe Text)
  , attDisplayName :: !(Maybe Text)
  , attText        :: !(Maybe Text)
  , attSelection   :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

instance ToJSON Attachment where
  toJSON Attachment{..} = object
    [ "type"        .= attType
    , "path"        .= attPath
    , "filePath"    .= attFilePath
    , "displayName" .= attDisplayName
    , "text"        .= attText
    , "selection"   .= attSelection
    ]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment
      <$> o .:  "type"
      <*> o .:? "path"
      <*> o .:? "filePath"
      <*> o .:? "displayName"
      <*> o .:? "text"
      <*> o .:? "selection"

-- | Options for sending a message to a session.
data MessageOptions = MessageOptions
  { moPrompt         :: !Text
  , moAttachments    :: !(Maybe [Attachment])
  , moMode           :: !(Maybe Text)  -- ^ "enqueue" or "immediate"
  , moResponseFormat :: !(Maybe ResponseFormat)  -- ^ Legacy format; no longer sent (use 'moResponseSchema')
  , moImageOptions   :: !(Maybe ImageOptions)
  , moRequestHeaders :: !(Maybe (Map.Map Text Text))  -- ^ Custom HTTP headers for outbound model requests
  , moAgentMode      :: !(Maybe Text)  -- ^ Agent mode override for this message
  , moDisplayPrompt  :: !(Maybe Text)  -- ^ Prompt text to display in transcripts (may differ from the model prompt)
  , moSource         :: !(Maybe MessageSource)  -- ^ 'messageSourceUser', 'messageSourceSystem', or 'messageSourceAgent'
  , moClientTurnId   :: !(Maybe Text)  -- ^ Opaque caller-chosen turn identifier
  , moResponseSchema :: !(Maybe Value)  -- ^ JSON Schema for structured output (sent as responseFormat)
  } deriving (Show, Eq, Generic)

-- | Message options with the given prompt and everything else unset.
defaultMessageOptions :: Text -> MessageOptions
defaultMessageOptions prompt = MessageOptions
  { moPrompt         = prompt
  , moAttachments    = Nothing
  , moMode           = Nothing
  , moResponseFormat = Nothing
  , moImageOptions   = Nothing
  , moRequestHeaders = Nothing
  , moAgentMode      = Nothing
  , moDisplayPrompt  = Nothing
  , moSource         = Nothing
  , moClientTurnId   = Nothing
  , moResponseSchema = Nothing
  }

-- ============================================================================
-- Permission Types
-- ============================================================================

-- | A permission request from the server.
data PermissionRequest = PermissionRequest
  { prKind       :: !Text
  , prToolCallId :: !(Maybe Text)
  , prExtra      :: !(Map.Map Text Value)
  } deriving (Show, Eq, Generic)

instance FromJSON PermissionRequest where
  parseJSON = withObject "PermissionRequest" $ \o -> do
    kind <- o .: "kind"
    toolCallId <- o .:? "toolCallId"
    -- Parse the entire object as a Map, then remove known keys
    let allFields = case fromJSON (Object o) of
          Aeson.Success (m :: Map.Map Text Value) -> m
          Aeson.Error _   -> Map.empty
        extra = Map.delete "kind" . Map.delete "toolCallId" $ allFields
    pure $ PermissionRequest kind toolCallId extra

instance ToJSON PermissionRequest where
  toJSON PermissionRequest{..} =
    let base = Map.insert "kind" (toJSON prKind)
             $ maybe id (\tc -> Map.insert "toolCallId" (toJSON tc)) prToolCallId
             $ prExtra
    in  toJSON base

-- | Result of a permission request.
data PermissionRequestResult = PermissionRequestResult
  { prrKind  :: !Text
  , prrRules :: !(Maybe [Value])
  } deriving (Show, Eq, Generic)

instance ToJSON PermissionRequestResult where
  toJSON PermissionRequestResult{..} = object
    [ "kind"  .= prrKind
    , "rules" .= prrRules
    ]

instance FromJSON PermissionRequestResult where
  parseJSON = withObject "PermissionRequestResult" $ \o ->
    PermissionRequestResult
      <$> o .:  "kind"
      <*> o .:? "rules"

-- | Permission handler callback.
type PermissionHandler = PermissionRequest -> Text -> IO PermissionRequestResult

-- ============================================================================
-- User Input Types
-- ============================================================================

-- | Request for user input from the agent.
--
-- 'uirImageAttachmentConstraints' is set when the runtime accepts image
-- attachments in the answer (see 'UserInputImageResponse') and 'Nothing'
-- otherwise. 'uirRequestId' is the pending request ID and 'uirMeta' the
-- request's opaque @_meta@ object, when the runtime sends them.
data UserInputRequest = UserInputRequest
  { uirQuestion                   :: !Text
  , uirChoices                    :: !(Maybe [Text])
  , uirAllowFreeform              :: !(Maybe Bool)
  , uirRequestId                  :: !(Maybe Text)
  , uirImageAttachmentConstraints :: !(Maybe UserInputImageAttachmentConstraints)
  , uirMeta                       :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

instance ToJSON UserInputRequest where
  toJSON UserInputRequest{..} = object $
    [ "question"      .= uirQuestion
    , "choices"       .= uirChoices
    , "allowFreeform" .= uirAllowFreeform
    ] ++ catMaybes
    [ ("requestId" .=) <$> uirRequestId
    , ("imageAttachmentConstraints" .=) <$> uirImageAttachmentConstraints
    , ("_meta" .=) <$> uirMeta
    ]

instance FromJSON UserInputRequest where
  parseJSON = withObject "UserInputRequest" $ \o ->
    UserInputRequest
      <$> o .:  "question"
      <*> o .:? "choices"
      <*> o .:? "allowFreeform"
      <*> o .:? "requestId"
      <*> o .:? "imageAttachmentConstraints"
      <*> o .:? "_meta"

-- | Response to a user input request. It is text only; to answer with images,
-- return a 'UserInputImageResponse' from a 'UserInputImageHandler'.
data UserInputResponse = UserInputResponse
  { uirAnswer      :: !Text
  , uirWasFreeform :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON UserInputResponse where
  toJSON UserInputResponse{..} = object
    [ "answer"      .= uirAnswer
    , "wasFreeform" .= uirWasFreeform
    ]

instance FromJSON UserInputResponse where
  parseJSON = withObject "UserInputResponse" $ \o ->
    UserInputResponse
      <$> o .:  "answer"
      <*> o .:  "wasFreeform"

-- | User input handler callback.
type UserInputHandler = UserInputRequest -> Text -> IO UserInputResponse

-- | Image support advertised by a @userInput.request@
-- ('uirImageAttachmentConstraints'). Absent when the runtime does not accept
-- images in the answer.
data UserInputImageAttachmentConstraints = UserInputImageAttachmentConstraints
  { uiacAllowedMimeTypes :: ![Text]
  , uiacMaxBytesPerImage :: !Int
  , uiacMaxCount         :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON UserInputImageAttachmentConstraints where
  toJSON UserInputImageAttachmentConstraints{..} = object
    [ "allowedMimeTypes" .= uiacAllowedMimeTypes
    , "maxBytesPerImage" .= uiacMaxBytesPerImage
    , "maxCount"         .= uiacMaxCount
    ]

instance FromJSON UserInputImageAttachmentConstraints where
  parseJSON = withObject "UserInputImageAttachmentConstraints" $ \o ->
    UserInputImageAttachmentConstraints
      <$> o .:? "allowedMimeTypes" .!= []
      <*> o .:? "maxBytesPerImage" .!= 0
      <*> o .:? "maxCount" .!= 0

-- | An inline image attached to an ask_user answer ('uiirAttachments'). The
-- wire @type@ is always @image@ and 'uiaData' is the base64-encoded image.
-- The runtime enforces the advertised MIME types, size, and count limits.
data UserInputImageAttachment = UserInputImageAttachment
  { uiaData        :: !Text
  , uiaMimeType    :: !Text
  , uiaDescription :: !(Maybe Text)
  , uiaDisplayName :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | An image attachment with the given base64 data and MIME type and no
-- description or display name.
defaultUserInputImageAttachment :: Text -> Text -> UserInputImageAttachment
defaultUserInputImageAttachment imageData mimeType =
  UserInputImageAttachment imageData mimeType Nothing Nothing

instance ToJSON UserInputImageAttachment where
  toJSON UserInputImageAttachment{..} = object $ catMaybes
    [ Just ("type" .= ("image" :: Text))
    , Just ("data" .= uiaData)
    , Just ("mimeType" .= uiaMimeType)
    , ("description" .=) <$> uiaDescription
    , ("displayName" .=) <$> uiaDisplayName
    ]

instance FromJSON UserInputImageAttachment where
  parseJSON = withObject "UserInputImageAttachment" $ \o ->
    UserInputImageAttachment
      <$> o .:  "data"
      <*> o .:  "mimeType"
      <*> o .:? "description"
      <*> o .:? "displayName"

-- | A user input answer that can carry image attachments, returned by a
-- 'UserInputImageHandler'. 'uiirAttachments' is only valid when the request
-- advertised 'uirImageAttachmentConstraints' and is sent only when non-empty.
-- For an image-only answer use an empty 'uiirAnswer' and 'uiirWasFreeform'
-- 'True', as 'defaultUserInputImageResponse' does.
data UserInputImageResponse = UserInputImageResponse
  { uiirAnswer      :: !Text
  , uiirWasFreeform :: !Bool
  , uiirAttachments :: ![UserInputImageAttachment]
  } deriving (Show, Eq, Generic)

-- | An image-only answer without images yet: empty 'uiirAnswer',
-- 'uiirWasFreeform' 'True', and no 'uiirAttachments'. Fill it in with record
-- update syntax.
defaultUserInputImageResponse :: UserInputImageResponse
defaultUserInputImageResponse = UserInputImageResponse
  { uiirAnswer      = ""
  , uiirWasFreeform = True
  , uiirAttachments = []
  }

instance ToJSON UserInputImageResponse where
  toJSON UserInputImageResponse{..} = object $
    [ "answer"      .= uiirAnswer
    , "wasFreeform" .= uiirWasFreeform
    ] ++
    [ "attachments" .= uiirAttachments | not (null uiirAttachments) ]

instance FromJSON UserInputImageResponse where
  parseJSON = withObject "UserInputImageResponse" $ \o ->
    UserInputImageResponse
      <$> o .:  "answer"
      <*> o .:  "wasFreeform"
      <*> o .:? "attachments" .!= []

-- | User input handler whose answers can carry image attachments. Set it with
-- 'scOnUserInputImageRequest' (or 'rscOnUserInputImageRequest'); when both it
-- and the plain 'UserInputHandler' are set, it takes precedence.
type UserInputImageHandler = UserInputRequest -> Text -> IO UserInputImageResponse

-- ============================================================================
-- Exit Plan Mode Types
-- ============================================================================

-- | Request to exit plan mode, sent by the agent when it wants to leave
-- planning and begin acting on a plan.
newtype ExitPlanModeRequest = ExitPlanModeRequest
  { epmrSessionId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ExitPlanModeRequest where
  parseJSON = withObject "ExitPlanModeRequest" $ \o ->
    ExitPlanModeRequest <$> o .:? "sessionId" .!= ""

instance ToJSON ExitPlanModeRequest where
  toJSON ExitPlanModeRequest{..} = object
    [ "sessionId" .= epmrSessionId ]

-- | Response to an exit-plan-mode request. 'True' approves exiting plan mode.
newtype ExitPlanModeResponse = ExitPlanModeResponse
  { epmApproved :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ExitPlanModeResponse where
  toJSON ExitPlanModeResponse{..} = object
    [ "approved" .= epmApproved ]

instance FromJSON ExitPlanModeResponse where
  parseJSON = withObject "ExitPlanModeResponse" $ \o ->
    ExitPlanModeResponse <$> o .:? "approved" .!= True

-- | Exit-plan-mode handler callback.
type ExitPlanModeHandler = ExitPlanModeRequest -> IO ExitPlanModeResponse

-- ============================================================================
-- Trace Context Types
-- ============================================================================

-- | W3C Trace Context (@traceparent@/@tracestate@) for distributed tracing.
data TraceContext = TraceContext
  { tcTraceparent :: !(Maybe Text)
  , tcTracestate  :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON TraceContext where
  toJSON TraceContext{..} = object
    [ "traceparent" .= tcTraceparent
    , "tracestate"  .= tcTracestate
    ]

instance FromJSON TraceContext where
  parseJSON = withObject "TraceContext" $ \o ->
    TraceContext
      <$> o .:? "traceparent"
      <*> o .:? "tracestate"

-- | Callback that returns the current W3C Trace Context. Wire this up to your
-- OpenTelemetry (or other tracing) SDK to propagate traces to the Copilot CLI.
type TraceContextProvider = IO TraceContext

-- ============================================================================
-- Hook Types
-- ============================================================================

-- | Context about a hook invocation.
data HookInvocation = HookInvocation
  { hiSessionId :: !Text
  } deriving (Show, Eq, Generic)

-- | Input for pre-tool-use hook.
data PreToolUseHookInput = PreToolUseHookInput
  { ptuTimestamp :: !Integer
  , ptuCwd      :: !Text
  , ptuToolName :: !Text
  , ptuToolArgs :: !Value
  } deriving (Show, Eq, Generic)

instance FromJSON PreToolUseHookInput where
  parseJSON = withObject "PreToolUseHookInput" $ \o ->
    PreToolUseHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "toolName"
      <*> o .: "toolArgs"

-- | Output for pre-tool-use hook.
data PreToolUseHookOutput = PreToolUseHookOutput
  { ptuoPermissionDecision       :: !(Maybe Text)
  , ptuoPermissionDecisionReason :: !(Maybe Text)
  , ptuoModifiedArgs             :: !(Maybe Value)
  , ptuoAdditionalContext        :: !(Maybe Text)
  , ptuoSuppressOutput           :: !(Maybe Bool)
  , ptuoDecisionContext          :: !(Maybe Value)  -- ^ Opaque context accompanying the permission decision (decisionContext)
  } deriving (Show, Eq, Generic)

instance ToJSON PreToolUseHookOutput where
  toJSON PreToolUseHookOutput{..} = object
    [ "permissionDecision"       .= ptuoPermissionDecision
    , "permissionDecisionReason" .= ptuoPermissionDecisionReason
    , "modifiedArgs"             .= ptuoModifiedArgs
    , "additionalContext"        .= ptuoAdditionalContext
    , "suppressOutput"           .= ptuoSuppressOutput
    , "decisionContext"          .= ptuoDecisionContext
    ]

-- | Input for post-tool-use hook.
data PostToolUseHookInput = PostToolUseHookInput
  { potuTimestamp  :: !Integer
  , potuCwd       :: !Text
  , potuToolName  :: !Text
  , potuToolArgs  :: !Value
  , potuToolResult :: !Value
  } deriving (Show, Eq, Generic)

instance FromJSON PostToolUseHookInput where
  parseJSON = withObject "PostToolUseHookInput" $ \o ->
    PostToolUseHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "toolName"
      <*> o .: "toolArgs"
      <*> o .: "toolResult"

-- | Output for post-tool-use hook.
data PostToolUseHookOutput = PostToolUseHookOutput
  { potuoModifiedResult    :: !(Maybe Value)
  , potuoAdditionalContext :: !(Maybe Text)
  , potuoSuppressOutput    :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)

instance ToJSON PostToolUseHookOutput where
  toJSON PostToolUseHookOutput{..} = object
    [ "modifiedResult"    .= potuoModifiedResult
    , "additionalContext" .= potuoAdditionalContext
    , "suppressOutput"    .= potuoSuppressOutput
    ]

-- | Input for user-prompt-submitted hook.
data UserPromptSubmittedHookInput = UserPromptSubmittedHookInput
  { upsTimestamp :: !Integer
  , upsCwd      :: !Text
  , upsPrompt   :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON UserPromptSubmittedHookInput where
  parseJSON = withObject "UserPromptSubmittedHookInput" $ \o ->
    UserPromptSubmittedHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "prompt"

-- | Output for user-prompt-submitted hook.
data UserPromptSubmittedHookOutput = UserPromptSubmittedHookOutput
  { upsoModifiedPrompt    :: !(Maybe Text)
  , upsoAdditionalContext :: !(Maybe Text)
  , upsoSuppressOutput    :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)

instance ToJSON UserPromptSubmittedHookOutput where
  toJSON UserPromptSubmittedHookOutput{..} = object
    [ "modifiedPrompt"    .= upsoModifiedPrompt
    , "additionalContext" .= upsoAdditionalContext
    , "suppressOutput"    .= upsoSuppressOutput
    ]

-- | Input for session-start hook.
data SessionStartHookInput = SessionStartHookInput
  { sshTimestamp     :: !Integer
  , sshCwd          :: !Text
  , sshSource       :: !Text
  , sshInitialPrompt :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON SessionStartHookInput where
  parseJSON = withObject "SessionStartHookInput" $ \o ->
    SessionStartHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "source"
      <*> o .:? "initialPrompt"

-- | Output for session-start hook.
data SessionStartHookOutput = SessionStartHookOutput
  { sshoAdditionalContext :: !(Maybe Text)
  , sshoModifiedConfig    :: !(Maybe (Map.Map Text Value))
  } deriving (Show, Eq, Generic)

instance ToJSON SessionStartHookOutput where
  toJSON SessionStartHookOutput{..} = object
    [ "additionalContext" .= sshoAdditionalContext
    , "modifiedConfig"    .= sshoModifiedConfig
    ]

-- | Input for session-end hook.
data SessionEndHookInput = SessionEndHookInput
  { sehTimestamp    :: !Integer
  , sehCwd         :: !Text
  , sehReason      :: !Text
  , sehFinalMessage :: !(Maybe Text)
  , sehError       :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON SessionEndHookInput where
  parseJSON = withObject "SessionEndHookInput" $ \o ->
    SessionEndHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "reason"
      <*> o .:? "finalMessage"
      <*> o .:? "error"

-- | Output for session-end hook.
data SessionEndHookOutput = SessionEndHookOutput
  { sehoSuppressOutput :: !(Maybe Bool)
  , sehoCleanupActions :: !(Maybe [Text])
  , sehoSessionSummary :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SessionEndHookOutput where
  toJSON SessionEndHookOutput{..} = object
    [ "suppressOutput" .= sehoSuppressOutput
    , "cleanupActions" .= sehoCleanupActions
    , "sessionSummary" .= sehoSessionSummary
    ]

-- | Input for error-occurred hook.
data ErrorOccurredHookInput = ErrorOccurredHookInput
  { eohTimestamp    :: !Integer
  , eohCwd         :: !Text
  , eohError       :: !Text
  , eohErrorContext :: !Text
  , eohRecoverable :: !Bool
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorOccurredHookInput where
  parseJSON = withObject "ErrorOccurredHookInput" $ \o ->
    ErrorOccurredHookInput
      <$> o .: "timestamp"
      <*> o .: "cwd"
      <*> o .: "error"
      <*> o .: "errorContext"
      <*> o .: "recoverable"

-- | Output for error-occurred hook.
data ErrorOccurredHookOutput = ErrorOccurredHookOutput
  { eohoSuppressOutput   :: !(Maybe Bool)
  , eohoErrorHandling    :: !(Maybe Text)
  , eohoRetryCount       :: !(Maybe Int)
  , eohoUserNotification :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorOccurredHookOutput where
  toJSON ErrorOccurredHookOutput{..} = object
    [ "suppressOutput"   .= eohoSuppressOutput
    , "errorHandling"    .= eohoErrorHandling
    , "retryCount"       .= eohoRetryCount
    , "userNotification" .= eohoUserNotification
    ]

-- | Input for the hook fired before a sub-agent's first turn (@subagentStart@).
-- Session metadata belongs to the parent session.
data SubagentStartHookInput = SubagentStartHookInput
  { ssaTimestamp        :: !(Maybe Value)
  , ssaWorkingDirectory :: !Text
  , ssaTranscriptPath   :: !Text
  , ssaAgentName        :: !Text
  , ssaAgentDisplayName :: !(Maybe Text)
  , ssaAgentDescription :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON SubagentStartHookInput where
  parseJSON = withObject "SubagentStartHookInput" $ \o -> do
    wd  <- o .:? "workingDirectory"
    cwd <- o .:? "cwd"
    SubagentStartHookInput
      <$> o .:? "timestamp"
      <*> pure (maybe (maybe "" id cwd) id wd)
      <*> o .:? "transcriptPath" .!= ""
      <*> o .:? "agentName" .!= ""
      <*> o .:? "agentDisplayName"
      <*> o .:? "agentDescription"

-- | Output for the @subagentStart@ hook; @additionalContext@ is prepended to
-- the sub-agent's initial prompt.
newtype SubagentStartHookOutput = SubagentStartHookOutput
  { ssaoAdditionalContext :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON SubagentStartHookOutput where
  toJSON SubagentStartHookOutput{..} = object $ catMaybes
    [ ("additionalContext" .=) <$> ssaoAdditionalContext ]

-- | Input for the hook fired after a sub-agent completes a turn (@subagentStop@).
data SubagentStopHookInput = SubagentStopHookInput
  { sstTimestamp        :: !(Maybe Value)
  , sstWorkingDirectory :: !Text
  , sstTranscriptPath   :: !Text
  , sstAgentName        :: !Text
  , sstAgentDisplayName :: !(Maybe Text)
  , sstAgentDescription :: !(Maybe Text)
  , sstAgentId          :: !(Maybe Text)
  , sstAgentType        :: !Text
  , sstStopReason       :: !Text
  , sstResponse         :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON SubagentStopHookInput where
  parseJSON = withObject "SubagentStopHookInput" $ \o -> do
    wd  <- o .:? "workingDirectory"
    cwd <- o .:? "cwd"
    SubagentStopHookInput
      <$> o .:? "timestamp"
      <*> pure (maybe (maybe "" id cwd) id wd)
      <*> o .:? "transcriptPath" .!= ""
      <*> o .:? "agentName" .!= ""
      <*> o .:? "agentDisplayName"
      <*> o .:? "agentDescription"
      <*> o .:? "agentId"
      <*> o .:? "agentType" .!= ""
      <*> o .:? "stopReason" .!= ""
      <*> o .:? "response" .!= ""

-- | Output for the @subagentStop@ hook: decision @block@ with a reason keeps
-- the sub-agent going; @modifiedResponse@ replaces the reported response.
data SubagentStopHookOutput = SubagentStopHookOutput
  { sstoDecision         :: !(Maybe Text)
  , sstoReason           :: !(Maybe Text)
  , sstoModifiedResponse :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SubagentStopHookOutput where
  toJSON SubagentStopHookOutput{..} = object $ catMaybes
    [ ("decision" .=) <$> sstoDecision
    , ("reason" .=) <$> sstoReason
    , ("modifiedResponse" .=) <$> sstoModifiedResponse
    ]

-- | Session hooks configuration.
data SessionHooks = SessionHooks
  { shOnPreToolUse          :: !(Maybe (PreToolUseHookInput -> HookInvocation -> IO (Maybe PreToolUseHookOutput)))
  , shOnPostToolUse         :: !(Maybe (PostToolUseHookInput -> HookInvocation -> IO (Maybe PostToolUseHookOutput)))
  , shOnPreMcpToolCall      :: !(Maybe (PreToolUseHookInput -> HookInvocation -> IO (Maybe PreToolUseHookOutput)))
  , shOnUserPromptSubmitted :: !(Maybe (UserPromptSubmittedHookInput -> HookInvocation -> IO (Maybe UserPromptSubmittedHookOutput)))
  , shOnSessionStart        :: !(Maybe (SessionStartHookInput -> HookInvocation -> IO (Maybe SessionStartHookOutput)))
  , shOnSessionEnd          :: !(Maybe (SessionEndHookInput -> HookInvocation -> IO (Maybe SessionEndHookOutput)))
  , shOnErrorOccurred       :: !(Maybe (ErrorOccurredHookInput -> HookInvocation -> IO (Maybe ErrorOccurredHookOutput)))
  , shOnUserPromptTransformed :: !(Maybe (UserPromptSubmittedHookInput -> HookInvocation -> IO (Maybe UserPromptSubmittedHookOutput)))  -- ^ Hook fired for userPromptTransformed events
  , shOnSubagentStart       :: !(Maybe (SubagentStartHookInput -> HookInvocation -> IO (Maybe SubagentStartHookOutput)))  -- ^ Hook fired before a sub-agent's first turn
  , shOnSubagentStop        :: !(Maybe (SubagentStopHookInput -> HookInvocation -> IO (Maybe SubagentStopHookOutput)))  -- ^ Hook fired after a sub-agent completes a turn
  }

-- | Default (empty) session hooks.
defaultSessionHooks :: SessionHooks
defaultSessionHooks = SessionHooks Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- ============================================================================
-- Session Events
-- ============================================================================

-- | A session event from the server.
-- Uses a generic representation: type tag + JSON data payload.
data SessionEvent = SessionEvent
  { seId        :: !Text
  , seTimestamp :: !Text
  , seParentId  :: !(Maybe Text)
  , seEphemeral :: !(Maybe Bool)
  , seAgentId   :: !(Maybe Text)
  , seType      :: !Text
  , seData      :: !Value
  } deriving (Show, Eq, Generic)

instance ToJSON SessionEvent where
  toJSON SessionEvent{..} = object
    [ "id"        .= seId
    , "timestamp" .= seTimestamp
    , "parentId"  .= seParentId
    , "ephemeral" .= seEphemeral
    , "agentId"   .= seAgentId
    , "type"      .= seType
    , "data"      .= seData
    ]

instance FromJSON SessionEvent where
  parseJSON = withObject "SessionEvent" $ \o ->
    SessionEvent
      <$> o .:  "id"
      <*> o .:  "timestamp"
      <*> o .:? "parentId"
      <*> o .:? "ephemeral"
      <*> o .:? "agentId"
      <*> o .:  "type"
      <*> o .:  "data"

-- | Session event handler callback.
type SessionEventHandler = SessionEvent -> IO ()

-- ============================================================================
-- Ping / Status / Auth Responses
-- ============================================================================

-- | Response from a ping request.
data PingResponse = PingResponse
  { pingMessage         :: !Text
  , pingTimestamp       :: !Text
  , pingProtocolVersion :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

instance FromJSON PingResponse where
  parseJSON = withObject "PingResponse" $ \o ->
    PingResponse
      <$> o .:  "message"
      <*> o .:  "timestamp"
      <*> o .:? "protocolVersion"

instance ToJSON PingResponse where
  toJSON PingResponse{..} = object
    [ "message"         .= pingMessage
    , "timestamp"       .= pingTimestamp
    , "protocolVersion" .= pingProtocolVersion
    ]

-- | Response from status.get.
data GetStatusResponse = GetStatusResponse
  { gsrVersion         :: !Text
  , gsrProtocolVersion :: !Int
  } deriving (Show, Eq, Generic)

instance FromJSON GetStatusResponse where
  parseJSON = withObject "GetStatusResponse" $ \o ->
    GetStatusResponse
      <$> o .: "version"
      <*> o .: "protocolVersion"

instance ToJSON GetStatusResponse where
  toJSON GetStatusResponse{..} = object
    [ "version"         .= gsrVersion
    , "protocolVersion" .= gsrProtocolVersion
    ]

-- | Response from auth.getStatus.
data GetAuthStatusResponse = GetAuthStatusResponse
  { gasIsAuthenticated :: !Bool
  , gasAuthType        :: !(Maybe Text)
  , gasHost            :: !(Maybe Text)
  , gasLogin           :: !(Maybe Text)
  , gasStatusMessage   :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON GetAuthStatusResponse where
  parseJSON = withObject "GetAuthStatusResponse" $ \o ->
    GetAuthStatusResponse
      <$> o .:  "isAuthenticated"
      <*> o .:? "authType"
      <*> o .:? "host"
      <*> o .:? "login"
      <*> o .:? "statusMessage"

instance ToJSON GetAuthStatusResponse where
  toJSON GetAuthStatusResponse{..} = object
    [ "isAuthenticated" .= gasIsAuthenticated
    , "authType"        .= gasAuthType
    , "host"            .= gasHost
    , "login"           .= gasLogin
    , "statusMessage"   .= gasStatusMessage
    ]

-- ============================================================================
-- Model Types
-- ============================================================================

-- | Model support flags.
data ModelSupports = ModelSupports
  { msVision          :: !Bool
  , msReasoningEffort :: !Bool
  } deriving (Show, Eq, Generic)

instance FromJSON ModelSupports where
  parseJSON = withObject "ModelSupports" $ \o ->
    ModelSupports
      <$> o .: "vision"
      <*> o .: "reasoningEffort"

instance ToJSON ModelSupports where
  toJSON ModelSupports{..} = object
    [ "vision"          .= msVision
    , "reasoningEffort" .= msReasoningEffort
    ]

-- | Vision-specific limits.
data ModelVisionLimits = ModelVisionLimits
  { mvlSupportedMediaTypes :: ![Text]
  , mvlMaxPromptImages     :: !Int
  , mvlMaxPromptImageSize  :: !Int
  } deriving (Show, Eq, Generic)

instance FromJSON ModelVisionLimits where
  parseJSON = withObject "ModelVisionLimits" $ \o ->
    ModelVisionLimits
      <$> o .: "supported_media_types"
      <*> o .: "max_prompt_images"
      <*> o .: "max_prompt_image_size"

instance ToJSON ModelVisionLimits where
  toJSON ModelVisionLimits{..} = object
    [ "supported_media_types" .= mvlSupportedMediaTypes
    , "max_prompt_images"     .= mvlMaxPromptImages
    , "max_prompt_image_size" .= mvlMaxPromptImageSize
    ]

-- | Model limits.
data ModelLimits = ModelLimits
  { mlMaxPromptTokens        :: !(Maybe Int)
  , mlMaxContextWindowTokens :: !Int
  , mlVision                 :: !(Maybe ModelVisionLimits)
  } deriving (Show, Eq, Generic)

instance FromJSON ModelLimits where
  parseJSON = withObject "ModelLimits" $ \o ->
    ModelLimits
      <$> o .:? "max_prompt_tokens"
      <*> o .:  "max_context_window_tokens"
      <*> o .:? "vision"

instance ToJSON ModelLimits where
  toJSON ModelLimits{..} = object
    [ "max_prompt_tokens"         .= mlMaxPromptTokens
    , "max_context_window_tokens" .= mlMaxContextWindowTokens
    , "vision"                    .= mlVision
    ]

-- | Model capabilities.
data ModelCapabilities = ModelCapabilities
  { mcSupports :: !ModelSupports
  , mcLimits   :: !ModelLimits
  } deriving (Show, Eq, Generic)

instance FromJSON ModelCapabilities where
  parseJSON = withObject "ModelCapabilities" $ \o ->
    ModelCapabilities
      <$> o .: "supports"
      <*> o .: "limits"

instance ToJSON ModelCapabilities where
  toJSON ModelCapabilities{..} = object
    [ "supports" .= mcSupports
    , "limits"   .= mcLimits
    ]

-- | Model policy state.
data ModelPolicy = ModelPolicy
  { mpState :: !Text
  , mpTerms :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON ModelPolicy where
  parseJSON = withObject "ModelPolicy" $ \o ->
    ModelPolicy
      <$> o .: "state"
      <*> o .: "terms"

instance ToJSON ModelPolicy where
  toJSON ModelPolicy{..} = object
    [ "state" .= mpState
    , "terms" .= mpTerms
    ]

-- | Model billing information.
data ModelBilling = ModelBilling
  { mbMultiplier :: !Double
  } deriving (Show, Eq, Generic)

instance FromJSON ModelBilling where
  parseJSON = withObject "ModelBilling" $ \o ->
    ModelBilling <$> o .: "multiplier"

instance ToJSON ModelBilling where
  toJSON ModelBilling{..} = object [ "multiplier" .= mbMultiplier ]

-- | Information about an available model.
data ModelInfo = ModelInfo
  { miId                        :: !Text
  , miName                      :: !Text
  , miCapabilities              :: !ModelCapabilities
  , miPolicy                    :: !(Maybe ModelPolicy)
  , miBilling                   :: !(Maybe ModelBilling)
  , miSupportedReasoningEfforts :: !(Maybe [Text])
  , miDefaultReasoningEffort    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \o ->
    ModelInfo
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:  "capabilities"
      <*> o .:? "policy"
      <*> o .:? "billing"
      <*> o .:? "supportedReasoningEfforts"
      <*> o .:? "defaultReasoningEffort"

instance ToJSON ModelInfo where
  toJSON ModelInfo{..} = object
    [ "id"                        .= miId
    , "name"                      .= miName
    , "capabilities"              .= miCapabilities
    , "policy"                    .= miPolicy
    , "billing"                   .= miBilling
    , "supportedReasoningEfforts" .= miSupportedReasoningEfforts
    , "defaultReasoningEffort"    .= miDefaultReasoningEffort
    ]

-- ============================================================================
-- Session Metadata & Lifecycle
-- ============================================================================

-- | Metadata about a session.
data SessionMetadata = SessionMetadata
  { smSessionId    :: !Text
  , smStartTime    :: !Text
  , smModifiedTime :: !Text
  , smSummary      :: !(Maybe Text)
  , smIsRemote     :: !Bool
  } deriving (Show, Eq, Generic)

instance FromJSON SessionMetadata where
  parseJSON = withObject "SessionMetadata" $ \o ->
    SessionMetadata
      <$> o .:  "sessionId"
      <*> o .:  "startTime"
      <*> o .:  "modifiedTime"
      <*> o .:? "summary"
      <*> o .:  "isRemote"

instance ToJSON SessionMetadata where
  toJSON SessionMetadata{..} = object
    [ "sessionId"    .= smSessionId
    , "startTime"    .= smStartTime
    , "modifiedTime" .= smModifiedTime
    , "summary"      .= smSummary
    , "isRemote"     .= smIsRemote
    ]

-- | Session lifecycle event notification.
data SessionLifecycleEvent = SessionLifecycleEvent
  { sleType      :: !Text
  , sleSessionId :: !Text
  , sleMetadata  :: !(Maybe Value)
  } deriving (Show, Eq, Generic)

instance FromJSON SessionLifecycleEvent where
  parseJSON = withObject "SessionLifecycleEvent" $ \o ->
    SessionLifecycleEvent
      <$> o .:  "type"
      <*> o .:  "sessionId"
      <*> o .:? "metadata"

instance ToJSON SessionLifecycleEvent where
  toJSON SessionLifecycleEvent{..} = object
    [ "type"      .= sleType
    , "sessionId" .= sleSessionId
    , "metadata"  .= sleMetadata
    ]

-- | Session lifecycle handler callback.
type SessionLifecycleHandler = SessionLifecycleEvent -> IO ()

-- ============================================================================
-- Session Filesystem Types
-- ============================================================================

-- | Configuration for a custom session filesystem provider.
data SessionFsConfig = SessionFsConfig
  { sfcInitialCwd      :: !Text
  , sfcSessionStatePath :: !Text
  , sfcConventions      :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON SessionFsConfig where
  parseJSON = withObject "SessionFsConfig" $ \o ->
    SessionFsConfig
      <$> o .: "initialCwd"
      <*> o .: "sessionStatePath"
      <*> o .: "conventions"

instance ToJSON SessionFsConfig where
  toJSON SessionFsConfig{..} = object
    [ "initialCwd"      .= sfcInitialCwd
    , "sessionStatePath" .= sfcSessionStatePath
    , "conventions"      .= sfcConventions
    ]

-- | File metadata returned by session filesystem operations.
data SessionFsFileInfo = SessionFsFileInfo
  { sffiName       :: !Text
  , sffiSize       :: !Int
  , sffiIsDirectory :: !Bool
  , sffiIsFile      :: !Bool
  , sffiCreatedAt   :: !(Maybe Text)
  , sffiModifiedAt  :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON SessionFsFileInfo where
  parseJSON = withObject "SessionFsFileInfo" $ \o ->
    SessionFsFileInfo
      <$> o .:  "name"
      <*> o .:  "size"
      <*> o .:  "isDirectory"
      <*> o .:  "isFile"
      <*> o .:? "createdAt"
      <*> o .:? "modifiedAt"

instance ToJSON SessionFsFileInfo where
  toJSON SessionFsFileInfo{..} = object $ catMaybes
    [ Just $ "name"        .= sffiName
    , Just $ "size"        .= sffiSize
    , Just $ "isDirectory" .= sffiIsDirectory
    , Just $ "isFile"      .= sffiIsFile
    , ("createdAt"  .=) <$> sffiCreatedAt
    , ("modifiedAt" .=) <$> sffiModifiedAt
    ]

-- | Type class for session filesystem providers.
class SessionFsProvider a where
  sfReadFile        :: a -> Text -> Text -> IO Text
  sfWriteFile       :: a -> Text -> Text -> Text -> IO ()
  sfAppendFile      :: a -> Text -> Text -> Text -> IO ()
  sfExists          :: a -> Text -> Text -> IO Bool
  sfStat            :: a -> Text -> Text -> IO SessionFsFileInfo
  sfMkdir           :: a -> Text -> Text -> Bool -> IO ()
  sfReaddir         :: a -> Text -> Text -> IO [Text]
  sfReaddirWithTypes :: a -> Text -> Text -> IO [SessionFsFileInfo]
  sfRm              :: a -> Text -> Text -> Bool -> IO ()
  sfRename          :: a -> Text -> Text -> Text -> IO ()

-- ============================================================================
-- Client Options
-- ============================================================================

-- | Options for creating a 'CopilotClient'.
data CopilotClientOptions = CopilotClientOptions
  { ccoCliPath                   :: !(Maybe FilePath)
  , ccoCliArgs                   :: ![String]
  , ccoCwd                       :: !(Maybe FilePath)
  , ccoLogLevel                  :: !Text
  , ccoAutoStart                 :: !Bool
  , ccoAutoRestart               :: !Bool
  , ccoEnv                       :: !(Maybe [(String, String)])
  , ccoGithubToken               :: !(Maybe Text)
  , ccoUseLoggedInUser           :: !(Maybe Bool)
  , ccoSessionIdleTimeoutSeconds :: !(Maybe Int)  -- ^ Server-wide idle timeout for sessions in seconds
  , ccoSessionFs                 :: !(Maybe SessionFsConfig)  -- ^ Custom session filesystem provider configuration
  , ccoCopilotHome               :: !(Maybe Text)  -- ^ Custom path to the copilot home directory
  , ccoTcpConnectionToken        :: !(Maybe Text)  -- ^ Token for TCP connections
  , ccoRequestHandler            :: !(Maybe CopilotRequestHandler)  -- ^ Intercept outbound LLM inference requests
  , ccoBearerTokenProvider       :: !(Maybe BearerTokenProvider)  -- ^ BYOK bearer-token provider (per-session)
  , ccoBuiltinPluginDirectories  :: !(Maybe [Text])  -- ^ Built-in plugin directories (builtinPluginDirectories)
  , ccoInProcess                 :: !(Maybe Bool)  -- ^ Use in-process FFI transport (inProcess)
  , ccoOnGetTraceContext         :: !(Maybe TraceContextProvider)  -- ^ Provider invoked to supply trace context (onGetTraceContext)
  , ccoClientInfo                :: !(Maybe CopilotClientInfo)  -- ^ Application identity sent on the connect handshake
  , ccoMode                      :: !ClientMode  -- ^ 'ClientModeCodingAgent' (default) or 'ClientModeEmpty'
  , ccoNetwork                   :: !(Maybe NetworkOptions)  -- ^ Proxy and proxy-authentication callbacks
  }

-- | Default client options.
defaultClientOptions :: CopilotClientOptions
defaultClientOptions = CopilotClientOptions
  { ccoCliPath                   = Nothing
  , ccoCliArgs                   = []
  , ccoCwd                       = Nothing
  , ccoLogLevel                  = "info"
  , ccoAutoStart                 = True
  , ccoAutoRestart               = True
  , ccoEnv                       = Nothing
  , ccoGithubToken               = Nothing
  , ccoUseLoggedInUser           = Nothing
  , ccoSessionIdleTimeoutSeconds = Nothing
  , ccoSessionFs                 = Nothing
  , ccoCopilotHome               = Nothing
  , ccoTcpConnectionToken        = Nothing
  , ccoRequestHandler            = Nothing
  , ccoBearerTokenProvider       = Nothing
  , ccoBuiltinPluginDirectories  = Nothing
  , ccoInProcess                 = Nothing
  , ccoOnGetTraceContext         = Nothing
  , ccoClientInfo                = Nothing
  , ccoMode                      = ClientModeCodingAgent
  , ccoNetwork                   = Nothing
  }
