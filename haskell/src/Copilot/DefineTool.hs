---------------------------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
---------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- | Helper for defining tools with typed arguments.
--
-- @
-- import qualified Data.Aeson as Aeson
-- import Copilot.DefineTool
-- import Copilot.Types
--
-- data WeatherArgs = WeatherArgs { city :: Text, unit :: Text }
--   deriving (Generic)
-- instance FromJSON WeatherArgs
--
-- weatherTool :: Tool
-- weatherTool = defineTool "get_weather" (Just "Get weather for a city")
--   (Just $ Aeson.object
--     [ "type" .= ("object" :: Text)
--     , "properties" .= Aeson.object
--         [ "city" .= Aeson.object [ "type" .= ("string" :: Text) ]
--         , "unit" .= Aeson.object [ "type" .= ("string" :: Text) ]
--         ]
--     , "required" .= (["city", "unit"] :: [Text])
--     ])
--   (\\args inv -> do
--     case Aeson.fromJSON args of
--       Aeson.Success (WeatherArgs c u) ->
--         pure $ ToolResultText $ "Weather in " <> c <> ": 22 degrees " <> u
--       Aeson.Error e ->
--         pure $ ToolResultStruct $ ToolResultObject
--           { troTextResultForLlm = "Failed to parse arguments"
--           , troBinaryResultsForLlm = Nothing
--           , troResultType = Failure
--           , troError = Just (T.pack e)
--           , troSessionLog = Nothing
--           , troToolTelemetry = Nothing
--           })
-- @
module Copilot.DefineTool
  ( defineTool
  , defineSimpleTool
  ) where

import           Data.Aeson    (Value (..), FromJSON, fromJSON, Result (..))
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Copilot.Types

-- | Define a tool with a name, description, JSON schema parameters, and a handler.
--
-- The handler receives the raw JSON arguments and the 'ToolInvocation' context.
defineTool
  :: Text                -- ^ Tool name
  -> Maybe Text          -- ^ Tool description
  -> Maybe Value         -- ^ JSON schema for parameters
  -> ToolHandler         -- ^ Handler function
  -> Tool
defineTool name desc params handler = Tool
  { toolName        = name
  , toolDescription = desc
  , toolParameters  = params
  , toolHandler     = handler
  }

-- | Define a simple tool with typed argument parsing.
--
-- The handler receives parsed arguments of type @a@. If parsing fails,
-- the tool automatically returns a failure result.
defineSimpleTool
  :: FromJSON a
  => Text                              -- ^ Tool name
  -> Maybe Text                        -- ^ Tool description
  -> Maybe Value                       -- ^ JSON schema for parameters
  -> (a -> ToolInvocation -> IO Text)  -- ^ Handler that returns a text result
  -> Tool
defineSimpleTool name desc params handler = Tool
  { toolName        = name
  , toolDescription = desc
  , toolParameters  = params
  , toolHandler     = \args inv -> do
      case fromJSON args of
        Success parsed -> do
          resultText <- handler parsed inv
          pure $ ToolResultText resultText
        Error e ->
          pure $ ToolResultStruct $ ToolResultObject
            { troTextResultForLlm    = "Failed to parse tool arguments"
            , troBinaryResultsForLlm = Nothing
            , troResultType          = Failure
            , troError               = Just (T.pack e)
            , troSessionLog          = Nothing
            , troToolTelemetry       = Nothing
            }
  }
