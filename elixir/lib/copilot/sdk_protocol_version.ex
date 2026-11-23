defmodule Copilot.SdkProtocolVersion do
  @moduledoc """
  SDK protocol version constant.

  This must match the version expected by the copilot-agent-runtime server
  (SDK protocol 4 requires Copilot CLI 1.0.113 or newer).
  Code generated from sdk-protocol-version.json. DO NOT EDIT.
  """

  @sdk_protocol_version 4

  @doc """
  Returns the SDK protocol version number.
  """
  @spec get() :: non_neg_integer()
  def get, do: @sdk_protocol_version
end
