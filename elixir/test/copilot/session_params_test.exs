defmodule Copilot.SessionParamsTest do
  use ExUnit.Case, async: true

  alias Copilot.Client
  alias Copilot.Types

  test "groups flat resume params into SDK protocol 4 groups" do
    tool = %{"name" => "t", "idempotent" => true}

    grouped =
      Client.group_session_params(
        %{
          "sessionId" => "s1",
          "model" => "m",
          "name" => "create only",
          "streaming" => true,
          "requestPermission" => true,
          "tools" => [tool],
          "hooks" => true,
          "disableResume" => true,
          "trustWorkingDirectory" => "session",
          "reasoningEffort" => nil,
          "futureOption" => 1
        },
        :resume
      )

    assert grouped == %{
             "sessionId" => "s1",
             "model" => "m",
             "suppressResumeEvent" => true,
             "futureOption" => 1,
             "events" => %{"streaming" => true},
             "permissions" => %{"request" => true},
             "tools" => %{"definitions" => [tool]},
             "hooks" => %{"hasHandlers" => true}
           }
  end

  test "translates the legacy provider unless named providers are explicit" do
    grouped =
      Client.group_session_params(
        %{
          "model" => "gpt-4o",
          "provider" => %{
            "type" => "openai",
            "baseUrl" => "https://models.example.com/v1",
            "apiKeyCommand" => "print-key",
            "maxOutputTokens" => 2048
          }
        },
        :create
      )

    assert grouped["inference"] == %{
             "providers" => [
               %{
                 "name" => "default",
                 "type" => "openai",
                 "baseUrl" => "https://models.example.com/v1",
                 "apiKeyCommand" => "print-key"
               }
             ],
             "defaultProvider" => "default",
             "models" => [%{"provider" => "default", "id" => "gpt-4o", "maxOutputTokens" => 2048}]
           }

    assert grouped["github"] == %{"capi" => %{"enabled" => false}}

    explicit =
      Client.group_session_params(
        %{
          "sessionId" => "s1",
          "provider" => %{"baseUrl" => "https://legacy.example.com"},
          "defaultProvider" => nil
        },
        :resume
      )

    assert explicit == %{"sessionId" => "s1", "inference" => %{"defaultProvider" => nil}}
  end

  test "empty mode defaults never override session values" do
    params =
      %{
        "availableTools" => ["view"],
        "memory" => %{"enabled" => true},
        "mcpServers" => %{"s" => %{"command" => "x"}}
      }
      |> Client.group_session_params(:create)
      |> Client.apply_mode_defaults(:empty)

    assert params["tools"] == %{"availableTools" => ["view"]}
    assert params["state"]["memory"] == %{"enabled" => true}
    assert params["state"]["skipEmbeddingRetrieval"] == true

    assert params["mcp"] == %{
             "servers" => %{"s" => %{"command" => "x"}},
             "envValueMode" => "direct",
             "oauthTokenStorage" => "in-memory"
           }

    assert params["git"] == %{"enableHostGitOperations" => false}
    assert Client.apply_mode_defaults(%{"model" => "m"}, :coding_agent) == %{"model" => "m"}
  end

  test "v3 wire helpers" do
    assert {:ok, :coding_agent} = Types.ClientMode.parse("copilot-cli")
    assert {:ok, :empty} = Types.ClientMode.parse("empty")

    assert %Types.ToolShellOutput{text: "hi", sequence: 3} =
             Types.ToolShellOutput.from_map(%{"toolCallId" => "tc", "text" => "hi", "sequence" => 3})

    assert Types.Tool.to_wire(%Types.Tool{name: "t", idempotent: true}) ==
             %{"name" => "t", "idempotent" => true}

    assert %Types.PolicyHelperFailure{category: "timeout"} =
             Types.PolicyHelperFailure.from_error(
               {:error, %{"data" => %{"policyHelperFailure" => %{"category" => "timeout"}}}}
             )

    assert Types.PolicyHelperFailure.from_error({:error, %{"code" => -32603}}) == nil

    assert Types.NetworkConfig.set_provider_params(%Types.NetworkConfig{
             scope: :runtime,
             resolve_proxy: fn _request -> nil end
           }) == %{"scope" => "runtime", "resolveProxy" => %{}}

    assert Types.ShellAuthenticationRequest.response_to_wire({:accept, "pw"}) ==
             %{"action" => "accept", "value" => "pw"}

    assert Types.ShellAuthenticationRequest.response_to_wire(:cancel) == %{"action" => "cancel"}
  end

  test "v3.1 ask-user image attachment wire helpers" do
    request =
      Types.UserInputRequest.from_map(%{
        "question" => "Attach a screenshot?",
        "requestId" => "req-1",
        "_meta" => %{"ext" => %{"k" => 1}},
        "imageAttachmentConstraints" => %{
          "allowedMimeTypes" => ["image/png"],
          "maxBytesPerImage" => 1024,
          "maxCount" => 2
        }
      })

    assert %Types.UserInputRequest{
             request_id: "req-1",
             meta: %{"ext" => %{"k" => 1}},
             image_attachment_constraints: %Types.UserInputImageAttachmentConstraints{
               allowed_mime_types: ["image/png"],
               max_bytes_per_image: 1024,
               max_count: 2
             }
           } = request

    assert Types.UserInputRequest.from_map(%{"question" => "q"}).image_attachment_constraints ==
             nil

    plain = %Types.UserInputResponse{answer: "ok", was_freeform: false}
    assert Types.UserInputResponse.to_map(plain) == %{"answer" => "ok", "wasFreeform" => false}

    image = %Types.UserInputImageAttachment{data: "aGk=", mime_type: "image/png"}

    assert Types.UserInputResponse.to_map(%Types.UserInputResponse{
             answer: "",
             was_freeform: true,
             attachments: [image]
           }) == %{
             "answer" => "",
             "wasFreeform" => true,
             "attachments" => [%{"type" => "image", "data" => "aGk=", "mimeType" => "image/png"}]
           }
  end
end
