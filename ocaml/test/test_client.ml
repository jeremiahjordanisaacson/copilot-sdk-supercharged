(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Alcotest tests for the Copilot OCaml SDK. *)

open Copilot_sdk

(* ========================================================================== *)
(* Types tests                                                                *)
(* ========================================================================== *)

let test_connection_state_to_string () =
  Alcotest.(check string) "disconnected"
    "disconnected"
    (Types.connection_state_to_string Disconnected);
  Alcotest.(check string) "connecting"
    "connecting"
    (Types.connection_state_to_string Connecting);
  Alcotest.(check string) "connected"
    "connected"
    (Types.connection_state_to_string Connected);
  Alcotest.(check string) "error"
    "error"
    (Types.connection_state_to_string Error)

let test_tool_result_type_roundtrip () =
  let types =
    [ Types.Success; Failure; Rejected; Denied; Timeout ]
  in
  List.iter
    (fun t ->
      let s = Types.tool_result_type_to_string t in
      let t' = Types.tool_result_type_of_string s in
      Alcotest.(check string)
        (Printf.sprintf "roundtrip %s" s)
        s
        (Types.tool_result_type_to_string t'))
    types

let test_reasoning_effort_to_string () =
  Alcotest.(check string) "low" "low"
    (Types.reasoning_effort_to_string Low);
  Alcotest.(check string) "medium" "medium"
    (Types.reasoning_effort_to_string Medium);
  Alcotest.(check string) "high" "high"
    (Types.reasoning_effort_to_string High);
  Alcotest.(check string) "xhigh" "xhigh"
    (Types.reasoning_effort_to_string Xhigh)

let test_default_session_config () =
  let config = Types.default_session_config () in
  Alcotest.(check bool) "streaming default" true config.streaming;
  Alcotest.(check bool) "no model" true (config.model = None);
  Alcotest.(check bool) "no system_prompt" true (config.system_prompt = None);
  Alcotest.(check bool) "empty tools" true (config.tools = [])

let test_make_message () =
  let msg = Types.make_message "Hello" in
  Alcotest.(check string) "prompt" "Hello" msg.prompt;
  Alcotest.(check bool) "no mode" true (msg.mode = None);
  let msg2 = Types.make_message ~mode:"chat" "Hello" in
  Alcotest.(check (option string)) "with mode" (Some "chat") msg2.mode

let test_make_tool_result () =
  let r = Types.make_tool_result "output" in
  Alcotest.(check string) "text_result" "output" r.text_result;
  Alcotest.(check string) "result_type" "success"
    (Types.tool_result_type_to_string r.result_type);
  Alcotest.(check bool) "no error" true (r.error = None)

let test_make_tool_result_with_error () =
  let r = Types.make_tool_result ~result_type:Failure ~error:"boom" "oops" in
  Alcotest.(check string) "result_type" "failure"
    (Types.tool_result_type_to_string r.result_type);
  Alcotest.(check (option string)) "error" (Some "boom") r.error

let test_make_tool_definition () =
  let td = Types.make_tool_definition "my_tool" "A test tool" in
  Alcotest.(check string) "name" "my_tool" td.tool_name;
  Alcotest.(check string) "description" "A test tool" td.tool_description;
  Alcotest.(check bool) "no parameters" true (td.tool_parameters = None)

let test_default_client_options () =
  let opts = Types.default_client_options () in
  Alcotest.(check bool) "no cli_path" true (opts.cli_path = None);
  Alcotest.(check bool) "no cli_url" true (opts.cli_url = None);
  Alcotest.(check bool) "no log_level" true (opts.log_level = None)

(* ========================================================================== *)
(* JSON serialization tests                                                   *)
(* ========================================================================== *)

let test_tool_result_to_yojson () =
  let r = Types.make_tool_result "hello" in
  let json = Types.tool_result_to_yojson r in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "textResultForLlm"
    "hello"
    (json |> member "textResultForLlm" |> to_string);
  Alcotest.(check string) "resultType"
    "success"
    (json |> member "resultType" |> to_string)

let test_message_options_to_yojson () =
  let msg = Types.make_message ~mode:"agent" "test" in
  let json = Types.message_options_to_yojson msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "prompt"
    "test"
    (json |> member "prompt" |> to_string);
  Alcotest.(check string) "mode"
    "agent"
    (json |> member "mode" |> to_string)

let test_tool_definition_to_yojson () =
  let td = Types.make_tool_definition "calc" "Calculator" in
  let json = Types.tool_definition_to_yojson td in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "name"
    "calc"
    (json |> member "name" |> to_string);
  Alcotest.(check string) "description"
    "Calculator"
    (json |> member "description" |> to_string)

let test_session_config_to_yojson () =
  let config =
    { (Types.default_session_config ()) with
      model = Some "gpt-4o"
    ; reasoning_effort = Some High
    }
  in
  let json = Types.session_config_to_yojson config in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model"
    "gpt-4o"
    (json |> member "model" |> to_string);
  Alcotest.(check bool) "streaming"
    true
    (json |> member "streaming" |> to_bool);
  Alcotest.(check string) "reasoningEffort"
    "high"
    (json |> member "reasoningEffort" |> to_string)

let test_permission_result_to_yojson () =
  let r = Types.{ decision = Approved; rules = None } in
  let json = Types.permission_result_to_yojson r in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "kind"
    "approved"
    (json |> member "kind" |> to_string)

(* ========================================================================== *)
(* JSON deserialization tests                                                  *)
(* ========================================================================== *)

let test_tool_invocation_of_yojson () =
  let json =
    `Assoc
      [ ("sessionId", `String "s1")
      ; ("toolCallId", `String "tc1")
      ; ("toolName", `String "calc")
      ; ("arguments", `Assoc [ ("x", `Int 42) ])
      ]
  in
  match Types.tool_invocation_of_yojson json with
  | Ok inv ->
    Alcotest.(check string) "session_id" "s1" inv.session_id;
    Alcotest.(check string) "tool_call_id" "tc1" inv.tool_call_id;
    Alcotest.(check string) "tool_name" "calc" inv.tool_name
  | Error msg -> Alcotest.fail msg

let test_session_event_of_yojson () =
  let json =
    `Assoc
      [ ("type", `String "assistant.message")
      ; ("data", `Assoc [ ("content", `String "Hello!") ])
      ]
  in
  match Types.session_event_of_yojson json with
  | Ok ev ->
    Alcotest.(check bool) "is AssistantMessage"
      true
      (ev.event_type = Types.AssistantMessage)
  | Error msg -> Alcotest.fail msg

let test_session_event_unknown_type () =
  let json =
    `Assoc
      [ ("type", `String "custom.event")
      ; ("data", `Null)
      ]
  in
  match Types.session_event_of_yojson json with
  | Ok ev ->
    (match ev.event_type with
     | Types.Unknown s -> Alcotest.(check string) "unknown type" "custom.event" s
     | _ -> Alcotest.fail "Expected Unknown variant")
  | Error msg -> Alcotest.fail msg

let test_get_status_response_of_yojson () =
  let json =
    `Assoc
      [ ("protocolVersion", `Int 3)
      ; ("serverVersion", `String "1.2.3")
      ]
  in
  match Types.get_status_response_of_yojson json with
  | Ok resp ->
    Alcotest.(check int) "protocol_version" 3 resp.protocol_version;
    Alcotest.(check (option string)) "server_version"
      (Some "1.2.3") resp.server_version
  | Error msg -> Alcotest.fail msg

(* ========================================================================== *)
(* Version tests                                                              *)
(* ========================================================================== *)

let test_version_constants () =
  Alcotest.(check int) "sdk_protocol_version" 3 Version.sdk_protocol_version;
  Alcotest.(check int) "min_protocol_version" 2 Version.min_protocol_version;
  Alcotest.(check string) "sdk_name"
    "copilot-sdk-supercharged-ocaml" Version.sdk_name

(* ========================================================================== *)
(* Tools helper tests                                                         *)
(* ========================================================================== *)

let test_make_string_param () =
  let name, json = Tools.make_string_param ~name:"city" ~description:"City name" () in
  Alcotest.(check string) "param name" "city" name;
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type"
    "string"
    (json |> member "type" |> to_string)

let test_make_object_schema () =
  let props = [ Tools.make_string_param ~name:"x" ~description:"X" () ] in
  let schema = Tools.make_object_schema ~properties:props ~required:["x"] () in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type"
    "object"
    (schema |> member "type" |> to_string);
  let required = schema |> member "required" |> to_list |> List.map to_string in
  Alcotest.(check (list string)) "required" ["x"] required

(* ========================================================================== *)
(* Client construction tests                                                  *)
(* ========================================================================== *)

let test_client_initial_state () =
  let client = Client.create () in
  Alcotest.(check string) "initial state"
    "disconnected"
    (Types.connection_state_to_string (Client.connection_state client))

let test_client_with_options () =
  let opts =
    { (Types.default_client_options ()) with
      cli_path = Some "/usr/bin/copilot-cli"
    ; log_level = Some "debug"
    }
  in
  let client = Client.create ~options:opts () in
  Alcotest.(check string) "still disconnected"
    "disconnected"
    (Types.connection_state_to_string (Client.connection_state client))

(* ========================================================================== *)
(* JSON-RPC unit tests                                                        *)
(* ========================================================================== *)

let test_jsonrpc_create () =
  let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.create 0) in
  let oc = Lwt_io.of_bytes ~mode:Lwt_io.Output (Lwt_bytes.create 1024) in
  let rpc = Jsonrpc.create ic oc in
  Alcotest.(check bool) "not running initially" false (Jsonrpc.is_running rpc)

(* ========================================================================== *)
(* Test runner                                                                *)
(* ========================================================================== *)

let () =
  Alcotest.run "copilot-sdk-ocaml"
    [ ( "types"
      , [ Alcotest.test_case "connection_state_to_string" `Quick
            test_connection_state_to_string
        ; Alcotest.test_case "tool_result_type_roundtrip" `Quick
            test_tool_result_type_roundtrip
        ; Alcotest.test_case "reasoning_effort_to_string" `Quick
            test_reasoning_effort_to_string
        ; Alcotest.test_case "default_session_config" `Quick
            test_default_session_config
        ; Alcotest.test_case "make_message" `Quick test_make_message
        ; Alcotest.test_case "make_tool_result" `Quick test_make_tool_result
        ; Alcotest.test_case "make_tool_result_with_error" `Quick
            test_make_tool_result_with_error
        ; Alcotest.test_case "make_tool_definition" `Quick test_make_tool_definition
        ; Alcotest.test_case "default_client_options" `Quick
            test_default_client_options
        ] )
    ; ( "json_serialization"
      , [ Alcotest.test_case "tool_result_to_yojson" `Quick
            test_tool_result_to_yojson
        ; Alcotest.test_case "message_options_to_yojson" `Quick
            test_message_options_to_yojson
        ; Alcotest.test_case "tool_definition_to_yojson" `Quick
            test_tool_definition_to_yojson
        ; Alcotest.test_case "session_config_to_yojson" `Quick
            test_session_config_to_yojson
        ; Alcotest.test_case "permission_result_to_yojson" `Quick
            test_permission_result_to_yojson
        ] )
    ; ( "json_deserialization"
      , [ Alcotest.test_case "tool_invocation_of_yojson" `Quick
            test_tool_invocation_of_yojson
        ; Alcotest.test_case "session_event_of_yojson" `Quick
            test_session_event_of_yojson
        ; Alcotest.test_case "session_event_unknown_type" `Quick
            test_session_event_unknown_type
        ; Alcotest.test_case "get_status_response_of_yojson" `Quick
            test_get_status_response_of_yojson
        ] )
    ; ( "version"
      , [ Alcotest.test_case "version_constants" `Quick test_version_constants
        ] )
    ; ( "tools"
      , [ Alcotest.test_case "make_string_param" `Quick test_make_string_param
        ; Alcotest.test_case "make_object_schema" `Quick test_make_object_schema
        ] )
    ; ( "client"
      , [ Alcotest.test_case "initial_state" `Quick test_client_initial_state
        ; Alcotest.test_case "with_options" `Quick test_client_with_options
        ] )
    ; ( "jsonrpc"
      , [ Alcotest.test_case "create" `Quick test_jsonrpc_create
        ] )
    ]
