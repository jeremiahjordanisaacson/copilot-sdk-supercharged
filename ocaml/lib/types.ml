(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Core types for the Copilot SDK. *)

open Yojson.Safe.Util

(* ========================================================================== *)
(* Connection State                                                           *)
(* ========================================================================== *)

type connection_state =
  | Disconnected
  | Connecting
  | Connected
  | Error

let connection_state_to_string = function
  | Disconnected -> "disconnected"
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Error -> "error"

(* ========================================================================== *)
(* Tool Types                                                                 *)
(* ========================================================================== *)

type tool_result_type =
  | Success
  | Failure
  | Rejected
  | Denied
  | Timeout

let tool_result_type_to_string = function
  | Success -> "success"
  | Failure -> "failure"
  | Rejected -> "rejected"
  | Denied -> "denied"
  | Timeout -> "timeout"

let tool_result_type_of_string = function
  | "success" -> Success
  | "failure" -> Failure
  | "rejected" -> Rejected
  | "denied" -> Denied
  | "timeout" -> Timeout
  | _ -> Failure

type tool_result = {
  text_result : string;
  result_type : tool_result_type;
  error : string option;
  session_log : string option;
}

let make_tool_result ?(result_type = Success) ?error ?session_log text =
  { text_result = text; result_type; error; session_log }

let tool_result_to_yojson (r : tool_result) : Yojson.Safe.t =
  let fields =
    [ ("textResultForLlm", `String r.text_result)
    ; ("resultType", `String (tool_result_type_to_string r.result_type))
    ]
  in
  let fields =
    match r.error with
    | Some e -> ("error", `String e) :: fields
    | None -> fields
  in
  let fields =
    match r.session_log with
    | Some s -> ("sessionLog", `String s) :: fields
    | None -> fields
  in
  `Assoc fields

type tool_invocation = {
  session_id : string;
  tool_call_id : string;
  tool_name : string;
  arguments : Yojson.Safe.t;
}

let tool_invocation_of_yojson (json : Yojson.Safe.t)
    : (tool_invocation, string) result =
  try
    Ok
      { session_id = json |> member "sessionId" |> to_string
      ; tool_call_id = json |> member "toolCallId" |> to_string
      ; tool_name = json |> member "toolName" |> to_string
      ; arguments =
          (try json |> member "arguments" with _ -> `Null)
      }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Session Filesystem Configuration                                           *)
(* ========================================================================== *)

type session_fs_config = {
  initial_cwd : string;
  session_state_path : string;
  conventions : string;  (** "windows" or "posix" *)
}

let default_session_fs_config () =
  { initial_cwd = ""; session_state_path = ""; conventions = "posix" }

let session_fs_config_to_yojson (c : session_fs_config) : Yojson.Safe.t =
  `Assoc
    [ ("initialCwd", `String c.initial_cwd)
    ; ("sessionStatePath", `String c.session_state_path)
    ; ("conventions", `String c.conventions)
    ]

(* ========================================================================== *)
(* MCP Server Configuration                                                   *)
(* ========================================================================== *)

type mcp_server_type = Stdio | Http

let mcp_server_type_to_string = function
  | Stdio -> "stdio"
  | Http -> "http"

type mcp_server_config = {
  mcp_type : mcp_server_type;
  command : string option;
  args : string list;
  url : string option;
  env : (string * string) list;
  headers : (string * string) list;
}

let mcp_server_config_to_yojson (c : mcp_server_config) : Yojson.Safe.t =
  let fields =
    [ ("type", `String (mcp_server_type_to_string c.mcp_type)) ]
  in
  let fields =
    match c.command with
    | Some cmd -> ("command", `String cmd) :: fields
    | None -> fields
  in
  let fields =
    match c.args with
    | [] -> fields
    | args -> ("args", `List (List.map (fun s -> `String s) args)) :: fields
  in
  let fields =
    match c.url with
    | Some u -> ("url", `String u) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Command Definition                                                         *)
(* ========================================================================== *)

type command_definition = {
  cmd_name : string;
  cmd_description : string;
}

let command_definition_to_yojson (c : command_definition) : Yojson.Safe.t =
  `Assoc
    [ ("name", `String c.cmd_name)
    ; ("description", `String c.cmd_description)
    ]

(* ========================================================================== *)
(* Canvas / Cloud Session Types                                               *)
(* ========================================================================== *)

type canvas_action = {
  canvas_action_name : string;
  canvas_action_description : string;
  canvas_action_input_schema : Yojson.Safe.t option;
}

let canvas_action_of_yojson (json : Yojson.Safe.t)
    : (canvas_action, string) result =
  try
    Ok
      { canvas_action_name = json |> member "name" |> to_string
      ; canvas_action_description = json |> member "description" |> to_string
      ; canvas_action_input_schema =
          (match json |> member "inputSchema" with
           | `Null -> None
           | v -> Some v)
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_action_to_yojson (a : canvas_action) : Yojson.Safe.t =
  let fields =
    [ ("name", `String a.canvas_action_name)
    ; ("description", `String a.canvas_action_description)
    ]
  in
  let fields =
    match a.canvas_action_input_schema with
    | Some schema -> ("inputSchema", schema) :: fields
    | None -> fields
  in
  `Assoc fields

type canvas_declaration = {
  canvas_declaration_id : string;
  canvas_declaration_display_name : string;
  canvas_declaration_description : string;
  canvas_declaration_input_schema : Yojson.Safe.t option;
  canvas_declaration_actions : canvas_action list option;
}

let canvas_declaration_of_yojson (json : Yojson.Safe.t)
    : (canvas_declaration, string) result =
  try
    Ok
      { canvas_declaration_id = json |> member "id" |> to_string
      ; canvas_declaration_display_name = json |> member "displayName" |> to_string
      ; canvas_declaration_description = json |> member "description" |> to_string
      ; canvas_declaration_input_schema =
          (match json |> member "inputSchema" with
           | `Null -> None
           | v -> Some v)
      ; canvas_declaration_actions =
          (match json |> member "actions" with
           | `Null -> None
           | `List xs ->
             Some
               (List.filter_map
                  (fun v -> match canvas_action_of_yojson v with Ok a -> Some a | Error _ -> None)
                  xs)
           | _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_declaration_to_yojson (c : canvas_declaration) : Yojson.Safe.t =
  let fields =
    [ ("id", `String c.canvas_declaration_id)
    ; ("displayName", `String c.canvas_declaration_display_name)
    ; ("description", `String c.canvas_declaration_description)
    ]
  in
  let fields =
    match c.canvas_declaration_input_schema with
    | Some schema -> ("inputSchema", schema) :: fields
    | None -> fields
  in
  let fields =
    match c.canvas_declaration_actions with
    | Some actions ->
      ("actions", `List (List.map canvas_action_to_yojson actions)) :: fields
    | None -> fields
  in
  `Assoc fields

type canvas_open_response = {
  canvas_open_response_url : string option;
  canvas_open_response_title : string option;
  canvas_open_response_status : string option;
}

let canvas_open_response_of_yojson (json : Yojson.Safe.t)
    : (canvas_open_response, string) result =
  try
    Ok
      { canvas_open_response_url =
          (match json |> member "url" with `Null -> None | v -> Some (to_string v))
      ; canvas_open_response_title =
          (match json |> member "title" with `Null -> None | v -> Some (to_string v))
      ; canvas_open_response_status =
          (match json |> member "status" with `Null -> None | v -> Some (to_string v))
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_open_response_to_yojson (r : canvas_open_response) : Yojson.Safe.t =
  let fields = [] in
  let fields = match r.canvas_open_response_url with Some v -> ("url", `String v) :: fields | None -> fields in
  let fields = match r.canvas_open_response_title with Some v -> ("title", `String v) :: fields | None -> fields in
  let fields = match r.canvas_open_response_status with Some v -> ("status", `String v) :: fields | None -> fields in
  `Assoc fields

type canvas_host_capabilities = {
  canvas_host_capabilities_canvases : bool;
}

let canvas_host_capabilities_of_yojson (json : Yojson.Safe.t)
    : (canvas_host_capabilities, string) result =
  try
    Ok
      { canvas_host_capabilities_canvases =
          (try json |> member "canvases" |> to_bool with _ -> false)
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_host_capabilities_to_yojson (c : canvas_host_capabilities) : Yojson.Safe.t =
  `Assoc [ ("canvases", `Bool c.canvas_host_capabilities_canvases) ]

type canvas_host_context = {
  canvas_host_context_capabilities : canvas_host_capabilities;
}

let canvas_host_context_of_yojson (json : Yojson.Safe.t)
    : (canvas_host_context, string) result =
  try
    match canvas_host_capabilities_of_yojson (json |> member "capabilities") with
    | Ok capabilities -> Ok { canvas_host_context_capabilities = capabilities }
    | Error err -> Error err
  with exn -> Error (Printexc.to_string exn)

let canvas_host_context_to_yojson (c : canvas_host_context) : Yojson.Safe.t =
  `Assoc [ ("capabilities", canvas_host_capabilities_to_yojson c.canvas_host_context_capabilities) ]

type canvas_open_context = {
  canvas_open_context_session_id : string;
  canvas_open_context_extension_id : string;
  canvas_open_context_canvas_id : string;
  canvas_open_context_instance_id : string;
  canvas_open_context_input : Yojson.Safe.t;
  canvas_open_context_host : canvas_host_context option;
}

let canvas_open_context_of_yojson (json : Yojson.Safe.t)
    : (canvas_open_context, string) result =
  try
    Ok
      { canvas_open_context_session_id = json |> member "sessionId" |> to_string
      ; canvas_open_context_extension_id = json |> member "extensionId" |> to_string
      ; canvas_open_context_canvas_id = json |> member "canvasId" |> to_string
      ; canvas_open_context_instance_id = json |> member "instanceId" |> to_string
      ; canvas_open_context_input = json |> member "input"
      ; canvas_open_context_host =
          (match json |> member "host" with
           | `Null -> None
           | v ->
             (match canvas_host_context_of_yojson v with
              | Ok host -> Some host
              | Error _ -> None))
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_open_context_to_yojson (c : canvas_open_context) : Yojson.Safe.t =
  let fields =
    [ ("sessionId", `String c.canvas_open_context_session_id)
    ; ("extensionId", `String c.canvas_open_context_extension_id)
    ; ("canvasId", `String c.canvas_open_context_canvas_id)
    ; ("instanceId", `String c.canvas_open_context_instance_id)
    ; ("input", c.canvas_open_context_input)
    ]
  in
  let fields =
    match c.canvas_open_context_host with
    | Some host -> ("host", canvas_host_context_to_yojson host) :: fields
    | None -> fields
  in
  `Assoc fields

type canvas_action_context = {
  canvas_action_context_session_id : string;
  canvas_action_context_extension_id : string;
  canvas_action_context_canvas_id : string;
  canvas_action_context_instance_id : string;
  canvas_action_context_action_name : string;
  canvas_action_context_input : Yojson.Safe.t;
  canvas_action_context_host : canvas_host_context option;
}

let canvas_action_context_of_yojson (json : Yojson.Safe.t)
    : (canvas_action_context, string) result =
  try
    Ok
      { canvas_action_context_session_id = json |> member "sessionId" |> to_string
      ; canvas_action_context_extension_id = json |> member "extensionId" |> to_string
      ; canvas_action_context_canvas_id = json |> member "canvasId" |> to_string
      ; canvas_action_context_instance_id = json |> member "instanceId" |> to_string
      ; canvas_action_context_action_name = json |> member "actionName" |> to_string
      ; canvas_action_context_input = json |> member "input"
      ; canvas_action_context_host =
          (match json |> member "host" with
           | `Null -> None
           | v ->
             (match canvas_host_context_of_yojson v with
              | Ok host -> Some host
              | Error _ -> None))
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_action_context_to_yojson (c : canvas_action_context) : Yojson.Safe.t =
  let fields =
    [ ("sessionId", `String c.canvas_action_context_session_id)
    ; ("extensionId", `String c.canvas_action_context_extension_id)
    ; ("canvasId", `String c.canvas_action_context_canvas_id)
    ; ("instanceId", `String c.canvas_action_context_instance_id)
    ; ("actionName", `String c.canvas_action_context_action_name)
    ; ("input", c.canvas_action_context_input)
    ]
  in
  let fields =
    match c.canvas_action_context_host with
    | Some host -> ("host", canvas_host_context_to_yojson host) :: fields
    | None -> fields
  in
  `Assoc fields

type canvas_lifecycle_context = {
  canvas_lifecycle_context_session_id : string;
  canvas_lifecycle_context_extension_id : string;
  canvas_lifecycle_context_canvas_id : string;
  canvas_lifecycle_context_instance_id : string;
  canvas_lifecycle_context_host : canvas_host_context option;
}

let canvas_lifecycle_context_of_yojson (json : Yojson.Safe.t)
    : (canvas_lifecycle_context, string) result =
  try
    Ok
      { canvas_lifecycle_context_session_id = json |> member "sessionId" |> to_string
      ; canvas_lifecycle_context_extension_id = json |> member "extensionId" |> to_string
      ; canvas_lifecycle_context_canvas_id = json |> member "canvasId" |> to_string
      ; canvas_lifecycle_context_instance_id = json |> member "instanceId" |> to_string
      ; canvas_lifecycle_context_host =
          (match json |> member "host" with
           | `Null -> None
           | v ->
             (match canvas_host_context_of_yojson v with
              | Ok host -> Some host
              | Error _ -> None))
      }
  with exn -> Error (Printexc.to_string exn)

let canvas_lifecycle_context_to_yojson (c : canvas_lifecycle_context) : Yojson.Safe.t =
  let fields =
    [ ("sessionId", `String c.canvas_lifecycle_context_session_id)
    ; ("extensionId", `String c.canvas_lifecycle_context_extension_id)
    ; ("canvasId", `String c.canvas_lifecycle_context_canvas_id)
    ; ("instanceId", `String c.canvas_lifecycle_context_instance_id)
    ]
  in
  let fields =
    match c.canvas_lifecycle_context_host with
    | Some host -> ("host", canvas_host_context_to_yojson host) :: fields
    | None -> fields
  in
  `Assoc fields

type cloud_session_repository = {
  cloud_session_repository_owner : string;
  cloud_session_repository_name : string;
  cloud_session_repository_branch : string option;
}

let cloud_session_repository_of_yojson (json : Yojson.Safe.t)
    : (cloud_session_repository, string) result =
  try
    Ok
      { cloud_session_repository_owner = json |> member "owner" |> to_string
      ; cloud_session_repository_name = json |> member "name" |> to_string
      ; cloud_session_repository_branch =
          (match json |> member "branch" with `Null -> None | v -> Some (to_string v))
      }
  with exn -> Error (Printexc.to_string exn)

let cloud_session_repository_to_yojson (r : cloud_session_repository) : Yojson.Safe.t =
  let fields =
    [ ("owner", `String r.cloud_session_repository_owner)
    ; ("name", `String r.cloud_session_repository_name)
    ]
  in
  let fields =
    match r.cloud_session_repository_branch with
    | Some branch -> ("branch", `String branch) :: fields
    | None -> fields
  in
  `Assoc fields

type cloud_session_options = {
  cloud_session_options_repository : cloud_session_repository option;
}

let cloud_session_options_of_yojson (json : Yojson.Safe.t)
    : (cloud_session_options, string) result =
  try
    Ok
      { cloud_session_options_repository =
          (match json |> member "repository" with
           | `Null -> None
           | v ->
             (match cloud_session_repository_of_yojson v with
              | Ok repo -> Some repo
              | Error _ -> None))
      }
  with exn -> Error (Printexc.to_string exn)

let cloud_session_options_to_yojson (o : cloud_session_options) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match o.cloud_session_options_repository with
    | Some repo -> ("repository", cloud_session_repository_to_yojson repo) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* System Message / User Input / Image Types                                  *)
(* ========================================================================== *)

type section_override_action = SOAReplace | SOARemove | SOAAppend | SOAPrepend | SOATransform

let section_override_action_to_string = function
  | SOAReplace -> "replace"
  | SOARemove -> "remove"
  | SOAAppend -> "append"
  | SOAPrepend -> "prepend"
  | SOATransform -> "transform"

let section_override_action_of_string = function
  | "replace" -> SOAReplace
  | "remove" -> SOARemove
  | "append" -> SOAAppend
  | "prepend" -> SOAPrepend
  | "transform" -> SOATransform
  | _ -> SOAReplace

type section_override = {
  section_override_action : section_override_action;
  section_override_content : string option;
}

let section_override_of_yojson (json : Yojson.Safe.t)
    : (section_override, string) result =
  try
    Ok
      { section_override_action =
          section_override_action_of_string (json |> member "action" |> to_string)
      ; section_override_content =
          (match json |> member "content" with `Null -> None | v -> Some (to_string v))
      }
  with exn -> Error (Printexc.to_string exn)

let section_override_to_yojson (o : section_override) : Yojson.Safe.t =
  let fields = [ ("action", `String (section_override_action_to_string o.section_override_action)) ] in
  let fields =
    match o.section_override_content with
    | Some content -> ("content", `String content) :: fields
    | None -> fields
  in
  `Assoc fields

type system_message_append_config = {
  system_message_append_mode : string option;
  system_message_append_content : string option;
}

let system_message_append_config_to_yojson (c : system_message_append_config) : Yojson.Safe.t =
  let fields = [] in
  let fields = match c.system_message_append_mode with Some v -> ("mode", `String v) :: fields | None -> fields in
  let fields = match c.system_message_append_content with Some v -> ("content", `String v) :: fields | None -> fields in
  `Assoc fields

type system_message_replace_config = {
  system_message_replace_mode : string;
  system_message_replace_content : string;
}

let system_message_replace_config_to_yojson (c : system_message_replace_config) : Yojson.Safe.t =
  `Assoc [ ("mode", `String c.system_message_replace_mode); ("content", `String c.system_message_replace_content) ]

type system_message_customize_config = {
  system_message_customize_mode : string;
  system_message_customize_sections : (string * section_override) list option;
  system_message_customize_content : string option;
}

let system_message_customize_config_to_yojson (c : system_message_customize_config) : Yojson.Safe.t =
  let fields = [ ("mode", `String c.system_message_customize_mode) ] in
  let fields =
    match c.system_message_customize_sections with
    | Some sections ->
      ("sections", `Assoc (List.map (fun (k, v) -> (k, section_override_to_yojson v)) sections)) :: fields
    | None -> fields
  in
  let fields =
    match c.system_message_customize_content with
    | Some content -> ("content", `String content) :: fields
    | None -> fields
  in
  `Assoc fields

type user_input_request = {
  user_input_question : string option;
  user_input_choices : string list option;
  user_input_allow_freeform : bool option;
}

let user_input_request_of_yojson (json : Yojson.Safe.t)
    : (user_input_request, string) result =
  try
    Ok
      { user_input_question =
          (match json |> member "question" with `Null -> None | v -> Some (to_string v))
      ; user_input_choices =
          (match json |> member "choices" with
           | `Null -> None
           | `List xs -> Some (List.map to_string xs)
           | _ -> None)
      ; user_input_allow_freeform =
          (match json |> member "allowFreeform" with `Null -> None | v -> Some (to_bool v))
      }
  with exn -> Error (Printexc.to_string exn)

let user_input_request_to_yojson (r : user_input_request) : Yojson.Safe.t =
  let fields = [] in
  let fields = match r.user_input_question with Some v -> ("question", `String v) :: fields | None -> fields in
  let fields = match r.user_input_choices with Some xs -> ("choices", `List (List.map (fun s -> `String s) xs)) :: fields | None -> fields in
  let fields = match r.user_input_allow_freeform with Some v -> ("allowFreeform", `Bool v) :: fields | None -> fields in
  `Assoc fields

type user_input_response = {
  user_input_answer : string;
  user_input_was_freeform : bool;
}

let user_input_response_of_yojson (json : Yojson.Safe.t)
    : (user_input_response, string) result =
  try
    Ok
      { user_input_answer = json |> member "answer" |> to_string
      ; user_input_was_freeform = json |> member "wasFreeform" |> to_bool
      }
  with exn -> Error (Printexc.to_string exn)

let user_input_response_to_yojson (r : user_input_response) : Yojson.Safe.t =
  `Assoc
    [ ("answer", `String r.user_input_answer)
    ; ("wasFreeform", `Bool r.user_input_was_freeform)
    ]

type image_options = {
  image_options_size : string option;
  image_options_quality : string option;
  image_options_style : string option;
}

let image_options_of_yojson (json : Yojson.Safe.t)
    : (image_options, string) result =
  try
    Ok
      { image_options_size =
          (match json |> member "size" with `Null -> None | v -> Some (to_string v))
      ; image_options_quality =
          (match json |> member "quality" with `Null -> None | v -> Some (to_string v))
      ; image_options_style =
          (match json |> member "style" with `Null -> None | v -> Some (to_string v))
      }
  with exn -> Error (Printexc.to_string exn)

let image_options_to_yojson (o : image_options) : Yojson.Safe.t =
  let fields = [] in
  let fields = match o.image_options_size with Some v -> ("size", `String v) :: fields | None -> fields in
  let fields = match o.image_options_quality with Some v -> ("quality", `String v) :: fields | None -> fields in
  let fields = match o.image_options_style with Some v -> ("style", `String v) :: fields | None -> fields in
  `Assoc fields

(* ========================================================================== *)
(* Image Response Format                                                      *)
(* ========================================================================== *)

type image_response_format = FormatText | FormatImage | FormatJsonObject

let image_response_format_to_string = function
  | FormatText -> "text"
  | FormatImage -> "image"
  | FormatJsonObject -> "json_object"

(* ========================================================================== *)
(* Session Configuration                                                      *)
(* ========================================================================== *)

type reasoning_effort =
  | Low
  | Medium
  | High
  | Xhigh

let reasoning_effort_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Xhigh -> "xhigh"

type message_options = {
  prompt : string;
  mode : string option;
}

let message_options_to_yojson (m : message_options) : Yojson.Safe.t =
  let fields = [ ("prompt", `String m.prompt) ] in
  let fields =
    match m.mode with
    | Some v -> ("mode", `String v) :: fields
    | None -> fields
  in
  `Assoc fields

type tool_definition = {
  tool_name : string;
  tool_description : string;
  tool_parameters : Yojson.Safe.t option;
}

let tool_definition_to_yojson (td : tool_definition) : Yojson.Safe.t =
  let fields =
    [ ("name", `String td.tool_name)
    ; ("description", `String td.tool_description)
    ]
  in
  let fields =
    match td.tool_parameters with
    | Some p -> ("parameters", p) :: fields
    | None -> fields
  in
  `Assoc fields

type session_config = {
  model : string option;
  system_prompt : string option;
  reasoning_effort : reasoning_effort option;
  streaming : bool;
  tools : tool_definition list;
  excluded_tools : string list;
  mcp_servers : (string * mcp_server_config) list;
  model_capabilities : (string * Yojson.Safe.t) list;
  enable_config_discovery : bool;
  include_sub_agent_streaming_events : bool;
  commands : command_definition list;
  skill_directories : string list;
  disabled_skills : string list;
  working_directory : string option;
  github_token : string option;
  response_format : image_response_format option;
  request_headers : (string * string) list;
  on_elicitation_request : bool;
  instruction_directories : string list;
  enable_session_telemetry : bool;
}

let default_session_config () =
  { model = None
  ; system_prompt = None
  ; reasoning_effort = None
  ; streaming = true
  ; tools = []
  ; excluded_tools = []
  ; mcp_servers = []
  ; model_capabilities = []
  ; enable_config_discovery = false
  ; include_sub_agent_streaming_events = false
  ; commands = []
  ; skill_directories = []
  ; disabled_skills = []
  ; working_directory = None
  ; github_token = None
  ; response_format = None
  ; request_headers = []
  ; on_elicitation_request = false
  ; instruction_directories = []
  ; enable_session_telemetry = false
  }

let session_config_to_yojson (c : session_config) : Yojson.Safe.t =
  let fields = [ ("streaming", `Bool c.streaming) ] in
  let fields =
    match c.model with
    | Some m -> ("model", `String m) :: fields
    | None -> fields
  in
  let fields =
    match c.system_prompt with
    | Some p -> ("systemPrompt", `String p) :: fields
    | None -> fields
  in
  let fields =
    match c.reasoning_effort with
    | Some e ->
      ("reasoningEffort", `String (reasoning_effort_to_string e)) :: fields
    | None -> fields
  in
  let fields =
    match c.tools with
    | [] -> fields
    | ts ->
      let tools_json = `List (List.map tool_definition_to_yojson ts) in
      ("tools", tools_json) :: fields
  in
  let fields =
    match c.excluded_tools with
    | [] -> fields
    | et -> ("excludedTools", `List (List.map (fun s -> `String s) et)) :: fields
  in
  let fields =
    match c.mcp_servers with
    | [] -> fields
    | servers ->
      let assocs = List.map (fun (name, cfg) -> (name, mcp_server_config_to_yojson cfg)) servers in
      ("mcpServers", `Assoc assocs) :: fields
  in
  let fields =
    match c.model_capabilities with
    | [] -> fields
    | caps -> ("modelCapabilities", `Assoc caps) :: fields
  in
  let fields =
    if c.enable_config_discovery then
      ("enableConfigDiscovery", `Bool true) :: fields
    else fields
  in
  let fields =
    if c.include_sub_agent_streaming_events then
      ("includeSubAgentStreamingEvents", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.commands with
    | [] -> fields
    | cmds ->
      ("commands", `List (List.map command_definition_to_yojson cmds)) :: fields
  in
  let fields =
    match c.skill_directories with
    | [] -> fields
    | dirs -> ("skillDirectories", `List (List.map (fun s -> `String s) dirs)) :: fields
  in
  let fields =
    match c.disabled_skills with
    | [] -> fields
    | sk -> ("disabledSkills", `List (List.map (fun s -> `String s) sk)) :: fields
  in
  let fields =
    match c.working_directory with
    | Some d -> ("workingDirectory", `String d) :: fields
    | None -> fields
  in
  let fields =
    match c.github_token with
    | Some t -> ("gitHubToken", `String t) :: fields
    | None -> fields
  in
  let fields =
    match c.response_format with
    | Some f -> ("responseFormat", `String (image_response_format_to_string f)) :: fields
    | None -> fields
  in
  let fields =
    match c.request_headers with
    | [] -> fields
    | hdrs ->
      ("requestHeaders",
       `Assoc (List.map (fun (k, v) -> (k, `String v)) hdrs))
      :: fields
  in
  let fields =
    if c.on_elicitation_request then
      ("requestElicitation", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.instruction_directories with
    | [] -> fields
    | dirs -> ("instructionDirectories", `List (List.map (fun s -> `String s) dirs)) :: fields
  in
  let fields =
    if c.enable_session_telemetry then
      ("enableSessionTelemetry", `Bool true) :: fields
    else fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Slash Command Types                                                        *)
(* ========================================================================== *)

type slash_command_input_completion = SCICDirectory

let slash_command_input_completion_to_string = function
  | SCICDirectory -> "directory"

let slash_command_input_completion_of_string = function
  | "directory" -> SCICDirectory
  | _ -> SCICDirectory

type slash_command_kind = SCKBuiltin | SCKClient | SCKSkill

let slash_command_kind_to_string = function
  | SCKBuiltin -> "builtin"
  | SCKClient -> "client"
  | SCKSkill -> "skill"

let slash_command_kind_of_string = function
  | "builtin" -> SCKBuiltin
  | "client" -> SCKClient
  | "skill" -> SCKSkill
  | _ -> SCKBuiltin

type model_picker_price_category = MPPCHigh | MPPCLow | MPPCMedium | MPPCVeryHigh

let model_picker_price_category_to_string = function
  | MPPCHigh -> "high"
  | MPPCLow -> "low"
  | MPPCMedium -> "medium"
  | MPPCVeryHigh -> "very_high"

let model_picker_price_category_of_string = function
  | "high" -> MPPCHigh
  | "low" -> MPPCLow
  | "medium" -> MPPCMedium
  | "very_high" -> MPPCVeryHigh
  | _ -> MPPCMedium

type slash_command_input = {
  sci_hint : string;
  sci_completion : slash_command_input_completion option;
}

let slash_command_input_of_yojson (json : Yojson.Safe.t)
    : (slash_command_input, string) result =
  try
    Ok
      { sci_hint = json |> member "hint" |> to_string
      ; sci_completion =
          (try Some (slash_command_input_completion_of_string
                       (json |> member "completion" |> to_string))
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let slash_command_input_to_yojson (s : slash_command_input) : Yojson.Safe.t =
  let fields = [ ("hint", `String s.sci_hint) ] in
  let fields =
    match s.sci_completion with
    | Some c -> ("completion", `String (slash_command_input_completion_to_string c)) :: fields
    | None -> fields
  in
  `Assoc fields

type slash_command_info = {
  sc_allow_during_agent_execution : bool;
  sc_description : string;
  sc_kind : slash_command_kind;
  sc_name : string;
  sc_aliases : string list option;
  sc_experimental : bool option;
  sc_input : slash_command_input option;
}

let slash_command_info_of_yojson (json : Yojson.Safe.t)
    : (slash_command_info, string) result =
  try
    Ok
      { sc_allow_during_agent_execution =
          json |> member "allowDuringAgentExecution" |> to_bool
      ; sc_description = json |> member "description" |> to_string
      ; sc_kind =
          slash_command_kind_of_string (json |> member "kind" |> to_string)
      ; sc_name = json |> member "name" |> to_string
      ; sc_aliases =
          (try Some (json |> member "aliases" |> to_list |> List.map to_string)
           with _ -> None)
      ; sc_experimental =
          (try Some (json |> member "experimental" |> to_bool)
           with _ -> None)
      ; sc_input =
          (try
             match slash_command_input_of_yojson (json |> member "input") with
             | Ok v -> Some v
             | Error _ -> None
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let slash_command_info_to_yojson (s : slash_command_info) : Yojson.Safe.t =
  let fields =
    [ ("allowDuringAgentExecution", `Bool s.sc_allow_during_agent_execution)
    ; ("description", `String s.sc_description)
    ; ("kind", `String (slash_command_kind_to_string s.sc_kind))
    ; ("name", `String s.sc_name)
    ]
  in
  let fields =
    match s.sc_aliases with
    | Some a -> ("aliases", `List (List.map (fun s -> `String s) a)) :: fields
    | None -> fields
  in
  let fields =
    match s.sc_experimental with
    | Some e -> ("experimental", `Bool e) :: fields
    | None -> fields
  in
  let fields =
    match s.sc_input with
    | Some i -> ("input", slash_command_input_to_yojson i) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Command Request Types                                                      *)
(* ========================================================================== *)

type commands_invoke_request = {
  cir_name : string;
  cir_input : string option;
}

let commands_invoke_request_of_yojson (json : Yojson.Safe.t)
    : (commands_invoke_request, string) result =
  try
    Ok
      { cir_name = json |> member "name" |> to_string
      ; cir_input =
          (try Some (json |> member "input" |> to_string) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let commands_invoke_request_to_yojson (r : commands_invoke_request) : Yojson.Safe.t =
  let fields = [ ("name", `String r.cir_name) ] in
  let fields =
    match r.cir_input with
    | Some i -> ("input", `String i) :: fields
    | None -> fields
  in
  `Assoc fields

type commands_list_request = {
  clr_include_builtins : bool option;
  clr_include_client_commands : bool option;
  clr_include_skills : bool option;
}

let commands_list_request_of_yojson (json : Yojson.Safe.t)
    : (commands_list_request, string) result =
  try
    Ok
      { clr_include_builtins =
          (try Some (json |> member "includeBuiltins" |> to_bool) with _ -> None)
      ; clr_include_client_commands =
          (try Some (json |> member "includeClientCommands" |> to_bool) with _ -> None)
      ; clr_include_skills =
          (try Some (json |> member "includeSkills" |> to_bool) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let commands_list_request_to_yojson (r : commands_list_request) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match r.clr_include_builtins with
    | Some b -> ("includeBuiltins", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match r.clr_include_client_commands with
    | Some b -> ("includeClientCommands", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match r.clr_include_skills with
    | Some b -> ("includeSkills", `Bool b) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Model Billing Types                                                        *)
(* ========================================================================== *)

type model_billing_token_prices = {
  mbtp_batch_size : int option;
  mbtp_cache_price : int option;
  mbtp_input_price : int option;
  mbtp_output_price : int option;
}

let model_billing_token_prices_of_yojson (json : Yojson.Safe.t)
    : (model_billing_token_prices, string) result =
  try
    Ok
      { mbtp_batch_size =
          (try Some (json |> member "batchSize" |> to_int) with _ -> None)
      ; mbtp_cache_price =
          (try Some (json |> member "cachePrice" |> to_int) with _ -> None)
      ; mbtp_input_price =
          (try Some (json |> member "inputPrice" |> to_int) with _ -> None)
      ; mbtp_output_price =
          (try Some (json |> member "outputPrice" |> to_int) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let model_billing_token_prices_to_yojson (p : model_billing_token_prices) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match p.mbtp_batch_size with
    | Some v -> ("batchSize", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_cache_price with
    | Some v -> ("cachePrice", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_input_price with
    | Some v -> ("inputPrice", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_output_price with
    | Some v -> ("outputPrice", `Int v) :: fields
    | None -> fields
  in
  `Assoc fields

type model_billing = {
  mb_multiplier : float;
  mb_token_prices : model_billing_token_prices option;
  mb_picker_price_category : model_picker_price_category option;
}

let model_billing_of_yojson (json : Yojson.Safe.t)
    : (model_billing, string) result =
  try
    Ok
      { mb_multiplier = json |> member "multiplier" |> to_float
      ; mb_token_prices =
          (try
             match model_billing_token_prices_of_yojson (json |> member "tokenPrices") with
             | Ok v -> Some v
             | Error _ -> None
           with _ -> None)
      ; mb_picker_price_category =
          (try Some (model_picker_price_category_of_string
                       (json |> member "pickerPriceCategory" |> to_string))
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let model_billing_to_yojson (b : model_billing) : Yojson.Safe.t =
  let fields = [ ("multiplier", `Float b.mb_multiplier) ] in
  let fields =
    match b.mb_token_prices with
    | Some tp -> ("tokenPrices", model_billing_token_prices_to_yojson tp) :: fields
    | None -> fields
  in
  let fields =
    match b.mb_picker_price_category with
    | Some c -> ("pickerPriceCategory", `String (model_picker_price_category_to_string c)) :: fields
    | None -> fields
  in
  `Assoc fields

(* Experimental *)
(* Diagnostics from loading skills. *)

type skills_load_diagnostics = {
  sld_errors : string list;
  sld_warnings : string list;
}

let skills_load_diagnostics_of_yojson (json : Yojson.Safe.t)
    : (skills_load_diagnostics, string) result =
  try
    Ok
      { sld_errors = json |> member "errors" |> to_list |> List.map to_string
      ; sld_warnings = json |> member "warnings" |> to_list |> List.map to_string
      }
  with exn -> Error (Printexc.to_string exn)

let skills_load_diagnostics_to_yojson (d : skills_load_diagnostics) : Yojson.Safe.t =
  `Assoc
    [ ("errors", `List (List.map (fun s -> `String s) d.sld_errors))
    ; ("warnings", `List (List.map (fun s -> `String s) d.sld_warnings))
    ]

(* Experimental *)
(* Mode for remote sessions. *)

type remote_session_mode =
  | RemoteExport
  | RemoteOff
  | RemoteOn

let remote_session_mode_to_string = function
  | RemoteExport -> "export"
  | RemoteOff -> "off"
  | RemoteOn -> "on"

let remote_session_mode_of_string = function
  | "export" -> RemoteExport
  | "off" -> RemoteOff
  | "on" -> RemoteOn
  | _ -> RemoteOff

(* Experimental *)
(* Request to enable remote mode. *)

type remote_enable_request = {
  rer_mode : remote_session_mode option;
}

let remote_enable_request_of_yojson (json : Yojson.Safe.t)
    : (remote_enable_request, string) result =
  try
    let mode =
      match json |> member "mode" with
      | `Null -> None
      | v -> Some (remote_session_mode_of_string (to_string v))
    in
    Ok { rer_mode = mode }
  with exn -> Error (Printexc.to_string exn)

let remote_enable_request_to_yojson (r : remote_enable_request) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match r.rer_mode with
    | Some m -> ("mode", `String (remote_session_mode_to_string m)) :: fields
    | None -> fields
  in
  `Assoc fields

(* Experimental *)
(* Result of enabling remote mode. *)

type remote_enable_result = {
  rer_remote_steerable : bool;
  rer_url : string option;
}

let remote_enable_result_of_yojson (json : Yojson.Safe.t)
    : (remote_enable_result, string) result =
  try
    Ok
      { rer_remote_steerable = json |> member "remoteSteerable" |> to_bool
      ; rer_url =
          (match json |> member "url" with
           | `Null -> None
           | v -> Some (to_string v))
      }
  with exn -> Error (Printexc.to_string exn)

let remote_enable_result_to_yojson (r : remote_enable_result) : Yojson.Safe.t =
  let fields = [ ("remoteSteerable", `Bool r.rer_remote_steerable) ] in
  let fields =
    match r.rer_url with
    | Some u -> ("url", `String u) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Session Events                                                             *)
(* ========================================================================== *)

type session_event_type =
  | AssistantMessage
  | AssistantMessageDelta
  | AssistantReasoning
  | AssistantReasoningDelta
  | SessionIdle
  | SessionError
  | PermissionRequested
  | ToolCalled
  | Unknown of string

let session_event_type_of_string = function
  | "assistant.message" -> AssistantMessage
  | "assistant.message_delta" -> AssistantMessageDelta
  | "assistant.reasoning" -> AssistantReasoning
  | "assistant.reasoning_delta" -> AssistantReasoningDelta
  | "session.idle" -> SessionIdle
  | "session.error" -> SessionError
  | "permission.requested" -> PermissionRequested
  | "tool.called" -> ToolCalled
  | s -> Unknown s

type session_event = {
  event_type : session_event_type;
  id : string;
  timestamp : string;
  parent_id : string option;
  agent_id : string option;
  ephemeral : bool option;
  data : Yojson.Safe.t;
}

let session_event_of_yojson (json : Yojson.Safe.t) : (session_event, string) result =
  try
    let etype = json |> member "type" |> to_string in
    let data = try json |> member "data" with _ -> `Null in
    let id = try json |> member "id" |> to_string with _ -> "" in
    let timestamp = try json |> member "timestamp" |> to_string with _ -> "" in
    let parent_id = try Some (json |> member "parentId" |> to_string) with _ -> None in
    let agent_id = try Some (json |> member "agentId" |> to_string) with _ -> None in
    let ephemeral = try Some (json |> member "ephemeral" |> to_bool) with _ -> None in
    Ok { event_type = session_event_type_of_string etype; id; timestamp; parent_id; agent_id; ephemeral; data }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Permission Types                                                           *)
(* ========================================================================== *)

type permission_decision =
  | Approved
  | DeniedByUser
  | DeniedByPolicy

let permission_decision_to_string = function
  | Approved -> "approved"
  | DeniedByUser -> "deniedInteractivelyByUser"
  | DeniedByPolicy -> "deniedByPolicy"

type permission_result = {
  decision : permission_decision;
  rules : Yojson.Safe.t option;
}

let permission_result_to_yojson (r : permission_result) : Yojson.Safe.t =
  let fields =
    [ ("kind", `String (permission_decision_to_string r.decision)) ]
  in
  let fields =
    match r.rules with
    | Some v -> ("rules", v) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Exit Plan Mode                                                             *)
(* ========================================================================== *)

type exit_plan_mode_request = {
  epm_session_id : string;
}

let exit_plan_mode_request_of_yojson (json : Yojson.Safe.t)
    : (exit_plan_mode_request, string) result =
  try
    Ok { epm_session_id = json |> member "sessionId" |> to_string }
  with exn -> Error (Printexc.to_string exn)

type exit_plan_mode_response = {
  epm_approved : bool;
}

let exit_plan_mode_response_to_yojson (r : exit_plan_mode_response) : Yojson.Safe.t =
  `Assoc [ ("approved", `Bool r.epm_approved) ]

let default_exit_plan_mode_response () = { epm_approved = true }

(* ========================================================================== *)
(* Trace Context                                                              *)
(* ========================================================================== *)

type trace_context = {
  traceparent : string option;
  tracestate : string option;
}

(* ========================================================================== *)
(* Status Types                                                               *)
(* ========================================================================== *)

type get_status_response = {
  protocol_version : int;
  server_version : string option;
}

let get_status_response_of_yojson (json : Yojson.Safe.t)
    : (get_status_response, string) result =
  try
    Ok
      { protocol_version = json |> member "protocolVersion" |> to_int
      ; server_version =
          (try Some (json |> member "serverVersion" |> to_string)
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

type model_info = {
  model_id : string;
  model_name : string option;
}

let model_info_of_yojson (json : Yojson.Safe.t) : (model_info, string) result =
  try
    Ok
      { model_id = json |> member "modelId" |> to_string
      ; model_name =
          (try Some (json |> member "modelName" |> to_string)
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Client Options                                                             *)
(* ========================================================================== *)

type client_options = {
  cli_path : string option;
  cli_url : string option;
  log_level : string option;
  github_token : string option;
  use_logged_in_user : bool option;
  session_idle_timeout_seconds : int option;
  session_fs : session_fs_config option;
  copilot_home : string option;
  tcp_connection_token : string option;
  remote : bool option;
  on_get_trace_context : (unit -> trace_context) option;
}

let default_client_options () =
  { cli_path = None
  ; cli_url = None
  ; log_level = None
  ; github_token = None
  ; use_logged_in_user = None
  ; session_idle_timeout_seconds = None
  ; session_fs = None
  ; copilot_home = None
  ; tcp_connection_token = None
  ; remote = None
  ; on_get_trace_context = None
  }

(* ========================================================================== *)
(* Helper Constructors                                                        *)
(* ========================================================================== *)

let make_message ?mode prompt = { prompt; mode }

let make_tool_definition ?parameters name description =
  { tool_name = name; tool_description = description; tool_parameters = parameters }
