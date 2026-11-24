--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Types -- Core type definitions used across the SDK.
--
--  All public records, enumerations, and access types live here so that
--  every other child package can "with Copilot.Types; use Copilot.Types;"
--  and get a consistent view of the domain model.
--

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Calendar;           use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package Copilot.Types is

   --  -----------------------------------------------------------------------
   --  Primitive helpers
   --  -----------------------------------------------------------------------

   subtype UString is Unbounded_String;
   function "+" (S : String) return UString renames To_Unbounded_String;
   function "-" (U : UString) return String renames To_String;

   Null_UString : constant UString := Null_Unbounded_String;

   --  -----------------------------------------------------------------------
   --  Connection state
   --  -----------------------------------------------------------------------

   type Connection_State is
     (Disconnected,
      Connecting,
      Connected,
      Reconnecting,
      Stopping,
      Stopped);

   --  -----------------------------------------------------------------------
   --  Log level
   --  -----------------------------------------------------------------------

   type Log_Level is (Trace, Debug, Info, Warn, Error, Fatal);

   --  -----------------------------------------------------------------------
   --  Upstream-sync feature types & constants (parity with @github/copilot-sdk)
   --  -----------------------------------------------------------------------

   --  Per-session AI-credit spending limits (SessionLimitsConfig).
   type Session_Limits_Config is record
      Max_Ai_Credits : Natural := 0;
   end record;

   --  Persistent session memory configuration (MemoryConfiguration / memory_config).
   type Memory_Configuration is record
      Enabled : Boolean := False;
   end record;

   --  Arguments passed to a BYOK bearer_token_provider (per-session scoping).
   type Provider_Token_Args is record
      Session_Id : UString := Null_UString;
   end record;

   --  HTTP request_handler (CopilotRequestHandler): intercept/mutate an
   --  outbound LLM inference request (JSON in, possibly-modified JSON out).
   type Request_Handler_Access is
     access function (Request_Json : String) return String;

   --  BYOK bearer_token_provider (per-session token minting; a.k.a. get_bearer_token).
   type Bearer_Token_Provider_Access is
     access function (Args : Provider_Token_Args) return String;

   --  MCP OAuth host token handler (on_mcp_auth_request); when set, the runtime
   --  is signalled via the mcpAuthHandler flag on session.create.
   type Mcp_Auth_Handler_Access is
     access function (Session_Id : String; Server_Url : String) return String;

   --  Post-tool-use hook (on_post_tool_use / post_tool_use): inspect a tool
   --  result and optionally return an override.
   type Post_Tool_Use_Hook_Access is
     access function
       (Tool_Name : String; Result_Json : String; Session_Id : String)
        return String;

   --  Pre-MCP-tool-call hook (on_pre_mcp_tool_call / pre_mcp_tool_call): gate an
   --  MCP server tool call, returning a permission decision.
   type Pre_Mcp_Tool_Call_Hook_Access is
     access function
       (Server_Name : String; Tool_Name : String;
        Args_Json : String; Session_Id : String) return String;

   --  Tool "defer" loading policy (ToolDefer): eager pre-load or lazy search.
   Tool_Defer_Auto  : constant String := "auto";
   Tool_Defer_Never : constant String := "never";

   --  System-message section identifiers.  "preamble" targets the identity
   --  preamble; "preserve" shields a section from a group-level remove.
   Section_Preamble          : constant String := "preamble";
   Section_Identity          : constant String := "identity";
   Section_Tool_Instructions : constant String := "tool_instructions";
   Section_Preserve          : constant String := "preserve";

   --  GitHub-anchored attachment variants.
   GitHub_Commit          : constant String := "GitHubCommit";
   GitHub_Release         : constant String := "GitHubRelease";
   GitHub_Actions_Job     : constant String := "GitHubActionsJob";
   GitHub_Repository      : constant String := "GitHubRepository";
   GitHub_File_Diff       : constant String := "GitHubFileDiff";
   GitHub_Tree_Comparison : constant String := "GitHubTreeComparison";
   GitHub_Url             : constant String := "GitHubUrl";
   GitHub_File            : constant String := "GitHubFile";
   GitHub_Snippet         : constant String := "GitHubSnippet";

   --  Exit-plan-mode handler: receives the plan JSON, returns True to
   --  approve leaving plan mode.
   type Exit_Plan_Mode_Handler is access
     function (Plan_Json : String) return Boolean;

   --  W3C trace context provider: returns a JSON object with traceparent /
   --  tracestate members, or "" when no trace is active.
   type Trace_Context_Provider is access function return String;

   --  -----------------------------------------------------------------------
   --  SDK protocol 4 / v3.0.0 feature types (Microsoft Copilot SDK 2.0)
   --  -----------------------------------------------------------------------

   --  Optional boolean: Unset leaves the option out of the wire payload.
   type Tri_Bool is (Unset, No, Yes);

   --  Client mode (Client_Options.Mode).  "coding-agent" is the default and
   --  keeps today's behavior.  "empty" requires Available_Tools_Json on
   --  every create/resume and turns off ambient features (telemetry,
   --  repository hooks, embeddings, cross-session store, memory).
   --  "copilot-cli" is accepted as a legacy alias of "coding-agent".
   Client_Mode_Coding_Agent : constant String := "coding-agent";
   Client_Mode_Copilot_Cli  : constant String := "copilot-cli";
   Client_Mode_Empty        : constant String := "empty";

   --  Identity of the integrating application, sent as clientInfo in the
   --  connect handshake.  Empty fields are dropped and nothing is sent when
   --  every field is empty.
   type Client_Info_Config is record
      Editor_Name       : UString := Null_UString;
      Editor_Version    : UString := Null_UString;
      Extension_Name    : UString := Null_UString;
      Extension_Version : UString := Null_UString;
   end record;

   Null_Client_Info : constant Client_Info_Config :=
     (others => Null_UString);

   --  Auto routing tiers for Copilot.Session.Switch_Auto_Tier.
   Auto_Tier_Fast         : constant String := "fast";
   Auto_Tier_Efficiency   : constant String := "efficiency";
   Auto_Tier_Balance      : constant String := "balance";
   Auto_Tier_Intelligence : constant String := "intelligence";

   --  Ask-user tool variants (Session_Config.Ask_User_Variant).
   Ask_User_Variant_Legacy      : constant String := "legacy";
   Ask_User_Variant_Elicitation : constant String := "elicitation";

   --  Working-directory trust grants (Session_Config.Trust_Working_Directory):
   --  "session" trusts it for this session, "user" persists the grant.
   Working_Directory_Trust_Session : constant String := "session";
   Working_Directory_Trust_User    : constant String := "user";

   --  Message sources (upstream MessageSource) for Message_Options.Source:
   --  "user", "system" or "agent-<id>".
   Message_Source_User   : constant String := "user";
   Message_Source_System : constant String := "system";

   function Agent_Message_Source (Agent_Id : String) return String is
     ("agent-" & Agent_Id);

   --  Streamed shell output event.  Replaces the removed
   --  tool.execution_partial_result event (which is still tolerated).
   Event_Tool_Shell_Output : constant String := "tool.shell_output";

   type Shell_Output_Event_Data is record
      Tool_Call_Id : UString := Null_UString;
      Text         : UString := Null_UString;
      Sequence     : Natural := 0;
      --  "stdout", "stderr", or empty when the runtime omits it.
      Stream       : UString := Null_UString;
   end record;

   --  Permission-mode refusal reason (PermissionsSetModeResult.reason).
   Permission_Mode_Reason_Managed_Policy : constant String := "managed_policy";

   type Permission_Mode_Result is record
      Success : Boolean := False;
      Mode    : UString := Null_UString;
      --  Why the mode was refused, when known (for example "managed_policy").
      Reason  : UString := Null_UString;
   end record;

   --  Structured Policy Helper failure (PolicyHelperFailure), read from
   --  error.data.policyHelperFailure when session.create/resume fails.
   type Policy_Helper_Failure is record
      Present                  : Boolean := False;
      Category                 : UString := Null_UString;
      Failure_Mode             : UString := Null_UString;
      Message                  : UString := Null_UString;
      Previous_Policy_Retained : Boolean := False;
      Reason                   : UString := Null_UString;
      Severity                 : UString := Null_UString;
      Authority                : UString := Null_UString;
      Code                     : UString := Null_UString;
      Details_Json             : UString := Null_UString;
      Diagnostic               : UString := Null_UString;
      Help_Link_Url            : UString := Null_UString;
      Help_Link_Label          : UString := Null_UString;
      Remediation              : UString := Null_UString;
   end record;

   --  Sandbox configuration (permissions.sandbox).  Allow_Bypass lets the
   --  agent ask to run a single command outside the sandbox; every such
   --  request still goes through the permission flow.
   type Sandbox_Config is record
      Enabled                       : Boolean := False;
      Allow_Bypass                  : Boolean := False;
      Add_Current_Working_Directory : Tri_Bool := Unset;
      User_Policy_Json              : UString := Null_UString;
   end record;

   --  Image generation (tools.imageGeneration).
   type Image_Generation_Config is record
      Enabled      : Tri_Bool := Unset;
      Watermark    : Tri_Bool := Unset;
      Save_To_Disk : Tri_Bool := Unset;
   end record;

   --  Autopilot continuation (execution.autopilot).  Max_Continues = 0
   --  leaves the runtime default.
   type Autopilot_Config is record
      Continuation_Enabled : Tri_Bool := Unset;
      Max_Continues        : Natural  := 0;
   end record;

   --  Named BYOK provider (inference.providers[]).
   type Named_Provider_Config is record
      Name            : UString := Null_UString;
      Provider_Type   : UString := Null_UString;  --  "openai", "azure", ...
      Base_Url        : UString := Null_UString;
      Api_Key         : UString := Null_UString;
      --  Shell command whose stdout is the API key (wire: apiKeyCommand).
      Api_Key_Command : UString := Null_UString;
      Bearer_Token    : UString := Null_UString;
      Wire_Api        : UString := Null_UString;  --  "completions"/"responses"
      Transport       : UString := Null_UString;  --  "http"/"websockets"
      Azure_Json      : UString := Null_UString;
      Headers_Json    : UString := Null_UString;
      Model_Provider  : UString := Null_UString;
   end record;

   package Provider_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Named_Provider_Config);
   subtype Provider_List is Provider_Vectors.Vector;

   --  Model served by a named provider (inference.models[]).  Select it as
   --  the session model with "provider/id".
   type Provider_Model_Config is record
      Provider                  : UString := Null_UString;
      Id                        : UString := Null_UString;
      Model_Id                  : UString := Null_UString;
      Wire_Model                : UString := Null_UString;
      Name                      : UString := Null_UString;
      Max_Prompt_Tokens         : Natural := 0;
      Max_Context_Window_Tokens : Natural := 0;
      Max_Output_Tokens         : Natural := 0;
      Capabilities_Json         : UString := Null_UString;
   end record;

   package Provider_Model_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Provider_Model_Config);
   subtype Provider_Model_List is Provider_Model_Vectors.Vector;

   --  GitHub token provider, answered through gitHubToken.getToken.
   type GitHub_Token_Request is record
      Registration_Id : UString := Null_UString;
      Host            : UString := Null_UString;
      Session_Id      : UString := Null_UString;
      Reason          : UString := Null_UString;
   end record;

   type GitHub_Token_Result is record
      Cancelled    : Boolean := False;
      Access_Token : UString := Null_UString;
      Token_Type   : UString := Null_UString;
      Expires_In   : Natural := 0;  --  seconds; 0 = not reported
   end record;

   type GitHub_Token_Provider_Access is access
     function (Request : GitHub_Token_Request) return GitHub_Token_Result;

   --  Shell authentication prompts (shellAuthentication.request).
   type Shell_Authentication_Request is record
      Session_Id : UString := Null_UString;
      Request_Id : UString := Null_UString;
      Program    : UString := Null_UString;
      Prompt     : UString := Null_UString;
      Input_Kind : UString := Null_UString;  --  "secret", "text", "confirm"
      Command    : UString := Null_UString;
      Cwd        : UString := Null_UString;
   end record;

   type Shell_Authentication_Response is record
      --  False replies {"action":"cancel"}.
      Accepted : Boolean := False;
      Value    : UString := Null_UString;
   end record;

   type Shell_Authentication_Handler_Access is access
     function (Request : Shell_Authentication_Request)
               return Shell_Authentication_Response;

   --  Memory store callback (memoryStore.store).
   type Stored_Memory is record
      Session_Id     : UString := Null_UString;
      Subject        : UString := Null_UString;
      Fact           : UString := Null_UString;
      Citations_Json : UString := Null_UString;
      Reason         : UString := Null_UString;
   end record;

   type Memory_Store_Handler_Access is access
     procedure (Memory : Stored_Memory);

   --  Network callbacks (network.setProvider).  Return "" to decline.
   type Proxy_Resolver_Access is access
     function (Url : String) return String;

   type Negotiate_Authenticator_Access is access
     function (Spn : String) return String;

   type Basic_Credentials is record
      Username : UString := Null_UString;
      Password : UString := Null_UString;
   end record;

   type Basic_Authenticator_Access is access
     function (Proxy             : String;
               Realm             : String;
               Previous_Rejected : Boolean) return Basic_Credentials;

   type Network_Options is record
      Scope                  : UString := Null_UString;  --  "connection"/"runtime"
      Resolve_Proxy          : Proxy_Resolver_Access := null;
      Authenticate_Negotiate : Negotiate_Authenticator_Access := null;
      Authenticate_Basic     : Basic_Authenticator_Access := null;
   end record;

   Null_Network_Options : constant Network_Options :=
     (Scope                  => Null_UString,
      Resolve_Proxy          => null,
      Authenticate_Negotiate => null,
      Authenticate_Basic     => null);

   --  Sub-agent lifecycle hooks (hookType "subagentStart"/"subagentStop").
   type Subagent_Hook_Input is record
      Session_Id         : UString := Null_UString;
      Agent_Name         : UString := Null_UString;
      Agent_Display_Name : UString := Null_UString;
      Agent_Description  : UString := Null_UString;
      Transcript_Path    : UString := Null_UString;
      --  subagentStop only.
      Agent_Id           : UString := Null_UString;
      Agent_Type         : UString := Null_UString;
      Stop_Reason        : UString := Null_UString;
      Response           : UString := Null_UString;
   end record;

   --  Returns additional context prepended to the child prompt ("" = none).
   type Subagent_Start_Hook_Access is access
     function (Input : Subagent_Hook_Input) return String;

   type Subagent_Stop_Decision is record
      Block             : Boolean := False;
      Reason            : UString := Null_UString;
      Modified_Response : UString := Null_UString;
   end record;

   type Subagent_Stop_Hook_Access is access
     function (Input : Subagent_Hook_Input) return Subagent_Stop_Decision;

   --  -----------------------------------------------------------------------
   --  User input handling (userInput.request, the ask_user tool)
   --  -----------------------------------------------------------------------

   --  Image support advertised by a userInput.request.  Present is False
   --  when the runtime does not accept images in the answer.  The runtime
   --  enforces these limits.
   type User_Input_Image_Attachment_Constraints is record
      Present                 : Boolean := False;
      --  Accepted MIME types as a JSON array of strings.
      Allowed_Mime_Types_Json : UString := Null_UString;
      --  Maximum decoded byte length of each image.
      Max_Bytes_Per_Image     : Natural := 0;
      --  Maximum number of images in one answer.
      Max_Count               : Natural := 0;
   end record;

   --  An inline image attached to an ask_user answer (see
   --  User_Input_Image_Response).  Data is the base64 encoded image; the wire
   --  type is always "image".
   type User_Input_Image_Attachment is record
      Data         : UString := Null_UString;
      Mime_Type    : UString := Null_UString;
      Description  : UString := Null_UString;  --  optional
      Display_Name : UString := Null_UString;  --  optional
   end record;

   package User_Input_Image_Attachment_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => User_Input_Image_Attachment);
   subtype User_Input_Image_Attachment_List is
     User_Input_Image_Attachment_Vectors.Vector;

   type User_Input_Request is record
      Question                     : UString := Null_UString;
      Session_Id                   : UString := Null_UString;
      --  Offered choices as a JSON array of strings ("" when none).
      Choices_Json                 : UString := Null_UString;
      --  Whether a freeform answer is allowed.
      Allow_Freeform               : Boolean := True;
      --  Id of the pending request ("" when the runtime omits it).
      Request_Id                   : UString := Null_UString;
      Image_Attachment_Constraints : User_Input_Image_Attachment_Constraints;
      --  The request's opaque _meta object as raw JSON ("" when absent).
      Meta_Json                    : UString := Null_UString;
   end record;

   --  A text-only answer.  To answer with images, register a
   --  User_Input_Image_Handler (Session_Config.On_User_Input_Image_Request).
   type User_Input_Response is record
      Answer       : UString  := Null_UString;
      Was_Freeform : Boolean  := True;
   end record;

   type User_Input_Handler is access
     function (Req : User_Input_Request) return User_Input_Response;

   --  An answer that can also carry images, returned by a
   --  User_Input_Image_Handler.  Attachments are only valid when the request
   --  advertised image support and are sent only when the list is not empty.
   --  An image-only answer uses an empty Answer and Was_Freeform => True.
   type User_Input_Image_Response is record
      Answer       : UString := Null_UString;
      Was_Freeform : Boolean := True;
      Attachments  : User_Input_Image_Attachment_List :=
        User_Input_Image_Attachment_Vectors.Empty_Vector;
   end record;

   --  Ask_user handler whose answers can include images
   --  (Session_Config.On_User_Input_Image_Request).
   type User_Input_Image_Handler is access
     function (Req : User_Input_Request) return User_Input_Image_Response;

   --  -----------------------------------------------------------------------
   --  v3.1.0 feature types (Microsoft Copilot SDK 2.0.3-preview.0)
   --  -----------------------------------------------------------------------

   --  Result of Copilot.Client.Mcp_Secrets_Save (mcp.secrets.save).
   type Mcp_Secrets_Save_Result is record
      --  True when the OS keychain stored the credential, False for the
      --  file backend.
      Used_Keychain : Boolean := False;
   end record;

   --  A user-scoped approval for one installed plugin extension.
   type Permissions_Extension_Approval is record
      --  Stable identity, for example "plugin:plugin-name:extension-name".
      Extension_Name     : UString := Null_UString;
      Plugin_Name        : UString := Null_UString;
      --  The exact privileged capabilities the user approved, as a JSON
      --  array of strings.
      Capabilities_Json  : UString := Null_UString;
      --  RFC 3339 timestamp.
      Granted_At         : UString := Null_UString;
      --  Opaque; pass it back unchanged to Extension_Approvals_Revoke.
      Installation_Key   : UString := Null_UString;
      Installation_Label : UString := Null_UString;  --  optional
   end record;

   package Permissions_Extension_Approval_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Permissions_Extension_Approval);
   subtype Permissions_Extension_Approval_List is
     Permissions_Extension_Approval_Vectors.Vector;

   --  Result of Copilot.Client.Extension_Approvals_Grant
   --  (permissions.extensionApprovals.grant).  Approval holds the stored
   --  approval when the runtime returned one.
   type Permissions_Extension_Approvals_Grant_Result is record
      Granted  : Boolean := False;
      Approval : Permissions_Extension_Approval;
   end record;

   --  Result of Copilot.Client.Extension_Approvals_Revoke
   --  (permissions.extensionApprovals.revoke).
   type Permissions_Extension_Approvals_Revoke_Result is record
      Revoked : Boolean := False;
   end record;

   --  -----------------------------------------------------------------------
   --  Client options
   --  -----------------------------------------------------------------------

   type Client_Options is record
      CLI_Path        : UString  := Null_UString;
      CLI_Url         : UString  := Null_UString;
      CLI_Args        : UString  := Null_UString;
      Cwd             : UString  := Null_UString;
      Port            : Natural  := 0;
      Use_Stdio       : Boolean  := True;
      Log             : Log_Level := Info;
      Auto_Start      : Boolean  := True;
      Auto_Restart    : Boolean  := True;
      Github_Token    : UString  := Null_UString;
      Use_Logged_In   : Boolean  := False;
      Session_Idle_Timeout_Seconds : Natural := 0;
      Session_Fs_Initial_Cwd      : UString := Null_UString;
      Session_Fs_State_Path       : UString := Null_UString;
      Session_Fs_Conventions      : UString := Null_UString;
      Copilot_Home                : UString := Null_UString;
      Tcp_Connection_Token        : UString := Null_UString;
      --  HTTP request_handler / CopilotRequestHandler for outbound inference requests.
      Request_Handler             : Request_Handler_Access := null;
      --  BYOK bearer_token_provider (per-session token minting).
      Bearer_Token_Provider       : Bearer_Token_Provider_Access := null;
      --  Integrating application identity sent as clientInfo on connect.
      Client_Info                 : Client_Info_Config := Null_Client_Info;
      --  Client mode: "coding-agent" (default), "empty", or the legacy
      --  alias "copilot-cli".
      Mode                        : UString := +Client_Mode_Coding_Agent;
      --  Proxy / authentication callbacks (network.setProvider).
      Network                     : Network_Options := Null_Network_Options;
   end record;

   Default_Options : constant Client_Options :=
     (CLI_Path        => Null_UString,
      CLI_Url         => Null_UString,
      CLI_Args        => Null_UString,
      Cwd             => Null_UString,
      Port            => 0,
      Use_Stdio       => True,
      Log             => Info,
      Auto_Start      => True,
      Auto_Restart    => True,
      Github_Token    => Null_UString,
      Use_Logged_In   => False,
      Session_Idle_Timeout_Seconds => 0,
      Session_Fs_Initial_Cwd      => Null_UString,
      Session_Fs_State_Path       => Null_UString,
      Session_Fs_Conventions      => Null_UString,
      Copilot_Home                => Null_UString,
      Tcp_Connection_Token        => Null_UString,
      Request_Handler             => null,
      Bearer_Token_Provider       => null,
      Client_Info                 => Null_Client_Info,
      Mode                        => +Client_Mode_Coding_Agent,
      Network                     => Null_Network_Options);

   --  -----------------------------------------------------------------------
   --  Session configuration
   --  -----------------------------------------------------------------------

   type Session_Config is record
      System_Prompt                    : UString  := Null_UString;
      Model                            : UString  := Null_UString;
      Github_Token                     : UString  := Null_UString;
      Streaming                        : Boolean  := True;
      Workspace_Path                   : UString  := Null_UString;
      Session_Idle_Timeout             : Natural  := 0;
      Skill_Directories                : UString  := Null_UString;
      Disabled_Skills                  : UString  := Null_UString;
      Excluded_Tools                   : UString  := Null_UString;
      Include_Sub_Agent_Streaming      : Boolean  := False;
      Enable_Config_Discovery          : Boolean  := False;
      Model_Capabilities_Json          : UString  := Null_UString;
      Request_Headers_Json             : UString  := Null_UString;
      Mcp_Servers_Json                 : UString  := Null_UString;
      Commands_Json                    : UString  := Null_UString;
      Response_Format                  : UString  := Null_UString;
      Image_Size                       : UString  := Null_UString;
      Image_Quality                    : UString  := Null_UString;
      Image_Style                      : UString  := Null_UString;
      Available_Tools_Json          : UString  := Null_UString;
      Auth_Token                    : UString  := Null_UString;
      Elicitation_Handler_Set       : Boolean  := False;
      Instruction_Directories_Json  : UString  := Null_UString;
      --  --- Upstream-sync session options (parity with @github/copilot-sdk) ---
      Enable_Citations              : Boolean  := False;
      Excluded_Builtin_Agents_Json  : UString  := Null_UString;
      Session_Limits                : Session_Limits_Config := (Max_Ai_Credits => 0);
      Memory                        : Memory_Configuration  := (Enabled => False);
      Otlp_Protocol                 : UString  := Null_UString;
      Enable_Web_Socket_Responses   : Boolean  := False;
      Exp_Assignments_Json          : UString  := Null_UString;
      On_Mcp_Auth_Request           : Mcp_Auth_Handler_Access       := null;
      On_Post_Tool_Use              : Post_Tool_Use_Hook_Access     := null;
      On_Pre_Mcp_Tool_Call          : Pre_Mcp_Tool_Call_Hook_Access := null;
      --  --- Newly-synced session options (parity with @github/copilot-sdk) ---
      Rewind_Enabled                : Boolean  := False;
      Additional_Directories_Json   : UString  := Null_UString;
      Disabled_Mcp_Servers_Json     : UString  := Null_UString;
      Github_Mcp_Tool_Config_Json   : UString  := Null_UString;
      Canvas_Provider_Json          : UString  := Null_UString;
      Custom_Agents_Local_Only      : Boolean  := False;
      Decision_Context_Json         : UString  := Null_UString;
      Builtin_Plugin_Directories_Json : UString := Null_UString;
      Args_Schema_Json              : UString  := Null_UString;
      Reasoning_Effort              : UString  := Null_UString;
      Tool_Search_Json              : UString  := Null_UString;
      In_Process                    : Boolean  := False;
      Experimental_Mode             : Boolean  := False;
      Content_Exclusion             : Boolean  := False;
      On_User_Prompt_Transformed    : Post_Tool_Use_Hook_Access     := null;
      --  --- SDK protocol 4 / v3.0.0 session options ---
      --  Working directory of the session (wire: workingDirectory).
      Working_Directory             : UString  := Null_UString;
      --  Tool definitions as a JSON array (see Copilot.Tools.Tools_To_Json).
      Tools_Json                    : UString  := Null_UString;
      --  Ask-user tool variant: "legacy" or "elicitation".
      Ask_User_Variant              : UString  := Null_UString;
      --  Callback minting GitHub tokens (gitHubToken.getToken).  Mutually
      --  exclusive with Github_Token.
      GitHub_Token_Provider         : GitHub_Token_Provider_Access := null;
      Sandbox                       : Sandbox_Config;
      --  Feature flag overrides as a JSON object, e.g. {"flag":true}.
      Feature_Flags_Json            : UString  := Null_UString;
      --  Model allowlist as a JSON array of model ids.
      Allowed_Models_Json           : UString  := Null_UString;
      --  OAuth client id metadata document URL for MCP servers.
      Auth_Client_Id_Metadata_Url   : UString  := Null_UString;
      --  Model capabilities override limits.max_output_tokens (ignored when
      --  Model_Capabilities_Json is set).
      Max_Output_Tokens             : Natural  := 0;
      --  Extra agent / hook directories as JSON arrays of paths.
      Agent_Directories_Json        : UString  := Null_UString;
      Hook_Directories_Json         : UString  := Null_UString;
      --  Grant directory trust at creation (create only): "session" or
      --  "user" (see the Working_Directory_Trust_* constants); needs a
      --  working directory.
      Trust_Working_Directory       : UString  := Null_UString;
      Image_Generation              : Image_Generation_Config;
      Autopilot                     : Autopilot_Config;
      On_Subagent_Start             : Subagent_Start_Hook_Access := null;
      On_Subagent_Stop              : Subagent_Stop_Hook_Access  := null;
      --  Legacy single BYOK provider as a JSON object (v1 "provider").
      Provider_Json                 : UString  := Null_UString;
      --  Named BYOK providers and the models they serve.
      Providers                     : Provider_List :=
        Provider_Vectors.Empty_Vector;
      Models                        : Provider_Model_List :=
        Provider_Model_Vectors.Empty_Vector;
      Default_Provider              : UString  := Null_UString;
      --  Send an explicit null defaultProvider on resume to clear it.
      Clear_Default_Provider        : Boolean  := False;
      --  Copilot API (CAPI) models: No disables them (BYOK only).
      Capi_Enabled                  : Tri_Bool := Unset;
      On_Shell_Authentication       : Shell_Authentication_Handler_Access :=
        null;
      On_Store_Memory               : Memory_Store_Handler_Access := null;
      --  --- v3.1.0 session options ---
      --  Answers ask_user requests (userInput.request) with text.  Setting it
      --  or On_User_Input_Image_Request enables the ask_user tool
      --  (interaction.requestUserInput).
      On_User_Input_Request         : User_Input_Handler := null;
      --  Answers ask_user requests and can attach images.  Takes precedence
      --  over On_User_Input_Request when both are set.
      On_User_Input_Image_Request   : User_Input_Image_Handler := null;
   end record;

   Default_Session_Config : constant Session_Config :=
     (System_Prompt                    => Null_UString,
      Model                            => Null_UString,
      Github_Token                     => Null_UString,
      Streaming                        => True,
      Workspace_Path                   => Null_UString,
      Session_Idle_Timeout             => 0,
      Skill_Directories                => Null_UString,
      Disabled_Skills                  => Null_UString,
      Excluded_Tools                   => Null_UString,
      Include_Sub_Agent_Streaming      => False,
      Enable_Config_Discovery          => False,
      Model_Capabilities_Json          => Null_UString,
      Request_Headers_Json             => Null_UString,
      Mcp_Servers_Json                 => Null_UString,
      Commands_Json                    => Null_UString,
      Response_Format                  => Null_UString,
      Image_Size                       => Null_UString,
      Image_Quality                    => Null_UString,
      Image_Style                      => Null_UString,
       Available_Tools_Json          => Null_UString,
       Auth_Token                    => Null_UString,
       Elicitation_Handler_Set       => False,
       Instruction_Directories_Json  => Null_UString,
       Enable_Citations              => False,
       Excluded_Builtin_Agents_Json  => Null_UString,
       Session_Limits                => (Max_Ai_Credits => 0),
       Memory                        => (Enabled => False),
       Otlp_Protocol                 => Null_UString,
       Enable_Web_Socket_Responses   => False,
       Exp_Assignments_Json          => Null_UString,
       On_Mcp_Auth_Request           => null,
       On_Post_Tool_Use              => null,
       On_Pre_Mcp_Tool_Call          => null,
       Rewind_Enabled                => False,
       Additional_Directories_Json   => Null_UString,
       Disabled_Mcp_Servers_Json     => Null_UString,
       Github_Mcp_Tool_Config_Json   => Null_UString,
       Canvas_Provider_Json          => Null_UString,
       Custom_Agents_Local_Only      => False,
       Decision_Context_Json         => Null_UString,
       Builtin_Plugin_Directories_Json => Null_UString,
       Args_Schema_Json              => Null_UString,
       Reasoning_Effort              => Null_UString,
       Tool_Search_Json              => Null_UString,
       In_Process                    => False,
       Experimental_Mode             => False,
       Content_Exclusion             => False,
       On_User_Prompt_Transformed    => null,
       Working_Directory             => Null_UString,
       Tools_Json                    => Null_UString,
       Ask_User_Variant              => Null_UString,
       GitHub_Token_Provider         => null,
       Sandbox                       => <>,
       Feature_Flags_Json            => Null_UString,
       Allowed_Models_Json           => Null_UString,
       Auth_Client_Id_Metadata_Url   => Null_UString,
       Max_Output_Tokens             => 0,
       Agent_Directories_Json        => Null_UString,
       Hook_Directories_Json         => Null_UString,
       Trust_Working_Directory       => Null_UString,
       Image_Generation              => <>,
       Autopilot                     => <>,
       On_Subagent_Start             => null,
       On_Subagent_Stop              => null,
       Provider_Json                 => Null_UString,
       Providers                     => Provider_Vectors.Empty_Vector,
       Models                        => Provider_Model_Vectors.Empty_Vector,
       Default_Provider              => Null_UString,
       Clear_Default_Provider        => False,
       Capi_Enabled                  => Unset,
       On_Shell_Authentication       => null,
       On_Store_Memory               => null,
       On_User_Input_Request         => null,
       On_User_Input_Image_Request   => null);

   --  -----------------------------------------------------------------------
   --  Resume session configuration
   --  -----------------------------------------------------------------------

   type Resume_Session_Config is record
      Session_Id                    : UString := Null_UString;
      Model                         : UString := Null_UString;
      Instruction_Directories_Json  : UString := Null_UString;
      --  Options shared with Create_Session (tools, providers, hooks, ...).
      Options                       : Session_Config := Default_Session_Config;
      --  Do not emit a session.resume event (wire: suppressResumeEvent).
      Disable_Resume                : Boolean := False;
      --  Continue work that was pending when the session was persisted.
      Continue_Pending_Work         : Boolean := False;
      --  Rebuild the session from its transcript if the state is damaged.
      Allow_Transcript_Recovery     : Boolean := False;
   end record;

   --  -----------------------------------------------------------------------
   --  Session metadata
   --  -----------------------------------------------------------------------

   type Session_Metadata is record
      Session_Id   : UString  := Null_UString;
      Model        : UString  := Null_UString;
      Created_At   : Time     := Clock;
      Status       : UString  := +"unknown";
      Turn_Count   : Natural  := 0;
   end record;

   --  -----------------------------------------------------------------------
   --  Message / send options
   --  -----------------------------------------------------------------------

   type Message_Options is record
      Prompt      : UString := Null_UString;
      Mode        : UString := Null_UString;
      --  Per-send agent mode override (wire key: agentMode).
      Agent_Mode           : UString := Null_UString;
      --  Prompt text shown to the user instead of the real prompt (wire: displayPrompt).
      Display_Prompt       : UString := Null_UString;
      --  Custom HTTP headers for this send as a JSON object string (wire: requestHeaders).
      Request_Headers_Json : UString := Null_UString;
      --  Who sent the message (upstream MessageSource, wire: source):
      --  "user", "system" or "agent-<id>".
      Source               : UString := Null_UString;
      --  JSON schema for structured output (upstream responseSchema).  Sent
      --  as responseFormat {type: "json_schema", jsonSchema: {...}}.
      Response_Schema_Json : UString := Null_UString;
      --  Opaque caller-chosen id echoed on this turn's events (clientTurnId).
      Client_Turn_Id       : UString := Null_UString;
   end record;

   --  -----------------------------------------------------------------------
   --  Status responses
   --  -----------------------------------------------------------------------

   type Get_Status_Response is record
      Version          : UString := Null_UString;
      Protocol_Version : Natural := 0;
   end record;

   type Auth_Status_Response is record
      Authenticated : Boolean := False;
      User          : UString := Null_UString;
   end record;

   --  -----------------------------------------------------------------------
   --  Model information
   --  -----------------------------------------------------------------------

   type Model_Info is record
      Id          : UString := Null_UString;
      Name        : UString := Null_UString;
      Vendor      : UString := Null_UString;
      Family      : UString := Null_UString;
      Max_Tokens  : Natural := 0;
   end record;

   package Model_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Model_Info);
   subtype Model_List is Model_Vectors.Vector;

   --  -----------------------------------------------------------------------
   --  Session events
   --  -----------------------------------------------------------------------

   type Session_Event is record
      Id            : UString := Null_UString;
      Timestamp     : UString := Null_UString;
      Parent_Id     : UString := Null_UString;
      Agent_Id      : UString := Null_UString;
      Ephemeral     : Boolean := False;
      Event_Type    : UString := Null_UString;
      Session_Id    : UString := Null_UString;
      Content       : UString := Null_UString;
      Raw_Json      : UString := Null_UString;
   end record;

   package Event_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Session_Event);
   subtype Event_List is Event_Vectors.Vector;

   --  -----------------------------------------------------------------------
   --  Session lifecycle events
   --  -----------------------------------------------------------------------

   type Lifecycle_Event_Kind is
     (Session_Created,
      Session_Destroyed,
      Session_Failed,
      Connection_Lost,
      Connection_Restored);

   type Session_Lifecycle_Event is record
      Kind       : Lifecycle_Event_Kind := Session_Created;
      Session_Id : UString := Null_UString;
      Message    : UString := Null_UString;
   end record;

   --  -----------------------------------------------------------------------
   --  Tool definition & handler types
   --  -----------------------------------------------------------------------

   type Tool_Parameter is record
      Name        : UString := Null_UString;
      Param_Type  : UString := +"string";
      Description : UString := Null_UString;
      Required    : Boolean := False;
   end record;

   package Param_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Tool_Parameter);
   subtype Param_List is Param_Vectors.Vector;

   type Tool_Definition is record
      Name        : UString    := Null_UString;
      Description : UString    := Null_UString;
      Parameters  : Param_List := Param_Vectors.Empty_Vector;
      --  Defer loading policy (ToolDefer): "auto" (lazy) or "never" (eager).
      Defer       : UString    := Null_UString;
      --  Safe to retry: repeated calls have no extra effect (wire: idempotent).
      Idempotent  : Boolean    := False;
   end record;

   package Tool_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Tool_Definition);
   subtype Tool_List is Tool_Vectors.Vector;

   --  A tool handler receives raw JSON args and returns a JSON result string.
   type Tool_Handler is access function (Args_Json : String) return String;

   --  -----------------------------------------------------------------------
   --  Permission handling
   --  -----------------------------------------------------------------------

   type Permission_Kind is (Read, Write, Execute, Network, Admin);

   type Permission_Result_Kind is
     (Approved,
      Denied_By_User,
      Denied_By_Policy);

   type Permission_Request is record
      Kind       : Permission_Kind := Read;
      Resource   : UString := Null_UString;
      Session_Id : UString := Null_UString;
   end record;

   type Permission_Result is record
      Kind : Permission_Result_Kind := Denied_By_User;
   end record;

   type Permission_Handler is access
     function (Req : Permission_Request) return Permission_Result;

   --  -----------------------------------------------------------------------
   --  Exceptions
   --  -----------------------------------------------------------------------

   Copilot_Error        : exception;
   Connection_Error     : exception;
   Protocol_Error       : exception;
   Session_Error        : exception;
   Timeout_Error        : exception;
   Tool_Error           : exception;
   Authentication_Error : exception;

end Copilot.Types;
