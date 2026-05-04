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
      Tcp_Connection_Token        => Null_UString);

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
       Instruction_Directories_Json  => Null_UString);

   --  -----------------------------------------------------------------------
   --  Resume session configuration
   --  -----------------------------------------------------------------------

   type Resume_Session_Config is record
      Session_Id                    : UString := Null_UString;
      Model                         : UString := Null_UString;
      Instruction_Directories_Json  : UString := Null_UString;
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
      Session_Error,
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
   --  User input handling
   --  -----------------------------------------------------------------------

   type User_Input_Request is record
      Question   : UString := Null_UString;
      Session_Id : UString := Null_UString;
   end record;

   type User_Input_Response is record
      Answer       : UString  := Null_UString;
      Was_Freeform : Boolean  := True;
   end record;

   type User_Input_Handler is access
     function (Req : User_Input_Request) return User_Input_Response;

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
