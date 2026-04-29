--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Tools -- Helpers for defining tools that the Copilot agent can
--  call during a session.
--

with Copilot.Types; use Copilot.Types;

package Copilot.Tools is

   --  Convenience: create a Tool_Definition with a single call.
   function Define_Tool
     (Name        : String;
      Description : String;
      Params      : Param_List := Param_Vectors.Empty_Vector)
      return Tool_Definition;

   --  Create a required string parameter.
   function String_Param
     (Name        : String;
      Description : String) return Tool_Parameter;

   --  Create an optional string parameter.
   function Optional_String_Param
     (Name        : String;
      Description : String) return Tool_Parameter;

   --  Create a required integer parameter.
   function Integer_Param
     (Name        : String;
      Description : String) return Tool_Parameter;

   --  Create a required boolean parameter.
   function Boolean_Param
     (Name        : String;
      Description : String) return Tool_Parameter;

   --  Serialize a Tool_Definition list to JSON suitable for the session
   --  configuration "tools" field.
   function Tools_To_Json (Defs : Tool_List) return String;

end Copilot.Tools;
