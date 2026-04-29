--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Copilot.Types;         use Copilot.Types;

package body Copilot.Tools is

   function Define_Tool
     (Name        : String;
      Description : String;
      Params      : Param_List := Param_Vectors.Empty_Vector)
      return Tool_Definition
   is
   begin
      return (Name        => To_Unbounded_String (Name),
              Description => To_Unbounded_String (Description),
              Parameters  => Params);
   end Define_Tool;

   function String_Param
     (Name        : String;
      Description : String) return Tool_Parameter
   is
   begin
      return (Name        => To_Unbounded_String (Name),
              Param_Type  => To_Unbounded_String ("string"),
              Description => To_Unbounded_String (Description),
              Required    => True);
   end String_Param;

   function Optional_String_Param
     (Name        : String;
      Description : String) return Tool_Parameter
   is
   begin
      return (Name        => To_Unbounded_String (Name),
              Param_Type  => To_Unbounded_String ("string"),
              Description => To_Unbounded_String (Description),
              Required    => False);
   end Optional_String_Param;

   function Integer_Param
     (Name        : String;
      Description : String) return Tool_Parameter
   is
   begin
      return (Name        => To_Unbounded_String (Name),
              Param_Type  => To_Unbounded_String ("integer"),
              Description => To_Unbounded_String (Description),
              Required    => True);
   end Integer_Param;

   function Boolean_Param
     (Name        : String;
      Description : String) return Tool_Parameter
   is
   begin
      return (Name        => To_Unbounded_String (Name),
              Param_Type  => To_Unbounded_String ("boolean"),
              Description => To_Unbounded_String (Description),
              Required    => True);
   end Boolean_Param;

   function Tools_To_Json (Defs : Tool_List) return String is
      Result : Unbounded_String := To_Unbounded_String ("[");
      First  : Boolean := True;
   begin
      for Def of Defs loop
         if not First then
            Append (Result, ",");
         end if;
         First := False;

         Append (Result, "{""name"":""" & To_String (Def.Name) & """,");
         Append (Result, """description"":""" &
                         To_String (Def.Description) & """,");
         Append (Result, """parameters"":{""type"":""object"",");
         Append (Result, """properties"":{");

         declare
            PFirst : Boolean := True;
            Req    : Unbounded_String := To_Unbounded_String ("[");
            RFirst : Boolean := True;
         begin
            for P of Def.Parameters loop
               if not PFirst then
                  Append (Result, ",");
               end if;
               PFirst := False;
               Append (Result, """" & To_String (P.Name) & """:{");
               Append (Result, """type"":""" &
                               To_String (P.Param_Type) & """,");
               Append (Result, """description"":""" &
                               To_String (P.Description) & """}");

               if P.Required then
                  if not RFirst then
                     Append (Req, ",");
                  end if;
                  RFirst := False;
                  Append (Req, """" & To_String (P.Name) & """");
               end if;
            end loop;
            Append (Req, "]");
            Append (Result, "},""required"":" & To_String (Req) & "}}");
         end;
      end loop;

      Append (Result, "]");
      return To_String (Result);
   end Tools_To_Json;

end Copilot.Tools;
