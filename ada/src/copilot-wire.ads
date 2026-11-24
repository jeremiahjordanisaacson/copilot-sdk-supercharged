--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Wire -- JSON helpers and the SDK protocol 4 payload translator.
--
--  The client builds flat (v1-style) session.create / session.resume
--  payloads from Session_Config.  SDK protocol 4 groups most options into
--  objects such as "inference", "tools" and "permissions".
--  Group_Session_Params converts the flat payload into the grouped payload
--  right before it is sent, so callers keep using the same configuration
--  fields they use today.
--

package Copilot.Wire is

   --  Escape a string for inclusion inside a JSON string literal.
   function Escape (S : String) return String;

   --  JSON string literal (with quotes) for S.
   function Quote (S : String) return String;

   --  Raw JSON text of a top-level member of a JSON object, or "" when the
   --  member is absent.  String values keep their quotes.
   function Member (Json : String; Key : String) return String;

   --  Unescaped value of a top-level string member, or "" when the member
   --  is absent or is not a string.
   function String_Member (Json : String; Key : String) return String;

   --  True when a top-level member exists and is the JSON literal true.
   function Bool_Member (Json : String; Key : String) return Boolean;

   --  Value of a top-level integer member, or Default when absent.
   function Natural_Member
     (Json    : String;
      Key     : String;
      Default : Natural := 0) return Natural;

   --  Call Process with the raw JSON text of every element of the JSON array
   --  Arr (string values keep their quotes).  Does nothing when Arr is not
   --  an array.
   procedure For_Each_Element
     (Arr     : String;
      Process : not null access procedure (Value : String));

   --  Convert a flat (v1-style) session payload into the SDK protocol 4
   --  grouped payload.  Is_Resume selects the session.resume rules
   --  (disableResume becomes suppressResumeEvent, create-only options are
   --  dropped).  When Empty_Mode is True the "empty" client-mode defaults
   --  are applied underneath the caller's values.
   function Group_Session_Params
     (Flat       : String;
      Is_Resume  : Boolean;
      Empty_Mode : Boolean := False) return String;

   --  Random RFC 4122 version 4 UUID, used for callback registrations.
   function New_Registration_Id return String;

end Copilot.Wire;
