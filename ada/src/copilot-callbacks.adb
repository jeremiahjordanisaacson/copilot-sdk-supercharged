--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Copilot.Wire;           use Copilot.Wire;

package body Copilot.Callbacks is

   type Session_Callbacks is record
      Shell_Auth        : Shell_Authentication_Handler_Access := null;
      Store_Memory      : Memory_Store_Handler_Access := null;
      On_Subagent_Start : Subagent_Start_Hook_Access := null;
      On_Subagent_Stop  : Subagent_Stop_Hook_Access := null;
      User_Input        : User_Input_Handler := null;
      User_Input_Image  : User_Input_Image_Handler := null;
   end record;

   package Session_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Session_Callbacks,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   package Token_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => GitHub_Token_Provider_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Sessions          : Session_Maps.Map;
   Token_Providers   : Token_Maps.Map;
   Network_Callbacks : Network_Options := Null_Network_Options;

   function Image (N : Natural) return String is
      Img : constant String := Natural'Image (N);
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   --  -------------------------------------------------------------------
   --  Registration
   --  -------------------------------------------------------------------

   procedure Register_Session
     (Session_Id        : String;
      Shell_Auth        : Shell_Authentication_Handler_Access;
      Store_Memory      : Memory_Store_Handler_Access;
      On_Subagent_Start : Subagent_Start_Hook_Access;
      On_Subagent_Stop  : Subagent_Stop_Hook_Access;
      User_Input        : User_Input_Handler := null;
      User_Input_Image  : User_Input_Image_Handler := null) is
   begin
      if Session_Id'Length = 0 then
         return;
      end if;
      Sessions.Include
        (Session_Id,
         (Shell_Auth        => Shell_Auth,
          Store_Memory      => Store_Memory,
          On_Subagent_Start => On_Subagent_Start,
          On_Subagent_Stop  => On_Subagent_Stop,
          User_Input        => User_Input,
          User_Input_Image  => User_Input_Image));
   end Register_Session;

   procedure Unregister_Session (Session_Id : String) is
   begin
      if Sessions.Contains (Session_Id) then
         Sessions.Delete (Session_Id);
      end if;
   end Unregister_Session;

   procedure Set_User_Input_Handler
     (Session_Id : String;
      Handler    : User_Input_Handler)
   is
      Callbacks : Session_Callbacks;
   begin
      if Session_Id'Length = 0 then
         return;
      end if;
      if Sessions.Contains (Session_Id) then
         Callbacks := Sessions.Element (Session_Id);
      end if;
      Callbacks.User_Input       := Handler;
      Callbacks.User_Input_Image := null;
      Sessions.Include (Session_Id, Callbacks);
   end Set_User_Input_Handler;

   procedure Register_Token_Provider
     (Registration_Id : String;
      Provider        : GitHub_Token_Provider_Access) is
   begin
      if Provider /= null then
         Token_Providers.Include (Registration_Id, Provider);
      end if;
   end Register_Token_Provider;

   procedure Unregister_Token_Provider (Registration_Id : String) is
   begin
      if Token_Providers.Contains (Registration_Id) then
         Token_Providers.Delete (Registration_Id);
      end if;
   end Unregister_Token_Provider;

   procedure Set_Network (Options : Network_Options) is
   begin
      Network_Callbacks := Options;
   end Set_Network;

   function Has_Network_Callbacks (Options : Network_Options) return Boolean is
     (Options.Resolve_Proxy /= null
      or else Options.Authenticate_Negotiate /= null
      or else Options.Authenticate_Basic /= null);

   --  -------------------------------------------------------------------
   --  Request handlers
   --  -------------------------------------------------------------------

   function Get_GitHub_Token
     (Params : String;
      Error  : in out Unbounded_String) return String
   is
      Reg_Id : constant String := String_Member (Params, "registrationId");
   begin
      if not Token_Providers.Contains (Reg_Id) then
         Error := U ("No GitHub token provider is registered for " & Reg_Id);
         return "";
      end if;

      declare
         Request : constant GitHub_Token_Request :=
           (Registration_Id => U (Reg_Id),
            Host            => U (String_Member (Params, "host")),
            Session_Id      => U (String_Member (Params, "sessionId")),
            Reason          => U (String_Member (Params, "reason")));
         Token   : constant GitHub_Token_Result :=
           Token_Providers.Element (Reg_Id).all (Request);
         Reply   : Unbounded_String;
      begin
         if Token.Cancelled or else Length (Token.Access_Token) = 0 then
            return "{""kind"":""cancelled""}";
         end if;
         Reply := U ("{""kind"":""token"",""accessToken"":"
                     & Quote (To_String (Token.Access_Token)));
         if Length (Token.Token_Type) > 0 then
            Append (Reply, ",""tokenType"":"
                           & Quote (To_String (Token.Token_Type)));
         end if;
         if Token.Expires_In > 0 then
            Append (Reply, ",""expiresIn"":" & Image (Token.Expires_In));
         end if;
         Append (Reply, "}");
         return To_String (Reply);
      end;
   end Get_GitHub_Token;

   function Authenticate_Shell
     (Params    : String;
      Callbacks : Session_Callbacks) return String is
   begin
      if Callbacks.Shell_Auth = null then
         return "{""action"":""cancel""}";
      end if;

      declare
         Request : constant Shell_Authentication_Request :=
           (Session_Id => U (String_Member (Params, "sessionId")),
            Request_Id => U (String_Member (Params, "requestId")),
            Program    => U (String_Member (Params, "program")),
            Prompt     => U (String_Member (Params, "prompt")),
            Input_Kind => U (String_Member (Params, "inputKind")),
            Command    => U (String_Member (Params, "command")),
            Cwd        => U (String_Member (Params, "cwd")));
         Reply   : constant Shell_Authentication_Response :=
           Callbacks.Shell_Auth.all (Request);
      begin
         if Reply.Accepted then
            return "{""action"":""accept"",""value"":"
              & Quote (To_String (Reply.Value)) & "}";
         end if;
         return "{""action"":""cancel""}";
      end;
   end Authenticate_Shell;

   function Invoke_Hook
     (Params    : String;
      Callbacks : Session_Callbacks) return String
   is
      Hook_Type  : constant String := String_Member (Params, "hookType");
      Input_Json : constant String := Member (Params, "input");
      Input      : constant Subagent_Hook_Input :=
        (Session_Id         => U (String_Member (Params, "sessionId")),
         Agent_Name         => U (String_Member (Input_Json, "agentName")),
         Agent_Display_Name =>
           U (String_Member (Input_Json, "agentDisplayName")),
         Agent_Description  =>
           U (String_Member (Input_Json, "agentDescription")),
         Transcript_Path    =>
           U (String_Member (Input_Json, "transcriptPath")),
         Agent_Id           => U (String_Member (Input_Json, "agentId")),
         Agent_Type         => U (String_Member (Input_Json, "agentType")),
         Stop_Reason        => U (String_Member (Input_Json, "stopReason")),
         Response           => U (String_Member (Input_Json, "response")));
   begin
      if Hook_Type = "subagentStart"
        and then Callbacks.On_Subagent_Start /= null
      then
         declare
            Context : constant String := Callbacks.On_Subagent_Start.all (Input);
         begin
            if Context'Length > 0 then
               return "{""output"":{""additionalContext"":"
                 & Quote (Context) & "}}";
            end if;
         end;

      elsif Hook_Type = "subagentStop"
        and then Callbacks.On_Subagent_Stop /= null
      then
         declare
            Decision : constant Subagent_Stop_Decision :=
              Callbacks.On_Subagent_Stop.all (Input);
            Output   : Unbounded_String;
         begin
            if Decision.Block and then Length (Decision.Reason) > 0 then
               Output := U ("""decision"":""block"",""reason"":"
                            & Quote (To_String (Decision.Reason)));
            end if;
            if Length (Decision.Modified_Response) > 0 then
               if Length (Output) > 0 then
                  Append (Output, ",");
               end if;
               Append (Output, """modifiedResponse"":"
                               & Quote (To_String (Decision.Modified_Response)));
            end if;
            if Length (Output) > 0 then
               return "{""output"":{" & To_String (Output) & "}}";
            end if;
         end;
      end if;

      --  No handler for this hook type: no output.
      return "{}";
   end Invoke_Hook;

   --  Wire form of an ask_user answer: answer, wasFreeform and, only when
   --  images were attached, attachments (each {"type":"image",...}).
   function User_Input_Reply
     (Reply : User_Input_Image_Response) return String
   is
      Json           : Unbounded_String :=
        U ("{""answer"":" & Quote (To_String (Reply.Answer))
           & ",""wasFreeform"":"
           & (if Reply.Was_Freeform then "true" else "false"));
      Is_First_Image : Boolean := True;
   begin
      if not Reply.Attachments.Is_Empty then
         Append (Json, ",""attachments"":[");
         for Image_Attachment of Reply.Attachments loop
            if not Is_First_Image then
               Append (Json, ",");
            end if;
            Is_First_Image := False;
            Append (Json, "{""type"":""image"",""data"":"
                          & Quote (To_String (Image_Attachment.Data))
                          & ",""mimeType"":"
                          & Quote (To_String (Image_Attachment.Mime_Type)));
            if Length (Image_Attachment.Description) > 0 then
               Append (Json, ",""description"":"
                             & Quote (To_String (Image_Attachment.Description)));
            end if;
            if Length (Image_Attachment.Display_Name) > 0 then
               Append (Json, ",""displayName"":"
                             & Quote (To_String (Image_Attachment.Display_Name)));
            end if;
            Append (Json, "}");
         end loop;
         Append (Json, "]");
      end if;
      Append (Json, "}");
      return To_String (Json);
   end User_Input_Reply;

   --  Answer a userInput.request (ask_user) with the session's handler: the
   --  image handler when one is registered, otherwise the text handler.  The
   --  request's imageAttachmentConstraints, requestId and _meta are passed
   --  through.
   function Ask_User
     (Params    : String;
      Callbacks : Session_Callbacks;
      Error     : in out Unbounded_String) return String
   is
      Constraints : constant String :=
        Member (Params, "imageAttachmentConstraints");
      Request     : User_Input_Request;
   begin
      if Callbacks.User_Input = null
        and then Callbacks.User_Input_Image = null
      then
         Error := U ("User input requested but no handler registered");
         return "";
      end if;

      Request.Question       := U (String_Member (Params, "question"));
      Request.Session_Id     := U (String_Member (Params, "sessionId"));
      Request.Choices_Json   := U (Member (Params, "choices"));
      Request.Allow_Freeform := Member (Params, "allowFreeform") /= "false";
      Request.Request_Id     := U (String_Member (Params, "requestId"));
      Request.Meta_Json      := U (Member (Params, "_meta"));
      if Constraints'Length > 0 and then Constraints (Constraints'First) = '{'
      then
         Request.Image_Attachment_Constraints :=
           (Present                 => True,
            Allowed_Mime_Types_Json =>
              U (Member (Constraints, "allowedMimeTypes")),
            Max_Bytes_Per_Image     =>
              Natural_Member (Constraints, "maxBytesPerImage"),
            Max_Count               =>
              Natural_Member (Constraints, "maxCount"));
      end if;

      if Callbacks.User_Input_Image /= null then
         return User_Input_Reply (Callbacks.User_Input_Image.all (Request));
      end if;

      declare
         Text_Reply : constant User_Input_Response :=
           Callbacks.User_Input.all (Request);
      begin
         return User_Input_Reply
           ((Answer       => Text_Reply.Answer,
             Was_Freeform => Text_Reply.Was_Freeform,
             Attachments  => <>));
      end;
   end Ask_User;

   function Network_Reply (Method : String; Params : String) return String is
   begin
      if Method = "network.resolveProxy"
        and then Network_Callbacks.Resolve_Proxy /= null
      then
         declare
            Proxy : constant String :=
              Network_Callbacks.Resolve_Proxy.all (String_Member (Params, "url"));
         begin
            if Proxy'Length > 0 then
               return "{""proxy"":" & Quote (Proxy) & "}";
            end if;
         end;

      elsif Method = "network.authenticateNegotiate"
        and then Network_Callbacks.Authenticate_Negotiate /= null
      then
         declare
            Token : constant String :=
              Network_Callbacks.Authenticate_Negotiate.all
                (String_Member (Params, "spn"));
         begin
            if Token'Length > 0 then
               return "{""token"":" & Quote (Token) & "}";
            end if;
         end;

      elsif Method = "network.authenticateBasic"
        and then Network_Callbacks.Authenticate_Basic /= null
      then
         declare
            Credentials : constant Basic_Credentials :=
              Network_Callbacks.Authenticate_Basic.all
                (String_Member (Params, "proxy"),
                 String_Member (Params, "realm"),
                 Bool_Member (Params, "previousRejected"));
         begin
            if Length (Credentials.Username) > 0 then
               return "{""username"":"
                 & Quote (To_String (Credentials.Username))
                 & ",""password"":"
                 & Quote (To_String (Credentials.Password)) & "}";
            end if;
         end;
      end if;

      --  Declined: let the runtime fall back to its default behavior.
      return "{}";
   end Network_Reply;

   --  -------------------------------------------------------------------
   --  Dispatch
   --  -------------------------------------------------------------------

   procedure Dispatch
     (Method  : String;
      Params  : String;
      Result  : out Unbounded_String;
      Error   : out Unbounded_String;
      Handled : out Boolean)
   is
      Session_Id : constant String := String_Member (Params, "sessionId");
      Callbacks  : Session_Callbacks;
   begin
      Result  := Null_Unbounded_String;
      Error   := Null_Unbounded_String;
      Handled := True;

      if Sessions.Contains (Session_Id) then
         Callbacks := Sessions.Element (Session_Id);
      end if;

      if Method = "gitHubToken.getToken" then
         Result := U (Get_GitHub_Token (Params, Error));

      elsif Method = "shellAuthentication.request" then
         Result := U (Authenticate_Shell (Params, Callbacks));

      elsif Method = "shellAuthentication.cancel" then
         --  Prompts are answered synchronously, so nothing is pending.
         Result := U ("{}");

      elsif Method = "memoryStore.store" then
         if Callbacks.Store_Memory = null then
            Error := U ("Memory store owner is unavailable");
         else
            Callbacks.Store_Memory.all
              ((Session_Id     => U (Session_Id),
                Subject        => U (String_Member (Params, "subject")),
                Fact           => U (String_Member (Params, "fact")),
                Citations_Json => U (Member (Params, "citations")),
                Reason         => U (String_Member (Params, "reason"))));
            Result := U ("null");
         end if;

      elsif Method = "hooks.invoke" then
         Result := U (Invoke_Hook (Params, Callbacks));

      elsif Method = "userInput.request" then
         Result := U (Ask_User (Params, Callbacks, Error));

      elsif Method = "network.resolveProxy"
        or else Method = "network.authenticateNegotiate"
        or else Method = "network.authenticateBasic"
      then
         Result := U (Network_Reply (Method, Params));

      else
         Handled := False;
      end if;
   end Dispatch;

end Copilot.Callbacks;
