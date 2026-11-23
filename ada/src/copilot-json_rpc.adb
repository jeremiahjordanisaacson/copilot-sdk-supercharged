--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Copilot.Types;              use Copilot.Types;
with Copilot.Wire;

package body Copilot.JSON_RPC is

   CRLF : constant String :=
     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   --  -------------------------------------------------------------------
   --  Internal helpers
   --  -------------------------------------------------------------------

   function Int_Image (V : Natural) return String is
      Img : constant String := Natural'Image (V);
   begin
      --  Natural'Image has a leading space; strip it.
      return Img (Img'First + 1 .. Img'Last);
   end Int_Image;

   --  Build a minimal JSON-RPC 2.0 request envelope.
   function Build_Request
     (Id     : Positive;
      Method : String;
      Params : String) return String
   is
   begin
      return "{""jsonrpc"":""2.0"","
           & """id"":" & Int_Image (Id) & ","
           & """method"":""" & Method & ""","
           & """params"":" & Params & "}";
   end Build_Request;

   --  Build a JSON-RPC 2.0 notification (no id field).
   function Build_Notification
     (Method : String;
      Params : String) return String
   is
   begin
      return "{""jsonrpc"":""2.0"","
           & """method"":""" & Method & ""","
           & """params"":" & Params & "}";
   end Build_Notification;

   --  Read exactly N bytes from a file descriptor.
   procedure Read_Exact
     (FD   : File_Descriptor;
      Buf  : out String;
      Last : out Natural)
   is
      Offset : Natural := 0;
      N      : Integer;
   begin
      Last := 0;
      while Offset < Buf'Length loop
         N := Read (FD, Buf (Buf'First + Offset)'Address,
                    Buf'Length - Offset);
         if N <= 0 then
            raise Protocol_Error with "Connection closed during read";
         end if;
         Offset := Offset + N;
      end loop;
      Last := Buf'Length;
   end Read_Exact;

   --  Read a single byte.
   function Read_Byte (FD : File_Descriptor) return Character is
      Buf  : String (1 .. 1);
      Last : Natural;
   begin
      Read_Exact (FD, Buf, Last);
      return Buf (1);
   end Read_Byte;

   --  Read bytes until we see CRLF CRLF (end of headers).
   function Read_Headers (FD : File_Descriptor) return String is
      Result : Unbounded_String := Null_Unbounded_String;
      C      : Character;
   begin
      loop
         C := Read_Byte (FD);
         Append (Result, C);
         declare
            S : constant String := To_String (Result);
         begin
            if S'Length >= 4 and then
              S (S'Last - 3 .. S'Last) = CRLF & CRLF
            then
               return S;
            end if;
         end;
      end loop;
   end Read_Headers;

   --  Parse Content-Length from the header block.
   function Parse_Content_Length (Headers : String) return Natural is
      Key : constant String := "Content-Length:";
      Pos : constant Natural := Index (Headers, Key);
   begin
      if Pos = 0 then
         raise Protocol_Error with "Missing Content-Length header";
      end if;
      declare
         After  : constant Natural := Pos + Key'Length;
         Line_End : Natural := Index (Headers, CRLF, After);
         Value  : constant String :=
           Trim (Headers (After .. Line_End - 1), Ada.Strings.Both);
      begin
         return Natural'Value (Value);
      end;
   end Parse_Content_Length;

   --  -------------------------------------------------------------------
   --  Public API
   --  -------------------------------------------------------------------

   function Create
     (Read_FD  : File_Descriptor;
      Write_FD : File_Descriptor) return RPC_Connection
   is
   begin
      return (Read_FD  => Read_FD,
              Write_FD => Write_FD,
              Next_Id  => 1,
              Closed   => False,
              Handler  => null,
              Queue    => Message_Queues.Empty_Vector,
              Last_Err => Null_Unbounded_String);
   end Create;

   procedure Write_Message
     (Self    : in out RPC_Connection;
      Payload : String)
   is
      Header : constant String :=
        "Content-Length: " & Int_Image (Payload'Length) & CRLF & CRLF;
      Full   : constant String := Header & Payload;
      Written : Integer;
   begin
      Written := Write (Self.Write_FD, Full'Address, Full'Length);
      if Written /= Full'Length then
         raise Protocol_Error with "Incomplete write";
      end if;
   end Write_Message;

   function Read_Message (Self : in out RPC_Connection) return String is
      Headers : constant String := Read_Headers (Self.Read_FD);
      Length  : constant Natural := Parse_Content_Length (Headers);
      Body_Buf : String (1 .. Length);
      Last     : Natural;
   begin
      Read_Exact (Self.Read_FD, Body_Buf, Last);
      return Body_Buf;
   end Read_Message;

   --  Answer a server-to-client request through the registered handler.
   procedure Answer_Request
     (Self   : in out RPC_Connection;
      Id_Raw : String;
      Method : String;
      Params : String)
   is
      Result  : Unbounded_String;
      Error   : Unbounded_String;
      Handled : Boolean := False;
   begin
      if Self.Handler /= null then
         begin
            Self.Handler (Method, Params, Result, Error, Handled);
         exception
            when E : others =>
               Handled := True;
               Error := To_Unbounded_String
                 (Ada.Exceptions.Exception_Message (E));
         end;
      end if;

      if not Handled then
         Write_Message
           (Self,
            "{""jsonrpc"":""2.0"",""id"":" & Id_Raw
            & ",""error"":{""code"":-32601,""message"":"
            & Copilot.Wire.Quote ("Method not found: " & Method) & "}}");
      elsif Length (Error) > 0 then
         Write_Message
           (Self,
            "{""jsonrpc"":""2.0"",""id"":" & Id_Raw
            & ",""error"":{""code"":-32603,""message"":"
            & Copilot.Wire.Quote (To_String (Error)) & "}}");
      elsif Length (Result) = 0 then
         Write_Message
           (Self, "{""jsonrpc"":""2.0"",""id"":" & Id_Raw & ",""result"":null}");
      else
         Write_Message
           (Self,
            "{""jsonrpc"":""2.0"",""id"":" & Id_Raw
            & ",""result"":" & To_String (Result) & "}");
      end if;
   end Answer_Request;

   function Send_Request
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}") return String
   is
      Req_Id  : constant Positive := Self.Next_Id;
      Payload : constant String := Build_Request (Req_Id, Method, Params);
   begin
      Self.Next_Id := Self.Next_Id + 1;
      Self.Last_Err := Null_Unbounded_String;
      Write_Message (Self, Payload);

      --  Read messages until the response matching our request id arrives.
      loop
         declare
            Msg        : constant String := Read_Message (Self);
            Msg_Method : constant String :=
              Copilot.Wire.String_Member (Msg, "method");
            Id_Raw     : constant String := Copilot.Wire.Member (Msg, "id");
         begin
            if Msg_Method'Length > 0 then
               if Id_Raw'Length > 0 then
                  --  Server-to-client request (hooks, token providers, ...).
                  Answer_Request
                    (Self, Id_Raw, Msg_Method,
                     Copilot.Wire.Member (Msg, "params"));
               else
                  --  Notification: keep it for Read_Notification.
                  Self.Queue.Append (Msg);
               end if;
            elsif Id_Raw = Int_Image (Req_Id) then
               declare
                  Err : constant String := Copilot.Wire.Member (Msg, "error");
               begin
                  if Err'Length > 0 then
                     Self.Last_Err := To_Unbounded_String (Err);
                     raise Protocol_Error
                       with "JSON-RPC error for " & Method & ": "
                          & Copilot.Wire.String_Member (Err, "message")
                          & " (code " & Copilot.Wire.Member (Err, "code")
                          & ")";
                  end if;
               end;
               return Msg;
            end if;
         end;
      end loop;
   end Send_Request;

   procedure Set_Server_Request_Handler
     (Self    : in out RPC_Connection;
      Handler : Server_Request_Handler) is
   begin
      Self.Handler := Handler;
   end Set_Server_Request_Handler;

   function Read_Notification (Self : in out RPC_Connection) return String is
   begin
      if not Self.Queue.Is_Empty then
         declare
            Msg : constant String := Self.Queue.First_Element;
         begin
            Self.Queue.Delete_First;
            return Msg;
         end;
      end if;

      loop
         declare
            Msg        : constant String := Read_Message (Self);
            Msg_Method : constant String :=
              Copilot.Wire.String_Member (Msg, "method");
            Id_Raw     : constant String := Copilot.Wire.Member (Msg, "id");
         begin
            if Msg_Method'Length > 0 then
               if Id_Raw'Length = 0 then
                  return Msg;
               end if;
               Answer_Request
                 (Self, Id_Raw, Msg_Method,
                  Copilot.Wire.Member (Msg, "params"));
            end if;
            --  Stray responses (no pending request) are ignored.
         end;
      end loop;
   end Read_Notification;

   function Last_Error (Self : RPC_Connection) return String is
   begin
      return To_String (Self.Last_Err);
   end Last_Error;

   procedure Send_Notification
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}")
   is
      Payload : constant String := Build_Notification (Method, Params);
   begin
      Write_Message (Self, Payload);
   end Send_Notification;

   procedure Close (Self : in out RPC_Connection) is
   begin
      if not Self.Closed then
         GNAT.OS_Lib.Close (Self.Read_FD);
         if Self.Write_FD /= Self.Read_FD then
            GNAT.OS_Lib.Close (Self.Write_FD);
         end if;
         Self.Closed := True;
      end if;
   end Close;

   function Is_Closed (Self : RPC_Connection) return Boolean is
   begin
      return Self.Closed;
   end Is_Closed;

end Copilot.JSON_RPC;
