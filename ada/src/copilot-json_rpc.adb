--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Copilot.Types;              use Copilot.Types;

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
              Closed   => False);
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

   function Send_Request
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}") return String
   is
      Req_Id  : constant Positive := Self.Next_Id;
      Payload : constant String := Build_Request (Req_Id, Method, Params);
   begin
      Self.Next_Id := Self.Next_Id + 1;
      Write_Message (Self, Payload);

      --  Read responses until we get one matching our request id.
      loop
         declare
            Resp : constant String := Read_Message (Self);
         begin
            --  Simple check: look for our id in the response.
            if Index (Resp, """id"":" & Int_Image (Req_Id)) > 0 then
               return Resp;
            end if;
            --  Otherwise it is a notification/event; skip it for now.
         end;
      end loop;
   end Send_Request;

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
