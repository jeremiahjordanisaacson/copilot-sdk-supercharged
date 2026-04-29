# Persisting Sessions

Save and resume sessions across application restarts using the GitHub Copilot SDK in Ada.

## Save and Resume a Session

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Types;          use Copilot.Types;

procedure Persist_Session is
   Client    : Copilot_Client := Create;
   Sess      : Copilot_Session;
   Saved_Id  : Unbounded_String;
begin
   Start (Client);

   --  Create a new session and do some work.
   Sess := Create_Session (Client);
   Saved_Id := To_Unbounded_String (Session_Id (Sess));

   declare
      Opts : Message_Options;
   begin
      Opts.Prompt := To_Unbounded_String ("Remember: the project uses Ada 2012.");
      declare
         Ignore : constant Session_Event := Send_And_Wait (Sess, Opts);
      begin
         null;
      end;
   end;

   --  Save the session ID for later.
   Put_Line ("Session ID to persist: " & To_String (Saved_Id));

   --  Stop the client (session state is kept server-side).
   Stop (Client);

   --  ... later, in a new run ...

   declare
      Client2 : Copilot_Client := Create;
      Resumed : Copilot_Session;
      Cfg     : Resume_Session_Config;
   begin
      Start (Client2);

      Cfg.Session_Id := Saved_Id;
      Resumed := Resume_Session (Client2, Cfg);

      Put_Line ("Resumed session: " & Session_Id (Resumed));

      --  Continue the conversation; context is preserved.
      declare
         Opts : Message_Options;
      begin
         Opts.Prompt := To_Unbounded_String ("What language does this project use?");
         declare
            Resp : constant Session_Event := Send_And_Wait (Resumed, Opts);
         begin
            Put_Line ("Answer: " & To_String (Resp.Content));
         end;
      end;

      Destroy (Resumed);
      Stop (Client2);
   end;
end Persist_Session;
```

## Persisting to a File

```ada
with Ada.Sequential_IO;

--  Save
declare
   package Char_IO is new Ada.Sequential_IO (Character);
   use Char_IO;
   F : File_Type;
   Id : constant String := Session_Id (Sess);
begin
   Create (F, Out_File, "session.id");
   for C of Id loop
      Write (F, C);
   end loop;
   Close (F);
end;

--  Load
declare
   package Char_IO is new Ada.Sequential_IO (Character);
   use Char_IO;
   F      : File_Type;
   C      : Character;
   Result : Unbounded_String;
begin
   Open (F, In_File, "session.id");
   while not End_Of_File (F) loop
      Read (F, C);
      Append (Result, C);
   end loop;
   Close (F);
   --  Result now holds the session ID
end;
```

## Session Metadata

Check session status before resuming.

```ada
declare
   Meta : constant Session_Metadata :=
     Get_Session_Metadata (Client, To_String (Saved_Id));
begin
   Put_Line ("Status: " & To_String (Meta.Status));
   Put_Line ("Turns:  " & Natural'Image (Meta.Turn_Count));
end;
```
