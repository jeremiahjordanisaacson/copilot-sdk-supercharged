# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Ada.

## Two Independent Sessions

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Types;          use Copilot.Types;

procedure Two_Sessions is
   Client : Copilot_Client := Create;
   Code_Session : Copilot_Session;
   Doc_Session  : Copilot_Session;
   Opts : Message_Options;
begin
   Start (Client);

   --  Create two sessions with different system prompts.
   Code_Session := Create_Session (Client,
     (System_Prompt => To_Unbounded_String ("You are a code reviewer."),
      others        => <>));

   Doc_Session := Create_Session (Client,
     (System_Prompt => To_Unbounded_String ("You are a technical writer."),
      others        => <>));

   --  Use each independently.
   Opts.Prompt := To_Unbounded_String ("Review this function...");
   declare
      R1 : constant Session_Event := Send_And_Wait (Code_Session, Opts);
   begin
      Put_Line ("Code review: " & To_String (R1.Content));
   end;

   Opts.Prompt := To_Unbounded_String ("Write docs for this module...");
   declare
      R2 : constant Session_Event := Send_And_Wait (Doc_Session, Opts);
   begin
      Put_Line ("Documentation: " & To_String (R2.Content));
   end;

   --  Cleanup both.
   Destroy (Code_Session);
   Destroy (Doc_Session);
   Stop (Client);
end Two_Sessions;
```

## Session Pool Pattern

Use an array to manage a pool of sessions.

```ada
Pool_Size : constant := 4;
Pool : array (1 .. Pool_Size) of Copilot_Session;

for I in Pool'Range loop
   Pool (I) := Create_Session (Client);
end loop;

--  Distribute work across sessions.
for I in Pool'Range loop
   declare
      Opts : Message_Options;
   begin
      Opts.Prompt := To_Unbounded_String (
        "Task " & Positive'Image (I));
      Send (Pool (I), Opts);
   end;
end loop;

--  Cleanup.
for I in Pool'Range loop
   Destroy (Pool (I));
end loop;
```

## Concurrent Sessions with Tasks

Use Ada tasks for true parallelism.

```ada
task type Session_Worker (Id : Positive) is
   entry Start_Work (Prompt : String);
   entry Get_Result (Result : out Unbounded_String);
end Session_Worker;

task body Session_Worker is
   Client : Copilot_Client := Create;
   Sess   : Copilot_Session;
   Output : Unbounded_String;
begin
   Start (Client);
   Sess := Create_Session (Client);

   accept Start_Work (Prompt : String) do
      declare
         Opts : Message_Options;
      begin
         Opts.Prompt := To_Unbounded_String (Prompt);
         declare
            Resp : constant Session_Event :=
              Send_And_Wait (Sess, Opts);
         begin
            Output := Resp.Content;
         end;
      end;
   end Start_Work;

   accept Get_Result (Result : out Unbounded_String) do
      Result := Output;
   end Get_Result;

   Destroy (Sess);
   Stop (Client);
end Session_Worker;
```
