--  ---------------------------------------------------------------------------
--  basic_example.adb -- Minimal example of using the Copilot Ada SDK.
--  ---------------------------------------------------------------------------
--
--  Build:
--    gprbuild -P ../copilot_sdk.gpr
--    gnatmake basic_example.adb -I../src -L../lib -lcopilot_sdk
--
--  Or with Alire:
--    alr exec -- gnatmake basic_example.adb
--

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Types;          use Copilot.Types;

procedure Basic_Example is
   --  Create a client with default options (spawns CLI via stdio).
   Client : Copilot_Client := Create;

   --  Session handle.
   Sess : Copilot_Session;

   --  Message options.
   Opts : Message_Options;

   --  Response event.
   Resp : Session_Event;
begin
   Put_Line ("Starting Copilot client...");
   Start (Client);
   Put_Line ("Connected.");

   --  Optionally verify the server is alive.
   declare
      Echo : constant String := Ping (Client, "hello from Ada!");
   begin
      Put_Line ("Ping response: " & Echo);
   end;

   --  Create a session (uses default config: streaming, no system prompt).
   Sess := Create_Session (Client);
   Put_Line ("Session created: " & Session_Id (Sess));

   --  Send a message and wait for the assistant response.
   Opts.Prompt := To_Unbounded_String ("What is the capital of France?");
   Resp := Send_And_Wait (Sess, Opts);

   Put_Line ("Assistant: " & To_String (Resp.Content));

   --  Send a follow-up.
   Opts.Prompt := To_Unbounded_String ("What about Germany?");
   Resp := Send_And_Wait (Sess, Opts);
   Put_Line ("Assistant: " & To_String (Resp.Content));

   --  Cleanup.
   Destroy (Sess);
   Put_Line ("Session destroyed.");

   Stop (Client);
   Put_Line ("Client stopped.");

exception
   when Connection_Error =>
      Put_Line ("ERROR: Could not connect to the Copilot CLI.");
      Put_Line ("Make sure the CLI is installed or set COPILOT_CLI_PATH.");
   when Protocol_Error =>
      Put_Line ("ERROR: Protocol mismatch with the CLI server.");
   when others =>
      Put_Line ("ERROR: Unexpected exception.");
      Stop (Client);
end Basic_Example;
