# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Ada.

## Basic Error Handling

Use Ada exceptions and begin/exception blocks for clean error propagation.

```ada
with Ada.Text_IO;       use Ada.Text_IO;
with Copilot.Client;    use Copilot.Client;
with Copilot.Session;   use Copilot.Session;
with Copilot.Types;     use Copilot.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Safe_Chat is
   Client : Copilot_Client := Create;
   Sess   : Copilot_Session;
   Opts   : Message_Options;
begin
   Start (Client);
   Sess := Create_Session (Client);

   Opts.Prompt := To_Unbounded_String ("Explain Ada exceptions");
   declare
      Resp : constant Session_Event := Send_And_Wait (Sess, Opts);
   begin
      Put_Line (To_String (Resp.Content));
   end;

   Destroy (Sess);
   Stop (Client);
exception
   when Connection_Error =>
      Put_Line ("Could not connect to the Copilot CLI.");
   when Protocol_Error =>
      Put_Line ("Protocol version mismatch.");
   when Timeout_Error =>
      Put_Line ("Request timed out.");
   when others =>
      Put_Line ("Unexpected error.");
end Safe_Chat;
```

## Handling Specific Error Types

Match on different exception types for targeted recovery.

```ada
begin
   Start (Client);
exception
   when E : Connection_Error =>
      Put_Line ("Connection failed. Is the CLI installed?");
      --  Retry logic, alternative path, etc.
   when E : Protocol_Error =>
      Put_Line ("Incompatible CLI version. Please update.");
   when E : Authentication_Error =>
      Put_Line ("Authentication failed. Check your token.");
end;
```

## Cleanup with Controlled Types

Use Ada controlled types for RAII-style cleanup that runs even on exceptions.

```ada
with Ada.Finalization; use Ada.Finalization;

type Safe_Client is new Limited_Controlled with record
   Inner : Copilot_Client;
end record;

overriding procedure Finalize (Self : in out Safe_Client) is
begin
   if Is_Connected (Self.Inner) then
      Stop (Self.Inner);
   end if;
end Finalize;
```

## Retry on Connection Loss

```ada
Max_Retries : constant := 3;

for Attempt in 1 .. Max_Retries loop
   begin
      Start (Client);
      exit;  --  success
   exception
      when Connection_Error =>
         if Attempt = Max_Retries then
            raise;
         end if;
         delay 1.0 * Duration (Attempt);  --  exponential backoff
   end;
end loop;
```

## Timeout Handling

```ada
declare
   Opts : Message_Options;
begin
   Opts.Prompt := To_Unbounded_String ("Complex analysis...");
   declare
      Resp : constant Session_Event :=
        Send_And_Wait (Sess, Opts, Timeout => 120.0);
   begin
      Put_Line (To_String (Resp.Content));
   end;
exception
   when Timeout_Error =>
      Put_Line ("Operation timed out after 120 seconds.");
      Abort_Turn (Sess);
end;
```
