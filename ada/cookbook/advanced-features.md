# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications.

```ada
with Copilot.Client;  use Copilot.Client;
with Copilot.Session; use Copilot.Session;
with Copilot.Types;   use Copilot.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

declare
   Client : Copilot_Client := Create;
   Cfg    : Session_Config := Default_Session_Config;
begin
   Start (Client);

   Cfg.Github_Token := To_Unbounded_String ("ghu_tenant_abc_token_here");
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
   begin
      --  This session authenticates as a specific tenant user.
      Destroy (Sess);
   end;

   Stop (Client);
end;
```

## Session Idle Timeout

Auto-cleanup inactive sessions after a configurable duration.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Session_Idle_Timeout := 600;  --  10 minutes
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
   begin
      --  Session automatically closes after 600 seconds of inactivity.
      null;
   end;
end;
```

## Custom System Prompt

Customize the agent's behavior with a system prompt.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.System_Prompt := To_Unbounded_String (
     "You are an Ada programming expert. "
     & "Always provide Ada 2012 code examples. "
     & "Prefer tagged types over plain records.");
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
   begin
      --  The agent now behaves as an Ada expert.
      null;
   end;
end;
```

## Model Selection

Choose a specific model for a session.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Model := To_Unbounded_String ("gpt-4o");
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
   begin
      null;
   end;
end;
```

## Non-Streaming Mode

Disable streaming to receive a single complete response instead of deltas.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Streaming := False;
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
      Opts : Message_Options;
   begin
      Opts.Prompt := To_Unbounded_String ("Summarize Ada concurrency.");
      declare
         Resp : constant Session_Event := Send_And_Wait (Sess, Opts);
      begin
         Put_Line (To_String (Resp.Content));
      end;
   end;
end;
```

## Server Status Check

Verify the CLI server is alive and compatible.

```ada
declare
   Status : constant Get_Status_Response := Get_Status (Client);
begin
   Put_Line ("CLI version: " & To_String (Status.Version));
   Put_Line ("Protocol:    " & Natural'Image (Status.Protocol_Version));
end;
```

## Authentication Status

Check whether the user is authenticated.

```ada
declare
   Auth : constant Auth_Status_Response := Get_Auth_Status (Client);
begin
   if Auth.Authenticated then
      Put_Line ("Logged in as: " & To_String (Auth.User));
   else
      Put_Line ("Not authenticated.");
   end if;
end;
```

## Workspace Scoping

Scope a session to a specific directory.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Workspace_Path := To_Unbounded_String ("/home/user/project");
   declare
      Sess : Copilot_Session := Create_Session (Client, Cfg);
   begin
      Put_Line ("Workspace: " & Workspace_Path (Sess));
   end;
end;
```

## Graceful Shutdown Pattern

```ada
declare
   Client : Copilot_Client := Create;
begin
   Start (Client);
   --  ... do work ...
   Stop (Client);
exception
   when others =>
      --  Ensure cleanup even on unexpected errors.
      if Is_Connected (Client) then
         Force_Stop (Client);
      end if;
end;
```
