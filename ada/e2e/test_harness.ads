--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Test_Harness -- Manages the replaying CAPI proxy for E2E tests.
--
--  The proxy is the shared Node.js test harness from test/harness/server.ts.
--  It replays stored YAML snapshot exchanges so that E2E tests run without
--  hitting the real Copilot API.
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Test_Harness is

   type Capi_Proxy is tagged private;

   procedure Start (Self : in out Capi_Proxy);
   --  Spawns the replay proxy server (npx tsx .../test/harness/server.ts)
   --  and blocks until it prints "Listening: http://..." on stdout.
   --  Sets COPILOT_API_URL in the current process environment.

   procedure Stop (Self : in out Capi_Proxy; Skip_Cache : Boolean := False);
   --  Sends POST /stop to the proxy (optionally with ?skipWritingCache=true)
   --  and waits for the child process to exit.

   procedure Configure
     (Self      : in out Capi_Proxy;
      File_Path : String;
      Work_Dir  : String);
   --  Sends POST /config with the snapshot file path and working directory
   --  so the proxy knows which YAML exchanges to replay for the next test.

   function Proxy_Url (Self : Capi_Proxy) return String;
   --  Returns the base URL of the running proxy (e.g. "http://127.0.0.1:PORT").
   --  Returns "" if the proxy has not been started.

   function Cli_Path return String;
   --  Copilot CLI entry point for the E2E tests, in lookup order:
   --    1. COPILOT_CLI_PATH when set and the file exists (CI points it at
   --       nodejs/node_modules/.bin/copilot, which runs directly);
   --    2. nodejs/node_modules/@github/copilot/npm-loader.js (CLI 1.0.113+,
   --       run with node by the SDK because of the .js suffix);
   --    3. legacy nodejs/node_modules/@github/copilot-*/index.js and
   --       nodejs/node_modules/@github/copilot/index.js.
   --  Returns "" when nothing is found.

private

   type Capi_Proxy is tagged record
      Url     : Unbounded_String := Null_Unbounded_String;
      Running : Boolean := False;
      Pid     : Integer := -1;
   end record;

end Test_Harness;
