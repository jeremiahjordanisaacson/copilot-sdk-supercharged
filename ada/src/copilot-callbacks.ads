--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Callbacks -- Host callbacks answered through server-to-client
--  JSON-RPC requests (SDK protocol 4).
--
--  The client registers session callbacks (shell authentication, memory
--  store, sub-agent hooks, user input), GitHub token providers and network
--  callbacks here, and installs Dispatch as the connection's
--  Server_Request_Handler.  Requests handled:
--
--    gitHubToken.getToken        -> GitHub token provider
--    shellAuthentication.request -> shell authentication handler
--    shellAuthentication.cancel  -> acknowledged with {}
--    memoryStore.store           -> memory store handler
--    hooks.invoke                -> subagentStart / subagentStop hooks
--    userInput.request           -> user input (ask_user) handler
--    network.resolveProxy        -> proxy resolver
--    network.authenticateNegotiate / network.authenticateBasic
--
--  Tokens and passwords are never logged.
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Copilot.Types;         use Copilot.Types;

package Copilot.Callbacks is

   --  Register the callbacks of a session (null handlers are ignored).
   --  User_Input_Image takes precedence over User_Input when both are set.
   procedure Register_Session
     (Session_Id        : String;
      Shell_Auth        : Shell_Authentication_Handler_Access;
      Store_Memory      : Memory_Store_Handler_Access;
      On_Subagent_Start : Subagent_Start_Hook_Access;
      On_Subagent_Stop  : Subagent_Stop_Hook_Access;
      User_Input        : User_Input_Handler := null;
      User_Input_Image  : User_Input_Image_Handler := null);

   procedure Unregister_Session (Session_Id : String);

   --  Replace the user input (ask_user) handler of a session.  Handler
   --  answers from now on: it also replaces an image handler registered with
   --  the session.
   procedure Set_User_Input_Handler
     (Session_Id : String;
      Handler    : User_Input_Handler);

   --  GitHub token providers are keyed by their registration id.
   procedure Register_Token_Provider
     (Registration_Id : String;
      Provider        : GitHub_Token_Provider_Access);

   procedure Unregister_Token_Provider (Registration_Id : String);

   --  Connection-level network callbacks.
   procedure Set_Network (Options : Network_Options);

   --  True when at least one network callback is set.
   function Has_Network_Callbacks (Options : Network_Options) return Boolean;

   --  Server_Request_Handler for Copilot.JSON_RPC connections.
   procedure Dispatch
     (Method  : String;
      Params  : String;
      Result  : out Unbounded_String;
      Error   : out Unbounded_String;
      Handled : out Boolean);

end Copilot.Callbacks;
