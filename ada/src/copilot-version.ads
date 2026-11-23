--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Version -- SDK and protocol version constants.
--

package Copilot.Version is

   SDK_Version      : constant String := "2.0.2";
   --  SDK protocol version spoken with the Copilot CLI (CLI 1.0.113 or newer).
   Protocol_Version : constant Natural := 4;
   --  The startup handshake accepts exactly Min_Protocol .. Max_Protocol.
   Min_Protocol     : constant Natural := 4;
   Max_Protocol     : constant Natural := 4;

end Copilot.Version;
