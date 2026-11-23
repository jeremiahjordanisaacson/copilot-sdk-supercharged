--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;

package body Copilot.Wire is

   package L1 renames Ada.Characters.Latin_1;

   Hex_Digits : constant String := "0123456789abcdef";

   --  -------------------------------------------------------------------
   --  Scanning helpers
   --  -------------------------------------------------------------------

   function Is_Blank (C : Character) return Boolean is
     (C = ' ' or else C = L1.HT or else C = L1.LF or else C = L1.CR);

   --  Index of the first non-blank character at or after From
   --  (S'Last + 1 when there is none).
   function Skip_Blanks (S : String; From : Positive) return Positive is
      I : Positive := From;
   begin
      while I <= S'Last and then Is_Blank (S (I)) loop
         I := I + 1;
      end loop;
      return I;
   end Skip_Blanks;

   --  Index of the last character of the JSON value starting at From.
   function Value_End (S : String; From : Positive) return Natural is
      Depth     : Natural := 0;
      In_String : Boolean := False;
      I         : Positive := From;
   begin
      if From > S'Last then
         return From - 1;
      end if;

      if S (From) = '"' then
         I := From + 1;
         while I <= S'Last loop
            if S (I) = '\' then
               I := I + 2;
            elsif S (I) = '"' then
               return I;
            else
               I := I + 1;
            end if;
         end loop;
         return S'Last;

      elsif S (From) = '{' or else S (From) = '[' then
         while I <= S'Last loop
            if In_String then
               if S (I) = '\' then
                  I := I + 1;
               elsif S (I) = '"' then
                  In_String := False;
               end if;
            elsif S (I) = '"' then
               In_String := True;
            elsif S (I) = '{' or else S (I) = '[' then
               Depth := Depth + 1;
            elsif S (I) = '}' or else S (I) = ']' then
               Depth := Depth - 1;
               if Depth = 0 then
                  return I;
               end if;
            end if;
            I := I + 1;
         end loop;
         return S'Last;

      else
         --  Numbers and the literals true / false / null.
         while I <= S'Last
           and then S (I) /= ','
           and then S (I) /= '}'
           and then S (I) /= ']'
           and then not Is_Blank (S (I))
         loop
            I := I + 1;
         end loop;
         return I - 1;
      end if;
   end Value_End;

   --  Call Process for every top-level member of the JSON object Obj.
   --  Key is the raw (still escaped) key text, Value the raw JSON value.
   procedure For_Each_Member
     (Obj     : String;
      Process : not null access procedure (Key : String; Value : String))
   is
      I           : Positive;
      Key_First   : Positive;
      Key_Last    : Natural;
      Value_First : Positive;
      Value_Last  : Natural;
   begin
      if Obj'Length = 0 then
         return;
      end if;
      I := Skip_Blanks (Obj, Obj'First);
      if I > Obj'Last or else Obj (I) /= '{' then
         return;
      end if;
      I := I + 1;

      loop
         I := Skip_Blanks (Obj, I);
         if I <= Obj'Last and then Obj (I) = ',' then
            I := Skip_Blanks (Obj, I + 1);
         end if;
         exit when I > Obj'Last or else Obj (I) /= '"';

         Key_First := I + 1;
         Key_Last  := Value_End (Obj, I) - 1;
         I := Skip_Blanks (Obj, Key_Last + 2);
         exit when I > Obj'Last or else Obj (I) /= ':';

         Value_First := Skip_Blanks (Obj, I + 1);
         Value_Last  := Value_End (Obj, Value_First);
         exit when Value_Last < Value_First;

         Process (Obj (Key_First .. Key_Last),
                  Obj (Value_First .. Value_Last));
         I := Value_Last + 1;
      end loop;
   end For_Each_Member;

   procedure Append_Utf8 (R : in out Unbounded_String; Code : Natural) is
   begin
      if Code < 16#80# then
         Append (R, Character'Val (Code));
      elsif Code < 16#800# then
         Append (R, Character'Val (16#C0# + Code / 64));
         Append (R, Character'Val (16#80# + Code mod 64));
      else
         Append (R, Character'Val (16#E0# + Code / 4096));
         Append (R, Character'Val (16#80# + (Code / 64) mod 64));
         Append (R, Character'Val (16#80# + Code mod 64));
      end if;
   end Append_Utf8;

   --  Unescape a raw JSON string literal (including its quotes).
   function Unescape (Raw : String) return String is
      R : Unbounded_String;
      I : Positive;
   begin
      if Raw'Length < 2 or else Raw (Raw'First) /= '"' then
         return "";
      end if;
      I := Raw'First + 1;
      while I < Raw'Last loop
         if Raw (I) = '\' and then I + 1 < Raw'Last then
            case Raw (I + 1) is
               when 'n' =>
                  Append (R, L1.LF);
                  I := I + 2;
               when 't' =>
                  Append (R, L1.HT);
                  I := I + 2;
               when 'r' =>
                  Append (R, L1.CR);
                  I := I + 2;
               when 'b' =>
                  Append (R, L1.BS);
                  I := I + 2;
               when 'f' =>
                  Append (R, L1.FF);
                  I := I + 2;
               when 'u' =>
                  declare
                     Code  : Natural := 0;
                     Valid : Boolean := I + 5 < Raw'Last;
                  begin
                     if Valid then
                        for J in I + 2 .. I + 5 loop
                           case Raw (J) is
                              when '0' .. '9' =>
                                 Code := Code * 16 + Character'Pos (Raw (J))
                                   - Character'Pos ('0');
                              when 'a' .. 'f' =>
                                 Code := Code * 16 + Character'Pos (Raw (J))
                                   - Character'Pos ('a') + 10;
                              when 'A' .. 'F' =>
                                 Code := Code * 16 + Character'Pos (Raw (J))
                                   - Character'Pos ('A') + 10;
                              when others =>
                                 Valid := False;
                           end case;
                        end loop;
                     end if;
                     if Valid then
                        Append_Utf8 (R, Code);
                        I := I + 6;
                     else
                        Append (R, 'u');
                        I := I + 2;
                     end if;
                  end;
               when others =>
                  Append (R, Raw (I + 1));
                  I := I + 2;
            end case;
         else
            Append (R, Raw (I));
            I := I + 1;
         end if;
      end loop;
      return To_String (R);
   end Unescape;

   --  -------------------------------------------------------------------
   --  Public JSON helpers
   --  -------------------------------------------------------------------

   function Escape (S : String) return String is
      R : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"'   => Append (R, "\""");
            when '\'   => Append (R, "\\");
            when L1.LF => Append (R, "\n");
            when L1.CR => Append (R, "\r");
            when L1.HT => Append (R, "\t");
            when L1.BS => Append (R, "\b");
            when L1.FF => Append (R, "\f");
            when Character'Val (0) .. Character'Val (7)
               | Character'Val (11)
               | Character'Val (14) .. Character'Val (31) =>
               Append (R, "\u00");
               Append (R, Hex_Digits (Character'Pos (C) / 16 + 1));
               Append (R, Hex_Digits (Character'Pos (C) mod 16 + 1));
            when others => Append (R, C);
         end case;
      end loop;
      return To_String (R);
   end Escape;

   function Quote (S : String) return String is
   begin
      return """" & Escape (S) & """";
   end Quote;

   function Member (Json : String; Key : String) return String is
      Result : Unbounded_String;
      Found  : Boolean := False;

      procedure Check (K : String; V : String) is
      begin
         if not Found and then K = Key then
            Result := To_Unbounded_String (V);
            Found := True;
         end if;
      end Check;
   begin
      For_Each_Member (Json, Check'Access);
      return To_String (Result);
   end Member;

   function String_Member (Json : String; Key : String) return String is
   begin
      return Unescape (Member (Json, Key));
   end String_Member;

   function Bool_Member (Json : String; Key : String) return Boolean is
   begin
      return Member (Json, Key) = "true";
   end Bool_Member;

   function Natural_Member
     (Json    : String;
      Key     : String;
      Default : Natural := 0) return Natural
   is
      Raw : constant String := Member (Json, Key);
   begin
      if Raw'Length = 0 then
         return Default;
      end if;
      return Natural'Value (Raw);
   exception
      when Constraint_Error =>
         return Default;
   end Natural_Member;

   --  -------------------------------------------------------------------
   --  SDK protocol 4 grouped session payloads
   --  -------------------------------------------------------------------

   type Group_Id is
     (G_Inference, G_Tools, G_Permissions, G_Interaction, G_Events,
      G_Agents, G_Skills, G_Mcp, G_Github, G_Filesystem, G_Execution,
      G_Instructions, G_Hooks, G_Git, G_State, G_Plugins, G_Canvases,
      G_Extensions, G_Experiments, G_Policy);

   type Group_Buffers is array (Group_Id) of Unbounded_String;

   --  Wire name of a group ("inference", "tools", ...).
   function Group_Wire_Name (G : Group_Id) return String is
      Img : constant String := Group_Id'Image (G);
   begin
      return Ada.Characters.Handling.To_Lower (Img (Img'First + 2 .. Img'Last));
   end Group_Wire_Name;

   procedure Find_Group
     (Name  : String;
      G     : out Group_Id;
      Found : out Boolean) is
   begin
      G := Group_Id'First;
      Found := False;
      for Candidate in Group_Id loop
         if Group_Wire_Name (Candidate) = Name then
            G := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Group;

   --  Options that stay at the top level of the payload.
   Root_Keys : constant String :=
     "|sessionId|model|name|clientName|workingDirectory|configDirectory"
     & "|enableConfigDiscovery|systemMessage|diagnostics"
     & "|lifecycleDiagnostics|cloud|traceparent|tracestate"
     & "|suppressResumeEvent|continuePendingWork|allowTranscriptRecovery"
     & "|requestedEnvironmentVariables|";

   --  Flat wire key => group.member (docs/v2-breaking-changes.md).
   Group_Table : constant String :=
     "|reasoningEffort=inference.reasoningEffort"
     & "|reasoningSummary=inference.reasoningSummary"
     & "|contextTier=inference.contextTier"
     & "|allowMultipleToolCalls=inference.allowMultipleToolCalls"
     & "|enableCitations=inference.enableCitations"
     & "|modelCapabilities=inference.capabilities"
     & "|verbosity=inference.verbosity"
     & "|providers=inference.providers"
     & "|models=inference.models"
     & "|defaultProvider=inference.defaultProvider"
     & "|tools=tools.definitions"
     & "|availableTools=tools.availableTools"
     & "|excludedTools=tools.excludedTools"
     & "|toolSearch=tools.search"
     & "|largeOutput=tools.largeOutput"
     & "|imageGeneration=tools.imageGeneration"
     & "|manageScheduleEnabled=tools.manageScheduleEnabled"
     & "|webSearch=tools.webSearch"
     & "|shell=tools.shell"
     & "|maxInlineBinaryBytes=tools.maxInlineBinaryBytes"
     & "|requestPermission=permissions.request"
     & "|sandboxConfig=permissions.sandbox"
     & "|sandbox=permissions.sandbox"
     & "|permissionMode=permissions.mode"
     & "|sandboxConfigSource=permissions.sandboxSource"
     & "|commands=interaction.commands"
     & "|askUserVariant=interaction.askUserVariant"
     & "|requestUserInput=interaction.requestUserInput"
     & "|requestElicitation=interaction.requestElicitation"
     & "|requestShellAuthentication=interaction.requestShellAuthentication"
     & "|requestExitPlanMode=interaction.requestExitPlanMode"
     & "|requestAutoModeSwitch=interaction.requestAutoModeSwitch"
     & "|requestAutoModeRecovery=interaction.requestAutoModeRecovery"
     & "|streaming=events.streaming"
     & "|includeSubAgentStreamingEvents=events.includeSubAgentStreamingEvents"
     & "|customAgents=agents.definitions"
     & "|defaultAgent=agents.default"
     & "|agent=agents.selected"
     & "|includedBuiltinAgents=agents.includedBuiltinAgents"
     & "|excludedBuiltinAgents=agents.excludedBuiltinAgents"
     & "|customAgentsLocalOnly=agents.localDiscoveryOnly"
     & "|agentDirectories=agents.directories"
     & "|skillDirectories=skills.directories"
     & "|enableSkills=skills.enabled"
     & "|includedBuiltinSkills=skills.includedBuiltinSkills"
     & "|disabledSkills=skills.disabledSkills"
     & "|hasSkillProvider=skills.hasProvider"
     & "|ignoredSkillsLocations=skills.ignoredLocations"
     & "|mcpServers=mcp.servers"
     & "|disabledMcpServers=mcp.disabledServers"
     & "|requestMcpApps=mcp.enableApps"
     & "|enableMcpApps=mcp.enableApps"
     & "|mcpOAuthTokenStorage=mcp.oauthTokenStorage"
     & "|authClientIdMetadataUrl=mcp.authClientIdMetadataUrl"
     & "|envValueMode=mcp.envValueMode"
     & "|managedMcpServers=mcp.managedServers"
     & "|allowAllMcpServerInstructions=mcp.allowAllServerInstructions"
     & "|gitHubToken=github.token"
     & "|gitHubTokenProviderRegistrationId=github.tokenProviderRegistrationId"
     & "|githubMcpToolConfig=github.mcp"
     & "|capi=github.capi"
     & "|enableSessionTelemetry=github.telemetryEnabled"
     & "|enableManagedSettings=github.fetchManagedSettings"
     & "|interactiveUser=github.interactiveUser"
     & "|enableGitHubTelemetryForwarding=github.telemetryForwarding"
     & "|authInfo=github.authInfo"
     & "|copilotUrl=github.copilotUrl"
     & "|additionalDirectories=filesystem.additionalDirectories"
     & "|enableFileChangeTracking=filesystem.trackFileChanges"
     & "|trustWorkingDirectory=filesystem.trustWorkingDirectory"
     & "|sessionLimits=execution.limits"
     & "|autopilot=execution.autopilot"
     & "|autopilotContinuation=execution.autopilotContinuation"
     & "|instructionDirectories=instructions.directories"
     & "|organizationCustomInstructions=instructions.organizationText"
     & "|enableOnDemandInstructionDiscovery=instructions.onDemandDiscovery"
     & "|skipCustomInstructions=instructions.skipCustomInstructions"
     & "|refreshCustomInstructions=instructions.refreshCustomInstructions"
     & "|hooks=hooks.hasHandlers"
     & "|hookDirectories=hooks.directories"
     & "|enableFileHooks=hooks.enableRepositoryHooks"
     & "|coauthorEnabled=git.coauthorEnabled"
     & "|enableHostGitOperations=git.enableHostGitOperations"
     & "|infiniteSessions=state.infiniteSessions"
     & "|memory=state.memory"
     & "|hasMemoryStore=state.hasMemoryStore"
     & "|enableSessionStore=state.enableCrossSessionStore"
     & "|skipEmbeddingRetrieval=state.skipEmbeddingRetrieval"
     & "|embeddingCacheStorage=state.embeddingCacheStorage"
     & "|pluginDirectories=plugins.directories"
     & "|installedPlugins=plugins.installed"
     & "|canvases=canvases.definitions"
     & "|canvasProvider=canvases.provider"
     & "|requestCanvasRenderer=canvases.requestRenderer"
     & "|openCanvases=canvases.open"
     & "|requestExtensions=extensions.enabled"
     & "|extensionSdkPath=extensions.sdkPath"
     & "|extensionInfo=extensions.identity"
     & "|enableExperimentalMode=experiments.enableExperimentalMode"
     & "|isExperimentalMode=experiments.enableExperimentalMode"
     & "|featureFlags=experiments.flags"
     & "|expAssignments=experiments.assignments"
     & "|featureFlagConfiguration=experiments.configuration"
     & "|allowedModels=policy.allowedModels"
     & "|managedSettings=policy.managedSettings"
     & "|excludedManagedSettings=policy.excludedManagedSettings"
     & "|enforceManagedModelDefaults=policy.enforceManagedModelDefaults"
     & "|";

   --  Fields of the legacy single provider that describe the model rather
   --  than the connection.
   Provider_Model_Keys : constant String :=
     "|modelId|wireModel|maxPromptTokens|maxContextWindowTokens"
     & "|maxOutputTokens|capabilities|";

   function Has_Key (Members : Unbounded_String; Key : String) return Boolean
   is
   begin
      return Member ("{" & To_String (Members) & "}", Key)'Length > 0;
   end Has_Key;

   --  Add "Key":Value unless the value is null or the key already exists
   --  (earlier values win, which lets explicit grouped options win).
   procedure Add_Member
     (Members : in out Unbounded_String;
      Key     : String;
      Value   : String) is
   begin
      if Value'Length = 0 or else Value = "null" or else Has_Key (Members, Key)
      then
         return;
      end if;
      if Length (Members) > 0 then
         Append (Members, ",");
      end if;
      Append (Members, """" & Key & """:" & Value);
   end Add_Member;

   function Group_Session_Params
     (Flat       : String;
      Is_Resume  : Boolean;
      Empty_Mode : Boolean := False) return String
   is
      Root     : Unbounded_String;
      Groups   : Group_Buffers;
      Provider : Unbounded_String;
      Model    : Unbounded_String;

      --  First pass: merge objects that are already grouped
      --  (for example "inference":{...}); their members win.
      procedure Merge_Explicit (Key : String; Value : String) is
         G     : Group_Id;
         Found : Boolean;

         procedure Add (K : String; V : String) is
         begin
            Add_Member (Groups (G), K, V);
         end Add;
      begin
         if Value'Length = 0 or else Value (Value'First) /= '{' then
            return;
         end if;
         Find_Group (Key, G, Found);
         if Found then
            For_Each_Member (Value, Add'Access);
         end if;
      end Merge_Explicit;

      --  Second pass: place every flat key.
      procedure Place_Flat (Key : String; Value : String) is
         G      : Group_Id;
         Found  : Boolean;
         Needle : constant String := "|" & Key & "=";
         Pos    : constant Natural := Index (Group_Table, Needle);
      begin
         if Value = "null" then
            --  An explicit null defaultProvider clears it on resume.
            if Is_Resume
              and then Key = "defaultProvider"
              and then not Has_Key (Groups (G_Inference), Key)
            then
               if Length (Groups (G_Inference)) > 0 then
                  Append (Groups (G_Inference), ",");
               end if;
               Append (Groups (G_Inference), """defaultProvider"":null");
            end if;
            return;
         end if;
         Find_Group (Key, G, Found);
         if Found and then Value (Value'First) = '{' then
            return;  --  merged in the first pass
         end if;
         if Found and then Pos = 0 then
            --  A group name with a non-object value (for example a fork's
            --  "skills" list) is rejected by protocol 4 servers; drop it.
            return;
         end if;
         if Key = "model" then
            Model := To_Unbounded_String (Value);
         end if;

         if Key = "configDir" then
            Add_Member (Root, "configDirectory", Value);
         elsif Key = "disableResume" and then Is_Resume then
            Add_Member (Root, "suppressResumeEvent", Value);
         elsif Key = "provider" then
            Provider := To_Unbounded_String (Value);
         elsif Is_Resume
           and then (Key = "trustWorkingDirectory"
                     or else Key = "refreshCustomInstructions")
         then
            null;  --  create-only options
         elsif not Is_Resume and then Key = "openCanvases" then
            null;  --  resume-only option
         elsif Index (Root_Keys, "|" & Key & "|") > 0 then
            Add_Member (Root, Key, Value);
         elsif Pos > 0 then
            declare
               Start : constant Positive := Pos + Needle'Length;
               Stop  : constant Natural := Index (Group_Table, "|", Start);
               Dest  : constant String := Group_Table (Start .. Stop - 1);
               Dot   : constant Natural := Index (Dest, ".");
            begin
               Find_Group (Dest (Dest'First .. Dot - 1), G, Found);
               if Found then
                  Add_Member (Groups (G), Dest (Dot + 1 .. Dest'Last), Value);
               end if;
            end;
         else
            --  Unknown keys stay at the root (forward compatibility).
            Add_Member (Root, Key, Value);
         end if;
      end Place_Flat;

      --  Legacy single BYOK provider => named "default" provider.
      procedure Translate_Provider is
         Conn   : Unbounded_String := To_Unbounded_String ("""name"":""default""");
         Fields : Unbounded_String;

         procedure Split (K : String; V : String) is
         begin
            if V = "null" or else K = "name" then
               return;
            elsif Index (Provider_Model_Keys, "|" & K & "|") > 0 then
               Append (Fields, ",""" & K & """:" & V);
            else
               Append (Conn, ",""" & K & """:" & V);
            end if;
         end Split;
      begin
         if Length (Provider) = 0
           or else Has_Key (Groups (G_Inference), "providers")
           or else Has_Key (Groups (G_Inference), "defaultProvider")
           or else Has_Key (Groups (G_Github), "capi")
         then
            return;  --  explicit named providers win over the legacy option
         end if;
         For_Each_Member (To_String (Provider), Split'Access);
         Add_Member (Groups (G_Inference), "providers",
                     "[{" & To_String (Conn) & "}]");
         Add_Member (Groups (G_Inference), "defaultProvider", """default""");
         Add_Member (Groups (G_Github), "capi", "{""enabled"":false}");
         if Length (Fields) > 0 and then Length (Model) > 0 then
            Add_Member (Groups (G_Inference), "models",
                        "[{""provider"":""default"",""id"":"
                        & To_String (Model) & To_String (Fields) & "}]");
         end if;
      end Translate_Provider;

      Result : Unbounded_String;
   begin
      For_Each_Member (Flat, Merge_Explicit'Access);
      For_Each_Member (Flat, Place_Flat'Access);

      if Has_Key (Groups (G_Mcp), "servers") then
         Add_Member (Groups (G_Mcp), "envValueMode", """direct""");
      end if;

      Translate_Provider;

      if Empty_Mode then
         Add_Member (Groups (G_Experiments), "enableExperimentalMode", "false");
         Add_Member (Groups (G_Github), "telemetryEnabled", "false");
         Add_Member (Groups (G_Mcp), "oauthTokenStorage", """in-memory""");
         Add_Member (Groups (G_State), "skipEmbeddingRetrieval", "true");
         Add_Member (Groups (G_State), "embeddingCacheStorage", """in-memory""");
         Add_Member (Groups (G_State), "enableCrossSessionStore", "false");
         Add_Member (Groups (G_State), "memory", "{""enabled"":false}");
         Add_Member (Groups (G_Instructions), "onDemandDiscovery", "false");
         Add_Member (Groups (G_Hooks), "enableRepositoryHooks", "false");
         Add_Member (Groups (G_Git), "enableHostGitOperations", "false");
      end if;

      Result := "{" & Root;
      for G in Group_Id loop
         if Length (Groups (G)) > 0 then
            if Length (Result) > 1 then
               Append (Result, ",");
            end if;
            Append (Result, """" & Group_Wire_Name (G) & """:{");
            Append (Result, Groups (G));
            Append (Result, "}");
         end if;
      end loop;
      Append (Result, "}");
      return To_String (Result);
   end Group_Session_Params;

   --  -------------------------------------------------------------------
   --  Registration ids
   --  -------------------------------------------------------------------

   subtype Hex_Value is Natural range 0 .. 15;
   package Random_Hex is new Ada.Numerics.Discrete_Random (Hex_Value);

   Generator : Random_Hex.Generator;
   Seeded    : Boolean := False;

   function New_Registration_Id return String is
      Result : String (1 .. 36);
   begin
      if not Seeded then
         Random_Hex.Reset (Generator);
         Seeded := True;
      end if;
      for I in Result'Range loop
         case I is
            when 9 | 14 | 19 | 24 =>
               Result (I) := '-';
            when 15 =>
               Result (I) := '4';
            when 20 =>
               Result (I) :=
                 Hex_Digits (8 + Random_Hex.Random (Generator) mod 4 + 1);
            when others =>
               Result (I) := Hex_Digits (Random_Hex.Random (Generator) + 1);
         end case;
      end loop;
      return Result;
   end New_Registration_Id;

end Copilot.Wire;
