      *================================================================*
      * COPILOT-SESSION-OPTS.cpy                                       *
      * Session option records shared by COPILOT-SESSION and the       *
      * protocol 4 session payload builder (COPILOT-PARAMS.cpy). Set   *
      * these items before CREATE-SESSION or RESUME-SESSION.           *
      *================================================================*

      *----------------------------------------------------------------*
      * Upstream-sync session options (parity with @github/copilot).
      * BUILD-SESSION-PARAMS places each one in its SDK protocol 4
      * group: enableCitations and reasoningEffort -> inference,
      * excludedBuiltinAgents and customAgentsLocalOnly -> agents,
      * sessionLimits / maxAiCredits -> execution.limits,
      * memory -> state, expAssignments -> experiments,
      * additionalDirectories -> filesystem, disabledMcpServers ->
      * mcp, githubMcpToolConfig -> github, canvasProvider ->
      * canvases, toolSearch -> tools. Options without a protocol 4
      * group (otlpProtocol, enableWebSocketResponses, mcpAuthHandler,
      * rewindEnabled, decisionContext, userPromptTransformed,
      * builtinPluginDirectories, argsSchema, inProcess,
      * experimentalMode, contentExclusion) stay at the root.
      *----------------------------------------------------------------*
       01  WS-UPSTREAM-CONFIG.
      *    Inline citations: enableCitations
           05 WS-ENABLE-CITATIONS   PIC 9       VALUE 0.
               88 CITATIONS-ON                  VALUE 1.
               88 CITATIONS-OFF                 VALUE 0.
      *    Excluded built-in agents (JSON array): excludedBuiltinAgents
           05 WS-EXCLUDED-BUILTIN   PIC X(1024) VALUE SPACES.
      *    Session spending limits: sessionLimits / maxAiCredits
           05 WS-MAX-AI-CREDITS     PIC 9(10)   VALUE 0.
           05 WS-MAX-AI-CREDITS-ED  PIC Z(9)9.
      *    Session memory config: MemoryConfiguration / memory_config
           05 WS-MEMORY-ENABLED     PIC 9       VALUE 0.
               88 MEMORY-ON                     VALUE 1.
               88 MEMORY-OFF                    VALUE 0.
      *    OTLP telemetry protocol: otlpProtocol
           05 WS-OTLP-PROTOCOL      PIC X(16)   VALUE SPACES.
      *    WebSocket transport: enableWebSocketResponses
           05 WS-ENABLE-WEBSOCKET   PIC 9       VALUE 0.
               88 WEBSOCKET-ON                  VALUE 1.
               88 WEBSOCKET-OFF                 VALUE 0.
      *    Experiment assignments (JSON object): expAssignments
           05 WS-EXP-ASSIGNMENTS    PIC X(1024) VALUE SPACES.
      *    MCP OAuth token handler: mcpAuthHandler / on_mcp_auth_request
           05 WS-MCP-AUTH-HANDLER   PIC 9       VALUE 0.
               88 MCP-AUTH-ON                   VALUE 1.
               88 MCP-AUTH-OFF                  VALUE 0.
      *    Session rewind: rewindEnabled
           05 WS-REWIND-ENABLED     PIC 9       VALUE 0.
               88 REWIND-ON                     VALUE 1.
               88 REWIND-OFF                    VALUE 0.
      *    Additional session directories (JSON array): additionalDirectories
           05 WS-ADDITIONAL-DIRS    PIC X(1024) VALUE SPACES.
      *    Disabled MCP servers (JSON array): disabledMcpServers
           05 WS-DISABLED-MCP       PIC X(1024) VALUE SPACES.
      *    GitHub MCP tool config (JSON object): githubMcpToolConfig
           05 WS-GITHUB-MCP-CONFIG  PIC X(1024) VALUE SPACES.
      *    Canvas provider (JSON object): canvasProvider
           05 WS-CANVAS-PROVIDER    PIC X(1024) VALUE SPACES.
      *    Custom agents local-only: customAgentsLocalOnly
           05 WS-CUSTOM-AGENTS-LCL  PIC 9       VALUE 0.
               88 CUSTOM-AGENTS-LOCAL-ON        VALUE 1.
               88 CUSTOM-AGENTS-LOCAL-OFF       VALUE 0.
      *    Permission-reply decision context (JSON object): decisionContext
           05 WS-DECISION-CONTEXT   PIC X(1024) VALUE SPACES.
      *    User-prompt-transformed hook: userPromptTransformed
           05 WS-USER-PROMPT-XFORM  PIC 9       VALUE 0.
               88 USER-PROMPT-XFORM-ON          VALUE 1.
               88 USER-PROMPT-XFORM-OFF         VALUE 0.
      *    Built-in plugin directories (JSON array): builtinPluginDirectories
           05 WS-BUILTIN-PLUGIN-DIR PIC X(1024) VALUE SPACES.
      *    Agent-factory authoring args schema (JSON object): argsSchema
           05 WS-ARGS-SCHEMA        PIC X(1024) VALUE SPACES.
      *    Reasoning effort control: reasoningEffort
           05 WS-REASONING-EFFORT   PIC X(16)   VALUE SPACES.
      *    Tool search configuration (JSON object): toolSearch
           05 WS-TOOL-SEARCH        PIC X(1024) VALUE SPACES.
      *    In-process FFI transport: inProcess
           05 WS-IN-PROCESS         PIC 9       VALUE 0.
               88 IN-PROCESS-ON                 VALUE 1.
               88 IN-PROCESS-OFF                VALUE 0.
      *    Experimental mode: experimentalMode
           05 WS-EXPERIMENTAL-MODE  PIC 9       VALUE 0.
               88 EXPERIMENTAL-ON               VALUE 1.
               88 EXPERIMENTAL-OFF              VALUE 0.
      *    Content exclusion: contentExclusion
           05 WS-CONTENT-EXCLUSION  PIC 9       VALUE 0.
               88 CONTENT-EXCLUSION-ON          VALUE 1.
               88 CONTENT-EXCLUSION-OFF         VALUE 0.

      *----------------------------------------------------------------*
      * SDK protocol 4 / v3.0.0 session options. Tri-state flags hold  *
      * "Y" (true), "N" (false) or SPACE (not sent). Callback programs *
      * are CALLed dynamically when the runtime sends the request.     *
      *----------------------------------------------------------------*
       01  WS-V4-SESSION-OPTIONS.
      *    Ask-user tool variant (askUserVariant)
           05 WS-ASK-USER-VARIANT    PIC X(16)  VALUE SPACES.
               88 ASK-USER-LEGACY               VALUE "legacy".
               88 ASK-USER-ELICITATION          VALUE "elicitation".
      *    GitHub token provider program (gitHubToken.getToken). The
      *    SDK generates WS-GH-TOKEN-REG-ID. Mutually exclusive with a
      *    static token (WS-AUTH-TOKEN or WS-SESS-AUTH-TOKEN).
           05 WS-GH-TOKEN-PGM        PIC X(30)  VALUE SPACES.
           05 WS-GH-TOKEN-REG-ID     PIC X(36)  VALUE SPACES.
      *    Command sandbox (permissions.sandbox): enabled, allowBypass
           05 WS-SANDBOX-ENABLED     PIC X      VALUE SPACE.
               88 SANDBOX-ON                    VALUE "Y".
               88 SANDBOX-OFF                   VALUE "N".
           05 WS-SANDBOX-ALLOW-BYPASS PIC X     VALUE SPACE.
               88 SANDBOX-BYPASS-ON             VALUE "Y".
               88 SANDBOX-BYPASS-OFF            VALUE "N".
      *    Feature flag overrides (featureFlags), JSON object
           05 WS-FEATURE-FLAG-OVERRIDES PIC X(1024) VALUE SPACES.
      *    Model allowlist (allowedModels), JSON array
           05 WS-ALLOWED-MODELS      PIC X(1024) VALUE SPACES.
      *    MCP OAuth client id metadata URL (authClientIdMetadataUrl)
           05 WS-AUTH-CLIENT-MD-URL  PIC X(512) VALUE SPACES.
      *    Max output tokens override (limits.max_output_tokens);
      *    0 = not sent. A raw WS-MODEL-CAPABILITIES object wins.
           05 WS-MAX-OUTPUT-TOKENS   PIC 9(10)  VALUE 0.
      *    Agent and hook directories (JSON arrays): agentDirectories,
      *    hookDirectories
           05 WS-AGENT-DIRECTORIES   PIC X(1024) VALUE SPACES.
           05 WS-HOOK-DIRECTORIES    PIC X(1024) VALUE SPACES.
      *    Working directory trust (trustWorkingDirectory, create
      *    only): "session" or "user"; needs WS-SESS-WORKING-DIR.
           05 WS-TRUST-WORKING-DIR   PIC X(8)   VALUE SPACES.
               88 TRUST-WD-SESSION              VALUE "session".
               88 TRUST-WD-USER                 VALUE "user".
      *    Image generation (tools.imageGeneration): enabled,
      *    watermark and saveToDisk
           05 WS-IG-ENABLED          PIC X      VALUE SPACE.
               88 IMAGE-GEN-ON                  VALUE "Y".
               88 IMAGE-GEN-OFF                 VALUE "N".
           05 WS-IG-WATERMARK        PIC X      VALUE SPACE.
               88 IMAGE-GEN-WATERMARK-ON        VALUE "Y".
               88 IMAGE-GEN-WATERMARK-OFF       VALUE "N".
           05 WS-IG-SAVE-TO-DISK     PIC X      VALUE SPACE.
               88 IMAGE-GEN-SAVE-TO-DISK        VALUE "Y".
               88 IMAGE-GEN-NO-SAVE             VALUE "N".
      *    Autopilot (execution.autopilot): continuationEnabled and
      *    maxContinues (0 = runtime default)
           05 WS-AUTOPILOT-CONTINUE  PIC X      VALUE SPACE.
               88 AUTOPILOT-CONTINUE-ON         VALUE "Y".
               88 AUTOPILOT-CONTINUE-OFF        VALUE "N".
           05 WS-AUTOPILOT-MAX-CONT  PIC 9(5)   VALUE 0.
      *    Sub-agent lifecycle hook programs (subagentStart and
      *    subagentStop, delivered through hooks.invoke)
           05 WS-SUBAGENT-START-PGM  PIC X(30)  VALUE SPACES.
           05 WS-SUBAGENT-STOP-PGM   PIC X(30)  VALUE SPACES.
      *    Shell authentication program (shellAuthentication.request)
           05 WS-SHELL-AUTH-PGM      PIC X(30)  VALUE SPACES.
      *    User input program for the ask_user tool (userInput.request,
      *    WS-USER-INPUT-REQ / WS-USER-INPUT-RESP); sends
      *    interaction.requestUserInput
           05 WS-USER-INPUT-PGM      PIC X(30)  VALUE SPACES.
      *    Store-memory program (memoryStore.store)
           05 WS-MEMORY-STORE-PGM    PIC X(30)  VALUE SPACES.
      *    Program CALLed with WS-SHELL-OUTPUT-EVENT for every
      *    tool.shell_output event (optional)
           05 WS-SHELL-OUTPUT-PGM    PIC X(30)  VALUE SPACES.
      *    Named BYOK providers (inference.providers, models and
      *    defaultProvider) and Copilot API models (github.capi).
      *    Select a provider model with "provider/id" in
      *    WS-SESS-MODEL. On resume CLEAR-DEFAULT-PROVIDER sends
      *    defaultProvider null.
           05 WS-DEFAULT-PROVIDER    PIC X(64)  VALUE SPACES.
           05 WS-CLEAR-DEFAULT-PROV  PIC 9      VALUE 0.
               88 CLEAR-DEFAULT-PROVIDER        VALUE 1.
           05 WS-CAPI-ENABLED        PIC X      VALUE SPACE.
               88 CAPI-MODELS-ON                VALUE "Y".
               88 CAPI-MODELS-OFF               VALUE "N".
           05 WS-PROVIDER-COUNT      PIC 9(2)   VALUE 0.
           05 WS-PROVIDER-ENTRY      OCCURS 4 TIMES.
               10 WS-NP-NAME         PIC X(64)  VALUE SPACES.
               10 WS-NP-TYPE         PIC X(16)  VALUE SPACES.
               10 WS-NP-BASE-URL     PIC X(256) VALUE SPACES.
               10 WS-NP-API-KEY      PIC X(256) VALUE SPACES.
      *        Command whose output is the API key (apiKeyCommand)
               10 WS-NP-API-KEY-CMD  PIC X(256) VALUE SPACES.
               10 WS-NP-BEARER-TOKEN PIC X(256) VALUE SPACES.
               10 WS-NP-WIRE-API     PIC X(16)  VALUE SPACES.
               10 WS-NP-TRANSPORT    PIC X(16)  VALUE SPACES.
               10 WS-NP-MODEL-PROV   PIC X(64)  VALUE SPACES.
           05 WS-PROVIDER-MODEL-COUNT PIC 9(2)  VALUE 0.
           05 WS-PROVIDER-MODEL      OCCURS 8 TIMES.
               10 WS-PM-PROVIDER     PIC X(64)  VALUE SPACES.
               10 WS-PM-ID           PIC X(128) VALUE SPACES.
               10 WS-PM-MODEL-ID     PIC X(128) VALUE SPACES.
               10 WS-PM-WIRE-MODEL   PIC X(128) VALUE SPACES.
               10 WS-PM-NAME         PIC X(128) VALUE SPACES.
               10 WS-PM-MAX-PROMPT   PIC 9(10)  VALUE 0.
               10 WS-PM-MAX-CONTEXT  PIC 9(10)  VALUE 0.
               10 WS-PM-MAX-OUTPUT   PIC 9(10)  VALUE 0.
      *    Resume-only root options: suppressResumeEvent (no
      *    session.resume event), continuePendingWork and
      *    allowTranscriptRecovery
           05 WS-RESUME-DISABLE      PIC 9      VALUE 0.
               88 RESUME-EVENT-SUPPRESSED       VALUE 1.
           05 WS-RESUME-CONTINUE     PIC 9      VALUE 0.
               88 RESUME-CONTINUE-PENDING       VALUE 1.
           05 WS-RESUME-RECOVERY     PIC 9      VALUE 0.
               88 RESUME-TRANSCRIPT-RECOVERY    VALUE 1.

      *----------------------------------------------------------------*
      * Work fields for BUILD-SESSION-PARAMS (COPILOT-PARAMS.cpy)      *
      *----------------------------------------------------------------*
       01  WS-PB-WORK.
           05 WS-PB-KIND             PIC X      VALUE "C".
               88 PB-FOR-CREATE                 VALUE "C".
               88 PB-FOR-RESUME                 VALUE "R".
           05 WS-PB-STATUS           PIC 9      VALUE 0.
               88 PB-OK                         VALUE 0.
               88 PB-FAILED                     VALUE 1.
           05 WS-PB-ROOT-COUNT       PIC 9(4)   VALUE 0.
           05 WS-PB-GROUP            PIC X(16)  VALUE SPACES.
           05 WS-PB-GROUP-OPEN       PIC 9      VALUE 0.
               88 PB-GROUP-IS-OPEN              VALUE 1.
               88 PB-GROUP-IS-CLOSED            VALUE 0.
           05 WS-PB-OBJ-FIRST        PIC 9      VALUE 1.
           05 WS-PB-KEY              PIC X(48)  VALUE SPACES.
           05 WS-PB-TEXT             PIC X(4096) VALUE SPACES.
           05 WS-PB-LEN              PIC 9(5)   VALUE 0.
           05 WS-PB-IDX              PIC 9(5)   VALUE 0.
           05 WS-PB-CHAR             PIC X      VALUE SPACE.
           05 WS-PB-FLAG             PIC X      VALUE SPACE.
           05 WS-PB-NUM              PIC 9(10)  VALUE 0.
           05 WS-PB-NUM-ED           PIC Z(9)9.
           05 WS-PB-ENTRY            PIC 9(2)   VALUE 0.
           05 WS-PB-SEED             PIC 9(9)   VALUE 0.
           05 WS-PB-SERIAL           PIC 9(5)   VALUE 0.
           05 WS-PB-RAND             PIC 9V9(9) VALUE 0.
           05 WS-PB-HEX-IDX          PIC 9(2)   VALUE 0.
           05 WS-PB-HEX-DIGITS       PIC X(16)
               VALUE "0123456789abcdef".
