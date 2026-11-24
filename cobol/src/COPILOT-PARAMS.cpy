      *================================================================*
      * COPILOT-PARAMS.cpy                                             *
      * SDK protocol 4 session payload builder (procedure copybook).   *
      *                                                                *
      * This is the COBOL translation layer. Callers keep setting the  *
      * same flat working-storage options (WS-SESSION-CONFIG,          *
      * WS-SESSION-EXTENDED, WS-FEATURE-FLAGS, WS-UPSTREAM-CONFIG,     *
      * WS-V4-SESSION-OPTIONS, WS-TOOL-DEF, ...) and                   *
      * BUILD-SESSION-PARAMS writes them straight into the protocol 4  *
      * grouped JSON for session.create and session.resume:            *
      *  - root keys (sessionId, model, clientName, workingDirectory,  *
      *    enableConfigDiscovery and the resume-only                   *
      *    suppressResumeEvent, continuePendingWork and                *
      *    allowTranscriptRecovery) stay at the top level;             *
      *  - every other known option becomes a member of its group      *
      *    (inference, tools, permissions, interaction, events,        *
      *    agents, mcp, github, filesystem, execution, instructions,   *
      *    hooks, git, state, canvases, experiments, policy);          *
      *  - options without a protocol 4 group stay at the root         *
      *    unchanged, empty groups are never written, and the          *
      *    create-only filesystem.trustWorkingDirectory is dropped on  *
      *    resume;                                                     *
      *  - client mode "empty" requires availableTools and adds its    *
      *    defaults under the caller's values.                         *
      * Requires in WORKING-STORAGE: COPILOT-TYPES.cpy,                *
      * COPILOT-SESSION-OPTS.cpy, WS-PARAMS-BUFFER PIC X(4096) and     *
      * WS-PARAMS-PTR PIC 9(5).                                        *
      *================================================================*

      *----------------------------------------------------------------*
      * BUILD-SESSION-PARAMS: Build the grouped params for             *
      * session.create (PB-FOR-CREATE) or session.resume               *
      * (PB-FOR-RESUME) into WS-PARAMS-BUFFER. Invalid options set     *
      * PB-FAILED, WS-RETURN-CODE -1 and WS-LAST-ERROR.                *
      *----------------------------------------------------------------*
       BUILD-SESSION-PARAMS.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR
           MOVE 0 TO WS-PB-ROOT-COUNT
           SET PB-GROUP-IS-CLOSED TO TRUE
           SET PB-OK TO TRUE

           PERFORM PB-VALIDATE-OPTIONS
           IF PB-FAILED
               EXIT PARAGRAPH
           END-IF

           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           PERFORM PB-ROOT-KEYS
           PERFORM PB-ROOT-PASSTHROUGH
           PERFORM PB-GROUP-INFERENCE
           PERFORM PB-GROUP-TOOLS
           PERFORM PB-GROUP-PERMISSIONS
           PERFORM PB-GROUP-INTERACTION
           PERFORM PB-GROUP-EVENTS
           PERFORM PB-GROUP-AGENTS
           PERFORM PB-GROUP-MCP
           PERFORM PB-GROUP-GITHUB
           PERFORM PB-GROUP-FILESYSTEM
           PERFORM PB-GROUP-EXECUTION
           PERFORM PB-GROUP-INSTRUCTIONS
           PERFORM PB-GROUP-HOOKS
           PERFORM PB-GROUP-GIT
           PERFORM PB-GROUP-STATE
           PERFORM PB-GROUP-CANVASES
           PERFORM PB-GROUP-EXPERIMENTS
           PERFORM PB-GROUP-POLICY
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *----------------------------------------------------------------*
      * PB-VALIDATE-OPTIONS: Reject option combinations the SDK can    *
      * detect before sending.                                         *
      *----------------------------------------------------------------*
       PB-VALIDATE-OPTIONS.
           IF CLIENT-MODE-EMPTY AND WS-AVAILABLE-TOOLS = SPACES
               MOVE "Client mode 'empty' requires availableTools"
                   TO WS-LAST-ERROR
               PERFORM PB-FAIL
           END-IF
           IF WS-GH-TOKEN-PGM NOT = SPACES
              AND (WS-AUTH-TOKEN NOT = SPACES
                   OR WS-SESS-AUTH-TOKEN NOT = SPACES)
               MOVE "gitHubToken and gitHubTokenProvider are exclusive"
                   TO WS-LAST-ERROR
               PERFORM PB-FAIL
           END-IF
           IF PB-FOR-CREATE AND WS-TRUST-WORKING-DIR NOT = SPACES
               IF NOT TRUST-WD-SESSION AND NOT TRUST-WD-USER
                   MOVE "trustWorkingDirectory must be session or user"
                       TO WS-LAST-ERROR
                   PERFORM PB-FAIL
               END-IF
               IF WS-SESS-WORKING-DIR = SPACES
                   MOVE "trustWorkingDirectory needs workingDirectory"
                       TO WS-LAST-ERROR
                   PERFORM PB-FAIL
               END-IF
           END-IF
           .

       PB-FAIL.
           SET PB-FAILED TO TRUE
           MOVE -1 TO WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * PB-ROOT-KEYS: Keys that stay at the top level in protocol 4.   *
      *----------------------------------------------------------------*
       PB-ROOT-KEYS.
           IF PB-FOR-RESUME
               MOVE "sessionId" TO WS-PB-KEY
               MOVE WS-SESSION-ID TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
           IF WS-SESS-MODEL NOT = SPACES
               MOVE "model" TO WS-PB-KEY
               MOVE WS-SESS-MODEL TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
      *    The integrating application (agent name and version) is
      *    sent as clientName. Protocol 4 rejects the old "agent"
      *    identity object: "agent" now means agents.selected.
           IF WS-SESS-AGENT-NAME NOT = SPACES
               MOVE SPACES TO WS-PB-TEXT
               IF WS-SESS-AGENT-VER = SPACES
                   MOVE WS-SESS-AGENT-NAME TO WS-PB-TEXT
               ELSE
                   STRING FUNCTION TRIM(WS-SESS-AGENT-NAME)
                       "/" FUNCTION TRIM(WS-SESS-AGENT-VER)
                       DELIMITED SIZE INTO WS-PB-TEXT
                   END-STRING
               END-IF
               MOVE "clientName" TO WS-PB-KEY
               PERFORM PB-ROOT-STRING
           END-IF
           IF WS-SESS-WORKING-DIR NOT = SPACES
               MOVE "workingDirectory" TO WS-PB-KEY
               MOVE WS-SESS-WORKING-DIR TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
           IF CONFIG-DISCOVERY-ON OR CONFIG-DISC-ON
               MOVE "enableConfigDiscovery" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF PB-FOR-RESUME
               IF RESUME-EVENT-SUPPRESSED
                   MOVE "suppressResumeEvent" TO WS-PB-KEY
                   PERFORM PB-ROOT-TRUE
               END-IF
               IF RESUME-CONTINUE-PENDING
                   MOVE "continuePendingWork" TO WS-PB-KEY
                   PERFORM PB-ROOT-TRUE
               END-IF
               IF RESUME-TRANSCRIPT-RECOVERY
                   MOVE "allowTranscriptRecovery" TO WS-PB-KEY
                   PERFORM PB-ROOT-TRUE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * PB-ROOT-PASSTHROUGH: Options without a protocol 4 group stay   *
      * at the root unchanged (the runtime tolerates unknown root      *
      * keys).                                                         *
      *----------------------------------------------------------------*
       PB-ROOT-PASSTHROUGH.
           IF WS-SESS-SYS-PROMPT NOT = SPACES
               MOVE "systemPrompt" TO WS-PB-KEY
               MOVE WS-SESS-SYS-PROMPT TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
           IF WS-OTLP-PROTOCOL NOT = SPACES
               MOVE "otlpProtocol" TO WS-PB-KEY
               MOVE WS-OTLP-PROTOCOL TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
           IF WEBSOCKET-ON
               MOVE "enableWebSocketResponses" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF MCP-AUTH-ON
               MOVE "mcpAuthHandler" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF REWIND-ON
               MOVE "rewindEnabled" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF WS-DECISION-CONTEXT NOT = SPACES
               MOVE "decisionContext" TO WS-PB-KEY
               MOVE WS-DECISION-CONTEXT TO WS-PB-TEXT
               PERFORM PB-ROOT-RAW
           END-IF
           IF USER-PROMPT-XFORM-ON
               MOVE "userPromptTransformed" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF WS-BUILTIN-PLUGIN-DIR NOT = SPACES
               MOVE "builtinPluginDirectories" TO WS-PB-KEY
               MOVE WS-BUILTIN-PLUGIN-DIR TO WS-PB-TEXT
               PERFORM PB-ROOT-RAW
           END-IF
           IF WS-ARGS-SCHEMA NOT = SPACES
               MOVE "argsSchema" TO WS-PB-KEY
               MOVE WS-ARGS-SCHEMA TO WS-PB-TEXT
               PERFORM PB-ROOT-RAW
           END-IF
           IF IN-PROCESS-ON
               MOVE "inProcess" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF EXPERIMENTAL-ON
               MOVE "experimentalMode" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           IF CONTENT-EXCLUSION-ON
               MOVE "contentExclusion" TO WS-PB-KEY
               PERFORM PB-ROOT-TRUE
           END-IF
           .

      *----------------------------------------------------------------*
      * Protocol 4 groups                                              *
      *----------------------------------------------------------------*
       PB-GROUP-INFERENCE.
           MOVE "inference" TO WS-PB-GROUP
           IF WS-REASONING-EFFORT NOT = SPACES
               MOVE "reasoningEffort" TO WS-PB-KEY
               MOVE WS-REASONING-EFFORT TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           IF CITATIONS-ON
               MOVE "enableCitations" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
      *    Model capabilities override. WS-MAX-OUTPUT-TOKENS becomes
      *    limits.max_output_tokens unless a raw object is given.
           IF WS-MODEL-CAPABILITIES NOT = SPACES
               MOVE "capabilities" TO WS-PB-KEY
               MOVE WS-MODEL-CAPABILITIES TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           ELSE
               IF WS-MAX-OUTPUT-TOKENS > 0
                   MOVE "capabilities" TO WS-PB-KEY
                   PERFORM PB-MEMBER-KEY
                   MOVE WS-MAX-OUTPUT-TOKENS TO WS-PB-NUM-ED
                   STRING '{"limits":{"max_output_tokens":'
                       FUNCTION TRIM(WS-PB-NUM-ED) '}}'
                       DELIMITED SIZE INTO WS-PARAMS-BUFFER
                       WITH POINTER WS-PARAMS-PTR
                   END-STRING
               END-IF
           END-IF
           IF WS-PROVIDER-COUNT > 0
               MOVE "providers" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-PUT-PROVIDERS
           END-IF
           IF WS-PROVIDER-MODEL-COUNT > 0
               MOVE "models" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-PUT-PROVIDER-MODELS
           END-IF
           IF WS-DEFAULT-PROVIDER NOT = SPACES
               MOVE "defaultProvider" TO WS-PB-KEY
               MOVE WS-DEFAULT-PROVIDER TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           ELSE
               IF PB-FOR-RESUME AND CLEAR-DEFAULT-PROVIDER
                   MOVE "defaultProvider" TO WS-PB-KEY
                   PERFORM PB-MEMBER-KEY
                   PERFORM PB-PUT-NULL
               END-IF
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-TOOLS.
           MOVE "tools" TO WS-PB-GROUP
      *    The tool definition is sent only when WS-TOOL-HANDLER-PGM
      *    can answer its external_tool.requested calls.
           IF WS-TOOL-NAME NOT = SPACES
              AND WS-TOOL-HANDLER-PGM NOT = SPACES
               MOVE "definitions" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               MOVE "[" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
               PERFORM PB-OBJ-OPEN
               MOVE "name" TO WS-PB-KEY
               MOVE WS-TOOL-NAME TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "description" TO WS-PB-KEY
               MOVE WS-TOOL-DESCRIPTION TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               IF WS-TOOL-SCHEMA NOT = SPACES
                   MOVE "parameters" TO WS-PB-KEY
                   PERFORM PB-OBJ-KEY
                   MOVE WS-TOOL-SCHEMA TO WS-PB-TEXT
                   PERFORM PB-PUT-RAW
               END-IF
               MOVE "idempotent" TO WS-PB-KEY
               MOVE WS-TOOL-IDEMPOTENT TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               PERFORM PB-OBJ-CLOSE
               MOVE "]" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           IF WS-AVAILABLE-TOOLS NOT = SPACES
               MOVE "availableTools" TO WS-PB-KEY
               MOVE WS-AVAILABLE-TOOLS TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF WS-TOOL-SEARCH NOT = SPACES
               MOVE "search" TO WS-PB-KEY
               MOVE WS-TOOL-SEARCH TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF WS-IG-ENABLED NOT = SPACE
              OR WS-IG-WATERMARK NOT = SPACE
              OR WS-IG-SAVE-TO-DISK NOT = SPACE
               MOVE "imageGeneration" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-OBJ-OPEN
               MOVE "enabled" TO WS-PB-KEY
               MOVE WS-IG-ENABLED TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               MOVE "watermark" TO WS-PB-KEY
               MOVE WS-IG-WATERMARK TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               MOVE "saveToDisk" TO WS-PB-KEY
               MOVE WS-IG-SAVE-TO-DISK TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               PERFORM PB-OBJ-CLOSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-PERMISSIONS.
           MOVE "permissions" TO WS-PB-GROUP
           IF WS-SANDBOX-ENABLED NOT = SPACE
              OR WS-SANDBOX-ALLOW-BYPASS NOT = SPACE
               MOVE "sandbox" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-OBJ-OPEN
               MOVE "enabled" TO WS-PB-KEY
               MOVE WS-SANDBOX-ENABLED TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               MOVE "allowBypass" TO WS-PB-KEY
               MOVE WS-SANDBOX-ALLOW-BYPASS TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               PERFORM PB-OBJ-CLOSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-INTERACTION.
           MOVE "interaction" TO WS-PB-GROUP
           IF WS-ASK-USER-VARIANT NOT = SPACES
               MOVE "askUserVariant" TO WS-PB-KEY
               MOVE WS-ASK-USER-VARIANT TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           IF WS-SHELL-AUTH-PGM NOT = SPACES
               MOVE "requestShellAuthentication" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           IF WS-USER-INPUT-PGM NOT = SPACES
               MOVE "requestUserInput" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-EVENTS.
           MOVE "events" TO WS-PB-GROUP
           IF STREAMING-ON
               MOVE "streaming" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           IF SUB-AGT-STREAM-ON OR SUB-AGENT-STREAM-ON
               MOVE "includeSubAgentStreamingEvents" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-AGENTS.
           MOVE "agents" TO WS-PB-GROUP
           IF WS-EXCLUDED-BUILTIN NOT = SPACES
               MOVE "excludedBuiltinAgents" TO WS-PB-KEY
               MOVE WS-EXCLUDED-BUILTIN TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF CUSTOM-AGENTS-LOCAL-ON
               MOVE "localDiscoveryOnly" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           IF WS-AGENT-DIRECTORIES NOT = SPACES
               MOVE "directories" TO WS-PB-KEY
               MOVE WS-AGENT-DIRECTORIES TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-MCP.
           MOVE "mcp" TO WS-PB-GROUP
           IF WS-DISABLED-MCP NOT = SPACES
               MOVE "disabledServers" TO WS-PB-KEY
               MOVE WS-DISABLED-MCP TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF WS-AUTH-CLIENT-MD-URL NOT = SPACES
               MOVE "authClientIdMetadataUrl" TO WS-PB-KEY
               MOVE WS-AUTH-CLIENT-MD-URL TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           IF CLIENT-MODE-EMPTY
               MOVE "oauthTokenStorage" TO WS-PB-KEY
               MOVE "in-memory" TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-GITHUB.
           MOVE "github" TO WS-PB-GROUP
           IF WS-SESS-AUTH-TOKEN NOT = SPACES
               MOVE "token" TO WS-PB-KEY
               MOVE WS-SESS-AUTH-TOKEN TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           ELSE
               IF WS-AUTH-TOKEN NOT = SPACES
                   MOVE "token" TO WS-PB-KEY
                   MOVE WS-AUTH-TOKEN TO WS-PB-TEXT
                   PERFORM PB-MEMBER-STRING
               END-IF
           END-IF
      *    GitHub token provider: the runtime asks for tokens with
      *    gitHubToken.getToken and this registration id.
           IF WS-GH-TOKEN-PGM NOT = SPACES
               IF WS-GH-TOKEN-REG-ID = SPACES
                   PERFORM PB-NEW-REGISTRATION-ID
               END-IF
               MOVE "tokenProviderRegistrationId" TO WS-PB-KEY
               MOVE WS-GH-TOKEN-REG-ID TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           IF WS-GITHUB-MCP-CONFIG NOT = SPACES
               MOVE "mcp" TO WS-PB-KEY
               MOVE WS-GITHUB-MCP-CONFIG TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF WS-CAPI-ENABLED NOT = SPACE
               MOVE "capi" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-OBJ-OPEN
               MOVE "enabled" TO WS-PB-KEY
               MOVE WS-CAPI-ENABLED TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               PERFORM PB-OBJ-CLOSE
           END-IF
           IF SESS-TELEMETRY-ON
               MOVE "telemetryEnabled" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           ELSE
               IF CLIENT-MODE-EMPTY
                   MOVE "telemetryEnabled" TO WS-PB-KEY
                   PERFORM PB-MEMBER-FALSE
               END-IF
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-FILESYSTEM.
           MOVE "filesystem" TO WS-PB-GROUP
           IF WS-ADDITIONAL-DIRS NOT = SPACES
               MOVE "additionalDirectories" TO WS-PB-KEY
               MOVE WS-ADDITIONAL-DIRS TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
      *    trustWorkingDirectory is create only.
           IF PB-FOR-CREATE AND WS-TRUST-WORKING-DIR NOT = SPACES
               MOVE "trustWorkingDirectory" TO WS-PB-KEY
               MOVE WS-TRUST-WORKING-DIR TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-EXECUTION.
           MOVE "execution" TO WS-PB-GROUP
           IF WS-MAX-AI-CREDITS > 0
               MOVE "limits" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-OBJ-OPEN
               MOVE "maxAiCredits" TO WS-PB-KEY
               MOVE WS-MAX-AI-CREDITS TO WS-PB-NUM
               PERFORM PB-OBJ-NUMBER
               PERFORM PB-OBJ-CLOSE
           END-IF
           IF WS-AUTOPILOT-CONTINUE NOT = SPACE
              OR WS-AUTOPILOT-MAX-CONT > 0
               MOVE "autopilot" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               PERFORM PB-OBJ-OPEN
               MOVE "continuationEnabled" TO WS-PB-KEY
               MOVE WS-AUTOPILOT-CONTINUE TO WS-PB-FLAG
               PERFORM PB-OBJ-FLAG
               MOVE "maxContinues" TO WS-PB-KEY
               MOVE WS-AUTOPILOT-MAX-CONT TO WS-PB-NUM
               PERFORM PB-OBJ-NUMBER
               PERFORM PB-OBJ-CLOSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-INSTRUCTIONS.
           MOVE "instructions" TO WS-PB-GROUP
           IF WS-INSTR-DIR-COUNT > 0
               MOVE "directories" TO WS-PB-KEY
               PERFORM PB-MEMBER-KEY
               MOVE "[" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
               PERFORM VARYING WS-PB-ENTRY FROM 1 BY 1
                       UNTIL WS-PB-ENTRY > WS-INSTR-DIR-COUNT
                          OR WS-PB-ENTRY > 10
                   IF WS-PB-ENTRY > 1
                       MOVE "," TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                   END-IF
                   MOVE WS-INSTR-DIR-PATH(WS-PB-ENTRY) TO WS-PB-TEXT
                   PERFORM PB-PUT-STRING
               END-PERFORM
               MOVE "]" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           IF CLIENT-MODE-EMPTY
               MOVE "onDemandDiscovery" TO WS-PB-KEY
               PERFORM PB-MEMBER-FALSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-HOOKS.
           MOVE "hooks" TO WS-PB-GROUP
           IF WS-SUBAGENT-START-PGM NOT = SPACES
              OR WS-SUBAGENT-STOP-PGM NOT = SPACES
               MOVE "hasHandlers" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           IF WS-HOOK-DIRECTORIES NOT = SPACES
               MOVE "directories" TO WS-PB-KEY
               MOVE WS-HOOK-DIRECTORIES TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF CLIENT-MODE-EMPTY
               MOVE "enableRepositoryHooks" TO WS-PB-KEY
               PERFORM PB-MEMBER-FALSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-GIT.
           MOVE "git" TO WS-PB-GROUP
           IF CLIENT-MODE-EMPTY
               MOVE "enableHostGitOperations" TO WS-PB-KEY
               PERFORM PB-MEMBER-FALSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-STATE.
           MOVE "state" TO WS-PB-GROUP
           IF MEMORY-ON
               MOVE "memory" TO WS-PB-KEY
               MOVE '{"enabled":true}' TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           ELSE
               IF CLIENT-MODE-EMPTY
                   MOVE "memory" TO WS-PB-KEY
                   MOVE '{"enabled":false}' TO WS-PB-TEXT
                   PERFORM PB-MEMBER-RAW
               END-IF
           END-IF
           IF WS-MEMORY-STORE-PGM NOT = SPACES
               MOVE "hasMemoryStore" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
           END-IF
           IF CLIENT-MODE-EMPTY
               MOVE "skipEmbeddingRetrieval" TO WS-PB-KEY
               PERFORM PB-MEMBER-TRUE
               MOVE "embeddingCacheStorage" TO WS-PB-KEY
               MOVE "in-memory" TO WS-PB-TEXT
               PERFORM PB-MEMBER-STRING
               MOVE "enableCrossSessionStore" TO WS-PB-KEY
               PERFORM PB-MEMBER-FALSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-CANVASES.
           MOVE "canvases" TO WS-PB-GROUP
           IF WS-CANVAS-PROVIDER NOT = SPACES
               MOVE "provider" TO WS-PB-KEY
               MOVE WS-CANVAS-PROVIDER TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-EXPERIMENTS.
           MOVE "experiments" TO WS-PB-GROUP
           IF WS-FEATURE-FLAG-OVERRIDES NOT = SPACES
               MOVE "flags" TO WS-PB-KEY
               MOVE WS-FEATURE-FLAG-OVERRIDES TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF WS-EXP-ASSIGNMENTS NOT = SPACES
               MOVE "assignments" TO WS-PB-KEY
               MOVE WS-EXP-ASSIGNMENTS TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           IF CLIENT-MODE-EMPTY AND NOT EXPERIMENTAL-ON
               MOVE "enableExperimentalMode" TO WS-PB-KEY
               PERFORM PB-MEMBER-FALSE
           END-IF
           PERFORM PB-END-GROUP
           .

       PB-GROUP-POLICY.
           MOVE "policy" TO WS-PB-GROUP
           IF WS-ALLOWED-MODELS NOT = SPACES
               MOVE "allowedModels" TO WS-PB-KEY
               MOVE WS-ALLOWED-MODELS TO WS-PB-TEXT
               PERFORM PB-MEMBER-RAW
           END-IF
           PERFORM PB-END-GROUP
           .

      *----------------------------------------------------------------*
      * PB-PUT-PROVIDERS / PB-PUT-PROVIDER-MODELS: Named BYOK          *
      * providers and the models they serve.                           *
      *----------------------------------------------------------------*
       PB-PUT-PROVIDERS.
           MOVE "[" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           PERFORM VARYING WS-PB-ENTRY FROM 1 BY 1
                   UNTIL WS-PB-ENTRY > WS-PROVIDER-COUNT
                      OR WS-PB-ENTRY > 4
               IF WS-PB-ENTRY > 1
                   MOVE "," TO WS-PB-CHAR
                   PERFORM PB-PUT-CHAR
               END-IF
               PERFORM PB-OBJ-OPEN
               MOVE "name" TO WS-PB-KEY
               MOVE WS-NP-NAME(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "type" TO WS-PB-KEY
               MOVE WS-NP-TYPE(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "baseUrl" TO WS-PB-KEY
               MOVE WS-NP-BASE-URL(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "apiKey" TO WS-PB-KEY
               MOVE WS-NP-API-KEY(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "apiKeyCommand" TO WS-PB-KEY
               MOVE WS-NP-API-KEY-CMD(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "bearerToken" TO WS-PB-KEY
               MOVE WS-NP-BEARER-TOKEN(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "wireApi" TO WS-PB-KEY
               MOVE WS-NP-WIRE-API(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "transport" TO WS-PB-KEY
               MOVE WS-NP-TRANSPORT(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "modelProvider" TO WS-PB-KEY
               MOVE WS-NP-MODEL-PROV(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               PERFORM PB-OBJ-CLOSE
           END-PERFORM
           MOVE "]" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

       PB-PUT-PROVIDER-MODELS.
           MOVE "[" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           PERFORM VARYING WS-PB-ENTRY FROM 1 BY 1
                   UNTIL WS-PB-ENTRY > WS-PROVIDER-MODEL-COUNT
                      OR WS-PB-ENTRY > 8
               IF WS-PB-ENTRY > 1
                   MOVE "," TO WS-PB-CHAR
                   PERFORM PB-PUT-CHAR
               END-IF
               PERFORM PB-OBJ-OPEN
               MOVE "provider" TO WS-PB-KEY
               MOVE WS-PM-PROVIDER(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "id" TO WS-PB-KEY
               MOVE WS-PM-ID(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "modelId" TO WS-PB-KEY
               MOVE WS-PM-MODEL-ID(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "wireModel" TO WS-PB-KEY
               MOVE WS-PM-WIRE-MODEL(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "name" TO WS-PB-KEY
               MOVE WS-PM-NAME(WS-PB-ENTRY) TO WS-PB-TEXT
               PERFORM PB-OBJ-STRING
               MOVE "maxPromptTokens" TO WS-PB-KEY
               MOVE WS-PM-MAX-PROMPT(WS-PB-ENTRY) TO WS-PB-NUM
               PERFORM PB-OBJ-NUMBER
               MOVE "maxContextWindowTokens" TO WS-PB-KEY
               MOVE WS-PM-MAX-CONTEXT(WS-PB-ENTRY) TO WS-PB-NUM
               PERFORM PB-OBJ-NUMBER
               MOVE "maxOutputTokens" TO WS-PB-KEY
               MOVE WS-PM-MAX-OUTPUT(WS-PB-ENTRY) TO WS-PB-NUM
               PERFORM PB-OBJ-NUMBER
               PERFORM PB-OBJ-CLOSE
           END-PERFORM
           MOVE "]" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *----------------------------------------------------------------*
      * PB-NEW-REGISTRATION-ID: Random version 4 UUID for the GitHub   *
      * token provider registration (WS-GH-TOKEN-REG-ID).              *
      *----------------------------------------------------------------*
       PB-NEW-REGISTRATION-ID.
           ADD 1 TO WS-PB-SERIAL
           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-PB-SEED
           COMPUTE WS-PB-SEED =
               FUNCTION MOD(WS-PB-SEED + WS-PB-SERIAL * 7919,
                            999999937)
           COMPUTE WS-PB-RAND = FUNCTION RANDOM(WS-PB-SEED)
           MOVE SPACES TO WS-GH-TOKEN-REG-ID
           PERFORM VARYING WS-PB-IDX FROM 1 BY 1
                   UNTIL WS-PB-IDX > 36
               EVALUATE WS-PB-IDX
                   WHEN 9
                   WHEN 14
                   WHEN 19
                   WHEN 24
                       MOVE "-" TO WS-GH-TOKEN-REG-ID(WS-PB-IDX:1)
                   WHEN 15
                       MOVE "4" TO WS-GH-TOKEN-REG-ID(WS-PB-IDX:1)
                   WHEN 20
                       COMPUTE WS-PB-HEX-IDX =
                           FUNCTION INTEGER(FUNCTION RANDOM * 4) + 9
                       MOVE WS-PB-HEX-DIGITS(WS-PB-HEX-IDX:1)
                           TO WS-GH-TOKEN-REG-ID(WS-PB-IDX:1)
                   WHEN OTHER
                       COMPUTE WS-PB-HEX-IDX =
                           FUNCTION INTEGER(FUNCTION RANDOM * 16) + 1
                       MOVE WS-PB-HEX-DIGITS(WS-PB-HEX-IDX:1)
                           TO WS-GH-TOKEN-REG-ID(WS-PB-IDX:1)
               END-EVALUATE
           END-PERFORM
           .

      *----------------------------------------------------------------*
      * JSON writers. Keys come from WS-PB-KEY, string and raw values  *
      * from WS-PB-TEXT, numbers from WS-PB-NUM and tri-state flags    *
      * ("Y", "N" or SPACE) from WS-PB-FLAG.                           *
      *----------------------------------------------------------------*
       PB-ROOT-KEY.
           IF WS-PB-ROOT-COUNT > 0
               MOVE "," TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           ADD 1 TO WS-PB-ROOT-COUNT
           PERFORM PB-PUT-KEY
           .

       PB-ROOT-TRUE.
           PERFORM PB-ROOT-KEY
           PERFORM PB-PUT-TRUE
           .

       PB-ROOT-STRING.
           PERFORM PB-ROOT-KEY
           PERFORM PB-PUT-STRING
           .

       PB-ROOT-RAW.
           PERFORM PB-ROOT-KEY
           PERFORM PB-PUT-RAW
           .

      *    Opens WS-PB-GROUP on its first member, then writes "key":.
       PB-MEMBER-KEY.
           IF PB-GROUP-IS-CLOSED
               IF WS-PB-ROOT-COUNT > 0
                   MOVE "," TO WS-PB-CHAR
                   PERFORM PB-PUT-CHAR
               END-IF
               ADD 1 TO WS-PB-ROOT-COUNT
               STRING '"' FUNCTION TRIM(WS-PB-GROUP) '":{'
                   DELIMITED SIZE INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
               SET PB-GROUP-IS-OPEN TO TRUE
           ELSE
               MOVE "," TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           PERFORM PB-PUT-KEY
           .

       PB-END-GROUP.
           IF PB-GROUP-IS-OPEN
               MOVE "}" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
               SET PB-GROUP-IS-CLOSED TO TRUE
           END-IF
           .

       PB-MEMBER-TRUE.
           PERFORM PB-MEMBER-KEY
           PERFORM PB-PUT-TRUE
           .

       PB-MEMBER-FALSE.
           PERFORM PB-MEMBER-KEY
           PERFORM PB-PUT-FALSE
           .

       PB-MEMBER-STRING.
           PERFORM PB-MEMBER-KEY
           PERFORM PB-PUT-STRING
           .

       PB-MEMBER-RAW.
           PERFORM PB-MEMBER-KEY
           PERFORM PB-PUT-RAW
           .

       PB-OBJ-OPEN.
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           MOVE 1 TO WS-PB-OBJ-FIRST
           .

       PB-OBJ-CLOSE.
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

       PB-OBJ-KEY.
           IF WS-PB-OBJ-FIRST = 1
               MOVE 0 TO WS-PB-OBJ-FIRST
           ELSE
               MOVE "," TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           PERFORM PB-PUT-KEY
           .

      *    Object fields are skipped when blank, zero or unset.
       PB-OBJ-STRING.
           IF WS-PB-TEXT NOT = SPACES
               PERFORM PB-OBJ-KEY
               PERFORM PB-PUT-STRING
           END-IF
           .

       PB-OBJ-NUMBER.
           IF WS-PB-NUM > 0
               PERFORM PB-OBJ-KEY
               PERFORM PB-PUT-NUMBER
           END-IF
           .

       PB-OBJ-FLAG.
           EVALUATE WS-PB-FLAG
               WHEN "Y"
                   PERFORM PB-OBJ-KEY
                   PERFORM PB-PUT-TRUE
               WHEN "N"
                   PERFORM PB-OBJ-KEY
                   PERFORM PB-PUT-FALSE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

       PB-PUT-KEY.
           STRING '"' FUNCTION TRIM(WS-PB-KEY) '":'
               DELIMITED SIZE INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

       PB-PUT-TRUE.
           STRING "true" DELIMITED SIZE INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

       PB-PUT-FALSE.
           STRING "false" DELIMITED SIZE INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

       PB-PUT-NULL.
           STRING "null" DELIMITED SIZE INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

       PB-PUT-NUMBER.
           MOVE WS-PB-NUM TO WS-PB-NUM-ED
           STRING FUNCTION TRIM(WS-PB-NUM-ED)
               DELIMITED SIZE INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

      *    Raw JSON value (object, array, number) from WS-PB-TEXT.
       PB-PUT-RAW.
           IF WS-PB-TEXT = SPACES
               PERFORM PB-PUT-NULL
           ELSE
               STRING FUNCTION TRIM(WS-PB-TEXT)
                   DELIMITED SIZE INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF
           .

      *    JSON string from WS-PB-TEXT (trailing blanks dropped) with
      *    quotes, backslashes and control characters escaped.
       PB-PUT-STRING.
           MOVE 0 TO WS-PB-LEN
           IF WS-PB-TEXT NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-PB-TEXT TRAILING))
                   TO WS-PB-LEN
           END-IF
           MOVE '"' TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           PERFORM VARYING WS-PB-IDX FROM 1 BY 1
                   UNTIL WS-PB-IDX > WS-PB-LEN
               MOVE WS-PB-TEXT(WS-PB-IDX:1) TO WS-PB-CHAR
               EVALUATE TRUE
                   WHEN WS-PB-CHAR = '"' OR WS-PB-CHAR = "\"
                       PERFORM PB-PUT-ESCAPE
                       PERFORM PB-PUT-CHAR
                   WHEN WS-PB-CHAR = X"0A"
                       PERFORM PB-PUT-ESCAPE
                       MOVE "n" TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                   WHEN WS-PB-CHAR = X"0D"
                       PERFORM PB-PUT-ESCAPE
                       MOVE "r" TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                   WHEN WS-PB-CHAR = X"09"
                       PERFORM PB-PUT-ESCAPE
                       MOVE "t" TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                   WHEN WS-PB-CHAR < SPACE
                       CONTINUE
                   WHEN OTHER
                       PERFORM PB-PUT-CHAR
               END-EVALUATE
           END-PERFORM
           MOVE '"' TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

       PB-PUT-ESCAPE.
           IF WS-PARAMS-PTR <= FUNCTION LENGTH(WS-PARAMS-BUFFER)
               MOVE "\" TO WS-PARAMS-BUFFER(WS-PARAMS-PTR:1)
               ADD 1 TO WS-PARAMS-PTR
           END-IF
           .

       PB-PUT-CHAR.
           IF WS-PARAMS-PTR <= FUNCTION LENGTH(WS-PARAMS-BUFFER)
               MOVE WS-PB-CHAR TO WS-PARAMS-BUFFER(WS-PARAMS-PTR:1)
               ADD 1 TO WS-PARAMS-PTR
           END-IF
           .
