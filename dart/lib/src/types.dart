// Copyright (c) Microsoft Corporation. All rights reserved.

/// Type definitions for the Copilot Dart SDK.
///
/// These types mirror the TypeScript SDK types and represent the JSON-RPC
/// protocol messages exchanged with the Copilot CLI server.

// ---------------------------------------------------------------------------
// Connection State
// ---------------------------------------------------------------------------

/// Connection state of the client to the CLI server.
enum ConnectionState {
  disconnected,
  connecting,
  connected,
  error,
}

// ---------------------------------------------------------------------------
// Tool Result Types
// ---------------------------------------------------------------------------

/// Result type for tool execution.
enum ToolResultType {
  success,
  failure,
  rejected,
  denied,
  timeout,
}

/// Extension to convert [ToolResultType] to/from JSON string.
extension ToolResultTypeJson on ToolResultType {
  String toJson() => name;

  static ToolResultType fromJson(String value) {
    return ToolResultType.values.firstWhere(
      (e) => e.name == value,
      orElse: () => ToolResultType.failure,
    );
  }
}

/// Binary result attached to a tool result.
class ToolBinaryResult {
  final String data;
  final String mimeType;
  final String type;
  final String? description;

  const ToolBinaryResult({
    required this.data,
    required this.mimeType,
    required this.type,
    this.description,
  });

  Map<String, dynamic> toJson() => {
        'data': data,
        'mimeType': mimeType,
        'type': type,
        if (description != null) 'description': description,
      };

  factory ToolBinaryResult.fromJson(Map<String, dynamic> json) {
    return ToolBinaryResult(
      data: json['data'] as String,
      mimeType: json['mimeType'] as String,
      type: json['type'] as String,
      description: json['description'] as String?,
    );
  }
}

/// Structured result returned from a tool handler.
class ToolResultObject {
  final String textResultForLlm;
  final List<ToolBinaryResult>? binaryResultsForLlm;
  final ToolResultType resultType;
  final String? error;
  final String? sessionLog;
  final Map<String, dynamic>? toolTelemetry;

  const ToolResultObject({
    required this.textResultForLlm,
    required this.resultType,
    this.binaryResultsForLlm,
    this.error,
    this.sessionLog,
    this.toolTelemetry,
  });

  Map<String, dynamic> toJson() => {
        'textResultForLlm': textResultForLlm,
        'resultType': resultType.toJson(),
        if (binaryResultsForLlm != null)
          'binaryResultsForLlm':
              binaryResultsForLlm!.map((b) => b.toJson()).toList(),
        if (error != null) 'error': error,
        if (sessionLog != null) 'sessionLog': sessionLog,
        if (toolTelemetry != null) 'toolTelemetry': toolTelemetry,
      };

  factory ToolResultObject.fromJson(Map<String, dynamic> json) {
    return ToolResultObject(
      textResultForLlm: json['textResultForLlm'] as String,
      resultType: ToolResultTypeJson.fromJson(json['resultType'] as String),
      binaryResultsForLlm: (json['binaryResultsForLlm'] as List<dynamic>?)
          ?.map((b) => ToolBinaryResult.fromJson(b as Map<String, dynamic>))
          .toList(),
      error: json['error'] as String?,
      sessionLog: json['sessionLog'] as String?,
      toolTelemetry: json['toolTelemetry'] as Map<String, dynamic>?,
    );
  }

  /// Returns true if [value] is shaped like a [ToolResultObject].
  static bool isToolResultObject(dynamic value) {
    return value is Map<String, dynamic> &&
        value.containsKey('textResultForLlm') &&
        value['textResultForLlm'] is String &&
        value.containsKey('resultType');
  }
}

// ---------------------------------------------------------------------------
// Trace Context (W3C distributed tracing)
// ---------------------------------------------------------------------------

/// W3C Trace Context propagation data.
class TraceContext {
  final String? traceparent;
  final String? tracestate;

  const TraceContext({this.traceparent, this.tracestate});

  Map<String, dynamic> toJson() => {
        if (traceparent != null) 'traceparent': traceparent,
        if (tracestate != null) 'tracestate': tracestate,
      };
}

/// Callback that returns the current W3C Trace Context.
typedef TraceContextProvider = Future<TraceContext> Function();

// ---------------------------------------------------------------------------
// Tool Invocation
// ---------------------------------------------------------------------------

/// Context passed to a tool handler when a tool is called.
class ToolInvocation {
  final String sessionId;
  final String toolCallId;
  final String toolName;
  final dynamic arguments;

  const ToolInvocation({
    required this.sessionId,
    required this.toolCallId,
    required this.toolName,
    this.arguments,
  });
}

/// Handler function signature for tool calls.
///
/// Receives the parsed arguments and the [ToolInvocation] context.
/// Returns a value that will be sent back to the CLI server.
typedef ToolHandler = Future<dynamic> Function(
  dynamic args,
  ToolInvocation invocation,
);

// ---------------------------------------------------------------------------
// Tool Definition
// ---------------------------------------------------------------------------

/// A tool definition that can be registered with a session.
class Tool {
  /// Unique name of the tool.
  final String name;

  /// Human-readable description of the tool.
  final String? description;

  /// JSON Schema describing the tool parameters.
  final Map<String, dynamic>? parameters;

  /// Handler function invoked when the tool is called.
  final ToolHandler handler;

  /// When true, repeating a call with the same arguments has no additional
  /// effect beyond the first call (true of any tool without side effects).
  /// The runtime may then safely repeat a call whose outcome was lost.
  final bool? idempotent;

  const Tool({
    required this.name,
    this.description,
    this.parameters,
    required this.handler,
    this.idempotent,
  });

  /// Serialize the tool definition for the JSON-RPC session.create call.
  Map<String, dynamic> toJson() => {
        'name': name,
        if (description != null) 'description': description,
        if (parameters != null) 'parameters': parameters,
        if (idempotent != null) 'idempotent': idempotent,
      };
}

// ---------------------------------------------------------------------------
// Cloud Session Types
// ---------------------------------------------------------------------------

/// GitHub repository metadata associated with a cloud session.
class CloudSessionRepository {
  final String owner;
  final String name;
  final String? branch;

  const CloudSessionRepository({
    required this.owner,
    required this.name,
    this.branch,
  });

  Map<String, dynamic> toJson() => {
        'owner': owner,
        'name': name,
        if (branch != null) 'branch': branch,
      };

  factory CloudSessionRepository.fromJson(Map<String, dynamic> json) {
    return CloudSessionRepository(
      owner: json['owner'] as String,
      name: json['name'] as String,
      branch: json['branch'] as String?,
    );
  }
}

/// Options for creating a remote session in the cloud.
class CloudSessionOptions {
  final CloudSessionRepository? repository;

  const CloudSessionOptions({this.repository});

  Map<String, dynamic> toJson() => {
        if (repository != null) 'repository': repository!.toJson(),
      };

  factory CloudSessionOptions.fromJson(Map<String, dynamic> json) {
    return CloudSessionOptions(
      repository: json['repository'] == null
          ? null
          : CloudSessionRepository.fromJson(
              json['repository'] as Map<String, dynamic>,
            ),
    );
  }
}

// ---------------------------------------------------------------------------
// Canvas Types
// ---------------------------------------------------------------------------

/// A single agent-callable action contributed by a canvas.
class CanvasAction {
  final String name;
  final String description;
  final Map<String, dynamic>? inputSchema;

  const CanvasAction({
    required this.name,
    required this.description,
    this.inputSchema,
  });

  Map<String, dynamic> toJson() => {
        'name': name,
        'description': description,
        if (inputSchema != null) 'inputSchema': inputSchema,
      };

  factory CanvasAction.fromJson(Map<String, dynamic> json) {
    return CanvasAction(
      name: json['name'] as String,
      description: json['description'] as String,
      inputSchema: json['inputSchema'] as Map<String, dynamic>?,
    );
  }
}

/// Declarative metadata for a single canvas.
class CanvasDeclaration {
  final String id;
  final String displayName;
  final String description;
  final Map<String, dynamic>? inputSchema;
  final List<CanvasAction>? actions;

  const CanvasDeclaration({
    required this.id,
    required this.displayName,
    required this.description,
    this.inputSchema,
    this.actions,
  });

  Map<String, dynamic> toJson() => {
        'id': id,
        'displayName': displayName,
        'description': description,
        if (inputSchema != null) 'inputSchema': inputSchema,
        if (actions != null) 'actions': actions!.map((a) => a.toJson()).toList(),
      };

  factory CanvasDeclaration.fromJson(Map<String, dynamic> json) {
    return CanvasDeclaration(
      id: json['id'] as String,
      displayName: json['displayName'] as String,
      description: json['description'] as String,
      inputSchema: json['inputSchema'] as Map<String, dynamic>?,
      actions: (json['actions'] as List<dynamic>?)
          ?.map((a) => CanvasAction.fromJson(a as Map<String, dynamic>))
          .toList(),
    );
  }
}

/// Response returned from a canvas open handler.
class CanvasOpenResponse {
  final String? url;
  final String? title;
  final String? status;

  const CanvasOpenResponse({this.url, this.title, this.status});

  Map<String, dynamic> toJson() => {
        if (url != null) 'url': url,
        if (title != null) 'title': title,
        if (status != null) 'status': status,
      };

  factory CanvasOpenResponse.fromJson(Map<String, dynamic> json) {
    return CanvasOpenResponse(
      url: json['url'] as String?,
      title: json['title'] as String?,
      status: json['status'] as String?,
    );
  }
}

/// Host capability details passed to canvas callbacks.
class CanvasHostCapabilities {
  final bool canvases;

  const CanvasHostCapabilities({this.canvases = false});

  Map<String, dynamic> toJson() => {
        'canvases': canvases,
      };

  factory CanvasHostCapabilities.fromJson(Map<String, dynamic> json) {
    return CanvasHostCapabilities(
      canvases: json['canvases'] as bool? ?? false,
    );
  }
}

/// Host context passed to canvas callbacks.
class CanvasHostContext {
  final CanvasHostCapabilities capabilities;

  const CanvasHostContext({
    this.capabilities = const CanvasHostCapabilities(),
  });

  Map<String, dynamic> toJson() => {
        'capabilities': capabilities.toJson(),
      };

  factory CanvasHostContext.fromJson(Map<String, dynamic> json) {
    return CanvasHostContext(
      capabilities: json['capabilities'] == null
          ? const CanvasHostCapabilities()
          : CanvasHostCapabilities.fromJson(
              json['capabilities'] as Map<String, dynamic>,
            ),
    );
  }
}

/// Context handed to a canvas open handler.
class CanvasOpenContext {
  final String sessionId;
  final String extensionId;
  final String canvasId;
  final String instanceId;
  final dynamic input;
  final CanvasHostContext? host;

  const CanvasOpenContext({
    required this.sessionId,
    required this.extensionId,
    required this.canvasId,
    required this.instanceId,
    required this.input,
    this.host,
  });

  factory CanvasOpenContext.fromJson(Map<String, dynamic> json) {
    return CanvasOpenContext(
      sessionId: json['sessionId'] as String,
      extensionId: json['extensionId'] as String,
      canvasId: json['canvasId'] as String,
      instanceId: json['instanceId'] as String,
      input: json['input'],
      host: json['host'] == null
          ? null
          : CanvasHostContext.fromJson(json['host'] as Map<String, dynamic>),
    );
  }
}

/// Context handed to a canvas action handler.
class CanvasActionContext {
  final String sessionId;
  final String extensionId;
  final String canvasId;
  final String instanceId;
  final String actionName;
  final dynamic input;
  final CanvasHostContext? host;

  const CanvasActionContext({
    required this.sessionId,
    required this.extensionId,
    required this.canvasId,
    required this.instanceId,
    required this.actionName,
    required this.input,
    this.host,
  });

  factory CanvasActionContext.fromJson(Map<String, dynamic> json) {
    return CanvasActionContext(
      sessionId: json['sessionId'] as String,
      extensionId: json['extensionId'] as String,
      canvasId: json['canvasId'] as String,
      instanceId: json['instanceId'] as String,
      actionName: json['actionName'] as String,
      input: json['input'],
      host: json['host'] == null
          ? null
          : CanvasHostContext.fromJson(json['host'] as Map<String, dynamic>),
    );
  }
}

/// Context handed to a canvas lifecycle handler.
class CanvasLifecycleContext {
  final String sessionId;
  final String extensionId;
  final String canvasId;
  final String instanceId;
  final CanvasHostContext? host;

  const CanvasLifecycleContext({
    required this.sessionId,
    required this.extensionId,
    required this.canvasId,
    required this.instanceId,
    this.host,
  });

  factory CanvasLifecycleContext.fromJson(Map<String, dynamic> json) {
    return CanvasLifecycleContext(
      sessionId: json['sessionId'] as String,
      extensionId: json['extensionId'] as String,
      canvasId: json['canvasId'] as String,
      instanceId: json['instanceId'] as String,
      host: json['host'] == null
          ? null
          : CanvasHostContext.fromJson(json['host'] as Map<String, dynamic>),
    );
  }
}

// ---------------------------------------------------------------------------
// System Message Configuration
// ---------------------------------------------------------------------------

/// System message in append mode (default).
/// Uses the CLI foundation with optional appended content.
class SystemMessageAppendConfig {
  final String? mode;
  final String? content;

  const SystemMessageAppendConfig({this.mode = 'append', this.content});

  Map<String, dynamic> toJson() => {
        'mode': mode ?? 'append',
        if (content != null) 'content': content,
      };
}

/// System message in replace mode.
/// Replaces the entire SDK-managed system message.
class SystemMessageReplaceConfig {
  final String mode;
  final String content;

  const SystemMessageReplaceConfig({this.mode = 'replace', required this.content});

  Map<String, dynamic> toJson() => {
        'mode': mode,
        'content': content,
      };
}

// ---------------------------------------------------------------------------
// System Prompt Section Constants
// ---------------------------------------------------------------------------

/// Known system prompt section identifiers for the "customize" mode.
class SystemPromptSection {
  static const String identity = 'identity';
  static const String tone = 'tone';
  static const String toolEfficiency = 'tool_efficiency';
  static const String environmentContext = 'environment_context';
  static const String codeChangeRules = 'code_change_rules';
  static const String guidelines = 'guidelines';
  static const String safety = 'safety';
  static const String toolInstructions = 'tool_instructions';
  static const String customInstructions = 'custom_instructions';
  static const String runtimeInstructions = 'runtime_instructions';
  static const String lastInstructions = 'last_instructions';
}

/// Override action for a system prompt section.
class SectionOverrideAction {
  static const String replace = 'replace';
  static const String remove = 'remove';
  static const String append = 'append';
  static const String prepend = 'prepend';
  static const String transform = 'transform';
}

/// Override operation for a single system prompt section.
class SectionOverride {
  final dynamic action;
  final String? content;

  const SectionOverride({required this.action, this.content});

  Map<String, dynamic> toJson() => {
        'action': action is Function ? SectionOverrideAction.transform : action,
        if (content != null) 'content': content,
      };

  factory SectionOverride.fromJson(Map<String, dynamic> json) {
    return SectionOverride(
      action: json['action'] as String,
      content: json['content'] as String?,
    );
  }
}

/// System message in customize mode — section-level overrides.
class SystemMessageCustomizeConfig {
  final String mode;
  final Map<String, SectionOverride>? sections;
  final String? content;

  const SystemMessageCustomizeConfig({
    this.mode = 'customize',
    this.sections,
    this.content,
  });

  Map<String, dynamic> toJson() => {
        'mode': mode,
        if (sections != null)
          'sections': sections!.map((k, v) => MapEntry(k, v.toJson())),
        if (content != null) 'content': content,
      };
}

// ---------------------------------------------------------------------------
// Permission Types
// ---------------------------------------------------------------------------

/// Permission request from the CLI server.
class PermissionRequest {
  final String kind;
  final String? toolCallId;
  final Map<String, dynamic> extra;

  const PermissionRequest({
    required this.kind,
    this.toolCallId,
    this.extra = const {},
  });

  factory PermissionRequest.fromJson(Map<String, dynamic> json) {
    final kind = json['kind'] as String;
    final toolCallId = json['toolCallId'] as String?;
    final extra = Map<String, dynamic>.from(json)
      ..remove('kind')
      ..remove('toolCallId');
    return PermissionRequest(kind: kind, toolCallId: toolCallId, extra: extra);
  }
}

/// Result of a permission decision.
class PermissionRequestResult {
  /// One of: approved, denied-by-rules,
  /// denied-no-approval-rule-and-could-not-request-from-user,
  /// denied-interactively-by-user
  final String kind;
  final List<dynamic>? rules;

  const PermissionRequestResult({required this.kind, this.rules});

  Map<String, dynamic> toJson() => {
        'kind': kind,
        if (rules != null) 'rules': rules,
      };
}

/// Handler for permission requests.
typedef PermissionHandler = Future<PermissionRequestResult> Function(
  PermissionRequest request,
  SessionInvocationContext context,
);

// ---------------------------------------------------------------------------
// User Input Types
// ---------------------------------------------------------------------------

/// Request for user input from the agent (enables ask_user tool).
class UserInputRequest {
  final String? question;
  final List<String>? choices;
  final bool? allowFreeform;

  /// Runtime ID of the pending request, when the runtime sends it.
  final String? requestId;

  /// Image support advertised by the runtime; null when the answer cannot
  /// carry images (see [UserInputResponse.attachments]).
  final UserInputImageAttachmentConstraints? imageAttachmentConstraints;

  /// The request's opaque `_meta` object, forwarded unchanged when the
  /// runtime sends it.
  final Map<String, dynamic>? meta;

  const UserInputRequest({
    this.question,
    this.choices,
    this.allowFreeform,
    this.requestId,
    this.imageAttachmentConstraints,
    this.meta,
  });

  factory UserInputRequest.fromJson(Map<String, dynamic> json) {
    final constraints = json['imageAttachmentConstraints'];
    final meta = json['_meta'];
    return UserInputRequest(
      question: json['question'] as String?,
      choices: (json['choices'] as List<dynamic>?)?.cast<String>(),
      allowFreeform: json['allowFreeform'] as bool?,
      requestId: json['requestId'] as String?,
      imageAttachmentConstraints: constraints is Map<String, dynamic>
          ? UserInputImageAttachmentConstraints.fromJson(constraints)
          : null,
      meta: meta is Map<String, dynamic> ? meta : null,
    );
  }
}

/// Response to a user input request.
///
/// [attachments] is only valid when the request advertised
/// [UserInputRequest.imageAttachmentConstraints] and is sent only when set.
/// Image-only answers use an empty [answer] and `wasFreeform: true`; the
/// runtime enforces the advertised MIME types, size and count limits.
class UserInputResponse {
  final String answer;
  final bool wasFreeform;

  /// Images attached to the answer, or null to send none.
  final List<UserInputImageAttachment>? attachments;

  const UserInputResponse({
    required this.answer,
    required this.wasFreeform,
    this.attachments,
  });

  Map<String, dynamic> toJson() => {
        'answer': answer,
        'wasFreeform': wasFreeform,
        if (attachments != null)
          'attachments': attachments!.map((a) => a.toJson()).toList(),
      };
}

/// Handler for user input requests.
typedef UserInputHandler = Future<UserInputResponse> Function(
  UserInputRequest request,
  SessionInvocationContext context,
);

// ---------------------------------------------------------------------------
// Session Invocation Context (shared by permission / input / hooks)
// ---------------------------------------------------------------------------

/// Minimal context passed to permission, user-input and hook handlers.
class SessionInvocationContext {
  final String sessionId;
  const SessionInvocationContext({required this.sessionId});
}

// ---------------------------------------------------------------------------
// Hook Types
// ---------------------------------------------------------------------------

/// Base input fields shared by all hooks.
class BaseHookInput {
  final int timestamp;
  final String cwd;

  const BaseHookInput({required this.timestamp, required this.cwd});

  factory BaseHookInput.fromJson(Map<String, dynamic> json) {
    return BaseHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
    );
  }
}

/// Input for pre-tool-use hook.
class PreToolUseHookInput extends BaseHookInput {
  final String toolName;
  final dynamic toolArgs;

  PreToolUseHookInput({
    required super.timestamp,
    required super.cwd,
    required this.toolName,
    this.toolArgs,
  });

  factory PreToolUseHookInput.fromJson(Map<String, dynamic> json) {
    return PreToolUseHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      toolName: json['toolName'] as String,
      toolArgs: json['toolArgs'],
    );
  }
}

/// Output for pre-tool-use hook.
class PreToolUseHookOutput {
  /// One of: allow, deny, ask (or null to use default)
  final String? permissionDecision;
  final String? permissionDecisionReason;
  final dynamic modifiedArgs;
  final String? additionalContext;
  final bool? suppressOutput;

  /// Opaque decision context returned with the permission reply.
  final Map<String, dynamic>? decisionContext;

  const PreToolUseHookOutput({
    this.permissionDecision,
    this.permissionDecisionReason,
    this.modifiedArgs,
    this.additionalContext,
    this.suppressOutput,
    this.decisionContext,
  });

  Map<String, dynamic> toJson() => {
        if (permissionDecision != null) 'permissionDecision': permissionDecision,
        if (permissionDecisionReason != null)
          'permissionDecisionReason': permissionDecisionReason,
        if (modifiedArgs != null) 'modifiedArgs': modifiedArgs,
        if (additionalContext != null) 'additionalContext': additionalContext,
        if (suppressOutput != null) 'suppressOutput': suppressOutput,
        if (decisionContext != null) 'decisionContext': decisionContext,
      };
}

typedef PreToolUseHandler = Future<PreToolUseHookOutput?> Function(
  PreToolUseHookInput input,
  SessionInvocationContext context,
);

/// Input for post-tool-use hook.
class PostToolUseHookInput extends BaseHookInput {
  final String toolName;
  final dynamic toolArgs;
  final ToolResultObject toolResult;

  PostToolUseHookInput({
    required super.timestamp,
    required super.cwd,
    required this.toolName,
    this.toolArgs,
    required this.toolResult,
  });

  factory PostToolUseHookInput.fromJson(Map<String, dynamic> json) {
    return PostToolUseHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      toolName: json['toolName'] as String,
      toolArgs: json['toolArgs'],
      toolResult:
          ToolResultObject.fromJson(json['toolResult'] as Map<String, dynamic>),
    );
  }
}

/// Output for post-tool-use hook.
class PostToolUseHookOutput {
  final ToolResultObject? modifiedResult;
  final String? additionalContext;
  final bool? suppressOutput;

  const PostToolUseHookOutput({
    this.modifiedResult,
    this.additionalContext,
    this.suppressOutput,
  });

  Map<String, dynamic> toJson() => {
        if (modifiedResult != null) 'modifiedResult': modifiedResult!.toJson(),
        if (additionalContext != null) 'additionalContext': additionalContext,
        if (suppressOutput != null) 'suppressOutput': suppressOutput,
      };
}

typedef PostToolUseHandler = Future<PostToolUseHookOutput?> Function(
  PostToolUseHookInput input,
  SessionInvocationContext context,
);

/// Input for user-prompt-submitted hook.
class UserPromptSubmittedHookInput extends BaseHookInput {
  final String prompt;

  UserPromptSubmittedHookInput({
    required super.timestamp,
    required super.cwd,
    required this.prompt,
  });

  factory UserPromptSubmittedHookInput.fromJson(Map<String, dynamic> json) {
    return UserPromptSubmittedHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      prompt: json['prompt'] as String,
    );
  }
}

/// Output for user-prompt-submitted hook.
class UserPromptSubmittedHookOutput {
  final String? modifiedPrompt;
  final String? additionalContext;
  final bool? suppressOutput;

  const UserPromptSubmittedHookOutput({
    this.modifiedPrompt,
    this.additionalContext,
    this.suppressOutput,
  });

  Map<String, dynamic> toJson() => {
        if (modifiedPrompt != null) 'modifiedPrompt': modifiedPrompt,
        if (additionalContext != null) 'additionalContext': additionalContext,
        if (suppressOutput != null) 'suppressOutput': suppressOutput,
      };
}

typedef UserPromptSubmittedHandler
    = Future<UserPromptSubmittedHookOutput?> Function(
  UserPromptSubmittedHookInput input,
  SessionInvocationContext context,
);

/// Input for session-start hook.
class SessionStartHookInput extends BaseHookInput {
  /// One of: startup, resume, new
  final String source;
  final String? initialPrompt;

  SessionStartHookInput({
    required super.timestamp,
    required super.cwd,
    required this.source,
    this.initialPrompt,
  });

  factory SessionStartHookInput.fromJson(Map<String, dynamic> json) {
    return SessionStartHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      source: json['source'] as String,
      initialPrompt: json['initialPrompt'] as String?,
    );
  }
}

/// Output for session-start hook.
class SessionStartHookOutput {
  final String? additionalContext;
  final Map<String, dynamic>? modifiedConfig;

  const SessionStartHookOutput({this.additionalContext, this.modifiedConfig});

  Map<String, dynamic> toJson() => {
        if (additionalContext != null) 'additionalContext': additionalContext,
        if (modifiedConfig != null) 'modifiedConfig': modifiedConfig,
      };
}

typedef SessionStartHandler = Future<SessionStartHookOutput?> Function(
  SessionStartHookInput input,
  SessionInvocationContext context,
);

/// Input for session-end hook.
class SessionEndHookInput extends BaseHookInput {
  /// One of: complete, error, abort, timeout, user_exit
  final String reason;
  final String? finalMessage;
  final String? error;

  SessionEndHookInput({
    required super.timestamp,
    required super.cwd,
    required this.reason,
    this.finalMessage,
    this.error,
  });

  factory SessionEndHookInput.fromJson(Map<String, dynamic> json) {
    return SessionEndHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      reason: json['reason'] as String,
      finalMessage: json['finalMessage'] as String?,
      error: json['error'] as String?,
    );
  }
}

/// Output for session-end hook.
class SessionEndHookOutput {
  final bool? suppressOutput;
  final List<String>? cleanupActions;
  final String? sessionSummary;

  const SessionEndHookOutput({
    this.suppressOutput,
    this.cleanupActions,
    this.sessionSummary,
  });

  Map<String, dynamic> toJson() => {
        if (suppressOutput != null) 'suppressOutput': suppressOutput,
        if (cleanupActions != null) 'cleanupActions': cleanupActions,
        if (sessionSummary != null) 'sessionSummary': sessionSummary,
      };
}

typedef SessionEndHandler = Future<SessionEndHookOutput?> Function(
  SessionEndHookInput input,
  SessionInvocationContext context,
);

/// Input for error-occurred hook.
class ErrorOccurredHookInput extends BaseHookInput {
  final String error;

  /// One of: model_call, tool_execution, system, user_input
  final String errorContext;
  final bool recoverable;

  ErrorOccurredHookInput({
    required super.timestamp,
    required super.cwd,
    required this.error,
    required this.errorContext,
    required this.recoverable,
  });

  factory ErrorOccurredHookInput.fromJson(Map<String, dynamic> json) {
    return ErrorOccurredHookInput(
      timestamp: json['timestamp'] as int,
      cwd: json['cwd'] as String,
      error: json['error'] as String,
      errorContext: json['errorContext'] as String,
      recoverable: json['recoverable'] as bool,
    );
  }
}

/// Output for error-occurred hook.
class ErrorOccurredHookOutput {
  final bool? suppressOutput;

  /// One of: retry, skip, abort
  final String? errorHandling;
  final int? retryCount;
  final String? userNotification;

  const ErrorOccurredHookOutput({
    this.suppressOutput,
    this.errorHandling,
    this.retryCount,
    this.userNotification,
  });

  Map<String, dynamic> toJson() => {
        if (suppressOutput != null) 'suppressOutput': suppressOutput,
        if (errorHandling != null) 'errorHandling': errorHandling,
        if (retryCount != null) 'retryCount': retryCount,
        if (userNotification != null) 'userNotification': userNotification,
      };
}

typedef ErrorOccurredHandler = Future<ErrorOccurredHookOutput?> Function(
  ErrorOccurredHookInput input,
  SessionInvocationContext context,
);

/// Configuration for session hooks.
class SessionHooks {
  final PreToolUseHandler? onPreToolUse;
  final PostToolUseHandler? onPostToolUse;
  final PreMcpToolCallHandler? onPreMcpToolCall;
  final UserPromptSubmittedHandler? onUserPromptSubmitted;

  /// Invoked after the user prompt has been transformed, before submission.
  final UserPromptSubmittedHandler? onUserPromptTransformed;
  final SessionStartHandler? onSessionStart;
  final SessionEndHandler? onSessionEnd;
  final ErrorOccurredHandler? onErrorOccurred;

  /// Invoked before a sub-agent's first turn (hook type `subagentStart`).
  /// Return [SubagentStartHookOutput.additionalContext] to prepend context to
  /// the child's prompt.
  final SubagentStartHandler? onSubagentStart;

  /// Invoked after a sub-agent completes a turn (hook type `subagentStop`).
  /// Return `decision: 'block'` with a reason to continue the child, or a
  /// `modifiedResponse` to rewrite what the parent sees.
  final SubagentStopHandler? onSubagentStop;

  const SessionHooks({
    this.onPreToolUse,
    this.onPostToolUse,
    this.onPreMcpToolCall,
    this.onUserPromptSubmitted,
    this.onUserPromptTransformed,
    this.onSessionStart,
    this.onSessionEnd,
    this.onErrorOccurred,
    this.onSubagentStart,
    this.onSubagentStop,
  });

  /// Returns true if any hook handler is set.
  bool get hasAny =>
      onPreToolUse != null ||
      onPostToolUse != null ||
      onPreMcpToolCall != null ||
      onUserPromptSubmitted != null ||
      onUserPromptTransformed != null ||
      onSessionStart != null ||
      onSessionEnd != null ||
      onErrorOccurred != null ||
      onSubagentStart != null ||
      onSubagentStop != null;
}

/// Input for the `subagentStart` hook, fired before a sub-agent's first turn.
class SubagentStartHookInput extends BaseHookInput {
  final String transcriptPath;
  final String agentName;
  final String? agentDisplayName;
  final String? agentDescription;

  SubagentStartHookInput({
    required super.timestamp,
    required super.cwd,
    required this.transcriptPath,
    required this.agentName,
    this.agentDisplayName,
    this.agentDescription,
  });

  factory SubagentStartHookInput.fromJson(Map<String, dynamic> json) {
    return SubagentStartHookInput(
      timestamp: (json['timestamp'] as num?)?.toInt() ?? 0,
      cwd: json['cwd'] as String? ?? '',
      transcriptPath: json['transcriptPath'] as String? ?? '',
      agentName: json['agentName'] as String? ?? '',
      agentDisplayName: json['agentDisplayName'] as String?,
      agentDescription: json['agentDescription'] as String?,
    );
  }
}

/// Output for the `subagentStart` hook.
class SubagentStartHookOutput {
  /// Context prepended to the sub-agent's initial prompt.
  final String? additionalContext;

  const SubagentStartHookOutput({this.additionalContext});

  Map<String, dynamic> toJson() => {
        if (additionalContext != null) 'additionalContext': additionalContext,
      };
}

typedef SubagentStartHandler = Future<SubagentStartHookOutput?> Function(
  SubagentStartHookInput input,
  SessionInvocationContext context,
);

/// Input for the `subagentStop` hook, fired after a sub-agent completes a turn.
class SubagentStopHookInput extends SubagentStartHookInput {
  final String? agentId;
  final String agentType;
  final String stopReason;

  /// The sub-agent's last assistant message before any hook rewrite.
  final String response;

  SubagentStopHookInput({
    required super.timestamp,
    required super.cwd,
    required super.transcriptPath,
    required super.agentName,
    super.agentDisplayName,
    super.agentDescription,
    this.agentId,
    required this.agentType,
    required this.stopReason,
    required this.response,
  });

  factory SubagentStopHookInput.fromJson(Map<String, dynamic> json) {
    return SubagentStopHookInput(
      timestamp: (json['timestamp'] as num?)?.toInt() ?? 0,
      cwd: json['cwd'] as String? ?? '',
      transcriptPath: json['transcriptPath'] as String? ?? '',
      agentName: json['agentName'] as String? ?? '',
      agentDisplayName: json['agentDisplayName'] as String?,
      agentDescription: json['agentDescription'] as String?,
      agentId: json['agentId'] as String?,
      agentType: json['agentType'] as String? ?? '',
      stopReason: json['stopReason'] as String? ?? 'end_turn',
      response: json['response'] as String? ?? '',
    );
  }
}

/// Output for the `subagentStop` hook. `decision: 'block'` with a non-empty
/// [reason] continues the child; otherwise [modifiedResponse] replaces the
/// response reported to the parent.
class SubagentStopHookOutput {
  /// `'block'` or `'allow'`.
  final String? decision;
  final String? reason;
  final String? modifiedResponse;

  const SubagentStopHookOutput({
    this.decision,
    this.reason,
    this.modifiedResponse,
  });

  Map<String, dynamic> toJson() => {
        if (decision != null) 'decision': decision,
        if (reason != null) 'reason': reason,
        if (modifiedResponse != null) 'modifiedResponse': modifiedResponse,
      };
}

typedef SubagentStopHandler = Future<SubagentStopHookOutput?> Function(
  SubagentStopHookInput input,
  SessionInvocationContext context,
);

// ---------------------------------------------------------------------------
// MCP Server Configuration
// ---------------------------------------------------------------------------

/// Base MCP server configuration.
abstract class MCPServerConfig {
  /// List of tools to include from this server. [] means none, ['*'] means all.
  final List<String> tools;

  /// "local", "stdio", "http", or "sse"
  final String? type;

  /// Optional timeout in milliseconds for tool calls.
  final int? timeout;

  const MCPServerConfig({required this.tools, this.type, this.timeout});

  Map<String, dynamic> toJson();
}

/// Configuration for a local / stdio MCP server.
class MCPLocalServerConfig extends MCPServerConfig {
  final String command;
  final List<String> args;
  final Map<String, String>? env;
  final String? cwd;

  const MCPLocalServerConfig({
    required super.tools,
    super.type,
    super.timeout,
    required this.command,
    required this.args,
    this.env,
    this.cwd,
  });

  @override
  Map<String, dynamic> toJson() => {
        'tools': tools,
        if (type != null) 'type': type,
        if (timeout != null) 'timeout': timeout,
        'command': command,
        'args': args,
        if (env != null) 'env': env,
        if (cwd != null) 'cwd': cwd,
      };
}

/// Configuration for a remote MCP server (HTTP or SSE).
class MCPRemoteServerConfig extends MCPServerConfig {
  final String url;
  final Map<String, String>? headers;

  const MCPRemoteServerConfig({
    required super.tools,
    required String super.type,
    super.timeout,
    required this.url,
    this.headers,
  });

  @override
  Map<String, dynamic> toJson() => {
        'tools': tools,
        'type': type,
        if (timeout != null) 'timeout': timeout,
        'url': url,
        if (headers != null) 'headers': headers,
      };
}

// ---------------------------------------------------------------------------
// Custom Agent Configuration
// ---------------------------------------------------------------------------

/// Configuration for a custom agent.
class CustomAgentConfig {
  final String name;
  final String? displayName;
  final String? description;
  final List<String>? tools;
  final String prompt;
  final Map<String, MCPServerConfig>? mcpServers;
  final bool? infer;

  /// List of skill names to preload into this agent's context.
  final List<String>? skills;

  /// JSON schema describing the arguments accepted by an agent factory.
  final Map<String, dynamic>? argsSchema;

  const CustomAgentConfig({
    required this.name,
    this.displayName,
    this.description,
    this.tools,
    required this.prompt,
    this.mcpServers,
    this.infer,
    this.skills,
    this.argsSchema,
  });

  Map<String, dynamic> toJson() => {
        'name': name,
        if (displayName != null) 'displayName': displayName,
        if (description != null) 'description': description,
        if (tools != null) 'tools': tools,
        'prompt': prompt,
        if (mcpServers != null)
          'mcpServers':
              mcpServers!.map((k, v) => MapEntry(k, v.toJson())),
        if (infer != null) 'infer': infer,
        if (skills != null) 'skills': skills,
        if (argsSchema != null) 'argsSchema': argsSchema,
      };
}

// ---------------------------------------------------------------------------
// Infinite Session Configuration
// ---------------------------------------------------------------------------

/// Configuration for infinite sessions with automatic context compaction.
class InfiniteSessionConfig {
  final bool? enabled;
  final double? backgroundCompactionThreshold;
  final double? bufferExhaustionThreshold;

  const InfiniteSessionConfig({
    this.enabled,
    this.backgroundCompactionThreshold,
    this.bufferExhaustionThreshold,
  });

  Map<String, dynamic> toJson() => {
        if (enabled != null) 'enabled': enabled,
        if (backgroundCompactionThreshold != null)
          'backgroundCompactionThreshold': backgroundCompactionThreshold,
        if (bufferExhaustionThreshold != null)
          'bufferExhaustionThreshold': bufferExhaustionThreshold,
      };
}

// ---------------------------------------------------------------------------
// Provider Configuration (BYOK)
// ---------------------------------------------------------------------------

/// Configuration for a custom API provider (legacy single BYOK provider).
///
/// SDK protocol 4 no longer accepts a single `provider`: the client sends it
/// as a `"default"` named provider (`inference.providers`) with
/// `inference.defaultProvider = "default"` and Copilot API models disabled
/// (`github.capi.enabled = false`). The model fields ([modelId],
/// [wireModel], [maxPromptTokens], [maxContextWindowTokens],
/// [maxOutputTokens]) become an `inference.models` entry for the session
/// model.
class ProviderConfig {
  /// One of: openai, azure, anthropic
  final String? type;

  /// One of: completions, responses
  final String? wireApi;

  final String baseUrl;
  final String? apiKey;

  /// Command whose stdout is used as the API key for each provider request.
  final String? apiKeyCommand;
  final String? bearerToken;
  final AzureProviderOptions? azure;

  /// Product serving the model (for example "ollama" or "lm_studio"),
  /// reported in telemetry.
  final String? modelProvider;

  /// Transport: "http" (default) or "websockets".
  final String? transport;

  /// Custom HTTP headers sent with every provider request.
  final Map<String, String>? headers;

  /// Well-known base model id used for capability lookup.
  final String? modelId;

  /// Model name sent to the provider API.
  final String? wireModel;
  final int? maxPromptTokens;
  final int? maxContextWindowTokens;
  final int? maxOutputTokens;

  const ProviderConfig({
    this.type,
    this.wireApi,
    required this.baseUrl,
    this.apiKey,
    this.apiKeyCommand,
    this.bearerToken,
    this.azure,
    this.modelProvider,
    this.transport,
    this.headers,
    this.modelId,
    this.wireModel,
    this.maxPromptTokens,
    this.maxContextWindowTokens,
    this.maxOutputTokens,
  });

  Map<String, dynamic> toJson() => {
        if (type != null) 'type': type,
        if (wireApi != null) 'wireApi': wireApi,
        'baseUrl': baseUrl,
        if (apiKey != null) 'apiKey': apiKey,
        if (apiKeyCommand != null) 'apiKeyCommand': apiKeyCommand,
        if (bearerToken != null) 'bearerToken': bearerToken,
        if (azure != null) 'azure': azure!.toJson(),
        if (modelProvider != null) 'modelProvider': modelProvider,
        if (transport != null) 'transport': transport,
        if (headers != null) 'headers': headers,
        if (modelId != null) 'modelId': modelId,
        if (wireModel != null) 'wireModel': wireModel,
        if (maxPromptTokens != null) 'maxPromptTokens': maxPromptTokens,
        if (maxContextWindowTokens != null)
          'maxContextWindowTokens': maxContextWindowTokens,
        if (maxOutputTokens != null) 'maxOutputTokens': maxOutputTokens,
      };
}

/// Azure-specific provider options.
class AzureProviderOptions {
  final String? apiVersion;

  const AzureProviderOptions({this.apiVersion});

  Map<String, dynamic> toJson() => {
        if (apiVersion != null) 'apiVersion': apiVersion,
      };
}

// ---------------------------------------------------------------------------
// Ping Response
// ---------------------------------------------------------------------------

/// Response from a ping request.
class PingResponse {
  final String message;
  final String timestamp;
  final int? protocolVersion;

  const PingResponse({
    required this.message,
    required this.timestamp,
    this.protocolVersion,
  });

  factory PingResponse.fromJson(Map<String, dynamic> json) {
    return PingResponse(
      message: json['message'] as String? ?? '',
      timestamp: json['timestamp']?.toString() ?? '',
      protocolVersion: json['protocolVersion'] as int?,
    );
  }
}

// ---------------------------------------------------------------------------
// Model Types
// ---------------------------------------------------------------------------

/// Model capabilities and limits.
class ModelCapabilities {
  final ModelSupports supports;
  final ModelLimits limits;

  const ModelCapabilities({required this.supports, required this.limits});

  factory ModelCapabilities.fromJson(Map<String, dynamic> json) {
    return ModelCapabilities(
      supports: ModelSupports.fromJson(
          (json['supports'] as Map<String, dynamic>?) ?? {}),
      limits: ModelLimits.fromJson(
          (json['limits'] as Map<String, dynamic>?) ?? {}),
    );
  }
}

class ModelSupports {
  final bool vision;
  final bool reasoningEffort;

  const ModelSupports({required this.vision, required this.reasoningEffort});

  factory ModelSupports.fromJson(Map<String, dynamic> json) {
    return ModelSupports(
      vision: json['vision'] as bool? ?? false,
      reasoningEffort: json['reasoningEffort'] as bool? ?? false,
    );
  }
}

class ModelLimits {
  final int? maxPromptTokens;
  final int maxContextWindowTokens;

  const ModelLimits({this.maxPromptTokens, this.maxContextWindowTokens = 0});

  factory ModelLimits.fromJson(Map<String, dynamic> json) {
    return ModelLimits(
      maxPromptTokens: json['max_prompt_tokens'] as int?,
      maxContextWindowTokens: (json['max_context_window_tokens'] as int?) ?? 0,
    );
  }
}

/// Model policy state.
class ModelPolicy {
  final String state;
  final String terms;

  const ModelPolicy({required this.state, required this.terms});

  factory ModelPolicy.fromJson(Map<String, dynamic> json) {
    return ModelPolicy(
      state: json['state'] as String,
      terms: json['terms'] as String,
    );
  }
}

/// Completion type for slash command inputs.
enum SlashCommandInputCompletion {
  directory;

  factory SlashCommandInputCompletion.fromJson(String value) {
    return SlashCommandInputCompletion.values.firstWhere((e) => e.name == value);
  }
}

/// Kind of slash command.
enum SlashCommandKind {
  builtin,
  client,
  skill;

  factory SlashCommandKind.fromJson(String value) {
    return SlashCommandKind.values.firstWhere((e) => e.name == value);
  }
}

/// Price category for model picker.
enum ModelPickerPriceCategory {
  high,
  low,
  medium,
  veryHigh;

  static const _jsonMap = {
    'high': ModelPickerPriceCategory.high,
    'low': ModelPickerPriceCategory.low,
    'medium': ModelPickerPriceCategory.medium,
    'very_high': ModelPickerPriceCategory.veryHigh,
  };

  factory ModelPickerPriceCategory.fromJson(String value) {
    return _jsonMap[value]!;
  }
}

/// Input definition for a slash command.
class SlashCommandInput {
  final String hint;
  final SlashCommandInputCompletion? completion;

  const SlashCommandInput({required this.hint, this.completion});

  factory SlashCommandInput.fromJson(Map<String, dynamic> json) {
    return SlashCommandInput(
      hint: json['hint'] as String,
      completion: json['completion'] != null
          ? SlashCommandInputCompletion.fromJson(
              json['completion'] as String)
          : null,
    );
  }
}

/// Information about a slash command.
class SlashCommandInfo {
  final bool allowDuringAgentExecution;
  final String description;
  final SlashCommandKind kind;
  final String name;
  final List<String>? aliases;
  final bool? experimental;
  final SlashCommandInput? input;

  const SlashCommandInfo({
    required this.allowDuringAgentExecution,
    required this.description,
    required this.kind,
    required this.name,
    this.aliases,
    this.experimental,
    this.input,
  });

  factory SlashCommandInfo.fromJson(Map<String, dynamic> json) {
    return SlashCommandInfo(
      allowDuringAgentExecution: json['allowDuringAgentExecution'] as bool,
      description: json['description'] as String,
      kind: SlashCommandKind.fromJson(json['kind'] as String),
      name: json['name'] as String,
      aliases:
          (json['aliases'] as List<dynamic>?)?.cast<String>(),
      experimental: json['experimental'] as bool?,
      input: json['input'] != null
          ? SlashCommandInput.fromJson(json['input'] as Map<String, dynamic>)
          : null,
    );
  }
}

/// Request to invoke a command.
class CommandsInvokeRequest {
  final String name;
  final String? input;

  const CommandsInvokeRequest({required this.name, this.input});

  factory CommandsInvokeRequest.fromJson(Map<String, dynamic> json) {
    return CommandsInvokeRequest(
      name: json['name'] as String,
      input: json['input'] as String?,
    );
  }
}

/// Request to list available commands.
class CommandsListRequest {
  final bool? includeBuiltins;
  final bool? includeClientCommands;
  final bool? includeSkills;

  const CommandsListRequest({
    this.includeBuiltins,
    this.includeClientCommands,
    this.includeSkills,
  });

  factory CommandsListRequest.fromJson(Map<String, dynamic> json) {
    return CommandsListRequest(
      includeBuiltins: json['includeBuiltins'] as bool?,
      includeClientCommands: json['includeClientCommands'] as bool?,
      includeSkills: json['includeSkills'] as bool?,
    );
  }
}

/// Token pricing information for model billing.
class ModelBillingTokenPrices {
  final int? batchSize;
  final int? cachePrice;
  final int? inputPrice;
  final int? outputPrice;

  const ModelBillingTokenPrices({
    this.batchSize,
    this.cachePrice,
    this.inputPrice,
    this.outputPrice,
  });

  factory ModelBillingTokenPrices.fromJson(Map<String, dynamic> json) {
    return ModelBillingTokenPrices(
      batchSize: json['batchSize'] as int?,
      cachePrice: json['cachePrice'] as int?,
      inputPrice: json['inputPrice'] as int?,
      outputPrice: json['outputPrice'] as int?,
    );
  }
}

/// Experimental: Diagnostics from loading skills.
class SkillsLoadDiagnostics {
  final List<String> errors;
  final List<String> warnings;

  const SkillsLoadDiagnostics({
    required this.errors,
    required this.warnings,
  });

  factory SkillsLoadDiagnostics.fromJson(Map<String, dynamic> json) {
    return SkillsLoadDiagnostics(
      errors: (json['errors'] as List<dynamic>).cast<String>(),
      warnings: (json['warnings'] as List<dynamic>).cast<String>(),
    );
  }
}

/// Model billing information.
class ModelBilling {
  final double multiplier;
  final ModelBillingTokenPrices? tokenPrices;
  final ModelPickerPriceCategory? pickerPriceCategory;

  const ModelBilling({
    required this.multiplier,
    this.tokenPrices,
    this.pickerPriceCategory,
  });

  factory ModelBilling.fromJson(Map<String, dynamic> json) {
    return ModelBilling(
      multiplier: (json['multiplier'] as num).toDouble(),
      tokenPrices: json['tokenPrices'] != null
          ? ModelBillingTokenPrices.fromJson(
              json['tokenPrices'] as Map<String, dynamic>)
          : null,
      pickerPriceCategory: json['pickerPriceCategory'] != null
          ? ModelPickerPriceCategory.fromJson(
              json['pickerPriceCategory'] as String)
          : null,
    );
  }
}

/// Information about an available model.
class ModelInfo {
  final String id;
  final String name;
  final ModelCapabilities capabilities;
  final ModelPolicy? policy;
  final ModelBilling? billing;
  final List<String>? supportedReasoningEfforts;
  final String? defaultReasoningEffort;

  const ModelInfo({
    required this.id,
    required this.name,
    required this.capabilities,
    this.policy,
    this.billing,
    this.supportedReasoningEfforts,
    this.defaultReasoningEffort,
  });

  factory ModelInfo.fromJson(Map<String, dynamic> json) {
    return ModelInfo(
      id: json['id'] as String,
      name: json['name'] as String,
      capabilities: ModelCapabilities.fromJson(
          json['capabilities'] as Map<String, dynamic>),
      policy: json['policy'] != null
          ? ModelPolicy.fromJson(json['policy'] as Map<String, dynamic>)
          : null,
      billing: json['billing'] != null
          ? ModelBilling.fromJson(json['billing'] as Map<String, dynamic>)
          : null,
      supportedReasoningEfforts:
          (json['supportedReasoningEfforts'] as List<dynamic>?)
              ?.cast<String>(),
      defaultReasoningEffort: json['defaultReasoningEffort'] as String?,
    );
  }
}

// ---------------------------------------------------------------------------
// Remote Session
// ---------------------------------------------------------------------------

/// Mode for remote session control.
enum RemoteSessionMode {
  export_,
  off,
  on,
}

/// Extension to convert [RemoteSessionMode] to/from JSON string.
extension RemoteSessionModeJson on RemoteSessionMode {
  String toJson() {
    switch (this) {
      case RemoteSessionMode.export_:
        return 'export';
      case RemoteSessionMode.off:
        return 'off';
      case RemoteSessionMode.on:
        return 'on';
    }
  }

  static RemoteSessionMode fromJson(String value) {
    switch (value) {
      case 'export':
        return RemoteSessionMode.export_;
      case 'off':
        return RemoteSessionMode.off;
      case 'on':
        return RemoteSessionMode.on;
      default:
        return RemoteSessionMode.off;
    }
  }
}

/// Experimental: Request to enable or configure a remote session.
class RemoteEnableRequest {
  final RemoteSessionMode? mode;

  const RemoteEnableRequest({
    this.mode,
  });

  factory RemoteEnableRequest.fromJson(Map<String, dynamic> json) {
    return RemoteEnableRequest(
      mode: json['mode'] != null
          ? RemoteSessionModeJson.fromJson(json['mode'] as String)
          : null,
    );
  }
}

/// Experimental: Result of enabling a remote session.
class RemoteEnableResult {
  final bool remoteSteerable;
  final String? url;

  const RemoteEnableResult({
    required this.remoteSteerable,
    this.url,
  });

  factory RemoteEnableResult.fromJson(Map<String, dynamic> json) {
    return RemoteEnableResult(
      remoteSteerable: json['remoteSteerable'] as bool,
      url: json['url'] as String?,
    );
  }
}

// ---------------------------------------------------------------------------
// Session Metadata
// ---------------------------------------------------------------------------

/// Metadata about a session.
class SessionMetadata {
  final String sessionId;
  final DateTime startTime;
  final DateTime modifiedTime;
  final String? summary;
  final bool isRemote;

  const SessionMetadata({
    required this.sessionId,
    required this.startTime,
    required this.modifiedTime,
    this.summary,
    required this.isRemote,
  });

  factory SessionMetadata.fromJson(Map<String, dynamic> json) {
    return SessionMetadata(
      sessionId: json['sessionId'] as String,
      startTime: DateTime.parse(json['startTime'] as String),
      modifiedTime: DateTime.parse(json['modifiedTime'] as String),
      summary: json['summary'] as String?,
      isRemote: json['isRemote'] as bool? ?? false,
    );
  }
}

// ---------------------------------------------------------------------------
// Session Lifecycle Events
// ---------------------------------------------------------------------------

/// Session lifecycle event notification.
class SessionLifecycleEvent {
  /// One of: session.created, session.deleted, session.updated,
  /// session.foreground, session.background
  final String type;

  final String sessionId;

  final Map<String, dynamic>? metadata;

  const SessionLifecycleEvent({
    required this.type,
    required this.sessionId,
    this.metadata,
  });

  factory SessionLifecycleEvent.fromJson(Map<String, dynamic> json) {
    return SessionLifecycleEvent(
      type: json['type'] as String,
      sessionId: json['sessionId'] as String,
      metadata: json['metadata'] as Map<String, dynamic>?,
    );
  }
}

/// Handler for session lifecycle events.
typedef SessionLifecycleHandler = void Function(SessionLifecycleEvent event);

// ---------------------------------------------------------------------------
// Response Format & Image Generation
// ---------------------------------------------------------------------------

/// Response format for message responses.
enum ResponseFormat {
  text,
  image,
  jsonObject;

  String toJson() {
    switch (this) {
      case ResponseFormat.text:
        return 'text';
      case ResponseFormat.image:
        return 'image';
      case ResponseFormat.jsonObject:
        return 'json_object';
    }
  }

  static ResponseFormat fromJson(String value) {
    switch (value) {
      case 'text':
        return ResponseFormat.text;
      case 'image':
        return ResponseFormat.image;
      case 'json_object':
        return ResponseFormat.jsonObject;
      default:
        return ResponseFormat.text;
    }
  }
}

/// Options for image generation.
class ImageOptions {
  /// Image size (e.g. "1024x1024").
  final String? size;

  /// Image quality ("hd" or "standard").
  final String? quality;

  /// Image style ("natural" or "vivid").
  final String? style;

  const ImageOptions({this.size, this.quality, this.style});

  Map<String, dynamic> toJson() => {
        if (size != null) 'size': size,
        if (quality != null) 'quality': quality,
        if (style != null) 'style': style,
      };

  factory ImageOptions.fromJson(Map<String, dynamic> json) {
    return ImageOptions(
      size: json['size'] as String?,
      quality: json['quality'] as String?,
      style: json['style'] as String?,
    );
  }
}

/// Image data from an assistant image response.
class AssistantImageData {
  final String format;
  final String base64;
  final String? url;
  final String? revisedPrompt;
  final int width;
  final int height;

  const AssistantImageData({
    required this.format,
    required this.base64,
    this.url,
    this.revisedPrompt,
    required this.width,
    required this.height,
  });

  factory AssistantImageData.fromJson(Map<String, dynamic> json) {
    return AssistantImageData(
      format: json['format'] as String,
      base64: json['base64'] as String,
      url: json['url'] as String?,
      revisedPrompt: json['revisedPrompt'] as String?,
      width: json['width'] as int,
      height: json['height'] as int,
    );
  }
}

/// A content block in a mixed text+image response.
class ContentBlock {
  final String type;
  final String? text;
  final AssistantImageData? image;

  const ContentBlock({required this.type, this.text, this.image});

  factory ContentBlock.fromJson(Map<String, dynamic> json) {
    return ContentBlock(
      type: json['type'] as String,
      text: json['text'] as String?,
      image: json['image'] != null
          ? AssistantImageData.fromJson(json['image'] as Map<String, dynamic>)
          : null,
    );
  }
}

// ---------------------------------------------------------------------------
// Message Options
// ---------------------------------------------------------------------------

/// File attachment for a message.
class FileAttachment {
  final String path;
  final String? displayName;

  const FileAttachment({required this.path, this.displayName});

  Map<String, dynamic> toJson() => {
        'type': 'file',
        'path': path,
        if (displayName != null) 'displayName': displayName,
      };
}

/// Directory attachment for a message.
class DirectoryAttachment {
  final String path;
  final String? displayName;

  const DirectoryAttachment({required this.path, this.displayName});

  Map<String, dynamic> toJson() => {
        'type': 'directory',
        'path': path,
        if (displayName != null) 'displayName': displayName,
      };
}

/// Selection range within a file.
class SelectionRange {
  final int startLine;
  final int startCharacter;
  final int endLine;
  final int endCharacter;

  const SelectionRange({
    required this.startLine,
    required this.startCharacter,
    required this.endLine,
    required this.endCharacter,
  });

  Map<String, dynamic> toJson() => {
        'start': {'line': startLine, 'character': startCharacter},
        'end': {'line': endLine, 'character': endCharacter},
      };
}

/// Selection attachment for a message.
class SelectionAttachment {
  final String filePath;
  final String displayName;
  final SelectionRange? selection;
  final String? text;

  const SelectionAttachment({
    required this.filePath,
    required this.displayName,
    this.selection,
    this.text,
  });

  Map<String, dynamic> toJson() => {
        'type': 'selection',
        'filePath': filePath,
        'displayName': displayName,
        if (selection != null) 'selection': selection!.toJson(),
        if (text != null) 'text': text,
      };
}

/// Options for sending a message to a session.
class MessageOptions {
  final String prompt;
  final List<Map<String, dynamic>>? attachments;

  /// One of: enqueue, immediate
  final String? mode;

  /// Legacy per-message response format.
  ///
  /// Deprecated: SDK protocol 4 uses `responseFormat` for structured output,
  /// so this value is no longer sent. Use [responseSchema] for structured
  /// output and the session-level `imageGeneration` option for images.
  final ResponseFormat? responseFormat;

  /// Options for image generation (used when [responseFormat] is image).
  final ImageOptions? imageOptions;

  /// Agent mode to use for this message.
  final String? agentMode;

  /// Alternate prompt text to display in place of [prompt].
  final String? displayPrompt;

  /// Custom HTTP headers to include in outbound model requests for this turn.
  final Map<String, String>? requestHeaders;

  /// Message provenance: [MessageSource.user], [MessageSource.system], or
  /// `MessageSource.agent(id)` (`"agent-<id>"`). Omitted by default.
  final String? source;

  /// JSON Schema for a structured response. Sent as
  /// `responseFormat: {type: "json_schema", jsonSchema: {name: "response",
  /// strict: true, schema: <schema>}}`.
  final Map<String, dynamic>? responseSchema;

  /// Opaque caller-chosen ID for the turn this message starts, sent as
  /// `clientTurnId`. The runtime echoes it on the `user.message` event.
  final String? clientTurnId;

  const MessageOptions({
    required this.prompt,
    this.attachments,
    this.mode,
    this.responseFormat,
    this.imageOptions,
    this.agentMode,
    this.displayPrompt,
    this.requestHeaders,
    this.source,
    this.responseSchema,
    this.clientTurnId,
  });
}

/// Well-known [MessageOptions.source] values.
class MessageSource {
  /// A message typed by the user (the runtime default).
  static const String user = 'user';

  /// Application-generated context.
  static const String system = 'system';

  /// A message from the identified agent (`"agent-<id>"`).
  static String agent(String agentId) => 'agent-$agentId';
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

/// Context for a slash-command invocation.
class CommandContext {
  /// Session ID where the command was invoked.
  final String sessionId;

  /// The full command text (e.g. "/deploy production").
  final String command;

  /// Command name without leading /.
  final String commandName;

  /// Raw argument string after the command name.
  final String args;

  const CommandContext({
    required this.sessionId,
    required this.command,
    required this.commandName,
    this.args = '',
  });
}

/// Handler invoked when a registered slash-command is executed.
typedef CommandHandler = Future<void> Function(CommandContext context);

/// Definition of a slash command registered with the session.
class CommandDefinition {
  /// Command name (without leading /).
  final String name;

  /// Human-readable description shown in command completion UI.
  final String? description;

  /// Handler invoked when the command is executed.
  final CommandHandler handler;

  const CommandDefinition({
    required this.name,
    this.description,
    required this.handler,
  });
}

// ---------------------------------------------------------------------------
// UI Elicitation
// ---------------------------------------------------------------------------

/// Context for an elicitation request from the server.
class ElicitationContext {
  /// Session ID that triggered the request.
  final String sessionId;

  /// Prompt message describing what information is needed.
  final String message;

  /// JSON Schema describing the form fields (optional).
  final Map<String, dynamic>? requestedSchema;

  /// Elicitation mode ("form" or "url").
  final String? mode;

  /// Source that initiated the request.
  final String? elicitationSource;

  /// URL to open in the user's browser (url mode only).
  final String? url;

  const ElicitationContext({
    required this.sessionId,
    required this.message,
    this.requestedSchema,
    this.mode,
    this.elicitationSource,
    this.url,
  });
}

/// Result returned from an elicitation handler.
class ElicitationResult {
  /// User action: "accept", "decline", or "cancel".
  final String action;

  /// Form values submitted by the user (present when action is "accept").
  final Map<String, dynamic>? content;

  const ElicitationResult({
    required this.action,
    this.content,
  });
}

/// Handler for elicitation requests from the server.
typedef ElicitationHandler = Future<ElicitationResult> Function(
    ElicitationContext context);

// ---------------------------------------------------------------------------
// Exit Plan Mode
// ---------------------------------------------------------------------------

/// Request to exit plan mode from the agent.
class ExitPlanModeRequest {
  final String summary;
  final String? planContent;
  final List<String> actions;
  final String recommendedAction;

  const ExitPlanModeRequest({
    required this.summary,
    this.planContent,
    required this.actions,
    required this.recommendedAction,
  });

  factory ExitPlanModeRequest.fromJson(Map<String, dynamic> json) {
    return ExitPlanModeRequest(
      summary: json['summary'] as String,
      planContent: json['planContent'] as String?,
      actions: (json['actions'] as List<dynamic>).cast<String>(),
      recommendedAction: json['recommendedAction'] as String,
    );
  }
}

/// Response to an exit-plan-mode request.
class ExitPlanModeResult {
  final bool approved;
  final String? selectedAction;
  final String? feedback;

  const ExitPlanModeResult({
    required this.approved,
    this.selectedAction,
    this.feedback,
  });

  Map<String, dynamic> toJson() => {
        'approved': approved,
        if (selectedAction != null) 'selectedAction': selectedAction,
        if (feedback != null) 'feedback': feedback,
      };
}

/// Handler for exit-plan-mode requests.
typedef ExitPlanModeHandler = Future<ExitPlanModeResult> Function(
  ExitPlanModeRequest request,
  SessionInvocationContext context,
);

// ---------------------------------------------------------------------------
// Session Configuration
// ---------------------------------------------------------------------------

/// Configuration for creating a session.
class SessionConfig {
  final String? sessionId;
  final String? model;
  final String? reasoningEffort;
  final String? configDir;
  final List<Tool>? tools;

  /// One of [SystemMessageAppendConfig], [SystemMessageReplaceConfig],
  /// or [SystemMessageCustomizeConfig].
  final dynamic systemMessage;

  final List<String>? availableTools;
  final List<String>? excludedTools;
  final ProviderConfig? provider;
  final PermissionHandler? onPermissionRequest;
  final UserInputHandler? onUserInputRequest;
  final SessionHooks? hooks;
  final String? workingDirectory;
  final bool? streaming;

  /// Include sub-agent streaming events in the event stream. Default: true.
  final bool? includeSubAgentStreamingEvents;

  final Map<String, MCPServerConfig>? mcpServers;
  final List<CustomAgentConfig>? customAgents;
  final List<String>? skillDirectories;
  final List<String>? disabledSkills;
  final InfiniteSessionConfig? infiniteSessions;

  /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
  final Map<String, dynamic>? modelCapabilities;

  /// When true, auto-discovers MCP server configs from working directory. Default: false.
  final bool? enableConfigDiscovery;

  /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
  final String? gitHubToken;

  /// Slash commands registered for this session.
  final List<CommandDefinition>? commands;

  /// Handler for elicitation requests from the server.
  final ElicitationHandler? onElicitationRequest;

  /// Handler for exit-plan-mode requests from the agent.
  final ExitPlanModeHandler? onExitPlanMode;

  /// When true, enables session-level telemetry collection.
  final bool? enableSessionTelemetry;

  /// Directories to search for instruction files.
  final List<String>? instructionDirectories;

  /// Enable response citations for this session.
  final bool? enableCitations;

  /// Names of built-in agents to exclude from this session.
  final List<String>? excludedBuiltinAgents;

  /// Per-session AI-credit spending limits.
  final SessionLimitsConfig? sessionLimits;

  /// Persistent session memory configuration.
  final MemoryConfiguration? memory;

  /// OTLP telemetry protocol ("grpc" or "http/protobuf").
  final String? otlpProtocol;

  /// Use the WebSocket response transport instead of stdio streaming.
  final bool? enableWebSocketResponses;

  /// Experiment assignment overrides.
  final Map<String, dynamic>? expAssignments;

  /// Handler for MCP OAuth host token requests.
  final McpAuthRequestHandler? onMcpAuthRequest;

  /// BYOK callback resolving a bearer token from [ProviderTokenArgs].
  final BearerTokenProvider? bearerTokenProvider;

  // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
  /// Enable session rewind so the conversation can be rolled back.
  final bool? rewindEnabled;

  /// Extra workspace directories the session may access.
  final List<String>? additionalDirectories;

  /// Names of MCP servers to disable for this session.
  final List<String>? disabledMcpServers;

  /// GitHub MCP tool configuration.
  final Map<String, dynamic>? githubMcpToolConfig;

  /// Canvas provider configuration.
  final Map<String, dynamic>? canvasProvider;

  /// Restrict custom agents to locally-defined ones only.
  final bool? customAgentsLocalOnly;

  /// Tool-search configuration.
  final Map<String, dynamic>? toolSearch;

  /// Enable experimental mode for this session.
  final bool? experimentalMode;

  /// Enable content-exclusion enforcement.
  final bool? contentExclusion;

  // --- v2.6.0 upstream-sync session options ---

  /// Session name (create only).
  final String? name;

  /// Ask-user tool variant: `"legacy"` or `"elicitation"`.
  final String? askUserVariant;

  /// Per-session GitHub token callback. The client registers it under a
  /// generated registration ID, sends `gitHubTokenProviderRegistrationId`,
  /// and answers the runtime's `gitHubToken.getToken` requests with it.
  /// Mutually exclusive with [gitHubToken].
  final GitHubTokenProvider? gitHubTokenProvider;

  /// Sandbox configuration for tool execution (`permissions.sandbox`).
  final SandboxConfig? sandboxConfig;

  /// Runtime feature flag overrides (`experiments.flags`).
  final Map<String, bool>? featureFlags;

  /// Model allowlist (`policy.allowedModels`).
  final List<String>? allowedModels;

  /// OAuth Client ID Metadata Document URL for MCP authorization.
  final String? authClientIdMetadataUrl;

  /// Custom agent definition directories (`agents.directories`).
  final List<String>? agentDirectories;

  /// Hook definition directories (`hooks.directories`).
  final List<String>? hookDirectories;

  /// Grant working-directory trust at creation: `"session"` or `"user"`
  /// (create only).
  final String? trustWorkingDirectory;

  /// Image generation configuration (`tools.imageGeneration`).
  final ImageGenerationConfig? imageGeneration;

  /// Autopilot continuation options (`execution.autopilot`).
  final AutopilotOptions? autopilot;

  // --- v3.0.0 (SDK protocol 4) session options ---

  /// Named BYOK provider connections (`inference.providers`).
  final List<NamedProviderConfig>? providers;

  /// BYOK model definitions (`inference.models`). Select one with
  /// `"<provider>/<id>"`.
  final List<ProviderModelConfig>? models;

  /// Named connection for unqualified BYOK selections
  /// (`inference.defaultProvider`). Requires `capi: CapiSessionOptions(enabled: false)`.
  final String? defaultProvider;

  /// Copilot API options (`github.capi`).
  final CapiSessionOptions? capi;

  /// Answers `shellAuthentication.request` prompts (sudo, ssh, git) for
  /// commands the agent runs. When set, the client sends
  /// `interaction.requestShellAuthentication`.
  final ShellAuthenticationHandler? onShellAuthenticationRequest;

  /// Stores memories in the app. When set, the client sends
  /// `state.hasMemoryStore` and answers `memoryStore.store` requests.
  final MemoryStoreHandler? onStoreMemory;

  const SessionConfig({
    this.sessionId,
    this.model,
    this.reasoningEffort,
    this.configDir,
    this.tools,
    this.systemMessage,
    this.availableTools,
    this.excludedTools,
    this.provider,
    this.onPermissionRequest,
    this.onUserInputRequest,
    this.hooks,
    this.workingDirectory,
    this.streaming,
    this.includeSubAgentStreamingEvents,
    this.mcpServers,
    this.customAgents,
    this.skillDirectories,
    this.disabledSkills,
    this.infiniteSessions,
    this.modelCapabilities,
    this.enableConfigDiscovery,
    this.gitHubToken,
    this.commands,
    this.onElicitationRequest,
    this.onExitPlanMode,
    this.enableSessionTelemetry,
    this.instructionDirectories,
    this.enableCitations,
    this.excludedBuiltinAgents,
    this.sessionLimits,
    this.memory,
    this.otlpProtocol,
    this.enableWebSocketResponses,
    this.expAssignments,
    this.onMcpAuthRequest,
    this.bearerTokenProvider,
    this.rewindEnabled,
    this.additionalDirectories,
    this.disabledMcpServers,
    this.githubMcpToolConfig,
    this.canvasProvider,
    this.customAgentsLocalOnly,
    this.toolSearch,
    this.experimentalMode,
    this.contentExclusion,
    this.name,
    this.askUserVariant,
    this.gitHubTokenProvider,
    this.sandboxConfig,
    this.featureFlags,
    this.allowedModels,
    this.authClientIdMetadataUrl,
    this.agentDirectories,
    this.hookDirectories,
    this.trustWorkingDirectory,
    this.imageGeneration,
    this.autopilot,
    this.providers,
    this.models,
    this.defaultProvider,
    this.capi,
    this.onShellAuthenticationRequest,
    this.onStoreMemory,
  });
}

/// Configuration for resuming a session.
class ResumeSessionConfig {
  final String? model;
  final String? reasoningEffort;
  final String? configDir;
  final List<Tool>? tools;
  final dynamic systemMessage;
  final List<String>? availableTools;
  final List<String>? excludedTools;
  final ProviderConfig? provider;
  final PermissionHandler? onPermissionRequest;
  final UserInputHandler? onUserInputRequest;
  final SessionHooks? hooks;
  final String? workingDirectory;
  final bool? streaming;

  /// Include sub-agent streaming events in the event stream. Default: true.
  final bool? includeSubAgentStreamingEvents;

  final Map<String, MCPServerConfig>? mcpServers;
  final List<CustomAgentConfig>? customAgents;
  final List<String>? skillDirectories;
  final List<String>? disabledSkills;
  final InfiniteSessionConfig? infiniteSessions;

  /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
  final Map<String, dynamic>? modelCapabilities;

  /// When true, auto-discovers MCP server configs from working directory. Default: false.
  final bool? enableConfigDiscovery;

  /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
  final String? gitHubToken;

  /// Slash commands registered for this session.
  final List<CommandDefinition>? commands;

  /// Handler for elicitation requests from the server.
  final ElicitationHandler? onElicitationRequest;

  /// Handler for exit-plan-mode requests from the agent.
  final ExitPlanModeHandler? onExitPlanMode;

  /// When true, enables session-level telemetry collection.
  final bool? enableSessionTelemetry;

  final bool? disableResume;

  /// Directories to search for instruction files.
  final List<String>? instructionDirectories;

  // --- v2.6.0 upstream-sync resume options ---

  /// Ask-user tool variant: `"legacy"` or `"elicitation"`.
  final String? askUserVariant;

  /// Per-session GitHub token callback answering `gitHubToken.getToken`
  /// requests. Mutually exclusive with [gitHubToken].
  final GitHubTokenProvider? gitHubTokenProvider;

  /// Sandbox configuration for tool execution (`permissions.sandbox`).
  final SandboxConfig? sandboxConfig;

  /// Runtime feature flag overrides (`experiments.flags`).
  final Map<String, bool>? featureFlags;

  /// Model allowlist (`policy.allowedModels`).
  final List<String>? allowedModels;

  /// OAuth Client ID Metadata Document URL for MCP authorization.
  final String? authClientIdMetadataUrl;

  /// Custom agent definition directories (`agents.directories`).
  final List<String>? agentDirectories;

  /// Hook definition directories (`hooks.directories`).
  final List<String>? hookDirectories;

  /// Allow the runtime to recover a damaged transcript (resume only).
  final bool? allowTranscriptRecovery;

  /// Image generation configuration (`tools.imageGeneration`).
  final ImageGenerationConfig? imageGeneration;

  /// Autopilot continuation options (`execution.autopilot`).
  final AutopilotOptions? autopilot;

  // --- v3.0.0 (SDK protocol 4) resume options ---

  /// Skip emitting the `session.resume` event. Takes precedence over the
  /// legacy [disableResume] alias.
  final bool? suppressResumeEvent;

  /// Named BYOK provider connections (`inference.providers`).
  final List<NamedProviderConfig>? providers;

  /// BYOK model definitions (`inference.models`).
  final List<ProviderModelConfig>? models;

  /// Named connection for unqualified BYOK selections
  /// (`inference.defaultProvider`).
  final String? defaultProvider;

  /// When true, sends `defaultProvider: null` to clear the session's
  /// default provider.
  final bool? clearDefaultProvider;

  /// Copilot API options (`github.capi`).
  final CapiSessionOptions? capi;

  /// Answers `shellAuthentication.request` prompts for commands the agent
  /// runs (sends `interaction.requestShellAuthentication`).
  final ShellAuthenticationHandler? onShellAuthenticationRequest;

  /// Stores memories in the app (sends `state.hasMemoryStore` and answers
  /// `memoryStore.store`).
  final MemoryStoreHandler? onStoreMemory;

  const ResumeSessionConfig({
    this.model,
    this.reasoningEffort,
    this.configDir,
    this.tools,
    this.systemMessage,
    this.availableTools,
    this.excludedTools,
    this.provider,
    this.onPermissionRequest,
    this.onUserInputRequest,
    this.hooks,
    this.workingDirectory,
    this.streaming,
    this.includeSubAgentStreamingEvents,
    this.mcpServers,
    this.customAgents,
    this.skillDirectories,
    this.disabledSkills,
    this.infiniteSessions,
    this.modelCapabilities,
    this.enableConfigDiscovery,
    this.gitHubToken,
    this.commands,
    this.onElicitationRequest,
    this.onExitPlanMode,
    this.enableSessionTelemetry,
    this.disableResume,
    this.instructionDirectories,
    this.askUserVariant,
    this.gitHubTokenProvider,
    this.sandboxConfig,
    this.featureFlags,
    this.allowedModels,
    this.authClientIdMetadataUrl,
    this.agentDirectories,
    this.hookDirectories,
    this.allowTranscriptRecovery,
    this.imageGeneration,
    this.autopilot,
    this.suppressResumeEvent,
    this.providers,
    this.models,
    this.defaultProvider,
    this.clearDefaultProvider,
    this.capi,
    this.onShellAuthenticationRequest,
    this.onStoreMemory,
  });
}

// ---------------------------------------------------------------------------
// Session Filesystem Types
// ---------------------------------------------------------------------------

/// Configuration for a custom session filesystem provider.
class SessionFsConfig {
  /// Initial working directory for the session filesystem.
  final String initialCwd;

  /// Path to the session state directory.
  final String sessionStatePath;

  /// Conventions string for the session filesystem.
  final String conventions;

  const SessionFsConfig({
    required this.initialCwd,
    required this.sessionStatePath,
    required this.conventions,
  });

  Map<String, dynamic> toJson() => {
        'initialCwd': initialCwd,
        'sessionStatePath': sessionStatePath,
        'conventions': conventions,
      };

  factory SessionFsConfig.fromJson(Map<String, dynamic> json) {
    return SessionFsConfig(
      initialCwd: json['initialCwd'] as String,
      sessionStatePath: json['sessionStatePath'] as String,
      conventions: json['conventions'] as String,
    );
  }
}

/// File metadata returned by session filesystem operations.
class SessionFsFileInfo {
  final String name;
  final int size;
  final bool isDirectory;
  final bool isFile;
  final String? createdAt;
  final String? modifiedAt;

  const SessionFsFileInfo({
    required this.name,
    required this.size,
    required this.isDirectory,
    required this.isFile,
    this.createdAt,
    this.modifiedAt,
  });

  Map<String, dynamic> toJson() => {
        'name': name,
        'size': size,
        'isDirectory': isDirectory,
        'isFile': isFile,
        if (createdAt != null) 'createdAt': createdAt,
        if (modifiedAt != null) 'modifiedAt': modifiedAt,
      };

  factory SessionFsFileInfo.fromJson(Map<String, dynamic> json) {
    return SessionFsFileInfo(
      name: json['name'] as String,
      size: json['size'] as int,
      isDirectory: json['isDirectory'] as bool,
      isFile: json['isFile'] as bool,
      createdAt: json['createdAt'] as String?,
      modifiedAt: json['modifiedAt'] as String?,
    );
  }
}

/// Interface for session filesystem providers.
abstract class SessionFsProvider {
  Future<String> readFile(String sessionId, String path);
  Future<void> writeFile(String sessionId, String path, String content);
  Future<void> appendFile(String sessionId, String path, String content);
  Future<bool> exists(String sessionId, String path);
  Future<SessionFsFileInfo> stat(String sessionId, String path);
  Future<void> mkdir(String sessionId, String path, {bool recursive = false});
  Future<List<String>> readdir(String sessionId, String path);
  Future<List<SessionFsFileInfo>> readdirWithTypes(
      String sessionId, String path);
  Future<void> rm(String sessionId, String path, {bool recursive = false});
  Future<void> rename(String sessionId, String oldPath, String newPath);
}

// ---------------------------------------------------------------------------
// Client Options
// ---------------------------------------------------------------------------

/// Options for creating a [CopilotClient].
class CopilotClientOptions {
  /// Path to the CLI executable.
  final String? cliPath;

  /// Extra arguments inserted before SDK-managed args.
  final List<String>? cliArgs;

  /// Working directory for the CLI process.
  final String? cwd;

  /// Port for TCP mode. 0 means random available port.
  final int? port;

  /// Use stdio transport instead of TCP. Defaults to true.
  final bool? useStdio;

  /// URL of an existing Copilot CLI server (TCP only).
  /// Mutually exclusive with [cliPath] and [useStdio].
  final String? cliUrl;

  /// Log level for the CLI server.
  final String? logLevel;

  /// Auto-start the CLI server on first use. Defaults to true.
  final bool? autoStart;

  /// Auto-restart the CLI if it crashes. Defaults to true.
  final bool? autoRestart;

  /// Environment variables for the CLI process.
  final Map<String, String>? env;

  /// GitHub token for authentication.
  final String? githubToken;

  /// Whether to use logged-in user auth. Defaults to true
  /// (false when [githubToken] is provided).
  final bool? useLoggedInUser;

  /// Server-wide idle timeout for sessions in seconds.
  final int? sessionIdleTimeoutSeconds;

  /// Custom session filesystem provider configuration.
  final SessionFsConfig? sessionFs;

  /// Override path to the Copilot home directory.
  final String? copilotHome;

  /// Token for TCP connection authentication.
  final String? tcpConnectionToken;

  /// Callback that returns the current W3C Trace Context for distributed tracing.
  final TraceContextProvider? onGetTraceContext;

  /// Built-in plugin directories to load.
  final List<String>? builtinPluginDirectories;

  /// Use the in-process FFI transport instead of spawning a CLI.
  final bool? inProcess;

  /// Identity of the integrating application, declared on the `connect`
  /// handshake so the runtime attributes its telemetry to this surface.
  final ClientInfo? clientInfo;

  /// Client defaulting strategy: `"coding-agent"` (default, today's
  /// behavior) or `"empty"` (ambient features off; every session must set
  /// `availableTools`). `"copilot-cli"` is accepted as an alias of
  /// `"coding-agent"`. See [ClientMode].
  final String? mode;

  /// Network callbacks (proxy resolution and proxy authentication). When any
  /// callback is set, the client calls `network.setProvider` after
  /// connecting.
  final NetworkConfig? network;

  const CopilotClientOptions({
    this.cliPath,
    this.cliArgs,
    this.cwd,
    this.port,
    this.useStdio,
    this.cliUrl,
    this.logLevel,
    this.autoStart,
    this.autoRestart,
    this.env,
    this.githubToken,
    this.useLoggedInUser,
    this.sessionIdleTimeoutSeconds,
    this.sessionFs,
    this.copilotHome,
    this.tcpConnectionToken,
    this.onGetTraceContext,
    this.builtinPluginDirectories,
    this.inProcess,
    this.clientInfo,
    this.mode,
    this.network,
  });
}

// ---------------------------------------------------------------------------
// Status / Auth Responses
// ---------------------------------------------------------------------------

/// Response from status.get.
class GetStatusResponse {
  final String version;
  final int protocolVersion;

  const GetStatusResponse(
      {required this.version, required this.protocolVersion});

  factory GetStatusResponse.fromJson(Map<String, dynamic> json) {
    return GetStatusResponse(
      version: json['version'] as String,
      protocolVersion: json['protocolVersion'] as int,
    );
  }
}

/// Response from auth.getStatus.
class GetAuthStatusResponse {
  final bool isAuthenticated;
  final String? authType;
  final String? host;
  final String? login;
  final String? statusMessage;

  const GetAuthStatusResponse({
    required this.isAuthenticated,
    this.authType,
    this.host,
    this.login,
    this.statusMessage,
  });

  factory GetAuthStatusResponse.fromJson(Map<String, dynamic> json) {
    return GetAuthStatusResponse(
      isAuthenticated: json['isAuthenticated'] as bool,
      authType: json['authType'] as String?,
      host: json['host'] as String?,
      login: json['login'] as String?,
      statusMessage: json['statusMessage'] as String?,
    );
  }
}

// ---------------------------------------------------------------------------
// Upstream-sync feature types & constants (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Per-session AI-credit budget. Set [maxAiCredits] to cap spend.
class SessionLimitsConfig {
  final num? maxAiCredits;

  const SessionLimitsConfig({this.maxAiCredits});

  Map<String, dynamic> toJson() => {
        if (maxAiCredits != null) 'maxAiCredits': maxAiCredits,
      };
}

/// Opt-in persistent session memory.
class MemoryConfiguration {
  final bool? enabled;

  const MemoryConfiguration({this.enabled});

  Map<String, dynamic> toJson() => {
        if (enabled != null) 'enabled': enabled,
      };
}

/// Arguments passed to a BYOK [BearerTokenProvider] callback.
class ProviderTokenArgs {
  final String? sessionId;

  const ProviderTokenArgs({this.sessionId});
}

/// Resolves a bearer token for BYOK providers (per-session scoping).
typedef BearerTokenProvider = Future<String?> Function(ProviderTokenArgs args);

/// Handler for MCP OAuth host token requests.
typedef McpAuthRequestHandler = Future<String?> Function(
  Map<String, dynamic> request,
  SessionInvocationContext context,
);

/// Handler invoked before an MCP tool call is dispatched.
typedef PreMcpToolCallHandler = Future<dynamic> Function(
  Map<String, dynamic> input,
  SessionInvocationContext context,
);

/// Intercept outbound LLM inference HTTP/WebSocket requests. Subclass and
/// override [sendRequest] to mutate, replace, or forward the request. BYOK
/// providers may also supply a [BearerTokenProvider].
abstract class CopilotRequestHandler {
  Future<dynamic> sendRequest(dynamic request, SessionInvocationContext context);
}

/// Tool "defer" loading policy: eager pre-load ([never]) or lazy via search ([auto]).
enum ToolDefer {
  auto('auto'),
  never('never');

  final String value;
  const ToolDefer(this.value);
}

/// System-message section identifiers. The [preamble] section targets only the
/// identity preamble; the [preserve] action protects an individually-addressable
/// section from a group-level remove.
class SystemMessageSection {
  static const String preamble = 'preamble';
  static const String identity = 'identity';
  static const String toolInstructions = 'tool_instructions';
  static const String preserve = 'preserve';
}

/// GitHub-anchored attachment variants.
class GitHubAttachment {
  static const String gitHubCommit = 'GitHubCommit';
  static const String gitHubRelease = 'GitHubRelease';
  static const String gitHubActionsJob = 'GitHubActionsJob';
  static const String gitHubRepository = 'GitHubRepository';
  static const String gitHubFileDiff = 'GitHubFileDiff';
  static const String gitHubTreeComparison = 'GitHubTreeComparison';
  static const String gitHubUrl = 'GitHubUrl';
  static const String gitHubFile = 'GitHubFile';
  static const String gitHubSnippet = 'GitHubSnippet';
}

// ---------------------------------------------------------------------------
// v2.6.0 / v3.0.0 upstream-sync types (parity with @github/copilot-sdk,
// SDK protocol 4)
// ---------------------------------------------------------------------------

/// Identity of the integrating application, sent as `clientInfo` on the
/// `connect` handshake. Empty fields are dropped; an all-empty identity is
/// not sent.
class ClientInfo {
  /// Name of the application using the SDK (for example "acme-portal").
  final String? editorName;

  /// Version of the application using the SDK.
  final String? editorVersion;

  /// Name of the integration within the application (extension, plugin).
  final String? extensionName;

  /// Version of the integration.
  final String? extensionVersion;

  const ClientInfo({
    this.editorName,
    this.editorVersion,
    this.extensionName,
    this.extensionVersion,
  });

  /// The wire form, or null when every field is empty.
  Map<String, dynamic>? toWire() {
    final wire = <String, dynamic>{
      if (editorName != null && editorName!.isNotEmpty) 'editorName': editorName,
      if (editorVersion != null && editorVersion!.isNotEmpty)
        'editorVersion': editorVersion,
      if (extensionName != null && extensionName!.isNotEmpty)
        'extensionName': extensionName,
      if (extensionVersion != null && extensionVersion!.isNotEmpty)
        'extensionVersion': extensionVersion,
    };
    return wire.isEmpty ? null : wire;
  }
}

/// Values for [CopilotClientOptions.mode].
class ClientMode {
  /// Default: identical to the Copilot CLI's ambient behavior.
  static const String codingAgent = 'coding-agent';

  /// Ambient features off; every session must set `availableTools`.
  static const String empty = 'empty';

  /// Legacy alias of [codingAgent].
  static const String copilotCli = 'copilot-cli';
}

/// Auto routing tiers for [CopilotSession.switchAutoTier].
class AutoTier {
  static const String fast = 'fast';
  static const String efficiency = 'efficiency';
  static const String balance = 'balance';
  static const String intelligence = 'intelligence';
}

/// Values for the ask-user tool variant (`askUserVariant`).
class AskUserVariant {
  static const String legacy = 'legacy';
  static const String elicitation = 'elicitation';
}

/// Request passed to a [GitHubTokenProvider] (`gitHubToken.getToken`).
class GitHubTokenRequest {
  /// Effective GitHub host the token is for.
  final String host;

  /// Session receiving the token, when known.
  final String? sessionId;

  /// Why the runtime is requesting a credential.
  final String reason;

  const GitHubTokenRequest({
    required this.host,
    this.sessionId,
    required this.reason,
  });
}

/// Result of a [GitHubTokenProvider]: a token or a cancellation.
class GitHubTokenResult {
  /// `"token"` or `"cancelled"`.
  final String kind;
  final String? accessToken;

  /// OAuth token type (defaults to bearer when omitted).
  final String? tokenType;

  /// Remaining token lifetime in seconds (must exceed one hour).
  final int? expiresIn;

  const GitHubTokenResult.token({
    required String this.accessToken,
    required int this.expiresIn,
    this.tokenType,
  }) : kind = 'token';

  const GitHubTokenResult.cancelled()
      : kind = 'cancelled',
        accessToken = null,
        tokenType = null,
        expiresIn = null;

  Map<String, dynamic> toJson() => kind == 'cancelled'
      ? {'kind': 'cancelled'}
      : {
          'kind': 'token',
          'accessToken': accessToken,
          if (tokenType != null) 'tokenType': tokenType,
          'expiresIn': expiresIn,
        };
}

/// Acquires a GitHub token for one session on demand.
typedef GitHubTokenProvider = Future<GitHubTokenResult> Function(
    GitHubTokenRequest request);

/// Sandbox configuration (`permissions.sandbox`).
class SandboxConfig {
  final bool enabled;

  /// User sandbox policy (filesystem / network rules), passed through as-is.
  final Map<String, dynamic>? userPolicy;
  final bool? addCurrentWorkingDirectory;
  final bool? sandboxMcpServers;
  final bool? sandboxLspServers;
  final bool? sandboxExtensions;

  /// Opt in to letting the agent request an individual command outside the
  /// sandbox through the normal permission flow.
  final bool? allowBypass;

  const SandboxConfig({
    required this.enabled,
    this.userPolicy,
    this.addCurrentWorkingDirectory,
    this.sandboxMcpServers,
    this.sandboxLspServers,
    this.sandboxExtensions,
    this.allowBypass,
  });

  Map<String, dynamic> toJson() => {
        'enabled': enabled,
        if (userPolicy != null) 'userPolicy': userPolicy,
        if (addCurrentWorkingDirectory != null)
          'addCurrentWorkingDirectory': addCurrentWorkingDirectory,
        if (sandboxMcpServers != null) 'sandboxMcpServers': sandboxMcpServers,
        if (sandboxLspServers != null) 'sandboxLspServers': sandboxLspServers,
        if (sandboxExtensions != null) 'sandboxExtensions': sandboxExtensions,
        if (allowBypass != null) 'allowBypass': allowBypass,
      };
}

/// Session image generation configuration (`tools.imageGeneration`).
class ImageGenerationConfig {
  final bool? enabled;

  /// Request a visible "Made with AI" label on generated images.
  final bool? watermark;

  /// Save generated images under the session-state `images` folder.
  final bool? saveToDisk;

  const ImageGenerationConfig({this.enabled, this.watermark, this.saveToDisk});

  Map<String, dynamic> toJson() => {
        if (enabled != null) 'enabled': enabled,
        if (watermark != null) 'watermark': watermark,
        if (saveToDisk != null) 'saveToDisk': saveToDisk,
      };
}

/// Autopilot continuation options (`execution.autopilot`).
class AutopilotOptions {
  final bool? continuationEnabled;
  final int? maxContinues;

  const AutopilotOptions({this.continuationEnabled, this.maxContinues});

  Map<String, dynamic> toJson() => {
        if (continuationEnabled != null)
          'continuationEnabled': continuationEnabled,
        if (maxContinues != null) 'maxContinues': maxContinues,
      };
}

/// Model capability limit overrides. Wire keys are snake_case; for example
/// [maxOutputTokens] is sent as `max_output_tokens`.
class ModelLimitsOverride {
  final int? maxPromptTokens;
  final int? maxContextWindowTokens;
  final int? maxOutputTokens;

  const ModelLimitsOverride({
    this.maxPromptTokens,
    this.maxContextWindowTokens,
    this.maxOutputTokens,
  });

  Map<String, dynamic> toJson() => {
        if (maxPromptTokens != null) 'max_prompt_tokens': maxPromptTokens,
        if (maxContextWindowTokens != null)
          'max_context_window_tokens': maxContextWindowTokens,
        if (maxOutputTokens != null) 'max_output_tokens': maxOutputTokens,
      };
}

/// Typed builder for `SessionConfig.modelCapabilities`
/// (`inference.capabilities`).
class ModelCapabilitiesOverride {
  /// Feature support overrides (for example `{'vision': true}`).
  final Map<String, dynamic>? supports;
  final ModelLimitsOverride? limits;

  const ModelCapabilitiesOverride({this.supports, this.limits});

  Map<String, dynamic> toJson() => {
        if (supports != null) 'supports': supports,
        if (limits != null) 'limits': limits!.toJson(),
      };
}

/// A named BYOK provider connection (`inference.providers`).
class NamedProviderConfig {
  /// Stable identifier referenced by [ProviderModelConfig.provider].
  final String name;
  final String? type;
  final String baseUrl;
  final String? apiKey;
  final String? apiKeyCommand;
  final String? bearerToken;
  final String? wireApi;
  final String? transport;
  final AzureProviderOptions? azure;
  final Map<String, String>? headers;
  final String? modelProvider;

  const NamedProviderConfig({
    required this.name,
    this.type,
    required this.baseUrl,
    this.apiKey,
    this.apiKeyCommand,
    this.bearerToken,
    this.wireApi,
    this.transport,
    this.azure,
    this.headers,
    this.modelProvider,
  });

  Map<String, dynamic> toJson() => {
        'name': name,
        if (type != null) 'type': type,
        'baseUrl': baseUrl,
        if (apiKey != null) 'apiKey': apiKey,
        if (apiKeyCommand != null) 'apiKeyCommand': apiKeyCommand,
        if (bearerToken != null) 'bearerToken': bearerToken,
        if (wireApi != null) 'wireApi': wireApi,
        if (transport != null) 'transport': transport,
        if (azure != null) 'azure': azure!.toJson(),
        if (headers != null) 'headers': headers,
        if (modelProvider != null) 'modelProvider': modelProvider,
      };
}

/// A BYOK model definition (`inference.models`) served by a named provider.
class ProviderModelConfig {
  /// Name of the [NamedProviderConfig] serving this model.
  final String provider;

  /// Provider-local model id; select it with `"<provider>/<id>"`.
  final String id;
  final String? modelId;
  final String? wireModel;
  final String? name;
  final int? maxPromptTokens;
  final int? maxContextWindowTokens;
  final int? maxOutputTokens;
  final Map<String, dynamic>? capabilities;

  const ProviderModelConfig({
    required this.provider,
    required this.id,
    this.modelId,
    this.wireModel,
    this.name,
    this.maxPromptTokens,
    this.maxContextWindowTokens,
    this.maxOutputTokens,
    this.capabilities,
  });

  Map<String, dynamic> toJson() => {
        'provider': provider,
        'id': id,
        if (modelId != null) 'modelId': modelId,
        if (wireModel != null) 'wireModel': wireModel,
        if (name != null) 'name': name,
        if (maxPromptTokens != null) 'maxPromptTokens': maxPromptTokens,
        if (maxContextWindowTokens != null)
          'maxContextWindowTokens': maxContextWindowTokens,
        if (maxOutputTokens != null) 'maxOutputTokens': maxOutputTokens,
        if (capabilities != null) 'capabilities': capabilities,
      };
}

/// Copilot API (CAPI) options (`github.capi`).
class CapiSessionOptions {
  /// Enables Copilot API models; `false` for BYOK-only sessions.
  final bool? enabled;

  /// Auto routing preference (see [AutoTier]).
  final String? autoTier;

  /// Use the WebSocket Responses transport.
  final bool? enableWebSocketResponses;

  const CapiSessionOptions({
    this.enabled,
    this.autoTier,
    this.enableWebSocketResponses,
  });

  Map<String, dynamic> toJson() => {
        if (enabled != null) 'enabled': enabled,
        if (autoTier != null) 'autoTier': autoTier,
        if (enableWebSocketResponses != null)
          'enableWebSocketResponses': enableWebSocketResponses,
      };
}

/// A password or other answer a shell command run by the agent asks for
/// (`shellAuthentication.request`). The answer goes only to that program.
class ShellAuthenticationRequest {
  final String sessionId;

  /// Identifies the request; a `shellAuthentication.cancel` carries it too.
  final String requestId;

  /// The program that asked: `sudo`, `ssh`, or `git`.
  final String program;
  final String prompt;

  /// `"secret"` (mask the input), `"text"`, or `"confirm"` (yes/no).
  final String inputKind;
  final String command;
  final String cwd;

  const ShellAuthenticationRequest({
    required this.sessionId,
    required this.requestId,
    required this.program,
    required this.prompt,
    required this.inputKind,
    required this.command,
    required this.cwd,
  });

  factory ShellAuthenticationRequest.fromJson(Map<String, dynamic> json) {
    return ShellAuthenticationRequest(
      sessionId: json['sessionId'] as String? ?? '',
      requestId: json['requestId'] as String? ?? '',
      program: json['program'] as String? ?? '',
      prompt: json['prompt'] as String? ?? '',
      inputKind: json['inputKind'] as String? ?? 'secret',
      command: json['command'] as String? ?? '',
      cwd: json['cwd'] as String? ?? '',
    );
  }
}

/// Answer to a [ShellAuthenticationRequest].
class ShellAuthenticationResponse {
  /// `"accept"` or `"cancel"`.
  final String action;
  final String? value;

  /// Answers the prompt with [value] (use `"yes"` for a confirm prompt).
  const ShellAuthenticationResponse.accept(String this.value)
      : action = 'accept';

  /// Declines the prompt.
  const ShellAuthenticationResponse.cancel()
      : action = 'cancel',
        value = null;

  Map<String, dynamic> toJson() => action == 'accept'
      ? {'action': 'accept', 'value': value}
      : {'action': 'cancel'};
}

/// Answers shell authentication prompts. The answer is ignored when the
/// runtime cancels the request first.
typedef ShellAuthenticationHandler = Future<ShellAuthenticationResponse>
    Function(
  ShellAuthenticationRequest request,
  SessionInvocationContext context,
);

/// A memory the agent asked to store (`memoryStore.store`), after validation,
/// secret filtering and approval.
class StoredMemory {
  final String sessionId;
  final String subject;
  final String fact;
  final String citations;
  final String reason;

  const StoredMemory({
    required this.sessionId,
    required this.subject,
    required this.fact,
    required this.citations,
    required this.reason,
  });

  factory StoredMemory.fromJson(Map<String, dynamic> json) {
    return StoredMemory(
      sessionId: json['sessionId'] as String? ?? '',
      subject: json['subject'] as String? ?? '',
      fact: json['fact'] as String? ?? '',
      citations: json['citations'] as String? ?? '',
      reason: json['reason'] as String? ?? '',
    );
  }
}

/// Stores a memory in the app. `store_memory` succeeds once this completes.
typedef MemoryStoreHandler = Future<void> Function(
  StoredMemory memory,
  SessionInvocationContext context,
);

/// Request for `network.authenticateBasic`.
class NetworkAuthenticateBasicRequest {
  /// Proxy URL that issued the Basic challenge.
  final String proxy;

  /// Realm from the proxy (untrusted text).
  final String? realm;

  /// True when the proxy rejected the previously supplied credentials.
  final bool previousRejected;

  const NetworkAuthenticateBasicRequest({
    required this.proxy,
    this.realm,
    required this.previousRejected,
  });
}

/// Credentials returned for `network.authenticateBasic`.
class NetworkBasicCredentials {
  final String? username;
  final String? password;

  const NetworkBasicCredentials({this.username, this.password});
}

/// Network callbacks through which the host decides the runtime's proxies
/// and supplies proxy credentials. Never log the returned tokens or
/// passwords.
class NetworkConfig {
  /// `"connection"` (default) or `"runtime"`.
  final String? scope;

  /// Returns a PAC-style proxy list for a URL (`DIRECT` for none), or null to
  /// let the runtime resolve it (`network.resolveProxy`).
  final Future<String?> Function(String url)? resolveProxy;

  /// Returns a base64 SPNEGO token for a service principal name, or null
  /// (`network.authenticateNegotiate`).
  final Future<String?> Function(String spn)? authenticateNegotiate;

  /// Returns Basic credentials for a proxy, or null
  /// (`network.authenticateBasic`).
  final Future<NetworkBasicCredentials?> Function(
      NetworkAuthenticateBasicRequest request)? authenticateBasic;

  const NetworkConfig({
    this.scope,
    this.resolveProxy,
    this.authenticateNegotiate,
    this.authenticateBasic,
  });

  /// True when at least one callback is set.
  bool get hasCallbacks =>
      resolveProxy != null ||
      authenticateNegotiate != null ||
      authenticateBasic != null;
}

/// Session event type names.
class SessionEventTypes {
  /// Live, append-only shell output (`tool.shell_output`); not persisted.
  static const String toolShellOutput = 'tool.shell_output';

  /// Removed upstream; still tolerated when received.
  static const String toolExecutionPartialResult =
      'tool.execution_partial_result';
}

/// Data of a `tool.shell_output` event.
class ToolShellOutputData {
  final String toolCallId;
  final String text;

  /// Zero-based publication sequence across the call's output streams.
  final int sequence;

  /// `"stdout"`, `"stderr"`, or null (stdout).
  final String? stream;

  const ToolShellOutputData({
    required this.toolCallId,
    required this.text,
    required this.sequence,
    this.stream,
  });

  factory ToolShellOutputData.fromJson(Map<String, dynamic> json) {
    return ToolShellOutputData(
      toolCallId: json['toolCallId'] as String? ?? '',
      text: json['text'] as String? ?? '',
      sequence: (json['sequence'] as num?)?.toInt() ?? 0,
      stream: json['stream'] as String?,
    );
  }
}

/// A field-specific finding reported by a Policy Helper.
class PolicyHelperFailureDetail {
  final String message;
  final String? path;

  const PolicyHelperFailureDetail({required this.message, this.path});

  factory PolicyHelperFailureDetail.fromJson(Map<String, dynamic> json) {
    return PolicyHelperFailureDetail(
      message: json['message'] as String? ?? '',
      path: json['path'] as String?,
    );
  }
}

/// The latest unresolved Policy Helper failure. Exposed from the JSON-RPC
/// error data (`error.data.policyHelperFailure`) when `session.create` /
/// `session.resume` fails (see [JsonRpcError.policyHelperFailure]). Helper
/// text is untrusted: render it as plain text.
class PolicyHelperFailure {
  final String category;
  final String failureMode;
  final String message;
  final bool previousPolicyRetained;
  final String reason;
  final String? authority;
  final String? code;
  final List<PolicyHelperFailureDetail>? details;
  final String? diagnostic;

  /// Validated additional-information link (`{url, label?}`), if any.
  final Map<String, dynamic>? helpLink;

  const PolicyHelperFailure({
    required this.category,
    required this.failureMode,
    required this.message,
    required this.previousPolicyRetained,
    required this.reason,
    this.authority,
    this.code,
    this.details,
    this.diagnostic,
    this.helpLink,
  });

  factory PolicyHelperFailure.fromJson(Map<String, dynamic> json) {
    final details = json['details'];
    final helpLink = json['helpLink'];
    return PolicyHelperFailure(
      category: json['category'] as String? ?? '',
      failureMode: json['failureMode'] as String? ?? '',
      message: json['message'] as String? ?? '',
      previousPolicyRetained: json['previousPolicyRetained'] as bool? ?? false,
      reason: json['reason'] as String? ?? '',
      authority: json['authority'] as String?,
      code: json['code'] as String?,
      details: details is List
          ? details
              .whereType<Map<String, dynamic>>()
              .map(PolicyHelperFailureDetail.fromJson)
              .toList()
          : null,
      diagnostic: json['diagnostic'] as String?,
      helpLink: helpLink is Map<String, dynamic> ? helpLink : null,
    );
  }
}

/// Known refusal reasons on a [PermissionModeSetResult].
class PermissionModeRefusalReason {
  /// Managed (enterprise) policy refused the requested permission mode.
  static const String managedPolicy = 'managed_policy';
}

/// Result of `session.permissions.setMode`.
class PermissionModeSetResult {
  /// Whether the requested mode was applied.
  final bool success;

  /// Authoritative permission mode after the call.
  final String mode;

  /// Why the mode was refused, when known (see
  /// [PermissionModeRefusalReason.managedPolicy]).
  final String? reason;

  const PermissionModeSetResult({
    required this.success,
    required this.mode,
    this.reason,
  });

  factory PermissionModeSetResult.fromJson(Map<String, dynamic> json) {
    return PermissionModeSetResult(
      success: json['success'] as bool? ?? false,
      mode: json['mode'] as String? ?? '',
      reason: json['reason'] as String?,
    );
  }
}

// ---------------------------------------------------------------------------
// v3.1.0 types (parity with the Microsoft Copilot SDK 2.0.3-preview.0)
// ---------------------------------------------------------------------------

/// Image support advertised by a `userInput.request`
/// ([UserInputRequest.imageAttachmentConstraints]). Absent (null) when the
/// runtime does not accept images in the answer.
class UserInputImageAttachmentConstraints {
  /// MIME types accepted by the request.
  final List<String> allowedMimeTypes;

  /// Maximum decoded byte length of each image.
  final int maxBytesPerImage;

  /// Maximum number of images accepted in one answer.
  final int maxCount;

  const UserInputImageAttachmentConstraints({
    required this.allowedMimeTypes,
    required this.maxBytesPerImage,
    required this.maxCount,
  });

  factory UserInputImageAttachmentConstraints.fromJson(
      Map<String, dynamic> json) {
    final mimeTypes = json['allowedMimeTypes'];
    return UserInputImageAttachmentConstraints(
      allowedMimeTypes: mimeTypes is List
          ? mimeTypes.whereType<String>().toList()
          : const <String>[],
      maxBytesPerImage: (json['maxBytesPerImage'] as num?)?.toInt() ?? 0,
      maxCount: (json['maxCount'] as num?)?.toInt() ?? 0,
    );
  }
}

/// An inline image attached to an ask_user answer
/// ([UserInputResponse.attachments]). [data] is the base64-encoded image
/// without a data URL prefix; the wire `type` is always `"image"`.
class UserInputImageAttachment {
  /// Base64-encoded image bytes.
  final String data;

  /// MIME type of the image, matching one of the request's
  /// [UserInputImageAttachmentConstraints.allowedMimeTypes].
  final String mimeType;

  /// Optional description of the image.
  final String? description;

  /// Optional client-facing name of the image.
  final String? displayName;

  const UserInputImageAttachment({
    required this.data,
    required this.mimeType,
    this.description,
    this.displayName,
  });

  /// Attachment type; always `"image"`.
  String get type => 'image';

  Map<String, dynamic> toJson() => {
        'type': type,
        'data': data,
        'mimeType': mimeType,
        if (description != null) 'description': description,
        if (displayName != null) 'displayName': displayName,
      };
}

/// Result of `CopilotClient.mcpSecretsSave` (`mcp.secrets.save`).
class McpSecretsSaveResult {
  /// True when the OS keychain stored the credential, false for the file
  /// backend.
  final bool usedKeychain;

  const McpSecretsSaveResult({required this.usedKeychain});

  factory McpSecretsSaveResult.fromJson(Map<String, dynamic> json) {
    return McpSecretsSaveResult(
      usedKeychain: json['usedKeychain'] as bool? ?? false,
    );
  }
}

/// A user-scoped approval for one installed plugin extension.
class PermissionsExtensionApproval {
  /// Stable identity, for example `plugin:plugin-name:extension-name`.
  final String extensionName;

  /// Name of the plugin that contributes the extension.
  final String pluginName;

  /// Exact privileged capabilities the user approved.
  final List<String> capabilities;

  /// When the approval was granted (RFC 3339 timestamp).
  final String grantedAt;

  /// Opaque installation key; pass it back unchanged to
  /// `CopilotClient.extensionApprovalsRevoke`.
  final String installationKey;

  /// Human-readable installation label, when known.
  final String? installationLabel;

  const PermissionsExtensionApproval({
    required this.extensionName,
    required this.pluginName,
    required this.capabilities,
    required this.grantedAt,
    required this.installationKey,
    this.installationLabel,
  });

  factory PermissionsExtensionApproval.fromJson(Map<String, dynamic> json) {
    final capabilities = json['capabilities'];
    return PermissionsExtensionApproval(
      extensionName: json['extensionName'] as String? ?? '',
      pluginName: json['pluginName'] as String? ?? '',
      capabilities: capabilities is List
          ? capabilities.whereType<String>().toList()
          : const <String>[],
      grantedAt: json['grantedAt'] as String? ?? '',
      installationKey: json['installationKey'] as String? ?? '',
      installationLabel: json['installationLabel'] as String?,
    );
  }
}

/// Result of `CopilotClient.extensionApprovalsGrant`
/// (`permissions.extensionApprovals.grant`).
class PermissionsExtensionApprovalsGrantResult {
  /// Whether the pending request was eligible and its approval was persisted.
  final bool granted;

  /// The stored approval, when one was persisted.
  final PermissionsExtensionApproval? approval;

  const PermissionsExtensionApprovalsGrantResult({
    required this.granted,
    this.approval,
  });

  factory PermissionsExtensionApprovalsGrantResult.fromJson(
      Map<String, dynamic> json) {
    final approval = json['approval'];
    return PermissionsExtensionApprovalsGrantResult(
      granted: json['granted'] as bool? ?? false,
      approval: approval is Map<String, dynamic>
          ? PermissionsExtensionApproval.fromJson(approval)
          : null,
    );
  }
}

/// Result of `CopilotClient.extensionApprovalsList`
/// (`permissions.extensionApprovals.list`).
class PermissionsExtensionApprovalsListResult {
  /// The persisted plugin extension approvals.
  final List<PermissionsExtensionApproval> approvals;

  const PermissionsExtensionApprovalsListResult({required this.approvals});

  factory PermissionsExtensionApprovalsListResult.fromJson(
      Map<String, dynamic> json) {
    final approvals = json['approvals'];
    return PermissionsExtensionApprovalsListResult(
      approvals: approvals is List
          ? approvals
              .whereType<Map<String, dynamic>>()
              .map(PermissionsExtensionApproval.fromJson)
              .toList()
          : const <PermissionsExtensionApproval>[],
    );
  }
}

/// Result of `CopilotClient.extensionApprovalsRevoke`
/// (`permissions.extensionApprovals.revoke`).
class PermissionsExtensionApprovalsRevokeResult {
  /// Whether a matching approval was revoked.
  final bool revoked;

  const PermissionsExtensionApprovalsRevokeResult({required this.revoked});

  factory PermissionsExtensionApprovalsRevokeResult.fromJson(
      Map<String, dynamic> json) {
    return PermissionsExtensionApprovalsRevokeResult(
      revoked: json['revoked'] as bool? ?? false,
    );
  }
}
