package GitHub::Copilot::Types;
# Copyright (c) Microsoft Corporation. All rights reserved.

use strict;
use warnings;

# Response format constants
use constant RESPONSE_FORMAT_TEXT        => 'text';
use constant RESPONSE_FORMAT_IMAGE       => 'image';
use constant RESPONSE_FORMAT_JSON_OBJECT => 'json_object';

=head1 NAME

GitHub::Copilot::Types - Type definitions for the GitHub Copilot Perl SDK

=head1 DESCRIPTION

All Moo-based data classes used across the Copilot SDK, including session events,
permission requests, user input, tool definitions, model info, session metadata,
hooks, and configuration types.

=cut

# ============================================================================
# SessionEvent - A generic session event from the server
# ============================================================================
package GitHub::Copilot::Types::SessionEvent;
use Moo;
use Types::Standard qw(Str HashRef Any Maybe);

has type => (is => 'ro', required => 1);
has data => (is => 'ro', default => sub { {} });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        type => $hr->{type} // 'unknown',
        data => $hr->{data} // {},
    );
}

sub TO_JSON {
    my ($self) = @_;
    return { type => $self->type, data => $self->data };
}

# ============================================================================
# PermissionRequest
# ============================================================================
package GitHub::Copilot::Types::PermissionRequest;
use Moo;

has kind        => (is => 'ro', required => 1);
has toolCallId  => (is => 'ro', default => sub { undef });
has extra       => (is => 'ro', default => sub { {} });

sub from_hashref {
    my ($class, $hr) = @_;
    my %extra = %{ $hr // {} };
    my $kind       = delete $extra{kind}       // 'unknown';
    my $toolCallId = delete $extra{toolCallId};
    return $class->new(
        kind       => $kind,
        toolCallId => $toolCallId,
        extra      => \%extra,
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = %{ $self->extra // {} };
    $h{kind}       = $self->kind;
    $h{toolCallId} = $self->toolCallId if defined $self->toolCallId;
    return \%h;
}

# ============================================================================
# PermissionRequestResult
# ============================================================================
package GitHub::Copilot::Types::PermissionRequestResult;
use Moo;

has kind  => (is => 'ro', required => 1);
has rules => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        kind  => $hr->{kind} // 'denied-no-approval-rule-and-could-not-request-from-user',
        rules => $hr->{rules},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (kind => $self->kind);
    $h{rules} = $self->rules if defined $self->rules;
    return \%h;
}

# ============================================================================
# UserInputRequest
# ============================================================================
package GitHub::Copilot::Types::UserInputRequest;
use Moo;

has question      => (is => 'ro', required => 1);
has choices       => (is => 'ro', default => sub { [] });
has allowFreeform => (is => 'ro', default => sub { 1 });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        question      => $hr->{question}      // '',
        choices       => $hr->{choices}        // [],
        allowFreeform => $hr->{allowFreeform}  // 1,
    );
}

sub TO_JSON {
    my ($self) = @_;
    return {
        question      => $self->question,
        choices       => $self->choices,
        allowFreeform => $self->allowFreeform ? \1 : \0,
    };
}

# ============================================================================
# UserInputResponse
# ============================================================================
package GitHub::Copilot::Types::UserInputResponse;
use Moo;

has answer      => (is => 'ro', required => 1);
has wasFreeform => (is => 'ro', default => sub { 0 });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        answer      => $hr->{answer}      // '',
        wasFreeform => $hr->{wasFreeform} // 0,
    );
}

sub TO_JSON {
    my ($self) = @_;
    return {
        answer      => $self->answer,
        wasFreeform => $self->wasFreeform ? \1 : \0,
    };
}

# ============================================================================
# PingResponse
# ============================================================================
package GitHub::Copilot::Types::PingResponse;
use Moo;

has message         => (is => 'ro', required => 1);
has timestamp       => (is => 'ro', required => 1);
has protocolVersion => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        message         => $hr->{message}         // '',
        timestamp       => $hr->{timestamp}       // 0,
        protocolVersion => $hr->{protocolVersion},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        message   => $self->message,
        timestamp => $self->timestamp,
    );
    $h{protocolVersion} = $self->protocolVersion if defined $self->protocolVersion;
    return \%h;
}

# ============================================================================
# ModelInfo
# ============================================================================
package GitHub::Copilot::Types::ModelInfo;
use Moo;

has id                        => (is => 'ro', required => 1);
has name                      => (is => 'ro', required => 1);
has capabilities              => (is => 'ro', default => sub { {} });
has policy                    => (is => 'ro', default => sub { undef });
has billing                   => (is => 'ro', default => sub { undef });
has supportedReasoningEfforts => (is => 'ro', default => sub { undef });
has defaultReasoningEffort    => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        id                        => $hr->{id}           // '',
        name                      => $hr->{name}         // '',
        capabilities              => $hr->{capabilities}  // {},
        policy                    => $hr->{policy},
        billing                   => $hr->{billing},
        supportedReasoningEfforts => $hr->{supportedReasoningEfforts},
        defaultReasoningEffort    => $hr->{defaultReasoningEffort},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        id           => $self->id,
        name         => $self->name,
        capabilities => $self->capabilities,
    );
    $h{policy}                    = $self->policy                    if defined $self->policy;
    $h{billing}                   = $self->billing                   if defined $self->billing;
    $h{supportedReasoningEfforts} = $self->supportedReasoningEfforts if defined $self->supportedReasoningEfforts;
    $h{defaultReasoningEffort}    = $self->defaultReasoningEffort    if defined $self->defaultReasoningEffort;
    return \%h;
}

# ============================================================================
# SessionMetadata
# ============================================================================
package GitHub::Copilot::Types::SessionMetadata;
use Moo;

has sessionId    => (is => 'ro', required => 1);
has startTime    => (is => 'ro', required => 1);
has modifiedTime => (is => 'ro', required => 1);
has isRemote     => (is => 'ro', default => sub { 0 });
has summary      => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        sessionId    => $hr->{sessionId}    // '',
        startTime    => $hr->{startTime}    // '',
        modifiedTime => $hr->{modifiedTime} // '',
        isRemote     => $hr->{isRemote}     // 0,
        summary      => $hr->{summary},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        sessionId    => $self->sessionId,
        startTime    => $self->startTime,
        modifiedTime => $self->modifiedTime,
        isRemote     => $self->isRemote ? \1 : \0,
    );
    $h{summary} = $self->summary if defined $self->summary;
    return \%h;
}

# ============================================================================
# Tool - definition of a tool exposed to the CLI server
# ============================================================================
package GitHub::Copilot::Types::Tool;
use Moo;

has name        => (is => 'ro', required => 1);
has description => (is => 'ro', default => sub { '' });
has parameters  => (is => 'ro', default => sub { undef });
has handler     => (is => 'ro', required => 1);

sub to_wire {
    my ($self) = @_;
    my %h = (
        name        => $self->name,
        description => $self->description,
    );
    $h{parameters} = $self->parameters if defined $self->parameters;
    return \%h;
}

# ============================================================================
# ToolInvocation - context passed to tool handlers
# ============================================================================
package GitHub::Copilot::Types::ToolInvocation;
use Moo;

has sessionId  => (is => 'ro', required => 1);
has toolCallId => (is => 'ro', required => 1);
has toolName   => (is => 'ro', required => 1);
has arguments  => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        sessionId  => $hr->{sessionId}  // '',
        toolCallId => $hr->{toolCallId} // '',
        toolName   => $hr->{toolName}   // '',
        arguments  => $hr->{arguments},
    );
}

# ============================================================================
# ToolResultObject - structured tool result
# ============================================================================
package GitHub::Copilot::Types::ToolResultObject;
use Moo;

has textResultForLlm     => (is => 'ro', required => 1);
has resultType           => (is => 'ro', default => sub { 'success' });
has binaryResultsForLlm  => (is => 'ro', default => sub { undef });
has error                => (is => 'ro', default => sub { undef });
has sessionLog           => (is => 'ro', default => sub { undef });
has toolTelemetry        => (is => 'ro', default => sub { {} });

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        textResultForLlm => $self->textResultForLlm,
        resultType       => $self->resultType,
    );
    $h{binaryResultsForLlm} = $self->binaryResultsForLlm if defined $self->binaryResultsForLlm;
    $h{error}               = $self->error                if defined $self->error;
    $h{sessionLog}          = $self->sessionLog           if defined $self->sessionLog;
    $h{toolTelemetry}       = $self->toolTelemetry        if defined $self->toolTelemetry;
    return \%h;
}

# ============================================================================
# ImageOptions - configuration for image generation
# ============================================================================
package GitHub::Copilot::Types::ImageOptions;
use Moo;

has size    => (is => 'ro', default => sub { undef });
has quality => (is => 'ro', default => sub { undef });
has style   => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        size    => $hr->{size},
        quality => $hr->{quality},
        style   => $hr->{style},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h;
    $h{size}    = $self->size    if defined $self->size;
    $h{quality} = $self->quality if defined $self->quality;
    $h{style}   = $self->style   if defined $self->style;
    return \%h;
}

# ============================================================================
# AssistantImageData - image data returned by the assistant
# ============================================================================
package GitHub::Copilot::Types::AssistantImageData;
use Moo;

has format        => (is => 'ro', default => sub { undef });
has base64        => (is => 'ro', default => sub { undef });
has url           => (is => 'ro', default => sub { undef });
has revisedPrompt => (is => 'ro', default => sub { undef });
has width         => (is => 'ro', default => sub { undef });
has height        => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        format        => $hr->{format},
        base64        => $hr->{base64},
        url           => $hr->{url},
        revisedPrompt => $hr->{revisedPrompt},
        width         => $hr->{width},
        height        => $hr->{height},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h;
    $h{format}        = $self->format        if defined $self->format;
    $h{base64}        = $self->base64        if defined $self->base64;
    $h{url}           = $self->url           if defined $self->url;
    $h{revisedPrompt} = $self->revisedPrompt if defined $self->revisedPrompt;
    $h{width}         = $self->width         if defined $self->width;
    $h{height}        = $self->height        if defined $self->height;
    return \%h;
}

# ============================================================================
# ContentBlock - a block of content (text or image) in an assistant response
# ============================================================================
package GitHub::Copilot::Types::ContentBlock;
use Moo;

has type  => (is => 'ro', required => 1);
has text  => (is => 'ro', default => sub { undef });
has image => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    my $image;
    if (defined $hr->{image}) {
        $image = GitHub::Copilot::Types::AssistantImageData->from_hashref($hr->{image});
    }
    return $class->new(
        type  => $hr->{type} // 'text',
        text  => $hr->{text},
        image => $image,
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (type => $self->type);
    $h{text}  = $self->text  if defined $self->text;
    $h{image} = $self->image if defined $self->image;
    return \%h;
}

# ============================================================================
# MessageOptions
# ============================================================================
package GitHub::Copilot::Types::MessageOptions;
use Moo;

has prompt          => (is => 'ro', required => 1);
has attachments     => (is => 'ro', default => sub { undef });
has mode            => (is => 'ro', default => sub { undef });
has response_format => (is => 'ro', default => sub { undef });
has image_options   => (is => 'ro', default => sub { undef });
# Custom HTTP headers for outbound model requests
has request_headers => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h = (prompt => $self->prompt);
    $h{attachments}    = $self->attachments     if defined $self->attachments;
    $h{mode}           = $self->mode            if defined $self->mode;
    $h{responseFormat} = $self->response_format if defined $self->response_format;
    $h{imageOptions}   = $self->image_options   if defined $self->image_options;
    $h{requestHeaders} = $self->request_headers if defined $self->request_headers;
    return \%h;
}

# ============================================================================
# MCPLocalServerConfig - local/stdio MCP server configuration
# ============================================================================
package GitHub::Copilot::Types::MCPLocalServerConfig;
use Moo;

has tools   => (is => 'ro', default => sub { ['*'] });
has type    => (is => 'ro', default => sub { undef });
has timeout => (is => 'ro', default => sub { undef });
has command => (is => 'ro', required => 1);
has args    => (is => 'ro', default => sub { [] });
has env     => (is => 'ro', default => sub { undef });
has cwd     => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        tools   => $self->tools,
        command => $self->command,
        args    => $self->args,
    );
    $h{type}    = $self->type    if defined $self->type;
    $h{timeout} = $self->timeout if defined $self->timeout;
    $h{env}     = $self->env     if defined $self->env;
    $h{cwd}     = $self->cwd     if defined $self->cwd;
    return \%h;
}

# ============================================================================
# MCPRemoteServerConfig - remote HTTP/SSE MCP server configuration
# ============================================================================
package GitHub::Copilot::Types::MCPRemoteServerConfig;
use Moo;

has tools   => (is => 'ro', default => sub { ['*'] });
has type    => (is => 'ro', required => 1);
has timeout => (is => 'ro', default => sub { undef });
has url     => (is => 'ro', required => 1);
has headers => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        tools => $self->tools,
        type  => $self->type,
        url   => $self->url,
    );
    $h{timeout} = $self->timeout if defined $self->timeout;
    $h{headers} = $self->headers if defined $self->headers;
    return \%h;
}

# ============================================================================
# Commands
# ============================================================================
package GitHub::Copilot::Types::CommandContext;
use Moo;

has session_id    => (is => 'ro', required => 1);
has command       => (is => 'ro', required => 1);
has command_name  => (is => 'ro', required => 1);
has args          => (is => 'ro', default => sub { '' });

# Definition of a slash command registered with the session.
package GitHub::Copilot::Types::CommandDefinition;
use Moo;

has name        => (is => 'ro', required => 1);
has description => (is => 'ro', default => sub { undef });
has handler     => (is => 'ro', required => 1);

# ============================================================================
# UI Elicitation
# ============================================================================
package GitHub::Copilot::Types::ElicitationContext;
use Moo;

has session_id         => (is => 'ro', required => 1);
has message            => (is => 'ro', required => 1);
has requested_schema   => (is => 'ro', default => sub { undef });
has mode               => (is => 'ro', default => sub { undef });
has elicitation_source => (is => 'ro', default => sub { undef });
has url                => (is => 'ro', default => sub { undef });

# Result returned from an elicitation handler.
package GitHub::Copilot::Types::ElicitationResult;
use Moo;

has action  => (is => 'ro', required => 1);
has content => (is => 'ro', default => sub { undef });

# ============================================================================
# SessionConfig - configuration for creating a session
# ============================================================================
package GitHub::Copilot::Types::SessionConfig;
use Moo;

has session_id             => (is => 'ro', default => sub { undef });
has model                  => (is => 'ro', default => sub { undef });
has reasoning_effort       => (is => 'ro', default => sub { undef });
has tools                  => (is => 'ro', default => sub { [] });
has system_message         => (is => 'ro', default => sub { undef });
has available_tools        => (is => 'ro', default => sub { undef });
has excluded_tools         => (is => 'ro', default => sub { undef });
has provider               => (is => 'ro', default => sub { undef });
has on_permission_request  => (is => 'ro', default => sub { undef });
has on_user_input_request  => (is => 'ro', default => sub { undef });
has hooks                  => (is => 'ro', default => sub { undef });
has working_directory      => (is => 'ro', default => sub { undef });
has streaming              => (is => 'ro', default => sub { undef });
has mcp_servers            => (is => 'ro', default => sub { undef });
has custom_agents          => (is => 'ro', default => sub { undef });
has config_dir             => (is => 'ro', default => sub { undef });
has skill_directories      => (is => 'ro', default => sub { undef });
has disabled_skills        => (is => 'ro', default => sub { undef });
has infinite_sessions      => (is => 'ro', default => sub { undef });
# Model capabilities overrides
has model_capabilities     => (is => 'ro', default => sub { undef });
# Auto-discover MCP server configs (default: false)
has enable_config_discovery => (is => 'ro', default => sub { undef });
# Include sub-agent streaming events (default: true)
has include_sub_agent_streaming_events => (is => 'ro', default => sub { undef });
# GitHub token for authentication. Overrides client-level token for this session only.
has github_token              => (is => 'ro', default => sub { undef });
has commands                => (is => 'ro', default => sub { undef });
has on_elicitation_request  => (is => 'ro', default => sub { undef });

sub to_wire {
    my ($self) = @_;
    my %payload;

    $payload{sessionId}       = $self->session_id       if defined $self->session_id;
    $payload{model}           = $self->model             if defined $self->model;
    $payload{reasoningEffort} = $self->reasoning_effort  if defined $self->reasoning_effort;

    if ($self->tools && @{ $self->tools }) {
        $payload{tools} = [ map { $_->to_wire } @{ $self->tools } ];
    }

    $payload{systemMessage}    = $self->system_message    if defined $self->system_message;
    $payload{availableTools}   = $self->available_tools   if defined $self->available_tools;
    $payload{excludedTools}    = $self->excluded_tools    if defined $self->excluded_tools;
    $payload{provider}         = $self->provider          if defined $self->provider;
    $payload{workingDirectory} = $self->working_directory if defined $self->working_directory;
    $payload{configDir}        = $self->config_dir        if defined $self->config_dir;
    $payload{mcpServers}       = $self->mcp_servers       if defined $self->mcp_servers;
    $payload{customAgents}     = $self->custom_agents     if defined $self->custom_agents;
    $payload{skillDirectories} = $self->skill_directories if defined $self->skill_directories;
    $payload{disabledSkills}   = $self->disabled_skills   if defined $self->disabled_skills;
    $payload{infiniteSessions} = $self->infinite_sessions if defined $self->infinite_sessions;
    $payload{modelCapabilities} = $self->model_capabilities if defined $self->model_capabilities;
    if (defined $self->enable_config_discovery) {
        $payload{enableConfigDiscovery} = $self->enable_config_discovery ? \1 : \0;
    }
    if (defined $self->include_sub_agent_streaming_events) {
        $payload{includeSubAgentStreamingEvents} = $self->include_sub_agent_streaming_events ? \1 : \0;
    }
    if (defined $self->github_token) {
        $payload{gitHubToken} = $self->github_token;
    }

    $payload{requestPermission} = \1 if defined $self->on_permission_request;
    $payload{requestUserInput}  = \1 if defined $self->on_user_input_request;

    if (defined $self->hooks) {
        my $hooks = $self->hooks;
        my $has_hooks = 0;
        for my $key (keys %$hooks) {
            if (defined $hooks->{$key}) {
                $has_hooks = 1;
                last;
            }
        }
        $payload{hooks} = \1 if $has_hooks;
    }

    if (defined $self->streaming) {
        $payload{streaming} = $self->streaming ? \1 : \0;
    }

    return \%payload;
}

# ============================================================================
# System message configuration types
# ============================================================================

# Known system prompt section identifiers for "customize" mode.
package GitHub::Copilot::Types::SystemPromptSection;

use constant {
    IDENTITY            => 'identity',
    TONE                => 'tone',
    TOOL_EFFICIENCY     => 'tool_efficiency',
    ENVIRONMENT_CONTEXT => 'environment_context',
    CODE_CHANGE_RULES   => 'code_change_rules',
    GUIDELINES          => 'guidelines',
    SAFETY              => 'safety',
    TOOL_INSTRUCTIONS   => 'tool_instructions',
    CUSTOM_INSTRUCTIONS => 'custom_instructions',
    LAST_INSTRUCTIONS   => 'last_instructions',
};

# Override action for a system prompt section.
package GitHub::Copilot::Types::SectionOverrideAction;

use constant {
    REPLACE => 'replace',
    REMOVE  => 'remove',
    APPEND  => 'append',
    PREPEND => 'prepend',
};

# Override operation for a single system prompt section.
package GitHub::Copilot::Types::SectionOverride;
use Moo;

has action  => (is => 'ro', required => 1);
has content => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h = (action => $self->action);
    $h{content} = $self->content if defined $self->content;
    return \%h;
}

# System message configuration.
# Supports "append" (default), "replace", and "customize" modes.
package GitHub::Copilot::Types::SystemMessageConfig;
use Moo;

has mode     => (is => 'ro', default => sub { undef });
has content  => (is => 'ro', default => sub { undef });
has sections => (is => 'ro', default => sub { undef }); # hashref of section => SectionOverride

sub TO_JSON {
    my ($self) = @_;
    my %h;
    $h{mode}    = $self->mode    if defined $self->mode;
    $h{content} = $self->content if defined $self->content;
    if (defined $self->sections) {
        my %sec;
        for my $key (keys %{ $self->sections }) {
            $sec{$key} = $self->sections->{$key}->TO_JSON;
        }
        $h{sections} = \%sec;
    }
    return \%h;
}

# ============================================================================
# ClientConfig - client/connection configuration
# ============================================================================
package GitHub::Copilot::Types::ClientConfig;
use Moo;

# Server-wide idle timeout for sessions in seconds
has session_idle_timeout_seconds => (is => 'ro', default => sub { undef });
# Session filesystem provider (hashref of code references)
has session_fs => (is => 'ro', default => sub { undef });
# GitHub token for authentication.
has github_token => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h;
    $h{sessionIdleTimeoutSeconds} = $self->session_idle_timeout_seconds
        if defined $self->session_idle_timeout_seconds;
    $h{gitHubToken} = $self->github_token
        if defined $self->github_token;
    return \%h;
}

# ============================================================================
# CustomAgentConfig - configuration for a custom agent
# ============================================================================
package GitHub::Copilot::Types::CustomAgentConfig;
use Moo;

has name        => (is => 'ro', required => 1);
has description => (is => 'ro', default => sub { undef });
has prompt      => (is => 'ro', default => sub { undef });
# List of skill names to preload
has skills      => (is => 'ro', default => sub { undef });

sub TO_JSON {
    my ($self) = @_;
    my %h = (name => $self->name);
    $h{description} = $self->description if defined $self->description;
    $h{prompt}      = $self->prompt      if defined $self->prompt;
    $h{skills}      = $self->skills      if defined $self->skills;
    return \%h;
}

# ============================================================================
# SessionHooks - configuration for session hooks (hashref-based)
# ============================================================================
# Hooks are passed as a simple hashref with keys:
#   on_pre_tool_use         => sub { ... }
#   on_post_tool_use        => sub { ... }
#   on_user_prompt_submitted => sub { ... }
#   on_session_start        => sub { ... }
#   on_session_end          => sub { ... }
#   on_error_occurred       => sub { ... }
#
# The mapping to wire hookType strings is:
#   preToolUse           => on_pre_tool_use
#   postToolUse          => on_post_tool_use
#   userPromptSubmitted  => on_user_prompt_submitted
#   sessionStart         => on_session_start
#   sessionEnd           => on_session_end
#   errorOccurred        => on_error_occurred

# ============================================================================
# SessionFsConfig - configuration for a session filesystem provider
# ============================================================================
package GitHub::Copilot::Types::SessionFsConfig;
use Moo;

has initial_cwd        => (is => 'ro', required => 1);
has session_state_path => (is => 'ro', required => 1);
has conventions        => (is => 'ro', required => 1);

sub TO_JSON {
    my ($self) = @_;
    return {
        initialCwd       => $self->initial_cwd,
        sessionStatePath => $self->session_state_path,
        conventions      => $self->conventions,
    };
}

# ============================================================================
# SessionFsFileInfo - file metadata from session filesystem operations
# ============================================================================
package GitHub::Copilot::Types::SessionFsFileInfo;
use Moo;

has name         => (is => 'ro', required => 1);
has size         => (is => 'ro', default => sub { 0 });
has isDirectory  => (is => 'ro', default => sub { 0 });
has isFile       => (is => 'ro', default => sub { 0 });
has createdAt    => (is => 'ro', default => sub { undef });
has modifiedAt   => (is => 'ro', default => sub { undef });

sub from_hashref {
    my ($class, $hr) = @_;
    return $class->new(
        name        => $hr->{name}        // '',
        size        => $hr->{size}        // 0,
        isDirectory => $hr->{isDirectory} // 0,
        isFile      => $hr->{isFile}      // 0,
        createdAt   => $hr->{createdAt},
        modifiedAt  => $hr->{modifiedAt},
    );
}

sub TO_JSON {
    my ($self) = @_;
    my %h = (
        name        => $self->name,
        size        => $self->size,
        isDirectory => $self->isDirectory ? \1 : \0,
        isFile      => $self->isFile      ? \1 : \0,
    );
    $h{createdAt}  = $self->createdAt  if defined $self->createdAt;
    $h{modifiedAt} = $self->modifiedAt if defined $self->modifiedAt;
    return \%h;
}

# ============================================================================
# SessionFsProvider convention
# ============================================================================
# To implement a session filesystem provider in Perl, supply a hashref with
# the following code references:
#
#   read_file         => sub { my ($session_id, $path) = @_; ... }
#   write_file        => sub { my ($session_id, $path, $content) = @_; ... }
#   append_file       => sub { my ($session_id, $path, $content) = @_; ... }
#   exists            => sub { my ($session_id, $path) = @_; ... }
#   stat              => sub { my ($session_id, $path) = @_; ... }
#   mkdir             => sub { my ($session_id, $path, $recursive) = @_; ... }
#   readdir           => sub { my ($session_id, $path) = @_; ... }
#   readdir_with_types => sub { my ($session_id, $path) = @_; ... }
#   rm                => sub { my ($session_id, $path, $recursive) = @_; ... }
#   rename            => sub { my ($session_id, $old_path, $new_path) = @_; ... }

1;

__END__

=head1 NAME

GitHub::Copilot::Types - Type definitions for the Copilot Perl SDK

=head1 SYNOPSIS

    use GitHub::Copilot::Types;

    my $event = GitHub::Copilot::Types::SessionEvent->new(
        type => 'assistant.message',
        data => { content => 'Hello!' },
    );

    my $tool = GitHub::Copilot::Types::Tool->new(
        name        => 'get_weather',
        description => 'Get weather for a city',
        parameters  => { type => 'object', properties => { city => { type => 'string' } } },
        handler     => sub { my ($args, $invocation) = @_; return "72F in $args->{city}"; },
    );

=head1 DESCRIPTION

This module defines all Moo-based data classes used by the Copilot Perl SDK,
mirroring the types used in the Node.js and Python SDKs.

=cut
