package GitHub::Copilot::Session;
# Copyright (c) Microsoft Corporation. All rights reserved.

use strict;
use warnings;
use Carp qw(croak);
use JSON::PP;
use Scalar::Util qw(blessed);
use Time::HiRes qw(time sleep);

use GitHub::Copilot::Types;

=head1 NAME

GitHub::Copilot::Session - A single conversation session with the Copilot CLI

=head1 DESCRIPTION

Represents a single conversation session. Sessions maintain conversation state,
handle events, manage tool execution, and support permission and hook handlers.

Sessions are created via C<< GitHub::Copilot::Client->create_session() >> or
resumed via C<< GitHub::Copilot::Client->resume_session() >>.

=cut

my $json = JSON::PP->new->utf8->canonical->allow_blessed->convert_blessed;

sub new {
    my ($class, %args) = @_;

    my $self = bless {
        session_id         => $args{session_id} // croak("session_id required"),
        _client            => $args{client}     // croak("client required"),
        workspace_path     => $args{workspace_path},
        _event_handlers    => [],
        _tool_handlers     => {},
        _permission_handler => undef,
        _user_input_handler => undef,
        _hooks             => undef,
        _exit_plan_mode_handler => undef,
        _trace_context_provider => undef,
        _shell_authentication_handler => undef,
        # request id => 1 once cancelled (0 while the handler runs)
        _pending_shell_authentications => {},
        _memory_store_handler => undef,
        _destroy_callbacks => [],
    }, $class;

    return $self;
}

# --------------------------------------------------------------------------
# Accessors
# --------------------------------------------------------------------------

sub session_id     { return $_[0]->{session_id} }
sub id             { return $_[0]->{session_id} }
sub workspace_path { return $_[0]->{workspace_path} }

# --------------------------------------------------------------------------
# Sending messages
# --------------------------------------------------------------------------

sub send {
    my ($self, $options) = @_;

    my $prompt = ref($options) && blessed($options) ? $options->prompt : $options->{prompt};
    my $attachments;
    my $mode;
    my $response_format;
    my $image_options;
    my $agent_mode;
    my $display_prompt;
    my $request_headers;
    my $source;
    my $response_schema;
    my $client_turn_id;

    if (ref($options) && blessed($options)) {
        $attachments     = $options->attachments;
        $mode            = $options->mode;
        $response_format = $options->response_format if $options->can('response_format');
        $image_options   = $options->image_options   if $options->can('image_options');
        $agent_mode      = $options->agent_mode      if $options->can('agent_mode');
        $display_prompt  = $options->display_prompt   if $options->can('display_prompt');
        $request_headers = $options->request_headers  if $options->can('request_headers');
        $source          = $options->source           if $options->can('source');
        $response_schema = $options->response_schema  if $options->can('response_schema');
        $client_turn_id  = $options->client_turn_id   if $options->can('client_turn_id');
    } else {
        $attachments     = $options->{attachments};
        $mode            = $options->{mode};
        $response_format = $options->{response_format};
        $image_options   = $options->{image_options};
        $agent_mode      = $options->{agent_mode};
        $display_prompt  = $options->{display_prompt};
        $request_headers = $options->{request_headers};
        $source          = $options->{source};
        $response_schema = $options->{response_schema};
        $client_turn_id  = $options->{client_turn_id};
    }

    my %payload = (
        sessionId => $self->{session_id},
        prompt    => $prompt,
    );
    # Message provenance: 'user', 'system' or 'agent-<id>' (see MessageSource)
    $payload{source}         = $source          if defined $source;
    # Opaque caller-chosen turn identifier, echoed on the turn's events
    $payload{clientTurnId}   = $client_turn_id  if defined $client_turn_id;
    $payload{attachments}    = $attachments     if defined $attachments;
    $payload{mode}           = $mode            if defined $mode;
    if (defined $response_schema) {
        # Structured output takes precedence over the legacy string response format.
        $payload{responseFormat} = {
            type       => 'json_schema',
            jsonSchema => { name => 'response', strict => \1, schema => $response_schema },
        };
    } elsif (defined $response_format) {
        $payload{responseFormat} = $response_format;
    }
    $payload{imageOptions}   = $image_options   if defined $image_options;
    # Per-message overrides (upstream sync)
    $payload{agentMode}      = $agent_mode      if defined $agent_mode;
    $payload{displayPrompt}  = $display_prompt  if defined $display_prompt;
    $payload{requestHeaders} = $request_headers if defined $request_headers;

    # Inject trace context if provider is available
    if ($self->{_trace_context_provider}) {
        eval {
            my $tc = $self->{_trace_context_provider}->();
            if ($tc) {
                my $tp = ref($tc) && $tc->can('traceparent') ? $tc->traceparent : $tc->{traceparent};
                my $ts = ref($tc) && $tc->can('tracestate') ? $tc->tracestate : $tc->{tracestate};
                $payload{traceparent} = $tp if defined $tp;
                $payload{tracestate}  = $ts if defined $ts;
            }
        };
    }

    my $response = $self->{_client}->request('session.send', \%payload);
    return $response->{messageId};
}

sub send_and_wait {
    my ($self, $options, $timeout) = @_;
    $timeout //= 60;

    my $idle_received = 0;
    my $error_event;
    my $last_assistant_message;

    # Register event handler BEFORE sending to avoid race condition
    my $unsubscribe = $self->on(sub {
        my ($event) = @_;
        if ($event->type eq 'assistant.message') {
            $last_assistant_message = $event;
        } elsif ($event->type eq 'session.idle') {
            $idle_received = 1;
        } elsif ($event->type eq 'session.error') {
            $error_event = $event;
            $idle_received = 1;  # Also unblock on error
        }
    });

    eval {
        $self->send($options);

        my $deadline = time() + $timeout;

        while (!$idle_received) {
            # Process incoming messages from the JSON-RPC client
            $self->{_client}->process_incoming();

            if (time() > $deadline) {
                croak "Timeout after ${timeout}s waiting for session.idle";
            }

            # Small sleep to avoid busy-waiting
            sleep(0.01);
        }
    };
    my $err = $@;

    # Always unsubscribe
    $unsubscribe->();

    if ($err) {
        die $err;
    }

    if ($error_event) {
        my $msg = $error_event->data->{message} // 'Unknown session error';
        croak "Session error: $msg";
    }

    return $last_assistant_message;
}

# Send a message requesting structured output and wait until the session is idle.
# Sets response_schema, waits via send_and_wait, and returns the final assistant
# content decoded from JSON. $options may be a MessageOptions object, an options
# hashref, or a prompt string.
sub send_and_wait_structured {
    my ($self, $options, $schema, $timeout) = @_;
    croak "schema is required" unless defined $schema;

    my %opts;
    if (blessed($options)) {
        for my $field (qw(prompt attachments mode response_format image_options
                          request_headers agent_mode display_prompt source client_turn_id)) {
            next unless $options->can($field);
            my $value = $options->$field;
            $opts{$field} = $value if defined $value;
        }
    } elsif (ref($options) eq 'HASH') {
        %opts = %$options;
    } else {
        %opts = (prompt => $options);
    }
    $opts{response_schema} = $schema;

    my $event = $self->send_and_wait(\%opts, $timeout);
    my $content = $event ? $event->data->{content} : undef;
    croak "The requested run completed without a structured assistant response"
        unless defined $content && $content =~ /\S/;

    return JSON::PP->new->allow_nonref->decode($content);
}

# --------------------------------------------------------------------------
# Event subscription
# --------------------------------------------------------------------------

sub on {
    my ($self, $handler) = @_;
    croak "handler must be a code reference" unless ref($handler) eq 'CODE';

    push @{ $self->{_event_handlers} }, $handler;

    # Return unsubscribe function
    return sub {
        $self->{_event_handlers} = [
            grep { $_ != $handler } @{ $self->{_event_handlers} }
        ];
    };
}

sub _dispatch_event {
    my ($self, $event) = @_;

    for my $handler (@{ $self->{_event_handlers} }) {
        eval { $handler->($event) };
        if ($@) {
            warn "Error in session event handler: $@";
        }
    }
}

# --------------------------------------------------------------------------
# Tool registration
# --------------------------------------------------------------------------

sub register_tools {
    my ($self, $tools) = @_;
    $self->{_tool_handlers} = {};
    return unless $tools && ref($tools) eq 'ARRAY';

    for my $tool (@$tools) {
        next unless $tool->name && $tool->handler;
        $self->{_tool_handlers}{ $tool->name } = $tool->handler;
    }
}

sub get_tool_handler {
    my ($self, $name) = @_;
    return $self->{_tool_handlers}{$name};
}

# --------------------------------------------------------------------------
# Permission handling
# --------------------------------------------------------------------------

sub register_permission_handler {
    my ($self, $handler) = @_;
    $self->{_permission_handler} = $handler;
}

sub _handle_permission_request {
    my ($self, $request) = @_;

    if (!$self->{_permission_handler}) {
        return { kind => 'denied-no-approval-rule-and-could-not-request-from-user' };
    }

    my $result;
    eval {
        $result = $self->{_permission_handler}->($request, { session_id => $self->{session_id} });
    };
    if ($@) {
        return { kind => 'denied-no-approval-rule-and-could-not-request-from-user' };
    }

    return $result;
}

# --------------------------------------------------------------------------
# User input handling
# --------------------------------------------------------------------------

sub register_user_input_handler {
    my ($self, $handler) = @_;
    $self->{_user_input_handler} = $handler;
}

sub _handle_user_input_request {
    my ($self, $request) = @_;

    croak "User input requested but no handler registered"
        unless $self->{_user_input_handler};

    my $input_request = GitHub::Copilot::Types::UserInputRequest->from_hashref($request);

    my $result = $self->{_user_input_handler}->(
        $input_request,
        { session_id => $self->{session_id} },
    );

    return $result;
}

# --------------------------------------------------------------------------
# Hooks handling
# --------------------------------------------------------------------------

sub register_hooks {
    my ($self, $hooks) = @_;
    $self->{_hooks} = $hooks;
}

sub _handle_hooks_invoke {
    my ($self, $hook_type, $input) = @_;

    return undef unless $self->{_hooks};

    my %handler_map = (
        preToolUse           => $self->{_hooks}{on_pre_tool_use},
        postToolUse          => $self->{_hooks}{on_post_tool_use},
        userPromptSubmitted  => $self->{_hooks}{on_user_prompt_submitted},
        userPromptTransformed => $self->{_hooks}{on_user_prompt_transformed},
        sessionStart         => $self->{_hooks}{on_session_start},
        sessionEnd           => $self->{_hooks}{on_session_end},
        errorOccurred        => $self->{_hooks}{on_error_occurred},
        subagentStart        => $self->{_hooks}{on_subagent_start},
        subagentStop         => $self->{_hooks}{on_subagent_stop},
    );

    my $handler = $handler_map{$hook_type};
    return undef unless $handler;

    my $result;
    eval {
        $result = $handler->($input, { session_id => $self->{session_id} });
    };
    if ($@) {
        warn "Hook handler error ($hook_type): $@";
        return undef;
    }

    return $result;
}

# --------------------------------------------------------------------------
# Exit plan mode handling
# --------------------------------------------------------------------------

sub register_exit_plan_mode_handler {
    my ($self, $handler) = @_;
    $self->{_exit_plan_mode_handler} = $handler;
}

sub _handle_exit_plan_mode_request {
    my ($self, $request) = @_;

    if (!$self->{_exit_plan_mode_handler}) {
        return { approved => \1 };
    }

    my $result;
    eval {
        my $req = GitHub::Copilot::Types::ExitPlanModeRequest->from_hashref($request);
        $result = $self->{_exit_plan_mode_handler}->($req);
    };
    if ($@) {
        return { approved => \1 };
    }

    return $result;
}

# --------------------------------------------------------------------------
# Trace context provider
# --------------------------------------------------------------------------

sub register_trace_context_provider {
    my ($self, $provider) = @_;
    $self->{_trace_context_provider} = $provider;
}

# Internal: register a callback run once after destroy() releases the session.
sub _on_destroy {
    my ($self, $callback) = @_;
    push @{ $self->{_destroy_callbacks} }, $callback if ref($callback) eq 'CODE';
}

# --------------------------------------------------------------------------
# Shell authentication and memory store (v3.0.0)
# --------------------------------------------------------------------------

sub register_shell_authentication_handler {
    my ($self, $handler) = @_;
    $self->{_shell_authentication_handler} = $handler;
}

# Internal: ask the registered handler for a shell program's credentials. A
# failure, a malformed answer, or a cancellation answers 'cancel' so the program
# fails instead of receiving a wrong value. The answer is never logged.
sub _handle_shell_authentication_request {
    my ($self, $request) = @_;

    my $handler = $self->{_shell_authentication_handler};
    my $request_id = $request->request_id;
    return { action => 'cancel' }
        if !$handler || exists $self->{_pending_shell_authentications}{$request_id};

    $self->{_pending_shell_authentications}{$request_id} = 0;
    my $result = eval { $handler->($request, { session_id => $self->{session_id} }) };
    if ($@) {
        # The error text could contain the answer, so only its type is logged.
        my $type = ref($@) || 'string';
        warn "[CopilotSDK] Shell authentication handler failed ($type)\n";
        $result = undef;
    }
    my $cancelled = delete $self->{_pending_shell_authentications}{$request_id};
    return { action => 'cancel' } if $cancelled;

    $result = $result->TO_JSON if blessed($result) && $result->can('TO_JSON');
    if (ref($result) eq 'HASH'
        && defined $result->{action} && $result->{action} eq 'accept'
        && defined $result->{value} && !ref $result->{value}) {
        return { action => 'accept', value => "$result->{value}" };
    }
    return { action => 'cancel' };
}

# Internal: cancel a pending shell authentication prompt; it then answers 'cancel'.
sub _cancel_shell_authentication_request {
    my ($self, $request_id) = @_;
    $self->{_pending_shell_authentications}{$request_id} = 1
        if exists $self->{_pending_shell_authentications}{$request_id};
}

sub register_memory_store {
    my ($self, $handler) = @_;
    $self->{_memory_store_handler} = $handler;
}

# Internal: hand a memory the agent asked to store to the registered callback.
sub _handle_memory_store {
    my ($self, $memory) = @_;

    my $handler = $self->{_memory_store_handler};
    croak "No memory store is registered" unless $handler;

    eval { $handler->($memory, { session_id => $self->{session_id} }); 1 } or do {
        my $err = $@;
        # The runtime reports a generic failure to the model; keep the cause visible here.
        warn "[CopilotSDK] Memory store callback failed: $err";
        croak "Memory store callback failed";
    };
    return;
}

# Change the session's permission mode ('manual', 'assisted' or 'allow-all'; see
# PermissionMode). Returns a PermissionModeSetResult whose reason explains a
# refusal when known ('managed_policy').
sub set_permission_mode {
    my ($self, $mode) = @_;

    my $result = $self->{_client}->request(
        'session.permissions.setMode',
        { sessionId => $self->{session_id}, mode => $mode },
    );
    return GitHub::Copilot::Types::PermissionModeSetResult->from_hashref(
        ref($result) eq 'HASH' ? $result : {},
    );
}

sub get_metadata {
    my ($self) = @_;

    my $response = $self->{_client}->request(
        'session.getMetadata',
        { sessionId => $self->{session_id} },
    );

    return $response;
}

# --------------------------------------------------------------------------
# Auto routing tier and managed settings
# --------------------------------------------------------------------------

# Change the Auto routing preference without changing the selected model
# (experimental). Pass undef to return to the provider's default routing.
# Returns the result hashref (status, effectiveAutoTier, pendingAutoTier,
# activatingAutoTier, supersededAutoTier).
sub set_auto_tier {
    my ($self, $auto_tier) = @_;

    return $self->{_client}->request(
        'session.model.switchAutoTier',
        { sessionId => $self->{session_id}, autoTier => $auto_tier },
    );
}

# Managed (enterprise policy) settings in effect for this session.
sub get_managed_settings {
    my ($self) = @_;

    return $self->{_client}->request(
        'session.managedSettings.get',
        { sessionId => $self->{session_id} },
    );
}

# Ask the runtime to re-run the managed-settings policy helper for this session.
sub recheck_policy_helper {
    my ($self) = @_;

    return $self->{_client}->request(
        'session.managedSettings.recheckPolicyHelper',
        { sessionId => $self->{session_id} },
    );
}

# --------------------------------------------------------------------------
# Session history
# --------------------------------------------------------------------------

sub get_messages {
    my ($self) = @_;

    my $response = $self->{_client}->request(
        'session.getMessages',
        { sessionId => $self->{session_id} },
    );

    my @events;
    for my $e (@{ $response->{events} // [] }) {
        push @events, GitHub::Copilot::Types::SessionEvent->from_hashref($e);
    }
    return \@events;
}

# --------------------------------------------------------------------------
# Session lifecycle
# --------------------------------------------------------------------------

sub destroy {
    my ($self) = @_;

    # Detach (preserving on-disk state); fall back to session.destroy on CLIs
    # that do not implement session.detach.
    my $detached = eval {
        $self->{_client}->request(
            'session.detach',
            { sessionId => $self->{session_id} },
        );
        1;
    };
    if (!$detached) {
        my $err = $@;
        die $err unless $err =~ /JSON-RPC Error -32601\b/;
        $self->{_client}->request(
            'session.destroy',
            { sessionId => $self->{session_id} },
        );
    }

    $self->{_event_handlers}     = [];
    $self->{_tool_handlers}      = {};
    $self->{_permission_handler} = undef;
    $self->{_user_input_handler} = undef;
    $self->{_hooks}              = undef;
    $self->{_exit_plan_mode_handler} = undef;
    $self->{_trace_context_provider} = undef;
    $self->{_shell_authentication_handler} = undef;
    $self->{_memory_store_handler} = undef;
    $self->{_pending_shell_authentications}{$_} = 1
        for keys %{ $self->{_pending_shell_authentications} };

    my @callbacks = @{ $self->{_destroy_callbacks} || [] };
    $self->{_destroy_callbacks} = [];
    for my $callback (@callbacks) {
        eval { $callback->() };
    }
}

sub abort {
    my ($self) = @_;

    $self->{_client}->request(
        'session.abort',
        { sessionId => $self->{session_id} },
    );
}

1;

__END__

=head1 METHODS

=head2 new(%args)

Internal constructor. Use C<< $client->create_session() >> instead.

=head2 session_id()

Returns the session ID string.

=head2 workspace_path()

Returns the workspace path (undef if infinite sessions disabled).

=head2 send($options)

Send a message. $options is a MessageOptions object or hashref with 'prompt' key.
Optional keys include C<source> ('user', 'system' or 'agent-<id>'; see
GitHub::Copilot::Types::MessageSource) and C<response_schema> (a JSON Schema
hashref, sent as a C<json_schema> responseFormat).
Returns the messageId string.

=head2 send_and_wait($options, $timeout)

Send a message and block until the session becomes idle. Returns the last
assistant.message SessionEvent, or undef if none received.

$timeout is in seconds (default: 60).

=head2 send_and_wait_structured($options, $schema, $timeout)

Send a message with C<response_schema> set to $schema, wait via send_and_wait,
and return the final assistant content decoded from JSON. Dies if the run
completed without assistant content.

=head2 set_auto_tier($auto_tier)

Change the Auto routing preference (C<session.model.switchAutoTier>);
undef returns to the default routing. Returns the result hashref.

=head2 get_managed_settings()

Return the managed settings in effect for this session.

=head2 recheck_policy_helper()

Re-run the managed-settings policy helper for this session.

=head2 set_permission_mode($mode)

Change the permission mode (C<session.permissions.setMode>) to C<manual>,
C<assisted> or C<allow-all>. Returns a
C<GitHub::Copilot::Types::PermissionModeSetResult>; its C<reason> is
C<managed_policy> when enterprise policy refused the mode.

=head2 on($handler)

Subscribe to session events. $handler is a coderef receiving a SessionEvent.
Returns an unsubscribe coderef.

=head2 register_tools($tools)

Register tool handlers. $tools is an arrayref of Tool objects.

=head2 get_tool_handler($name)

Get a registered tool handler by name.

=head2 get_messages()

Retrieve conversation history. Returns arrayref of SessionEvent objects.

=head2 get_metadata()

Retrieve session metadata. Returns a hashref of session metadata.

=head2 destroy()

Detach the session (C<session.detach>, falling back to C<session.destroy> on
older CLIs) and release resources. On-disk state is preserved for resume.

=head2 abort()

Abort the currently processing message.

=cut
