package GitHub::Copilot::JsonRpcClient;
# Copyright (c) Microsoft Corporation. All rights reserved.

use strict;
use warnings;
use Carp qw(croak);
use Encode qw(is_utf8 encode);
use JSON::PP;
our $HAS_THREADS;
BEGIN {
    $HAS_THREADS = eval { require threads; threads->import(); 1 };
    if ($HAS_THREADS) {
        require threads::shared;
        threads::shared->import();
    }
}
use Thread::Queue;
use IO::Select;
use Time::HiRes qw(time sleep);
use Scalar::Util qw(blessed);
use Errno qw(EAGAIN EWOULDBLOCK);

=head1 NAME

GitHub::Copilot::JsonRpcClient - JSON-RPC 2.0 client over stdio with Content-Length framing

=head1 DESCRIPTION

A minimal JSON-RPC 2.0 client that communicates over stdin/stdout of a child
process using Content-Length header framing (the same wire format used by
LSP / vscode-jsonrpc).

Messages are read on demand (single-threaded): C<request()> and
C<process_incoming()> pull bytes from stdout, frame them by Content-Length, and
route each message to the appropriate handler:

=over

=item * Responses are matched to pending requests by id.

=item * Notifications (no id) are dispatched to a registered notification handler.

=item * Incoming requests (method + id from server) are dispatched to registered
request handlers and responses are sent back.

=back

=cut

my $json = JSON::PP->new->utf8->canonical->allow_blessed->convert_blessed;
my $REQUEST_COUNTER = 0;

sub _generate_request_id {
    $REQUEST_COUNTER = ($REQUEST_COUNTER + 1) & 0xffff;
    my $micros = int(time() * 1_000_000);
    my $random = int(rand(0xffff));
    return sprintf('perl-%d-%d-%04x-%04x', $$, $micros, $REQUEST_COUNTER, $random);
}

sub new {
    my ($class, %args) = @_;

    my $stdin_fh  = $args{stdin}  or croak "stdin filehandle required";
    my $stdout_fh = $args{stdout} or croak "stdout filehandle required";

    # Make sure filehandles are in binary/raw mode
    binmode($stdin_fh,  ':raw');
    binmode($stdout_fh, ':raw');

    my %self = (
        stdin_fh              => $stdin_fh,
        stdout_fh             => $stdout_fh,
        _pending              => {},   # id => 1 for in-flight requests
        _responses            => {},   # id => decoded response message
        _notification_handler => undef,
        _request_handlers     => {},
        _running              => 0,
        _incoming             => [],   # queued notifications/requests (decoded)
        _read_buf             => '',   # raw bytes not yet framed into a message
        _selector             => undef,# IO::Select over stdout_fh (created lazily)
    );

    return bless \%self, $class;
}

# --------------------------------------------------------------------------
# Public API
# --------------------------------------------------------------------------

sub start {
    my ($self) = @_;
    return if $self->{_running};
    $self->{_running} = 1;
    # Single-threaded design: messages are read on demand by request() and
    # process_incoming(). No background reader thread is used because Perl
    # ithreads cannot safely share the dynamically-created response map, and a
    # detached reader blocked on read() would keep the interpreter alive at exit.
    return $self;
}

sub stop {
    my ($self) = @_;
    $self->{_running} = 0;
    %{ $self->{_pending} }   = ();
    %{ $self->{_responses} } = ();
    @{ $self->{_incoming} }  = ();
}

sub set_notification_handler {
    my ($self, $handler) = @_;
    $self->{_notification_handler} = $handler;
}

sub set_request_handler {
    my ($self, $method, $handler) = @_;
    if (defined $handler) {
        $self->{_request_handlers}{$method} = $handler;
    } else {
        delete $self->{_request_handlers}{$method};
    }
}

sub request {
    my ($self, $method, $params, $timeout) = @_;
    $params  //= {};
    $timeout //= 60;

    croak "Client not started" unless $self->{_running};

    my $id = _generate_request_id();
    $self->{_pending}{$id} = 1;

    my $message = {
        jsonrpc => '2.0',
        id      => $id,
        method  => $method,
        params  => $params,
    };

    $self->_send_message($message);

    # Wait for the matching response, reading and dispatching any interleaved
    # notifications/requests as they arrive. Reads are bounded by select() so a
    # silent server cannot block us past the deadline.
    my $deadline = time() + $timeout;
    my $resp;
    while (1) {
        $self->_pump(0.05);          # read + route available messages
        $self->_process_incoming();  # dispatch notifications/requests

        if (exists $self->{_responses}{$id}) {
            $resp = delete $self->{_responses}{$id};
            last;
        }

        if (time() >= $deadline) {
            delete $self->{_pending}{$id};
            croak "JSON-RPC request '$method' timed out after ${timeout}s";
        }
    }

    delete $self->{_pending}{$id};

    if (exists $resp->{error}) {
        my $err = $resp->{error};
        croak sprintf(
            "JSON-RPC Error %d: %s",
            $err->{code}    // -1,
            $err->{message} // 'Unknown error',
        );
    }

    return $resp->{result};
}

sub notify {
    my ($self, $method, $params) = @_;
    $params //= {};

    my $message = {
        jsonrpc => '2.0',
        method  => $method,
        params  => $params,
    };

    $self->_send_message($message);
}

# --------------------------------------------------------------------------
# Process incoming requests/notifications that were queued by the reader thread
# --------------------------------------------------------------------------

sub _process_incoming {
    my ($self) = @_;

    # Pull any messages that are already available (non-blocking) so callers
    # polling for session events actually observe them without a reader thread.
    $self->_pump(0) if $self->{_running};

    while (my $item = shift @{ $self->{_incoming} }) {
        $self->_dispatch_incoming($item);
    }
}

sub process_incoming {
    my ($self) = @_;
    $self->_process_incoming();
}

# --------------------------------------------------------------------------
# Read available bytes and frame them into JSON-RPC messages, dispatching each
# via _handle_message. Waits up to $timeout seconds for the first bytes, then
# drains whatever else is immediately available and returns promptly.
# --------------------------------------------------------------------------

sub _pump {
    my ($self, $timeout) = @_;
    $timeout //= 0;

    $self->{_selector} //= IO::Select->new($self->{stdout_fh});

    my $deadline = time() + $timeout;
    while (1) {
        # Frame and dispatch every complete message already in the buffer.
        while (defined(my $msg = $self->_try_parse_message())) {
            $self->_handle_message($msg);
        }

        my $remaining = $deadline - time();
        $remaining = 0 if $remaining < 0;
        last unless $self->{_selector}->can_read($remaining);

        my $chunk = '';
        my $n = sysread($self->{stdout_fh}, $chunk, 65536);
        if (!defined $n) {
            last if $! == EAGAIN || $! == EWOULDBLOCK;
            die "JSON-RPC read error: $!";
        }
        last if $n == 0;  # EOF: server closed stdout
        $self->{_read_buf} .= $chunk;

        # After the first successful read only keep draining what is already
        # buffered/available (deadline now) so we never block for more data.
        $deadline = time();
    }
}

# Frame one complete Content-Length message out of the read buffer. Returns the
# decoded hashref, or undef if a full message is not yet buffered.
sub _try_parse_message {
    my ($self) = @_;

    my $sep = index($self->{_read_buf}, "\r\n\r\n");
    return undef if $sep < 0;

    my $header = substr($self->{_read_buf}, 0, $sep);
    return undef unless $header =~ /Content-Length:\s*(\d+)/i;
    my $len = $1;

    my $body_start = $sep + 4;
    return undef if length($self->{_read_buf}) < $body_start + $len;

    my $body = substr($self->{_read_buf}, $body_start, $len);
    substr($self->{_read_buf}, 0, $body_start + $len) = '';

    return $json->decode($body);
}

# --------------------------------------------------------------------------
# Message sending (thread-safe)
# --------------------------------------------------------------------------

sub _send_message {
    my ($self, $message) = @_;

    my $content = $json->encode($message);
    my $content_bytes = is_utf8($content) ?
        encode('UTF-8', $content) : $content;
    my $header = "Content-Length: " . length($content_bytes) . "\r\n\r\n";

    my $fh = $self->{stdin_fh};
    # Simple lock via local flock-like mechanism
    print $fh $header;
    print $fh $content_bytes;
    $fh->flush();
}

sub _send_response {
    my ($self, $id, $result) = @_;
    my $response = {
        jsonrpc => '2.0',
        id      => $id,
        result  => $result,
    };
    $self->_send_message($response);
}

sub _send_error_response {
    my ($self, $id, $code, $message, $data) = @_;
    my $response = {
        jsonrpc => '2.0',
        id      => $id,
        error   => {
            code    => $code,
            message => $message,
            (defined $data ? (data => $data) : ()),
        },
    };
    $self->_send_message($response);
}

sub _handle_message {
    my ($self, $message) = @_;

    # Response to a pending request
    if (exists $message->{id} && (exists $message->{result} || exists $message->{error})) {
        my $id = $message->{id};
        if (exists $self->{_pending}{$id}) {
            $self->{_responses}{$id} = $message;
        }
        return;
    }

    # Notification (no id) or incoming request (method + id): queue for dispatch
    # on the polling thread via _process_incoming().
    push @{ $self->{_incoming} }, $message;
}

sub _dispatch_incoming {
    my ($self, $message) = @_;

    # Notification from server (method, no id)
    if (exists $message->{method} && !exists $message->{id}) {
        if ($self->{_notification_handler}) {
            eval {
                $self->{_notification_handler}->(
                    $message->{method},
                    $message->{params} // {},
                );
            };
            warn "Notification handler error: $@" if $@;
        }
        return;
    }

    # Incoming request from server (method + id)
    if (exists $message->{method} && exists $message->{id}) {
        my $method  = $message->{method};
        my $id      = $message->{id};
        my $params  = $message->{params} // {};
        my $handler = $self->{_request_handlers}{$method};

        if (!$handler) {
            $self->_send_error_response(
                $id, -32601,
                "Method not found: $method",
                undef,
            );
            return;
        }

        eval {
            my $result = $handler->($params);
            $result = {} unless defined $result;
            $self->_send_response($id, $result);
        };
        if ($@) {
            $self->_send_error_response($id, -32603, "$@", undef);
        }
        return;
    }
}

1;

__END__

=head1 SYNOPSIS

    use GitHub::Copilot::JsonRpcClient;
    use IPC::Open3;

    my $pid = open3(my $stdin, my $stdout, my $stderr,
                    'copilot', '--headless', '--stdio');

    my $client = GitHub::Copilot::JsonRpcClient->new(
        stdin  => $stdin,
        stdout => $stdout,
    );
    $client->start();

    my $result = $client->request('ping', { message => 'hello' });
    print "Got: $result->{message}\n";

    $client->stop();

=cut
