# Persisting Sessions in Perl

Patterns for saving and resuming Copilot sessions across application restarts in Perl.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving and Loading Session IDs

```perl
use strict;
use warnings;
use File::Slurp qw(read_file write_file);

my $SESSION_FILE = "session_state.txt";

sub save_session_id {
    my ($session_id) = @_;
    eval { write_file($SESSION_FILE, $session_id) };
    if ($@) {
        warn "Failed to save session: $@\n";
        return 0;
    }
    return 1;
}

sub load_session_id {
    return undef unless -f $SESSION_FILE;

    my $id = eval { read_file($SESSION_FILE) };
    if ($@) {
        warn "Failed to read session: $@\n";
        return undef;
    }

    chomp $id;
    return length($id) ? $id : undef;
}
```

## Resuming a Session

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

sub get_or_create_session {
    my ($client) = @_;

    my $saved_id = load_session_id();

    if (defined $saved_id) {
        my $session = try {
            $client->create_session(session_id => $saved_id);
        }
        catch {
            warn "Could not resume session: $_\n";
            undef;
        };

        if ($session) {
            print "Resumed session: $saved_id\n";
            return $session;
        }
    }

    # Create a new session and save the ID
    my $session = $client->create_session();
    save_session_id($session->{id});
    print "Created new session: $session->{id}\n";
    return $session;
}
```

## Full Example: Persistent Chat

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

my $client = GitHub::Copilot->new();
$client->start();

my $session = get_or_create_session($client);

print "Chat (type 'quit' to exit):\n";

while (1) {
    print "> ";
    my $input = <STDIN>;
    last unless defined $input;
    chomp $input;
    last if $input eq "quit";

    try {
        my $response = $session->send_and_wait(message => $input);
        print "Assistant: $response->{message}\n";
    }
    catch {
        warn "Error: $_\n";
    };
}

$client->stop();
print "Session saved. You can resume later.\n";
```

## Storing Multiple Sessions with JSON

```perl
use strict;
use warnings;
use JSON;
use GitHub::Copilot;
use Try::Tiny;

my $SESSIONS_FILE = "sessions.json";

sub load_all_sessions {
    return {} unless -f $SESSIONS_FILE;

    my $content = eval { read_file($SESSIONS_FILE) };
    return {} if $@ || !$content;

    return eval { decode_json($content) } // {};
}

sub save_all_sessions {
    my ($sessions) = @_;
    write_file($SESSIONS_FILE, encode_json($sessions));
}

sub get_named_session {
    my ($client, $name, %opts) = @_;
    my $sessions = load_all_sessions();
    my $saved_id = $sessions->{$name};

    if (defined $saved_id) {
        my $session = try {
            $client->create_session(session_id => $saved_id);
        }
        catch { undef };

        if ($session) {
            print "Resumed $name session\n";
            return $session;
        }
    }

    my $session = $client->create_session(
        system_prompt => $opts{system_prompt} // "",
    );
    $sessions->{$name} = $session->{id};
    save_all_sessions($sessions);
    print "Created new $name session\n";
    return $session;
}

# Usage
my $client = GitHub::Copilot->new();
$client->start();

my $code_session = get_named_session($client, "code",
    system_prompt => "You are a Perl expert.");
my $docs_session = get_named_session($client, "docs",
    system_prompt => "You write Perl documentation.");

$client->stop();
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions with try/catch**: If resuming fails (session expired), create a fresh session.
- **Use File::Slurp for convenience**: Simplifies reading and writing files to single function calls.
- **Use JSON for multiple sessions**: Store a hash of name-to-ID mappings for multi-session persistence.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.
