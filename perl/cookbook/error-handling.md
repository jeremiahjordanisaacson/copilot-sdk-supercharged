# Error Handling in Perl

Patterns for handling errors when using the GitHub Copilot SDK in Perl.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully using Perl's eval/die and Try::Tiny patterns.

## Basic eval/die Pattern

```perl
use strict;
use warnings;
use GitHub::Copilot;

my $client;

eval {
    $client = GitHub::Copilot->new();
    $client->start();

    my $session = $client->create_session();
    my $response = $session->send_and_wait(message => "Hello from Perl!");

    print "Response: $response->{message}\n";
    1;
} or do {
    my $err = $@ || "Unknown error";
    warn "Error: $err\n";
};

if ($client) {
    eval { $client->stop() };
}
```

## Using Try::Tiny

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

sub run_conversation {
    my $client;

    try {
        $client = GitHub::Copilot->new();
        $client->start();

        my $session = $client->create_session();
        my $response = $session->send_and_wait(
            message => "Explain regular expressions in Perl"
        );

        print "Response: $response->{message}\n";
    }
    catch {
        warn "Caught error: $_\n";
    }
    finally {
        if ($client) {
            try { $client->stop() }
            catch { warn "Cleanup error: $_\n" };
        }
    };
}

run_conversation();
```

## Retry with Exponential Backoff

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

sub connect_with_retry {
    my (%args) = @_;
    my $max_retries = $args{max_retries} // 3;

    for my $attempt (1 .. $max_retries) {
        my $client;

        try {
            $client = GitHub::Copilot->new();
            $client->start();
            print "Connected on attempt $attempt\n";
            return $client;
        }
        catch {
            warn "Attempt $attempt failed: $_\n";
            if ($client) {
                try { $client->stop() } catch {};
            }

            if ($attempt < $max_retries) {
                my $delay = $attempt * 2;
                warn "Retrying in ${delay}s...\n";
                sleep($delay);
            }
        };
    }

    die "Failed to connect after $max_retries attempts\n";
}
```

## Custom Exception Classes

```perl
use strict;
use warnings;

package CopilotError {
    sub new {
        my ($class, %args) = @_;
        return bless {
            message => $args{message} // "Unknown error",
            type    => $args{type}    // "generic",
        }, $class;
    }
    sub message { return $_[0]->{message} }
    sub type    { return $_[0]->{type} }
    sub stringify { return "$_[0]->{type}: $_[0]->{message}" }
    use overload '""' => \&stringify;
}

package CopilotConnectionError {
    our @ISA = ('CopilotError');
    sub new {
        my ($class, %args) = @_;
        $args{type} = "connection";
        return $class->SUPER::new(%args);
    }
}

package CopilotTimeoutError {
    our @ISA = ('CopilotError');
    sub new {
        my ($class, %args) = @_;
        $args{type} = "timeout";
        return $class->SUPER::new(%args);
    }
}

# Usage
use Try::Tiny;

try {
    die CopilotConnectionError->new(message => "Server unreachable");
}
catch {
    if (ref $_ && $_->isa('CopilotConnectionError')) {
        warn "Connection problem: " . $_->message . "\n";
    }
    elsif (ref $_ && $_->isa('CopilotTimeoutError')) {
        warn "Timeout: " . $_->message . "\n";
    }
    else {
        warn "Unexpected error: $_\n";
    }
};
```

## Scope Guard for Cleanup

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

sub with_copilot {
    my ($callback) = @_;
    my $client;

    try {
        $client = GitHub::Copilot->new();
        $client->start();
        $callback->($client);
    }
    catch {
        warn "Error: $_\n";
    }
    finally {
        if ($client) {
            try { $client->stop() }
            catch { warn "Cleanup error: $_\n" };
        }
    };
}

# Usage
with_copilot(sub {
    my ($client) = @_;
    my $session = $client->create_session();
    my $response = $session->send_and_wait(
        message => "What is CPAN?"
    );
    print $response->{message}, "\n";
});
```

## Best Practices

- **Prefer Try::Tiny over raw eval**: It handles edge cases with `$@` that raw eval misses.
- **Always clean up in finally blocks**: Ensure `$client->stop()` runs regardless of errors.
- **Wrap cleanup in its own try/catch**: Prevent cleanup errors from masking the original failure.
- **Use custom exception objects**: Object-based exceptions allow `isa()` checks for specific error types.
- **Check `ref $_` before calling methods**: Verify the error is an object before using `->isa()`.
- **Use guard functions**: Wrap the client lifecycle in a `with_copilot` helper to avoid repetitive cleanup code.
