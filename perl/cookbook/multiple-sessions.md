# Multiple Sessions in Perl

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in Perl.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions

```perl
use strict;
use warnings;
use GitHub::Copilot;

my $client = GitHub::Copilot->new();
$client->start();

# Create separate sessions for different tasks
my $code_session = $client->create_session(
    system_prompt => "You are a Perl expert."
);
my $docs_session = $client->create_session(
    system_prompt => "You are a technical writer."
);

# Each session maintains its own conversation history
my $code_reply = $code_session->send_and_wait(
    message => "Explain references in Perl"
);
print "Code: $code_reply->{message}\n";

my $docs_reply = $docs_session->send_and_wait(
    message => "Write a POD template"
);
print "Docs: $docs_reply->{message}\n";

$client->stop();
```

## Session Manager Class

```perl
use strict;
use warnings;
use GitHub::Copilot;

package SessionManager {
    sub new {
        my ($class, %args) = @_;
        return bless {
            client   => $args{client},
            sessions => {},
        }, $class;
    }

    sub get_or_create {
        my ($self, $name, %opts) = @_;

        if (exists $self->{sessions}{$name}) {
            return $self->{sessions}{$name};
        }

        my $session = $self->{client}->create_session(
            system_prompt => $opts{system_prompt} // "",
        );
        $self->{sessions}{$name} = $session;
        return $session;
    }

    sub remove {
        my ($self, $name) = @_;
        delete $self->{sessions}{$name};
    }

    sub count {
        my ($self) = @_;
        return scalar keys %{$self->{sessions}};
    }

    sub names {
        my ($self) = @_;
        return keys %{$self->{sessions}};
    }
}

# Usage
my $client = GitHub::Copilot->new();
$client->start();

my $manager = SessionManager->new(client => $client);

my $frontend = $manager->get_or_create(
    "frontend",
    system_prompt => "You are a JavaScript expert."
);
my $backend = $manager->get_or_create(
    "backend",
    system_prompt => "You are a database expert."
);

my $r1 = $frontend->send_and_wait(message => "Explain closures");
print "Frontend: $r1->{message}\n";

my $r2 = $backend->send_and_wait(message => "Explain SQL joins");
print "Backend: $r2->{message}\n";

print "Active sessions: ", $manager->count(), "\n";

$client->stop();
```

## Processing Multiple Tasks

```perl
use strict;
use warnings;
use GitHub::Copilot;
use Try::Tiny;

sub run_tasks {
    my ($client, @tasks) = @_;
    my @results;

    for my $task (@tasks) {
        my $result = try {
            my $session = $client->create_session(
                system_prompt => $task->{persona},
            );
            my $response = $session->send_and_wait(
                message => $task->{prompt},
            );
            {
                name   => $task->{name},
                result => $response->{message},
                error  => undef,
            };
        }
        catch {
            {
                name   => $task->{name},
                result => undef,
                error  => $_,
            };
        };
        push @results, $result;
    }

    return @results;
}

# Usage
my $client = GitHub::Copilot->new();
$client->start();

my @tasks = (
    { name => "review", prompt => "Review this subroutine",  persona => "You review Perl code." },
    { name => "docs",   prompt => "Generate POD docs",       persona => "You write Perl docs." },
    { name => "tests",  prompt => "Write Test::More tests",  persona => "You write Perl tests." },
);

my @results = run_tasks($client, @tasks);

for my $r (@results) {
    if (!$r->{error}) {
        printf "[%s] %s\n", $r->{name}, $r->{result};
    } else {
        printf "[%s] ERROR: %s\n", $r->{name}, $r->{error};
    }
}

$client->stop();
```

## Best Practices

- **Reuse a single client**: Create one `GitHub::Copilot` instance and share it across all sessions.
- **Use a manager object**: Encapsulate session creation and lookup behind a clean interface.
- **Use hashes for named sessions**: Perl hashes provide fast lookup by session name.
- **Wrap each task in try/catch**: Prevent one failed session from stopping the rest.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Clean up finished sessions**: Call `remove()` on the manager to free sessions that are no longer needed.
