# Tools and Skills in Perl

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in Perl.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```perl
use strict;
use warnings;
use GitHub::Copilot;
use POSIX qw(strftime);

my $client = GitHub::Copilot->new();

$client->define_tool(
    "get_current_time",
    "Returns the current date and time",
    sub {
        my ($context) = @_;
        my $now = strftime("%Y-%m-%d %H:%M:%S", localtime);
        return { content => $now };
    }
);
```

## Defining a Tool with Parameters

```perl
use strict;
use warnings;
use GitHub::Copilot;
use File::Slurp qw(read_file);

$client->define_tool(
    "read_file",
    "Reads the contents of a file given its path",
    sub {
        my ($context) = @_;
        my $path = $context->get_param("path");

        unless (defined $path && length $path) {
            return { error => "Missing required parameter: path" };
        }

        unless (-f $path) {
            return { error => "File not found: $path" };
        }

        my $content = eval { read_file($path) };
        if ($@) {
            return { error => "Could not read file: $path" };
        }

        return { content => $content };
    }
);
```

## Registering Tools and Using Them

```perl
use strict;
use warnings;
use GitHub::Copilot;
use POSIX qw(strftime);

my $client = GitHub::Copilot->new();

# Register tools
$client->define_tool("get_current_time", "Returns the current date and time", sub {
    return { content => strftime("%Y-%m-%d %H:%M:%S", localtime) };
});

$client->define_tool("reverse_string", "Reverses a string", sub {
    my ($ctx) = @_;
    my $input = $ctx->get_param("text") // "";
    return { content => scalar reverse $input };
});

$client->start();

my $session = $client->create_session(
    system_prompt => "You have access to time and string tools."
);

my $response = $session->send_and_wait(
    message => "What time is it? Also reverse the word 'hello'."
);
print $response->{message}, "\n";

$client->stop();
```

## Defining Multiple Related Tools with Shared State (Skill Pattern)

```perl
use strict;
use warnings;
use GitHub::Copilot;

sub create_kv_skill {
    my $store = {};

    return (
        {
            name        => "kv_get",
            description => "Get a value by key",
            handler     => sub {
                my ($ctx) = @_;
                my $key = $ctx->get_param("key") // "";
                if (exists $store->{$key}) {
                    return { content => $store->{$key} };
                }
                return { content => "Key not found: $key" };
            },
        },
        {
            name        => "kv_set",
            description => "Set a key-value pair",
            handler     => sub {
                my ($ctx) = @_;
                my $key   = $ctx->get_param("key")   // "";
                my $value = $ctx->get_param("value") // "";
                $store->{$key} = $value;
                return { content => "Stored: $key" };
            },
        },
        {
            name        => "kv_list",
            description => "List all keys",
            handler     => sub {
                my @keys = keys %$store;
                if (@keys == 0) {
                    return { content => "Store is empty" };
                }
                return { content => join("\n", @keys) };
            },
        },
    );
}

# Register the skill
my $client = GitHub::Copilot->new();

my @kv_tools = create_kv_skill();
for my $tool (@kv_tools) {
    $client->define_tool(
        $tool->{name},
        $tool->{description},
        $tool->{handler},
    );
}

$client->start();

my $session = $client->create_session();
my $response = $session->send_and_wait(
    message => "Store my name as 'Alice' and then retrieve it"
);
print $response->{message}, "\n";

$client->stop();
```

## Sub-Agent Orchestration

```perl
use strict;
use warnings;
use GitHub::Copilot;

sub orchestrate {
    my ($client) = @_;

    # Create specialized sessions as sub-agents
    my $planner = $client->create_session(
        system_prompt => "You are a planner. Break tasks into steps."
    );
    my $coder = $client->create_session(
        system_prompt => "You are a Perl developer."
    );
    my $reviewer = $client->create_session(
        system_prompt => "You review Perl code for bugs."
    );

    # Step 1: Plan
    my $plan = $planner->send_and_wait(
        message => "Plan a Perl module for HTTP caching"
    );
    print "Plan: $plan->{message}\n\n";

    # Step 2: Code
    my $code = $coder->send_and_wait(
        message => "Implement this plan:\n$plan->{message}"
    );
    print "Code: $code->{message}\n\n";

    # Step 3: Review
    my $review = $reviewer->send_and_wait(
        message => "Review this code:\n$code->{message}"
    );
    print "Review: $review->{message}\n\n";
}

# Usage
my $client = GitHub::Copilot->new();
$client->start();
orchestrate($client);
$client->stop();
```

## Best Practices

- **Use closures for shared tool state**: Perl closures capture lexical variables, making them ideal for skill patterns.
- **Return hash references from handlers**: Use `{ content => ... }` or `{ error => ... }` consistently.
- **Use factory functions for skills**: Create a sub that returns a list of tool definitions sharing state.
- **Validate parameters with defined checks**: Use `defined` and `length` to verify parameters.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.
- **Chain agent outputs with string interpolation**: Pass one agent's output to the next using `"...$var..."`.
