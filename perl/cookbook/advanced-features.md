# Advanced v2.0 Features in Perl

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in Perl, including per-session auth, SessionFs, commands, system prompts, skills, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, agent skills, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```perl
use GitHub::Copilot;

my $client = GitHub::Copilot->new();
$client->start();

my $session = $client->create_session(
    config => {
        github_token => 'ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    },
);

my $response = $session->send_and_wait(
    message => 'Who am I authenticated as?',
);
print $response->message, "\n";

$client->stop();
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```perl
my $session = $client->create_session(
    config => {
        session_idle_timeout_seconds => 600,  # 10-minute idle timeout
    },
);

# Session will be cleaned up automatically if idle for 10 minutes
my $response = $session->send_and_wait(
    message => 'This session expires after 10 minutes of inactivity.',
);
```

## 3. SessionFs (Session Filesystem)

Configure a filesystem provider with the 10 required I/O operations.

```perl
use GitHub::Copilot;
use File::Slurp;
use File::Copy;
use File::Path qw(make_path);

# Implement the 10 required I/O operations
my $fs_provider = {
    read_file => sub {
        my ($path) = @_;
        my $content = read_file($path);
        return { content => $content };
    },

    write_file => sub {
        my ($path, $content) = @_;
        write_file($path, $content);
        return { success => 1 };
    },

    delete_file => sub {
        my ($path) = @_;
        unlink($path);
        return { success => 1 };
    },

    list_directory => sub {
        my ($path) = @_;
        opendir(my $dh, $path) or return { error => "Cannot open: $!" };
        my @entries = grep { $_ ne '.' && $_ ne '..' } readdir($dh);
        closedir($dh);
        return { entries => \@entries };
    },

    create_directory => sub {
        my ($path) = @_;
        make_path($path);
        return { success => 1 };
    },

    stat => sub {
        my ($path) = @_;
        my @info = stat($path);
        return { size => $info[7], is_directory => (-d $path ? 1 : 0) };
    },

    rename => sub {
        my ($old_path, $new_path) = @_;
        rename($old_path, $new_path);
        return { success => 1 };
    },

    copy => sub {
        my ($src, $dst) = @_;
        File::Copy::copy($src, $dst);
        return { success => 1 };
    },

    exists => sub {
        my ($path) = @_;
        return { exists => (-e $path ? 1 : 0) };
    },

    watch => sub {
        my ($path, $callback) = @_;
        # Set up file watching (platform-specific)
        return { success => 1 };
    },
};

my $session = $client->create_session(
    config => {
        session_fs => {
            enabled   => 1,
            root_path => '/workspace/project',
            provider  => $fs_provider,
        },
    },
);

my $response = $session->send_and_wait(
    message => 'List the files in the workspace.',
);
print $response->message, "\n";
```

## 4. Commands and UI Elicitation

Register slash commands and handle elicitation requests from the agent.

```perl
use GitHub::Copilot;

my $session = $client->create_session(
    config => {
        commands => [
            { name => '/deploy', description => 'Deploy the current project' },
            { name => '/test',   description => 'Run the test suite' },
            { name => '/status', description => 'Show project status' },
        ],
        elicitation_handler => sub {
            my ($req) = @_;
            print "Agent asks: $req->{message}\n";
            if ($req->{type} eq 'confirmation') {
                return { confirmed => 1 };
            }
            return { text => 'user-provided-value' };
        },
    },
);

my $response = $session->send_and_wait(
    message => '/deploy to staging',
);
print $response->message, "\n";
```

## 5. System Prompt Customization

Use replace or customize modes with structured sections.

```perl
# Mode 1: Replace the entire system prompt
my $session1 = $client->create_session(
    config => {
        system_prompt => {
            mode    => 'replace',
            content => 'You are a Perl expert. Only discuss Perl topics.',
        },
    },
);

# Mode 2: Customize with structured sections
my $session2 = $client->create_session(
    config => {
        system_prompt => {
            mode     => 'customize',
            sections => [
                { id => 'role',
                  content => 'You are a senior Perl developer.' },
                { id => 'constraints',
                  content => 'Always use strict and warnings. Prefer modern Perl idioms.' },
                { id => 'output_format',
                  content => 'Include POD documentation in all module examples.' },
            ],
        },
    },
);
```

## 6. Per-Agent Skills

Configure skill directories and disable specific skills per agent.

```perl
my $session = $client->create_session(
    config => {
        skill_directories => [
            '/home/user/.copilot/skills',
            '/project/.copilot/skills',
        ],
        disabled_skills => ['web-search', 'code-execution'],
    },
);

my $response = $session->send_and_wait(
    message => 'Use the custom project skills to analyze the codebase.',
);
```

## 7. Per-Agent Tool Visibility

Hide specific tools from certain agents using excluded_tools.

```perl
# Create a read-only agent that cannot modify files
my $reviewer = $client->create_session(
    config => {
        system_prompt => {
            mode    => 'replace',
            content => 'You are a code reviewer. Analyze but do not modify.',
        },
        excluded_tools => [
            'write_file', 'delete_file', 'execute_command', 'create_directory',
        ],
    },
);

# Create a full-access agent
my $developer = $client->create_session(
    config => {
        system_prompt => {
            mode    => 'replace',
            content => 'You are a developer with full access.',
        },
    },
);
```

## 8. Runtime Request Headers

Attach custom headers to individual requests for tracing or auth.

```perl
my $response = $session->send_and_wait(
    message         => 'Summarize the latest deploy logs.',
    request_headers => {
        'X-Request-Id'  => 'req-abc-123',
        'X-Trace-Id'    => 'trace-xyz-789',
        'Authorization'  => 'Bearer custom-token-here',
    },
);
```

## 9. Model Capabilities Override

Override model capabilities for a session to control behavior.

```perl
my $session = $client->create_session(
    config => {
        model_capabilities => {
            streaming      => 1,
            tool_calling   => 1,
            vision         => 0,
            max_tokens     => 8192,
            context_window => 128000,
        },
    },
);
```

## 10. Config Discovery

Enable automatic discovery of project-level configuration files.

```perl
my $session = $client->create_session(
    config => {
        enable_config_discovery => 1,
    },
);

# The SDK will automatically discover and load:
#   .copilot/config.yml
#   .copilot/prompts/*.md
#   .copilot/skills/
# from the project root and parent directories.
my $response = $session->send_and_wait(
    message => 'What project configuration did you discover?',
);
```

## 11. Sub-Agent Streaming Events

Receive streaming events from sub-agents during orchestration.

```perl
use GitHub::Copilot;

my $session = $client->create_session(
    config => {
        include_sub_agent_streaming_events => 1,
    },
);

$session->on_event(sub {
    my ($event) = @_;
    if ($event->{type} eq 'assistant.message_delta') {
        print "[delta] $event->{content}";
    }
    elsif ($event->{type} eq 'sub_agent.message_delta') {
        print "[sub-agent:$event->{agent_id}] $event->{content}";
    }
});

$session->send_message(
    message => 'Coordinate the planner and coder agents.',
);
```

## 12. Session Metadata

Retrieve metadata about the current session.

```perl
my $metadata = $session->get_session_metadata();
print "Session ID:  $metadata->{session_id}\n";
print "Created at:  $metadata->{created_at}\n";
print "Turn count:  $metadata->{turn_count}\n";
print "Model:       $metadata->{model}\n";
print "Token usage: $metadata->{total_tokens}\n";
```

## 13. MCP Server Configuration

Configure Model Context Protocol servers using stdio or HTTP transports.

```perl
my $session = $client->create_session(
    config => {
        mcp_servers => [
            # Stdio transport: launch a local MCP server process
            {
                name      => 'filesystem',
                transport => 'stdio',
                command   => 'npx',
                args      => ['-y', '@modelcontextprotocol/server-filesystem',
                              '/workspace'],
            },
            # HTTP transport: connect to a remote MCP server
            {
                name      => 'remote-db',
                transport => 'http',
                url       => 'https://mcp.example.com/db',
                headers   => { Authorization => 'Bearer token123' },
            },
        ],
    },
);

my $response = $session->send_and_wait(
    message => 'List the files in the workspace using the MCP server.',
);
```

## 14. Image Generation

Configure the response format to request image generation.

```perl
use GitHub::Copilot;
use File::Slurp qw(write_file);

my $session = $client->create_session(
    config => {
        response_format => {
            type          => 'image_generation',
            image_size    => '1024x1024',
            image_quality => 'high',
        },
    },
);

my $response = $session->send_and_wait(
    message => 'Generate an image of a futuristic cityscape at sunset.',
);

# Save the generated image
if ($response->image_data) {
    write_file('cityscape.png', { binmode => ':raw' }, $response->image_data);
    print "Image saved to cityscape.png\n";
}
```

## Best Practices

- **Scope tokens tightly**: Use per-session `github_token` with minimal scopes for multi-tenant apps.
- **Use hash-based config**: Perl's hash references provide a natural fit for nested configuration structures.
- **Combine features**: Per-agent skills, tool visibility, and system prompts work together to create specialized agents.
- **Use config discovery in dev**: Enable `enable_config_discovery` during development so project-level configs are picked up automatically.
- **Use callbacks for events**: Register event handlers with `on_event()` for reactive streaming processing.
- **Set idle timeouts in server apps**: Use `session_idle_timeout_seconds` to prevent resource leaks from abandoned sessions.
- **Always use strict and warnings**: Follow Perl best practices in all SDK usage code.
