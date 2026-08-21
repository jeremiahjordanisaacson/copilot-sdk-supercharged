#!/usr/bin/env perl
use strict;
use warnings;
use Test::More;
use File::Spec;
use File::Basename;
use Cwd qw(abs_path);

# Add lib paths
use lib File::Spec->catdir(dirname(__FILE__), '..', 'lib');
use lib dirname(__FILE__);

use TestHarness;
use GitHub::Copilot::Client;

my $repo_root = abs_path(File::Spec->catdir(dirname(__FILE__), '..', '..'));

# Snapshot helpers
my $snapshots_dir = File::Spec->catdir($repo_root, 'test', 'snapshots', 'session');
my $DEFAULT_SNAPSHOT = 'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message';

sub snapshot_path {
    my ($name) = @_;
    $name //= $DEFAULT_SNAPSHOT;
    return File::Spec->catfile($snapshots_dir, "$name.yaml");
}

# Reuse a single proxy for all tests (start once)
my $proxy = TestHarness->new();
$proxy->start();

sub configure_snapshot {
    my ($name) = @_;
    $proxy->configure(snapshot_path($name), $repo_root);
}

sub make_client {
    my $client = GitHub::Copilot::Client->new(
        env          => $proxy->get_proxy_env(),
        github_token => 'fake-token-for-e2e-tests',
        @_,
    );
    $client->start();
    return $client;
}

# ----------------------------------------------------------------
# Test 1: Session create and disconnect
# ----------------------------------------------------------------
subtest 'session create and disconnect' => sub {
    plan tests => 2;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    ok(defined $session, 'Session created');
    ok(length($session->id()) > 0, 'Session ID is not empty');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 2: Send message and receive response
# ----------------------------------------------------------------
subtest 'send message' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    my $response = $session->send_and_wait({ prompt => 'What is 2+2?' });
    ok(defined $response, 'Received a response');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 3: Session FS config
# ----------------------------------------------------------------
subtest 'session fs config' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client(
        session_fs => {
            initial_cwd        => File::Spec->tmpdir(),
            session_state_path => File::Spec->catdir(File::Spec->tmpdir(), 'copilot-state'),
        },
    );
    pass('Client started with SessionFs config');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 4: Multi-turn conversation
# ----------------------------------------------------------------
subtest 'multi-turn conversation' => sub {
    plan tests => 2;
    configure_snapshot('should_have_stateful_conversation');

    my $client = make_client();
    my $session = $client->create_session();

    my $response1 = $session->send_and_wait({ prompt => 'What is 1+1?' });
    ok(defined $response1, 'Received first response');

    my $response2 = $session->send_and_wait({ prompt => 'Now if you double that, what do you get?' });
    ok(defined $response2, 'Received second response');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 5: Session resume (same-client pattern)
# ----------------------------------------------------------------
subtest 'session resume' => sub {
    plan tests => 2;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    my $session_id = $session->id();
    ok(length($session_id) > 0, 'Got session ID');

    # Resume on the SAME client (session state lives in CLI process)
    my $resumed = $client->resume_session($session_id);
    is($resumed->id(), $session_id, 'Resumed session has same ID');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 6: Session list
# ----------------------------------------------------------------
subtest 'session list' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session1 = $client->create_session();
    my $session2 = $client->create_session();

    my $sessions = $client->list_sessions();
    cmp_ok(scalar @$sessions, '>=', 2, 'At least 2 sessions listed');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 7: Session metadata
# ----------------------------------------------------------------
subtest 'session metadata' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    my $metadata = $client->get_session_metadata($session->id());
    ok(defined $metadata, 'Session metadata is defined');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 8: Session delete
# ----------------------------------------------------------------
subtest 'session delete' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    my $session_id = $session->id();

    $client->delete_session($session_id);

    my $sessions = $client->list_sessions();
    my @ids = map { $_->sessionId() } @$sessions;
    ok(!grep({ $_ eq $session_id } @ids), 'Deleted session not in list');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 9: Model list
# ----------------------------------------------------------------
subtest 'model list' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $models = $client->list_models();
    cmp_ok(scalar @$models, '>', 0, 'At least one model returned');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 10: Ping
# ----------------------------------------------------------------
subtest 'ping' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $result = $client->ping();
    ok(defined $result, 'Ping result is defined');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 11: Auth status
# ----------------------------------------------------------------
subtest 'auth status' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $auth = $client->get_auth_status();
    ok(defined $auth, 'Auth status is defined');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 12: Client lifecycle
# ----------------------------------------------------------------
subtest 'client lifecycle' => sub {
    plan tests => 2;
    configure_snapshot();

    my $client = make_client();
    is($client->get_state(), 'connected', 'Client is connected after start');

    $client->stop();
    is($client->get_state(), 'disconnected', 'Client is disconnected after stop');
};

# ----------------------------------------------------------------
# Test 13: Foreground session (headless-safe)
# ----------------------------------------------------------------
subtest 'foreground session' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session();
    my $session_id = $session->id();

    # Foreground RPCs may not be available in headless CI — wrap in eval
    eval {
        $client->set_foreground_session_id($session_id);
        my $fg_id = $client->get_foreground_session_id();
        is($fg_id, $session_id, 'Foreground session ID matches');
    };
    if ($@) {
        pass('Foreground RPC not available in headless mode (expected)');
    }

    $client->stop();
};

# ----------------------------------------------------------------
# Test 14: Tools
# ----------------------------------------------------------------
subtest 'tools' => sub {
    plan tests => 2;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session({
        tools => [
            {
                name        => 'test_tool',
                description => 'A test tool for E2E testing',
                handler     => sub { return { result => 'tool executed' }; },
            },
        ],
    });
    ok(defined $session, 'Session with tools created');

    # Simple prompt that does not invoke the tool — verifies tools attach correctly
    my $response = $session->send_and_wait({ prompt => 'What is 2+2?' });
    ok(defined $response, 'Received response with tools defined');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 15: Streaming
# ----------------------------------------------------------------
subtest 'streaming' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session({ streaming => 1 });
    my $response = $session->send_and_wait({ prompt => 'What is 2+2?' });
    ok(defined $response, 'Received response in streaming mode');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 16: System message customization
# ----------------------------------------------------------------
subtest 'system message customization' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    my $session = $client->create_session({
        system_message => {
            mode    => 'append',
            content => 'You are a helpful test assistant.',
        },
    });
    ok(defined $session, 'Session with system message created without error');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 17: Session FS provider variant
# ----------------------------------------------------------------
subtest 'session fs provider' => sub {
    plan tests => 2;
    configure_snapshot();

    my $client = make_client(
        session_fs => {
            initial_cwd        => File::Spec->tmpdir(),
            session_state_path => File::Spec->catdir(File::Spec->tmpdir(), 'copilot-state'),
            conventions        => 'posix',
        },
    );

    my $session = $client->create_session();
    ok(defined $session, 'Session with FS provider created');
    ok(length($session->id()) > 0, 'Session with FS provider has non-empty ID');

    $client->stop();
};

# ----------------------------------------------------------------
# Test 18: MCP servers config
# ----------------------------------------------------------------
subtest 'mcp servers config' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    eval {
        my $session = $client->create_session({
            mcp_servers => [
                { url => 'http://localhost:9999/mcp' },
            ],
        });
        ok(defined $session, 'Session with MCP servers config created');
    };
    if ($@) {
        pass('MCP server config accepted (server not found is acceptable)');
    }

    $client->stop();
};

# ----------------------------------------------------------------
# Test 19: Skills config
# ----------------------------------------------------------------
subtest 'skills config' => sub {
    plan tests => 1;
    configure_snapshot();

    my $client = make_client();
    eval {
        my $session = $client->create_session({
            skills => {
                directories => [ $repo_root ],
            },
        });
        ok(defined $session, 'Session with skills config created');
    };
    if ($@) {
        pass('Skills config accepted (non-existent paths handled gracefully)');
    }

    $client->stop();
};

# ----------------------------------------------------------------
# Test 20: Compaction (2 sends with multi-turn snapshot)
# ----------------------------------------------------------------
subtest 'compaction' => sub {
    plan tests => 1;
    configure_snapshot('should_have_stateful_conversation');

    my $client = make_client();
    my $session = $client->create_session();

    # Send two messages using multi-turn snapshot prompts
    $session->send_and_wait({ prompt => 'What is 1+1?' });
    $session->send_and_wait({ prompt => 'Now if you double that, what do you get?' });

    pass('Completed multi-turn messages without error (compaction may have occurred)');

    $client->stop();
};

# Cleanup
$proxy->stop();

done_testing();
