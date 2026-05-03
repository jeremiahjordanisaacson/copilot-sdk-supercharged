#!/usr/bin/env perl
use strict;
use warnings;
use Test::More tests => 20;
use File::Spec;
use File::Basename;
use Cwd qw(abs_path);

# Add lib paths
use lib File::Spec->catdir(dirname(__FILE__), '..', 'lib');
use lib dirname(__FILE__);

use TestHarness;
use GitHub::Copilot::Client;

my $repo_root = abs_path(File::Spec->catdir(dirname(__FILE__), '..', '..'));

subtest 'session create and disconnect' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    ok(defined $session, 'Session created');
    ok(length($session->id()) > 0, 'Session ID is not empty');

    $client->stop();
    $proxy->stop();
};

subtest 'send message' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $response = $session->send_and_wait({ content => 'Hello' });
    ok(defined $response, 'Received a response');

    $client->stop();
    $proxy->stop();
};

subtest 'session fs config' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
        session_fs => {
            initial_cwd        => File::Spec->tmpdir(),
            session_state_path => File::Spec->catdir(File::Spec->tmpdir(), 'copilot-state'),
        },
    );
    # Starting should internally call sessionFs.setProvider
    $client->start();
    pass('Client started with SessionFs config');

    $client->stop();
    $proxy->stop();
};

subtest 'multi-turn conversation' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $response1 = $session->send_and_wait({ content => 'What is 1+1?' });
    ok(defined $response1, 'Received first response');

    my $response2 = $session->send_and_wait({ content => 'And what is 2+2?' });
    ok(defined $response2, 'Received second response');

    $client->stop();
    $proxy->stop();
};

subtest 'session resume' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $session_id = $session->id();
    ok(length($session_id) > 0, 'Got session ID');

    $client->stop();

    my $client2 = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client2->start();

    my $resumed = $client2->create_session({ session_id => $session_id });
    is($resumed->id(), $session_id, 'Resumed session has same ID');

    $client2->stop();
    $proxy->stop();
};

subtest 'session list' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session1 = $client->create_session();
    my $session2 = $client->create_session();

    my @sessions = $client->list_sessions();
    cmp_ok(scalar @sessions, '>=', 2, 'At least 2 sessions listed');

    $client->stop();
    $proxy->stop();
};

subtest 'session metadata' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $metadata = $client->get_session_metadata($session->id());
    ok(defined $metadata, 'Session metadata is defined');

    $client->stop();
    $proxy->stop();
};

subtest 'session delete' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $session_id = $session->id();

    $client->delete_session($session_id);

    my @sessions = $client->list_sessions();
    my @ids = map { $_->id() } @sessions;
    ok(!grep({ $_ eq $session_id } @ids), 'Deleted session not in list');

    $client->stop();
    $proxy->stop();
};

subtest 'model list' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my @models = $client->list_models();
    cmp_ok(scalar @models, '>', 0, 'At least one model returned');

    $client->stop();
    $proxy->stop();
};

subtest 'ping' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $result = $client->ping();
    ok(defined $result, 'Ping result is defined');

    $client->stop();
    $proxy->stop();
};

subtest 'auth status' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $auth = $client->get_auth_status();
    ok(defined $auth, 'Auth status is defined');

    $client->stop();
    $proxy->stop();
};

subtest 'client lifecycle' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();
    is($client->state(), 'connected', 'Client is connected after start');

    $client->stop();
    is($client->state(), 'disconnected', 'Client is disconnected after stop');

    $proxy->stop();
};

subtest 'foreground session' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();
    my $session_id = $session->id();

    $client->set_foreground_session_id($session_id);
    my $fg_id = $client->get_foreground_session_id();
    is($fg_id, $session_id, 'Foreground session ID matches');

    $client->stop();
    $proxy->stop();
};

subtest 'tools' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

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

    my $response = $session->send_and_wait({ content => 'Use the test_tool' });
    ok(defined $response, 'Received response with tools defined');

    $client->stop();
    $proxy->stop();
};

subtest 'streaming' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session({ streaming => 1 });
    my $response = $session->send_and_wait({ content => 'Hello' });
    ok(defined $response, 'Received response in streaming mode');

    $client->stop();
    $proxy->stop();
};

subtest 'system message customization' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session({
        system_message => {
            mode    => 'append',
            content => 'You are a helpful test assistant.',
        },
    });
    ok(defined $session, 'Session with system message created without error');

    $client->stop();
    $proxy->stop();
};

subtest 'session fs provider' => sub {
    plan tests => 2;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
        session_fs => {
            initial_cwd        => File::Spec->tmpdir(),
            session_state_path => File::Spec->catdir(File::Spec->tmpdir(), 'copilot-state'),
            conventions        => 'posix',
        },
    );
    $client->start();

    my $session = $client->create_session();
    ok(defined $session, 'Session with FS provider created');
    ok(length($session->id()) > 0, 'Session with FS provider has non-empty ID');

    $client->stop();
    $proxy->stop();
};

subtest 'mcp servers config' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session({
        mcp_servers => [
            { url => 'http://localhost:9999/mcp' },
        ],
    });
    ok(defined $session, 'Session with MCP servers config created');

    $client->stop();
    $proxy->stop();
};

subtest 'skills config' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session({
        skills => {
            directories => [ $repo_root ],
        },
    });
    ok(defined $session, 'Session with skills config created');

    $client->stop();
    $proxy->stop();
};

subtest 'compaction' => sub {
    plan tests => 1;

    my $proxy = TestHarness->new();
    $proxy->start();
    $proxy->configure('test/snapshots/basic.yaml', $repo_root);

    my $client = GitHub::Copilot::Client->new(
        cli_url    => $proxy->url(),
        auto_start => 0,
    );
    $client->start();

    my $session = $client->create_session();

    # Send multiple messages to trigger compaction events
    for my $i (1..5) {
        $session->send_and_wait({ content => "Message number $i" });
    }

    pass('Completed multiple messages without error (compaction may have occurred)');

    $client->stop();
    $proxy->stop();
};

done_testing();
