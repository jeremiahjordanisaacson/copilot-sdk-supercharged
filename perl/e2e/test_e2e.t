#!/usr/bin/env perl
use strict;
use warnings;
use Test::More tests => 3;
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

done_testing();
