package TestHarness;

use strict;
use warnings;
use File::Spec;
use File::Basename;
use IPC::Open3;
use HTTP::Tiny;
use JSON::PP;
use POSIX qw(:sys_wait_h);
use Cwd qw(abs_path);

sub new {
    my ($class) = @_;
    return bless {
        process_pid => undef,
        proxy_url   => undef,
        stdout_fh   => undef,
    }, $class;
}

sub start {
    my ($self) = @_;
    return $self->{proxy_url} if $self->{proxy_url};

    # Path to harness server (from e2e dir, up 3 levels to repo root)
    my $e2e_dir = dirname(__FILE__);
    my $harness_dir = File::Spec->catdir($e2e_dir, '..', '..', 'test', 'harness');
    $harness_dir = abs_path($harness_dir);
    my $server_path = File::Spec->catfile($harness_dir, 'server.ts');

    # Spawn the proxy
    my $pid = open3(my $in, my $out, my $err,
        'npx', 'tsx', $server_path);

    # Read first line for URL
    my $line = <$out>;
    chomp($line) if defined $line;

    unless ($line && $line =~ /Listening: (http:\/\/\S+)/) {
        kill 'TERM', $pid;
        die "Failed to parse proxy URL from: $line";
    }

    $self->{proxy_url}   = $1;
    $self->{process_pid} = $pid;
    $self->{stdout_fh}   = $out;

    $ENV{COPILOT_API_URL} = $self->{proxy_url};
    return $self->{proxy_url};
}

sub stop {
    my ($self) = @_;
    if ($self->{proxy_url}) {
        eval {
            HTTP::Tiny->new->post("$self->{proxy_url}/stop");
        };
    }
    if ($self->{process_pid}) {
        waitpid($self->{process_pid}, 0);
        $self->{process_pid} = undef;
    }
    $self->{proxy_url} = undef;
}

sub configure {
    my ($self, $file_path, $work_dir) = @_;
    die "Proxy not started" unless $self->{proxy_url};

    my $http = HTTP::Tiny->new;
    my $body = encode_json({ filePath => $file_path, workDir => $work_dir });
    my $resp = $http->post("$self->{proxy_url}/config", {
        content => $body,
        headers => { 'Content-Type' => 'application/json' },
    });
    die "Config failed: $resp->{status}" unless $resp->{success};
}

sub url {
    my ($self) = @_;
    return $self->{proxy_url};
}

sub DESTROY {
    my ($self) = @_;
    $self->stop();
}

1;
