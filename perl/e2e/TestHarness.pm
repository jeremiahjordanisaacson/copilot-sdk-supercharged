package TestHarness;

use strict;
use warnings;
use File::Spec;
use File::Basename;
use IPC::Open3;
use HTTP::Tiny;
use IO::Select;
use JSON::PP;
use POSIX qw(:sys_wait_h);
use Cwd qw(abs_path);
use Symbol qw(gensym);

sub new {
    my ($class) = @_;
    return bless {
        process_pid      => undef,
        proxy_url        => undef,
        connect_proxy_url => undef,
        ca_file_path     => undef,
        stdout_fh        => undef,
    }, $class;
}

sub start {
    my ($self) = @_;
    return $self->{proxy_url} if $self->{proxy_url};

    my $e2e_dir = dirname(__FILE__);
    my $harness_dir = File::Spec->catdir($e2e_dir, '..', '..', 'test', 'harness');
    $harness_dir = abs_path($harness_dir);
    my $server_path = File::Spec->catfile($harness_dir, 'server.ts');

    my $err = gensym();
    my $pid = open3(my $in, my $out, $err, 'npx', 'tsx', $server_path);

    my $selector = IO::Select->new($out, $err);
    my $startup_output = '';
    my $proxy_url;
    while (my @ready = $selector->can_read(15)) {
        for my $fh (@ready) {
            my $line = <$fh>;
            if (!defined $line) {
                $selector->remove($fh);
                next;
            }
            $startup_output .= $line;
            if ($line =~ /Listening:\s+(http:\/\/\S+)\s*(.*)/) {
                $proxy_url = $1;
                my $metadata_str = $2;
                # Parse JSON metadata if present
                if ($metadata_str && $metadata_str =~ /^\{/) {
                    eval {
                        my $meta = decode_json($metadata_str);
                        $self->{connect_proxy_url} = $meta->{connectProxyUrl};
                        $self->{ca_file_path} = $meta->{caFilePath};
                    };
                }
                last;
            }
        }
        last if $proxy_url || !$selector->count;
    }

    unless ($proxy_url) {
        kill 'TERM', $pid;
        die "Failed to parse proxy URL from startup output: $startup_output";
    }

    $self->{proxy_url}   = $proxy_url;
    $self->{process_pid} = $pid;
    $self->{stdout_fh}   = $out;

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

sub get_proxy_env {
    my ($self) = @_;
    my %env = %ENV;
    $env{COPILOT_API_URL} = $self->{proxy_url} if $self->{proxy_url};
    $env{GH_TOKEN} = 'fake-token-for-e2e-tests';
    $env{GITHUB_TOKEN} = 'fake-token-for-e2e-tests';

    if ($self->{connect_proxy_url}) {
        $env{HTTPS_PROXY} = $self->{connect_proxy_url};
        $env{https_proxy} = $self->{connect_proxy_url};
    }
    if ($self->{ca_file_path}) {
        $env{NODE_EXTRA_CA_CERTS} = $self->{ca_file_path};
    }

    # Isolation: prevent real config leaking in
    my $tmpdir = File::Spec->tmpdir();
    $env{COPILOT_HOME} = File::Spec->catdir($tmpdir, "copilot-test-$$");
    $env{GH_CONFIG_DIR} = File::Spec->catdir($tmpdir, "copilot-test-$$");
    $env{XDG_CONFIG_HOME} = File::Spec->catdir($tmpdir, "copilot-test-$$");
    $env{XDG_STATE_HOME} = File::Spec->catdir($tmpdir, "copilot-test-$$");

    return \%env;
}

sub DESTROY {
    my ($self) = @_;
    $self->stop();
}

1;
