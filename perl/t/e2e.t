#!/usr/bin/env perl
# Wrapper that runs E2E tests from the t/ directory (expected by prove).
# The actual tests live in e2e/test_e2e.t.

use strict;
use warnings;
use File::Spec;
use File::Basename;

my $e2e_test = File::Spec->catfile(dirname(__FILE__), '..', 'e2e', 'test_e2e.t');
do "./$e2e_test" or die "Failed to load $e2e_test: $@\n$!\n";
