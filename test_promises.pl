use strict;
use warnings;

use v5.22;

use AnyEvent;

use Carp qw(cluck);

use Promises (qw(collect deferred), backend => ['AnyEvent']);

our $test_global = 0;

sub do_async {
  my $random = rand;
  local $test_global = $test_global + 1;
  warn "do_async $test_global: $random\n";
  if ($random > 0.5) {
    return do_async()->then(sub {
      cluck("Call with level $test_global");
      return $random;
    });
  } else {
    return deferred->resolve($random)->promise;
  }
};

my $cv = AnyEvent->condvar;
do_async()
  ->then($cv, sub { $cv->croak(@_); });

say $cv->recv;
