use v5.24;

use strict;
use warnings;

=pod

use experimental 'signatures';

sub make_error($seed) {
  return sub($incr=1) {
    $x = $x + $incr;
    return $x;
  };
}
sub make_not_accumulator($seed) {
  my $x = $seed;
  return sub($incr=1) {
    my $x = $x + $incr;
    return $x;
  };
}
sub make_accumulator($seed) {
  my $x = $seed;
  return sub($incr=1) {
    $x = $x + $incr;
    return $x
  };
}

=cut

sub test_ors {
  local $\ = "\n";
  print "hello";
  {
    local $\ = "$\  ";
    print "indented";
  }
  print "world";
}

test_ors();
