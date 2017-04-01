use strict;
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
