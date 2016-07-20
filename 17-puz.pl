use strict;
use warnings;

use v5.22;

my @nums = (6, 6, 5, 2);

my $target = 17;

sub solve {
  my ($t, @n) = @_;
  if (@n == 0) {
    if ($t == $target) {
      say "found!";
      return 1;
    }
    return 0;
  }
  for my $i (0 .. $#n) {
    my $n = $n[$i];
    my @nn = (@n[0 .. $i - 1], @n[$i + 1 .. $#n]);
    if (solve($t + $n, @nn)) {
      say "$t + $n";
      return 1;
    }
    if (solve($t - $n, @nn)) {
      say "$t - $n";
      return 1;
    }
    if (solve($t * $n, @nn)) {
      say "$t * $n";
      return 1;
    }
    if (solve($t / $n, @nn)) {
      say "$t / $n";
      return 1;
    }
  }
  return 0;
}

solve(@nums);
solve(5, 6, 6, 2);
solve(2, 5, 6, 6);
