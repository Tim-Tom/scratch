use strict;
use warnings;

use List::Util qw(max);

use v5.24;

my @num;
my @remain = map { 0 } 1 .. 10;

sub pick {
  my $i = shift;
  my $minCount = 0;
  if ($i == 9) {
    my $n = -$remain[$i];
    return if $n < 0 || $n > 9;
    $remain[$n] -= 1;
    $remain[$i] += $n;
    if (grep { $remain[$_] != 0 } 0 .. $#remain) {
      $remain[$n] += 1;
      $remain[$i] -= $n;
    } else {
      $num[$i] = $n;
      say "@num";
    }
  } else {
    for my $n (0 .. 9) {
      $num[$i] = $n;
      $remain[$n] -= 1;
      $remain[$i] += $n;
      pick($i + 1);
      $remain[$n] += 1;
      $remain[$i] -= $n;
    }
  }
}

pick(0);
