use strict;
use warnings;


use v5.22;
use feature 'signatures';

no warnings 'experimental::signatures';

my @schools;

my @fact = (1, );

for my $n (1 .. 14) {
  $fact[$n] = $n*$fact[$n-1];
}

sub calculate_sizes {
  my $total = $fact[14];
  my ($one, $two, $onetwo) = ($total, $total, $total);
  for my $si (1 .. 4) {
    my $s = $schools[$si];
    $one /= $fact[$si] ** $s;
    $two /= $fact[$s];
    $onetwo /= $fact[$s] * $fact[$si] ** $s;
  }
  local $" = " ";
  printf "| (%8s) | %11d | %11d | %13d |\n", "@schools[1..4]", $one, $two, $onetwo;
}

sub pick($size, $remain) {
  if ($size == 1) {
    $schools[1] = $remain;
    return calculate_sizes;
  }
  for my $count (0 .. int($remain / $size)) {
    $schools[$size] = $count;
    pick($size - 1, $remain - $count * $size);
  }
}
printf "| %8s | %11s | %11s | %13s |\n", "( 1 2 3 4)", "Teams", "Schools", "Teams+Schools";
pick(4, 14);
