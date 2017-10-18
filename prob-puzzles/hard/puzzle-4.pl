use v5.24;

use strict;
use warnings;

=pod

You enter a metro station in a big hurry, and decide to take the first train that arrives.

There are two lines running thrtough this station: one runs every five minutes (line A),
the other every three (line B). to be precise, suppose the next arrival of the A train is
uniformly distributed on the interval [0, 5], and similaly for the B train on [0,3]. The
two arrivals are independent.

The trains run like clockwork, there's no uncertainty other than the next arrival
time. For example, given that the next B train arrives at time 0.87, you can be absolutely
certain that there will be another at time 3.87.

How many minutes on average will you wait on average until you get a train.

=cut

my $total;
my $count;

my $delta = $ARGV[0] // 1000;

for my $i (0 .. 3*$delta) {
  # for my $j = (0 .. 5*$delta) {
  #   $total += ($i < $j) ? $i : $j;
  #   ++$count;
  # }
  # Part of the expression where j < i
  $total += ($i+1) * (0 + $i) / 2;
  $count += $i + 1;
  # Part of the expression where i <= j
  $total += $i*((5*$delta)-$i);
  $count += ((5*$delta)-$i);
}

printf '%d / %d = %.5f'."\n", $total, $count, ($total / $count / $delta);
