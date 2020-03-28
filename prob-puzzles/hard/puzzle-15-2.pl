use v5.24;

use strict;
use warnings;

use PDL;

=pod

Jack and Jill are playing a game built on infinite fair coin tossing. Jack is betting on
the pattern HHHH occurring first, while Jill has her money on HTTH.

Let T_a denote the number of tosses until Jack's pattern first appears, whle T_i denotes
the same for Jill's. If Jack promises to pay Jill (T_a - T_i), which could be either
positive or negative, how much will she recieve on average?

=cut

=pod

The first part gives us that we expect T_a to be 30 and t_i to be 18, thus meaning payment
is 12. This will now give us in B, the probability of ending in either of the final states
from the start.

It ended up being that they just wanted the T_a-T_i, so this wasn't needed, but keeping it
for funzies.

=cut

my @transitions = (
  '' => ['H', ''],
  'H' => ['HH', 'HT'],
  'HH' => ['HHH', 'HT'],
  'HT' => ['H', 'HTT'],
  'HHH' => ['HHHH', 'HT'],
  'HTT' => ['HTTH', ''],
  'HHHH' => ['HHHH', 'HHHH'],
  'HTTH' => ['HTTH', 'HTTH']
);

my %transitions = @transitions;

my @labels =  grep {!ref} @transitions;
my @values = grep {ref} @transitions;

my %idx = map { $labels[$_] => $_ } 0 .. $#labels;

my @P = map { [(0, ) x scalar @labels] } 0 .. $#labels;

for my $i (0 .. $#values) {
  my ($h, $t) = @{$values[$i]};
  $P[$i][$idx{$h}] += .5;
  $P[$i][$idx{$t}] += .5;
}

my $transient = 6;
my $absorbing = 2;

die unless $transient + $absorbing == @labels;

my $cni = 6;
my $cnm = 5;

my $P = pdl([@P]);
say $P;
my $Q = $P->slice("0:$cnm,0:$cnm");
say $Q;
my $R = $P->slice("$cni:7,0:$cnm");
say $R;
my $N = (identity($cni,$cni) - $Q)->inv;
say $N;
my $B = $N x $R;
say $B;
my $t = $N x ones(1,$cni);
say $t;
