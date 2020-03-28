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


sub avg_distance {
  my @transitions = @_;

  my %transitions = @transitions;

  my @labels =  grep {!ref} @transitions;
  my @values = grep {ref} @transitions;

  my $cnt = @labels;
  my $cni = $#labels;
  my $cnm = $cni - 1;

  my %idx = map { $labels[$_] => $_ } 0 .. $cni;

  my @P = map { [(0, ) x scalar @labels] } 0 .. $cni;

  for my $i (0 .. $cni) {
    my ($h, $t) = @{$values[$i]};
    $P[$i][$idx{$h}] += .5;
    $P[$i][$idx{$t}] += .5;
  }

  my $P = pdl([@P]);
  my $Q = $P->slice("0:$cnm,0:$cnm");
  my $R = $P->slice("$cni,0:$cnm");
  my $N = (identity($cni,$cni) - $Q)->inv;
  my $B = $N x $R;
  my $t = $N x ones(1,$cni);

  return $t->at(0,0);

}

my $jack = avg_distance(
  '' => ['H', ''],
  'H' => ['HH', ''],
  'HH' => ['HHH', ''],
  'HHH' => ['HHHH', ''],
  'HHHH' => ['HHHH', 'HHHH']
 );


my $jill = avg_distance(
  '' => ['H', ''],
  'H' => ['H', 'HT'],
  'HT' => ['H', 'HTT'],
  'HTT' => ['HTTH', ''],
  'HTTH' => ['HTTH', 'HTTH']
 );

say "Jack: $jack";
say "Jill: $jill";

my $both = avg_distance(
  '' => ['H', ''],
  'H' => ['HH', 'HT'],
  'HH' => ['HHH', 'HT'],
  'HHH' => ['HHHH', 'HT'],
  'HHHH' => ['HHHH', 'HHHH'],
  'HT' => ['H', 'HTT'],
  'HTT' => ['HTTH', ''],
  'HTTH' => ['HTTH', 'HTTH']
);
