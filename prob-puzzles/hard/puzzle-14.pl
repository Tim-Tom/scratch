use v5.24;

use strict;
use warnings;

use PDL;

=pod

When considering an infinite sequence of tosses of a fair coin, how long will it take on
average until the pattern HTTH appears?

=cut

my @transitions = (
  '' => ['H', 'T'],
  'H' => ['H', 'HT'],
  'HT' => ['H', 'HTT'],
  'HTT' => ['HTTH', ''],
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

use Data::Printer;

my $P = pdl([@P]);
say $P;

my $Q = $P->slice('0:3,0:3');
say $Q;

my $R = $P->slice('4,0:3');
say $R;

my $N = (identity(4,4) - $Q)->inv;
say $N;

my $B = $N x $R;
say $B;

my $t = $N x ones(1,4);

say $t;
