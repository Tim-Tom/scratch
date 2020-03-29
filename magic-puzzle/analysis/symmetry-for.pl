use strict;
use warnings;

use v5.24;

use experimental qw(signatures);

my $solution = join('', <ARGV>);

my @solution = grep { /\S/ } split(/\s+/, $solution);

my @idxs = (
  [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15],
  [12, 13, 14, 15,  8,  9, 10, 11,  4,  5,  6,  7,  0,  1,  2,  3],
  [ 3,  2,  1,  0,  7,  6,  5,  4, 11, 10,  9,  8, 15, 14, 13, 12],
  [15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0],
  [ 0,  4,  8, 12,  1,  5,  9, 13,  2,  6, 10, 14,  3,  7, 11, 15],
  [12,  8,  4,  0, 13,  9,  5,  1, 14, 10,  6,  2, 15, 11,  7,  3],
  [ 3,  7, 11, 15,  2,  6, 10, 14,  1,  5,  9, 13,  0,  4,  8, 12],
  [15, 11,  7,  3, 14, 10,  6,  2, 13,  9,  5,  1, 12,  8,  4,  0],
);

@solution = map { sprintf '%2d', $_ } @solution;

my @permutations;
for my $idx (@idxs) {
  my @perm = @solution[@$idx];
  my $perm = '';
  for my $y (0 .. 3) {
    $perm .= join(' ', @perm[4*$y .. (4*($y+1) - 1)]) . "\n";
  }
  push(@permutations, $perm);
}

my $solNo = 1;


sub is_valid {
  my @solution = grep { /\S/ } split(/\s+/, $_[0]);
  my $width = int(sqrt(scalar @solution));
  my $tl = 0;
  my $tr = $width - 1;
  my $bl = ($width - 1) * $width;
  my $br = $#solution;
  return ($solution[$tl] < $solution[$tr],
          $solution[$tr] < $solution[$bl],
          $solution[$bl] < $solution[$br] || $solution[$tl] < $solution[$br]);
}


for my $perm (sort @permutations) {
  my $valid = join('', map { $_ ? 'T' : 'F' } is_valid($perm));
  say "--- Solution $solNo $valid ---";
  print $perm;
  ++$solNo;
}
