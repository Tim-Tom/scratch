use strict;
use warnings;

use Algorithm::Permute;

use v5.24;

use experimental qw(signatures);

my @corners = (
  [qw(tl tr)],
  [qw(bl br)]
 );

my @symmetry = (
  [0,1,2,3], #  x  y
  [2,3,0,1], #  x -y
  [1,0,3,2], # -x  y
  [3,2,1,0], # -x -y
  [0,2,1,3], #  y  x
  [2,0,3,1], #  y -x
  [1,3,0,2], # -y  x
  [3,1,2,0], # -y -x
 );


# Generate sample values for the 4 corners.
my $values = Algorithm::Permute->new([1 .. 4]);

my %seen;
while (my @vals = $values->next()) {
  my $rKey = join('', @vals);
  # Skip if top left > top right
  if ($vals[0] > $vals[1]) {
    next;
  }
  # Skip if top right < bottom left
  if ($vals[1] > $vals[2]) {
    next;
  }
  # Skip if bottom right is greater than bottom left and top left.
  if ($vals[2] > $vals[3] && $vals[0] > $vals[3]) {
    next;
  }

  for my $idx (@symmetry) {
    my $key = join('', @vals[@$idx]);
    push(@{$seen{$key}}, $rKey);
  }
}

$values = Algorithm::Permute->new([1 .. 4]);

while (my @vals = $values->next()) {
  my $key = join('', @vals);
  local $" = ", ";
  say "$key: @{$seen{$key} // []}";
}
