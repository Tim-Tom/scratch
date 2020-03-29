use strict;
use warnings;

use v5.24;

sub is_valid {
  my @solution = grep { /\S/ } split(/\s+/, $_[0]);
  my $width = int(sqrt(scalar @solution));
  my $tl = 0;
  my $tr = $width - 1;
  my $bl = ($width - 1) * $width;
  my $br = $#solution;
  return $solution[$tl] < $solution[$tr] &&
    $solution[$tr] < $solution[$bl] &&
    ($solution[$bl] < $solution[$br] || $solution[$tl] < $solution[$br]);
}

for my $file (@ARGV) {
  open(my $input, '<:encoding(ascii)', $file) or die;
  my @solutions;
  my $solution = '';
  while(<$input>) {
    if (/--- Solution \d+ ---/a) {
      if ($solution) {
        push(@solutions, $solution);
      }
      $solution = '';
    } else {
      $solution .= $_;
    }
  }
  if ($solution) {
    push(@solutions, $solution);
  }
  close $input;
  open(my $output, '>:encoding(ascii)', "$file.trimmed") or die;
  my $idx = 1;
  for my $solution (sort grep { is_valid($_) } @solutions) {
    say $output "--- Solution $idx ---";
    print $output $solution;
    ++$idx;
  }
}
