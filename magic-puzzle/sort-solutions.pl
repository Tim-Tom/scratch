use strict;
use warnings;

use v5.24;

for my $file (@ARGV) {
  open(my $input, '<:encoding(ascii)', $file) or die;
  my @solutions;
  my $solution = '';
  while(<$input>) {
    if (/--- Solution (\d+ )?---/a) {
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
  open(my $output, '>:encoding(ascii)', $file) or die;
  my $idx = 1;
  for my $solution (sort @solutions) {
    say $output "--- Solution $idx ---";
    print $output $solution;
    ++$idx;
  }
}
