use v5.24;

use strict;
use warnings;

use List::Util qw(min);

use experimental 'signatures';

use Carp qw(confess);

my %memory;

my $best_size = 0;
my $best_key = '1;1;1';

my $level = 0;
sub find_best($p1, $p2, $p3) {
  # Since we're sorted, we don't have to check p1 == p3
  say (('-'x$level++) . "Checking $p1 $p2 $p3");
  if ($p1 == $p2 || $p2 == $p3) {
    say (('-'x$level--) . "Base Case");
    return 0;
  }
  my $key = join(';', $p1, $p2, $p3);
  if (exists $memory{$key}) {
    say (('-'x$level--) . "Remembered $memory{$key}");
    return $memory{$key};
  }
  $memory{$key} = 1e100;
  my $min = min(
    find_best(sort { $a <=> $b } $p1+$p1, $p2 - $p1, $p3),
    find_best(sort { $a <=> $b } $p1+$p1, $p2,       $p3 - $p1),
    find_best(sort { $a <=> $b } $p1,     $p2+$p2,   $p3 - $p2)
   ) + 1;
  if ($min > $best_size) {
    $best_size = $min;
    $best_key = $key;
  }
  say (('-'x$level--) . "Best of $p1, $p2, $p3 is $min");
  return $memory{$key} = $min;
}

my $max = 10;
for my $p1 (1 .. $max) {
  for my $p2 ($p1+1 .. $max) {
    for my $p3 ($p2+1 .. $max) {
      find_best($p1, $p2, $p3);
    }
  }
}

# find_best(3,4,8);

say "Best was $best_key with $best_size turns";
