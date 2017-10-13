use v5.24;

use strict;
use warnings;

=pod

You pick a signle number uniformly at random from the set 1, 2, ..., 1000.

What's the probability that it's a multiple of one (or more) of the numbers 7, 11 or 3.

=cut

=pod

So you can just use the fact that 6/7, 10/11, and 12/13 of numbers are not multiples of
these and multiply them out and get 719.28 numbers are not multiples. Subtract that out
of the total and you get 280 numbers are multiples.

Here's the boring way to do it though.

=cut


my @next = (7, 11, 13);
my @increment = @next;

my $count = 0;
for my $i (1 .. 1000) {
  my $is_multiple = 0;
  for my $j (0 .. $#next) {
    if ($i == $next[$j]) {
      $is_multiple = 1;
      $next[$j] += $increment[$j];
    }
  }
  $count += $is_multiple;
}

say "$count / 1000";
