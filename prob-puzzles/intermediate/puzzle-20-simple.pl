use v5.24;

use strict;
use warnings;

use List::Util qw(sum);

=pod

Imagine that, on a given day, the weather can be either sunny (with probability 0.4),
cloudy (ditto), or rainy (with probability 0.2). Let's make the heroic assumption that
weather is independent across days.

Define blocks or runs of weather to be (the largest possible) groups of consecutive days
in which the weather is the same. For example, if it rained for eight days, followed by a
day of sun, then a day of rain, we'd have 3 blocks.

Across a 10 day period, what's the expected number of blocks of identical weather.

=cut

=pod

So this is how I would normally solve this problem. We start off with an expected block
count of 1 for every weather type. Then for every additional day, we calculate the new
expected block count for that weather type by doing the weighted sum of the three possibilities.


So if it is currently sunny we have:

1. A 40% chance of the next day being sunny (in which case we just re-use the block count for sunny)
2. A 40% chance of being cloudy (in which case we add one to the block count stored for cloudy)
3. A 20% chance of being rainy (in which case we add one to the block count stored for rainy).

This works because the expected block count for any day doesn't depend on any days that
came before it. And the block count for n days only cares about the first day of a n-1 day
block.

=cut

my @types = qw(sunny cloudy rainy);

my @prob = (0.4, 0.4, 0.2);

my @expected = (1.0, 1.0, 1.0);

my $max_days = $ARGV[0] || 10;
for my $day (2 .. $max_days) {
  my @new_expected = (0, ) x @types;
  for my $current (0 .. $#types) {
    for my $next (0 .. $#types) {
      $new_expected[$current] += $prob[$next] * (($current == $next ? 0 : 1) + $expected[$next]);
    }
  }
  @expected = @new_expected;
}


my $expected = sum(map { $prob[$_] * $expected[$_] } 0 .. $#types);

say "[" . join(' ', map { sprintf '%0.3f', $_ } @expected) . ']';
say "Epected after $max_days days: $expected";
