use v5.24;

use strict;
use warnings;

use List::Util qw(sum);

=pod

Let's ask the same question as in the previous puzzle, but in a more realistic setting: we
still have 0.4 probability of sun, ditto for clouds, and 0.2 probabiliy of rain, but we'll
abandon the assumption that weather is independant across days.

Instead the weather follows a first-order Markov process: if it's either sunny or cloudy
today, the probabilities for tomorrow's weather are: 0.5 chance of sun, 0.25 clouds, 0.25
rain. If it's raining today, we'll have clouds tomorrow with probability 1 -- it'll never
rain twice in a row.

Now again, across a 10 day period, what's the expected number of blocks of identical
weather?

=cut

=pod

See previous solution notes. Nothing changes except the probabilities used in the transition points.

=cut

my @types = qw(sunny cloudy rainy);

my @prob = (0.4, 0.4, 0.2);

my @prob_pairs = ([0.5, 0.25, 0.25], [0.5, 0.25, 0.25], [0.0, 1.0, 0.0]);

my @expected = (1.0, 1.0, 1.0);

my $max_days = $ARGV[0] || 10;
for my $day (2 .. $max_days) {
  my @new_expected = (0, ) x @types;
  for my $current (0 .. $#types) {
    for my $next (0 .. $#types) {
      $new_expected[$current] += $prob_pairs[$current][$next] * (($current == $next ? 0 : 1) + $expected[$next]);
    }
  }
  @expected = @new_expected;
}


my $expected = sum(map { $prob[$_] * $expected[$_] } 0 .. $#types);

say "[" . join(' ', map { sprintf '%0.3f', $_ } @expected) . ']';
say "Epected after $max_days days: $expected";
