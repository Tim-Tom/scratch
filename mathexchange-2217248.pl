use strict;
use warnings;

use v5.24;

use List::Util qw(sum);

# https://math.stackexchange.com/questions/2217248/which-answer-in-this-list-is-the-correct-answer-to-this-question
# Which answer in this list is the correct answer to this question?
# 1. All of the below.
# 2. None of the below.
# 3. All of the above.
# 4. One of the above.
# 5. None of the above.
# 6. None of the above.

my @state;

my @validators = (
  sub { 5 == sum @state[1..5]},
  sub { 0 == sum @state[2..5]},
  sub { 2 == sum @state[0,1] },
  sub { 1 == sum @state[0..2] },
  sub { 0 == sum @state[0..3] },
  sub { 0 == sum @state[0..4] }
);

# Go through all configurations to find one such that the input is equal to the output.
# Given the problem statement we could presume that there is only one valid answer, but
# but given the small number of possible configurations it doesn't make it any slower to
# be exhaustive and prove multiple can't be true simultaneously.
for my $i (0 .. 63) {
  @state = map { $_ ? 1 : 0 } map { $i & (1 << $_) } 0 .. 5;
  my $state = join('', @state);
  my @valid = map { $_ ? 1 : 0 } map { $_->() } @validators;
  my $valid = join('', @valid);
  printf "%2d %s %s %s\n", $i, $state, $valid, $valid eq $state ? '  valid' : 'invalid';
}
