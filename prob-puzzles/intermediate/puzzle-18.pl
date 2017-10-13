use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use PDL;

=pod

You're a biologist studying a population of bacteria: there are initailly two of them,
swimming happily in their petri dish.

Each period, each bacterium will either die p = 0.25, or it will divide into two happy
bacteria. Assume deaths are independant accross time and individuals, and that this
process continues forever. We'll keep thigns simple and ignore the possibility that the
colony fills the petri dish or runs out of nutrients.

Over an infinite time horizon, what's the probability that the entire population dies out?

=cut

=pod

This problem seems like some form of infinite markov chain. Each state has definable
transitions to (almost) all other states between 0 and 2*b. I think the key observation
is that probabilistically it is equivalent to look at the bacteria one at a time. You
can see this in the first couple equations.

D(0) = 100%
D(1) = 0.25*D(0) + 0.75*D(2)
D(2) = 0.25(0.25*D(0) + 0.75*D(2)) + 0.75*(0.25*D(2) + 0.75*D(4)) = 0.25*D(1) + 0.75*??
D(3) = 0.25(0.25(0.25*D(0) + 0.75*D(2)) + 0.75*(0.25*D(2) + 0.75*D(4))) + 0.75(0.25(0.25*D(2) + 0.75*D(4)) + 0.75(0.25*D(4) + 0.75*D(6))) = 0.25*D(2) + 0.75*??

So you either have a 25% chance that the first bacteria dies, and you follow the same
probability of the previous state, or you get one more bacteria and get the same shape,
except with all the variables increased by two.

Note: This is probably fatally flawed because I handwave things I probably shouldn't be
able to handwave. But it makes logical sense.

So let's define a new function D'(x) that is that function. Namely the infinitely
recursive D'(x) = D'(x-1) + D''(x+1) with recursion on prime forever.

This gives us
D(x)  = 0.25*D(x-1) + 0.75*D'(x+1)
D(1)  = 0.25*D(0) + 0.75*D'(2)
D(2)  = 0.25*D(1) + 0.75*D'(3)
D(3)  = 0.25*D(2) + 0.75*D'(4)
D'(3) = 0.25*D(2) + 0.75*D(4)
D(4)  = 0.25*D(3) + 0.75*D'(5)
D'(4) = 0.25*(0.25*D(2) + 0.75*D(4)) + 0.75*(0.25*D(4) + 0.25*D(6))

And because we know D(1) is also 0.25*D(0) + 0.75*D(2) we know D'(2) = D(2). And that
D''(4) = D(4). And that D'''(6) = D(6). So we can follow that chain infinitely and know
that D(x) = D'{x/2}(x) for all even x.

D'(3) = 0.25*D'(2) + 0.75*D''(4) = 0.25*D(2) + 0.75*D(4)

And somehow using the above, we can prove that D'(3) = D(3), so all of thing is equal.

All this just lets us say it is equivalent probability wise to consider one bacteria
living or dying at a time and the rest staying static. We require more tiny steps to get
to the decision, but the final probabilities will be the same and given infinite time, it
doesn't really matter how many steps we take.

So now we can build a simple absorbing markov matrix. Q is the boring matrix of
transitions from all our states to the one above and below (except 1, which only goes
above). R is a column with 0.25 in the first row followed by all zeros. 0 is a row of
zeros and 1 is a single 1.

=cut

# Even though the petri dish is infinitely sized, we need to specify a maximum population size.
my $size = 100;

my $Q = [map { [map { 0 } 1 .. $size] } 1 .. $size];

for my $i (1 .. $size -  2) {
  $Q->[$i][$i-1] = 0.25;
  $Q->[$i][$i+1] = 0.75;
}
$Q->[0][1] = 0.75;
$Q->[$size-1][$size-2] = 0.25;

$Q = pdl($Q);
my $R = pdl [[0.25, 0], (map { [0, 0] } 3 .. $size), [0, 0.75]];
my $N = (identity($size, $size) - $Q)->inv;
my $B = $N x $R;

print $B->slice('(0), (1)') . "\n";

# Answer comes out to 1/9. Which makes it seem like there's a pretty way to think about
# the problem.
