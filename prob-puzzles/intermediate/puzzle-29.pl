use v5.24;

use strict;
use warnings;


=pod

Ten people are attending a party in a two room apartment and, initially, it is equally
likely that all ten are in room A or nine are in Room A and one in room B. This is no
ordinary party: every minute, one person is chosen uniformly at random (independently of
whatever occurred in the past), and they must move into the other room (i.e. if they were
in room A they move to B and vice-versa).

For example, suppose we're in the 50% of cases where everyone is initially in room A. At
the end of the first minute, someone will be chosen and will walk into room B, leaving 9
people in room A. At the end of the second minute, with probability 9/10 someone in room A
will be selected (in which case they'd go join the person in room B), or, with probability
1/10 the person in room B will return to room A, restoring the initial configuration.

We're interested in the probability that the group is evenly split between rooms A and B
(i.e. 5 people in each) after N minutes. What does the probability converge to as N goes
to infinity?

=cut

=pod

So obviously the initial conditions don't matter at all. That was just thrown in to
confuse the problem. We're looking at T = inf, so it will just go to whatever state it
wants to by then.

I've got nothing at this time. This is a simulation, the accepted answer is 0.24610. I get
it being about a quarter of the time in the state, but have no better analysis at this
time.

=cut

my $a = 10;
my $b = 0;

my $equal = 0;
my $total = 10_000_000;

for my $step (1 .. $total) {
  if (rand(10) < $a) {
    --$a;
    ++$b;
  } else {
    --$b;
    ++$a;
  }
  ++$equal if ($a == 5);
}
say "Equal: $equal";
printf "ratio: %.5f\n", $equal / $total;
