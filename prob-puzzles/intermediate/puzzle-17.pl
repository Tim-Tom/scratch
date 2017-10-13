use v5.24;

use strict;
use warnings;

=pod

In front of you is an infinitelylived machine that proposes amounts of money, which you
can either accept or reject. If you accept, the machine hands over the proposed amount,
but shuts down and will never give you anything else. If you reject, it will show you a
new proposal next period.

Each period's proposal is an independant draw from a uniform distribution on [0, 100]. The
time between periods is long -- several months, say -- and you are impatient: a dollar
next period is worth only 0.9 to you today; similarly a dollar two periods from now is
worth 0.9*0.9=0.81 today,et cetera.

If your strategy were to always accept, you'd expect to make $50, i.e. the mean of the
first draw. If instead you decided to accept any first proposal above 50, and -- in case
you reject the first -- any second proposal whatsoever, your expected discounted payoff
would be $60. But you can do better!

If you follow the strategy that maximizes your expected discounted payoff, what is the
threshold above which you should accept the machines first proposal?

=cut

# I'm solving this by simulating what better math could probably tell me through some form
# of integration.  Assume a final case where you expect to make $50, then expand your
# estimate outward with the discount each time maximizing the results as of that many steps.

my $expected = 0.50;
my $discount = 0.9;

my $limit = $ARGV[0];

for my $i (1 .. $limit) {
  # At every step I want to maximize the equation:
  # f(t) = P(take)*E(take) + P(~take)*0.9*f(t-1)
  # with respect to P(take)
  # Where E(take) is (1 + (1-P(take)))/2 or 1 - P(take)/2
  # so: f(t) = p*(1 - p/2) + 0.9*(1-p)*f(t-1)
  # f(t) = (10p - 5p^2 + 9*(1-p)*f(t-1))/10
  # f(t) = (-5p^2 + 10p - 9p(f(t-1)) + 9f(t-1))/10
  # f'(t) = (-10p + 10 - 9f(t-1))/10
  # 10*0 = 10p - 10 + 9f(t-1)
  # p = 1 - 0.9f(t-1)
  my $discounted = $expected * $discount;
  my $prob = 1 - $discounted;
  # my $prob = 0.5;
  $expected = $prob*(1 - $prob/2) + (1-$prob)*$discounted;
  # printf 'Step %d: [Discounted %.2f] [Prob(take) %.2f] [Expect %.2f]'."\n", $i, $discounted, $prob, $expected;
}

printf "%.4f\n", 90*$expected;
