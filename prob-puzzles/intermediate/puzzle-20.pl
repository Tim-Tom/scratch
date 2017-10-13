use v5.24;

use strict;
use warnings;

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

I know there's a way to model this in pure math because I remember doing it in college,
but I can't find anything in my search off of stochastic processes in Wikipedia.

So I could just iterate through all possible 3^10 states and count them since it's
tiny. Or I could be a little smarter and memoize the counts and reduce it to a small
fraction of the total number of visits, but let's try building another absorbing markov
chain and see how that plays out.

=cut
