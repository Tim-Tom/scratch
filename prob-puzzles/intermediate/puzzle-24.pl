use v5.24;

use strict;
use warnings;

=pod

Let's stick with our usual assumptions regarding babies: they're girls with probability
0.49, boys with probability 0.51, and gender is independent across births.

If a large number of parents keep having children until they have a girl -- at which point
they stop, regardless of whether she's an only child or has lots of brothers -- what will
be the fraction of female children in the overall population?

=cut

=pod

The hint of this puzzle makes it obvious the answer is the global percentage, but that
seemed like one of those true at infinity, not at anything less kind of answer so I
decided to just simulate a million families and see what the ratio was.

=cut

my $boys = 0;
my $girls = 1_000_000;

for my $family (0 .. $girls) {
  ++$boys while (rand > 0.49);
}

say "boys: $boys";
say "girls: $girls";
printf "ratio: %.3f\n", ($girls / ($boys + $girls));


