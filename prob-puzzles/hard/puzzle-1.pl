use v5.24;

use strict;
use warnings;

=pod

Five foxes and seven hounds run into a fox-hole. While they're inside they get all jumbled
up, so thgat all orderings are equally likely.

The foxes and hounds run out of the hole in a neat line. On average, how many foxes are
immediately follwoed by a hound?

=cut

=pod

The hint makes it pretty obious that it is 5 foxes * 7/12 chance of a hound, but I wanted
to look whether that breaks down in any limits. So if there are a dearth of foxes or
hounds does it still work?

5 foxes, 7 hounds = 5 * 7/12 = Validated
7 foxes, 5 hounds = 7 * 5/12 = Validated
5 foxes, 30 hounds = 5 * 30/35 = Validated
30 foxes, 5 hounds = 30 * 5/35 = Validated

So it looks like even when you risk running out of hounds, the math still works out, cool.

=cut


my @items = sort (('fox', ) x 30, ('hound', ) x 5);

local $, = ' ';

sub count {
  my $count = 0;
  for my $i (1 .. $#items) {
    $count++ if ($items[$i-1] eq 'fox' && $items[$i] eq 'hound');
  }
  return $count;
}

my $total;
my $fox_hounds;
++$total;
$fox_hounds += count();
while (1) {
  my ($i, $j) = ($#items, $#items);
  # Find the left pivot (first item that is less than a successor)
  while($i > 0 && $items[$i-1] ge $items[$i]) {
    --$i;
  }
  # Dropped off the left side, no more pivots.
  last if $i == 0;
  # Previously i was the head of the ascending suffix, change it to be the pivot.
  --$i;
  while ($items[$j] le $items[$i]) {
    --$j;
  }
  # Swap the pivot and that element
  @items[$i, $j] = @items[$j, $i];
  # Reverse the suffix
  ($i, $j) = ($i + 1, $#items);
  while ($i < $j) {
    @items[$i, $j] = @items[$j, $i];
    ++$i;
    --$j;
  }
  ++$total;
  $fox_hounds += count();
}

say "$total total permutations";
say "$fox_hounds foxes followed by hounds";
printf '%d / %d = %.3f'."\n", $total, $fox_hounds, ($fox_hounds / $total);
