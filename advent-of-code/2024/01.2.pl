use strict;
use warnings;

use List::Util qw(sum0 zip unpairs pairmap);

use v5.26;

my @data = map { [split] } <ARGV>;

my (@left, %right);
for my $entry (@data) {
  my ($left, $right) = $entry->@*;
  push(@left, $left);
  $right{$right}++;
}

my @intermediate = map { $_*($right{$_} // 0) } @left;
# use Data::Printer;
# p(@intermediate);
say sum0 @intermediate;
