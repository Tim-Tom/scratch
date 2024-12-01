use strict;
use warnings;

use List::Util qw(sum0 zip unpairs pairmap);

use v5.26;

my @data = map { [split] } <ARGV>;
close *ARGV;
my @left = sort { $a <=> $b } map { $_->[0] } @data;
my @right = sort { $a <=> $b } map { $_->[1] } @data;

say sum0 pairmap { abs($a - $b) } unpairs zip \(@left, @right);
