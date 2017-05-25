use strict;
use warnings;

use v5.24;

die unless @ARGV == 3;
my ($file, $ki, $vi) = @ARGV;

open(my $in, '<', $file) or die;

my $mi = $vi;
$mi = $ki if $ki > $vi;
$mi += 2;

my %sum;
while(<$in>) {
  chomp;
  my @a = split(/\t/, $_, $mi);
  $sum{$a[$ki]} += $a[$vi];
}

my ($max, $maxKey) = (0, 'N/A');
while (my ($key, $value) = each %sum) {
  if ($value > $max) {
    $max = $value;
    $maxKey = $key;
  }
}

say "max_key: $maxKey sum: $max";
