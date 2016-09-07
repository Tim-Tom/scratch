use strict;
use warnings;

use v5.22;

use feature 'signatures';

sub pow2 ($base, $exp) {
  my ($prod, $sum) = ($base, 1);
  while($exp > 0) {
    say "$prod, $sum, $exp";
    if ($exp % 2 == 1) {
      $sum *= $prod;
    }
    $prod *= $prod;
    $exp = int($exp / 2);
  }
  return $sum;
}

say pow2(2, 10);

#for my $base (1 .. 20) {
#  for my $exp (1 .. 7) {
#    if (pow2($base, $exp) != $base ** $exp) {
#      say "Oops: $base ** $exp";
#    }
#  }
#}
