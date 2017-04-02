use v5.24;

use strict;
use warnings;

my $max = 255;
my $limit = 3*255;
my $cnt = 0;
my @x;
for my $x (1 .. $max) {
  for my $y ($x .. int(($limit - $x)/2)) {
    my $z = $limit - $x - 2*$y + 1;
    $x[$x] += $z;
    # say "$x $y $z";
    $cnt += $z;
  }
}
# say "$_: $x[$_]" foreach (1 .. $max);
# say "$cnt";

# foreach my $x (1 .. $max) {
#   my $n = $max - $x + 1;
#  my $calculated = 18*$n**2 - 12*$n;
#  $calculated += 2 if $n % 2;
#  $calculated /= 8;
#  say "$x: $x[$x] $calculated" if $calculated != $x[$x];
#}

my $sum = 0;

print "{";
for my $x (reverse 1 .. $max) {
  $sum += $x[$x];
  print "$sum,";
}
print "...}\n";

# say 255*256*257/6;


# (4z**2 + 4z + 1) / (z - 1)**4 * (z - 1)
# 4 + 4 + 1 = 9
# 4*4 + 4*2 + 1 = 16 + 8 + 1 = 25 / 
