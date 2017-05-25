use strict;
use warnings;

use Text::CSV_XS;

use v5.24;

use List::Util qw(reduce);

die unless @ARGV == 3;
my ($file, $ki, $vi) = @ARGV;

open(my $in, '<', $file) or die;

my $csv = Text::CSV_XS->new({ binary => 1, sep_char => "\t", quote_char => undef });

my %sum;
die unless $ki = 1 && $vi == 2;

my ($k, $v, $u1, $u2);
$csv->bind_columns(\($u1, $k, $v, $u2));
while($csv->getline($in)) {
  $sum{$k} += $v;
}

my ($max, $maxKey) = (0, 'N/A');

while (my ($key, $value) = each %sum) {
  if ($value > $max) {
    $max = $value;
    $maxKey = $key;
  }
}

say "max_key: $maxKey sum: $max";
