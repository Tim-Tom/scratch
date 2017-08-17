use strict;
use warnings;

use v5.24;

open(my $input, '<:encoding(ascii)', '../../data/s1-c8.txt');

# I don't really like this because it assumes there will be repeated blocks, especially
# given that the encrypted snippets are all 10 blocks wide. So essentially they had to
# repeat the same text over and over to make this happen. They worked hard to make this
# toy work.
my $best = '';
my $best_score = -1;
while(my $line = <$input>) {
  chomp($line);
  my $data = pack('H*', $line);
  my @blocks = unpack('(a16)*', $data);
  my $matches = 0;
  for my $i (0 .. $#blocks) {
    for my $j ($i + 1 .. $#blocks) {
      ++$matches if $blocks[$i] eq $blocks[$j];
    }
  }
  if ($matches > $best_score) {
    $best_score = $matches;
    $best = $line;
  }
}

say "$best_score: $best";
