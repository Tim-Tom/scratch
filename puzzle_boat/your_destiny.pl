use strict;
use warnings;

use v5.20;

my $regex = qr/^[ts][il][ra][ow]$/;
# my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
# open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
while(<$words>) {
  chomp;
  my $word = lc $_;
  next unless $word =~ /$regex/;
  say $word;
}
close $words;
