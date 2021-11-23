use strict;
use warnings;

use utf8;

use v5.20;

use List::Util qw(uniq);

my $full_clue = 'CWZE 1: CZSGRLYULLYIOYX ON XDPY HBAU! FWC QEICJVL';

my $clue = 'UVWGDCDY';

my @alphabet = ('A', 'T', 'Q', 'I', 'W', 'M', 'E', 'J', 'P', 'S', 'B', 'R', 'Y', 'U', 'H', 'F', 'V', 'K', 'G', 'N', 'C', 'X', 'Z', 'O', 'L', 'D');# 'A' .. 'Z';

say join('', @alphabet);
print "\n";

my %encode;
my %decode;
{
  my @letters = @alphabet;
  for my $l (@alphabet) {
    $encode{$l} = {map { $alphabet[$_] => $letters[$_]} keys @letters};
    $decode{$l} = {map { $letters[$_] => $alphabet[$_]} keys @letters};
    @letters = (@letters[1..$#letters], $letters[0]);
  }
}

say join('', ' ', @alphabet);
for my $letter (@alphabet) {
  say join('', $letter, map { $encode{$letter}{$_} // '.' } @alphabet);
}

my $key = 'LOKI';

my $plain = 'THE RAIN IN SPAIN FALLS MAINLY IN THE PLAINS';

my @key = split(//, $key);
my $encoded = '';
my $index = 0;
for my $letter (split(//, $plain)) {
  my $trans = $encode{$key[$index]}{$letter};
  if (defined $trans) {
    $encoded .=  $trans;
    $index = ($index + 1) % @key;
  } else {
    $encoded .= $letter;
  }
}
say $encoded;
my $decoded = '';
for my $letter (split(//, $encoded)) {
  my $trans = $decode{$key[$index]}{$letter};
  if (defined $trans) {
    $decoded .=  $trans;
    $index = ($index + 1) % @key;
  } else {
    $decoded .= $letter;
  }
}
say $decoded;
