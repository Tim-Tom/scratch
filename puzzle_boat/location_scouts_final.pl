use strict;
use warnings;

use v5.10;

my @words;
while (<DATA>) {
  chomp;
  s/[^\w\[\] ]//g;
  push(@words, uc);
}

my $target = join(' ', map { uc } @ARGV);

my @t = split(//, $target);
word:
for my $word (@words) {
  my %mapping;
  my @w = split(//, $word);
  my ($index, ) = grep { $w[$_] eq '[' } keys @w;
  @w = grep { $_ ne '[' && $_ ne ']' } @w;
  next unless @t == @w;
  for my $i (keys @t) {
    my $t = $t[$i];
    my $w = $w[$i];
    next word if exists $mapping{$w} && $mapping{$w} ne $t;
    $mapping{$w} = $t;
  }
  say "$word: $t[$index]";
}


__DATA__
Tampa [B]ay
Arctic Circ[l]e
Orinoco Ri[v]er
Lake [P]lacid
A[u]gusta, Maine
Panama Cana[l]
Urugua[y]
Ba[g]hdad, Iraq
[Q]uebec, Canada
Ka[z]akhstan
Carson City, Neva[d]a
Ellis Isla[n]d
Lansing, Mi[c]higan
Sonoran Deser[t]
Mad[r]id, Spain
Silico[n] Valley
