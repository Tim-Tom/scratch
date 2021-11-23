use strict;
use warnings;

use v5.10;

my @phrase;
while (<DATA>) {
  chomp;
  s/[^\w\[\] ]//g;
  push(@phrase, uc);
}

for my $target (map { uc } @ARGV) {

  my @t = split(//, $target);

  say $target;
  for my $phrase (@phrase) {
 word:
    for my $word (split(/ /, $phrase)) {
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
      my @from = keys %mapping;
      my @to = @mapping{@from};
      for my $l ('A' .. 'Z') {
        if (!exists  $mapping{$l}) {
          push(@from, $l);
          push(@to, '.');
        }
      }
      my $trans = $phrase;
      my $from = join('', @from);
      my $to = join('', @to);
      eval qq{\$trans =~ tr/$from/$to/};
      say "$phrase :: $word :: $trans";
      # say "$phrase: $t[$index]";
    }
  }
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
