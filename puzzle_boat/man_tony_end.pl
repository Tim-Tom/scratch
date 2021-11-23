use strict;
use warnings;

use v5.20;

use experimental qw(signatures);

use List::Util qw(uniq pairs unpairs pairmap);
use Data::Printer;

sub count_word($word) {
  my @letters = grep { /[A-Z]/ } split(//, uc $word);
  my %count;
  $count{$_}++ foreach @letters;
  return %count;
}

my %lengths;
$lengths{3} = 1;
$lengths{4} = 1;
$lengths{5} = 1;
$lengths{6} = 1;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless $lengths{length $_};
  next if uc =~ /[^ACDEIMNR]/;
  push($words[length $_]->@*, uc $_);
}
close $words;

for my $w1 ($words[5]->@*) {
  for my $w2 ($words[4]->@*) {
    push($words[0]->@*, "$w1-$w2");
  }
}

for my $w1 ($words[6]->@*) {
  for my $w2 ($words[3]->@*) {
    push($words[0]->@*, "$w1-$w2");
  }
}

say scalar $words[0]->@*;

my @clues = 'Acme-Nadir';
@clues = map { uc s/ //gr } @clues;

for my $i (keys @clues) {
  my $clue = $clues[$i];
  my %cc = count_word($clue);
  say $clue;
 combo:
  for my $combo ($words[0]->@*) {
    my %wc = count_word($combo);
    my @exceed;
    for my $letter (keys %wc) {
      die "$clue $letter $combo" unless $cc{$letter};
      if ($cc{$letter} < $wc{$letter}) {
        next combo;
      }
    }
    say "\t$combo";
  }
}
