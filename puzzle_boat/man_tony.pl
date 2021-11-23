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

my @parts = (
#  ['M7', 'V5'],
#  ['A7', 'N7'],
#  ['P7', 'R5'],
#  ['P4', 'P6'],
#  ['H6', 'P6'],
  ['S7', 'T-1'],
 );

my %lengths = map { $_ => 1 } map { 1 + substr($_, 1) } map { @$_} @parts;
$lengths{4} = 1;
$lengths{5} = 1;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless $lengths{length $_};
  next if /\W/;
  push($words[length $_]->@*, uc $_);
}
close $words;

for my $w1 ($words[4]->@*) {
  next unless substr($w1, 0, 1) eq 'T';
  for my $w2 ($words[5]->@*) {
    push($words[0]->@*, "$w1-$w2");
  }
}

say scalar $words[0]->@*;

for my $bucket (@words) {
  next unless defined $bucket;
  $bucket = [map { [$_, { count_word($_), }] } sort { $a cmp $b } uniq ($bucket->@*, )];
}

my @clues = (
#  'Novelty Alumni',
#  'Innocent Malaria',
#  'Natural Speech',
#  'Special Part',
#  'Catfish Police',
  'The Pointless Maps-',
);

@clues = map { uc s/ //gr } @clues;

die unless @parts == @clues;

for my $i (keys @clues) {
  my $clue = $clues[$i];
  my $parts = $parts[$i];
  my %cc = count_word($clue);
  say $clue;
  my @possibles;
  for my $part ($parts->@*) {
    my @possible;
    my ($start, $length) = split(//, $part, 2);
    $length += 1;
    word: for my $pkg ($words[$length]->@*) {
      my ($word, $wc) = $pkg->@*;
      next unless substr($word, 0, 1) eq $start;
      for my $letter (keys $wc->%*) {
        next word if (($cc{$letter} // 0) < $wc->{$letter});
      }
      push(@possible, $pkg);
    }
    push(@possibles, \@possible);
  }
  die unless @possibles == 2;
  for my $p1 ($possibles[0]->@*) {
    my ($p1w, $p1c) = $p1->@*;
    p2: for my $p2 ($possibles[1]->@*) {
      my ($p2w, $p2c) = $p2->@*;
      my %wc = $p1c->%*;
      $wc{$_} += $p2c->{$_} foreach (keys $p2c->%*);
      my @exceed;
      for my $letter (keys %wc) {
        die "$clue $letter $p1w $p2w" unless $cc{$letter};
        if ($cc{$letter} < $wc{$letter}) {
          if ($cc{$letter} + 1 == $wc{$letter}) {
            push(@exceed, $letter);
          } else {
            next p2;
          }
        }
      }
      if (@exceed == 1) {
        say "\t$p1w\t$p2w\t@exceed";
      }
    }
  }
}
