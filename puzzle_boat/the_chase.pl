use strict;
use warnings;

use v5.28;

use Data::Printer;

use List::Util qw(uniq);

use feature qw(postderef);

use experimental qw(signatures);

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  push(@words, uc $_);
}
close $words;

@words = uniq sort { $a cmp $b } @words;

my @people = (
  'Frank Abagnale Jr',
  'Scarlet Pimpernel',
  'Richard Kimble',
  'Jean Valjean',
  'Charles Lytton',
  'Al Capone',
  'Professor Moriarty',
  'Ferris Beuller'
 );

my @clues = ('Albania', 'Anvil', 'arc', 'ardor', 'arts', 'bean', 'button', 'clone', 'fermi', 'flan', 'lear', 'lich', 'more', 'mule', 'secret', 'trim', 'visor', 'ye');

sub count_word($word) {
  my @letters = grep { /[A-Z]/ } split(//, uc $word);
  my %count;
  $count{$_}++ foreach @letters;
  return %count;
}

my @clues2 = map { { count_word($_), }; } @clues;

for my $person (@people) {
  my %person = count_word($person);

  my @matches;
 clue:
  for my $i (keys @clues) {
    my $clue = $clues[$i];;
    my %clue = $clues2[$i]->%*;
    for my $letter (keys %clue) {
      next clue if (($person{$letter} // 0) < $clue{$letter});
    }
    push(@matches, $clue);
  }
  say join("\t", $person, @matches);
}
