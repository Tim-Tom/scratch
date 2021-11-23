use strict;
use warnings;

use List::Util qw(uniq);

use v5.28;

my @clues = (
  ['About', qw(C E G I O)],
  # 2. Affluent London area (GIOST)
  #3. Fried side (2 wds) (GIORS)
  #4. Instrument (2 wds) (CEFORR)
  #5. Large instrument (2 wds) (AAGIOR)
  ['Laxness', qw(E E E G I L)],
  ['Loose', qw(C D E O U)],
  ['Relaxed', qw(A C H L O)],
  #9. Second wife of Henry VIII (2 wds) (AELOY)
  #10. Serious (hyph.) (EOOS)
  #11. Terminator target (2 wds) (CHOOO)
);

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
# open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
while(<$words>) {
  chomp;
  next unless length() == 10;
  next if /[^A-Za-z]/;
  push(@words, uc $_);
}
close $words;

@words = sort { $a cmp $b } uniq @words;

my %sorted;
for my $word (@words) {
  my $key = join('', sort { $a cmp $b } split(//, $word));
  push($sorted{$key}->@*, $word);
}

for my $clue (@clues) {
  my ($text, @letters) = $clue->@*;
  my @found;
 word:
  for my $key (keys %sorted) {
    my $start = 0;
    for my $letter (@letters) {
      my $next = index($key, $letter, $start);
      next word if ($next == -1);
      $start = $next + 1;
    }
    push(@found, $sorted{$key}->@*);
  }
  @found = ('(none)') unless @found;
  @found = sort { $a cmp $b } @found;
  say join("\t", $text, @found);
}
