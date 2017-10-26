use v5.24;

use strict;
use warnings;

use experimental qw(signatures);

use List::Util qw(uniq);

=pod

Simple script that just makes sure that a given piece of text doesn't contain any words
that the decipherer will have trouble with. This obviously assumes you have the text
already decoded, which doesn't help if you find one of these in the wild and someone had a
typo, but it finds any typos you made when creating a test text.

=cut

my %words;

{
  open(my $words, '<:encoding(utf-8)', 'words.txt') or die;
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    push(@{$words{length $_}}, lc);
  }
}

my @words = map { lc } map { chomp; split } <ARGV>;

my @possible;

my $any_not_found;

foreach my $word (@words) {
  my $words = $words{length $word};
  my $found = grep { $_ eq $word } @{$words};
  $any_not_found ||= !$found;
  $found = $found ? 'found' : 'not found';
  say "$word: $found";
}

if ($any_not_found) {
  print STDERR "There were unknown words in the text\n";
  exit 1;
}
