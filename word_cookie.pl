use strict;
use warnings;

use v5.24;

use experimental 'signatures';

my %result;

sub add($trie, $word, $remain) {
  if (length($remain) == 0) {
    $trie->{final} = $word;
  } else {
    my $letter = substr($remain, 0, 1);
    my $rest = substr($remain, 1);
    add($trie->{$letter} //= {}, $word, $rest);
  }
}

sub find_words($trie, @letters) {
  if ($trie->{final}) {
    $result{$trie->{final}}++;
  }
  for my $i (0 .. $#letters) {
    my $new_trie = $trie->{$letters[$i]};
    if (defined $new_trie) {
      find_words($new_trie, @letters[0 .. $i - 1], @letters[$i + 1 .. $#letters]);
    }
  }
}

my $trie = {};

my @letters = map { lc } map { split(//) } @ARGV;

{
  open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    next unless length() >= 3;
    next unless length() <= @letters;
    add($trie, lc $_, lc $_);
  }
}

find_words($trie, @letters);

my @result = sort { length($a) <=> length($b) or $a cmp $b } keys %result;
say foreach (@result);
