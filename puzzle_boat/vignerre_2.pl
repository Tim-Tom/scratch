use strict;
use warnings;

use utf8;

use v5.20;

use List::Util qw(uniq);

my $full_clue = 'CWZE 1: CZSGRLYULLYIOYX ON XDPY HBAU! FWC QEICJVL';

die "script alphabet clue" unless @ARGV == 2;

my ($alphabet, $clue) = @ARGV;

my @alphabet = split(//, $alphabet);

die "Alphabet should be 26 letters, not @{[scalar @alphabet]}" unless @alphabet == 26;
for my $letter ('A' .. 'Z') {
  die "Alphabet should contain $letter" unless grep { $_ eq $letter } @alphabet;
}

my %cipher;
{
  my @letters = @alphabet;
  for my $l (@alphabet) {
    $cipher{$l} = {map { $letters[$_] => $alphabet[$_] } keys @letters};
    # $cipher{$l} = {map { $alphabet[$_] => $letters[$_] } keys @letters};
    @letters = (@letters[1..$#letters], $letters[0]);
  }
}
use Data::Printer;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless length >= 4;
  next if /\W/;
  push(@words, uc $_);
}
close $words;
# except for alfa, they are probably already in the list, but it doesn't hurt to be sure
push(@words, qw(ALFA BRAVO CHARLIE DELTA ECHO FOXTROT GOLF HOTEL INDIA JULIETT KILO LIMA MIKE NOVEMBER OSCAR PAPA QUEBEC ROMEO SIERRA TANGO UNIFORM VICTOR WHISKEY XRAY YANKEE ZULU));
push(@words, qw(AMY BASIL CLARA DESMOND ERNEST FANNY GEORGE HECTOR IDA JAMES KATE LEO MAUD NEVILLE OLIVE PRUE QUENTIN RHODA SUSAN TITUS UNA VICTOR WINNIE XERXES YORICK ZILLAH));
@words = sort {$a cmp $b } uniq @words;

my %words = map { $_ => 1 } @words;

say "@{[scalar @words]} words being tested";

=pod

my $book = 'THEGASHLYCRUMBTINIESAVERYGOREYALPHABETBOOK';

@words = ();
for my $l (1 .. length $book) {
  for my $start (0 .. $l - 1) {
    push(@words, substr($book, $start, $l - $start));
    say $words[$#words];
  }
}

=cut

for my $word (@words) {
  my @key = split(//, $word);
  my $result = '';
  # Since we are looking at an arbitrary word, try every offset of the keys.
  for my $start_index (keys @key) {
    my $index = $start_index;
    $result = '';
    for my $letter (split(//, $clue)) {
      my $trans = $cipher{$key[$index]}{$letter};
      if (defined $trans) {
        $result .=  $trans;
        $index = ($index + 1) % @key;
      } else {
        $result .= $letter;
      }
    }
    if ($words{$result}) {
      say "$word: $start_index : $result";
    }
  }
}
