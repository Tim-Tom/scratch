use strict;
use warnings;

use v5.20;

my @people = map { lc } qw(
  Ella
  Ana
  Joe
  Bert
  Sal
  Archie

  Aster
  Chet
  Thora
  Herb
  Dave
  Ian

  Pete
  Dana
  Tina
  Otto
  Rick
  Grant

       Carl
Evan
Evita
Cate
Gina       
losure
);


my %words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless length >= 3;
  next if /\W/;
  $words{lc $_} = 1;
}
close $words;

for my $word (map { lc } @ARGV) {
  for my $person (@people) {
    if ($word =~ /$person/) {
      for my $replace (@people) {
        my $new_word = $word;
        $new_word =~ s/$person/$replace/;
        my $real = $words{$new_word} || 0;
        say "$new_word : $real";
      }
    }
  }
}
