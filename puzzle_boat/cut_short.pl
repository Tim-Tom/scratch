use strict;
use warnings;

use List::Util qw(sum uniq);

use v5.20;

my @clues = (
  [[qw(AS? BE? IL? RA? VA? RY)], [7, 10]],
  [[qw(AN? DU? ER? NE? NE? OM? OT? TY? ER)], [5, 7, 3, 11]],
  [[qw(AC? AD? NT? TE? US? YP)], [8, 10]],
  [[qw(AR? CO? IO? LY? RE? RO? SC)], [9, 8, 4]],
  [[qw(DE? EY? OU? E)], [5, 5]],
  [[qw(KU? LA? RA)], [5, 4]],
  [[qw(CA? CI? GO? MI? OD? OS? PL? RE? AS)], [7, 7, 4, 8]],
  [[qw(AR? MI? PO? TH? S)], [6, 7]],
  [[qw(AR? HO? IN? LA? MO? RY)], [3, 6, 5, 4]],
  [[qw(FA? HA? IT? TH)], [5, 7]],
  [[qw(AT? CI? IS? LE? UD? VE? L)], [11, 8]],
  [[qw(AR? EL? KT? LO? NG? OK? OS)], [4, 3, 7, 7]],
  [[qw(ET? IM? MO? NT? PH? RY? SE? C)], [11, 11]],
  [[qw(AL? DO? HE? IN? E)], [5, 8]],
 );

for my $clue (@clues) {
  my ($letters, $lengths) = $clue->@*;
  my $llen = sum map { length } $letters->@*;
  my $ilen = sum $lengths->@*;
  say "$llen $ilen";
}

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
# open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
while(<$words>) {
  chomp;
  next unless length() == 3;
  push(@words, uc $_);
}
close $words;

@words = sort { $a cmp $b } uniq @words;

my @choices ([], [], [], [qw(TH)]);
for my $word (@words) {
  if ($word =~ /^FA.$/) {
    push($choices[0]->@*, $word);
  } elsif ($word =~ /^HA.$/) {
    push($choices[1]->@*, $word);
  } elsif ($word =~ (^IT.$/) {
    push($choices[2]->@*, $word);
  }
}

open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
# open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
while(<$words>) {
  chomp;
  next unless length() == 5 || length() == 7;
}
close $words;
