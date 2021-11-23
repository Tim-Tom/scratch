use strict;
use warnings;

use v5.26;

use List::Util qw(uniq);

my @phrases = @ARGV;

my %lengths = map { length $_ => length $_ } @phrases;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless $lengths{length $_};
  push($words[length()]->@*, uc $_);
}
close $words;

@words = map { [sort { $a cmp $b } uniq $_->@*] } @words;

# Uncomment for lax mode
my %letters;

if ($ENV{LAX}) {
  %letters = (
    A => [qw(Q W A Z)],
    S => [qw(W E S X Z)],
    D => [qw(E R D X C)],
    F => [qw(R T F C V)],
    G => [qw(R T Y G V B)],
    H => [qw(T Y U H B N)],
    J => [qw(U I J N M)],
    K => [qw(I O K M)],
    L => [qw(O P L)],
  ';' => [qw(P)],
 );
} else {
  %letters = (
    A => [qw(Q A Z)],
    S => [qw(W S X)],
    D => [qw(E D C)],
    F => [qw(R F V)],
    G => [qw(T G B)],
    H => [qw(Y H N)],
    J => [qw(U J M)],
    K => [qw(I K)],
    L => [qw(O L)],
  ';' => [qw(P)],
   );
}

for my $phrase (@phrases) {
  my @phrase = map { $letters{uc $_} or die "$_ not in mapping" } split(//, $phrase);
  my $regex = '^' . join('', map { '[' . join('', @$_) . ']' } @phrase) . '$';
  $regex = qr/$regex/;
  my @matches = grep { /$regex/ } $words[length $phrase]->@*;
  say "$phrase: @matches";
}
