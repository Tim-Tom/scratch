use strict;
use warnings;

use v5.28;

use Data::Printer;

use List::Util qw(uniq);

use feature qw(postderef);

use experimental qw(signatures);

my @targets = (
  [qw(L 3 R)],
  [qw(B 5 E)],
  [qw(E 5 S)],
  # [qw(B 10 E)],
  [qw(R 5 A)],
  # [qw(S 14 N)],
  # [qw(E 9 E)], Two clues skip for now
  [qw(B 5 E)],
  [qw(Z 2 A)],
  # [qw(S 7 N)],
  [qw(J 2 A)],
  [qw(D 3 N)],
  # [qw(B 7 N)],
  [qw(K 2 Y)],
  [qw(P 1 A)],
  # [qw(L 8 I)],
  [qw(H 3 A)],
 );

my %lengths = map { $_ => $_ } map { 2 + $_->[1] } @targets;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless $lengths{length $_};
  $words[length $_]{uc $_} = 1;
}
close $words;

my @grid = (
  [qw(P D J I B U R G N E K)],
  [qw(A I P M I T K A M Y E)],
  [qw(N H S E E W S U R C A)],
  [qw(A S A N G R D A N I R)],
  [qw(B E M A R K G A V D A)],
  [qw(A L R S I A F A S O I)],
  [qw(B L T O W N Q R U R S)],
  [qw(W E U R N E S I A U T)],
  [qw(E B G N A T S C Z A O)],
  [qw(R P A R A G W A I T B)],
  [qw(B E L D O R A C L U B)]
 );

my $height = @grid;
my $width = $grid[0]->@*;

my %location;
for my $row (keys @grid) {
  for my $col (keys $grid[$row]->@*) {
    my $letter = $grid[$row][$col];
    push($location{$letter}->@*, [$row, $col]);
  }
}

sub move($path, $row, $col, $dy, $dx) {
  my $new_row = $row + $dy;
  my $new_col = $col + $dx;
  return undef if $new_row < 0 || $new_row >= $height;
  return undef if $new_col < 0 || $new_col >= $width;
  return [$path . $grid[$new_row][$new_col], $new_row, $new_col];
}

my @actions = ([ 0,  1], [ 0, -1],
               [ 1,  0], [-1,  0],
               [ 1,  1], [ 1, -1],
               [-1,  1], [-1, -1]);

for my $target (@targets) {
  my ($start, $distance, $end) = $target->@*;
  my $words = $words[2 + $distance];
  my @positions = map { [$start, $_->@*] } $location{$start}->@*;
  for my $i (0 .. $distance) {
    my @next_positions = ();
    for my $position (@positions) {
      push(@next_positions, grep { defined } map { move($position->@*, $_->@*) } @actions);
    }
    @positions = @next_positions;
    @next_positions = ();
  }
  my @total_paths = map { $_->[0] } grep { $grid[$_->[1]][$_->[2]] eq $end } @positions;
  my @valid_paths = grep { $words->{$_} } @total_paths;
  my @uniq = uniq @valid_paths;
  my $total = @total_paths;
  my $valid = @valid_paths;
  my $uniq = join("\t", @uniq);
  say "$start$distance$end\t$total\t$valid\t$uniq";
}
