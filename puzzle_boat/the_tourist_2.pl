use strict;
use warnings;

use v5.28;

use feature qw(postderef);

use experimental qw(signatures);

my @data = (
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

my %seen;

sub work($x, $y, $dx, $dy, $tag) {
  my $word = '';
  my $count = 1;
  my $full_tag = join(':', ($x+1, $y+1, $tag));
  do {
    $word .= $data[$y][$x];
    push($seen{$word}->@*, "$full_tag:$count");
    $x += $dx;
    $y += $dy;
    ++$count;
  } while ($y >= 0 && $y < @data && $x >= 0 && $x < $data[$y]->@*);
  return;
}

for my $row (keys @data) {
  for my $col (keys $data[$row]->@*) {
    # left
    work($col, $row, -1, 0, 'L');
    # right
    work($col, $row, 1, 0, 'R');
    # up
    work($col, $row, 0, -1, 'U');
    # down
    work($col, $row, 0, 1, 'D');
    # DR
    work($col, $row, 1, 1, 'DR');
    # UR
    work($col, $row, 1, -1, 'UR');
    # DL
    work($col, $row, -1, 1, 'DL');
    # UL
    work($col, $row, -1, -1, 'UL');
  }
}

open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
# open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
while(<$words>) {
  chomp;
  next unless length() >= 3;
  next unless $seen{uc $_};
  say join("\t", $_, $seen{uc $_}->@*);
}
