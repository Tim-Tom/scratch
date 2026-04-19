use strict;
use warnings;

use v5.20;

use experimental qw(signatures);

use List::MoreUtils qw(first_index);

my @cells = qw(B R Y B R B Y R Y R* B Y R B Y B Y R B);

my $branch = first_index { $_ eq 'R*' } @cells;

die unless $branch >= 0;

$cells[$branch] = 'R';

$cells[$_] = {
  index => $_,
  neighbors => [$_-1, $_+1],
  color => $cells[$_]
 } for keys @cells;

$cells[0]{neighbors} = [$cells[0]{neighbors}[1]];
$cells[-1]{neighbors} = [$cells[-1]{neighbors}[0]];

my $end = $#cells;

push($cells[$branch]{neighbors}->@*, scalar @cells);
push(@cells, { index => scalar @cells, color => 'B', neighbors => [$branch, @cells + 1, 23] });
push(@cells, { index => scalar @cells, color => 'Y', neighbors => [$#cells, @cells + 1] });
push(@cells, { index => scalar @cells, color => 'B', neighbors => [$#cells, @cells + 1] });
push(@cells, { index => scalar @cells, color => 'Y', neighbors => [$#cells, @cells + 1] });
push(@cells, { index => scalar @cells, color => 'R', neighbors => [$#cells, 19] });

my ($l, $r) = (0, $end);

my %seen;

my %memo;
my $depth = 1;
sub check($l, $r) {
  return [0, []] if $l == $r;
  if (!exists $memo{$l,$r}) {
    say(('-' x $depth) . "check[$l,$r]...");
    $memo{$l,$r} = [1e100, ['error']];
    my ($L, $R) = @cells[$l, $r];
    my $min = [1e100, ['no-match']];
    for my $ln ($L->{neighbors}->@*) {
      my $LN = $cells[$ln];
      for my $rn ($R->{neighbors}->@*) {
        my $RN = $cells[$rn];
        if ($LN->{color} eq $RN->{color}) {
          ++$depth;
          my $result = check($ln, $rn);
          --$depth;
          say(('-' x $depth) . "check[$l,$r]=" . $result->[0] . "+1?");
          $min = [$result->[0]+1, [[$ln, $rn], $result->[1]->@*]] if $result->[0] + 1 < $min->[0];
        }
      }
    }
    $memo{$l,$r} = $min if $min->[0] < 1e100;
  }
  say(('-' x $depth) . "check[$l,$r]=" . $memo{$l,$r}[0]);
  return $memo{$l,$r};
}

my $result = check($l, $r);

use Data::Printer;
p($result);
