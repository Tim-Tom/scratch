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

my @best = ('R', ) x 50;
sub check($l, $r, @path) {
  return 0 if @path > @best;
  return 0 if exists $seen{$l,$r} && $seen{$l,$r} <= @path;
  $seen{$l,$r} = @path;
  my ($L, $R) = @cells[$l, $r];
  for my $ln ($L->{neighbors}->@*) {
    my $LN = $cells[$ln];
    for my $rn ($R->{neighbors}->@*) {
      my $RN = $cells[$rn];
      if ($LN->{color} eq $RN->{color}) {
        if ($ln == $rn) {
          if (@path <= @best) {
            @best = (@path, $ln);
            say "Met on turn " . scalar @path;
          }
          # push(@path, [$ln, $rn]);
          # use Data::Printer;
          # p(@path);
          return $#path;
        }
        check($ln, $rn, @path, [$ln, $rn]);
      }
    }
  }
  return 0;
}

check($l, $r, [$l, $r]);

use Data::Printer;
p(@best);
