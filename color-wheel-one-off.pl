use strict;
use warnings;

use v5.20;

use experimental qw(signatures);
use Time::HiRes qw(gettimeofday tv_interval);

use List::Util qw(pairs pairmap pairgrep);
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

sub cross($la,$ra) {
  my @result;
  for my $l ($la->@*) {
    push(@result, map { join($;, $l <= $_ ? ($l, $_) : ($_, $l)) } grep { $cells[$_]{color} eq $cells[$l]{color} } $ra->@*);
  }
  return @result;
}

my $before = [gettimeofday];
if (@ARGV == 0 || $ARGV[0] eq 'd') {
  my %seen;

  my @best = ('R', ) x 50;
  sub depth($l, $r, @path) {
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
              # say "Met on turn " . scalar @path;
            }
            # push(@path, [$ln, $rn]);
            # use Data::Printer;
            # p(@path);
            return $#path;
          }
          depth($ln, $rn, @path, [$ln, $rn]);
        }
      }
    }
    return 0;
  }
  depth(0, $end, [0, $end]);
  say join(' ', map { ref ? join(',', @$_) : $_ } @best);
} elsif ($ARGV[0] eq 't') {
  my %g;
  my $goal = { name => 'goal', neighbors => [] };
  for my $l (keys @cells) {
    $g{$l,$l} = $goal;
    my $L = $cells[$l];
    for my $r ($l+1 .. $#cells) {
      next unless $cells[$r]{color} eq $L->{color};
      my $R = $cells[$r];
      my @neighbors = cross($L->{neighbors}, $R->{neighbors});
      $g{$l,$r} = { name => join('-', $l,$r), neighbors => \@neighbors };
    }
  }
  my $start = $g{0,$end};
  $start->{ancestor} = $start;
  my @path;
  my @nodes = ($start, );
  while(my $n = shift @nodes) {
    for my $neighbor ($n->{neighbors}->@*) {
      my $N = $g{$neighbor} // die $neighbor;
      next if defined $N->{ancestor};
      $N->{ancestor} = $n;
      if ($N == $goal) {
        push(@path, join('-', split($;, $neighbor)));
        my $x = $n;
        while ($x != $start) {
          push(@path, $x->{name});
          $x = $x->{ancestor};
        }
        push(@path, $x->{name});
      } else {
        push(@nodes, $N);
      }
    }
  }
  die unless $goal->{ancestor};
  say join(' ', reverse @path);
} elsif ($ARGV[0] eq 'b') {
  my @search = ([0, $end], );
  my %seen;
  $seen{0,$end} = $seen{$end,0} = '';
  my @path;
  search:
  while (my $n = shift @search) {
    my ($l, $r) = $n->@*;
    my ($L, $R) = @cells[$l, $r];
    for my $ln ($L->{neighbors}->@*) {
      my $LN = $cells[$ln];
      for my $rn ($R->{neighbors}->@*) {
        my $RN = $cells[$rn];
        if ($LN->{color} eq $RN->{color}) {
          if ($ln == $rn) {
            my @path = ($ln, );
            my $x = join($;, $l, $r);
            while ($x ne '') {
              push(@path, join('-', split($;, $x)));
              $x = $seen{$x};
            }
            say scalar @path . ': ' . join(' ', reverse @path);
            last search;
          } else {
            next if defined $seen{$ln,$rn};
            $seen{$ln,$rn} = $seen{$rn,$ln} = join($;, $l, $r);
            push(@search, [$ln, $rn]);
          }
        }
      }
    }
  }
} else {
  die "Unknown type $ARGV[0]";
}
my $elapsed = tv_interval($before, [gettimeofday]) * 1000.0;
printf "%.3f\n", $elapsed;
