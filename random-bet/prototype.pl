use v5.24;

use strict;
use warnings;

use List::Util qw(min);

use experimental 'signatures';

use Carp qw(confess);

my %memory;

my @queue;

my $max = 255;
for my $x (1 .. $max) {
  push(@queue, ["$x-$x-$x", $x, $x, $x]) if $x % 2 == 0;
  my $ymax = int($max*3 - ($x*2));
  for my $y ($x+1 .. $ymax) {
    if ($x % 2 == 0 || $y % 2 == 0) {
      push(@queue, ["$x-$x-$y", $x, $x, $y]);
    }
  }
  $ymax = int(($max*3 - $x) / 2);
  for my $y ($x+1 .. $ymax) {
    if ($x % 2 == 0 || $y % 2 == 0) {
      push(@queue, ["$x-$y-$y", $x, $y, $y]);
    }
  }
}

my $limit = 11;
my $length = 1;

my @solutions;

sub evaluate_candidate($pred, $p1, $p2, $p3) {
  if ($p1 == $p2 || $p2 == $p3) {
    return;
  }
  my $key = "$p1-$p2-$p3";
  return if exists $memory{$key};
  $memory{$key} = $pred;
  push(@solutions, $key) if $length >= $limit && $p3 <= $max;
  push(@queue, [$key, $p1, $p2, $p3]);
}
push(@queue, 'delim');

while(1) {
  my $pkg = shift (@queue);
  unless (ref $pkg) {
    print STDERR "$length: queue length is " . scalar @queue . "\n";
    ++$length;
    last if @queue == 0;
    push(@queue, 'delim');
    next;
  }
  my ($key, $p1, $p2, $p3) = @$pkg;
  # Only even numbers can be used to get to the next candidate.
  if ($p1 % 2 == 0) {
    my $p1_2 = $p1 / 2;
    evaluate_candidate($key, $p1_2, sort { $a <=> $b } $p2 + $p1_2, $p3);
    evaluate_candidate($key, $p1_2, $p2, $p3 + $p1_2);
  }
  if ($p2 % 2 == 0) {
    my $p2_2 = $p2 / 2;
    if ($p2_2 < $p1) {
      evaluate_candidate($key, $p2_2, sort { $a <=> $b} $p1 + $p2_2, $p3);
      evaluate_candidate($key, $p2_2, $p1, $p3 + $p2_2);
    } else {
      evaluate_candidate($key, $p1, $p2_2, $p3 + $p2_2);
    }
  }
  if ($p3 % 2 == 0) {
    my $p3_2 = $p3 / 2;
    if ($p3_2 < $p1) {
      evaluate_candidate($key, $p3_2, sort { $a <=> $b } $p1 + $p3_2, $p2);
      evaluate_candidate($key, $p3_2, $p1, $p2 + $p3_2);
    } elsif ($p3_2 < $p2) {
      evaluate_candidate($key, $p1, $p3_2, $p2 + $p3_2);
    } else {
      # Our number is so large, there are no bets it would have won in the previous round
    }
  }
}

say "Found " . scalar @solutions . " solutions of length $limit";
foreach my $solution (sort @solutions) {
  say "$solution";
  my $next = $solution;
  my $i = 1;
  while($next) {
    say "\t$i. $next";
    $next = $memory{$next};
    ++$i;
  }
}
