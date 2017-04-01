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
  # Not sure if this one is neccessary
  push(@queue, ["$x-$x-$x", $x, $x, $x]) if $x % 2 == 0;
  for my $y ($x+1 .. $max) {
    if ($x % 2 == 0 || $y % 2 == 0) {
      push(@queue, ["$x-$x-$y", $x, $x, $y]);
      push(@queue, ["$x-$y-$y", $x, $y, $y]);
    }
  }
}

my $length = 1;

sub evaluate_candidate($pred, $p1, $p2, $p3) {
  if ($p1 == $p2 || $p2 == $p3) {
    return;
  }
  my $key = "$p1-$p2-$p3";
  return if exists $memory{$key};
  $memory{$key} = $pred;
  if ($length == 11) {
    return if $p3 > 255;
    say "$key produces a valid chain of length 11";
    while($key) {
      say "$key";
      $key = $memory{$key};
    }
    exit;
  }
  push(@queue, [$key, $p1, $p2, $p3]);
}
push(@queue, 'delim');

while(1) {
  my $pkg = shift (@queue);
  unless (ref $pkg) {
    say "$length: queue length is " . scalar @queue;
    last if ++$length == 13;
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
    }
  }
}
