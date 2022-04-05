use strict;
use warnings;

use v5.20;

use experimental qw(signatures postderef);

my @operations = (
  # Name, should we swap operands so that a >= b instead of a <= b, does the operation make sense, perform operation.
  ['+', 0, sub($a,$b) { 1 }, sub($a,$b) { $a + $b }],
  ['-', 1, sub($a,$b) { $a > $b }, sub($a,$b) { $a - $b }],
  ['*', 0, sub($a,$b) { 1 }, sub($a,$b) { $a * $b }],
  ['/', 1, sub($a,$b) { $a % $b == 0 }, sub($a,$b) { $a / $b }],
);

my ($target, @parts) = @ARGV;

# Each search candidate is a path of how we got here and the set of tiles (sorted).
my @search = (['', sort { $a <=> $b } @parts]);

# Compares two elements and returns whether they are different
# $a can be undefined, $b cannot.
sub diff($a, $b) {
  return (!defined $a) || cmp_search($a,$b) != 0;
}

# Compares two candidates tile by tile.
sub cmp_search($a,$b) {
  die unless $a->@* == $b->@*;
  for my $i (1 .. $#{$a}) {
    my $cmp = $a->[$i] <=> $b->[$i];
    return $cmp if $cmp;
  }
  return 0;
}

# Search up to a maximum depth of 5, stop the search after any given depth if we found candidates.
for my $depth (1 .. 5) {
  my $found = 0;
  my @next;
  for my $candidate (@search) {
    my ($path, @candidate) = $candidate->@*;
    for my $i (0 .. $#candidate) {
      for my $j ($i+1 .. $#candidate) {
        for my $op (@operations) {
          my ($str, $swap, $can, $do) = $op->@*;
          my ($l, $r) = @candidate[$i,$j];
          ($l, $r) = ($r, $l) if $swap;
          if ($can->($l,$r)) {
            my $result = $do->($l,$r);
            # Align text, mainly for the search candidate sort, but it also looks a little neater on output.
            my $key = sprintf('%3d %s %3d = %3d', $l, $str, $r, $result);
            my $path = (length $path ? "$path, $key" : $key);
            # Found a match!
            if ($result == $target) {
              $found = 1;
              say $path;
            } else {
              # All the candidate tiles except the two we wanted plus our new result
              my @new = sort { $a <=> $b } @candidate[0 .. $i-1], @candidate[$i+1 .. $j-1], @candidate[$j+1 .. $#candidate], $result;
              push(@next, [$path, @new]);
            }
          }
        }
      }
    }
  }
  last if $found;
  # Sort the search candidates such that any with the same set of tiles are next to each other.
  # If there are multiple sort on the path as an arbitrary tie-breaker.
  @search = sort { cmp_search($a,$b) || $a->[0] cmp $b->[0] } @next;
  my $last = undef;
  # Skip any candidates that have the same set of tiles as the previous one now that we've sorted.
  @search = grep { diff($last, $_) && ($last = $_) } @search;
}
