use strict;
use warnings;

use v5.24;

use experimental qw(signatures);

my $size = $ARGV[0] // 3;
my $sm = $size - 1;

my @flat_idx = 0 .. ($size**2 - 1);

my @idx = ();

for my $i (0 .. ($size - 1)) {
  $idx[$i] = [@flat_idx[$size*$i .. ($size*($i+1)-1)]];
}

use Data::Printer;

my @symmetry = (
  sub($x, $y) { return (    $x,       $y); },
  sub($x, $y) { return (    $x, $sm - $y); },
  sub($x, $y) { return ($sm-$x,       $y); },
  sub($x, $y) { return ($sm-$x, $sm - $y); },
  sub($x, $y) { return (    $y,       $x); },
  sub($x, $y) { return (    $y, $sm - $x); },
  sub($x, $y) { return ($sm-$y,       $x); },
  sub($x, $y) { return ($sm-$y, $sm - $x); },
 );

my $fmt = '%' . (scalar length $flat_idx[$#flat_idx]) . 'd';

for my $symmetry (@symmetry) {
  my @flat;
  for my $y (0 .. $sm) {
    for my $x (0 .. $sm) {
      my ($rx, $ry) = $symmetry->($x,$y);
      push(@flat, $idx[$ry][$rx]);
    }
  }
  say join(', ', map { sprintf $fmt, $_ } @flat);
}
