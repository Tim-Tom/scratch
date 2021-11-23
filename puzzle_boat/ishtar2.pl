use strict;
use warnings;

use v5.24;

use File::Slurp qw(read_file write_file);

my $ishtar = read_file('ishtar2.txt');

my @targets = qw(
    SAND
    DIRT
    MICA
    SADN
    SNAD
    SNDA
    SDAN
    SDNA
    ASND
    ASDN
    ANSD
    ANDS
    ADSN
    ADNS
    NSAD
    NSDA
    NASD
    NADS
    NDSA
    NDAS
    DSAN
    DSNA
    DASN
    DANS
    DNSA
    DNAS
);

my $continue = 1;
my $pass = 1;
while ($continue) {
  $continue = 0;
  say "Pass $pass";
  my @indexes;
  for my $target (@targets) {
    my $start = 0;
    my $index;
    my $count = 0;
    while(($index = index($ishtar, $target, $start)) != -1) {
      push(@indexes, $index);
      $start = $index + 1;
      ++$count;
    }
    say "Found $count $target";
    $continue = 1 if $count;
  }
  say scalar @indexes;
  last;
  ++$pass;
}

write_file('ishtar.out', $ishtar);
