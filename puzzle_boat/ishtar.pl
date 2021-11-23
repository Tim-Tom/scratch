use strict;
use warnings;

use v5.24;

use File::Slurp qw(read_file write_file);

my $ishtar = read_file('ishtar.txt');

my @targets = qw(
    SAND
    DIRT
    MICA
    SAND
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
  for my $target (@targets) {
    my $count = ($ishtar =~ s/$target//g) || 0;
    say "Removed $count units of $target";
    $continue = 1 if $count;
  }
  ++$pass;
}

write_file('ishtar.out', $ishtar);
