use strict;
use warnings;

use v5.24;

my @data;
while(<DATA>) {
  chomp;
  my ($phrase, $left, $unused, $right) = split(/\t/);
  $phrase =~ s/ //g;
  $phrase = uc $phrase;
  my @left = sort { $a cmp $b } split(//, $phrase);
  my @right = sort { $a cmp $b } split(//, "$left$right");
  my $l = shift @left;
  my $r = shift @right;
  my $intersect = '';
  while($l && $r) {
    if ($l lt $r) {
      warn "$phrase: $l";
      $l = shift @left;
    } elsif ($l gt $r) {
      $intersect .= $r;
      $r = shift @right;
    } else {
      $l = shift @left;
      $r = shift @right;
    }
  }
  say "$left\t$right\t$intersect";
}

__DATA__
Novelty Alumni	MOUNTAIN	V5	VALLEY
Tibetan Rock	BROKEN	I5	INTACT
Add Eleven Branches	BEARDED	C4 - S5	CLEANSHAVEN
Throne Glue	GENTLE	R4	ROUGH
Touched Trouts	DETOUR	S7	SHORTCUT
Dilating Goal	ANALOG	D6	DIGITAL
Almost Cry	CALM	S5	STORMY
Bared Deltoid	BROAD	D7	DETAILED
Ran to School	CHAOS	C6	CONTROL
Rod Academy	COMEDY	D4	DRAMA
Rat Alley	EARLY	L3	LATE
Callous Farm	CASUAL	F5	FORMAL
Train Cluster	INSTRUCT	L4	LEARN
They Wanted Print	HANDWRITTEN	T4	TYPED
Inculcate Cone	CANCEL	C7	CONTINUE
Innocent Malaria	AMNIOTIC	N7	NEONATAL
Nebula Asteroid	LEAD	S10	SUBORDINATE
Catfish Police	HOSTILE	P6	PACIFIC
Natural Speech	PURCHASE	R5	RESALE
Flew Carpeted	FLAWED	P6	PERFECT
From Direct Entries	FREEDOM	R10	RESTRICTION
Special Part	PAPER	P6	PLASTIC
Blues Promotion	PROBLEM	S7	SOLUTION
Matronly Adoption	MANDATORY	O7	OPTIONAL
Alien Troubadour	LINEAR	R9	ROUNDABOUT
Slithery Life	FILTHY	S6	STERILE
