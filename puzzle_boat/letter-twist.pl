use strict;
use warnings;

use v5.20;

while(<DATA>) {
  chomp;
  my ($title, $year, $direction) = split(/\t/, $_);
  my $word = $title;
  $word =~ s/\W//g;
  my $amount = 1 + ($year % length $word);
  if ($direction eq 'R') {
    $amount = -$amount;
  }
  say "$title\[$amount]: " . substr($word, $amount, 1);
}
chomp(my @data = <DATA>);



__DATA__
Creepshow	1982	L
Alita: Battle Angel	2019	L
Who Framed Roger Rabbit	1988	L
Singing in the Rain	1952	R
The Truth About Cats and Dogs	1996	R
Gunfight at the OK Corral	1957	L
Beauty and the Beast	1991	R
Silver Linings Playbook	2012	L
Regarding Henry	1991	R
Kubo and the Two Strings	2016	R
