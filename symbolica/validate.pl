use strict;
use warnings;

use v5.24;

my (@original, @solution);

my %symbols;
my $bad = 0;

sub cmp_tile {
  my ($a, $b) = @_;
  my $diff = (substr($a, 0, 1) eq substr($b, 0, 1)) + (substr($a, 1, 1) eq substr($b, 1, 1));
  if ($diff != 1) {
    say "$a found adjacent to $b";
    $bad = 1;
  }
};

while (<DATA>) {
  chomp;
  last unless /\S/;
  my @row = split;
  ++$symbols{$_}{orig} foreach (@row);
  push(@original, \@row);
}

while (<DATA>) {
  chomp;
  last unless /\S/;
  my @row = split;
  ++$symbols{$_}{soln} foreach (@row);
  push(@solution, \@row);
}

while (my ($symbol, $val) = each %symbols) {
  $val->{$_} //= 0 foreach (qw(orig soln));
  if ($val->{orig} != $val->{soln}) {
    say "Count of $symbol is incorrect: $val->{orig} vs. $val->{soln}";
    $bad = 1;
  }
}

if (@original != @solution) {
  say "Row count incorrect";
  $bad = 1;
} else {
  for my $i (0 .. $#original) {
    my @o = @{$original[$i]};
    my @s = @{$solution[$i]};
     if (@o != @s) {
      say "Column count of row $i is incorrect";
      $bad = 1;
    }
   }
}

for my $ri (0 .. $#solution) {
  my @s = @{$solution[$ri]};
  for my $ci (0 .. $#s) {
    cmp_tile($s[$ci], $s[$ci+1]) unless $ci == $#s;
    cmp_tile($s[$ci], $solution[$ri+1][$ci]) unless $ri == $#solution;
  }
}

exit $bad;

=pod

Ra Bb Ba Bb Bb
Ga Ba Yc Ba Gb
Yb Yb Bb Bc Ya
Ba Ra Rb Bb Bc
Rb Ya Yb Ra Ba

Ra Rb Gb Bb Bb
Ga Gb Gc Gb Gb
Ga Ga Gb Rb Gb
Ba Ra Rb Bb Bb
Ra Ya Yb Bb Ba

=cut

__DATA__
Ra Rb Ra
Rb Ra Rb
Ra Rb Ra

Ra Rc Ra
Rb Ra Ba
Ra Rb Ra
