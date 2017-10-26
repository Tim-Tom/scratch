use strict;
use warnings;

my @stuff = map { [ reverse @$_ ] } @{ eval(scalar <STDIN>) };

my @locations = (0,1,2,4,8,5,3,6,7);

my $solution = 0;
foreach my $arr (@stuff) {
  print "--- Solution " . ++$solution . " ---\n";
  my @vals = @$arr;
  # my ($c1, $g1, $c2, $g2) = map { pop @vals } (0 .. 3);
  my @a = ' ' x @locations;
  for my $i (0 .. $#vals) {
    $a[$locations[$i]] = $vals[$i];
  }
  # printf "%2d : %2d | %2d : %2d\n", $c1, $g1, $c2, $g2;
  for(my $i = 0; $i < @a; $i += 3) {
    printf "%2s %2s %2s\n", @a[$i .. $i + 2];
  }
}
