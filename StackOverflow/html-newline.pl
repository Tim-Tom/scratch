use strict;
use warnings;

# Default to being outside a tag
my $inside = 0;

while(my $line = <DATA>) {
  my ($open, $close) = map { rindex($line, $_) } qw(< >);
  if ($open > $close) {
    $inside = 1;
  } elsif ($open < $close) {
    $inside = 0;
  }
  if ($inside) {
    # chomp($line);
    substr($line, -1) = ' ';
  }
  print $line;
}

__DATA__
This is some text
and some more
<enclosed><a
 b
 c
> <d
 e
 f
>
<g h i



>
