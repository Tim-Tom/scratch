use strict;
use warnings;

use v5.24;

my $re = '';
while(<DATA>) {
  chomp;
  my ($path, $team) = split(/\s*->\s*/);
  $path = quotemeta $path;
  $path =~ s!\\\*\\\*!.*!g;
  $path =~ s!\\\*![^/]*!g;
  $team =~ s/\\/\\\\/g;
  $team =~ s/'/\\'/g;
  $path .= qq<\\Z(?{'$team'})>;
  if ($re) {
    $re .= "|$path";
  } else {
    $re = "\\A(?:$path";
  }
}
$re .= ')';
close *DATA;

$re = do {
  use re 'eval';
  qr/$re/;
};

while(my $path = <STDIN>) {
  chomp $path;
  if ($path =~ /$re/) {
    say "$path belongs to $^R";
  } else {
    say "$path doesn't belong to any team";
  }
}

__DATA__
src/A/** -> A
inc/A/** -> A
inc/A*/B*/C* -> D
inc/foo.h -> A
src/B/** -> B
inc/B/** -> B
src/C/** -> C
inc/C/** -> C
build/** -> D
makefile -> D
