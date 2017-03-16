use strict;
use warnings;

my %colors = (
  'R' => 'red',
  'B' => 'blue',
  'G' => 'green',
  'Y' => 'yellow'
 );

print "<table>\n";
while(<STDIN>) {
  chomp;
  print "  <tr>\n";
  for (split) {
    @_ = split(//);
    print qq(    <td><font color="$colors{$_[0]}">$_[1]</font></td>\n);
  }
  print "  </tr>\n";
}
print "</table>\n";
