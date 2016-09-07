use strict;
use warnings;

my $buf = '\0'x1024;
foreach my $filename (@ARGV) {
  open(my $in, '<:raw', $filename) or die "Unable to open '$filename' for read: $!";
  while(read($in, $buf, 1024)) {
    if ($buf =~ /([\x80-\xFF])/) {
      print $filename . ": " . (ord $1) . "\n";
      last;
    }
  }
}
