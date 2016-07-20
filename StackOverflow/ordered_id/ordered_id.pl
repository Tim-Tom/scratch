# http://stackoverflow.com/questions/38373782/perl-remove-identical-id-and-similar-id-maintain-the-array-order-and-print-them

use strict;
use warnings;

# List::Util also exports uniq, but the stock one usually doesn't so
# you may still need to define it if you don't want to upgrade to the
# newest version of the library for some reason.
use List::Util qw(first uniq);

my ($ipFilename, $orderFilename) = @ARGV;

open(my $ip, '<', $ipFilename) or die "Unable to open $ipFilename for read: $!";
my @orderKeys = do {
  open(my $order, '<', $orderFilename) or die "Unable to open $orderFilename for read: $!";
  <$order>;
};
chomp(@orderKeys);

local $" = "\t";

print "Column1\t@orderKeys\n";

while (my $line = <$ip>) {
  chomp($line);
  my ($header, $matches) = split(/\s*\:\s*/, $line, 2);
  my @matches = ($header, split(/\s*\|\s*/, $matches));
  # I don't use or need this, but if you need it for some other purpose, it can still work.
  my @uniqueMatches = uniq @matches;
  # Get the first item from each category that starts with the given key.
  my @slots = map { my $key = $_; first { /^\Q$key\E/ } @matches } @orderKeys;
  no warnings 'uninitialized';
  print "$header\t@slots\n";
}
