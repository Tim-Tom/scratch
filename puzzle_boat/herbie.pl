use strict;
use warnings;

use List::Util qw(sum uniq);

use v5.20;

my ($original_length, $letters) = @ARGV;

my @words;
open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless length() == $original_length;
  push(@words, uc $_);
}
close $words;

my $dots = $original_length - length $letters;

my $adder = '.?' x $dots;

my @letters = split(//, $letters);
my @parts = map { "$adder$_" } (@letters, '');
my $regex = join('', @parts);
$regex = qr/$regex/;

for my $word (@words) {
  say $word if $word =~ /$regex/;
}
