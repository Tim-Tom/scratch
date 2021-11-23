use strict;
use warnings;

use v5.20;

my @targets = @ARGV;

my %lengths = map { length $_ => 1 } @ARGV;

my %find;
for my $target (@targets) {
  my $key = join('', sort { $a cmp $b } split(//, uc $target));
  $find{$key} = 1;
  say "$target: $key";
}


open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
while(<$words>) {
  chomp;
  next unless $lengths{length $_};
  next if /\W/;
  my $key = join('', sort { $a cmp $b } split(//, uc $_));
  next unless $find{$key};
  say "$key: " . uc $_;
}
close $words;
