use strict;
use warnings;

use v5.24;

use experimental 'signatures';

my %graphs;
my %digraphs;
my %trigraphs;

sub build_frequency_table($filename) {
  open(my $input, '<:encoding(utf-8)', $filename);
  my ($last, $last2);
  while(read($input, my $buffer, 1024)) {
    $buffer = lc $buffer;
    $buffer =~ s/\s+/ /g;
    $buffer =~ s/[^\x09-\x0d\x20-\x7e]//g;
    my @letters = split(//, $buffer);
    foreach my $letter (@letters) {
      $graphs{$letter}++;
      $digraphs{"$last$letter"}++ if defined $last;
      $trigraphs{"$last2$last$letter"}++ if defined $last2;
      ($last, $last2) = ($letter, $last);
    }
  }
  # foreach my $key (keys %graphs) {
  #   $graphs{$key} = log $graphs{$key};
  # }
  # foreach my $key (keys %digraphs) {
  #   $digraphs{$key} = log $digraphs{$key};
  # }
  # foreach my $key (keys %trigraphs) {
  #   $trigraphs{$key} = log $trigraphs{$key};
  # }
}

sub write_frequency($filename, $counts) {
  open(my $out, '>:encoding(ascii)', $filename);
  foreach my $key (sort keys %$counts) {
    print $out "$key: $counts->{$key}\n";
  }
}

build_frequency_table('sherlock.txt');

write_frequency('graph.txt', \%graphs);
write_frequency('digraph.txt', \%digraphs);
write_frequency('trigraph.txt', \%trigraphs);

