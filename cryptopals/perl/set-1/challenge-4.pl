use strict;
use warnings;

use v5.24;

use experimental 'signatures';

my %graphs;
my %digraphs;
my %trigraphs;

sub build_frequency_table($filename) {
  open(my $input, '<:encoding(utf-8)', $filename);
  my ($last, $last2) = (' ', ' ');
  while(read($input, my $buffer, 1024)) {
    $buffer = lc $buffer;
    $buffer =~ s/\s+/ /g;
    $buffer =~ s/[^\x09-\x0d\x20-\x7e]//g;
    my @letters = split(//, $buffer);
    foreach my $letter (@letters) {
      $graphs{$letter}++;
      $digraphs{"$last$letter"}++;
      $trigraphs{"$last2$last$letter"}++;
      ($last, $last2) = ($letter, $last);
    }
  }
  foreach my $key (keys %graphs) {
    $graphs{$key} = log $graphs{$key};
  }
  foreach my $key (keys %digraphs) {
    $digraphs{$key} = log $digraphs{$key};
  }
  foreach my $key (keys %trigraphs) {
    $trigraphs{$key} = log $trigraphs{$key};
  }
}

sub get_frequency($str) {
  no warnings 'uninitialized';
  my ($last, $last2) = (' ', ' ');
  my $candidate = lc $str;
  $candidate =~ s/\s+/ /g;
  my ($g, $d, $t);
  foreach my $letter (split(//, $candidate)) {
    $g += $graphs{$letter};
    $d += $digraphs{$last . $letter};
    $t += $trigraphs{$last2 . $last . $letter};
    ($last, $last2) = ($letter, $last);
  }
  return 30*$t + 5*$d + $g;
}

build_frequency_table('../../sherlock.txt');

my @keys = ('a' .. 'z', 'A' .. 'Z', '0' .. '9');
my @possible;

{
  open(my $fh, '<:encoding(ascii)', '../../data/s1-c4.txt');
  while(<$fh>) {
    chomp;
    my $input = pack('H*', $_);
    foreach my $key (@keys) {
      my $padded = $key x length($input);
      my $decoded = $input ^ $padded;
      next if $decoded =~ /[^\x09-\x0d\x20-\x7e]/;
      push(@possible, [$key, $decoded, get_frequency($decoded)]);
    }
  }
}

@possible = sort { $b->[2] <=> $a->[2] } @possible;

foreach my $possible (@possible[0, ]) {
  my ($key, $str, $freq) = @$possible;
  printf '%s: [%6.2f] %s'."\n", $key, $freq, $str;
}
