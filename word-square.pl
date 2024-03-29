use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use List::Util qw(uniq);

my @groupings = map { lc } @ARGV;

my @letters = sort { $a cmp $ b } uniq map { split(//) } @groupings;
my $letters = join('', @letters);
my $valid = qr/^[$letters]+$/;

# start with assumption letters are uniq, it's possible to do otherwise, but we have to
# make one tr pattern per duplicate.
my %mapping;
for my $i (keys @groupings) {
  my @l = split(//, $groupings[$i]);
  for my $l (@l) {
    die if $mapping{$l};
    $mapping{$l} = $i;
  }
}

sub suniq {
  my $last = '';
  return @_[grep { my $ret_val = $last ne $_[$_]; $last = $_[$_]; $ret_val } keys @_];
}

my $from = join('', sort keys %mapping);
my $to   = join('', @mapping{sort keys %mapping});

my %words;
{
  open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    next unless length() >= 3;
    my $word = lc;
    next unless $word =~ m/$valid/;
    my $trans = $word;
    eval "\$trans =~ tr/$from/$to/";
    # say "$word $trans";
    next if $trans =~ m/(.)\1/;
    # add($trie, lc $_, lc $_);
    push($words{substr($word, 0, 1)}->@*, $word);
    # say "$word $trans";
  }
}

my %memo;
sub best($start, @so_far) {
  if (@so_far == @letters) {
    return ();
  }
  my $key = join('-', $start, @so_far);
  # say "$start: @so_far";
  if (!exists $memo{$key}) {
    my @best = ('x', ) x 100;
    my @choices;
    for my $word ($words{$start}->@*) {
      my @s = suniq(sort { $a cmp $b } @so_far, split(//, $word));
      next if @s == @so_far; # no progress
      my @additional = ($word, best(substr($word, -1, 1), @s));
      if (@additional < @best) {
        @best = @additional;
      }
    }
    # say "$key: @best";
    $memo{$key} = \@best;
  }
  return $memo{$key}->@*;
}

for my $letter (@letters) {
  say "Best starting at $letter:";
  my @best = best($letter);
  say "\t$_" foreach @best;
}
