# perl -ne 's/[^a-zA-Z]//g;print uc . "\n" if length == 5' /usr/share/dict/american-english  | sort | uniq | wc -l
use strict;
use warnings;

use v5.26;

use experimental qw(signatures postderef);

use List::Util qw(first pairs unpairs);
use Sereal;

my $length = 5;
my $lm = $length - 1;


my (@words, @similarity, $decision_tree);

my $encoder = Sereal::get_sereal_encoder({compress => Sereal::Encoder::SRL_ZLIB});
my $decoder = Sereal::get_sereal_decoder;

my $words_file = "wordle-$length.words";
my $similarity_file = "wordle-$length.similarity";
my $decision_file = "wordle-$length.decision-tree";

sub get_words {
  if (-e $words_file) {
    @words = $decoder->decode_from_file($words_file)->@*;
  } else {
    open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
    while(<$words>) {
      s/[^a-zA-Z]//g;
      next unless length == $length;
      push(@words, uc);
    }
    @words = sort { $a cmp $b } @words;
    my $last = '';
    @words = grep { my $result = $_ ne $last; $last = $_; $result } @words;
    $encoder->encode_to_file($words_file, \@words, 0);
  }
  return;
}

sub get_similarity {
  if (-e $similarity_file) {
    @similarity = $decoder->decode_from_file($similarity_file)->@*;
  } else {
    get_words unless @words;
    for my $i (keys @words) {
      my $wi = $words[$i];
      my %mi;
      for (0 .. $lm) {
        my $l = substr($wi, $_, 1);
        push($mi{$l}->@*, $_);
      }
      $similarity[$i][$i] = 'M' x $length;
      for my $j ($i+1 .. $#words) {
        my $wj = $words[$j];
        my $rij = my $rji = 'x' x $length;
        my %ind;
        for (0 .. $lm) {
          my $l = substr($wj, $_, 1);
          my $idx = $mi{$l}[$ind{$l}++];
          if (defined $idx) {
            my $r = ($idx == $_) ? 'M' : 'm';
            substr($rij, $idx, 1) = $r;
            substr($rji, $_, 1) = $r;
          }
        }
        $similarity[$i][$j] = $rij;
        $similarity[$j][$i] = $rji;
      }
    }
    $encoder->encode_to_file($similarity_file, \@similarity, 0);
  }
}

get_words;
get_similarity;

my @possible = keys @words;

my $solution = 'FAVOR';
my $si = first { $words[$_] eq $solution } @possible;
say "$solution : $si";
my %si;
for (0 .. $lm) {
  my $l = substr($solution, $_, 1);
  push($si{$l}->@*, $_);
}

while (@possible > 1) {
  my $best = 1e100;
  my $best_contains = 0;
  my $best_index;
  my $best_groups;
  for my $wi (0 .. $#words) {
    my %groups;
    my $contains = 0;
    for my $pi (@possible) {
      $contains = 1 if $pi == $wi;
      push($groups{$similarity[$wi][$pi]}->@*, $pi);
    }
    my $priority = 0;
    for my $group (values %groups) {
      my $v = $group->@*;
      $priority += $v*log($v);
    }
    if ($priority < $best || ($priority == $best && $contains > $best_contains)) {
      $best = $priority;
      $best_contains = $contains;
      $best_index = $wi;
      $best_groups = \%groups;
    }
  }
  say "Best is $words[$best_index] (index:$best_index score: $best, contains: $best_contains)";
  my $rji;
  my $wj = $words[$best_index];
  my %ind;
  for (0 .. $lm) {
    my $l = substr($wj, $_, 1);
    my $idx = $si{$l}[$ind{$l}++];
    if (defined $idx) {
      my $r = ($idx == $_) ? 'M' : 'm';
      $rji .= $r;
    } else {
      $rji .= 'x';
    }
  }
  @possible = $best_groups->{$rji}->@*;
  say "$solution : $wj : $rji [@possible]";
}

if (@possible) {
  say "Word is $words[$possible[0]]";
} else {
  die "Failure";
}
