# perl -ne 's/[^a-zA-Z]//g;print uc . "\n" if length == 5' /usr/share/dict/american-english  | sort | uniq | wc -l
use strict;
use warnings;

use v5.26;

use experimental qw(signatures postderef);

use List::Util qw(first pairs unpairs);
use Sereal;
use IO::Prompt qw(prompt);

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
    say "Loading words from cache";
    @words = $decoder->decode_from_file($words_file)->@*;
  } else {
    say "Building word list";
#    open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
    open(my $words, '<:encoding(utf-8)', 'wordle-list.txt') or die;
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
  say "Done getting word list";
  return;
}

sub get_similarity {
  if (-e $similarity_file) {
    say "Loading similarity matrix from cache";
    @similarity = $decoder->decode_from_file($similarity_file)->@*;
  } else {
    get_words unless @words;
    say "Building similarity matrix";
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
  say "Done getting similarity matrix";
  return;
}

sub get_decision_node(@possible) {
  my $best = 1e100;
  my $best_contains = 0;
  my $best_index;
  my $best_groups;
  if (@possible == 1) {
    return {
      choice => $possible[0],
      terminal => 1,
      children => {}
    };
  }
  for my $wi (keys @similarity) {
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
  return bless {
    choice => $best_index,
    terminal => 0,
    children => {
      map { $_ => get_decision_node($best_groups->{$_}->@*); } keys $best_groups->%*
     }
   }, 'DecisionNode';
}

sub get_decision_tree {
  if (-e $decision_file) {
    say "Loading decision tree from cache";
    $decision_tree = $decoder->decode_from_file($decision_file);
  } else {
    get_similarity unless @similarity;
    say "Building decision tree";
    $decision_tree = get_decision_node(keys @similarity);
    $encoder->encode_to_file($decision_file, $decision_tree, 0);
  }
  say "Done getting decision tree";
  return;
}

get_words;
get_decision_tree;

my $node = $decision_tree;
my $solution = $ARGV[0];
if ($solution) {
  my $si = first { $words[$_] eq $solution } keys @words;
  say "$solution : $si";
  my %si;
  for (0 .. $lm) {
    my $l = substr($solution, $_, 1);
    push($si{$l}->@*, $_);
  }

  while (!$node->{terminal}) {
    my $choice = $words[$node->{choice}];
    say "Best is $choice ($node->{choice})";
    my $rji;
    my %ind;
    for (0 .. $lm) {
      my $l = substr($choice, $_, 1);
      my $idx = $si{$l}[$ind{$l}++];
      if (defined $idx) {
        my $r = ($idx == $_) ? 'M' : 'm';
        $rji .= $r;
      } else {
        $rji .= 'x';
      }
    }
    $node = $node->{children}{$rji} or die "Failed to decode word";
  }
} else {
  while(!$node->{terminal}) {
    my $choice = $words[$node->{choice}];
    say "Best is $choice ($node->{choice})";
    my $rji = prompt(-p => "Response: ", -until => qr/^[mMx]{5}$/);
    $node = $node->{children}{$rji};
  }
}

say "The solution is $words[$node->{choice}]";
