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

sub sdiff($start, $orig, $new) {
  my ($o, $n) = ($start, $start);
  my (@o, @n);
  while ($o < $orig->@* && $n < $new->@*) {
    my $cmp = $orig->[$o] cmp $new->[$n];
    if ($cmp < 0) {
      push(@o, $o++);
    } elsif ($cmp > 0) {
      push(@n, $n++);
    } else {
      ++$n;
      ++$o;
    }
  }
  push(@o, $o .. $#{$orig});
  push(@n, $n .. $#{$new});
  return ([$orig->@[@o]], [$new->@[@n]]);
}

sub wcmp($ip, $jp) {
  my $iw = $ip->[0];
  my $jw = $jp->[0];
  # Words are distinct if they don't end on the same letter
  return 2 if substr($iw, -1, 1) ne substr($jw, -1, 1);
  my ($iu, $ju) = sdiff(1, $ip, $jp);
  if ($iu->@*) {
    if ($ju->@*) {
      # Both add unique letters, so they're okay
      return 2;
    } else {
      # $j is a subset of $i
      return 0;
    }
  } elsif ($ju->@*) {
    # $i is a subset of $j, delete it
    return 1;
  } else {
    # Same set of unique letters pick the shortest word, or if the same the
    # alphabetically first
    my $cmp = length $iw <=> length $jw || $iw cmp $jw;
    if ($cmp > 0) {
      return 1;
    } elsif ($cmp < 0) {
      return 0;
    } else {
      # $iw and $jw are the same word
      return 0;
    }
  }
  die;
}

sub wfilter(@words) {
  my %deleted;
 i:
  for my $i (keys @words) {
    next if $deleted{$i};
    for my $j ($i+1 .. $#words) {
      next if $deleted{$j};
      my $status = wcmp(@words[$i, $j]);
      if ($status == 0) {
        # $j is a subset of $i
        $deleted{$j} = 1;
      } elsif ($status == 1) {
        $deleted{$i} = 1;
        next i;
      } else {
        die if $status != 2;
        # words are both good, continue checking
      }
    }
  }
  return @words[grep {!$deleted{$_} } keys @words];
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
    push($words{substr($word, 0, 1)}->@*, [$word, suniq sort { $a cmp $b } split(//, $word)]);
    # say "$word $trans";
  }
}

# Filter the word list for inferior words that can be covered by another word
for my $start (sort keys %words) {
  $words{$start} = [wfilter($words{$start}->@*)];
}

my %memo;
sub best($start, @so_far) {
  if (@so_far == @letters) {
    return ();
  }
  my $key = join('-', $start, @so_far);
  if (!exists $memo{$key}) {
    # say "$start: @so_far";
    my @best = ('x', ) x 100;
    my @choices;
    for my $i (keys $words{$start}->@*) {
      my ($word, @letters) = $words{$start}[$i]->@*;
      my @diff = sdiff(0, \@so_far, \@letters);
      # next if @add == 0 && substr($word, -1, 1) eq $start;
      next if $diff[1]->@* == 0;
      push(@choices, [$word, $diff[1]->@*]);
    }
    use Data::Printer;
    # p(@choices);
    my @choices2 = wfilter(@choices);
    # p(@choices2);
    # exit if @choices > @choices2;
    for my $choice (@choices2) {
      my ($word, @add) = $choice->@*;
      my @s = sort { $a cmp $b } @so_far, @add;
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
