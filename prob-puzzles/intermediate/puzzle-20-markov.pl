use v5.24;

use strict;
use warnings;

use PDL;

=pod

Imagine that, on a given day, the weather can be either sunny (with probability 0.4),
cloudy (ditto), or rainy (with probability 0.2). Let's make the heroic assumption that
weather is independent across days.

Define blocks or runs of weather to be (the largest possible) groups of consecutive days
in which the weather is the same. For example, if it rained for eight days, followed by a
day of sun, then a day of rain, we'd have 3 blocks.

Across a 10 day period, what's the expected number of blocks of identical weather.

=cut

=pod

I know there's a way to model this in pure math because I remember doing it in college,
but I can't find anything in my search off of stochastic processes in Wikipedia.

So I could just iterate through all possible 3^10 states and count them since it's
tiny. Or I could be a little smarter and memoize the counts and reduce it to a small
fraction of the total number of visits, but let's try building another absorbing markov
chain and see how that plays out.

=cut

my @types = qw(sunny cloudy rainy);

my @prob = (0.4, 0.4, 0.2);
my @transient_tuples = (
  [0.4, 0.0, 0.0, 0.0, 0.4, 0.2],
  [0.0, 0.4, 0.0, 0.4, 0.0, 0.2],
  [0.0, 0.0, 0.2, 0.4, 0.4, 0.0]
);
my @absorbing_tuples = (
  [0.4, 0.6],
  [0.4, 0.6],
  [0.2, 0.8]
);

my %prob = (
  sunny => 0.4,
  cloudy => 0.4,
  rainy => 0.2
);

sub make_dot {

  say "digraph weather {";
  say "  start [shape=diamond]";
  for my $day (1 .. 9) {
    say "  subgraph cluster_day_$day {";
    say "    label = \"Day $day\";";
    say "    color = black;";
    for my $count (1 .. $day) {
      say "    subgraph cluster_day_${day}_${count} {";
      say "      label = \"$count blocks\";";
      say "      color = grey;";
      for my $type (@types) {
        my $name = "${type}_${day}_${count}";
        say "      $name [shape=box, label=\"$type\"];";
      }
      say "    }";
    }
    say "  }";
  }
  {
    my $day = 10;
    say "  subgraph cluster_day_${day} {";
    say "    label = \"Day $day\";";
    say "    color = black;";
    for my $count (1 .. $day) {
      say "    day_${day}_${count} [shape=diamond, label=\"$count blocks\"]";
    }
    say "  }";
  }
  {
    my $day = 1;
    my $count = 1;
    for my $type (@types) {
      my $name = "${type}_${day}_${count}";
      say "start -> $name";
    }
  }
  for my $day (1 .. 8) {
    my $next_day = $day + 1;
    for my $count (1 .. $day) {
      for my $type (@types) {
        my $name = "${type}_${day}_${count}";
        for my $next_type (@types) {
          my $same = $type eq $next_type;
          my $next_count = $count + ($same ? 0 : 1);
          my $next_name = "${next_type}_${next_day}_${next_count}";
          my $color = $same ? 'grey' : 'black';
          say "  $name -> $next_name [color=$color];";
        }
      }
    }
  }
  {
    my $day = 9;
    my $next = 10;
    for my $count (1 .. 9) {
      for my $type (@types) {
        my $name = "${type}_${day}_${count}";
        my $count1 = $count + 1;
        say "  $name -> day_${next}_${count} [color=grey];";
        say "  $name -> day_${next}_${count1} [color=black];";
      }
    }
  }
  say "}";

}

# make_dot();

my $num_days = $ARGV[0] // 10;
my $n = $num_days - 1;
my $transient_count = do {
  # each day has $day block counts in it, each of which has @types states, except for the last
  # day which is my absorbing state (doesn't count in Q) and one more for the start state.
  @types * ($n*($n+1)/2) + 1;
};

my $absorbing_count = $num_days;

say "Transient: $transient_count";
say "Absorbing: $absorbing_count";

my ($Q, $R, $N, $B);
$Q = [];
# Start State
push(@$Q, [0, @prob, map { 0 } 1 .. ($transient_count - 4)]);

# Normal Days
{
  my @prefix = (0, );
  for my $day (1 .. $n - 1) {
    my $day_count = $day*@types;
    for my $count (1 .. $day) {
      push(@prefix, ((0, ) x @types));
      for my $tuple (@transient_tuples) {
        my @row = (@prefix, @{$tuple});
        push(@$Q, [@row, ((0, ) x ($transient_count - @row))]);
      }
    }
    push(@prefix, ((0, ) x @types));
  }
}

# Day before last (transitions to absorbing states)
for my $count (1 .. $n) {
  for my $type (@types) {
    push(@$Q, [(0, ) x $transient_count]);
  }
}

=pod

Debug Print $Q

my @labels = (' S ', map { my $day = $_; map { my $count = $_; map { my $type = $_; $day . $count . $type } qw(S C R) } 1 .. $day } 1 .. $n);

say join(' ', '   ', @labels);
for my $i (0 .. $#$Q) {
  my $row = $Q->[$i];
  my $label = $labels[$i];
  say join(' ', $label, map { sprintf '%0.1f', $_ } @$row);
}

=cut

$Q = pdl($Q);
$R = pdl do {
  my @R = map { [(0, ) x $absorbing_count] } 1 .. ($transient_count - $n*@types);
  my @prefix;
  for my $count (1 .. $n) {
    for my $tuple (@absorbing_tuples) {
      my @row = (@prefix, @$tuple);
      push(@R, [ @row, ((0, ) x ($absorbing_count - @row))])
    }
    push(@prefix, 0);
  }
  @R;
};
$N = (identity($transient_count, $transient_count) - $Q)->inv;
$B = $N x $R;

# say $Q;
# say $R;
# say $N;
# say $B;
say $B->slice(':, (0)');
say $B->slice(':, (0)') * (sequence($absorbing_count) + 1);
say sumover($B->slice(':, (0)') * (sequence($absorbing_count) + 1));

