use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use Term::ANSIColor qw(colored);

my @nodes;
my $root;

my %nodes;

my %colors = (
  'R' => 'red',
  'G' => 'green',
  'B' => 'blue',
  'C' => 'cyan',
  'M' => 'magenta',
  'Y' => 'yellow',
  'O' => 'white'
 );

sub get_node($id) {
  my $node = $id;
  while (!ref $node) {
    $node = $nodes{$node};
  }
  return $node;
}

sub make_node($color, $y, $x) {
  state $id = 0;
  my $node = { id => ++$id, color => $color, pos => [{x => $x, y => $y}], neighbors => [] };
  $nodes{$node->{id}} = $node;
  return $node->{id};
}

sub merge_nodes($n, $m) {
  return ($n, $m) if ($n->{id} == $m->{id});
  return merge_nodes($m, $n) if ($n->{id} > $m->{id});
  die "invalid" if $n->{color} ne $m->{color};
  $n->{pos} = [@{$n->{pos}}, @{$m->{pos}}];
  # warn "Merging $m->{id} into $n->{id}" if ($n->{id} == 24 || $m->{id} == 24);
  foreach my $neighbor (@{$m->{neighbors}}) {
    my @neighbors = grep { $_->{id} != $m->{id} } @{ $neighbor->{neighbors} };
    if (grep { $_->{id} == $n->{id} } @neighbors) {
      $neighbor->{neighbors} = \@neighbors;
    } else {
      $neighbor->{neighbors} = [@neighbors, $n];
      push(@{ $n->{neighbors} }, $neighbor);
    }
  }
  # warn "Merged $n->{id} $m->{id}" if ($n->{id} == 24 || $m->{id} == 24);
  $nodes{$m->{id}} = $n->{id};
  return ($n, $n);
}

sub neighbor_nodes($n, $m) {
  # warn "Neighbor $n->{id} $m->{id}" if ($n->{id} == 24 || $m->{id} == 24);
  return merge_nodes($n, $m) if ($n->{color} eq $m->{color});
  if (!grep { $_->{id} == $m->{id} } @{ $n->{neighbors} }) {
    die if grep { $_->{id} == $n->{id} } @{ $m->{neighbors} };
    push(@{$n->{neighbors}}, $m);
    push(@{$m->{neighbors}}, $n);
  }
  return ($n, $m);
}

{
  my @above;
  my $first = 1;
  my $line = 0;
  while(<ARGV>) {
    next if (/^#/);
    chomp;
    my $col = 0;
    my @current = map { make_node($_, $line, $col++) } split(//);
    if ($first) {
      $root = $nodes{$current[0]};
      for my $i (1 .. $#current) {
        neighbor_nodes(map { get_node($_) } @current[$i-1, $i]);
      }
      $first = 0;
    } else {
      for my $i (0 .. $#current) {
        neighbor_nodes(map { get_node($_) } $above[$i], $current[$i]);
      }
      for my $i (1 .. $#current) {
        neighbor_nodes(map { get_node($_) } @current[$i-1, $i]);
      }
    }
    @above = @current;
    ++$line;
  }
  close ARGV;
}

# foreach my $id (sort { $a <=> $b } keys %nodes) {
#   say "$id: $nodes{$id}->{id}";
# }

my @board;
{
  my %seen;
  my @queue = $root;
  while(my $node = shift @queue) {
    next if $seen{$node->{id}}++;
    push(@nodes, $node);
    $node->{real_id} = @nodes;
    foreach my $pos (@{$node->{'pos'}}) {
      warn "$pos->{x} $pos->{y} $node->{id}: $board[$pos->{y}][$pos->{x}]->{id}" if $board[$pos->{y}][$pos->{x}];
      $board[$pos->{y}][$pos->{x}] = $node;
    }
    push(@queue, @{$node->{neighbors}});
  }
  # print join('', "\N{BOX DRAWINGS LIGHT DOWN AND RIGHT}", (map { "\N{BOX DRAWINGS LIGHT DOWN AND HORIZONTAL}" } @{$board[0]}), "\N{BOX DRAWINGS LIGHT DOWN AND LEFT}")."\n";
  for my $row (@board) {
    my $fmt = '%' . (length scalar @nodes) . 'd';
    # print join(' ', map { colored([$colors{$_->{color}}], sprintf($fmt, $_->{real_id})) } @$row)."\n";
    print join(' ', map { sprintf($fmt, $_->{real_id}) } @$row)."\n";
    # print join('', "\N{BOX DRAWINGS LIGHT VERTICAL AND RIGHT}", (map { die $_->{color} unless $colors{$_->{color}}; colored([$colors{$_->{color}}], "\N{BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL}") } @$row), "\N{BOX DRAWINGS LIGHT VERTICAL AND LEFT}")."\n";
  }
  # print join('', "\N{BOX DRAWINGS LIGHT UP AND RIGHT}", (map { "\N{BOX DRAWINGS LIGHT UP AND HORIZONTAL}" } @{$board[0]}), "\N{BOX DRAWINGS LIGHT UP AND LEFT}")."\n";
}
