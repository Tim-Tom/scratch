use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use Term::ANSIColor qw(colored);

use Hash::PriorityQueue;

use Data::Printer;

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

my ($type, $filename, $goal) = @ARGV;
{
  my @above;
  my $first = 1;
  my $line = 0;
  open(my $input, '<', $filename);
  while(<$input>) {
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
    $node->{index} = $#nodes;
    $node->{real_id} = @nodes;
    foreach my $pos (@{$node->{'pos'}}) {
      warn "$pos->{x} $pos->{y} $node->{id}: $board[$pos->{y}][$pos->{x}]->{id}" if $board[$pos->{y}][$pos->{x}];
      $board[$pos->{y}][$pos->{x}] = $node->{id};
    }
    push(@queue, @{$node->{neighbors}});
  }
}

sub print_board {
  print join('', "\N{BOX DRAWINGS LIGHT DOWN AND RIGHT}", (map { "\N{BOX DRAWINGS LIGHT DOWN AND HORIZONTAL}" } @{$board[0]}), "\N{BOX DRAWINGS LIGHT DOWN AND LEFT}")."\n";
  for my $row (@board) {
    # my $fmt = '%' . (length scalar @nodes) . 'd';
    # print join(' ', map { colored([$colors{$_->{color}}], sprintf($fmt, $_->{real_id})) } @$row)."\n";
    print join('', "\N{BOX DRAWINGS LIGHT VERTICAL AND RIGHT}", (map { colored([$colors{get_node($_)->{color}}], "\N{BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL}") } @$row), "\N{BOX DRAWINGS LIGHT VERTICAL AND LEFT}")."\n";
  }
  print join('', "\N{BOX DRAWINGS LIGHT UP AND RIGHT}", (map { "\N{BOX DRAWINGS LIGHT UP AND HORIZONTAL}" } @{$board[0]}), "\N{BOX DRAWINGS LIGHT UP AND LEFT}")."\n";
};

my @colors = keys %colors;

sub clone_state($state) {
  return {
    nodes => { %{ $state->{nodes} } },
    path => [ @{ $state->{path} } ],
    neighbors => {
      map { $_ => [@{ $state->{neighbors}{$_} }] } @colors
     }
  }
}

sub add_node($state, $node) {
  return if $state->{nodes}{$node->{index}}++;
  foreach my $node (grep { !$state->{nodes}{$_->{index}} } @{$node->{neighbors}}) {
    push(@{ $state->{neighbors}{$node->{color}} }, $node->{index});
  }
}

sub distance_to_goal($state) {
  my @queue = map { @$_ } values %{ $state->{neighbors} };
  push(@queue, 'level');
  my $depth = 1;
  my %seen = %{ $state->{nodes} };
#  my %colors;
  while(my $index = shift @queue) {
    if ($index eq 'level') {
      ++$depth;
      if (@queue) {
        push(@queue, 'level');
#       %colors = ();
      }
      next;
    }
    my $n = $nodes[$index];
#   $colors{$n->{color}}++;
    push(@queue, grep { !$seen{$_}++ } map { $_->{index} } @{ $n->{neighbors}});
  }
  return $depth; # + keys(%colors) - 1;
}

# Breadth first search
sub breadth
{
  my @queue;
  my $start_state = {
    nodes => {},
    path => [],
    neighbors => {
      map { $_ => [] } @colors
     }
   };
  my $max_depth = -1;
  add_node($start_state, $root);
  push(@queue, $start_state);
  while(my $state = shift @queue) {
    if ($max_depth < @{ $state->{path} }) {
      $max_depth = @{ $state->{path} };
      say "depth $max_depth (" . scalar(keys %{ $state->{nodes} }) . " of " . scalar(@nodes) . ")";
    }

    # p($state);
    my $completed = @nodes == keys %{ $state->{nodes} };
    if ($completed) {
      say join(' -> ', @{ $state->{path} });
      exit;
    }
    foreach my $color (@colors) {
      # p($state->{neighbors}{$color});
      my @neighbors = grep { !$state->{nodes}{$_} } @{ $state->{neighbors}{$color} };
      if (@neighbors) {
        my $new_state = clone_state($state);
        push(@{ $new_state->{path} }, $color);
        $new_state->{neighbors}{$color} = [];
        foreach my $neighbor (map { $nodes[$_] } @neighbors) {
          # warn "Adding node $neighbor->{index}";
          add_node($new_state, $neighbor);
        }
        push(@queue, $new_state);
      }
    }
  }
}

sub depth
{
  my @stack;
  my $start_state = {
    nodes => {},
    path => [],
    neighbors => {
      map { $_ => [] } @colors
     }
   };
  my $max_depth = 0;
  add_node($start_state, $root);
  push(@stack, $start_state);
  while(my $state = pop @stack) {
    my $completed = @nodes == keys %{ $state->{nodes} };
    if ($completed) {
      say join(' -> ', @{ $state->{path} });
      exit;
    }
    next if @{ $state->{path} } == $goal;
    foreach my $color (@colors) {
      # p($state->{neighbors}{$color});
      my @neighbors = grep { !$state->{nodes}{$_} } @{ $state->{neighbors}{$color} };
      if (@neighbors) {
        my $new_state = clone_state($state);
        push(@{ $new_state->{path} }, $color);
        $new_state->{neighbors}{$color} = [];
        foreach my $neighbor (map { $nodes[$_] } @neighbors) {
          # warn "Adding node $neighbor->{index}";
          add_node($new_state, $neighbor);
        }
        push(@stack, $new_state);
      }
    }
  }
}

sub star
{
  my $queue = Hash::PriorityQueue->new;
  my $start_state = {
    nodes => {},
    path => [],
    neighbors => {
      map { $_ => [] } @colors
     }
   };
  my $max_depth = 0;
  add_node($start_state, $root);
  $queue->insert($start_state, 0);
  while(my $state = $queue->pop) {
    my $completed = @nodes == keys %{ $state->{nodes} };
    # say join('', @{$state->{path}});
    if ($completed) {
      say join(' -> ', @{ $state->{path} });
      exit;
    }
    if (@{$state->{path}} > $max_depth) {
      $max_depth = @{$state->{path}};
      say "Got to depth of $max_depth";
    }
    foreach my $color (@colors) {
      # p($state->{neighbors}{$color});
      my @neighbors = grep { !$state->{nodes}{$_} } @{ $state->{neighbors}{$color} };
      if (@neighbors) {
        my $new_state = clone_state($state);
        push(@{ $new_state->{path} }, $color);
        $new_state->{neighbors}{$color} = [];
        foreach my $neighbor (map { $nodes[$_] } @neighbors) {
          # warn "Adding node $neighbor->{index}";
          add_node($new_state, $neighbor);
        }
        my $distance = @{$new_state->{path}} + distance_to_goal($new_state);
        # if ($distance <= $goal) {
          $queue->insert($new_state, $distance);
        # }
      }
    }
  }
}

# breadth();
# depth();
# star();
{
  no strict 'refs';
  $type->();
}
