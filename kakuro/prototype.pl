use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use Data::Printer;
use Scalar::Util qw(refaddr);

# Used to initialize and constrain cells. I'm not sure I need the possible sums, but I may
# be able to jury rig them into the future constraint process.
my %possible_sums;
my %mask;
my %required;

sub determine_required(@sums) {
  my %seen;
  $seen{$_}++ foreach (map {@$_} @sums);
  return { map { $_ => 1 } grep { $seen{$_} == @sums } keys %seen };
}

{
  my @queue = map { [1, $_, $_+1, $_] } 1 .. 9;
  while(my $pkg = shift @queue) {
    my ($len, $sum, $min_continue, @path) = @$pkg;
    my $key = "$len-$sum";
    push(@{$possible_sums{$key}}, \@path);
    $mask{$key}{$_} = 1 foreach (@path);
    for my $n ($min_continue .. 9) {
      push(@queue, [$len + 1, $sum + $n, $n + 1, @path, $n]);
    }
  }
  foreach my $key (keys %possible_sums) {
    my @sums = @{$possible_sums{$key}};
    if (@sums == 1) {
      $required{$key} = $mask{$key};
    } else {
      $required{$key} = determine_required(@sums);
    }
  }
}

# Queues containing the heuristic work items to perform.
my @unique_queue;
my @mask_queue;
my @required_queue;
my @multi_unique_queue;
my @simulation_queue;
my $solved;

sub add_constraint_to_non_mask_queues($constraint) {
  if (!$constraint->{req_dirty}) {
    $constraint->{req_dirty} = 1;
    push(@required_queue, $constraint);
  }
  if (!$constraint->{unq_dirty}) {
    $constraint->{unq_dirty} = 1;
    push(@multi_unique_queue, $constraint);
  }
  if (!$constraint->{sim_dirty}) {
    $constraint->{sim_dirty} = 1;
    push(@simulation_queue, $constraint);
  }
}

sub add_constraint_to_queues($constraint) {
  if (!$constraint->{mask_dirty}) {
    $constraint->{mask_dirty} = 1;
    push(@mask_queue, $constraint);
  }
  add_constraint_to_non_mask_queues($constraint);
}


sub add_cell_to_queues($cell) {
  if (@{$cell->{data}} == 1) {
    push(@unique_queue, $cell);
    ++$solved;
  }
  add_constraint_to_non_mask_queues($cell->{row});
  add_constraint_to_non_mask_queues($cell->{col});
}

sub build_constraint($index, $cell, $type) {
  my $constraint = bless({
    name => 'Unknown',
    start => $index,
    end => $index,
    cells => [$cell],
    req_dirty => 1,
    unq_dirty => 1,
    sim_dirty => 1,
    mask_dirty => 0,
    type => $type
  }, 'Constraint');
  push(@required_queue, $constraint);
  push(@multi_unique_queue, $constraint);
  push(@simulation_queue, $constraint);
  return $constraint;
}

sub read_line {
  my $line;
  while($line = <ARGV>) {
    last unless $line =~ /^#/;
  }
  die unless defined $line;
  chomp $line;
  return $line;
}

my ($hsize, $vsize) = split(/\s+/, read_line);

my @columns;
my @rows;

my @current_cols;
my @cells;
for my $row (0 .. ($vsize - 1)) {
  my @dots = split(//, read_line);
  die "Too few characters in board" unless @dots == $hsize;
  die "Invalid characters in board" if grep { $_ ne '.' && $_ ne 'X' } @dots;
  @dots = map { $_ eq '.' } @dots;
  my $cur_row = undef;
  for my $col (0 .. $#dots) {
    my $open = $dots[$col];
    if ($open) {
      my $cell = bless({ data => [ 1 .. 9 ], row_index => $row, col_index => $col }, 'Cell');
      if ($current_cols[$col]) {
        push(@{$current_cols[$col]->{cells}}, $cell);
        $current_cols[$col]->{end} = $row;
      } else {
        my $data = build_constraint($row, $cell, 'col');
        push(@{$columns[$col]}, $data);
        $current_cols[$col] = $data;
        $data->{name} = "Column $col." . scalar @{$columns[$col]};
      }
      if ($cur_row) {
        push(@{$cur_row->{cells}}, $cell);
        $cur_row->{end} = $col;
      } else {
        $cur_row = build_constraint($col, $cell, 'row');
        push(@{$rows[$row]}, $cur_row);
        $cur_row->{name} = "Row $row." . scalar @{$rows[$row]};
      }
      $cell->{col} = $current_cols[$col];
      $cell->{row} = $cur_row;
      push(@cells, $cell);
    } else {
      $cur_row = $current_cols[$col] = undef;
    }
  }
}

for my $row (0 .. $vsize - 1) {
  my @constraints = split(/\s+/, read_line);
  die "Number of constraints does not match board for row $row" if (@constraints != @{$rows[$row]});
  for my $i (0 .. $#constraints) {
    my $r = $rows[$row][$i];
    $r->{constraint} = $constraints[$i];
    $r->{length} = $rows[$row][$i]->{end} - $rows[$row][$i]->{start} + 1;
    my $key = $r->{length} . '-' . $r->{constraint};
    $r->{key} = $key;
    $r->{possible} = [@{$possible_sums{$key}}];
  }
}

for my $col (0 .. $hsize - 1) {
  my @constraints = split(/\s+/, read_line);
  die "Number of constraints does not match board for column $col" if (@constraints != @{$columns[$col]});
  for my $i (0 .. $#constraints) {
    my $c = $columns[$col][$i];
    $c->{constraint} = $constraints[$i];
    $c->{length} = $columns[$col][$i]->{end} - $columns[$col][$i]->{start} + 1;
    my $key = $c->{length} . '-' . $c->{constraint};
    $c->{key} = $key;
    $c->{possible} = [@{$possible_sums{$key}}];
    $c->{required} = $required{$key};
  }
}

for my $cell (@cells) {
  my %rm = %{$mask{$cell->{row}{key}}};
  my %cm = %{$mask{$cell->{col}{key}}};
  $cell->{data} = [grep { $rm{$_} && $cm{$_} } @{$cell->{data}}];
  push(@unique_queue, $cell) if (@{$cell->{data}} == 1);
}

$solved = @unique_queue;

while($solved < @cells) {
  if (@unique_queue) {
    my $cell = shift @unique_queue;
    my $n = $cell->{data}[0];
    print "Implementing Uniqueness for cell [$cell->{row_index}, $cell->{col_index}] ($n)\n";
    for my $constraint (($cell->{row}, $cell->{col})) {
      print "-Removing $n from $constraint->{type} siblings\n";
      for my $sibling (@{$constraint->{cells}}) {
        next if refaddr($sibling) == refaddr($cell);
        my @new_data = grep { $_ != $n } @{ $sibling->{data} };
        if (@new_data != @{ $sibling->{data} }) {
          print "--Improved cell [$sibling->{row_index}, $sibling->{col_index}] to [@new_data].\n";
          $sibling->{data} = \@new_data;
          add_cell_to_queues($sibling);
        }
      }
      if (!$constraint->{required}{$n}) {
        print "-Removing $constraint->{name} possibilities that do not include $n\n";
        my @possible = grep { grep { $_ == $n } @$_ } @{$constraint->{possible}};
        $constraint->{possible} = \@possible;
        $constraint->{required} = determine_required(@possible);
        add_constraint_to_queues($constraint);
      }
    }
  } elsif (@mask_queue) {
    my $constraint = shift(@mask_queue);
    $constraint->{mask_dirty} = 0;
    my %m = map { $_ => 1 } map { @$_ } @{$constraint->{possible}};
    print "Masking $constraint->{name} using improved possibilities\n";
    for my $cell (@{$constraint->{cells}}) {
      my @new_data = grep { $m{$_} } @{$cell->{data}};
      if (@new_data != @{$cell->{data}}) {
        print "-Improved cell [$cell->{row_index}, $cell->{col_index}] to [@new_data].\n";
        $cell->{data} = \@new_data;
        add_cell_to_queues($cell);
      }
    }
  } elsif (@required_queue) {
    my $constraint = shift(@required_queue);
    $constraint->{req_dirty} = 0;
    print "Finding cells with unique required elements in $constraint->{name}\n";
    my %seen;
    for my $cell (@{$constraint->{cells}}) {
      push(@{$seen{$_}}, $cell) foreach (grep { $constraint->{required}{$_} } @{$cell->{data}});
    }
    for my $n (keys %{$constraint->{required}}) {
      if (@{$seen{$n}} == 1) {
        my $cell = $seen{$n}[0];
        if (@{$cell->{data}} != 1) {
          print "-Determined that cell [$cell->{row_index}, $cell->{col_index}] had to be a $n\n";
          $cell->{data} = [$n];
          add_cell_to_queues($cell);
        }
      }
    }
  } elsif (@multi_unique_queue) {
    my $constraint = shift(@multi_unique_queue);
    $constraint->{unq_dirty} = 0;
    die 'Not implemented';
  } elsif (@simulation_queue) {
    my $constraint = shift(@simulation_queue);
    $constraint->{sim_dirty} = 0;
    die 'Not implemented';
  } else {
    die "Could not solve puzzle :(";
  }
}

# p(@cells);

say "==Final board==";

my @board = map { [ map { ' ' } 1 .. $hsize ] } 1 .. $vsize;

for my $cell (@cells) {
  $board[$cell->{row_index}][$cell->{col_index}] = $cell->{data}[0];
}
say join("\n", map { join("", @$_) } @board);
