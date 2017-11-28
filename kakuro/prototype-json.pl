use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use Data::Printer;
use Scalar::Util qw(refaddr);
use Ref::Util qw(is_arrayref is_hashref);

use JSON qw(to_json);

# Used to initialize and constrain cells. I'm not sure I need the possible sums, but I may
# be able to jury rig them into the future constraint process.
my %possible_sums;
my %mask;
my %required;

sub set_difference($a1, $a2) {
  return grep { my $e = $_; !grep { $_ == $e } @$a2 } @$a1;
}

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
my $solved = 0;

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
    --$cell->{row}{remaining};
    --$cell->{col}{remaining};
    ++$solved;
  } elsif (@{$cell->{data}} == 0) {
    die "Impossible state";
  }
  add_constraint_to_non_mask_queues($cell->{row});
  add_constraint_to_non_mask_queues($cell->{col});
}

sub build_constraint($index, $cell, $type, $major, $minor) {
  my $constraint = bless({
    name => 'Unknown',
    start => $index,
    end => $index,
    cells => [$cell],
    req_dirty => 1,
    unq_dirty => 1,
    sim_dirty => 1,
    mask_dirty => 0,
    type => $type,
    index_major => $major,
    index_minor => $minor
  }, 'Constraint');
  push(@required_queue, $constraint);
  push(@multi_unique_queue, $constraint);
  push(@simulation_queue, $constraint);
  return $constraint;
}

sub clean_constraint($constraint) {
  return { map { $_ => $constraint->{$_} } qw(type index_major index_minor start end possible) };
}

sub clean_cell($cell) {
  return { map { $_ => $cell->{$_} } qw(row_index col_index data) }
}

sub loc_cell($cell) {
  return {
    type => 'Cell',
    row => $cell->{row_index},
    col => $cell->{col_index}
  };
}

sub loc_constraint($constraint) {
  return {
    type => 'Constraint',
    orientation => $constraint->{type},
    index_major => $constraint->{index_major},
    index_minor => $constraint->{index_minor}
  };
}

sub ensure_numeric($thing) {
  if (is_hashref($thing)) {
    return { map { $_ => ensure_numeric($thing->{$_}) } keys %$thing };
  } elsif (is_arrayref($thing)) {
    return [ map { ensure_numeric($_) } @$thing];
  } elsif (ref($thing)) {
    die "huh?";
  } elsif ($thing =~ /^\d+$/a) {
    return $thing + 0;
  } else {
    return $thing;
  }
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

sub unique_cell($cell) {
  my $n = $cell->{data}[0];
  my @results;
  for my $constraint (($cell->{row}, $cell->{col})) {
    for my $sibling (@{$constraint->{cells}}) {
      next if refaddr($sibling) == refaddr($cell);
      my @new_data = grep { $_ != $n } @{ $sibling->{data} };
      if (@new_data != @{ $sibling->{data} }) {
        $sibling->{data} = \@new_data;
        add_cell_to_queues($sibling);
        push(@results, {
          location => loc_cell($sibling),
          properties => {
            data => \@new_data
          }
         });
      }
    }
    if (!$constraint->{required}{$n}) {
      my @possible = grep { grep { $_ == $n } @$_ } @{$constraint->{possible}};
      $constraint->{possible} = \@possible;
      $constraint->{required} = determine_required(@possible);
      add_constraint_to_queues($constraint);
      push(@results, {
        location => loc_constraint($constraint),
        properties => {
          possible => $constraint->{possible},
          # required => $constraint->{required}
        },
      });
    }
  }
  return {
    action => 'unique',
    location => loc_cell($cell),
    results => \@results,
  }
}

sub mask_constraint($constraint) {
  my %m = map { $_ => 1 } map { @$_ } @{$constraint->{possible}};
  my @results;
  for my $cell (@{$constraint->{cells}}) {
    my @new_data = grep { $m{$_} } @{$cell->{data}};
    if (@new_data != @{$cell->{data}}) {
      $cell->{data} = \@new_data;
      add_cell_to_queues($cell);
      push(@results, {
        location => loc_cell($cell),
        properties => {
          data => \@new_data
         }
      });
    }
  }
  return {
    action => 'mask',
    location => loc_constraint($constraint),
    results => \@results
  };
}

sub required_constraint($constraint) {
  my %seen;
  my @results;
  for my $cell (@{$constraint->{cells}}) {
    push(@{$seen{$_}}, $cell) foreach (grep { $constraint->{required}{$_} } @{$cell->{data}});
  }
  for my $n (keys %{$constraint->{required}}) {
    if (@{$seen{$n}} == 1) {
      my $cell = $seen{$n}[0];
      if (@{$cell->{data}} != 1) {
        $cell->{data} = [$n];
        add_cell_to_queues($cell);
        push(@results, {
          location => loc_cell($cell),
          properties => {
            data => $cell->{data}
          }
        });
      }
    }
  }
  return {
    action => 'required',
    location => loc_constraint($constraint),
    results => \@results
  };
}

sub multi_unique_constraint($constraint) {
  my %seen;
  my @results;
  for my $cell (@{$constraint->{cells}}) {
    my $len = @{$cell->{data}};
    next if $len == 1 || $len >= $constraint->{remaining};
    my $key = join('', @{$cell->{data}});
    push(@{$seen{$key}}, $cell);
  }
  foreach my $set (keys %seen) {
    if (length($set) == @{$seen{$set}}) {
      my @nums = @{$seen{$set}[0]{data}};
      for my $cell (set_difference($constraint->{cells}, $seen{$set})) {
        my @new_data = set_difference($cell->{data}, \@nums);
        if (@new_data != @{$cell->{data}}) {
          $cell->{data} = \@new_data;
          add_cell_to_queues($cell);
          push(@results, {
            location => loc_cell($cell),
            properties => {
              data => $cell->{data}
            }
          });
        }
      }
    }
  }
  return {
    action => 'constrained-set',
    location => loc_constraint($constraint),
    results => \@results
  };
}

sub simulate($cells, $possible, $index, $remain) {
  my $found = 0;
  return 1 if ($index >= @$cells);
  foreach my $n (@{$cells->[$index]{data}}) {
    next unless $remain->{$n};
    $remain->{$n} = 0;
    if (simulate($cells, $possible, $index + 1, $remain)) {
      $possible->[$index]{$n} = 1;
      $found  = 1;
    }
    $remain->{$n} = 1;
  }
  return $found;
}

sub simulate_constraint($constraint) {
  my @cell_possible;
  my @new_possible;
  my @results;
  for my $pos (@{$constraint->{possible}}) {
    my %remain = map { $_ => 1 } @{$pos};
    if (simulate($constraint->{cells}, \@cell_possible, 0, \%remain)) {
      push(@new_possible, $pos);
    }
  }
  if (@new_possible != @{$constraint->{possible}}) {
    $constraint->{possible} = \@new_possible;
    $constraint->{required} = determine_required(@new_possible);
    add_constraint_to_queues($constraint);
    push(@results, {
      location => loc_constraint($constraint),
      properties => {
        possible => $constraint->{possible},
        # required => $constraint->{required}
      }
    });
  }
  for my $i (0 .. $#cell_possible) {
    my $cell = $constraint->{cells}[$i];
    my @new_data = grep { $cell_possible[$i]{$_} } @{$cell->{data}};
    if (@new_data != @{$cell->{data}}) {
      $cell->{data} = \@new_data;
      add_cell_to_queues($cell);
      push(@results, {
        location => loc_cell($cell),
        properites => {
          data => $cell->{data}
        }
      });
    }
  }
  return {
    action => 'simulate',
    location => loc_constraint($constraint),
    results => \@results
  };
}

my ($hsize, $vsize) = split(/\s+/, read_line);

my @columns;
my @rows;

my @current_cols;
my @cells;
for my $row (0 .. ($vsize - 1)) {
  my @dots = split(//, read_line);
  die "Too few characters in board (expected $hsize, got @{[scalar @dots]})" if @dots < $hsize;
  die "Too many characters in board (expected $hsize, got @{[scalar @dots]})" if @dots > $hsize;
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
        my $data = build_constraint($row, $cell, 'Column', $col, scalar @{$columns[$col]});
        push(@{$columns[$col]}, $data);
        $current_cols[$col] = $data;
        $data->{name} = "Column $col." . scalar @{$columns[$col]};
      }
      if ($cur_row) {
        push(@{$cur_row->{cells}}, $cell);
        $cur_row->{end} = $col;
      } else {
        $cur_row = build_constraint($col, $cell, 'Row', $row, scalar @{$rows[$row]});
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
    $r->{remaining} = $r->{length};
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
    $c->{remaining} = $c->{length};
    my $key = $c->{length} . '-' . $c->{constraint};
    $c->{key} = $key;
    $c->{possible} = [@{$possible_sums{$key}}];
    $c->{required} = $required{$key};
  }
}
close *ARGV;

for my $cell (@cells) {
  my %rm = %{$mask{$cell->{row}{key}}};
  my %cm = %{$mask{$cell->{col}{key}}};
  $cell->{data} = [grep { $rm{$_} && $cm{$_} } @{$cell->{data}}];
  add_cell_to_queues($cell);
}

my $result = {
  constraints => [ map { clean_constraint($_) } map { @$_ } @rows, @columns],
  cells => [ map { clean_cell($_) } @cells],
  steps => []
};

while($solved < @cells) {
  if (@unique_queue) {
    my $cell = shift @unique_queue;
    push(@{$result->{steps}}, unique_cell($cell));
  } elsif (@mask_queue) {
    my $constraint = shift(@mask_queue);
    next if $constraint->{remaining} == 0;
    $constraint->{mask_dirty} = 0;
    push(@{$result->{steps}}, mask_constraint($constraint));
  } elsif (@required_queue) {
    my $constraint = shift(@required_queue);
    next if $constraint->{remaining} == 0;
    $constraint->{req_dirty} = 0;
    push(@{$result->{steps}}, required_constraint($constraint));
  } elsif (@multi_unique_queue) {
    my $constraint = shift(@multi_unique_queue);
    next if $constraint->{remaining} == 0;
    $constraint->{unq_dirty} = 0;
    push(@{$result->{steps}}, multi_unique_constraint($constraint));
  } elsif (@simulation_queue) {
    @simulation_queue = map { $_->[1] } sort { $a->[0] <=> $b->[0] } map {
      my $product = @{$_->{possible}};
      for my $cell (@{$_->{cells}}) {
        $product *= @{$cell->{data}};
      }
      [$product, $_]
    } grep { $_->{remaining} > 0 } @simulation_queue;
    my $constraint = shift(@simulation_queue);
    next if $constraint->{remaining} == 0;
    $constraint->{sim_dirty} = 0;
    push(@{$result->{steps}}, simulate_constraint($constraint));
  } else {
    die "Could not solve puzzle :(";
  }
}

$result = ensure_numeric($result);

print to_json($result, { pretty => 1, canonical => 1 });

