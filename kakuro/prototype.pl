use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use Data::Printer;

# Used to initialize and constrain cells. I'm not sure I need the possible sums, but I may
# be able to jury rig them into the future constraint process.
my %possible_sums;
my %mask;
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
        my $data = bless({ start => $row, end => $row, cells => [$cell], dirty => 1 }, 'Constraint');
        push(@{$columns[$col]}, $data);
        $current_cols[$col] = $data;
      }
      if ($cur_row) {
        push(@{$cur_row->{cells}}, $cell);
        $cur_row->{end} = $col;
      } else {
        $cur_row = bless({ start => $col, end => $col, cells => [$cell], dirty => 1 }, 'Constraint');
        push(@{$rows[$row]}, $cur_row);
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
  }
}

for my $cell (@cells) {
  my %rm = %{$mask{$cell->{row}{key}}};
  my %cm = %{$mask{$cell->{col}{key}}};
  $cell->{data} = [grep { $rm{$_} && $cm{$_} } @{$cell->{data}}];
}

p(@cells);
