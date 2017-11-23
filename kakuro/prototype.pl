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
for my $row (0 .. ($vsize - 1)) {
  my @dots = split(//, read_line);
  die "Too few characters in board" unless @dots == $hsize;
  die "Invalid characters in board" if grep { $_ ne '.' && $_ ne 'X' } @dots;
  @dots = map { $_ eq '.' } @dots;
  my $cur_row = undef;
  for my $col (0 .. $#dots) {
    my $open = $dots[$col];
    if ($open) {
      if ($current_cols[$col]) {
        $current_cols[$col]->{end} = $row;
      } else {
        my $data = { start => $row, end => $row };
        push(@{$columns[$col]}, $data);
        $current_cols[$col] = $data;
      }
      if ($cur_row) {
        $cur_row->{end} = $col;
      } else {
        $cur_row = { start => $col, end => $col };
        push(@{$rows[$row]}, $cur_row);
      }
    } else {
      $cur_row = $current_cols[$col] = undef;
    }
  }
}

for my $row (0 .. $vsize - 1) {
  my @constraints = split(/\s+/, read_line);
  die "Number of constraints does not match board for row $row" if (@constraints != @{$rows[$row]});
  for my $i (0 .. $#constraints) {
    $rows[$row][$i]->{constraint} = $constraints[$i];
    $rows[$row][$i]->{length} = $rows[$row][$i]->{end} - $rows[$row][$i]->{start} + 1;
  }
}

for my $col (0 .. $hsize - 1) {
  my @constraints = split(/\s+/, read_line);
  die "Number of constraints does not match board for column $col" if (@constraints != @{$columns[$col]});
  for my $i (0 .. $#constraints) {
    $columns[$col][$i]->{constraint} = $constraints[$i];
    $columns[$col][$i]->{length} = $columns[$col][$i]->{end} - $columns[$col][$i]->{start} + 1;
  }
}
