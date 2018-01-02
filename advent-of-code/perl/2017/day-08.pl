use strict;
use warnings;

use v5.24;
use experimental 'signatures';

=pod

***** --- Day 8: I Heard You Like Registers --- *****
You receive a signal directly from the CPU. Because of your recent assistance
with jump_instructions, it would like you to compute the result of a series of
unusual register instructions.
Each instruction consists of several parts: the register to modify, whether to
increase or decrease that register's value, the amount by which to increase or
decrease it, and a condition. If the condition fails, skip the instruction
without modifying the register. The registers all start at 0. The instructions
look like this:
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
These instructions would be processed as follows:
    * Because a starts at 0, it is not greater than 1, and so b is not
      modified.
    * a is increased by 1 (to 1) because b is less than 5 (it is 0).
    * c is decreased by -10 (to 10) because a is now greater than or equal to 1
      (it is 1).
    * c is increased by -20 (to -10) because c is equal to 10.
After this process, the largest value in any register is 1.
You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the registers
are named, and leaves that to you to determine.
What is the largest value in any register after completing the instructions in
your puzzle input?

=cut

my %registers;

my %operations = (
  inc => sub($var, $val) {
    $registers{$var} += $val;
  },
  dec => sub($var, $val) {
    $registers{$var} -= $val;
  }
);

my %comparisons = (
  '<'  => sub($l, $r) { return $l < $r; },
  '>'  => sub($l, $r) { return $l > $r; },
  '<=' => sub($l, $r) { return $l <= $r; },
  '>=' => sub($l, $r) { return $l >= $r; },
  '==' => sub($l, $r) { return $l == $r; },
  '!=' => sub($l, $r) { return $l != $r; }
);

sub determine_value($val) {
  if ($val =~ /^-?\d+$/) {
    return $val;
  } else {
    return ($registers{$val} //= 0);
  }
}

while(<ARGV>) {
  chomp;
  die $_ unless (/^\s*(\w+)\s+(\S+)\s+(-?\d+|\w+)\s+if\s+(-?\d+|\w+)\s+(\S+)\s*(-?\d+|\w+)\s*$/);
  my ($register, $operation, $amount, $lop, $comparison, $rop) = ($1, $2, $3, $4, $5, $6);
  die "Unknown operation $operation" unless ($operations{$operation});
  die "Unknown comparison $comparison" unless ($comparisons{$comparison});
  ($amount, $lop, $rop) = map { determine_value($_) } ($amount, $lop, $rop);
  $registers{$register} //= 0;
  if ($comparisons{$comparison}($lop, $rop)) {
    $operations{$operation}($register, $amount);
  }
}

for my $register (sort { $registers{$b} <=> $registers{$a} } keys %registers) {
  say "$register: $registers{$register}";
}
