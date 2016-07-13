use strict;
use warnings;

use v5.24;
use feature 'signatures';

no warnings 'experimental::signatures';

use List::BinarySearch qw(binsearch_pos);
use Set::IntervalTree;
use Benchmark qw(cmpthese);

my @ranges;

sub make_range($f, $t, @nodes) {
    return { from => $f, to => $t, nodes => [@nodes] };
}

push(@ranges, make_range(1, 1_000_000));


sub print_range($r) {
    say "\t$r->{from} to $r->{to}: ". join(', ', $r->{nodes}->@*);
}

# Merge two ranges $a and $b. It is assumed that a and b are overlapping.
sub merge_range($a, $b) {
    my ($fc, $tc) = map { $a->{$_} <=> $b->{$_} } qw(from to);
    if ($fc == 0) {
        if ($tc == 0) {
            return (make_range($a->{from}, $a->{to}, $a->{nodes}->@*, $b->{nodes}->@*), );
        }
        if ($tc > 0) {
            ($a, $b) = ($b, $a);
        }
        return (
                make_range($a->{from}, $a->{to}, $a->{nodes}->@*, $b->{nodes}->@*),
                make_range($a->{to} + 1, $b->{to}, $b->{nodes}->@*)
               );
    }
    if ($fc > 0) {
        ($a, $b) = ($b, $a);
        $tc = -$tc;
    }
    if ($tc < 0) {
        return (
                make_range($a->{from}, $b->{from} - 1, $a->{nodes}->@*),
                make_range($b->{from}, $a->{to}, $a->{nodes}->@*, $b->{nodes}->@*),
                make_range($a->{to} + 1, $b->{to}, $b->{nodes}->@*)
               );
    } elsif ($tc > 0) {
        return (
                make_range($a->{from}, $b->{from} - 1, $a->{nodes}->@*),
                make_range($b->{from}, $b->{to}, $a->{nodes}->@*, $b->{nodes}->@*),
                make_range($b->{to} + 1, $a->{to}, $a->{nodes}->@*)
               );
    } else {
        return (
                make_range($a->{from}, $b->{from} - 1, $a->{nodes}->@*),
                make_range($b->{from}, $b->{to}, $a->{nodes}->@*, $b->{nodes}->@*)
               );
    }
}

sub insert_range($r) {
    my $left = binsearch_pos { $a <=> $b->{to} } $r->{from}, @ranges;
    my $right = binsearch_pos { $a <=> $b->{to} } $r->{to}, @ranges;
    my @nodes;
    for my $i (reverse $left .. $right) {
        ($r, my @rest) = merge_range($ranges[$i], $r);
        unshift(@nodes, @rest);
    }
    splice(@ranges, $left, $right - $left + 1, $r, @nodes);
}

my $intervalTree = Set::IntervalTree->new;

while(<DATA>) {
    next if /^#/;
    chomp;
    my $r = make_range(split);
    insert_range($r);
    $intervalTree->insert($r->{nodes}[0], $r->{from}, $r->{to} + 1);
}

cmpthese(500_000, {
    binary_search => sub { binsearch_pos(sub { $a <=> $b->{to} }, $_, @ranges) for (1 .. 500); },
    interval_tree => sub { $intervalTree->fetch($_, $_+1) for (1 .. 500); }
});

__DATA__
1  7  a
10 19 b
15 20 c
17 18 d
1  30 e
17 17 f
67 99 g
1  100 h
5 500 i
70 95 j
45 338 k
165 219 l
26 184 n
329 413 o
44 460 p
11 126 r
368 424 t
499 500 u
81 444 v
382 385 w
