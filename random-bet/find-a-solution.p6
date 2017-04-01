use v6;

die "3 arguments" unless @*ARGS == 3;

my @queue;

my $largest-length = 0;
my %seen;

sub add($predecessor, $length, @p) {
    my $k = @p.join('-');
    return if %seen{$k};
    %seen{$k} = $predecessor;
    @queue.push([$k, $length, |@p]);
}

sub predecessors($pred is copy) {
    while $pred {
        say "\t$pred";
        $pred = %seen{$pred};
    }
}

add(Str, 1, @*ARGS.map(+*).sort);

while @queue {
    my ($pred, $length, $p1, $p2, $p3) = @queue.shift;
    if ($length > $largest-length) {
        say "Got to depth $length: $p1 $p2 $p3";
        $largest-length = $length;
    }
    if ($p1 == $p2 || $p2 == $p3) {
        say "Found a solution of length $length: $p1 $p2 $p3";
        predecessors($pred);
        last;
    }
    add($pred, $length + 1, ($p1+$p1, $p2 - $p1, $p3).sort);
    add($pred, $length + 1, ($p1+$p1, $p2, $p3 - $p1).sort);
    add($pred, $length + 1, ($p1, $p2+$p2, $p3 - $p2).sort);
}

