use v6;

my @choices = 3 ... 11;
my @valid_sizes = (1 ... 10).map: * ** 2;
die "Size ({+@choices}) must be one of @valid_sizes[]" unless +@choices (elem) @valid_sizes;
my $sum = 21;
my @a[9];
my %picked = @choices.map: * => False;

sub pick0() {
    for @choices -> $n {
        @a[0] = $n;
        %picked{$n} = True;
        pick1();
        %picked{$n} = False;
    }
}

sub pick1() {
    for @choices -> $n {
        next if %picked{$n} // True;
        @a[1] = $n;
        %picked{$n} = True;
        pick2();
        %picked{$n} = False;
    }
}

sub pick2() {
    my $n = $sum - @a[1] - @a[0];
    return if %picked{$n} // True;
    @a[2] = $n;
    %picked{$n} = True;
    pick3();
    %picked{$n} = False;
}

sub pick3() {
    for @choices -> $n {
        next if %picked{$n} // True;
        @a[3] = $n;
        %picked{$n} = True;
        pick6();
        %picked{$n} = False;
    }
}

sub pick6() {
    my $n = $sum - @a[3] - @a[0];
    return if %picked{$n} // True;
    @a[6] = $n;
    %picked{$n} = True;
    pick4();
    %picked{$n} = False;
}

sub pick4() {
    for @choices -> $n {
        next if %picked{$n} // True;
        next if $sum != $n + @a[6] + @a[2];
        @a[4] = $n;
        %picked{$n} = True;
        pick5();
        %picked{$n} = False;
    }
}

sub pick5() {
    my $n = $sum - @a[4] - @a[3];
    return if %picked{$n} // True;
    @a[5] = $n;
    %picked{$n} = True;
    pick7();
    %picked{$n} = False;
}

sub pick7() {
    my $n = $sum - @a[4] - @a[1];
    return if %picked{$n} // True;
    @a[7] = $n;
    %picked{$n} = True;
    pick8();
    %picked{$n} = False;
}

my $solution_count = 0;
sub pick8() {
    my $n = $sum - @a[7] - @a[6];
    return if %picked{$n} // True;
    return if $sum != $n + @a[4] + @a[0];
    return if $sum != $n + @a[5] + @a[2];
    @a[8] = $n;
    say "--- Solution {++$solution_count} ---";
    say @a[0 .. 2];
    say @a[3 .. 5];
    say @a[6 .. 8];
}

pick0();
