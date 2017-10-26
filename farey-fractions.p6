use v6;

constant \n = 10;

my @farey = gather {
    my ($a, $b, $c, $d) = (0, 1, 1, n);
    loop {
        my $k = ((n + $b) / $d).Int;
        ($a, $b, $c, $d) = ($c, $d, $k*$c - $a, $k*$d - $b);
        last if $c >= n;
        take $a / $b;
    }
};

my %swap = <Less Greater Greater Less ??? ???>;

sub myCompare($an, $ad, $bn, $bd) {
    my ($cn, $cd) = ($an - $bn, $ad - $bd);
    my $result;
    if $cd < 0 {
        $cn = -$cn;
        $cd = -$cd;
    }
    if $cn < 0 {
        $result = 'Less';
    } elsif $cn == 0 {
        $result = $cd > 0 ?? 'Less' !! 'Greater';
    } elsif $cn == 1 && $cd == 1 {
        $result = 'Greater';
    } else {
        if $cd < $bd {
            $result = %swap{myCompare($bn, $bd, $cn, $cd)};
        } else {
            $result = myCompare($cn, $cd, $bn, $bd);
        }
    }
    # say "$an / $ad <> $bn / $bd = $cn / $cd: $result";
    return $result;
}

@farey = @farey.reverse;

my @diffs;
for 0 ..^ @farey -> $i {
    my ($an, $ad) = @farey[$i].nude;
    for $i ^..^ @farey -> $j {
        my ($bn, $bd) = @farey[$j].nude;
        next if $ad == $bd;
        my ($cn, $cd);
        my $result;
        if $ad < $bd {
            $result =  %swap{myCompare($bn, $bd, $an, $ad)};
        } else {
            $result =  myCompare($an, $ad, $bn, $bd);
        }
        die "$an $ad $bn $bd"  unless $result eq 'Greater';
    }
}


