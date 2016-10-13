use v6;

my $goal = 34;
my @choices = 1 ... 16;
my %valid_sizes = (3 ... 5).map: { $_ ** 2 => $_ };
my $width = %valid_sizes{+@choices};
die "Size ({+@choices}) must be one of {sort keys %valid_sizes}" unless $width;
my $wm = $width - 1;
my $wp = $width + 1;

my @a[+@choices];
my %picked = @choices.map: * => False;

class Choose {
    has $.pos;
    has $.next is rw;
    method action() {
        for @choices -> $n {
            next if %picked{$n};
            @a[$.pos] = $n;
            %picked{$n} = True;
            $.next.action();
            %picked{$n} = False;
        };
    }
};

class Decide {
    has $.pos;
    has @.indexes;
    has $.next is rw;
    method action() {
        my $n = $goal - [+] @a[@.indexes];
        return if %picked{$n} // True;
        @a[$.pos] = $n;
        %picked{$n} = True;
        $.next.action();
        %picked{$n} = False;
    }
};

class Validate {
    has @.indexes;
    has $.next is rw;
    method action() {
        my $calc = [+] @a[@.indexes];
        $.next.action() if $calc == $goal;
    }
};

class Solution {
    has $!count;
    method action() {
        say "--- Solution {++$!count} ---";
        say @a[$_ .. $_ + $wm].map({ .fmt("%2d") }).join(' ') for (0, * + $width ...^ +@choices);
    }
}

my @actions;
if ($width == 3) {
    @actions = (
        Choose.new(pos => 0),
        Choose.new(pos => 1),
        Decide.new(pos => 2, indexes => [0,1]),
        Choose.new(pos => 4),
        Decide.new(pos => 8, indexes => [0, 4]),
        Decide.new(pos => 5, indexes => [2, 8]),
        Decide.new(pos => 3, indexes => [4, 5]),
        Decide.new(pos => 6, indexes => [0, 3]),
        Decide.new(pos => 7, indexes => [6, 8]),
        Validate.new(indexes => [1,4,7]),
        Validate.new(indexes => [2,4,6]),
        Solution.new()
    );
} elsif ($width == 4) {
    @actions = (
        Choose.new(pos => 0),
        Choose.new(pos => 1),
        Choose.new(pos => 2),
        Decide.new(pos => 3, indexes => [0,1,2]),
        Choose.new(pos => 5),
        Choose.new(pos => 9),
        Decide.new(pos => 13, indexes => [1,5,9]),
        Choose.new(pos => 10),
        Decide.new(pos => 15, indexes => [0,5,10]),
        Choose.new(pos => 6),
        Decide.new(pos => 14, indexes => [2, 6, 10]),
        Decide.new(pos => 12, indexes => [13,14,15]),
        Choose.new(pos => 4),
        Decide.new(pos => 7, indexes => [4,5,6]),
        Decide.new(pos => 8, indexes => [0,4,12]),
        Decide.new(pos => 11, indexes => [8,9,10]),
        Validate.new(indexes => [3,7,11,15]),
        Validate.new(indexes => [3,6,9,12]),
        Solution.new()
    );
}

for 0 ..^ @actions - 1 -> $i {
     @actions[$i].next = @actions[$i + 1];
}

# Perform the first action and let it cascade.
@actions[0].action();
