use v6;

my $goal = 21;
my @choices = 3 ... 11;
my %valid_sizes = (1 ... 10).map: { $_ ** 2 => $_ };
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
        say @a[$_ .. $_ + $wm] for (0, * + $width ...^ +@choices);
    }
}

my @actions = (
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

for 0 ..^ @actions - 1 -> $i {
     @actions[$i].next = @actions[$i + 1];
}

# Perform the first action and let it cascade.
@actions[0].action();
