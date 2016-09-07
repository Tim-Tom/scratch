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

my %additionalConstraints = (
  # One square diagonal from bottom left (due to bottom snaking pattern)
  (+@choices - $width - 2) => sub {
    my $calc = [+] @a[$wm, * + $wm ... 6];
    # say "$goal == $calc (@a[$wm, * + $wm ... 6])?";
    return $goal != $calc;
  },
  # Buttom Right
  (+@choices - 1) => sub {
    my $i = +@choices - 1;
    my $c1 = [+] @a[($i - $i % $width) .. $i];
    my $c2 = [+] @a[0, * + $wp ... $i];
    # say "$goal == $c1 (@a[($i - $i % $width) .. $i]) == $c2 (@a[0, * + $wp ... $i])?";
    return $goal != $c1
        || $goal != $c2;
  }
);

my $right = $width - 1;
my $secondToLastRow = $width * ($width - 2);
my $lastRow = $width * ($width - 1);
# We go top to bottom, left to right except that on the second to last row we finish off
# the bottom before progressing right since it doesn't require any choices to fill out
# that box. An optimization would be to run this once and cache the selections for $next
# and which pick function to use.
sub pickNext($i) {
  my $next;
  # say ('.' x $i) ~ "a[$i] = @a[$i]";
  if ($i == +@choices - 1) {
    return solution()
  } elsif ($i >= $lastRow) {
    $next = $i - $wm;
  } elsif ($i >= $secondToLastRow) {
    $next = $i + $width;
    return pickBottom($next);
  } else {
    $next = $i + 1;
  }
  return pickRight($next) if $next % $width == $right;
  return pickInternal($next);
}

# Square that's not along the bottom or right edges, so we have to pick from the remaining
# choices.
sub pickInternal($i) {
  for @choices -> $n {
    next if %picked{$n} // True;
    @a[$i] = $n;
    next if %additionalConstraints{$i}:exists && %additionalConstraints{$i}();
    %picked{$n} = True;
    pickNext($i);
    %picked{$n} = False;
  }
}

# Square on the right edge, sum the elements horizontally, subtract from the goal, and
# check if it's a valid selection.
sub pickRight($i) {
  my $n = $goal - [+] @a[($i - $i % $width) ..^ $i];
  return if %picked{$n} // True;
  @a[$i] = $n;
  return if %additionalConstraints{$i}:exists && %additionalConstraints{$i}();
  %picked{$n} = True;
  pickNext($i);
  %picked{$n} = False;
}

# Square on the bottom edge, sum the elements vertically, subtract from the goal, and
# check if it's a valid selection.
sub pickBottom($i) {
  my $n = $goal - [+] @a[$i % $width, * + $width ...^ $i];
  return if %picked{$n} // True;
  @a[$i] = $n;
  return if %additionalConstraints{$i}:exists && %additionalConstraints{$i}();
  %picked{$n} = True;
  pickNext($i);
  %picked{$n} = False;
}

# Valid selections for all the squares, output choice.
sub solution() {
  state $count = 0;
  say "--- Solution {++$count} ---";
  say @a[$_ .. $_ + $wm] for (0, * + $width ...^ +@choices);
}

# Start with the top left sqaure.
pickInternal(0);
