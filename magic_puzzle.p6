use v6;

my $sum = 21;
my @choices = 3 ... 11;
my %valid_sizes = (1 ... 10).map: { $_ ** 2 => $_ };
my $width = %valid_sizes{+@choices};
my $wm = $width - 1;
my $wp = $width + 1;
die "Size ({+@choices}) must be one of {sort keys %valid_sizes}" unless $width;
my @a[+@choices];
my %picked = @choices.map: * => False;

my %additionalConstraints = (
  (+@choices - $width - 2) => sub {
    my $calc = [+] @a[$wm, * + $wm ... 6];
    # say "$sum == $calc (@a[$wm, * + $wm ... 6])?";
    return $sum != $calc;
   }
  (+@choices - 1) => sub {
    my $i = +@choices - 1;
    my $c1 = [+] @a[($i - $i % $width) .. $i];
    my $c2 = [+] @a[0, * + $wp ... $i];
    # say "$sum == $c1 (@a[($i - $i % $width) .. $i]) == $c2 (@a[0, * + $wp ... $i])?";
    return $sum != $c1
        || $sum != $c2;
   }
 );

my $right = $width - 1;
my $secondToLastRow = $width * ($width - 2);
my $lastRow = $width * ($width - 1);
sub pickNext($i) {
  my $next;
  # say ('.' x $i) ~ "a[$i] = @a[$i]";
  if ($i == +@choices - 1) {
    return solution()
  } elsif ($i >= $lastRow) {
    $next = $i - $width + 1;
  } elsif ($i >= $secondToLastRow) {
    $next = $i + $width;
    return pickBottom($next);
  } else {
    $next = $i + 1;
  }
  return pickRight($next) if $next % $width == $right;
  return pickInternal($next);
}

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

sub pickRight($i) {
  my $n = $sum - [+] @a[($i - $i % $width) ..^ $i];
  return if %picked{$n} // True;
  @a[$i] = $n;
  return if %additionalConstraints{$i}:exists && %additionalConstraints{$i}();
  %picked{$n} = True;
  pickNext($i);
  %picked{$n} = False;
}

sub pickBottom($i) {
  my $n = $sum - [+] @a[$i % $width, * + $width ...^ $i];
  return if %picked{$n} // True;
  @a[$i] = $n;
  return if %additionalConstraints{$i}:exists && %additionalConstraints{$i}();
  %picked{$n} = True;
  pickNext($i);
  %picked{$n} = False;
}

sub solution() {
  state $count = 0;
  say "--- Solution {++$count} ---";
  say @a[$_ .. $_ + $wm] for (0, * + $width ...^ +@choices);
}

pickInternal(0);
