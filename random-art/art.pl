use strict;
use warnings;

use v5.24;

use experimental 'signatures';

package Expressions {
  use Math::Trig;

  sub expr_x {
    return $_[0];
  }

  sub expr_y {
    return $_[1];
  }

  sub expr_negative_one {
    return -1;
  }

  sub expr_zero {
    return 0;
  }

  sub expr_one {
    return 1;
  }

  sub expr_sin($e) {
    return sin(Math::Trig::pi * $e);
  }

  sub expr_cos($e) {
    return cos(Math::Trig::pi * $e);
  }

  sub expr_negate($e) {
    return -$e;
  }

  sub expr_sqrt($e) {
    return sqrt($e) if $e >= 0;
    return -sqrt(-$e);
  }

  sub expr_clamp($e) {
    return $e <=> 0;
  }

  sub expr_round($e) {
    return -1 if $e < -1/3;
    return 1 if $e > 1/3;
    return 0;
  }

  sub expr_arith_mean($e1, $e2) {
    return ($e1 + $e2) / 2;
  }

  sub expr_geo_mean($e1, $e2) {
    return expr_sqrt($e1 * $e2);
  }

  sub expr_mult($e1, $e2) {
    return $e1 * $e2;
  }

  sub expr_max($e1, $e2) {
    return $e1 > $e2 ? $e1 : $e2;
  }

  sub expr_min($e1, $e2) {
    return $e1 > $e2 ? $e2 : $e1;
  }

  # Pretty sure these aren't going to return perfectly in my range so I may need to adjust them.
  # sub asin($e) {
  #   return Math::Trig::asin($e) / Math::Trig::pi;
  # }

  # sub acos($e) {
  #   return Math::Trig::acos($e) / Math::Trig::pi;
  # }

};

my @exprs = (
  {
    name => "x",
    sub => \&Expressions::expr_x,
    arguments => 0
  },
  {
    name => "y",
    sub => \&Expressions::expr_y,
    arguments => 0
   },
  # {
  #   name => "-1",
  #   sub => \&Expressions::expr_negative_one,
  #   arguments => 0
  #  },
  # {
  #   name => "0",
  #   sub => \&Expressions::expr_zero,
  #   arguments => 0
  #  },
  # {
  #   name => "1",
  #   sub => \&Expressions::expr_one,
  #   arguments => 0
  #  },
  {
    name => "sin",
    sub => \&Expressions::expr_sin,
    arguments => 1
   },
  {
    name => "cos",
    sub => \&Expressions::expr_cos,
    arguments => 1
   },
  {
    name => "neg",
    sub => \&Expressions::expr_negate,
    arguments => 1
   },
  {
    name => "sqrt",
    sub => \&Expressions::expr_sqrt,
    arguments => 1
   },
  {
    name => "clamp",
    sub => \&Expressions::expr_clamp,
    arguments => 1
   },
  {
   name => "round",
   sub => \&Expressions::expr_round,
   arguments => 1
  },
  {
    name => "arith_mean",
    sub => \&Expressions::expr_arith_mean,
    arguments => 2
   },
  {
    name => "geo_mean",
    sub => \&Expressions::expr_geo_mean,
    arguments => 2
   },
  {
    name => "mult",
    sub => \&Expressions::expr_mult,
    arguments => 2
   },
  {
    name => "max",
    sub => \&Expressions::expr_max,
    arguments => 2
   },
  {
    name => "min",
    sub => \&Expressions::expr_min,
    arguments => 2
   },
);

my @terminals = grep { $_->{arguments} == 0 } @exprs;
my @non_terminals = grep { $_->{arguments} > 0 } @exprs;

sub build_expr($depth) {
  my $expr = $depth == 1 ? $terminals[int rand @terminals] : $non_terminals[int rand @non_terminals];
  return {
    expr => $expr,
    children => [map { build_expr($depth-1) } 1 .. $expr->{arguments}]
  };
}

sub print_expr($tree) {
  my $name = $tree->{expr}{name};
  if ($tree->{expr}{arguments} == 0) {
    return $name;
  } else {
    my $args = join(', ', map { print_expr($_) } @{$tree->{children}});
    return "$name($args)";
  }
}

sub eval_expr($tree, $x, $y) {
  if ($tree->{expr}{arguments} == 0) {
    return $tree->{expr}{sub}($x, $y);
  } else {
    my @args = map { eval_expr($_, $x, $y) } @{$tree->{children}};
    return $tree->{expr}{sub}(@args);
  }
}

sub emitGreyscale($name, $scale, $expr) {
  open(my $pgm, '>:raw', "$name.pgm") or die;
  my $size = 2*$scale + 1;
  printf $pgm 'P5 %d %d 255'."\n", $size, $size;
  for my $xi (-$scale .. $scale) {
    for my $yi (-$scale .. $scale) {
      my ($x, $y) = map { $_ / $scale } ($xi, $yi);
      my $intensity = int(127.5 + 127.5*eval_expr($expr, $x, $y));
      print $pgm (pack 'C', $intensity);
    }
  }
  close $pgm;
}

my $seed;
if (@ARGV) {
  $seed = $ARGV[0];
  srand($seed);
} else {
  $seed = srand;
}
say "seed is $seed";
my $tree = build_expr(7);
say print_expr($tree);
emitGreyscale('test', 150, $tree);
