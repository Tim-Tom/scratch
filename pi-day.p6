use v6;

# Constants
my $max = 120;
my $count = 500;

# Start with $count numbers.
my $co-prime = (^$count)
# Pick out pairs of numbers from our range.
  .map( { |(1 .. $max).roll(2) })
# Transform those numbers into their corresponding gcd
  .map( * gcd * )
# Take only the elements that are co-prime
  .grep( * == 1 )
# Count them
  .elems;

# Calculate experimental pi value.
my $π = sqrt(6 * $count / $co-prime);

# Result: 3.12601774957917
# Absolute Difference: 1.56e-02
# Relative Difference: 4.96e-03
say "Result: $π";
say "Absolute Difference: { abs(π - $π).fmt('%.2e') }";
say "Relative Difference: { abs(1 - $π/π).fmt('%.2e') }";
