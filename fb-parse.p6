use v6;

grammar divspl {
  rule TOP { <x=int>..<y=int> <assignment>+ }
  rule assignment { <word> '=' <mod=int> }
  token word { \w+ }
  token int { \d+ }
}

# Custom operator:
#   "fizz" 〜 2      == "fizz"
#   2 〜 "buzz"      == "buzz"
#   "fizz" 〜 "buzz" == "fizzbuzz"
sub infix:<〜>($x,$y) {
  return $x if $x and $y ~~ /\d/;
  return $y if $y and $x ~~ /\d/;
  return $x ~ $y;
}

class actions {
  method TOP($/) {
    # Filter lists, and merge using 〜
    $/.make: [Z〜] $<assignment>».made().map:
        { (+$<x>..+$<y>).map: $^filter }
  }

  method assignment($/) {
    # Filter to change a number into $<word>
    $/.make: { $<word> x ( $^arg %% $<mod> ) or $^arg }
  }
}

sub MAIN($filename) {
  my $parsed = divspl.parse($filename.IO.lines, :actions(actions))
    or die "sorry, invalid divspl";
  say $parsed.made.join("\n");
}
