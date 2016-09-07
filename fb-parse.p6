use v6;

grammar divspl {
  rule TOP { <x=int>..<y=int> <assignment>+ }
  rule assignment { <word> '=' <mod=int> }
  token word { \w+ }
  token int { \d+ }
}

class actions {
  method TOP($/) {
    # Filter lists, and merge using 〜
    $/.make: (([Z~] $<assignment>».made()) Z|| 1..*)[+$<x>-1..^+$<y>];
  }

  method assignment($/) {
    # Filter to change a number into $<word>
    $/.make: |('' xx ($<mod> - 1), $<word>).flat xx *;
  }
}

sub MAIN($filename) {
  my $parsed = divspl.parse($filename.IO.lines, :actions(actions))
    or die "sorry, invalid divspl";
  say $parsed.made.join("\n");
}
