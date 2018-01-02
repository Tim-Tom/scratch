use strict;
use warnings;

use v5.24;

=pod

***** --- Day 4: High-Entropy Passphrases --- *****
A new system policy has been put in place that requires all accounts to use a
passphrase instead of simply a password. A passphrase consists of a series of
words (lowercase letters) separated by spaces.
To ensure security, a valid passphrase must contain no duplicate words.
For example:
    * aa bb cc dd ee is valid.
    * aa bb cc dd aa is not valid - the word aa appears more than once.
    * aa bb cc dd aaa is valid - aa and aaa count as different words.
The system's full passphrase list is available as your puzzle input. How many
passphrases are valid?


=cut

while(my $passphrase = <ARGV>) {
  chomp($passphrase);
  my @words = sort { $a cmp $b } split(/\s+/, $passphrase);
  my $last = '';
  my $valid = 1;
  for my $word (@words) {
    if ($word eq $last) {
      $valid = 0;
      last;
    }
    $last = $word;
  }
  say "$passphrase: " . ($valid ? 'valid' : 'invalid');
}
