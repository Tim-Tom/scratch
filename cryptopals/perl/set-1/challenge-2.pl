use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use MIME::Base64;

sub crypto_xor($left_str, $right_str) {
  my ($left, $right) = map { pack('H*', $_) } ($left_str, $right_str);
  return unpack('H*', $left ^ $right);
}

my $result = crypto_xor('1c0111001f010100061a024b53535009181c', '686974207468652062756c6c277320657965');
my $expected = '746865206b696420646f6e277420706c6179';

if ($result ne $expected) {
  say "wrong: $result";
} else {
  say "correct";
}
