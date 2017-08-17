use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use MIME::Base64;

sub crypto_xor($left_str, $right_str) {
  my ($left, $right) = map { pack('H*', $_) } ($left_str, $right_str);
  return unpack('H*', $left ^ $right);
}

my $input = <<'END';
Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal
END
chomp($input);

sub encode_xor($str, $key) {
  my $len = length $str;
  my $stride = length $key;
  my $leftover = $len % $stride;
  my $extended = ($key x ($len / $stride)) . substr($key, 0, $leftover);
  return unpack('H*', $str ^ $extended);
}

my $result = encode_xor($input, 'ICE');
my $expected = '0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f';

if ($result ne $expected) {
  say "Wrong!";
  say $result;
  say $expected;
} else {
  say "correct";
}
