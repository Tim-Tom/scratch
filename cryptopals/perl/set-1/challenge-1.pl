use strict;
use warnings;

use v5.24;

use MIME::Base64;

my $input = '49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d';
my $bytes = pack('H*', $input);

my $output = encode_base64($bytes, '');
my $expected = 'SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t';

if ($output ne $expected) {
  say "wrong: $output";
}
