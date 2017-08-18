use strict;
use warnings;

use v5.24;

use experimental 'signatures';

sub valid_pkcs7_padding($text, $block=16) {
  my $len = length($text);
  die "String is not padded" if $len % $block != 0;
  my $padding = substr($text, -1);
  my $padding_int = ord($padding);
  die "Cannot have $padding_int bytes of padding for a block size of $block" if ($padding_int > $block);
  my $re = quotemeta($padding) . "{$padding_int}";
  die "Padding incorrect" unless $text =~ /$re$/;
  return 1;
}

eval {
  valid_pkcs7_padding("ICE ICE BABY\x17\x17\x17\x17");
  say 'valid';
} || warn $@;
eval {
  valid_pkcs7_padding("ICE ICE BABY\x04\x04\x04\x04");
  say 'valid';
} // warn $@;
eval {
  valid_pkcs7_padding("ICE ICE BABY\x05\x05\x05\x05");
  say 'valid';
} // warn $@;
eval {
  valid_pkcs7_padding("ICE ICE BABY\x01\x02\x03\x04");
  say 'valid';
} // warn $@;
