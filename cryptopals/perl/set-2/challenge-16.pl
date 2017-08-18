use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use Crypt::Cipher::AES;
use MIME::Base64;

use Data::Printer;

my $key = join('', map { chr(ord('A') + int rand(26)) } 1 .. 16);

my $cipher = Crypt::Cipher::AES->new($key);

my $stock_prefix = 'comment1=cooking%20MCs;userdata=';

# I am lazy this time and am not going to bother using the real functions for this.
sub string_for($userdata) {
  return  $stock_prefix . $userdata . ";comment2=%20like%20a%20pound%20of%20bacon";
}

sub parse_str($str) {
  return map { [split(/=/, $_, 2)] } split(/;/, $str)
}

sub pseudo_cbc_decrypt($block, $iv) {
  my $decrypted = $cipher->decrypt($block);
  my $result = $decrypted ^ $iv;
  return $result;
}

sub pseudo_cbc_encrypt($block, $iv) {
  my $mutated = $block ^ $iv;
  return $cipher->encrypt($mutated);
}

sub remove_pkcs7_padding($text, $block=16) {
  my $len = length($text);
  die "String is not padded" if $len % $block != 0;
  my $padding = substr($text, -1);
  my $padding_int = ord($padding);
  die "Cannot have $padding_int bytes of padding for a block size of $block" if ($padding_int > $block);
  my $re = quotemeta($padding) . "{$padding_int}";
  die "Padding incorrect" unless $text =~ s/$re$//;
  return $text;
}

sub encode($text) {
  my $padding = 16 - ((length $text) % 16);
  my $decoded = $text . (chr($padding) x $padding);
  my $encoded = '';
  my $iv = "\0"x16;
  for my $block (unpack('(a16)*', $decoded)) {
    $iv = pseudo_cbc_encrypt($block, $iv);
    $encoded .= $iv;
  }
  return $encoded;
}


sub decode($text) {
  my $decoded = '';
  my $iv = "\0"x16;
  for my $block (unpack('(a16)*', $text)) {
    $decoded .= pseudo_cbc_decrypt($block, $iv);
    $iv = $block;
  }
  return remove_pkcs7_padding($decoded);
}

sub encrypted_string_for($userdata) {
  return encode(string_for($userdata));
}

sub decrypt_profile($encrypted) {
  return parse_str(decode($encrypted));
}

# Relevant part of the ascii table is the following
# : 3A 0b00111010
# ; 3B 0b00111011
# < 3C 0b00111100
# = 3D 0b00111101
# So assuming we can include : and < without issue (which we shouldn't be able to, but
# whatever we could do more work and get the codes in from any arbitrary character) we
# just put a 1 in all the correct places to xor with.

# Gives us at least one block of junk to play with
my $prefix_junk = 'A' x (32 - length($stock_prefix) % 16);
my $payload = 'AAA:admin<true';

my $encrypted = encrypted_string_for($prefix_junk . $payload);

substr($encrypted, 16 + length($stock_prefix) - length($stock_prefix) % 16, 16) ^= ("\x00" x 3) . "\x01" . ("\x00" x 5) . "\x01" . ("\x00" x 4);

say decode($encrypted);
my @profile = decrypt_profile($encrypted);

p(@profile);
