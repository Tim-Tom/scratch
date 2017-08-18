use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use Crypt::Cipher::AES;
use MIME::Base64;
use File::Slurp qw(read_file);

my $key = join('', map { chr(ord('A') + int rand(26)) } 1 .. 16);

my $cipher = Crypt::Cipher::AES->new($key);

my $real_text = decode_base64(<<'END_TEXT');
Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
YnkK
END_TEXT

sub encrypt_it($text) {
  my $decoded = $text . $real_text;
  my $rem = length($decoded) % 16;
  $decoded .= "\04" x (16 - $rem) if $rem != 0;
  return join('', map { $cipher->encrypt($_) } unpack('(a16)*', $decoded));
}

# So we determine block size in the following way: check the length of the encrypted
# text. When we add a character, check if the length changed. It will only go up in block
# size increments, thus telling us the real length of the text and the block size.
my $target_length = length encrypt_it('');
my $block_size;

for my $increment (1 .. 1e6) {
  my $candidate = 'A' x $increment;
  my $new_length = length(encrypt_it($candidate));
  if ($new_length != $target_length) {
    $block_size = $new_length - $target_length;
    $target_length -= $increment - 1;
    last;
  }
}

die "Couldn't find block size" unless defined $block_size;

my $get_block = 'a' . $block_size;
my $skip_block = 'x' . $block_size;

# Flipping the order on the head from what they poorly described in the intro. Instead of
# building a table I am instead going to build the first block, then check through all the
# bytes until I find my match. This means instead of going through all the possible bytes
# I can stop when I find the match. Meaning I could order the bytes to check english text
# if I wanted it to be faster.
sub search($block, $prefix) {
  for my $byte (map { chr($_) } 0 .. 255) {
    my $padded = $prefix . $byte;
    my $candidate = unpack($get_block, encrypt_it($padded));
    if ($candidate eq $block) {
      return $byte;
    }
  }
  die "Couldn't find a valid byte";
}

my $known = '';
while (length $known < $block_size) {
  my $padded = ('A' x ($block_size - 1 - length $known));
  my $block = unpack($get_block, encrypt_it($padded));
  $known .= search($block, $padded . $known);
}

# I can get this by looking at the length of the returned result instead of just knowing
# it. I'd decrypt the padding bytes as well, but that's not really a concern.
while (length $known < $target_length) {
  my $mod = (length $known) % $block_size;
  my $padded = ('A' x ($block_size - 1 - $mod));
  my $block = unpack(($skip_block x ((length $known) / $block_size)) . $get_block, encrypt_it($padded));
  $known .= search($block, substr($known, 1 - $block_size));
}

say $known;

