use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use Crypt::Cipher::AES;
use MIME::Base64;
use File::Slurp qw(read_file);

my $cipher = Crypt::Cipher::AES->new('YELLOW SUBMARINE');

# $cipher->cipher('Cipher::AES');
# $cipher->key('YELLOW SUBMARINE');

my $data = decode_base64(read_file('../../data/s2-c10.txt', { binmode => ':raw' }));

sub pseudo_cbc($block, $iv) {
  my $decrypted = $cipher->decrypt($block);
  my $result = $decrypted ^ $iv;
  $result =~ s/\x04+$//;
  return $result;
}

# my $decrypted = $cipher->decrypt($data);
my $iv = "\0" x 16;
foreach my $block (unpack('(a16)*', $data)) {
  print pseudo_cbc($block, $iv);
  $iv = $block;
}
