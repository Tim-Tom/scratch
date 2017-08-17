use strict;
use warnings;

use v5.24;

use Crypt::ECB;
use MIME::Base64;
use File::Slurp qw(read_file);

my $cipher = Crypt::ECB->new;
$cipher->cipher('Cipher::AES');
$cipher->key('YELLOW SUBMARINE');

my $data = decode_base64(read_file('../../data/s1-c7.txt', { binmode => ':raw' }));
my $decrypted = $cipher->decrypt($data);
say $decrypted;
