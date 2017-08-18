use strict;
use warnings;

use v5.24;
use Data::Printer;

use experimental 'signatures';

sub pad_pkcs7($str, $blocksize) {
  my $rem = length($str) % $blocksize;
  return $str . '\04'x($blocksize - $rem) if $rem != 0;
  return $str;
}

my $password = 'YELLOW SUBMARINE';

my $result = pad_pkcs7($password, 20);
p($result);
