use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use Crypt::Cipher::AES;
use MIME::Base64;
use WWW::Form::UrlEncoded qw(build_urlencoded parse_urlencoded);
use File::Slurp qw(read_file);

use Data::Printer;

# In order to make this work, you have to make at least one assumption about the web
# server. One assumption would be that the url encoding passes the padding character
# through unmodified. Based on their writing I think this the assumption they wanted us to
# make. That's crazy because a real url encoding function will in encode that character,
# not through some sense of security but because there's probably transport hosts out
# there that will strip them out since they're not printable. I'm instead making the
# assumption that the field will get trailing spaces trimmed from it. This is a much more
# likely thing to happen either by manual intervention (I don't really expect it on a role
# field, but it's a common practice on user entered fields to trim both sides and they may
# have gotten tired of it and just put the trim on the decoding step) or because they're
# comparing in a database using a fixed length column and those will perform that rtrim
# silently on compare.

# So we make this work by doing the following:
# 1. Make a profile for a user with a name that will pad out the first block, then finish
#    the email with admin followed by block sized padded spaces.
# 2. Fill in an email that will make role= end a block, then replace the trailing user
#    block with our admin block.

my $key = join('', map { chr(ord('A') + int rand(26)) } 1 .. 16);

my $cipher = Crypt::Cipher::AES->new($key);

sub profile_for($email) {
  return build_urlencoded(
    email => "$email",
    uid => 10,
    role => 'user'
  );
}

sub encode($text) {
  my $rem = (length $text) % 16;
  my $decoded = $text;
  $decoded .= ("\x04" x (16 - $rem)) if $rem != 0;
  return join('', map { $cipher->encrypt($_) } unpack('(a16)*', $decoded));
}

sub decode($text) {
  my $decoded = join('', map { $cipher->decrypt($_) } unpack('(a16)*', $text));
  $decoded =~ s/\x04+$//;
  return $decoded;
}

sub encrypted_profile_for($email) {
  return encode(profile_for($email));
}

sub decrypt_profile($encrypted) {
  return parse_urlencoded(decode($encrypted));
}

# This will work for any block size of 7 or more. You can do it with smaller blocks, but I
# would have to change the algorithm to deal with data crossing multiple blocks.
my $block_size = 16;

# First generate the initial block.
my $admin_email = 'A' x ($block_size - length('email='));
$admin_email .= 'admin' . (' ' x ($block_size - length('admin')));

my $admin_block = unpack('x' . $block_size . 'a' . $block_size, encrypted_profile_for($admin_email));

my $email_length = $block_size - (length 'email=tbollman%40kevdenti.com&uid=10&role=') % $block_size;

my $encrypted = encrypted_profile_for('tbollman@kevdenti.com' . ' ' x $email_length);
substr($encrypted, -$block_size) = $admin_block;

my %profile = decrypt_profile($encrypted);

p(%profile);
