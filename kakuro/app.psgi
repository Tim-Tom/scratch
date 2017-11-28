use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use File::Slurp qw(read_file);
use YAML::XS;

use Plack::Request;
use Plack::MIME;
use File::Slurp qw(read_file);
use Unicode::UTF8 qw(decode_utf8);

sub static_file($req) {
  my $filename = substr($req->path, 1);
  say $filename;
  # Hi, I'm a security hole.
  if (-f $filename) {
    my $mime = Plack::MIME->mime_type($filename);
    my $length = -s $filename;
    open(my $fh, '<:raw', $filename);
    return [200, ['Content-Type' => "$mime; charset=utf-8", 'Content-Length' => $length], $fh];
  } else {
    return [404, ['Content-Type' => 'text/plain', 'Content-Length' => 19 ], ['Path does not exist']];
  }
}

sub Application($env) {
  my $req = Plack::Request->new($env);
  say "Received request to " . $req->path;
  return static_file($req);
}

\&Application;
