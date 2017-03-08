use strict;
use warnings;

use JSON;
use YAML::XS;

use File::Slurp qw(read_file write_file);
use Unicode::UTF8 qw(encode_utf8 decode_utf8);

if (@ARGV) {
  for my $filename (@ARGV) {
    die "$filename does not end in json" unless $filename =~ /\.json$/;
    (my $yamlname = $filename) =~ s/\.json$/.yml/;
    my $content = decode_utf8(scalar read_file($filename, {binmode => ':raw' }));
    my $parsed = JSON::from_json($content);
    write_file($yamlname, { binmode => ':raw' }, \encode_utf8(YAML::XS::Dump($parsed)));
  }
} else {
  my $content = do {
    local $/;
    <STDIN>
  };
  my $parsed = JSON::from_json($content);
  print YAML::XS::Dump($parsed);
}
