use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use File::Slurp qw(read_file write_file);
use Unicode::UTF8 qw(decode_utf8 encode_utf8);

my $year = $ARGV[0] // 2017;

mkdir $year unless -d $year;

for my $day (1 .. 25) {
  my $input_name = sprintf('../articles/%04d/Day-%02d.txt', $year, $day);
  my $output_name = sprintf('%04d/day-%02d.pl', $year, $day);
  next if -f $output_name;
  die unless -f $input_name;
  my $text = decode_utf8(scalar read_file($input_name, { binmode => ':raw' }));
  write_file($output_name, { binmode => ':raw' }, encode_utf8(<<"END_FILE"));
use strict;
use warnings;

use v5.24;

=pod

$text

=cut
END_FILE
}
