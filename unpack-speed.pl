use strict;
use warnings;

use File::Slurp qw(read_file);
use Dumbbench;

use v5.24;

my $str = read_file('/etc/dictionaries-common/words', { binmode => ':raw'});

$str = $str x 10;

my $packed = pack('N', length $str) . $str;

my $bench = Dumbbench->new( initial_runs => 200, target_rel_precision => 0.005 );

$bench->add_instances(
  Dumbbench::Instance::PerlSub->new(name => 'unpack', code => sub { my ($length, $rest) = unpack('NA*', $packed); }),
  Dumbbench::Instance::PerlSub->new(name => 'substr', code => sub { my ($length, ) = unpack('N', $packed); my $rest = substr($packed, 4); }),
);

$bench->run;
$bench->report;
