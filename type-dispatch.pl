use v5.20;

use Dumbbench;

use strict;
use warnings;

use List::Util qw(pairmap pairfirst);

my @regexes = (
  qr!-?\d+!a => 'integer',
  qr!-?\d*\.\d+(?:[eE]\d+)?!a => 'real',
  qr!\d{4}-\d{2}-\d{2}!a => 'date',
  qr!\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}!a => 'datetime'
 );

my $re = join('|', pairmap { $a . q<$(?{'> . $b . q<'})> } @regexes);
{
  use re 'eval';
  $re = qr/^(?:$re)/;
}

sub get_type {
  my $v = shift;
  if ($v =~ /$re/) {
    return $^R;
  } else {
    return 'string';
  }
}

sub get_type2 {
  my $v = shift;
  my ($unused, $type) = pairfirst { $v =~ /^$a$/ } @regexes;
  return $type // 'string';
}

sub check {
  my $v = shift;
  say "$v: " . get_type($v) . " " . get_type2($v);
}

my @test_cases = qw(200 -200 200.5 2.5e100 2017-03-17 2017-03-17T09:54:00);
push(@test_cases, 'A man, a plan, a canal, panama');

foreach my $tc (@test_cases) {
  check($tc)
}

my $bench = Dumbbench->new( initial_runs => 10, target_rel_precision => 0.005 );

$bench->add_instances(
  Dumbbench::Instance::PerlSub->new(name => 'dispatch', code => sub { for (0 .. 1000) { get_type($_) foreach (@test_cases); } 1; }),
  Dumbbench::Instance::PerlSub->new(name => 'linear', code => sub { for (0 .. 1000) { get_type2($_) foreach (@test_cases); } 1; })
);

$bench->run;
$bench->report;
