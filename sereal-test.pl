use strict;
use warnings;

use v5.24;

use List::Util qw(pairmap);

use Sereal::Encoder;
use Sereal::Decoder;

my $global_id;
my %people = pairmap { my $id = ++$global_id; return $id => { firstname => $a, lastname => $b, id => $id}; } 'A' .. 'Z';

foreach my $person (values %people) {
  my $id = $person->{id};
  $person->{neighbors} = [];
  push(@{$person->{neighbors}}, $people{$id - 1}) if exists $people{$id - 1};
  push(@{$person->{neighbors}}, $people{$id + 1}) if exists $people{$id + 1};
}

my $encoder = Sereal::Encoder->new({ compress => 0 });
my $decoder = Sereal::Decoder->new({  });

my $encoded = Sereal::Encoder::sereal_encode_with_object($encoder, \%people);
my $length = length $encoded;

my $people = Sereal::Decoder::sereal_decode_with_object($decoder, $encoded);

say "Encoded length was $length bytes";
