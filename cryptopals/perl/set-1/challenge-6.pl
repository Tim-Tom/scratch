use strict;
use warnings;

use v5.24;

use experimental 'signatures';

use File::Slurp qw(read_file);
use MIME::Base64;

my %hd;

for my $n (0 .. 255) {
  my $sum = 0;
  my $repr = $n;
  while($repr) {
    $sum += $repr & 1;
    $repr >>= 1;
  }
  $hd{chr $n} = $sum;
}

my %expected_freq;

{
  open(my $freqs, '<:encoding(ascii)', '../../freq/graph.txt');
  my $total = 0;
  while(<$freqs>) {
    chomp;
    my ($key, $count) = (substr($_, 0, 1), substr($_, 3));
    $expected_freq{$key} = $count;
    $total += $count;
  }
  %expected_freq = map { $_ => ($expected_freq{$_} / $total) } keys %expected_freq;
}


sub decode_xor($str, $key) {
  my $len = length $str;
  my $stride = length $key;
  my $leftover = $len % $stride;
  my $extended = ($key x ($len / $stride)) . substr($key, 0, $leftover);
  return $str ^ $extended;
}


my $data = decode_base64(read_file('../../data/s1-c6.txt', { binmode => ':raw' }));
my @data = split(//, $data);
my @strides;
for my $stride (1 .. 40) {
  my @parts = map { substr($data, $_, $stride) } (0, $stride, 2*$stride, 3*$stride);
  my $distance = 0;
  for my $i (0 .. 3) {
    for my $j ($i+1 .. 3) {
      foreach my $repr (split(//, $parts[$i] ^ $parts[$j])) {
        $distance += $hd{$repr};
      }
    }
  }
  $distance /= $stride;
  push(@strides, [$stride, $distance]);
}
@strides = sort { $a->[1] <=> $b->[1] } @strides;

my @keys = map { chr } 0 .. 255;

use Data::Printer;

stride: for my $pkg (@strides[0 .. 2]) {
  my ($stride, $distance) = @$pkg;
  my @candidates = map { { map { $_ => {} } @keys }; } 1 .. $stride;
  my @totals;
  my $index = 0;
  foreach my $encoded (@data) {
    my $candidate = $candidates[$index];
    $totals[$index]++;
    my @for_delete;
    foreach my $key (keys %$candidate) {
      my $decoded = $key ^ $encoded;
      my $od = ord($decoded);
      if ($od < 0x09 || ($od > 0x0d && $od < 0x20) || $od > 0x7e) {
        delete $candidate->{$key};
      } else {
        $decoded =~ s/\s/ /;
        $candidate->{$key}{$decoded}++;
      }
    }
    next stride unless (keys %$candidate);
    $index = ($index + 1) % $stride;
  }
  my $secret = '';
  for $index (0 .. $#candidates) {
    my $candidate = $candidates[$index];
    my $total = $totals[$index];
    my $best_key = '*';
    my $best_cost = 1e10;
    # say "$index: $total";
    foreach my $key (keys %$candidate) {
      # say "\t$key";
      my $cost = 0;
      my $cand = $candidate->{$key};
      foreach my $chr (keys %expected_freq) {
        my $freq = ($cand->{$chr} // 0) / $total;
        $cost += ($expected_freq{$chr} - $freq)**2;
      }
      if ($cost < $best_cost) {
        $best_key = $key;
        $best_cost = $cost;
      }
    }
    $secret .= $best_key;
  }
  say "$stride: secret: $secret";
  say decode_xor($data, $secret);
}
