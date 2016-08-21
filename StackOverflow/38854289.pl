# http://stackoverflow.com/questions/38854289/perl-lwp-submitting-a-query-error-400-url-must-be-absolute
use strict;
use warnings;

use LWP::UserAgent;
my $entry = $ARGV[0] or die;
my $url = 'http://genome.ucsc.edu/cgi-bin/hgBlat?hgsid=502819467_w6Oj12MTSqOIIJuSjAKsza4x9yeA&command=start';
my $browser = LWP::UserAgent->new;
my $response = $browser->post(
  '$url',
  [
    'sequence'  => $entry,
    'search' => 'submit'
  ],
);
die "Error: ", $response->status_line
 unless $response->is_success;
