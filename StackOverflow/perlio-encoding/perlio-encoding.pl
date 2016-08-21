use strict;
use warnings;

# http://stackoverflow.com/questions/38760455/how-to-detect-malformed-utf-8-at-the-end-of-the-file

use feature qw(say);
use strict;
use warnings;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my $bytes = "\x{61}\x{E5}\x{61}";  # 3 bytes in iso 8859-1: aåa
test_read_invalid( $bytes );
$bytes = "\x{61}\x{E5}";  # 2 bytes in iso 8859-1: aå
test_read_invalid( $bytes );

sub test_read_invalid {
    my ( $bytes ) = @_;
    say "Running test case..";
    my $fn = 'test.txt';
    open ( my $fh, '>:raw', $fn ) or die "Could not open file '$fn': $!";
    print $fh $bytes;
    close $fh;
    my $str = '';
    open ( $fh, "<:encoding(utf-8)", $fn ) or die "Could not open file '$fn': $!";
    $str = length do { local $/; <$fh> };
    binmode $fh, ':pop';
    my $remainder = do { local $/; <$fh>};
    close $fh;
    say "Read string: '$str'\n";
    say "Invalid" if length $remainder;
}
