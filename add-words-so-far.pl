use Modern::Perl;
use autodie qw(:all);

use Data::Printer;

sub munch {
    open(my $fh, '<:encoding(utf-8)', $_[0]);
    chomp(my @results = <$fh>);
    return @results;
}

my @seen = sort {$a cmp $b } munch $ARGV[0];
my @words = sort {$a cmp $b } munch $ARGV[1];

open(my $output, '>', $ARGV[1]);

my $seen = shift @seen;
my $word = shift @words;

while($seen && $word) {
    if ($seen eq $word) {
        say $output $word.'*';
        $seen = shift @seen;
        $word = shift @words;
    } elsif ($seen lt $word) {
        say STDERR "0: $seen isn't in the word list any more...";
        $seen = shift @seen;
    } else {
        say $output $word;
        $word = shift @words;
    }
}

while ($seen) {
    say STDERR "1: $seen isn't in the word list any more...";
    $seen = shift @seen;
}
while ($word) {
    say $output $word;
    $word = shift @words;
}
