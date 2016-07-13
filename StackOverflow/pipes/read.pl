use strict;
use warnings;

while (1) {
    seek *STDIN, 0, 0;
    print "eof: ".eof(*STDIN)."\n";
    if (!eof(*STDIN)) {
        print "Cleared!";
        while(<STDIN>) {}
        print "eof: ".eof(*STDIN)."\n";
    }
    sleep 2;
}
