#!/bin/sh

uconv -x any-nfd </usr/share/dict/american-english \
    | perl -ne 's/[\x80-\xff]//g; print lc' \
    | grep -v "'" \
    | sort \
    | uniq > words.txt

