#!/bin/sh

# Sanitize the word list by ensuring everything is a decomposed letter, then stripping out
# combining characters and ensuring every word is lowercase. Remove contractions since I
# assume everything is alphabettic and remove duplicates created by this operation.

uconv -x any-nfd </usr/share/dict/american-english \
    | perl -ne 's/[\x80-\xff]//g; print lc' \
    | grep -v "'" \
    | sort \
    | uniq > words.txt

