#!/bin/sh

# Shifts every letter over one letter in sequence to create a basic cipher.

perl -pe 'tr/a-zA-Z/b-zaB-Zb/'
