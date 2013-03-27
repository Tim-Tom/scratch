use v6;

class Trie {
    has %!elems;
    has KeySet $!seen = KeySet.new;

    method Insert(Str $word) {
        my $value = $word;
        if ($value ~~ /\*$/) {
            $value .= substr(0, * - 1);
            $!seen{$value} = True;
        }
        my $key = $value.comb.sort.join;
        %!elems{$key} //= [];
        %!elems{$key}.push($value) unless $value ~~ %!elems{$key};
    }

    method Remove(Str $word) {
        my $key = $word.comb.sort.join;
        %!elems{$key} .= grep: * ne $word;
    }

    method Matches(Str $letters) {
        my @choices = gather self.GetKeys($letters.comb.sort, '');
        return %!elems{@choices}.map: *.flat;
    }

    multi method GetKeys(@ [], Str $prefix) {
        take $prefix if %!elems.exists: $prefix;
    }

    multi method GetKeys(@ [$hd, *@tail], Str $prefix) {
        self.GetKeys(@tail, $prefix ~ $hd);
        self.GetKeys(@tail.grep({$_ ne $hd}), $prefix);
    }

    method Mark(Str $word) {
        $!seen{$word} = True;
    }
}

# sub TimeFormat(DateTime $dt) {
#     sprintf '%02d:%02d:%02d', $dt.hour, $dt.minute, $dt.whole-second;
# }

sub MAIN(Str $filename) {
    my $trie = Trie.new;

    print qx/date/;
    # my $start = TimeFormat(DateTime.new(Instant.now));
    say "Creating Trie.";
    {
        my $fh = open($filename);
        $trie.Insert($_) while ($_ = $fh.get);
    }
    print qx/date/;
    # my $end = TimeFormat(DateTime.new(Instant.now));
    say "Done creating Trie.";

    # my Bool $modified;

    loop {
        my $letters = prompt("Letters: ");
        last unless $letters;
        next unless $letters.chars == 6;
        my @matches = $trie.Matches($letters);
        .say for @matches.sort: { $^a.chars <=> $^b.chars || $^a cmp $^b };
    }
}
