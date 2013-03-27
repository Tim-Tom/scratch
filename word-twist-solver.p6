use v6;

my KeySet $seen .= new;

class Trie does Associative {
    has %!elems;

    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method Real { %!elems.Numeric.Real }
    method hash { %!elems.hash }
    method at_key($k) { %!elems{$k} }
    method exists_key($k) { self.exists($k) }
    method delete_key($k) { %!elems.delete($k) }

    method Insert(Str $word) {
        my $w = $word;
        if ($w ~~ /\*$/) {
            $w .= substr(0, * - 1);
            $seen{$w} = True;
        }
        my $end = self.Create($w.comb.sort);
        $end.Add-Word($w);
    }

    method Create(@ [$letter, *@rest]) {
        %!elems{$letter} //= Trie.new();
        return self{$letter}.Create(@rest) if @rest;
        return self{$letter};
    }

    multi method Remove($word) {
        self.Remove($word, $word.comb.sort);
    }

    multi method Remove($word, @ []) {
        self<words> .= grep: * ne $word;
        self.delete: 'words' unless +self<words>;
        return self;
    }

    multi method Remove($word, @ [$letter, *@rest]) {
        if self{$letter}.Remove($word, @rest) {
            self.delete: $letter;
        }
        return self;
    }

    multi method Matches(@ []) {
    }

    multi method Matches(@ [$letter, *@rest]) {
        self{$letter}.?Matches(@rest);
        say @(self{$letter}<words>) if self{$letter}.?exists('words');
        self.Matches(@rest.grep: { $_ ne $letter });
        1;
    }

    method Add-Word(Str $word) {
        %!elems<words> //= [];
        %!elems<words>.push($word) unless $word ~~ %!elems<words>
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
        my @matches = $trie.Matches($letters.comb.sort);
        .say for @matches.sort;
    }
}
