use v6;

my KeySet $seen .= new;

class Trie is Hash {

     method new() {
         self.bless(*, Hash.new);
    }

    method Insert(Str $word) {
        my $w = $word;
        if ($w ~~ /\*$/) {
            $w .= substr(0, * - 1);
            $seen{$w} = True;
        }
        my $end = self.Create($w.comb.sort);
        $end<words> //= [];
        $end<words>.push($w) unless $w ~~ $end<words>;
    }

    method Create(@ [$letter, *@rest]) {
        self{$letter} //= Trie.new();
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
        say "Matching [$letter, @rest[]]";
        self{$letter}.?Matches(@rest);
        say @(self{$letter}<words>) if self{$letter}.?exists('words');
        self.Matches(@rest.grep: { $_ ne $letter });
        1;
    }
}

sub TimeFormat(DateTime $dt) {
    sprintf '%02d:%02d:%02d', $dt.hour, $dt.minute, $dt.whole-second;
}

sub MAIN(Str $filename) {
    my $trie = Trie.new;

    my $start = TimeFormat(DateTime.now);
    say "Creating Trie: $start";
    {
        my $fh = open($filename);
        $trie.Insert($_) while ($_ = $fh.get);
    }
    my $end = TimeFormat(DateTime.now);
    say "Done creating Trie: $end";

    # my Bool $modified;

    loop {
        my $letters = prompt("Letters: ");
        last unless $letters;
        next unless $letters.chars == 6;
        my @matches = $trie.Matches($letters.comb.sort);
        .say for @matches.sort;
    }
}
