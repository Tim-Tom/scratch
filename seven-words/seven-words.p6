use v6;

class Trie-Node {
    has %.follows is rw;
    has Str $.final is rw;
    method add(Str $word, Str $remain = $word) {
        if $remain.chars == 0 {
            $.final = $word;
            return;
        }
        my $letter = $remain.substr(0, 1);
        my $rest = $remain.substr(1);
        (%.follows{$letter} //= Trie-Node.new()).add($word, $rest);
    }
    
    method find-partial(Str $word) {
        if $word.chars == 0 {
            return self;
        }
        my $letter = $word.substr(0, 1);
        my $rest = $word.substr(1);
        return %.follows{$letter}.?find-partial($rest);
    }

    method find-words(@partials) {
        if ($.final) {
            say $.final;
        }
        for @partials.kv -> $i, $word {
            self.find-partial($word).?find-words(@partials[grep * != $i, 0 .. *]);
        }
    }
}

my Trie-Node $trie .= new();

# for '/usr/share/dict/american-english'.IO.lines() -> $word {
for <inc inclusive cr crash crashed crete hygrometers adz ed rs ash ashed> -> $word {
# for 'the-word-ginger.txt'.IO.lines() -> $word {
    $trie.add($word.lc);
    my $found = $trie.find-partial($word.lc);
    die "$word" unless defined $found;
}

# $trie.find-words(@*ARGS);
$trie.find-words(<ive lus uki inc cr rom hyg adz ed rs ash ete>);
