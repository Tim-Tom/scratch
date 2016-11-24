use v5.22;

sub add {
  my ($trie, $word, $remain) = @_;
  if (length($remain) == 0) {
    $trie->{final} = $word;
  } else {
    my $letter = substr($remain, 0, 1);
    my $rest = substr($remain, 1);
    add($trie->{$letter} //= {}, $word, $rest);
  }
}

sub find_partial {
  my ($trie, $word) = @_;
  if (length $word == 0) {
    return $trie;
  } else {
    my $letter = substr($word, 0, 1);
    my $rest = substr($word, 1);
    if (defined $trie->{$letter}) {
      return find_partial($trie->{$letter}, $rest);
    }
  }
  return;
}

sub find_words {
  my ($trie, @partials) = @_;
  if ($trie->{final}) {
    say $trie->{final};
  }
  for my $i (0 .. $#partials) {
    my $new_trie = find_partial($trie, $partials[$i]);
    if (defined $new_trie) {
      find_words($new_trie, @partials[0 .. $i - 1], @partials[$i + 1 .. $#partials]);
    }
  }
}

my $trie = {};

{
  open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
  while(<$words>) {
    chomp;
    add($trie, lc $_, lc $_);
  }
}

find_words($trie, @ARGV);
