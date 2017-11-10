use v5.24;

use strict;
use warnings;

use experimental 'signatures';

use List::Util qw(uniq);

sub map_word($word) {
  my $i = 'a';
  my %s;
  return join('', map {
    $s{$_} = $i++ unless exists $s{$_};
    $s{$_};
  } split(//, $word));
}

chomp(my @content = <ARGV>);
close ARGV;

my @words = sort { (length $a) <=> (length $b) or $a cmp $b } uniq map { lc } map { /([a-zA-Z]+)/g } @content;

my @letters = sort { $a cmp $b} uniq map { split(//) } @words;
my @patterns = sort { (length $a) <=> (length $b) or $a cmp $b } uniq map { map_word($_) } @words;

my %contains = map {
  my $letter = $_;
  $letter => [grep { $words[$_] =~ /$letter/ } 0 .. $#words]
} @letters;

@letters = sort { @{$contains{$b}} <=> @{$contains{$a}} || $a cmp $b } @letters;

my @pattern_index = map { { from =>-1, to => -1 } } 0 .. (length $patterns[$#patterns]);

{
  my $last = 0;
  for my $i (0 .. $#patterns) {
    my $pattern = $patterns[$i];
    my $len = length $pattern;
    if ($len != $last) {
      $pattern_index[$last]{to} = $i - 1;
      $pattern_index[$len]{from} = $i;
    }
    $last = $len;
  }
  $pattern_index[$last]{to} = $#patterns;
}

my @sanitized_content = map { s/\\/\\\\/g; s/"/\\"/g; $_ } @content;

print "const char* const cipher_text =";
for my $content (@sanitized_content) {
  printf qq( \\\n    "$content\\n");
}
say ";";

print <<"END";
typedef struct pattern_range {
    int from;
    int to;
} pattern_range;
END

print "\n";
printf qq(#define NUM_LETTERS %d\n), scalar @letters;
printf qq(const char* const letters = "%s";\n), join('', @letters);


my %letter_mapping = map { $letters[$_] => $_ } 0 .. $#letters;

print "\n";
printf qq(#define NUM_PATTERNS %d\n), scalar @patterns;
printf qq(#define MAX_PATTERN_SIZE %d\n), $#pattern_index;
printf qq(const char* const patterns[NUM_PATTERNS] = {%s};\n), join(', ',  map { qq("$_") } @patterns);
printf qq(const int pattern_index[MAX_PATTERN_SIZE + 1] = {%s};\n), join(', ', map { sprintf '{%d,%d}', @{$_}{qw(from to)} } @pattern_index);

print <<"END";
typedef struct letter_mapping {
  int wordIndex,
  int encryptedIndex;
} letter_mapping_t;

typedef struct word {
  const char* const word;
  const char* const pattern;
  int num_mappings;
  const letter_mapping_t* const mappings
} word_t;

END

my @word_structs;
{
  my %pattern_indexes = map { $patterns[$_] => $_ } 0 .. $#patterns;
  for my $word (@words) {
    my $pattern = map_word $word;
    my @w = split(//, $word);
    my @p = split(//, $pattern);
    my $c = "\0";
    my @map = map { sprintf '{%d, %d}', $_->[0], $letter_mapping{$_->[1]} }
      grep { my $r = ($_->[2] gt $c); $c = $_->[2]; $r }
      map { [$_,  $w[$_], $p[$_] ] } 0 .. $#w;
    my $map = join(', ', @map);
    push(@word_structs, [$word, $pattern_indexes{$pattern}, (scalar @map), $map]);

  }
};

printf qq(#define NUM_WORDS %d\n), scalar @words;
printf qq(const word_t words[NUM_WORDS] = {\n  %s\n};\n), join(",\n  ", map { sprintf '{"%s", patterns[%d], %d, {%s}}', @{$_} } @word_structs);

printf qq(const inclusion_t inclusions[NUM_LETTERS] = {%s}\n), join(", ", map { sprintf "{%d, {%s}}", (scalar @$_), join(", ", @$_) } @contains{@letters});
