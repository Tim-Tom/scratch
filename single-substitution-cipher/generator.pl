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

{
  my %letter_mapping = map { $letters[$_] => $_ } 0 .. $#letters;
  printf qq(const int letter_mapping[26] = {%s};\n), join(', ', map { $letter_mapping{$_} // -1 } 'a' .. 'z');
}

print "\n";
printf qq(#define NUM_PATTERNS %d\n), scalar @patterns;
printf qq(#define MAX_PATTERN_SIZE %d\n), $#pattern_index;
printf qq(const char* const patterns[NUM_PATTERNS] = {%s};\n), join(', ',  map { qq("$_") } @patterns);
printf qq(const int pattern_index[MAX_PATTERN_SIZE + 1] = {%s};\n), join(', ', map { sprintf '{%d,%d}', @{$_}{qw(from to)} } @pattern_index);

print <<"END";
typedef struct word {
    const char* const word;
    const char* const pattern;
} word_t;
END

my @word_patterns = do {
  my %patterns = map { $patterns[$_] => $_ } 0 .. $#patterns;
  map { [$_, $patterns{map_word $_}] } @words;
};

printf qq(#define NUM_WORDS %d\n), scalar @words;
printf qq(const word_t words[] = {%s};\n), join(', ', map { sprintf '{"%s", patterns[%d]}', @{$_} } @word_patterns);
