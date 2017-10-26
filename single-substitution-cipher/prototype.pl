use v5.24;

use strict;
use warnings;

use experimental qw(signatures);

use List::Util qw(uniq);


my %words;

{
  open(my $words, '<:encoding(utf-8)', '/usr/share/dict/american-english') or die;
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    push(@{$words{length $_}}, lc);
  }
}

my @words = map { lc } map { chomp; split } <ARGV>;

my @letters = sort { $a cmp $b} uniq map { split(//) } @words;

say join('-', @letters);

my %mapping = map { $_ => '.' } @letters;

my @possible;

@possible = map { $words{length $_} } @words;

my %contains = map {
  my $letter = $_;
  $letter => [grep { $words[$_] =~ /$letter/ } 0 .. $#words]
} @letters;

sub trans($word) {
  my ($from, $to) = ('', '');
  for my $f (keys %mapping) {
    my $t = $mapping{$f};
    $from .= $f;
    $to .= $t;
  }
  return eval "\$word =~ tr/$from/$to/r";
}

my $max_index = 0;

sub pick($index, @available) {
  if ($index == @letters) {
    my ($from, $to) = ('', '');
    for my $f (keys %mapping) {
      my $t = $mapping{$f};
      $from .= $f;
      $to .= $t;
    }
    say $from;
    say $to;
    foreach my $word_list (@possible) {
      say join('|', @{$word_list});
    }
    say '----------';
    return;
  }
  if ($index > $max_index) {
    print STDERR "Assigned $index letters\n";
    for my $i ( 0 .. $#possible) {
      print STDERR "$words[$i]: " . (scalar @{$possible[$i]}) . "\n";
    }
    $max_index = $index;
  }
  my $letter = $letters[$index];
  my @backup = @possible;
  for my $destination (@available) {
    $mapping{$letter} = $destination;
    my $success = 1;
    for my $wi (@{$contains{$letter}}) {
      my $trans = trans($words[$wi]);
      $possible[$wi] = [ grep { /^$trans$/ } @{$possible[$wi]} ];
      if (@{$possible[$wi]} == 0) {
        $success = 0;
        last;
      }
    }
    if ($success) {
      pick($index + 1, grep { $_ ne $destination } @available);
    }
    @possible = @backup;
  }
  $mapping{$letter} = '.';
}

use Data::Printer;
p(%contains);

@letters = sort { @{$contains{$b}} <=> @{$contains{$a}} || $a cmp $b } @letters;

pick(0, 'a' .. 'z');
