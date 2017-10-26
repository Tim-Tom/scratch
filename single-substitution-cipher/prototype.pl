use v5.24;

use strict;
use warnings;

use experimental qw(signatures);

use List::Util qw(uniq);
use Data::Printer;

my %words;

{
  open(my $words, '<:encoding(utf-8)', 'words.txt') or die "Couldn't open word list. Run ./sanitize-word-list.sh to generate";
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    push(@{$words{length $_}}, lc);
  }
}

my @words = map { lc } map { chomp; split } <ARGV>;

close ARGV;

my @letters = sort { $a cmp $b} uniq map { split(//) } @words;

say join('-', @letters);

my %mapping = map { $_ => ['a' .. 'z'] } @letters;

my @possible_words;

@possible_words = map { $words{length $_} } @words;

my %contains = map {
  my $letter = $_;
  $letter => [grep { $words[$_] =~ /$letter/ } 0 .. $#words]
} @letters;

sub make_re($word) {
  my $re = '^';
  my (%count, %seen);
  my @letters = split(//, $word);
  for my $l (@letters) {
    ++$count{$l};
  }
  my $i = 1;
  for my $l (@letters) {
    if ($seen{$l}) {
      $re .= '\\' . $seen{$l};
    } else {
      my $part;
      if (ref $mapping{$l}) {
        $part = '[' . join('', @{$mapping{$l}}) . ']';
      } else {
        $part = $l;
      }
      if ($count{$l} > 1) {
        $re .= "($part)";
        $seen{$l} = $i++;
      } else {
        $re .= $part;
      }
    }
  }
  $re .= '$';
  return qr/$re/;
}

my $max_index = 0;

sub constrain_letters($index) {
  my @b = split(//, $words[$index]);
  my %seen;
  my @shrunk;
  for my $word (@{$possible_words[$index]}) {
    my @l = split(//, $word);
    for my $i (0 .. $#l) {
      $seen{$b[$i]}{$l[$i]} = 1;
    }
  }
  for my $l (keys %seen) {
    my @available = keys %{$seen{$l}};
    my @old = ref $mapping{$l} ? @{$mapping{$l}} : ($mapping{$l}, );
    if (@available < @old) {
      if (@available == 1) {
        $mapping{$l} = $available[0];
      } else {
        $mapping{$l} = [sort @available];
      }
      push(@shrunk, $l);
    }
  }
  return @shrunk;
}

sub check_constraints($letter) {
  my %follow_up;
  for my $wi (@{$contains{$letter}}) {
    my $word_re = make_re($words[$wi]);
    $possible_words[$wi] = [ grep { /$word_re/ } @{$possible_words[$wi]} ];
    return 0 if (@{$possible_words[$wi]} == 0);
    $follow_up{$_}++ foreach (constrain_letters($wi));
  }
  return !grep { !check_constraints($_) } keys %follow_up;
}

sub pick($index) {
  if ($index == @letters) {
    my ($from, $to) = ('', '');
    for my $f (keys %mapping) {
      my $t = $mapping{$f};
      die if ref $t;
      $from .= $f;
      $to .= $t;
    }
    say $from;
    say $to;
    foreach my $word_list (@possible_words) {
      say join('|', @{$word_list});
    }
    say '----------';
    return;
  }
  if ($index > $max_index) {
    print STDERR "Assigned $index letters\n";
    for my $i ( 0 .. $#possible_words) {
      print STDERR "$words[$i]: " . (scalar @{$possible_words[$i]}) . "\n";
    }
    $max_index = $index;
  }
  my $letter = $letters[$index];
  return pick($index + 1) unless ref $mapping{$letter};
  my @word_backup = @possible_words;
  my %mapping_backup = %mapping;
  my @available = @{$mapping{$letter}};
  for my $destination (@available) {
    $mapping{$letter} = $destination;
    my $success = check_constraints($letter);
    pick($index + 1) if $success;
    @possible_words = @word_backup;
    %mapping = %mapping_backup;
  }
}

p(%contains);

@letters = sort { @{$contains{$b}} <=> @{$contains{$a}} || $a cmp $b } @letters;

pick(0);
