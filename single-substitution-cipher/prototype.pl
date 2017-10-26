use v5.24;

use strict;
use warnings;

use experimental qw(signatures);

use List::Util qw(uniq);
use Data::Printer;

# Stores the list of all words binned by word length.
my %words;

# Read in list of valid words
{
  open(my $words, '<:encoding(utf-8)', 'words.txt') or die "Couldn't open word list. Run ./sanitize-word-list.sh to generate";
  # open(my $words, '<:encoding(utf-8)', 'seven-words/american-english-filtered') or die;
  while(<$words>) {
    chomp;
    push(@{$words{length $_}}, lc);
  }
}

# Read in the cipher text
my @words = uniq map { lc } map { chomp; split } <ARGV>;
close ARGV;

# Extract the list of unique letters from the words
my @letters = sort { $a cmp $b} uniq map { split(//) } @words;

print STDERR join('-', @letters) . "\n";

# Initially any cipher letter can map to anything
my %mapping = map { $_ => ['a' .. 'z'] } @letters;

# Set up the list of possible words that a given cipher word could be encoding
my @possible_words = map { $words{length $_} } @words;

# The index of what letters exist in what words so we know which word sets to constrain if
# we make a choice on the letter.
my %contains = map {
  my $letter = $_;
  $letter => [grep { $words[$_] =~ /$letter/ } 0 .. $#words]
} @letters;


# Using the mapping set, turns the cipher word into a regular expression to filter out
# words that no longer match it.
sub make_re($word) {
  my $re = '^';
  my (%count, %seen);
  my @letters = split(//, $word);
  for my $l (@letters) {
    ++$count{$l};
  }
  my $i = 1;
  for my $l (@letters) {
    # Every letter in the cipher word either matches a single letter, a set of letters, or
    # a backreference to a previous match (we would overflow if someone had a 20 letter
    # word of duplicates, but such words don't exist)
    if ($seen{$l}) {
      # back reference
      $re .= '\\' . $seen{$l};
    } else {
      my $part;
      if (ref $mapping{$l}) {
        # set of letters
        $part = '[' . join('', @{$mapping{$l}}) . ']';
      } else {
        # Single letter
        $part = $mapping{$l};
      }
      if ($count{$l} > 1) {
        # This letter shows up multiple times, so set up the back reference
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

# When only one letter is possible, this function ensures that letter will be removed from
# all other letter sets. Returns a success bit and a potentially non-unique set of letters
# that should have their word constraints tightented.
sub make_unique($letter) {
  my $mapped = $mapping{$letter};
  die 'Letter is not unique' if ref $mapped;
  my @result = $letter;
  for my $l2 (@letters) {
    next if $l2 eq $letter;
    my @m = ref $mapping{$l2} ? @{$mapping{$l2}} : ($mapping{$l2}, );
    my @new = grep { $_ ne $mapped } @m;
    # Found an impossible mapping
    return (0, ) if (@new == 0);
    if (@m != @new) {
      # The mapped letter was in the cipher's set
      if (@new == 1) {
        # Excellent, a new unique letter fell out of the selection, so repeat the process
        $mapping{$l2} = $new[0];
        my ($success, @res) = make_unique($l2);
        return (0, ) unless $success;
        push(@result, @res);
      }
      push(@result, $l2);
    }
  }
  return (1, @result);
}

# Tighten the constraints on the cipher letters by going through the possible words the
# provided cipher word could map to and removing impossible mappings for that cipher
# letter. Returns success and a possibly non-unique set of letters that can have their
# word list tightened.
sub constrain_letters($index) {
  my @b = split(//, $words[$index]);
  my %seen;
  my @shrunk;
  # Go through all the possible words and create a listing of all the letters that the
  # cipher letter in that position map. Those then become the new set of letters.
  for my $word (@{$possible_words[$index]}) {
    my @l = split(//, $word);
    for my $i (0 .. $#l) {
      $seen{$b[$i]}{$l[$i]} = 1;
    }
  }

  # Now that we have created the full mapping list, constrain the set of letters that can
  # occur
  for my $l (keys %seen) {
    my @available = keys %{$seen{$l}};
    my @old = ref $mapping{$l} ? @{$mapping{$l}} : ($mapping{$l}, );
    # Since any previous calls to make unique could have reduced the set of letters
    # available to us between checking the word set and now, we have to ensure we don't
    # accidentally add letters that we already removed.
    @available = grep { my $a = $_; grep { $a eq $_ } @old } @available;
    if (@available == 0) {
      # Excellent, found an impossible mapping
      return (0, );
    }
    if (@available < @old) {
      # We shrunk the size of the letter set, so update the variable and put it on the
      # list of letters we touched.
      if (@available == 1) {
        # Made a decision on the letter, update our sibling cipher letters
        $mapping{$l} = $available[0];
        my ($success, @l) = make_unique($l);
        return (0, ) unless $success;
        push(@shrunk, @l);
      } else {
        $mapping{$l} = [sort @available];
        push(@shrunk, $l);
      }
    }
  }
  return (1, @shrunk);
}

# Given a list of letters to check, checks all the word lists that include those letters
# for validity and then constrains the letters based on the updated word lists. Returns
# success.
sub check_constraints(@letters) {
  my %follow_up;
  for my $wi (uniq map { @{$contains{$_}} } @letters) {
    my $word_re = make_re($words[$wi]);
    $possible_words[$wi] = [ grep { /$word_re/ } @{$possible_words[$wi]} ];
    return 0 if (@{$possible_words[$wi]} == 0);
    my ($success, @follow_up) = constrain_letters($wi);
    return 0 unless $success;
    $follow_up{$_}++ foreach (@follow_up);
  }
  return 1 unless keys %follow_up;
  return check_constraints(keys %follow_up);
}


# For debug printing so we know how deep the search has travelled. Not really neccessary
# now that the code is fast, but there's little reason to remove it and it gives some idea
# of the speed of constraint.
my $max_index = 0;
sub pick($index) {
  if ($index == @letters) {
    # Made it to the end of the line! Print out the letter mapping and then what those
    # words became.
    my ($from, $to) = ('', '');
    for my $f (sort keys %mapping) {
      my $t = $mapping{$f};
      die 'Found solution in invalid state' if ref $t;
      $from .= $f;
      $to .= $t;
    }
    say $from;
    say $to;
    foreach my $wi ( 0 .. $#possible_words) {
      my $word_list = $possible_words[$wi];
      # It shouldn't be possible to have a cipher map to multiple words right now, but no
      # reason not to be safe.
      say "$words[$wi]: " . join('|', @{$word_list});
    }
    say '----------';
    return;
  }
  if ($index > $max_index) {
    # Debug print
    print STDERR "Assigned $index letters\n";
    for my $i ( 0 .. $#possible_words) {
      print STDERR "$words[$i]: " . (scalar @{$possible_words[$i]}) . "\n";
    }
    $max_index = $index;
  }
  my $letter = $letters[$index];
  return pick($index + 1) unless ref $mapping{$letter}; # Nothing to do, pass it up the line!
  my @word_backup = @possible_words;
  my %mapping_backup = %mapping;
  my @available = @{$mapping{$letter}};
  # Try all possible mappings
  for my $destination (@available) {
    $mapping{$letter} = $destination;
    my ($success, @check) = make_unique($letter);
    $success = check_constraints(@check) if $success;
    pick($index + 1) if $success;
    @possible_words = @word_backup;
    %mapping = %mapping_backup;
  }
}

p(%contains);

@letters = sort { @{$contains{$b}} <=> @{$contains{$a}} || $a cmp $b } @letters;

pick(0);
