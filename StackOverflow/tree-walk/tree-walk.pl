# http://stackoverflow.com/questions/38419449/walk-tree-structure-and-enumerate-all-paths

use strict;
use warnings;

# Rather than explicitly managing parents and children, you can quite literally store the whole tree
# in memory as a tree for more native iteration through it. For initialization you also need a
# global node list so you can do the lookup.
my @host_names;
my %tree;
my %nodes;

# You can use scalar variables ($fh) instead of global names (fh) for filehandles. This means that
# they will automatically close when they go out of scope if they are used in functions or loops and
# will be checked for existance by use strict which is great for me personally because I make
# variable name typos all the time.
open(my $parent,"<","parent.txt") or die "Could not open 'parent.txt' for read: $!";
open(my $child,"<","child.txt") or die "Could not open 'child.txt' for read: $!";

# As an example of that, this do block will read the host file into host_names and then
# automatically close the file once the block has exited.
@host_names = do {
  open(my $host, '<', 'data.txt') or die "Could not open 'data.txt' for read: $!";
  <$host>;
};
chomp(@host_names);

foreach my $host_name (@host_names) {
  $tree{$host_name} = $nodes{$host_name} = {};
}

# In the ultimate confusing thing that you only have to learn once, you actually want to use while
# instead of for to loop through files. Using a for loop will read the whole file into memory, split
# it on newline and then iterate through it.  While loops will read the file one line at a time.  It
# doesn't make a difference here, but if you were reading a 3GB file, you would notice.
while(<$parent>) {
  chomp;
  # You can automatically unpack arrays in perl and the default second argument for split is $_
  my ($parent, $child) = split(/###/);
  die "$parent does not exist" unless exists $nodes{$parent};
  die "$child already defined" if     exists $nodes{$child};
  $nodes{$parent}{$child} = $nodes{$child} = {};
}

# This loop is exactly the same as above now, you could really make the two files the same and see
# no differences (except that I changed split invocations to give you some more information)
while(<$child>) {
  chomp;
  # If you want, you can also specify the maximum number of items split will look to unpack. If it's
  # not specified and perl sees you unpack it immediately, it will default to the number of items in
  # your list plus 1 (in your case 3). It does this because the behavior is the same either way, and
  # it saves work if you have 200 splittable things in your line. We know there will be exactly two
  # items, so we can save it even more work and tell it that.
  my ($parent, $child) = split(/###/, $_, 2);
  die "$parent does not exist" unless exists $nodes{$parent};
  die "$child already defined" if     exists $nodes{$child};
  $nodes{$parent}{$child} = $nodes{$child} = {};
}

# You can specify what you're iterating over in a for/foreach loop (They are synonyms in perl)
# instead of renaming it on the first line of the loop.
foreach my $host (@host_names) {
  walk($tree{$host}, $host);
}

# With our tree now actually being a tree in memory, our 'derive' function can be a true tree walk.
sub walk {
  my ($node, $host, @path) = @_;
  for my $child (sort keys %$node) {
    if (@path > 0) {
      local $" = " + "; # comment to fix stack overflow highlighter "
      print "$host === $child ==== @path + $child\n";
    }
    walk($node->{$child}, $host, @path, $child);
  }
}
