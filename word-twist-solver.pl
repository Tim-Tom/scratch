use Modern::Perl;
use Data::Printer;
use autodie qw(:all);

my $trie = {};

my %seen;

sub insert {
    my ($root, $word) = @_;
    sub insert_rec {
        my $current = shift;
        return $current unless @_;
        my $letter = shift;
        my $next = $current->{$letter} //= {};
        return insert_rec($next, @_);
    }
    my $end = insert_rec($root, sort split //, $word);
    push(@{$end->{words}}, $word);
}

sub remove {
    my ($root, $word) = @_;
    my @stack;
    sub remove_rec {
        my ($current, $word, $letter, @rest) = @_;
        if ($letter) {
            my $next = $current->{$letter};
            if (remove_rec($next, $word, @rest)) {
                delete $current->{$letter};
            }
        }
        else {
            my @words = grep { $_ ne $word } @{$current->{words}};
            if (@words) {
                $current->{words} = [@words];
            } else {
                delete $current->{words};
            }
        }
        return keys %$current == 0;
    }
    remove_rec($root, $word, sort split //, $word);
}

say "Creating Trie";
{
    open(my $words, '<', $ARGV[0]);
    while(my $word = <$words>) {
        chomp($word);
        insert($trie, $word);
    }
    close $words;
}
say "Done creating Trie";

sub search
{
    my @result;
    my $current = shift;
    return unless @_;
    my $letter = shift;
    if (exists $current->{$letter}) {
        my $next = $current->{$letter};
        push(@result, @{$next->{words}}) if exists $next->{words};
        push(@result, search($next, @_));
    }
    # Skip duplicated letters when we are skipping this occurance of the letter to remove duplicated words.
    shift while @_ && $_[0] eq $letter;
    push(@result, search($current, @_));
    return @result;
}

sub all
{
    my ($current, $action) = @_;
    my @words = @{$current->{words} // []};
    $action->($_) foreach @words;
    while(my ($letter, $next) = each %$current) {
        all($next, $action) if $letter ne 'words';
    }
}

my $modified;

while (1) {
    print "Letters: ";
    my $letters = <STDIN> // '';
    chomp($letters);
    last unless $letters;
    my @letters = split //, $letters;
    next unless @letters == 6;
    my @result = search($trie, sort @letters);
    @result = sort { length $b <=> length $a or $a cmp $b } @result;
    say foreach (@result);
    foreach (@result) {
        next if $seen{$_};
        print "$_: ";
        chomp(my $resp = <STDIN> // '');
        if ($resp && 'remove' =~ /$resp/) {
            $modified = 1;
            remove($trie, $_);
        } else {
            $seen{$_} = 1;
        }
    }
    while (1) {
        print "Add: ";
        chomp(my $resp = <STDIN> // '');
        last unless $resp;
        insert($trie, $resp);
        $modified = 1;
    }
}

{
    open(my $words, '>', $ARGV[0]);
    all($trie, sub { print $words "$_[0]\n" });
}
