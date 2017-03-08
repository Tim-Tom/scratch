use strict;
use warnings;

use v5.24;

our ($ps, $pa, $ph) = (1, 0, 0);

package TestScalar {
  sub TIESCALAR {
    my ($class, $val) = @_;
    say "SCALAR: TIESCALAR Called" if $ps;
    return bless \$val, $class;
  }
  sub FETCH {
    my ($self, ) = @_;
    say "SCALAR: FETCH called" if $ps;
    return $$self;
  }
  sub STORE {
    my ($self, $val) = @_;
    say "SCALAR: STORE ($val) called" if $ps;
    $$self = $val;
  }
  sub UNTIE {
    say "SCALAR: UNTIE called" if $ps;
  }
  sub DESTROY {
    say "SCALAR: DESTROY called" if $ps;
  }
};

package TestArray {
  sub TIEARRAY {
    my $class = shift;
    say "ARRAY: TIEARRAY Called" if $pa;
    return bless [@_], $class;
  }

  sub FETCH {
    my ($self, $index) = @_;
    say "ARRAY: FETCH called" if $pa;
    return $self->[$index]
  }

  sub FETCHSIZE {
    say "ARRAY: FETCHSIZE called" if $pa;
    my ($self, ) = @_;
    return scalar @$self;
  }

  sub STORE {
    my ($self, $index, $val) = @_;
    say "ARRAY: STORE called" if $pa;
    $self->[$index] = $val;
  }

  sub EXTEND {
    say "ARRAY: EXTEND called?" if $pa;
  }

  sub STORESIZE {
    say "ARRAY: STORESIZE called" if $pa;
    my ($self, $length) = @_;
    $#$self = $length - 1;
  }

  sub CLEAR {
    say "ARRAY: CLEAR called" if $pa;
    my $self = shift;
    @$self = ();
  }

  sub PUSH {
    say "ARRAY: PUSH called" if $pa;
    my $self = shift;
    push(@$self, @_);
  }

  sub POP {
    say "ARRAY: POP called" if $pa;
    my $self = shift;
    pop(@$self);
  }

  sub UNSHIFT {
    say "ARRAY: UNSHIFT called" if $pa;
    my $self = shift;
    unshift(@$self, @_);
  }

  sub SHIFT {
    say "ARRAY: SHIFT called" if $pa;
    my $self = shift;
    shift(@$self);
  }

  sub SPLICE {
    say "ARRAY: SPLICE called" if $pa;
    my $self = shift;
    splice(@$self, @_);
  }

  sub DELETE {
    say "ARRAY: DELETE called" if $pa;
    die;
  }

  sub EXISTS {
    say "ARRAY: EXISTS called" if $pa;
    die;
  }

  sub UNTIE {
    say "ARRAY: UNTIE called" if $pa;
  }

  sub DESTROY {
    say "ARRAY: DESTROY called" if $pa;
  }
};

package TestHash {
  sub TIEHASH {
    my $class = shift;
    say "HASH: TIEHASH called" if $ph;
    return bless {@_}, $class;
  }

  sub FETCH {
    say "HASH: FETCH called" if $ph;
    my ($self, $key) = @_;
    return $self->{$key};
  }

  sub STORE {
    say "HASH: STORE called" if $ph;
    my ($self, $key, $value) = @_;
    $self->{$key} = $value;
  }

  sub EXISTS {
    say "HASH: EXISTS called" if $ph;
    my ($self, $key) = @_;
    exists $self->{$key}
  }


  sub DELETE {
    say "HASH: DELETE called" if $ph;
    my ($self, $key) = @_;
    delete $self->{$key};
  }


  sub CLEAR {
    say "HASH: CLEAR called" if $ph;
    my $self = shift;
    %$self = ();

  }

  sub FIRSTKEY {
    say "HASH: FIRSTKEY called" if $ph;
    my $self = shift;
    keys %$self;
    return each %$self;
  }


  sub NEXTKEY {
    say "HASH: NEXTKEY called" if $ph;
    my $self = shift;
    return each %$self;
  }


  sub SCALAR {
    say "HASH: SCALAR called" if $ph;
    my $self = shift;
    return scalar %$self;
  }


  sub UNTIE {
    say "HASH: UNTIE called" if $ph;
  }

  sub DESTROY {
    say "HASH: DESTROY called" if $ph;
  }

};

tie our $foo, 'TestScalar', qr/ap/;
tie our @foo, 'TestArray', qw(apple banana apricot);
tie our %foo, 'TestHash', qw(apple 5 banana 7);

sub test_variables {

  say 'STATUS: Testing @foo';
  for my $f (@foo) {
    say "OUTPUT: $f" if $f =~ $foo;
  }
  say 'STATUS: Done testing @foo';

  say 'STATUS: Testing %foo';
  while(my ($key, $val) = each %foo) {
    say "OUTPUT: There are $val ${key}s" unless $key =~ $foo;
  }
  say 'STATUS: Done testing %foo';

}

test_variables;
{
  say "STATUS: Localizing variable";
  local $foo = qr/ana/;
  say "STATUS: Done localizing variable";
  test_variables;
  say "STATUS: Terminating with local scope";
}
say "STATUS: Done with local scope";
test_variables;
