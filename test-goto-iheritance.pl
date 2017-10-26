use strict;
use warnings;

use v5.24;

package Foo {
  use Carp qw(cluck);
  sub new {
    my ($class, ) = @_;
    return bless {}, $class;
  }
  sub apple {
    my $self = shift;
    cluck "foo::apple called";
    return;
  }
  sub banana {
    my $self = shift;
    $self->apple;
  }
  sub carrot {
    my $self = shift;
    goto &{$self->can('apple')};
  }
};

package Bar {
  use base qw(Foo);
  use Carp qw(cluck);
  sub apple {
    my $self = shift;
    cluck 'bar::apple called';
    return;
  }
};

my $b = Bar->new;

$b->banana;
$b->carrot;
