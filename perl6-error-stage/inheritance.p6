use v6;

use Test;

role A {
    has %.hash;
}

role B {
    method hash() returns Associative { ... }
}

class C does A { }

class D does B {
    has %.hash;
}

class E does A does B {
    
}
