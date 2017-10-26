use v6;

use def;

class B {
    multi method saySounds(Array[sounds] $sounds) {
        say sounds;
    }
    multi method saySounds($other) {
        say "The other one!";
    }
}

B.new.saySounds(A.^methods[0].returns);
