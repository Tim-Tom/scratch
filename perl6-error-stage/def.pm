use v6;

enum sounds <meow arf bark>;

class A {
    method getAnimalSounds() returns Array[sounds] {
        return sounds.enums.values.map: { sounds($_) };
    }
};

