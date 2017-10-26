use v6;

role Extensible {
    has %.extensions;
}

my Str @banana does Extensible;

@banana.push(|<a b c d>);
@banana.extensions{5} = 6;

say @banana;
say @banana.extensions;

subset StrBool of Any where Str|Bool;

my StrBool $thing = "cat";
say $thing.WHAT;

$thing = True;

say $thing.WHAT;

role Simian { ... };

subset SimianBool of Any where Simian|Bool;

role Simian {
    has Str $.name is required;
    method eat($fruit = 'banana') {
        say "$.name eats a $fruit";
    }
    method siblings() returns Array[SimianBool] { ... } ;
};

class Monkey does Simian {
    has Str $.owner is required;
    has SimianBool @.siblings;
}

my SimianBool $monkey = Monkey.new(name => 'Curious George', owner => 'Yellow Hat');

$monkey.eat();
$monkey.eat('apple');

<pear apricot grape>.map: { $monkey.eat($_) };

my @monkeys;
for 'A'..'Z' {
    @monkeys.push(Monkey.new(name => "Monkey $_", owner => "Owner $_", siblings => @monkeys));
}

.siblings.elems.say for @monkeys;

role Attribute-Instantiator {
    proto instantiate-attribute($type, $item, Str $key) { ... }
}

role AttributeInstantiator-Default does Attribute-Instantiator {
    multi method instantiate-attribute(Array[Str] $, Str $item, Str $key) returns Array[Str] {
        return Array[Str].new([$item]);
    }
    multi method instantiate-attribute(Array[Str] $, @item, Str $key) returns Array[Str] {
        return Array[Str].new(|@item);
    }
    multi method instantiate-attribute(Int $, Int $item, Str $key) returns Int {
        return $item;
    }
}

class Schema-Instantiator {
    has $.attribute-instantiator = AttributeInstantiator-Default.new;
    method build(%schema, $name) {
        my @attr;
        my @errors;
        my %attributes = {
            foo => Array[Str],
            bar => Array[Str],
            baz => Int
        }
        for %attributes.kv -> $attr, $type {
            if (%schema{$attr}:exists) {
                @attr.push: Pair.new($attr, $.attribute-instantiator.instantiate-attribute($type, %schema{$attr}, "$name/$attr"));
                CATCH {
                    when X::Multi::NoMatch {
                        @errors.push("$name/$attr: Could not find a way to turn type {%schema{$attr}.WHAT.^name} into type {$type.^name}");
                    }
                    default {
                        die $_;
                    }
                }
            }
        }
        if (@errors) {
            die @errors;
        }
        return @attr;
    }
}

dd Schema-Instantiator.new.build({ foo => 'cat', bar => <dog horse>, baz => 123 }, '#');
