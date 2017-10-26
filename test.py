# Generate  the primes from 2 .. maximum (inclusive)
def gen_primes(maximum):
    # Handle degenerate cases
    if maximum >= 2:
        yield 2
    if maximum >= 3:
        yield 3
    # We generate our prime numbers by generating a Sieve of Eratosthenes on demand and
    # automatically skipping all multiples of two and three. We can do this because the
    # pattern in the multiples will repeat every 2*3=6 numbers and has the following
    # pattern: [0: f, 1: t, 2: f, 3: f, 4: f, 5: t] Those t value have distance 4 and 2
    # from each other so we can skip over all the values by alternating between jumping
    # that many numbers.
    primes = []
    n = 5
    incr = 2
    while n < maximum:
        composite = False
        for i in xrange(len(primes)):
            (increment, val) = primes[i]
            if n == val:
                composite = True
            if n >= val:
                primes[i] = (increment, val + increment)
        if not composite:
            yield n
            primes.append((n, n*n))
        n += incr
        # Since 2 and 4 are powers of 2, you can xor them with 2|4=6 to get the other.
        incr ^= 6 

class gen_primes_manual:
    def __init__(self, maximum):
        self.maximum = maximum
        self.primes = None
        self.n = None
        self.incr = None
        self.fun = self.first
    def first(self):
        if self.maximum > 2:
            self.fun = self.second
            return 2
        return self.second()
    def second(self):
        if self.maximum > 3:
            self.fun = self.third
            return 3
        return self.third()
    def third(self):
        self.primes = []
        self.n = 5
        self.incr = 2
        return self.fourth()
    def fourth(self):
        if self.n < self.maximum:
            composite = False
            for i in xrange(len(self.primes)):
                (increment, val) = self.primes[i]
                if self.n == val:
                    composite = True
                if self.n >= val:
                    self.primes[i] = (increment, val + increment)
            if not composite:
                self.fun = self.fifth
                return self.n
            return self.sixth()
        raise StopIteration()
    def fifth(self):
        self.primes.append((self.n, self.n*self.n))
        return self.sixth()
    def sixth(self):
        self.n += self.incr
        self.incr ^= 6
        return self.fourth()
    def __iter__(self):
        return self
    def __next__(self):
        return self.next()
    def next(self):
        return self.fun()

for p in gen_primes(1024):
    print p

for p in gen_primes_manual(1024):
    print p


mult3 = 2
mult5 = 4
for n in range(1, 30 + 1):
    if mult3 == 0 and mult5 == 0:
        print "FizzBuzz"
        mult3 = 3
        mult5 = 5
    elif mult3 == 0:
        print "Fizz"
        mult3 = 3
    elif mult5 == 0:
        print "Buzz"
        mult5 = 5
    else:
        print n
    mult3 -= 1
    mult5 -= 1
