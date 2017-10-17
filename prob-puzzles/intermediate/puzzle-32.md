Imagine a country in which 10 people have voted in an election opposing a woman named C
and a demagogue named T. Suppose C received 7 votes, while 3 voted for candidate T -- but
the public doesn't know that yet because the votes have not been counted.

The voters' ballots are placed in a large urn. Before it is opened, the urn is shaken in a
way that randomly shuffles the ballots, with all orderings equally likely. The urn is then
opened and the ballots taken out one by one and tallied. Given that 7 people voted for C
(and 3 for T), we know she'll end up with a net advantage of 4 votes at the end of the
count. There's randomness in how we get there, however.

What's the probability that C is strictly ahead of T at every point in the counting process.

We know there are 10! total ways and 10!/(7!3!) distinct ways to tally the votes. For
those who don't know, the easy way to do any counting problem is to define the total space
and then divide out duplicate tokens. So we have 10 token of which 7 are C and 3 are
T, so 10! total positions, divided by 7! for the 7 C tokens and 3! for the 3 T tokens.

Failed paths all start T, CT, CCTT, CCCTTT, CCTCTT and add up to:

```
T - 9!/(7!2!)    = 36
CT - 8!/(6!2!)   = 28
CCTT - 6!/(5!1!) =  6
CCCTTT - 4!/4!   =  1
CCTCTT - 4!/4!   =  1
---------------------
                   72
```

So 72 out of 120 ways result in T equaling or beating C at some time-slice in the process and
so 48 that result in complete victories for C.
