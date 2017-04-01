Solutions for a random puzzle mentioned in a [blog post][1]. Solutions made after reading
the problem description and before reading the rest of the writeup. Initial thoughts are
that it looks like a basic dynamic programming puzzle.

1. Store minimal distance to completion using player betting amounts.
2. Sort inputs to make key (which player is which doesn't matter since there is no turn order).

Simple dynamic programming didn't work because it's really easy to get into a loop and the
loops don't always resolve. So I would have to intelligently pick out soluitions to start
from and work from there.  I will instead try out a simple breadth first search with memory.

Breadth first search worked great. The only tweak I really had to do that I didn't think
of at first (it doesn't affect the solution as it turns out), was to allow the final state
to range over all possible states for `{x+x+x, x+x+y, x+y+y} < 255*3; x < y`. Because we
can't start at 1,1,256 but we could reach it in some other way.

[1]: http://www.boyter.org/2017/03/golang-solution-faster-equivalent-java-solution/ "Why is this GoLang solution faster than the equivalent Java Solution?"
