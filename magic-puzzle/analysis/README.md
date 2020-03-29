= Description

The point of this section is to track my attempts to fix the symmetry. It's pretty easy to
tell that with the symmetry of these puzzles, we are over-producing. The goal of this is
to find a way to detect we are generating a duplicate and that we can just stop the iteration.

For Example, we can easily make the arbitrary choice that if the top left is greater than
the top right, we can abort. We know that whatever we're doing would have already been
generated with a horizontal flip. I can't particularly think geometrically so the way that
I know how to generate these is to think in terms of x and y. We have x, -x, y and -y and
we have two places to put those variables (real x and y). Thus giving us 8 combinations:

|-----------------+----+----|
| Name            | x  | y  |
|-----------------+----+----|
| Normal          | x  | y  |
| Vertical Flip   | x  | -y |
| Horizontal Flip | -x | y  |
| Cross           | -x | -y |
| ...             | y  | x  |
| ...             | y  | -x |
| ...             | -y | x  |
| ...             | -y | -x |
|-----------------+----+----|

My first instinct is that we should be able to look at the 4 corners and perform an
arbitrary cut like the top left and top right above. You can see the results of this in
generate-permutations.pl, which permutes all the possible value orderings of the corner
and then arbitrarily cuts out values to give me a unique set across all the permutations.
