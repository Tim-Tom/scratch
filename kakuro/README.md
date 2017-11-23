Kakuro Solver
=============

Kakuro is a crossword-like number game. You are given a board that looks like a crossword
puzzle, but instead of word based clues, you are given a target sum. Additionally, each
sum must use unique numbers from 1 to 9. So you cannot get a 4 by using {4, 0} or {2, 2}.

Solution Heuristics
-------------------

This is not a complete list of possible heuristics, just those that I thought of. A lot of
more complex heuristics are possible, but since heuristic 5 catches any heuristic that
doesn't span multiple constraints it's hard to justify thinking about them. In general
anything that works for sudoku (and doesn't care about the board layout) would work for
this as well.

1. The first and most basic solution heuristic is that the possibilities of any tile are
   the intersection of the sum possibilities of its row constraint and column
   constraint. So if a clue has 3 and 4 as its row and column constraint (respectively),
   it must be a 1 because the possibilities of 3 are {1,2} and the possibilities of 4 are
   {1,3}, so if it used the 2 from the 3 clue, it wouldn't be able to solve the 4 clue.
2. The second heuristic is that if a number is required and only used in one place, it
   must be used in that place. So if a sum is 23 (requiring {6, 8, 9}) and two cells are
   {8,9} and the third is {6,8,9}, the third cell must be the 6, because otherwise the 6
   would not be available. Requires time proportional to the sum of the number of cell
   possibilities.
3. The third heuristic is the reverse of the above. Using the previous example, we could
   have instead stated that since the superposition of the first two cells used up the 8
   and 9, the third cell could not be an 8 or a 9. Requires time proportional to the sum
   of the number of cell possibilities.
4. The fourth heuristic (which I don't plan to implement) is the generalization of the
   second heuristic. It would state that if there are n cells with n possibilities in
   common (and they are the only cells with them), they must be those numbers and all
   other possibilities could be removed. So if two cells are {5,8,9} and {6,8,9}, the 8
   and 9 don't appear anywhere else, and are required, then the 5 and 6 can be dropped
   from their possibilities. Tracking all that information seems onerous, so unless I find
   a specific puzzle that is really slow to use the fifth heruristic and can be solved
   with this one I won't implement it.
5. The fifth heuristic is a general catch-all. It can find the same results as all the
   previous heuristics, but requires a lot more computational resources. This heuristic is
   to actually simulate all possible sums and constrain letters by what numbers were
   actually possible for a given cell. So in addition to finding the above conclusion, it
   will also determine things like a two-sum to 11 with possibilities {3,6}, {2,3,8,9} is
   actually constrained to be {3}, {8} because that is the only combination of numbers
   that actually sum to 11. Requires time proportional to the product of the number of
   cell possibilities.
