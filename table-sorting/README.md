Table Sorting
=============

This is a recreation of an old programming competition test exercise that I worked on long
ago and still remember fondly.

Problem
-------

1. You are the event planner for a programming competition and your aim is to minimize
   cheating.
2. You have w workstations filled by s schools. 2 <= w <= 14 ; 1 <= s < w
3. Each school can have at between 1 and 4 teams competing. At least one school will have
   two teams.
4. Each workstation has an x and y coordinate associated with it.
5. Cheating is proportional to the minimum euclidian distance between two teams from the
   same school.

Analysis
--------

So the first thing to notice from the problem statement is that all these numbers are
really tiny. So tiny that you can pretty much guaruntee you are supposed to brute force
the solution.  But there's a catch, 14! = 87,178,291,200, which is too much to truly brute
force. So you need to exploit at least one of the following redundancies (and possibly both):

1. Teams within a school are indistinguishable.
2. Schools are indistingushable from any other school with the same number of
   teams. (i.e. AABB and BBAA will just result in swapping the minimum distance between A
   and B which doesn't change our answer, but ABAB is different).


So for three schools with two teams each, these are the distinct permutations in lexicographic order:

1.  AABBCC
2.  AABCBC
3.  AABCCB
4.  ABABCC
5.  ABACBC
6.  ABACCB
7.  ABBACC
8.  ABBCAC
9.  ABBCCA
10. ABCABC
11. ABCACB
12. ABCBAC
13. ABCBCA
14. ABCCAB
15. ABCCBA

And we can use math to validate that there should be 15 solutions.  We have 6! total
permutations of 6 objects. Divided by 2!^3 for the two teams in each of our
schools. Divided by 3! for the fact we have 3 schools with 2 teams. = 720/(2*2*2*6) = 15

Given our problem space, here are the sizes for the various versions (worst case in bold):

| Team Sizes | Distinct Teams   | Distinct Schools   |  Distinct Teams & Schools  |
|------------|-----------------:|-------------------:|---------------------------:|
| (14 0 0 0) |  **87178291200** |                1   |                        1   |
| (12 1 0 0) |    43589145600   |              182   |                       91   |
| (10 2 0 0) |    21794572800   |            12012   |                     3003   |
| ( 8 3 0 0) |    10897286400   |           360360   |                    45045   |
| ( 6 4 0 0) |     5448643200   |          5045040   |                   315315   |
| ( 4 5 0 0) |     2724321600   |         30270240   |                   945945   |
| ( 2 6 0 0) |     1362160800   |         60540480   |                   945945   |
| ( 0 7 0 0) |      681080400   |         17297280   |                   135135   |
| (11 0 1 0) |    14529715200   |             2184   |                      364   |
| ( 9 1 1 0) |     7264857600   |           240240   |                    20020   |
| ( 7 2 1 0) |     3632428800   |          8648640   |                   360360   |
| ( 5 3 1 0) |     1816214400   |        121080960   |                  2522520   |
| ( 3 4 1 0) |      908107200   |        605404800   |                  6306300   |
| ( 1 5 1 0) |      454053600   |        726485760   |                  3783780   |
| ( 8 0 2 0) |     2421619200   |          1081080   |                    30030   |
| ( 6 1 2 0) |     1210809600   |         60540480   |                   840840   |
| ( 4 2 2 0) |      605404800   |        908107200   |                  6306300   |
| ( 2 3 2 0) |      302702400   |       3632428800   |               **12612600** |
| ( 0 4 2 0) |      151351200   |       1816214400   |                  3153150   |
| ( 5 0 3 0) |      403603200   |        121080960   |                   560560   |
| ( 3 1 3 0) |      201801600   |       2421619200   |                  5605600   |
| ( 1 2 3 0) |      100900800   |       7264857600   |                  8408400   |
| ( 2 0 4 0) |       67267200   |       1816214400   |                  1401400   |
| ( 0 1 4 0) |       33633600   |       3632428800   |                  1401400   |
| (10 0 0 1) |     3632428800   |            24024   |                     1001   |
| ( 8 1 0 1) |     1816214400   |          2162160   |                    45045   |
| ( 6 2 0 1) |      908107200   |         60540480   |                   630630   |
| ( 4 3 0 1) |      454053600   |        605404800   |                  3153150   |
| ( 2 4 0 1) |      227026800   |       1816214400   |                  4729725   |
| ( 0 5 0 1) |      113513400   |        726485760   |                   945945   |
| ( 7 0 1 1) |      605404800   |         17297280   |                   120120   |
| ( 5 1 1 1) |      302702400   |        726485760   |                  2522520   |
| ( 3 2 1 1) |      151351200   |       7264857600   |               **12612600** |
| ( 1 3 1 1) |       75675600   |      14529715200   |               **12612600** |
| ( 4 0 2 1) |      100900800   |       1816214400   |                  2102100   |
| ( 2 1 2 1) |       50450400   |      21794572800   |               **12612600** |
| ( 0 2 2 1) |       25225200   |      21794572800   |                  6306300   |
| ( 1 0 3 1) |       16816800   |      14529715200   |                  2802800   |
| ( 6 0 0 2) |      151351200   |         60540480   |                   105105   |
| ( 4 1 0 2) |       75675600   |       1816214400   |                  1576575   |
| ( 2 2 0 2) |       37837800   |      10897286400   |                  4729725   |
| ( 0 3 0 2) |       18918900   |       7264857600   |                  1576575   |
| ( 3 0 1 2) |       25225200   |       7264857600   |                  2102100   |
| ( 1 1 1 2) |       12612600   |    **43589145600** |                  6306300   |
| ( 0 0 2 2) |        4204200   |      21794572800   |                  1051050   |
| ( 2 0 0 3) |        6306300   |       7264857600   |                   525525   |
| ( 0 1 0 3) |        3153150   |      14529715200   |                   525525   |
| Worst Case |  **87178291200** |    **43589145600** |               **12612600** |

Implementation
--------------

So obviously our task is to generate the distinct permutations given our two threads of
redundancy. I choose to do this by generating the solutions in lexicographic order with
the additional constraint that the leftmost team for any given school can never cross the
leftmost team for any school with the same number of teams.
