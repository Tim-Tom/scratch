# Sets
set boardSize := 0 .. 2;
set boundedBoardSize := 0 .. 1;
set abs := {"pos", "neg"};
set types  := {"color", "symbol"};
set colors  := 0 .. 3;
set symbols := 0 .. 2;

# Parameters
param symbolNames{colors, symbols} symbolic;
param originalBoard{boardSize, boardSize, types} integer;

# Variables
var board{boardSize, boardSize, types} integer, >= 0;
var _boardDiff{boardSize, boardSize, types, abs} integer, >= 0;
var _boardDiffSelector{boardSize, boardSize, types} binary;
var _boardDiffCap{boardSize, boardSize} binary;
var _horzAdj{boardSize, boundedBoardSize, types, abs} integer, >= 0;
var _vertAdj{boundedBoardSize, boardSize, types, abs} integer, >= 0;
var _horzAdjSelector{boardSize, boundedBoardSize, types} binary;
var _vertAdjSelector{boundedBoardSize, boardSize, types} binary;
var _horzAdjCap{boardSize, boundedBoardSize, types} binary;
var _vertAdjCap{boundedBoardSize, boardSize, types} binary;

# Calculate the absolute value of the difference between a cell and the cell to the right
# of it.  This works by saying that X = X_1 - X_2; 0 <= X_1 <= 6*X_s; 0 <= X_2 <=
# 6*(1-X_s); where X_s is a binary variable. So either X_s is 2 and X_1 = X; or X_s = 2
# and X_2 = -X. 6 is an arbitrary upper bound on the value of X. (Or X=0, in which case
# both will be 0)
s.t. _horzAdj1{r in boardSize, c in boundedBoardSize, t in types}: board[r,c+0,t] - board[r,c+1,t] = _horzAdj[r,c,t,"pos"] - _horzAdj[r,c,t,"neg"];
s.t. _horzAdj2{r in boardSize, c in boundedBoardSize, t in types}: _horzAdj[r,c,t,"pos"] <= 6*_horzAdjSelector[r,c,t];
s.t. _horzAdj3{r in boardSize, c in boundedBoardSize, t in types}: _horzAdj[r,c,t,"neg"] <= 6*(1-_horzAdjSelector[r,c,t]);

# Calculate the absolute value of the difference between a cell and the cell below
# it. Works the same as above.
s.t. _vertAdj1{r in boundedBoardSize, c in boardSize, t in types}: board[r+0,c,t] - board[r+1,c,t] = _vertAdj[r,c,t,"pos"] - _vertAdj[r,c,t,"neg"];
s.t. _vertAdj2{r in boundedBoardSize, c in boardSize, t in types}: _vertAdj[r,c,t,"pos"] <= 6*_vertAdjSelector[r,c,t];
s.t. _vertAdj3{r in boundedBoardSize, c in boardSize, t in types}: _vertAdj[r,c,t,"neg"] <= 6*(1-_vertAdjSelector[r,c,t]);

# Now we use another binary value to turn our differences into either 0 or 1. If X_[12] =
# 0, then the first constraint will result in our capped value being constrained to zero. For
# anything else, the fact that their sum will be non-zero and the second constraint will
# result in our capped value being one.
s.t. _horzAdjCap1{r in boardSize, c in boundedBoardSize, t in types}: _horzAdjCap[r,c,t] <= sum{s in abs} _horzAdj[r,c,t,s];
s.t. _horzAdjCap2{r in boardSize, c in boundedBoardSize, t in types}: 6*_horzAdjCap[r,c,t] >= sum{s in abs} _horzAdj[r,c,t,s];
s.t. _vertAdjCap1{r in boundedBoardSize, c in boardSize, t in types}: _vertAdjCap[r,c,t] <= sum{s in abs} _vertAdj[r,c,t,s];
s.t. _vertAdjCap2{r in boundedBoardSize, c in boardSize, t in types}: 6*_vertAdjCap[r,c,t] >= sum{s in abs} _vertAdj[r,c,t,s];

# Now that we've ensured the adjacency values are exactly 0 or 1, we can ensure that the
# sum for each square in each direction is 1 and not 0 or 2.
s.t. horizontalAdjacency{r in boardSize, c in boundedBoardSize}: sum{t in types} _horzAdjCap[r,c,t] = 1;
s.t. verticalAdjacency{r in boundedBoardSize, c in boardSize}: sum{t in types} _vertAdjCap[r,c,t] = 1;

s.t. _boardDiff1{r in boardSize, c in boardSize, t in types}: originalBoard[r, c, t] - board[r, c, t] = _boardDiff[r,c,t,"pos"] - _boardDiff[r,c,t,"neg"];
s.t. _boardDiff2{r in boardSize, c in boardSize, t in types}: _boardDiff[r, c, t, "pos"] <= 6*_boardDiffSelector[r,c,t];
s.t. _boardDiff3{r in boardSize, c in boardSize, t in types}: _boardDiff[r, c, t, "neg"] <= 6*(1-_boardDiffSelector[r,c,t]);

s.t. _boardDiffCap1{r in boardSize, c in boardSize}: _boardDiffCap[r,c] <= sum{t in types, s in abs} _boardDiff[r,c,t,s];
s.t. _boardDiffCap2{r in boardSize, c in boardSize}: 1000*_boardDiffCap[r,c] >= sum{t in types, s in abs} _boardDiff[r,c,t,s];

# This is wrong on pretty much every level, but it somewhat gets close.
s.t. picked{t in types}: sum{r in boardSize, c in boundedBoardSize} (board[r,c,t] - originalBoard[r,c,t]) = 0;

# Goal
minimize boardDifference:
    sum{r in boardSize, c in boardSize} _boardDiffCap[r, c];

solve;

# Result output

printf "%d differences\n", sum{r in boardSize, c in boardSize} _boardDiffCap[r, c];

for {r in boardSize} {
    for {c in boardSize} {
        printf "%s ", symbolNames[board[r,c,"color"],board[r,c,"symbol"]];
    }
    printf "\n";
}

data;

param symbolNames :=
    :   0   1   2 :=
    0  Ra  Rb  Rc
    1  Ba  Bb  Bc
    2  Ga  Gb  Gc
    3  Ya  Yb  Yc
    ;

# Ra Bb Ba Bb Bb
# Ga Ba Yc Ba Gb
# Yb Yb Bb Bc Ya
# Ba Ra Rb Bb Bc
# Rb Ya Yb Ra Ba

# param originalBoard :=
# # Color
# [*,*,0] : 0 1 2 3 4 :=
#         0 0 1 1 1 1
#         1 2 1 3 1 2
#         2 3 3 1 1 3
#         3 1 0 0 1 1
#         4 0 3 3 0 1
# # Symbol
# [*,*,1] : 0 1 2 3 4 :=
#         0 0 1 0 1 1
#         1 0 0 2 0 1
#         2 1 1 1 2 0
#         3 0 0 1 1 2
#         4 1 0 1 0 0
#         ;

param originalBoard :=
# Color
[*,*,"color"] : 0 1 2 :=
        0 0 0 0
        1 0 0 0
        2 0 0 0

# Symbol
[*,*,"symbol"] : 0 1 2 :=
        0 0 1 0
        1 1 1 0
        2 0 1 0
        ;
