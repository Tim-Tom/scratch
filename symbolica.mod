# Sets
set boardSize := 0 .. 2;
set boundedBoardSize := 0 .. 1;
set types  := 0 .. 1;
set colors  := 0 .. 1;
set symbols := 0 .. 1;

# Parameters
param symbolNames{colors, symbols} symbolic;
param originalBoard{boardSize, boardSize, types} integer;

# Variables
var board{boardSize, boardSize, types} integer, >= 0, <= 3;
var _boardDiff{boardSize, boardSize, types} integer, >= 0, <= 1;
var _boardDiffCap{boardSize, boardSize} binary;
var _horzAdj{boardSize, boundedBoardSize, types} integer, >= 0;
var _vertAdj{boundedBoardSize, boardSize, types} integer, >= 0;
var _horzAdjCap{boardSize, boundedBoardSize, types} binary;
var _vertAdjCap{boundedBoardSize, boardSize, types} binary;

# Constraints
# TODO: Do this
# s.t. symbolsPicked: 
# TODO: Do this
# s.t. horizontalAdjacency{r in boardSize, c in boundedBoardSize}: adjacency[board[r, c], board[r, c + 1]] == 0;
# s.t. verticalAdjacency{r in boundedBoardSize, c in boardSize}:  adjacency[board[r, c], board[r + 1, c]] == 0;

s.t. _horzAdj1{r in boardSize, c in boundedBoardSize, t in types}: _horzAdj[r,c,t] >= board[r,c+0,t] - board[r,c+1,t];
s.t. _horzAdj2{r in boardSize, c in boundedBoardSize, t in types}: _horzAdj[r,c,t] >= board[r,c+1,t] - board[r,c+0,t];

s.t. _vertAdj1{r in boundedBoardSize, c in boardSize, t in types}: _vertAdj[r,c,t] >= board[r+0,c,t] - board[r+1,c,t];
s.t. _vertAdj2{r in boundedBoardSize, c in boardSize, t in types}: _vertAdj[r,c,t] >= board[r+1,c,t] - board[r+0,c,t];

s.t. _horzAdjCap1{r in boardSize, c in boundedBoardSize, t in types}: _horzAdjCap[r,c,t] <= _horzAdj[r,c,t];
s.t. _horzAdjCap2{r in boardSize, c in boundedBoardSize, t in types}: 1000*_horzAdjCap[r,c,t] >= _horzAdj[r,c,t];

s.t. _vertAdjCap1{r in boundedBoardSize, c in boardSize, t in types}: _vertAdjCap[r,c,t] <= _vertAdj[r,c,t];
s.t. _vertAdjCap2{r in boundedBoardSize, c in boardSize, t in types}: 1000*_vertAdjCap[r,c,t] >= _vertAdj[r,c,t];

s.t. _boardDiff1{r in boardSize, c in boardSize, t in types}: _boardDiff[r, c, t] >= originalBoard[r, c, t] - board[r, c, t];
s.t. _boardDiff2{r in boardSize, c in boardSize, t in types}: _boardDiff[r, c, t] >= board[r, c, t] - originalBoard[r, c, t];

s.t. _boardDiffCap1{r in boardSize, c in boardSize}: _boardDiffCap[r,c] <= _boardDiff[r,c,0] + _boardDiff[r,c,1];
s.t. _boardDiffCap2{r in boardSize, c in boardSize}: 1000*_boardDiffCap[r,c] >= _boardDiff[r,c,0] + _boardDiff[r,c,1];

s.t. horizontalAdjacency{r in boardSize, c in boundedBoardSize}: sum{t in types} _horzAdjCap[r,c,t] = 1;
s.t. verticalAdjacency{r in boundedBoardSize, c in boardSize}: sum{t in types} _vertAdjCap[r,c,t] = 1;

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
        printf "%s ", symbolNames[board[r,c,0],board[r,c,1]];
    }
    printf "\n";
}

data;

param symbolNames :=
    :   0   1 :=
    0  Rn  Ra
    1  Bn  Ba
    ;


# Rn Bn Rn
# Ba Ba Ra
# Ra Ra Rn

param originalBoard :=
# Color
[*,*,0] :  0  1  2 :=
        0  0  1  0
        1  1  1  0
        2  0  0  0
# Symbol
[*,*,1] :  0  1  2 :=
        0  0  0  0
        1  1  1  1
        2  1  1  0
        ;
