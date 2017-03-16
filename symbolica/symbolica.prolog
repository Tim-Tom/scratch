use_module(library(clpfd)).

idx(Matrix, R, C, V) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, V).    

adjacent(0, 1).
adjacent(0, 2).
adjacent(1, 0).
adjacent(1, 3).
adjacent(2, 0).
adjacent(2, 3).
adjacent(3, 1).
adjacent(3, 2).


validRow([]).
validRow([_]).
validRow([A|T]) :-
    [B|_] = T,
    adjacent(A, B),
    validRow(T).

validRows([],[]).
validRows([A|AT], [B|BT]) :-
    adjacent(A, B),
    validRows(AT, BT).

validBoard([]).
validBoard([X]) :- validRow(X).
validBoard([A|T]) :-
    [B|_] = T,
    validRow(A),
    validRows(A,B),
    validBoard(T).

list_count_([], _, [], N, N).
list_count_([E|As], E, Bs, N0, N) :-
    N1 is N0 + 1,
    list_count_(As, E, Bs, N1, N),
    !.
list_count_([A|As], E, [A|Bs], N0, N) :-
    list_count_(As, E, Bs, N0, N).

list_counts([], []).
list_counts([X|Xs], [X-N|Ys]) :-
   list_count_(Xs, X, Xs0, 1, N),
   list_counts(Xs0, Ys).

flatten([], [], 0, _) :- !.
flatten([X|T], Flat, H, W) :-
    length(X, W),
    append(X, Rest, Flat),
    flatten(T, Rest, H0, W),
    H is H0 + 1.

flatten(X, Y) :- flatten(X, Y, _, _).

rowDiff([], [], 0).
rowDiff([E|Xt], [E|Yt], D) :-
    !,
    rowDiff(Xt, Yt, D).
rowDiff([_|Xt], [_|Yt], D) :-
    rowDiff(Xt, Yt, D0),
    D is D0 + 1.

boardDiff([], [], 0).
boardDiff([X|Xt], [Y|Yt], D) :-
    rowDiff(X, Y, Dr),
    boardDiff(Xt, Yt, Dt),
    D is Dr + Dt.

symbolica(OriginalBoard, Board_Flat, D) :-
    flatten(OriginalBoard, OriginalBoard_Flat, L, W),
    S is L*W,
    length(Board_Flat, S),
    list_counts(OriginalBoard_Flat, Count),
    global_cardinality(Board_Flat, Count),
    flatten(Board, Board_Flat, L, W),
    boardDiff(OriginalBoard, Board, D),
    validBoard(Board),
    labeling([], Board_Flat).
