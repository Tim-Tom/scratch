use_module(library(clpfd)).

flatten([], [], 0, _) :- !.
flatten([X|T], Flat, H, W) :-
    length(X, W),
    append(X, Rest, Flat),
    flatten(T, Rest, H0, W),
    H is H0 + 1.

flatten(X, Y) :- flatten(X, Y, _, _).

diag([], [], _).
diag([Row|Rows], [V|T], D) :-
    D1 is D + 1,
    nth1(D, Row, V),
    diag(Rows, T, D1).

diag(Rows, Diag) :-
    diag(Rows, Diag, 1).

rdiag([], [], _).
rdiag([Row|Rows], [V|T], D) :-
    D1 is D - 1,
    nth1(D, Row, V),
    rdiag(Rows, T, D1).

rdiag(Rows, Diag) :-
    length(Rows, L),
    rdiag(Rows, Diag, L).

mysum(Value, Operator, List) :-
    sum(List, Operator, Value).

magic_box(Sum, Size, Domain, Box) :-
    length(Box, Size),
    maplist(same_length(Box), Box),
    append(Box, Vars),
    Vars ins Domain,
    all_distinct(Vars),
    transpose(Box, Cols),
    diag(Box, Diag),
    rdiag(Box, RDiag),
    maplist(mysum(Sum, #=), Box),
    maplist(mysum(Sum, #=), Cols),
    sum(Diag, #=, Sum),
    sum(RDiag, #=, Sum).

% time(findall(Box, (magic_box(21, 3, 3..11, Box), append(Box, Vars), labeling([ff], Vars)), Bag)).
