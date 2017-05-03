:- use_module(library(clpfd)).

cnt([], _, 0).
cnt([N|T], N, C) :-
    C #= C1 + 1,
    cnt(T, N, C1).
cnt([_|T], N, C) :-
    cnt(T, N, C).

cnt_descriptive(N, 10, N).
cnt_descriptive(A, X, B) :-
    

self_descriptive(N) :-
    [C0, C1, C2, C3, C4, C5, C6, C7, C8, C9] = N,
    N ins 0 .. 9,
    C0 #\= 0,
    cnt(N, 0, C0),
    cnt(N, 1, C1),
    cnt(N, 2, C2),
    cnt(N, 3, C3),
    cnt(N, 4, C4),
    cnt(N, 5, C5),
    cnt(N, 6, C6),
    cnt(N, 7, C7),
    cnt(N, 8, C8),
    cnt(N, 9, C9).
