3 constant width
21 constant goal
width 1- constant wm
width 1+ constant wp
width width * constant size

create choices  3 ,  4 ,  5 ,
                6 ,  7 ,  8 ,
                9 , 10 , 11 ,

variable picked size allot
variable additional-constraints size allot

: clear-picked
    size
    begin
        1- dup 0>= while
            dup picked + 0 swap C!
    repeat
    drop ;

: print-picked
    9 0 do
        i picked + C@ .
    loop ;


variable a size cells allot

: choice-search
    0
    begin
        dup size < while
            choices over cells + @
            rot tuck = if
                drop -1 exit
            endif
            swap 1+
    repeat
    2drop 0 ;

: reverse-cross-valid
    0
    wm wp * wm do
        a i cells + @ +
    wm +loop
    goal = ;

: cross-valid
    0
    size 0 do
        a i cells + @ +
    wp +loop
    goal = ;
        
: bottom-row-valid
    0
    size size width - do
        a i cells + @ +
    loop
    goal = ;

: bottom-right-valid
    bottom-row-valid if
        -1
    else
        cross-valid
    endif ;

0 VALUE pick-next-ref

: execute-predicate?
    dup 0<> if
        execute
    else
        drop -1
    endif ;

: pick-internal
    size 0 do
        dup cells a + choices i cells + @ swap !
        i picked + C@ 0= if
            dup cells additional-constraints + @ execute-predicate? if
                1 i picked + C!
                dup pick-next-ref execute
                0 i picked + C!
            endif
        endif
    loop
    drop ;

: pick-right
    0 over dup dup width mod - do
        a i cells + @ +
    loop
    goal swap - 
    over over swap cells a + !
    choice-search if
        dup picked + C@ 0= if
            over cells additional-constraints + @ execute-predicate? if
                1 over picked + C!
                over pick-next-ref execute
                0 swap picked + C!
            else
                drop
            endif
            1 over picked + C!
            over pick-next-ref execute
            0 swap picked + C!
        else
            drop
        endif
    endif
    drop ;

: pick-bottom
    0 over dup width mod do
        a i cells + @ +
    width +loop
    goal swap -
    over over swap cells a + !
    choice-search if
        dup picked + C@ 0= if
            over cells additional-constraints + @ execute-predicate? if
                1 over picked + C!
                over pick-next-ref execute
                0 swap picked + C!
            else
                drop
            endif
        else
            drop
        endif
    endif
    drop ;

create solution-count 0 ,

: solution
    solution-count @ 1+ dup solution-count !
    ." --- Solution " . ." ---" CR
    size 0 do
        a i cells + @ .
        i 3 mod 2 = if CR endif
    loop ;

: print-right-internal
    dup width mod wm = if
        ." pick-right " .
    else
        ." pick-internal " .
    endif ;

: print-next
    dup . ." -> "
    dup size 1- = if
        drop ." solution"
    else
        dup width wm * >= if
            wm -
            print-right-internal
        else
            dup width dup 2 - * >= if
                width +
                ." pick-bottom " .
            else
                1+
                print-right-internal
            endif
        endif
    endif ;

: pick-right-internal
    dup width mod wm = if
        pick-right
    else
        pick-internal
    endif ;

: pick-next
    dup size 1- = if
        drop solution
    else
        dup width wm * >= if
            wm -
            pick-right-internal
        else
            dup width dup 2 - * >= if
                width +
                pick-bottom
            else
                1+
                pick-right-internal
            endif
        endif
    endif ;

' pick-next TO pick-next-ref

: init-board
    size 0 do
        i 1+ a i cells + !
    loop ;

: clear-constraints
    size 0 do
        0 additional-constraints i cells + !
    loop ;


clear-constraints
' reverse-cross-valid additional-constraints size width - width - 1+ cells + !
' bottom-right-valid additional-constraints size 1- cells + !

: solve-puzzle
    clear-picked init-board 0 solution-count !
    0 pick-internal ;
