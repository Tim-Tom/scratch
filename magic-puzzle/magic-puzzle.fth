3 constant width
21 constant goal
width 1- constant wm
width 1+ constant wp
width width * constant size

create choices  3 ,  4 ,  5 ,
                6 ,  7 ,  8 ,
                9 , 10 , 11 ,

variable picked size allot

: clearPicked
    size
    begin
        1- dup 0>= while
            dup picked + 0 swap C!
    repeat
    drop ;

variable a size cells allot

: choiceSearch
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

: reverseCrossInvalid
    0
    wm wp * wm do
        a i cells + @ +
    wm +loop
    goal = ;

: crossInvalid
    0
    size 0 do
        a i cells + @ +
    wp +loop
    goal = ;
        
: bottomRowInvalid
    0
    size size width - do
        a i cells + @ +
    loop
    goal = ;

: bottomRightInvalid
    bottomRowInvalid if
        -1
    else
        crossInvalid
    endif ;

0 VALUE pickNextRef

: pickInternal
    size 0 do
        dup cells a + choices i cells + @ swap !
        i picked + C@ 0= if
            1 i picked + C!
            dup pickNextRef execute
            0 i picked + C!
        endif
    loop
    drop ;

: pickRight
    0 over dup dup width mod - do
        a i cells + @ +
    loop
    goal swap - 
    over over swap cells a + !
    choiceSearch if
        dup picked + C@ 0= if
            1 over picked + C!
            over pickNextRef execute
            0 swap picked + C!
        endif
    endif
    drop ;

: pickBottom
    0 over dup width mod do
        a i cells + @ +
    width +loop
    goal swap -
    over over swap cells a + !
    choiceSearch if
        dup picked + C@ 0= if
            1 over picked + C!
            over pickNextRef execute
            0 swap picked + C!
        endif
    endif
    drop ;

create solution_count 0 ,

: solution
    solution_count @ 1+ dup solution_count !
    ." --- Solution " . ." ---" CR
    size 0 do
        a i cells + @ .
        i 3 mod 2 = if CR endif
    loop ;

: pickNext
    dup ." pickNext " .
    dup size 1- = if
        drop ." -> solution" CR solution exit
    endif
    dup width wm * >= if
        wm -
    else
        dup width dup 2 - * >= if
            width + ." -> pickBottom " dup . CR pickBottom exit
        else
            1+
        endif
    endif
    dup width mod wm = if
        ." -> pickRight " dup . CR pickRight
    else
        ." -> pickInternal " dup . CR pickInternal
    endif ;

' pickNext TO pickNextRef

: initBoard
    9 0 do
        i 1+ a i cells + !
    loop ;

: solve-puzzle
    clearPicked initBoard 0 solution_count !
    0 pickInternal ;
