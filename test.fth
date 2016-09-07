( Nip: drop the item one back on the stack: swap drop )
( tuck: put a copy of the current item behind the last item dup rot rot)
( [a-b]*[a - 1] 11 6 swap dup rot - swap 1 - * )

: myNip ( n1 n2 -- n2 n1 )
    swap drop ;

: myTuck ( n1 n2 -- n2 n1 n2 )
    dup rot rot ;

: thirdThing ( n1 n2 -- [n1-n2]*[n1-1] )
    swap dup rot - swap 1 - * ;

: myNegate ( n1 -- -n1 )
    0 swap - ;

: myDivMod ( n1 n2 -- [n1/n2] [n1 mod n2] )
    2dup mod rot rot / ;

: myDivModLocal { n1 n2 -- [n1/n2] [n1 mod n2] }
    n1 n2 mod n1 n2 / ;

: myAbs ( n1 -- +n1 )
    dup 0 < if
        negate
    endif ;


: myMin ( n1 n2 -- n )
    2dup < if
        drop
    else
        nip
    endif ;

: myMin2 ( n1 n2 -- n )
    2dup > if
        swap
    endif
    drop ;

: theirMin ( n1 n2 -- n )
    2dup 2dup < rot rot >= rot and rot rot and + ;

: theirMin2 ( n1 n2 -- n )
    2dup 2dup > and rot  rot >= rot and + ;

: theirMin3 ( n1 n2 -- n )
    2dup tuck > and swap rot tuck >= and + ;

: theirMinLocal { n1 n2 -- n }
    n1 n1 n2 < and n2 n1 n2 > and + ;

: myMax ( n1 n2 -- n )
    2dup > if
        drop
    else
        nip
    endif ;


: 2mod/
    dup 2/ swap 1 and ;

: log2 ( +n1 -- n2 )
    \ logarithmus dualis of n1>0, rounded down to the next integer
    assert( dup 0> )
    2/ 0 begin
        over 0> while
            1+ swap 2/ swap
    repeat
    nip ;

: pow ( n1 n2 -- n1^n2 )
    1 swap
    begin
        dup 0> while
            2mod/ 0= if
                rot dup * -rot
            else
                -rot over * swap dup * swap rot
            endif
    repeat
    drop nip ;
            
: theirPow
    \ n = the uth power of n1
    1 swap 0 u+do
        over *
    loop
    nip ;

\ 1 2 3 4 5 6
\ 1 1 3 5 8 13 
: fib ( n -- n! )
    assert( dup 0> )
    1 1 rot 2 u+do
        tuck +
    loop
    nip ;

: fib-rec-int
    CR .s
    dup 2 > if
        1- -rot tuck + rot recurse
    else
        drop nip
    endif ;

: fib-rec ( n -- n! )
    assert( dup 0> )
    1 1 rot fib-rec-int ;

: gcd ( n1 n2 -- n )
    0 rot rot
    begin
        dup 1 and rot dup 1 and rot or 0= while
            2/ rot 1+ rot 2/ rot
    repeat
    begin
        2dup <> while
            dup 1 and 0= if
                2/
            else
                swap dup 1 and 0= if
                    2/
                else
                    2dup < if
                        swap
                    endif
                    tuck - 2/
                endif
            endif
    repeat
    drop 2 rot pow * ;

: @R I ;

: quadratic ( a b c x -- [ax + b]x + c )
    >R swap rot R@ * +  R> * + ;
