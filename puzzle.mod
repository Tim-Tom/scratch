# Sets
set horz := 0 .. 5;
set vert := 0 .. 5;


var a{-1 .. 6, -1 .. 6} binary;

s.t. c0{v in -1 .. 6}: a[-1, v] = 0;
s.t. c1{v in -1 .. 6}: a[5, v] = 0;
s.t. c2{h in -1 .. 5}: a[h, -1] = 0;
s.t. c3{h in -1 .. 5}: a[h, 6] = 0;

s.t. c{h in horz, v in vert}: a[h-1, v] + a[h, v] + a[h+1, v] + a[h, v-1] + a[h, v+1] = 3;

minimize chosen:
    sum{h in horz, v in vert} a[h, v];

solve;

for {h in horz, v in vert} {
    printf "[%d, %d]: %d\n", h, v, a[h, v];
}
