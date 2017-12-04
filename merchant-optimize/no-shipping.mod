set merchantIds := 0 .. 3;
set itemIds := 0 .. 4;

param merchants{merchantIds} symbolic;
param items{itemIds} symbolic;
param stocks{merchantIds, itemIds} integer;
param prices{merchantIds, itemIds};
param order{itemIds};

var fulfillment{merchantIds, itemIds} integer, >= 0;

s.t. canBeFulfilled{m in merchantIds, i in itemIds}: fulfillment[m,i] <= stocks[m,i];
s.t. orderFulfilled{i in itemIds}: sum{m in merchantIds} fulfillment[m,i] = order[i];

minimize cost:
        sum{m in merchantIds, i in itemIds} prices[m,i]*fulfillment[m,i];

solve;

printf "%10s", "merchant";

for {i in itemIds} {
    printf " %10s", items[i];
}
printf "\n";

for {m in merchantIds} {
    printf "%10s", merchants[m];
    for {i in itemIds} {
        printf " %10d", fulfillment[m, i];
    }
    printf "\n";
}

printf "Total Cost: $%0.2f\n", sum{m in merchantIds, i in itemIds} prices[m,i]*fulfillment[m,i];

data;

param merchants :=
        0        Bob
	1        Sam
        2        Fred
        3        George
        ;

param items :=
        0    Widget
        1    Sprocket
        2    Thing
        3    Whoozit
        4    Whatzit
        ;

param stocks :=
        :    0    1    2    3   4 :=
        0    3    2    5    6   0
        1    3    9    0    3   2
        2    3    3    5    6   3
        3   20   20   20   20  20
        ;


param prices :=
        :    0    1    2    3    4 :=
        0 3.21 9.15 4.65 8.15 8.50
        1 3.72 1.34 3.11 8.33 5.84
        2 8.75 2.26 0.73 4.43 7.84
        3 9.99 9.99 9.99 9.99 9.99
        ;

param order :=
        0    8
        1    6
        2    4
        3    2
        4    3
        ;
