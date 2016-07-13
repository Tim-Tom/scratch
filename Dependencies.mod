# Sets
set labelFamilies := 0 .. 2;
set labelFamilyLabels := 0 .. 5;

# Parameters
param familyName{labelFamilies} symbolic;
param labelName{labelFamilies, labelFamilyLabels} symbolic;
param labelValue{labelFamilies, labelFamilyLabels};
param labelDependencies{f in labelFamilies, l in labelFamilyLabels, df in labelFamilies, dl in labelFamilyLabels: f <> df} binary;

# Variables
var labelChosen{labelFamilies, labelFamilyLabels} binary;

# Constraints
s.t. onePerFamily{f in labelFamilies}: sum{l in labelFamilyLabels} labelChosen[f, l] = 1;
s.t. dependencies{f in labelFamilies, l in labelFamilyLabels}:
    (sum{df in labelFamilies, dl in labelFamilyLabels: f <> df} labelDependencies[f, l, df, dl] * labelChosen[df, dl])
 >= (card(labelFamilies) - 1) * labelChosen[f, l];

# Goal
maximize buildValue:
    sum{f in labelFamilies, l in labelFamilyLabels} labelValue[f, l]*labelChosen[f, l];

solve;

# Result Output
for {f in labelFamilies, l in labelFamilyLabels: labelChosen[f, l] = 1} {
    printf "Label Chosen for Family %s: %s (Value %0.5f)\n", familyName[f], labelName[f, l], labelValue[f, l];
}

data;

param familyName :=
        0           A
        1           B
        2           C
        ;

param labelName :=
        :       0       1       2       3       4       5 :=
        0      A1      A2      A3      A4      A5      A6
        1      B1      B2      B3      B4      B5      B6
        2      C1      C2      C3      C4      C5      C6
        ;

param labelValue :=
        :       0       1       2       3       4       5 :=
        0 0.16667 0.33333 0.50000 0.66667 -100000 -100000
        1 0.16667 0.33333 0.50000 0.66667 0.83333 1.00000
        2 0.16667 0.33333 0.50000 0.66667 0.83333 1.00000
        ;

param labelDependencies :=
[   0,   0,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   0,   1,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   0,   2,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   0,   3,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   0,   4,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   0,   5,   *,   *]   :       0       1       2       3       4       5 :=
                        1       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   0,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   1,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   2,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   3,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   4,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   1,   5,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        2       1       1       1       1       1       1
[   2,   0,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        1       1       1       1       1       1       1
[   2,   1,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        1       1       1       1       1       1       1
[   2,   2,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        1       1       1       1       1       1       1
[   2,   3,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        1       1       1       1       1       1       1
[   2,   4,   *,   *]   :       0       1       2       3       4       5 :=
                        0       1       1       1       1       1       1
                        1       1       1       1       1       1       1
[   2,   5,   *,   *]   :       0       1       2       3       4       5 :=
                        0       0       0       0       0       0       1
                        1       0       0       0       0       0       1
                        ;

end;
