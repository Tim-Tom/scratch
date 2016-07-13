use strict;
use warnings;

use v5.16;

my ($numFamilies, $numLabelsPerFamily) = @ARGV;

my ($maxFamilyIndex, $maxLabelIndex) = map {$_ - 1} ($numFamilies, $numLabelsPerFamily);

sub getFamilyName {
    my $id = shift;
    my $result = '';
    for my $i (0 .. int($maxFamilyIndex / 26)) {
        my $chr = chr(ord('A') + ($id % 26));
        $result = "$chr$result";
        $id = int($id / 26);
    }
    return $result;
}

print <<"END_HEADER";
# Sets
set labelFamilies := 0 .. $maxFamilyIndex;
set labelFamilyLabels := 0 .. $maxLabelIndex;

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
    printf "Label Chosen for Family %s: %s (Value %0.5f)\\n", familyName[f], labelName[f, l], labelValue[f, l];
}

data;

param familyName :=
END_HEADER

for my $f (0 .. $maxFamilyIndex) {
    my $name = getFamilyName($f);
    printf "%9d%12s\n", $f, $name;
}
say "        ;";
say "";

say "param labelName :=";
print "        :";

for my $l (0 .. $maxLabelIndex) {
    printf "%8d", $l;
}
say " :=";

for my $f (0 .. $maxFamilyIndex) {
    my $name = getFamilyName($f);
    printf "%9d", $f;
    for my $l (1 .. $numLabelsPerFamily) {
        printf "%8s", "${name}$l";
    }
    print "\n";
}
say "        ;";
say "";

say "param labelValue :=";
print "        :";

for my $l (0 .. $maxLabelIndex) {
    printf "%8d", $l;
}
say " :=";

for my $f (0 .. $maxFamilyIndex) {
    printf "%9d", $f;
    for my $l (1 .. $numLabelsPerFamily) {
        if ($l == 1 || rand > 0.3) {
            printf " %0.5f", $l / $numLabelsPerFamily;
        } else {
            print " -100000";
        }
    }
    print "\n";
}
say "        ;";
say "";

say "param labelDependencies :=";
for my $f (0 .. $maxFamilyIndex) {
    for my $l (0 .. $maxLabelIndex) {
        printf "[%4d,%4d,   *,   *]   :", $f, $l;
        for my $hdr (0 .. $maxLabelIndex) {
            printf "%8d", $hdr;
        }
        say " :=";
        for my $df (0 .. $maxFamilyIndex) {
            next if $f == $df;
            printf "                 %8d", $df;
            for my $dl (0 .. $maxLabelIndex) {
                printf "%8d", (rand 1 < (100 - ($dl - $l)**2) / 100);
            }
            print "\n";
        }
    }
}
say "                        ;";
say "";

say "end;";
