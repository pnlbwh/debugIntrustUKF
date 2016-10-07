#!/bin/bash -eu

case=${2##*/}
source SetUpData.sh

redo-ifchange $famask $dwiharm

tensor=$(mktemp).nrrd

tend estim -est lls -B kvp -knownB0 true -i $dwiharm -o $tensor
tend anvol -t -1 -a fa -i $tensor -o $3
unu 3op ifelse $famask $3 0 -o $3 -t FLOAT

rm $tensor
