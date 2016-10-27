#!/bin/bash -eu

source ./loglib.sh

case=${2##*/}
source SetUpData.sh

tmpvtk=/tmp/$case.vtk
run "gunzip -c $ukf > $tmpvtk"

[ -e "$tmpvtk" ] || { echo "Couldn't unzip $ukf"; exit 1; }

source $HOME/lib/env.sh
run "./countfibers.j $tmpvtk $3"
run "rm $tmpvtk"
