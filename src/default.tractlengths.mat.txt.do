#!/bin/bash -eu

case=${2##*/}
source SetUpData.sh

export MATLABPATH= 
MATLAB="matlab -nosplash -nodesktop -r"
if [ -f config/MATLAB ]; then
    MATLAB=$(<config/MATLAB)
    MATLAB="$MATLAB -nosplash -nodesktop -r"
fi

M_CMD="\
x = load('$mat');\
l = cellfun(@(x) size(x,2),x.ff);\
dlmwrite('/data/pnl/debugIntrustUKF/$3', l, ' ');\
quit"

echo "$M_CMD"
$MATLAB "$M_CMD"
