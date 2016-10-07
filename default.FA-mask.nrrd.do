#!/bin/bash -eu

case=${2##*/}
source SetUpData.sh

export PYTHONPATH=
source /data/pnl/soft/anaconda3/bin/activate py27
./scripts/famask.py $fsindwi $3

echo "Made '$1'"
