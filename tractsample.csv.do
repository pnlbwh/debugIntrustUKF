#!/bin/bash -eu

awk 'NR==1 {print} ; rand()>.9 {print}' < tracts.csv > $3
