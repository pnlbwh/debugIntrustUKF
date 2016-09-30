#!/bin/bash -eu

echo "length,ukftype" > $3
cat mat/tracts.csv vtk/tracts.csv >> $3
