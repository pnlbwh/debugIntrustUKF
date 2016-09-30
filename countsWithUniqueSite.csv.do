#!/bin/bash -eu

echo "uid,count,id,ukftype" > $3
sed -f intrust-caseids.sed <(cut -d"," -f2 < counts.csv) | paste -d"," - counts.csv | sed 's/ //g' >> $3
