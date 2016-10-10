#!/usr/bin/env python

import sys, glob, random
from os.path import basename
import os

txts = glob.glob('_data/tractlengths/*txt')
sampleProb = 0.03

fout = open ('_data/fiber_lengths_sample.csv', 'w')
fout2 = open('_data/fiber_counts.csv', 'w')

fout2.write('siteid,ukftype,count\n')
fout.write('siteid,ukftype,length\n')
for txt in txts:
    siteid = basename(txt).split('_')[0]
    ukftype = basename(txt).split('.')[2]
    with open(txt, 'r') as f:
        lines = f.read().splitlines()
        fout2.write(','.join([siteid,ukftype,str(len(lines))]) +'\n')
        for line in lines:
            if (random.random() < sampleProb):
                fout.write(','.join([siteid,ukftype,line]) + '\n')

fout2.close()
fout.close()

os.system("sed -i -f intrust-caseids.sed _data/fiber_lengths_sample.csv")
os.system("sed -i -f intrust-caseids.sed _data/fiber_counts.csv")
