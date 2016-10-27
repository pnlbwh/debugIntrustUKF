#!/usr/bin/env python

import glob
import random
from os.path import basename, splitext
import sys

#txts = glob.glob('_data/tractlengths/*txt')
txts = sys.argv[1:]
sampleProb = 0.03

fout = open('_data/fiber_lengths_sample.csv', 'w')
fout2 = open('_data/fiber_counts.csv', 'w')

fout2.write('caseid,ukftype,count\n')
fout.write('caseid,ukftype,length\n')
for txt in txts:
    caseid, ukftype = splitext(basename(txt))[0].split('-',1)
    ukftype = ukftype.split('-',1)[1] # remove FiberLengths-
    with open(txt, 'r') as f:
        lines = f.read().splitlines()
        fout2.write(','.join([caseid, ukftype, str(len(lines))]) + '\n')
        for line in lines:
            if (random.random() < sampleProb):
                fout.write(','.join([caseid, ukftype, line]) + '\n')

fout2.close()
fout.close()
