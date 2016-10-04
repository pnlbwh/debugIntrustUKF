#!/usr/bin/env python

import sys, glob, random
from os.path import basename

out = sys.argv[3]
txts = glob.glob('_data/tractlengths/*txt')
sampleProb = 0.03

with open (out, 'a') as fout:
    fout.write('siteid,ukftype,num\n')
    for txt in txts:
        siteid = basename(txt).split('_')[0]
        ukftype = basename(txt).split('.')[2]
        with open(txt, 'r') as f:
            for line in f.readlines():
                if (random.random() < sampleProb):
                    fout.write(','.join([siteid,ukftype,line]))
