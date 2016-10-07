#!/usr/bin/env python

import nrrd
import numpy as n
import scipy.ndimage as s
import sys

nrrdin=sys.argv[1]
nrrdout=sys.argv[2]

(a,hdr) = nrrd.read(nrrdin)

#4   Left-Lateral-Ventricle
#14  3rd-Ventricle
#15  4th-Ventricle
#43  Right-Lateral-Ventricle
#72  5th-Ventricle
#75  Left-Lateral-Ventricles
#76  Right-Lateral-Ventricles
#213 lateral_ventricle
#24  CSF

for csf in [4,14,15,43,72,75,76,213,24]:
    a[a==csf] = 0

a[a>0] = 1
a=s.binary_erosion(a).astype(a.dtype)

nrrd.write(nrrdout,a,hdr)
