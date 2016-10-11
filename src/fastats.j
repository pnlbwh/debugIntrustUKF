#!/PHShome/re098/bin/jconsole

out=: >2{ARGV
fas=: 3}.ARGV

load'/PHShome/re098/soft/jnrrd/nrrd.ijs'
load'stats'
load'csv'
getsiteid=: (3&{. each)@:{:@:fpathname
stats=:(mean,&<stddev)@,@readnrrd every fas
hdr=:'siteid';'filename';'mean';'stddev'
data=: (getsiteid every fas) ,. fas ,. stats
(hdr,data) writecsv out
exit''
