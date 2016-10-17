#!/PHShome/re098/bin/jconsole

out=: >2{ARGV
fas=: 3}.ARGV
load'/PHShome/re098/soft/jnrrd/nrrd.ijs'
load'stats'
load'csv'
getid=:({.@:('-FA'&splitstring) every)@:{:@:fpathname
getsiteid=: (3&{. each)@:{:@:fpathname
stats=:(mean,&<stddev)@,@readnrrd every fas
hdr=:'caseid';'siteid';'filename';'mean';'stddev'
data=: (getid every fas) ,. (getsiteid every fas) ,. fas ,. stats
(hdr,data) writecsv out
exit''
