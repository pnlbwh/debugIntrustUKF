#!/PHShome/re098/bin/jconsole

out=:4{ARGV
fas=:1 dir'_data/FA-masked/*nrrd'

load'/PHShome/re098/soft/jnrrd/nrrd.ijs'
load'stats'
load'csv'
getsiteid=:3&{.@{:@:fpathname
stats=:(mean,&<stddev)@,@readnrrd every fas
hdr=:'siteid';'filename';'mean';'stddev'
data=: (getsiteid every fas) ,. fas ,. stats
(hdr,data) writecsv out
exit''
