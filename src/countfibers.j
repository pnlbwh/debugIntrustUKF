#!/PHShome/re098/bin/jconsole

vtk=:2{ARGV
out=:3{ARGV

load'/PHShome/re098/soft/jvtk/vtk.ijs'
t=:vtkReadPoly vtk
(": #every t) fwrite out
exit''
