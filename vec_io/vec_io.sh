#! /bin/bash
#
mkdir temp
cd temp
rm *
~/bin/f90split ../vec_io.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f90
#
ar qc libvec_io.a *.o
rm *.o
#
mv libvec_io.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
