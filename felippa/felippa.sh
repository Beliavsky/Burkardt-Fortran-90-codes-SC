#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../felippa.f90
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
ar qc libfelippa.a *.o
rm *.o
#
mv libfelippa.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
