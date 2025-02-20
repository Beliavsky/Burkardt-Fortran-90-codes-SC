#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../r8lib.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Compile error."
    exit
  fi
done
rm *.f90
#
ar qc libr8lib.a *.o
rm *.o
#
mv libr8lib.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
