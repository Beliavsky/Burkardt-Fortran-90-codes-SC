#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../pdb_read.f90
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
ar qc libpdb_read.a *.o
rm *.o
#
mv libpdb_read.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
