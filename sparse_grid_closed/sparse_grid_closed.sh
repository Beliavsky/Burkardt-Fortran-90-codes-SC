#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../sparse_grid_closed.f90
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
ar qc libsparse_grid_closed.a *.o
rm *.o
#
mv libsparse_grid_closed.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
