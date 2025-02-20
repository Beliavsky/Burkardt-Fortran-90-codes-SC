#! /bin/bash
#
gfortran -c -Wall md3.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md3.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md3.o
#
mv a.out ~/bin/md3
#
echo "Normal end of execution."
