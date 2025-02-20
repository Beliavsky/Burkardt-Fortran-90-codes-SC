#! /bin/bash
#
gfortran -c -Wall md2.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md2.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md2.o
#
mv a.out ~/bin/md2
#
echo "Normal end of execution."
