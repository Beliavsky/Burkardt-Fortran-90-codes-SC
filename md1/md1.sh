#! /bin/bash
#
gfortran -c -Wall md1.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md1.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md1.o
#
mv a.out ~/bin/md1
#
echo "Normal end of execution."
