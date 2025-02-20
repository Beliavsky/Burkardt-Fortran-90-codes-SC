#! /bin/bash
#
gfortran -c -Wall mxv.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mxv.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mxv.o
#
mv a.out ~/bin/mxv
#
echo "Normal end of execution."
