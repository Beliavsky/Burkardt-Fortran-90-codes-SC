#! /bin/bash
#
gfortran -c -Wall mhd_flow.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mhd_flow.o -L$HOME/lib -llinpack_d -lblas1_d
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mhd_flow.o
#
mv a.out ~/bin/mhd_flow
#
echo "Normal end of execution."
