#! /bin/bash
#
gfortran -c -Wall mhd_control.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mhd_control.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mhd_control.o
#
chmod ugo+x a.out
mv a.out ~/bin/mhd_control
#
echo "Normal end of execution."
