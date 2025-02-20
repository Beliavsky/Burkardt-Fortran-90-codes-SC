#! /bin/bash
#
gfortran -c -Wall flow3.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran flow3.o -L$HOME/lib -ltoms611
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm flow3.o
#
mv a.out ~/bin/flow3
#
echo "Normal end of execution."
