#! /bin/bash
#
gfortran -c -Wall flow5.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran flow5.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm flow5.o
#
mv a.out ~/bin/flow5
#
echo "Normal end of execution."
