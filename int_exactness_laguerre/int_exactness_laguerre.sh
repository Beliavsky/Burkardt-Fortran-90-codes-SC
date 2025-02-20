#! /bin/bash
#
gfortran -c -Wall int_exactness_laguerre.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran int_exactness_laguerre.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm int_exactness_laguerre.o
#
chmod ugo+x a.out
mv a.out ~/bin/int_exactness_laguerre
#
echo "Normal end of execution."
