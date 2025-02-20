#! /bin/bash
#
gfortran -c -Wall starpac_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran starpac_test.o -L$HOME/lib -lstarpac
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm starpac_test.o
#
mv a.out starpac_test
./starpac_test > starpac_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm starpac_test
#
echo "Normal end of execution."
