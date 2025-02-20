#! /bin/bash
#
gfortran -c -Wall cvt_fixed_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cvt_fixed_test.o -L$HOME/lib -lcvt_fixed -lpbmlib
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_fixed_test.o
#
mv a.out cvt_fixed_test
./cvt_fixed_test > cvt_fixed_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cvt_fixed_test
#
echo "Normal end of execution."
