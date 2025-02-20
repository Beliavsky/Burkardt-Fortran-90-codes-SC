#! /bin/bash
#
gfortran -c -Wall cvt_size_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cvt_size_test.o -L$HOME/lib -lcvt_size
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_size_test.o
#
mv a.out cvt_size_test
./cvt_size_test > cvt_size_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cvt_size_test
#
echo "Normal end of execution."
