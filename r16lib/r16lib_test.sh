#! /bin/bash
#
gfortran -c -Wall r16lib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran r16lib_test.o -L$HOME/lib -lr16lib
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r16lib_test.o
#
mv a.out r16lib_test
./r16lib_test > r16lib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r16lib_test
#
echo "Normal end of execution."
